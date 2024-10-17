/*
 * Copyright 2001-2024 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest

import org.scalactic.Requirements._
import org.scalactic.{Resources => _, _}
import org.scalatest.Suite._
import java.util.ConcurrentModificationException
import java.util.concurrent.atomic.AtomicReference

import org.scalactic.exceptions.NullArgumentException
import org.scalatest.events.LineInFile
import org.scalatest.events.Location
import org.scalatest.events.SeeStackDepthException
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepth

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import Suite.IgnoreTagName

import collection.mutable.ListBuffer
import org.scalatest.exceptions._
import org.scalatest.time.{Seconds, Span}
import org.scalatest.tools.{SuiteSortingReporter, TestSortingReporter, TestSpecificReporter}
import org.scalatest.tools.Utils.wrapReporterIfNecessary
import scala.collection.immutable.ListSet
import scala.util.Try

// T will be () => Unit for FunSuite and FixtureParam => Any for fixture.FunSuite
private[scalatest] sealed abstract class AsyncSuperEngine[T](concurrentBundleModMessageFun: => String, simpleClassName: String) {

  sealed abstract class Node(val parentOption: Option[Branch]) {
    def indentationLevel: Int = {
      def calcLevel(currentParentOpt: Option[Branch], currentLevel: Int): Int = 
        currentParentOpt match {
          case None => currentLevel
          case Some(parent) => calcLevel(parent.parentOption, currentLevel + 1)
        }
      val level = calcLevel(parentOption, -1)
      if (level < 0) 0 else level
    }
  }

  abstract class Branch(parentOption: Option[Branch]) extends Node(parentOption) {
    var subNodes: List[Node] = Nil
    var pending: Boolean = false
  }

  case object Trunk extends Branch(None)

  case class TestLeaf(
    parent: Branch,
    testName: String, // The full test name
    testText: String, // The last portion of the test name that showed up on an inner most nested level
    testFun: T, 
    location: Option[Location],
    pos: Option[source.Position],
    recordedDuration: Option[Long] = None
  ) extends Node(Some(parent))

  case class InfoLeaf(parent: Branch, message: String, payload: Option[Any], location: Option[LineInFile]) extends Node(Some(parent))
  case class NoteLeaf(parent: Branch, message: String, payload: Option[Any], location: Option[LineInFile]) extends Node(Some(parent))
  case class AlertLeaf(parent: Branch, message: String, payload: Option[Any], location: Option[LineInFile]) extends Node(Some(parent))
  case class MarkupLeaf(parent: Branch, message: String, location: Option[LineInFile]) extends Node(Some(parent))

  case class DescriptionBranch(
    parent: Branch,
    descriptionText: String,
    childPrefix: Option[String], // If defined, put it at the beginning of any child descriptionText or testText 
    location: Option[Location]
  ) extends Branch(Some(parent))   

  // Access to the testNamesList, testsMap, and tagsMap must be synchronized, because the test methods are invoked by
  // the primary constructor, but testNames, tags, and runTest get invoked directly or indirectly
  // by run. When running tests concurrently with ScalaTest Runner, different threads can
  // instantiate and run the suite. Instead of synchronizing, I put them in an immutable Bundle object (and
  // all three collections--testNamesList, testsMap, and tagsMap--are immuable collections), then I put the Bundle
  // in an AtomicReference. Since the expected use case is the test method will be called
  // from the primary constructor, which will be all done by one thread, I just in effect use optimistic locking on the Bundle.
  // If two threads ever called test at the same time, they could get a ConcurrentModificationException.
  // Test names are in reverse order of test registration method invocations
  class Bundle private(
    val currentBranch: Branch,
    val testNamesList: List[String],
    val testsMap: Map[String, TestLeaf],
    val tagsMap: Map[String, Set[String]],
    val registrationClosed: Boolean
  ) {
    def unpack = (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed)
  }

  object Bundle {
    def apply(
      currentBranch: Branch,
      testNamesList: List[String],
      testsMap: Map[String, TestLeaf],
      tagsMap: Map[String, Set[String]],
      registrationClosed: Boolean
    ): Bundle =
      new Bundle(currentBranch, testNamesList, testsMap, tagsMap, registrationClosed)
  }

  final val atomic = new AtomicReference[Bundle](Bundle(Trunk, List(), Map(), Map(), false))

  def updateAtomic(oldBundle: Bundle, newBundle: Bundle): Unit = {
    val shouldBeOldBundle = atomic.getAndSet(newBundle)
    if (!(shouldBeOldBundle eq oldBundle))
      throw new ConcurrentModificationException(concurrentBundleModMessageFun)
  }

  class RegistrationInformer extends Informer {

    def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Unit = {
      requireNonNull(message, payload)
      val oldBundle = atomic.get
      var (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      currentBranch.subNodes ::= InfoLeaf(currentBranch, message, payload, Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname))))
      updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    }
  }

  class RegistrationNotifier extends Notifier {

    def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Unit = {
      requireNonNull(message, payload)
      val oldBundle = atomic.get
      var (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      currentBranch.subNodes ::= NoteLeaf(currentBranch, message, payload, Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname))))
      updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    }
  }

  class RegistrationAlerter extends Alerter {

    def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Unit = {
      requireNonNull(message, payload)
      val oldBundle = atomic.get
      var (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      currentBranch.subNodes ::= AlertLeaf(currentBranch, message, payload, Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname))))
      updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    }
  }

  class RegistrationDocumenter extends Documenter {
    def apply(message: String)(implicit pos: source.Position): Unit = {
      requireNonNull(message)
      val oldBundle = atomic.get
      var (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      currentBranch.subNodes ::= MarkupLeaf(currentBranch, message, Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname))))
      updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    }
  }

  // The informer will be a registration informer until run is called for the first time. (This
  // is the registration phase of a style trait's lifecycle.)
  final val atomicInformer = new AtomicReference[Informer](new RegistrationInformer)

  final val atomicNotifier = new AtomicReference[Notifier](new RegistrationNotifier)
  final val atomicAlerter = new AtomicReference[Alerter](new RegistrationAlerter)

  // The documenter will be a registration informer until run is called for the first time. (This
  // is the registration phase of a style trait's lifecycle.)
  final val atomicDocumenter = new AtomicReference[Documenter](new RegistrationDocumenter)

  final val zombieInformer =
    new Informer {
      def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Unit = {
        requireNonNull(message, payload)
        println(Resources.infoProvided(message))
        payload match {
          case Some(p) => println(Resources.payloadToString(payload.get.toString))
          case _ =>
        }
      }
    }

  final val zombieNotifier =
    new Notifier {
      def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Unit = {
        requireNonNull(message, payload)
        println(Resources.noteProvided(message))
        payload match {
          case Some(p) => println(Resources.payloadToString(payload.get.toString))
          case _ =>
        }
      }
    }

  final val zombieAlerter =
    new Alerter {
      def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Unit = {
        requireNonNull(message, payload)
        println(Resources.alertProvided(message))
        payload match {
          case Some(p) => println(Resources.payloadToString(payload.get.toString))
          case _ =>
        }
      }
    }

  final val zombieDocumenter =
    new Documenter {
      def apply(message: String)(implicit pos: source.Position): Unit = {
        requireNonNull(message)
        println(Resources.markupProvided(message))
      }
    }

  private def checkTestOrIgnoreParamsForNull(testName: String, testTags: Tag*): Unit = {
    requireNonNull(testName)
    if (testTags.exists(_ == null))
      throw new NullArgumentException("a test tag was null")
  }

  def runTestImpl(
    theSuite: Suite,
    testName: String,
    args: Args,
    includeIcon: Boolean,
    parallelAsyncTestExecution: Boolean,
    invokeWithFixture: (TestLeaf, Try[Outcome] => Unit) => AsyncOutcome
  )(implicit executionContext: ExecutionContext): Status = {

    requireNonNull(testName, args)

    if (!parallelAsyncTestExecution) {
      // Tell the TSR that the test is being distributed
      for (sorter <- args.distributedTestSorter)
        sorter.distributingTest(testName)
    }

    
    import args._

    val (theStopper, report, testStartTime) =
      Suite.getRunTestGoodies(theSuite, stopper, reporter, testName)

    if (!atomic.get.testsMap.contains(testName))
      throw new IllegalArgumentException("No test in this suite has name: \"" + testName + "\"")

    val theTest = atomic.get.testsMap(testName)

    reportTestStarting(theSuite, report, tracker, testName, theTest.testText, theSuite.rerunner, theTest.location)

    val testTextWithOptionalPrefix = prependChildPrefix(theTest.parent, theTest.testText)
    val formatter = getIndentedTextForTest(testTextWithOptionalPrefix, theTest.indentationLevel, includeIcon)

    val messageRecorderForThisTest = new MessageRecorder(report)
    val informerForThisTest =
      MessageRecordingInformer(
        messageRecorderForThisTest,
        (message, payload, isConstructingThread, testWasPending, testWasCanceled, location) => createInfoProvided(theSuite, report, tracker, Some(testName), message, payload, theTest.indentationLevel + 1, location, isConstructingThread, includeIcon)
      )

    val updaterForThisTest =
      ConcurrentNotifier(
        (message, payload, isConstructingThread, location) => {
          reportNoteProvided(theSuite, report, tracker, Some(testName), message, payload, 1, location, isConstructingThread)
        }
      )

    val alerterForThisTest =
      ConcurrentAlerter(
        (message, payload, isConstructingThread, location) => {
          reportAlertProvided(theSuite, report, tracker, Some(testName), message, payload, 1, location, isConstructingThread)
        }
      )

    val documenterForThisTest =
      MessageRecordingDocumenter(
        messageRecorderForThisTest,
        (message, None, isConstructingThread, testWasPending, testWasCanceled, location) => createMarkupProvided(theSuite, report, tracker, Some(testName), message, theTest.indentationLevel + 1, location, isConstructingThread)
      )

    val oldInformer = atomicInformer.getAndSet(informerForThisTest)
    val oldNotifier = atomicNotifier.getAndSet(updaterForThisTest)
    val oldAlerter = atomicAlerter.getAndSet(alerterForThisTest)
    val oldDocumenter = atomicDocumenter.getAndSet(documenterForThisTest)

    val onCompleteFun: Try[Outcome] => Unit = { trial =>
      trial match {
        case Success(outcome) =>
          outcome match {

            case Succeeded =>
              val duration = System.currentTimeMillis - testStartTime
              val durationToReport = theTest.recordedDuration.getOrElse(duration)
              val recordEvents = messageRecorderForThisTest.recordedEvents(false, false) // TODO: zap this
              reportTestSucceeded(theSuite, report, tracker, testName, theTest.testText, recordEvents, durationToReport, formatter, theSuite.rerunner, theTest.location)

            case Pending =>
              val duration = System.currentTimeMillis - testStartTime
              // testWasPending = true so info's printed out in the finally clause show up yellow
              val recordEvents = messageRecorderForThisTest.recordedEvents(true, false) // TODO: Zap this
              reportTestPending(theSuite, report, tracker, testName, theTest.testText, recordEvents, duration, formatter, theTest.location)

            case Canceled(e) =>
              val duration = System.currentTimeMillis - testStartTime
              // testWasCanceled = true so info's printed out in the finally clause show up yellow
              val recordEvents = messageRecorderForThisTest.recordedEvents(false, true) // TODO: zap this
              reportTestCanceled(theSuite, report, e, testName, theTest.testText, recordEvents, theSuite.rerunner, tracker, duration, formatter, theTest.location)

            case Failed(e) =>
              val duration = System.currentTimeMillis - testStartTime
              val durationToReport = theTest.recordedDuration.getOrElse(duration)
              val recordEvents = messageRecorderForThisTest.recordedEvents(false, false) // TODO: Zap this
              reportTestFailed(theSuite, report, e, testName, theTest.testText, recordEvents, theSuite.rerunner, tracker, durationToReport, formatter,  Some(SeeStackDepthException))
          }
        // We will only get here if an exception that should cause a Suite to abort rather than a test
        // to fail has happened. In that case, it will be reported as a suite abort, because of this line of code:
        // status.setUnreportedException(ex)
        // in AsyncOutcome.scala
        // Well not sure. Maybe we just throw it, like we did before. This was to let the thread die. But it is a tad odd,
        // because the test didn't complete. Almost need a TestAborted event, but if it is truly fatal, then that means
        // please try and die gracefully, not fire a scalatest and continue. More like want to have fatalException and
        // unreportedException. unreportedException is for when before or after code blows up, or a constructor blows up.
        // fatalException is for anExceptionThatShouldCauseASuiteToAbort no matter when it happens. And maybe once that
        // happens, everything just blows up with that exception.
        /*
          suiteAbortingException: Option[Throwable] // These would be 2 different things.
          threadKillingException: Option[Throwable]

           or could do unreportedException and fatalException
           extraTestException
           exceptionToReport
           exceptionToRethrow

           I like unreportedException and isFatal
        */
        case Failure(ex) =>
      }

      if (!parallelAsyncTestExecution) {
        for (sorter <- args.distributedTestSorter)
          sorter.completedTest(testName)
      }

      // SKIP-SCALATESTJS,NATIVE-START
      val shouldBeInformerForThisTest = atomicInformer.getAndSet(oldInformer)

      if (shouldBeInformerForThisTest ne informerForThisTest)
        throw new ConcurrentModificationException(Resources.concurrentInformerMod(theSuite.getClass.getName))

      val shouldBeNotifierForThisTest = atomicNotifier.getAndSet(oldNotifier)
      if (shouldBeNotifierForThisTest ne updaterForThisTest)
        throw new ConcurrentModificationException(Resources.concurrentNotifierMod(theSuite.getClass.getName))

      val shouldBeAlerterForThisTest = atomicAlerter.getAndSet(oldAlerter)
      if (shouldBeAlerterForThisTest ne alerterForThisTest)
        throw new ConcurrentModificationException(Resources.concurrentAlerterMod(theSuite.getClass.getName))

      val shouldBeDocumenterForThisTest = atomicDocumenter.getAndSet(oldDocumenter)
      if (shouldBeDocumenterForThisTest ne documenterForThisTest)
        throw new ConcurrentModificationException(Resources.concurrentDocumenterMod(theSuite.getClass.getName))
      // SKIP-SCALATESTJS,NATIVE-END
    }

    val asyncOutcome: AsyncOutcome =
      try {
        val outcome = invokeWithFixture(theTest, onCompleteFun)
        executionContext match {
          case dec: concurrent.SerialExecutionContext =>
            dec.runNow(outcome.toFutureOfOutcome)
          case _ =>
        }
        outcome
      }
      catch {
        case ex: TestCanceledException => PastAsyncOutcome(Canceled(ex), onCompleteFun) // Probably don't need these anymore.
        case _: TestPendingException => PastAsyncOutcome(Pending, onCompleteFun)
        case tfe: TestFailedException => PastAsyncOutcome(Failed(tfe), onCompleteFun)
        case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => PastAsyncOutcome(Failed(ex), onCompleteFun)
      }

    asyncOutcome.toStatus
  }

  private def runTestsInBranch(
    theSuite: Suite,
    branch: Branch,
    args: Args,
    includeIcon: Boolean,
    parallelAsyncTestExecution: Boolean,
    initStatusVec: Vector[Status],
    runTest: (String, Args) => Status
  ): Vector[Status] = {

    import args.stopper

    def traverseSubNodes(): Vector[Status] = {
      //branch.subNodes.reverse.flatMap { node =>
      branch.subNodes.reverse.foldLeft(initStatusVec) { case (statusVec, node) =>
        if (!stopper.stopRequested) {
          node match {
            case testLeaf @ TestLeaf(_, testName, testText, _, _, _, _) =>
              val (filterTest, ignoreTest) = args.filter(testName, theSuite.tags, theSuite.suiteId)
              if (!filterTest)
                if (ignoreTest) {
                  val testTextWithOptionalPrefix = prependChildPrefix(branch, testText)
                  val theTest = atomic.get.testsMap(testName)
                  statusVec :+ (
                    if (parallelAsyncTestExecution || statusVec.isEmpty) {
                      // Even if serial async test execution (i.e., not parallelAsyncTestExection), first time still just go for it
                      reportTestIgnored(theSuite, args.reporter, args.tracker, testName, testTextWithOptionalPrefix, getIndentedTextForTest(testTextWithOptionalPrefix, testLeaf.indentationLevel, true), theTest.location)
                      SucceededStatus
                    }
                    else {
                      statusVec.last thenRun {
                        reportTestIgnored(theSuite, args.reporter, args.tracker, testName, testTextWithOptionalPrefix, getIndentedTextForTest(testTextWithOptionalPrefix, testLeaf.indentationLevel, true), theTest.location)
                        SucceededStatus
                      }  // Only if serial async test execution (i.e., not parallelAsyncTestExecution), after first Status
                    }
                  )
                }
                else {
                  statusVec :+ (
                    if (parallelAsyncTestExecution || statusVec.isEmpty) {
                      runTest(testName, args) // Even if serial async test execution (i.e., not parallelAsyncTestExection), first time still just go for it
                    }
                    else {
                      statusVec.last thenRun runTest(testName, args)  // Only if serial async test execution (i.e., not parallelAsyncTestExecution), after first Status
                    }
                  )
                }
              else
                statusVec

            case infoLeaf @ InfoLeaf(_, message, payload, location) =>
              reportInfoProvided(theSuite, args.reporter, args.tracker, None, message, payload, infoLeaf.indentationLevel, location, true, includeIcon)
              statusVec

            case noteLeaf @ NoteLeaf(_, message, payload, location) =>
              reportNoteProvided(theSuite, args.reporter, args.tracker, None, message, payload, noteLeaf.indentationLevel, location, true, includeIcon)
              statusVec

            case alertLeaf @ AlertLeaf(_, message, payload, location) =>
              reportAlertProvided(theSuite, args.reporter, args.tracker, None, message, payload, alertLeaf.indentationLevel, location, true, includeIcon)
              statusVec

            case markupLeaf @ MarkupLeaf(_, message, location) =>
              reportMarkupProvided(theSuite, args.reporter, args.tracker, None, message, markupLeaf.indentationLevel, location, true, includeIcon)
              statusVec

            case branch: Branch =>
              if (parallelAsyncTestExecution || statusVec.isEmpty) {
                runTestsInBranch(theSuite, branch, args, includeIcon, parallelAsyncTestExecution, statusVec, runTest) // Even if serial async test execution (i.e., not parallelAsyncTestExection), first time still just go for it
              }
              else {
                statusVec :+ (statusVec.last thenRun (new CompositeStatus(ListSet(runTestsInBranch(theSuite, branch, args, includeIcon, parallelAsyncTestExecution, statusVec, runTest): _*))))  // Only if serial async test execution (i.e., not parallelAsyncTestExecution), after first Status
              }
          }
        }
        else
          statusVec
      }
    }

    branch match {

      case desc @ DescriptionBranch(parent, descriptionText, _, lineInFile) =>

        val descriptionTextWithOptionalPrefix = prependChildPrefix(parent, descriptionText)
        val indentationLevel = desc.indentationLevel
        reportScopeOpened(theSuite, args.reporter, args.tracker, descriptionTextWithOptionalPrefix, indentationLevel, false, lineInFile)
        val statusVec = traverseSubNodes()
        if (desc.pending)
          reportScopePending(theSuite, args.reporter, args.tracker, descriptionTextWithOptionalPrefix, indentationLevel, false, lineInFile)
        else
          reportScopeClosed(theSuite, args.reporter, args.tracker, descriptionTextWithOptionalPrefix, indentationLevel, false, lineInFile)
        statusVec

      case Trunk =>
        traverseSubNodes()
    }
  }

  def prependChildPrefix(branch: Branch, testText: String): String =
    branch match {
      case DescriptionBranch(_, _, Some(cp), _) => Resources.prefixSuffix(cp, testText)
      case _ => testText
    }

  def runTestsImpl(
    theSuite: Suite,
    testName: Option[String],
    passedInArgs: Args,
    includeIcon: Boolean,
    parallelAsyncTestExecution: Boolean,
    runTest: (String, Args) => Status
  ): Status = {
    requireNonNull(testName, passedInArgs)

    val args =
      if (!parallelAsyncTestExecution) {
        if (passedInArgs.runTestInNewInstance)
          passedInArgs // This is the test-specific instance
        else {
          if (passedInArgs.distributedTestSorter.isEmpty) {
            val testSortingTimeout =
              passedInArgs.distributedSuiteSorter match {
                case Some(ssr: SuiteSortingReporter) => ssr.testSortingTimeout
                case _ => Span(Suite.defaultTestSortingReporterTimeoutInSeconds, Seconds)
              }
            val testSortingReporter = new TestSortingReporter(theSuite.suiteId, passedInArgs.reporter, testSortingTimeout, theSuite.expectedTestCount(passedInArgs.filter), passedInArgs.distributedSuiteSorter, System.err)
            passedInArgs.copy(reporter = testSortingReporter, distributedTestSorter = Some(testSortingReporter))
          }
          else
            passedInArgs
        }
      }
      else
        passedInArgs

    import args._

    // Wrap any non-DispatchReporter, non-CatchReporter in a CatchReporter,
    // so that exceptions are caught and transformed
    // into error messages on the standard error stream.
    val report = wrapReporterIfNecessary(theSuite, reporter)
    val newArgs = if (report eq reporter) args else args.copy(reporter = report)
    
    val statusList: Vector[Status] =
      // If a testName is passed to run, just run that, else run the tests returned
      // by testNames.
      testName match {
        case Some(tn) =>
          val (filterTest, ignoreTest) = filter(tn, theSuite.tags, theSuite.suiteId)
          if (!filterTest) {
            if (ignoreTest) {
              val theTest = atomic.get.testsMap(tn)
              reportTestIgnored(theSuite, report, tracker, tn, tn, getIndentedTextForTest(tn, 1, true), theTest.location)
              Vector.empty
            }
            else {
              Vector(runTest(tn, newArgs))
            }
          }
          else
            Vector.empty

        case None => runTestsInBranch(theSuite, Trunk, newArgs, includeIcon, parallelAsyncTestExecution, Vector.empty, runTest)
      }
    new CompositeStatus(Set.empty ++ statusList)
  }

  def runImpl(
    theSuite: Suite,
    testName: Option[String],
    passedInArgs: Args,
    parallelAsyncTestExecution: Boolean,
    superRun: (Option[String], Args) => Status
  ): Status = {

    val args =
      if (!parallelAsyncTestExecution)
        (testName, passedInArgs.distributedTestSorter) match {
          case (Some(name), Some(sorter)) => passedInArgs.copy(reporter = new TestSpecificReporter(sorter, name))
          case _ => passedInArgs
        }
      else
        passedInArgs

    import args._

    // Set the flag that indicates registration is closed (because run has now been invoked),
    // which will disallow any further invocations of "test" or "ignore" with
    // an RegistrationClosedException.    
    val oldBundle = atomic.get
    val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
    if (!registrationClosed)
      updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, true))

    val report = wrapReporterIfNecessary(theSuite, reporter)

    val informerForThisSuite =
      ConcurrentInformer(
        (message, payload, isConstructingThread, location) => {
          reportInfoProvided(theSuite, report, tracker, None, message, payload, 1, location, isConstructingThread)
        }
      )

    atomicInformer.set(informerForThisSuite)

    val updaterForThisSuite =
      ConcurrentNotifier(
        (message, payload, isConstructingThread, location) => {
          reportNoteProvided(theSuite, report, tracker, None, message, payload, 1, location, isConstructingThread)
        }
      )

    atomicNotifier.set(updaterForThisSuite)

    val alerterForThisSuite =
      ConcurrentAlerter(
        (message, payload, isConstructingThread, location) => {
          reportAlertProvided(theSuite, report, tracker, None, message, payload, 1, location, isConstructingThread)
        }
      )

    atomicAlerter.set(alerterForThisSuite)

    val documenterForThisSuite =
      ConcurrentDocumenter(
        (message, payload, isConstructingThread, location) => {
          reportMarkupProvided(theSuite, report, tracker, None, message, 1, location, isConstructingThread)
        }
      )

    atomicDocumenter.set(documenterForThisSuite)

    val status = superRun(testName, args.copy(reporter = report))

    // SKIP-SCALATESTJS,NATIVE-START
    status.whenCompleted { r =>
      val shouldBeInformerForThisSuite = atomicInformer.getAndSet(zombieInformer)
      if (shouldBeInformerForThisSuite ne informerForThisSuite)
        throw new ConcurrentModificationException(Resources.concurrentInformerMod(theSuite.getClass.getName))

      val shouldBeNotifierForThisSuite = atomicNotifier.getAndSet(zombieNotifier)
      if (shouldBeNotifierForThisSuite ne updaterForThisSuite)
        throw new ConcurrentModificationException(Resources.concurrentNotifierMod(theSuite.getClass.getName))

      val shouldBeAlerterForThisSuite = atomicAlerter.getAndSet(zombieAlerter)
      if (shouldBeAlerterForThisSuite ne alerterForThisSuite)
        throw new ConcurrentModificationException(Resources.concurrentAlerterMod(theSuite.getClass.getName))

      val shouldBeDocumenterForThisSuite = atomicDocumenter.getAndSet(zombieDocumenter)
      if (shouldBeDocumenterForThisSuite ne documenterForThisSuite)
        throw new ConcurrentModificationException(Resources.concurrentDocumenterMod(theSuite.getClass.getName))
    }
    // SKIP-SCALATESTJS,NATIVE-END

    status
  }

  /*
  def describeImpl(description: String, fun: => Unit, registrationClosedResource: String, sourceFile: String, methodName: String) {

    val oldBundle = atomic.get
    val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    if (registrationClosed)
      throw new TestRegistrationClosedException(Resources(registrationClosedResource), getStackDepth(sourceFile, methodName))

    val oldBranch = currentBranch
    val newBranch = DescriptionBranch(currentBranch, description, None)
    oldBranch.subNodes ::= newBranch

    // Update atomic, making the current branch to the new branch
    updateAtomic(oldBundle, Bundle(newBranch, testNamesList, testsMap, tagsMap, registrationClosed))

    fun // Execute the function

    { // Put the old branch back as the current branch
      val oldBundle = atomic.get
      val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      updateAtomic(oldBundle, Bundle(oldBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    }
  } */

  def registerNestedBranch(description: String, childPrefix: Option[String], fun: => Unit, registrationClosedMessageFun: => String, location: Option[Location], pos: source.Position): Unit = {

    val oldBundle = atomic.get
    val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    if (registrationClosed)
      throw new TestRegistrationClosedException(registrationClosedMessageFun, pos)

    val branchLocation = 
      location match {
        case Some(loc) => Some(loc)
        case None => Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname)))
      }
    
    val oldBranch = currentBranch
    val newBranch = DescriptionBranch(currentBranch, description, childPrefix, branchLocation)

    // Update atomic, making the current branch to the new branch
    updateAtomic(oldBundle, Bundle(newBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    oldBranch.subNodes ::= newBranch
    
    try {
      fun // Execute the function
    }
    catch {
      case e: TestPendingException =>
        newBranch.pending = true
    }
      
    { // Put the old branch back as the current branch
      val oldBundle = atomic.get
      val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      updateAtomic(oldBundle, Bundle(oldBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    }
  }

  // Used by FlatSpec, which doesn't nest. So this one just makes a new one off of the trunk
  def registerFlatBranch(description: String, registrationClosedMessageFun: => String, pos: source.Position): Unit = {

    val oldBundle = atomic.get
    val (_, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    if (registrationClosed)
      throw new TestRegistrationClosedException(registrationClosedMessageFun, pos)

    // Need to use Trunk here. I think it will be visible to all threads because
    // of the atomic, even though it wasn't inside it.
    val newBranch = DescriptionBranch(Trunk, description, None, Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname))))
    Trunk.subNodes ::= newBranch

    // Update atomic, making the current branch to the new branch
    updateAtomic(oldBundle, Bundle(newBranch, testNamesList, testsMap, tagsMap, registrationClosed))
  }

  def currentBranchIsTrunk: Boolean = {

    val oldBundle = atomic.get
    var (currentBranch, _, _, _, _) = oldBundle.unpack
    currentBranch == Trunk
  }

  // Path traits need to register the message recording informer, so it can fire any info events later
  def registerAsyncTest(testText: String, testFun: T, testRegistrationClosedMessageFun: => String, duration: Option[Long], location: Option[Location], pos: source.Position, testTags: Tag*): String = { // returns testName

    checkRegisterTestParamsForNull(testText, testTags: _*)

    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(testRegistrationClosedMessageFun, pos)

    val oldBundle = atomic.get
    var (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    val testName = getTestName(testText, currentBranch)

    if (atomic.get.testsMap.keySet.contains(testName))
      throw new DuplicateTestNameException(testName, pos)
    val testLocation = 
      location match {
        case Some(loc) => Some(loc)
        case None => Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname)))
      }

    val testLeaf = TestLeaf(currentBranch, testName, testText, testFun, testLocation, Some(pos), duration)
    testsMap += (testName -> testLeaf)
    testNamesList = testNamesList :+ testName
    currentBranch.subNodes ::= testLeaf

    val tagNames = Set[String]() ++ testTags.map(_.name)
    if (!tagNames.isEmpty)
      tagsMap += (testName -> tagNames)

    updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, registrationClosed))

    testName
  }

  def registerIgnoredAsyncTest(testText: String, f: T, testRegistrationClosedMessageFun: => String, location: Option[Location], pos: source.Position, testTags: Tag*): Unit = {

    checkRegisterTestParamsForNull(testText, testTags: _*)

// If this works delete this. I think we can rely on registerAsyncTest's check
//    if (atomic.get.registrationClosed)
//      throw new TestRegistrationClosedException(Resources.ignoreCannotAppearInsideATest, getStackDepth(sourceFileName, "ignore"))

    val testName = registerAsyncTest(testText, f, testRegistrationClosedMessageFun, None, location, pos) // Call test without passing the tags

    val oldBundle = atomic.get
    var (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    val tagNames = Set[String]() ++ testTags.map(_.name)
    tagsMap += (testName -> (tagNames + IgnoreTagName))

    updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, registrationClosed))
  }

  private[scalatest] def getTestNamePrefix(branch: Branch): String =
    branch match {
      case Trunk => ""
      // Call to getTestNamePrefix is not tail recursive, but I don't expect
      // the describe nesting to be very deep (famous last words).
      case DescriptionBranch(parent, descriptionText, childPrefix, lineInFile) =>
        val optionalChildPrefixAndDescriptionText =
          childPrefix match {
            case Some(cp) => Resources.prefixSuffix(descriptionText, cp)
            case _ => descriptionText
          }
        Resources.prefixSuffix(getTestNamePrefix(parent), optionalChildPrefixAndDescriptionText.trim).trim
    }

  private[scalatest] def getTestName(testText: String, parent: Branch): String =
    Resources.prefixSuffix(getTestNamePrefix(parent), testText.trim).trim

  private def checkRegisterTestParamsForNull(testText: String, testTags: Tag*): Unit = {
    requireNonNull(testText)
    if (testTags.exists(_ == null))
      throw new NullArgumentException("a test tag was null")
  }
  
  private[scalatest] def testPath(testName: String): List[Int] = {
    val theTestOpt = atomic.get.testsMap.get(testName)
    theTestOpt match {
      case Some(theTest) =>
        findPath(theTest.parent, theTest, List.empty)
      case None => 
        throw new IllegalArgumentException("Test name '" + testName + "' not found.")
    }
  }
 
  @tailrec
  private def findPath(branch: Branch, node: Node, currentPath: List[Int]): List[Int] = {
    val idx = branch.subNodes.reverse.indexOf(node)
    branch.parentOption match {
      case Some(parent) => 
        findPath(parent, branch, idx :: currentPath)
      case None => 
        idx :: currentPath
    }
  }
  
  private[scalatest] def createTestDataFor(testName: String, theConfigMap: ConfigMap, theSuite: Suite) = {
    val theTest = atomic.get.testsMap(testName)
    new TestData {
      val configMap = theConfigMap
      val name = testName
      val scopes = testScopes(testName)
      val text = testText(testName)
      val tags = testTags(testName, theSuite)
      val pos = theTest.pos
    }
  }
  
  private[scalatest] def testTags(testName: String, theSuite: Suite): Set[String] = {
    // SKIP-SCALATESTJS,NATIVE-START
    val suiteTags = for { 
      a <- theSuite.getClass.getAnnotations
      annotationClass = a.annotationType
      if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
    } yield annotationClass.getName
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val suiteTags = Set.empty[String]
    val testTagSet = atomic.get.tagsMap.getOrElse(testName, Set.empty)
    Set.empty ++ suiteTags ++ testTagSet
  }
  
  private[scalatest] def testScopes(testName: String): collection.immutable.IndexedSeq[String] = {
    @tailrec
    def testScopesAcc(branch: Branch, acc: collection.immutable.IndexedSeq[String]): collection.immutable.IndexedSeq[String] = {
      branch match {
        case Trunk => acc.reverse
        case DescriptionBranch(parent, descriptionText, childPrefix, lineInFile) =>
          val optionalChildPrefixAndDescriptionText =
          childPrefix match {
            case Some(cp) => Resources.prefixSuffix(descriptionText.trim, cp.trim)
            case _ => descriptionText
          }
          testScopesAcc(parent, acc :+ optionalChildPrefixAndDescriptionText.trim)
      }
    }
    val theTestOpt = atomic.get.testsMap.get(testName)
    theTestOpt match {
      case Some(theTest) =>
        testScopesAcc(theTest.parent, collection.immutable.IndexedSeq.empty)
      case None =>
        throw new IllegalArgumentException("Test name '" + testName + "' not found.")
    }
  }
  
  private[scalatest] def testText(testName: String): String = {
    val theTestOpt = atomic.get.testsMap.get(testName)
    theTestOpt match {
      case Some(theTest) =>
        theTest.testText
      case None => 
        throw new IllegalArgumentException("Test name '" + testName + "' not found.")
    }
  }
}

private[scalatest] class AsyncEngine(concurrentBundleModMessageFun: => String, simpleClassName: String)
    extends AsyncSuperEngine[() => AsyncTestHolder](concurrentBundleModMessageFun, simpleClassName)

private[scalatest] class AsyncFixtureEngine[FixtureParam](concurrentBundleModMessageFun: => String, simpleClassName: String)
    extends AsyncSuperEngine[FixtureParam => AsyncTestHolder](concurrentBundleModMessageFun, simpleClassName)

