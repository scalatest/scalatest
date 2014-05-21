/*
 * Copyright 2001-2013 Artima, Inc.
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

import exceptions.TestCanceledException
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import FunSuite.IgnoreTagName
import org.scalatest.Suite._
import org.scalatest.events.LineInFile
import org.scalatest.events.SeeStackDepthException
import scala.annotation.tailrec
import org.scalatest.PathEngine.isInTargetPath
import org.scalatest.Suite.checkChosenStyles
import org.scalatest.events.Event
import org.scalatest.events.Location
import collection.mutable.ListBuffer

// T will be () => Unit for FunSuite and FixtureParam => Any for fixture.FunSuite
private[scalatest] sealed abstract class SuperEngine[T](concurrentBundleModResourceName: String, simpleClassName: String)  {

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

  // Path traits need to register a function that returns a MessageRecordingInformer, because its tests are ru
  // at construction time when these five args aren't available.
  // type RecorderFun = (Suite, Reporter, Tracker, String, TestLeaf, Boolean) => MessageRecordingInformer2

  case class TestLeaf(
    parent: Branch,
    testName: String, // The full test name
    testText: String, // The last portion of the test name that showed up on an inner most nested level
    testFun: T, 
    location: Option[Location],
    recordedDuration: Option[Long] = None,
    recordedMessages: Option[PathMessageRecordingInformer] = None
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

  def updateAtomic(oldBundle: Bundle, newBundle: Bundle) {
    val shouldBeOldBundle = atomic.getAndSet(newBundle)
    if (!(shouldBeOldBundle eq oldBundle))
      throw new ConcurrentModificationException(Resources(concurrentBundleModResourceName))
  }

  class RegistrationInformer extends Informer {

    def apply(message: String, payload: Option[Any] = None) {
      if (message == null)
        throw new NullPointerException
      if (payload == null)
        throw new NullPointerException
      val oldBundle = atomic.get
      var (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      currentBranch.subNodes ::= InfoLeaf(currentBranch, message, payload, getLineInFile(Thread.currentThread().getStackTrace, 2))
      updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    }
  }

  class RegistrationNotifier extends Notifier {

    def apply(message: String, payload: Option[Any] = None) {
      if (message == null)
        throw new NullPointerException
      if (payload == null)
        throw new NullPointerException
      val oldBundle = atomic.get
      var (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      currentBranch.subNodes ::= NoteLeaf(currentBranch, message, payload, getLineInFile(Thread.currentThread().getStackTrace, 2))
      updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    }
  }

  class RegistrationAlerter extends Alerter {

    def apply(message: String, payload: Option[Any] = None) {
      if (message == null)
        throw new NullPointerException
      if (payload == null)
        throw new NullPointerException
      val oldBundle = atomic.get
      var (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      currentBranch.subNodes ::= AlertLeaf(currentBranch, message, payload, getLineInFile(Thread.currentThread().getStackTrace, 2))
      updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    }
  }

  class RegistrationDocumenter extends Documenter {
    def apply(message: String) {
      if (message == null)
        throw new NullPointerException
      val oldBundle = atomic.get
      var (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      currentBranch.subNodes ::= MarkupLeaf(currentBranch, message, getLineInFile(Thread.currentThread().getStackTrace, 2))
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
      def apply(message: String, payload: Option[Any] = None) {
        if (message == null)
          throw new NullPointerException
        if (payload == null)
          throw new NullPointerException
        println(Resources("infoProvided", message))
        payload match {
          case Some(p) => println(Resources("payloadToString", payload.get.toString))
          case _ => 
        }
      }
    }

  final val zombieNotifier =
    new Notifier {
      def apply(message: String, payload: Option[Any] = None) {
        if (message == null)
          throw new NullPointerException
        if (payload == null)
          throw new NullPointerException
        println(Resources("noteProvided", message))
        payload match {
          case Some(p) => println(Resources("payloadToString", payload.get.toString))
          case _ => 
        }
      }
    }

  final val zombieAlerter =
    new Alerter {
      def apply(message: String, payload: Option[Any] = None) {
        if (message == null)
          throw new NullPointerException
        if (payload == null)
          throw new NullPointerException
        println(Resources("alertProvided", message))
        payload match {
          case Some(p) => println(Resources("payloadToString", payload.get.toString))
          case _ => 
        }
      }
    }

  final val zombieDocumenter =
    new Documenter {
      def apply(message: String) {
        if (message == null)
          throw new NullPointerException
        println(Resources("markupProvided", message))
      }
    }

  private def checkTestOrIgnoreParamsForNull(testName: String, testTags: Tag*) {
    if (testName == null)
      throw new NullPointerException("testName was null")
    if (testTags.exists(_ == null))
      throw new NullPointerException("a test tag was null")
  }

  def runTestImpl(
    theSuite: Suite,
    testName: String,
    args: Args,
    includeIcon: Boolean,
    invokeWithFixture: TestLeaf => Outcome
  ): Status = {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")
    
    import args._

    val (stopRequested, report, testStartTime) =
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

    try {

      invokeWithFixture(theTest).toUnit

      val duration = System.currentTimeMillis - testStartTime
      val durationToReport = theTest.recordedDuration.getOrElse(duration)
      val recordEvents = messageRecorderForThisTest.recordedEvents(false, false) ++ 
                         (if (theTest.recordedMessages.isDefined) 
                            theTest.recordedMessages.get.recordedEvents(false, theSuite, report, tracker, testName, theTest.indentationLevel + 1, includeIcon)
                          else
                            Vector.empty)
      reportTestSucceeded(theSuite, report, tracker, testName, theTest.testText, recordEvents, durationToReport, formatter, theSuite.rerunner, theTest.location)
      SucceededStatus
    }
    catch {
      case _: TestPendingException =>
        val duration = System.currentTimeMillis - testStartTime
        // testWasPending = true so info's printed out in the finally clause show up yellow
        val recordEvents = messageRecorderForThisTest.recordedEvents(true, false) ++ 
                           (if (theTest.recordedMessages.isDefined) 
                             theTest.recordedMessages.get.recordedEvents(true, theSuite, report, tracker, testName, theTest.indentationLevel + 1, includeIcon)
                           else
                             Vector.empty)
        reportTestPending(theSuite, report, tracker, testName, theTest.testText, recordEvents, duration, formatter, theTest.location)
        SucceededStatus
      case e: TestCanceledException =>
        val duration = System.currentTimeMillis - testStartTime
        // testWasCanceled = true so info's printed out in the finally clause show up yellow
        val recordEvents = messageRecorderForThisTest.recordedEvents(false, true) ++ 
                           (if (theTest.recordedMessages.isDefined) 
                             theTest.recordedMessages.get.recordedEvents(false, theSuite, report, tracker, testName, theTest.indentationLevel + 1, includeIcon)
                           else
                             Vector.empty)
        reportTestCanceled(theSuite, report, e, testName, theTest.testText, recordEvents, theSuite.rerunner, tracker, duration, formatter, theTest.location)
        SucceededStatus
      case e if !anExceptionThatShouldCauseAnAbort(e) =>
        val duration = System.currentTimeMillis - testStartTime
        val durationToReport = theTest.recordedDuration.getOrElse(duration)
        val recordEvents = messageRecorderForThisTest.recordedEvents(false, false) ++ 
                           (if (theTest.recordedMessages.isDefined)
                             theTest.recordedMessages.get.recordedEvents(false, theSuite, report, tracker, testName, theTest.indentationLevel + 1, includeIcon)
                           else
                             Vector.empty)
        reportTestFailed(theSuite, report, e, testName, theTest.testText, recordEvents, theSuite.rerunner, tracker, durationToReport, formatter,  Some(SeeStackDepthException))
        FailedStatus
      case e: Throwable => throw e
    }
    finally {
      val shouldBeInformerForThisTest = atomicInformer.getAndSet(oldInformer)
      if (shouldBeInformerForThisTest ne informerForThisTest)
        throw new ConcurrentModificationException(Resources("concurrentInformerMod", theSuite.getClass.getName))

      val shouldBeNotifierForThisTest = atomicNotifier.getAndSet(oldNotifier)
      if (shouldBeNotifierForThisTest ne updaterForThisTest)
        throw new ConcurrentModificationException(Resources("concurrentNotifierMod", theSuite.getClass.getName))

      val shouldBeAlerterForThisTest = atomicAlerter.getAndSet(oldAlerter)
      if (shouldBeAlerterForThisTest ne alerterForThisTest)
        throw new ConcurrentModificationException(Resources("concurrentAlerterMod", theSuite.getClass.getName))

      val shouldBeDocumenterForThisTest = atomicDocumenter.getAndSet(oldDocumenter)
      if (shouldBeDocumenterForThisTest ne documenterForThisTest)
        throw new ConcurrentModificationException(Resources("concurrentDocumenterMod", theSuite.getClass.getName))
    }
  }

  private def runTestsInBranch(
    theSuite: Suite,
    branch: Branch,
    args: Args,
    includeIcon: Boolean,
    runTest: (String, Args) => Status
  ): Status = {

    val stopRequested = args.stopper
    
    // TODO: Inspect this and make sure it does not need synchronization, and either way, document why.
    val statusList = new ListBuffer[Status]()

    branch match {

      case desc @ DescriptionBranch(parent, descriptionText, _, lineInFile) =>

        val descriptionTextWithOptionalPrefix = prependChildPrefix(parent, descriptionText)
        val indentationLevel = desc.indentationLevel
        reportScopeOpened(theSuite, args.reporter, args.tracker, descriptionTextWithOptionalPrefix, indentationLevel, false, lineInFile)
        traverseSubNodes()
        if (desc.pending) 
          reportScopePending(theSuite, args.reporter, args.tracker, descriptionTextWithOptionalPrefix, indentationLevel, false, lineInFile)
        else 
          reportScopeClosed(theSuite, args.reporter, args.tracker, descriptionTextWithOptionalPrefix, indentationLevel, false, lineInFile)
      case Trunk =>
        traverseSubNodes()
    }


    def traverseSubNodes() {
      branch.subNodes.reverse.foreach { node =>
        if (!stopRequested()) {
          node match {
            case testLeaf @ TestLeaf(_, testName, testText, _, _, _, _) =>
              val (filterTest, ignoreTest) = args.filter(testName, theSuite.tags, theSuite.suiteId)
              if (!filterTest)
                if (ignoreTest) {
                  val testTextWithOptionalPrefix = prependChildPrefix(branch, testText)
                  val theTest = atomic.get.testsMap(testName)
                  reportTestIgnored(theSuite, args.reporter, args.tracker, testName, testTextWithOptionalPrefix, getIndentedTextForTest(testTextWithOptionalPrefix, testLeaf.indentationLevel, true), theTest.location)
                }
                else
                  statusList += runTest(testName, args)

            case infoLeaf @ InfoLeaf(_, message, payload, location) =>
              reportInfoProvided(theSuite, args.reporter, args.tracker, None, message, payload, infoLeaf.indentationLevel, location, true, includeIcon)

            case noteLeaf @ NoteLeaf(_, message, payload, location) =>
              reportNoteProvided(theSuite, args.reporter, args.tracker, None, message, payload, noteLeaf.indentationLevel, location, true, includeIcon)

            case alertLeaf @ AlertLeaf(_, message, payload, location) =>
              reportAlertProvided(theSuite, args.reporter, args.tracker, None, message, payload, alertLeaf.indentationLevel, location, true, includeIcon)

            case markupLeaf @ MarkupLeaf(_, message, location) =>
              reportMarkupProvided(theSuite, args.reporter, args.tracker, None, message, markupLeaf.indentationLevel, location, true, includeIcon)

            case branch: Branch => statusList += runTestsInBranch(theSuite, branch, args, includeIcon, runTest)
          }
        }
      }
    }
    new CompositeStatus(Set.empty ++ statusList)
  }

  def prependChildPrefix(branch: Branch, testText: String): String =
    branch match {
      case DescriptionBranch(_, _, Some(cp), _) => Resources("prefixSuffix", cp, testText)
      case _ => testText
    }

  def runTestsImpl(
    theSuite: Suite,
    testName: Option[String],
    args: Args,
    info: Informer,
    includeIcon: Boolean,
    runTest: (String, Args) => Status
  ): Status = {
    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    if (theSuite.testNames.size > 0)
      checkChosenStyles(configMap, theSuite.styleName)

    val stopRequested = stopper

    // Wrap any non-DispatchReporter, non-CatchReporter in a CatchReporter,
    // so that exceptions are caught and transformed
    // into error messages on the standard error stream.
    val report = Suite.wrapReporterIfNecessary(theSuite, reporter)
    val newArgs = if (report eq reporter) args else args.copy(reporter = report)
    
    val statusBuffer = new ListBuffer[Status]()

    // If a testName is passed to run, just run that, else run the tests returned
    // by testNames.
    testName match {
      case Some(tn) =>
        val (filterTest, ignoreTest) = filter(tn, theSuite.tags, theSuite.suiteId)
        if (!filterTest) {
          if (ignoreTest) {
            val theTest = atomic.get.testsMap(tn)
            reportTestIgnored(theSuite, report, tracker, tn, tn, getIndentedTextForTest(tn, 1, true), theTest.location)
          }
          else {
            statusBuffer += runTest(tn, newArgs)
          }
        }
      case None => statusBuffer += runTestsInBranch(theSuite, Trunk, newArgs, includeIcon, runTest)
    }
    new CompositeStatus(Set.empty ++ statusBuffer)
  }

  def runImpl(
    theSuite: Suite,
    testName: Option[String],
    args: Args,
    superRun: (Option[String], Args) => Status
  ): Status = {
    import args._

    val stopRequested = stopper

    // Set the flag that indicates registration is closed (because run has now been invoked),
    // which will disallow any further invocations of "test" or "ignore" with
    // an RegistrationClosedException.    
    val oldBundle = atomic.get
    val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
    if (!registrationClosed)
      updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, true))

    val report = Suite.wrapReporterIfNecessary(theSuite, reporter)

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

    try {
      superRun(testName, args.copy(reporter = report))
    }
    finally {
      val shouldBeInformerForThisSuite = atomicInformer.getAndSet(zombieInformer)
      if (shouldBeInformerForThisSuite ne informerForThisSuite)
        throw new ConcurrentModificationException(Resources("concurrentInformerMod", theSuite.getClass.getName))

      val shouldBeNotifierForThisSuite = atomicNotifier.getAndSet(zombieNotifier)
      if (shouldBeNotifierForThisSuite ne updaterForThisSuite)
        throw new ConcurrentModificationException(Resources("concurrentNotifierMod", theSuite.getClass.getName))

      val shouldBeAlerterForThisSuite = atomicAlerter.getAndSet(zombieAlerter)
      if (shouldBeAlerterForThisSuite ne alerterForThisSuite)
        throw new ConcurrentModificationException(Resources("concurrentAlerterMod", theSuite.getClass.getName))

      val shouldBeDocumenterForThisSuite = atomicDocumenter.getAndSet(zombieDocumenter)
      if (shouldBeDocumenterForThisSuite ne documenterForThisSuite)
        throw new ConcurrentModificationException(Resources("concurrentDocumenterMod", theSuite.getClass.getName))
    }
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

  def registerNestedBranch(description: String, childPrefix: Option[String], fun: => Unit, registrationClosedResource: String, sourceFile: String, methodName: String, stackDepth: Int, adjustment: Int, location: Option[Location]) {

    val oldBundle = atomic.get
    val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    if (registrationClosed)
      throw new TestRegistrationClosedException(Resources(registrationClosedResource), getStackDepthFun(sourceFile, methodName, stackDepth + adjustment))

    val branchLocation = 
      location match {
        case Some(loc) => Some(loc)
        case None => getLineInFile(Thread.currentThread().getStackTrace, stackDepth)
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
      case e: exceptions.TestPendingException =>
        newBranch.pending = true
    }
      
    { // Put the old branch back as the current branch
      val oldBundle = atomic.get
      val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      updateAtomic(oldBundle, Bundle(oldBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    }
  }

  // Used by FlatSpec, which doesn't nest. So this one just makes a new one off of the trunk
  def registerFlatBranch(description: String, registrationClosedResource: String, sourceFile: String, methodName: String, stackDepth: Int, adjustment: Int) {

    val oldBundle = atomic.get
    val (_, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    if (registrationClosed)
      throw new TestRegistrationClosedException(Resources(registrationClosedResource), getStackDepthFun(sourceFile, methodName, stackDepth + adjustment))

    // Need to use Trunk here. I think it will be visible to all threads because
    // of the atomic, even though it wasn't inside it.
    val newBranch = DescriptionBranch(Trunk, description, None, getLineInFile(Thread.currentThread().getStackTrace, stackDepth))
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
  def registerTest(testText: String, testFun: T, testRegistrationClosedResourceName: String, sourceFileName: String, methodName: String, stackDepth: Int, adjustment: Int, duration: Option[Long], location: Option[Location], informer: Option[PathMessageRecordingInformer], testTags: Tag*): String = { // returns testName

    checkRegisterTestParamsForNull(testText, testTags: _*)

    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(Resources(testRegistrationClosedResourceName), getStackDepthFun(sourceFileName, methodName, stackDepth + adjustment))
//    throw new TestRegistrationClosedException(Resources("testCannotAppearInsideAnotherTest"), getStackDepth(sourceFileName, "test"))

    val oldBundle = atomic.get
    var (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    val testName = getTestName(testText, currentBranch)

    if (atomic.get.testsMap.keySet.contains(testName))
      throw new DuplicateTestNameException(testName, getStackDepthFun(sourceFileName, methodName, stackDepth + adjustment))
    
    val testLocation = 
      location match {
        case Some(loc) => Some(loc)
        case None => getLineInFile(Thread.currentThread().getStackTrace, stackDepth)
      }

    val testLeaf = TestLeaf(currentBranch, testName, testText, testFun, testLocation, duration, informer)
    testsMap += (testName -> testLeaf)
    testNamesList ::= testName
    currentBranch.subNodes ::= testLeaf

    val tagNames = Set[String]() ++ testTags.map(_.name)
    if (!tagNames.isEmpty)
      tagsMap += (testName -> tagNames)

    updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, registrationClosed))

    testName
  }

  def registerIgnoredTest(testText: String, f: T, testRegistrationClosedResourceName: String, sourceFileName: String, methodName: String, stackDepth: Int, adjustment: Int, location: Option[Location], testTags: Tag*) {

    checkRegisterTestParamsForNull(testText, testTags: _*)

// If this works delete this. I think we can rely on registerTest's check
//    if (atomic.get.registrationClosed)
//      throw new TestRegistrationClosedException(Resources("ignoreCannotAppearInsideATest"), getStackDepth(sourceFileName, "ignore"))

    val testName = registerTest(testText, f, testRegistrationClosedResourceName, sourceFileName, methodName, stackDepth + 1, adjustment, None, location, None) // Call test without passing the tags

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
            case Some(cp) => Resources("prefixSuffix", descriptionText, cp)
            case _ => descriptionText
          }
        Resources("prefixSuffix", getTestNamePrefix(parent), optionalChildPrefixAndDescriptionText.trim).trim
    }

  private[scalatest] def getTestName(testText: String, parent: Branch): String =
    Resources("prefixSuffix", getTestNamePrefix(parent), testText.trim).trim

  private def checkRegisterTestParamsForNull(testText: String, testTags: Tag*) {
    if (testText == null)
      throw new NullPointerException("testText was null")
    if (testTags.exists(_ == null))
      throw new NullPointerException("a test tag was null")
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
  
  private[scalatest] def createTestDataFor(testName: String, theConfigMap: ConfigMap, theSuite: Suite) = 
    new TestData {
      val configMap = theConfigMap 
      val name = testName
      val scopes = testScopes(testName)
      val text = testText(testName)
      val tags = testTags(testName, theSuite)
    }
  
  private[scalatest] def testTags(testName: String, theSuite: Suite): Set[String] = {
    val suiteTags = for { 
      a <- theSuite.getClass.getAnnotations
      annotationClass = a.annotationType
      if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
    } yield annotationClass.getName
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
            case Some(cp) => Resources("prefixSuffix", descriptionText.trim, cp.trim)
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

private[scalatest] class Engine(concurrentBundleModResourceName: String, simpleClassName: String)
    extends SuperEngine[() => Outcome](concurrentBundleModResourceName, simpleClassName)

private[scalatest] class FixtureEngine[FixtureParam](concurrentBundleModResourceName: String, simpleClassName: String)
    extends SuperEngine[FixtureParam => Outcome](concurrentBundleModResourceName, simpleClassName)



private[scalatest] class PathEngine(concurrentBundleModResourceName: String, simpleClassName: String)
    extends Engine(concurrentBundleModResourceName, simpleClassName) { thisEngine =>
 
  /*
  Anything that the initial instance sets will need to be made volatile or atomic.
  But stuff used only by ensure probably does not, because ..., well ensure gets called
  manh times. So things used by ensure to determine whether or not ensure already did its
  job must be visible to all threads. But anything else that's used only during that ensure
  call could be moved inside the method.
   */
  // registeredPathSet can be changed after construction, so must be volatile.
  @volatile private var registeredPathSet = Set.empty[List[Int]]

  // Target path can be read and changed by the ensure thread
  @volatile private var targetPath: Option[List[Int]] = None

  // A describe clause registered no tests
  @volatile private var describeRegisteredNoTests: Boolean = false
  @volatile private var insideAPathTest: Boolean = false
  @volatile private var currentPath = List.empty[Int]
  @volatile private var usedPathSet = Set.empty[String]
  // Used in each instance to track the paths of things encountered, so can figure out
  // the next path. Each instance must use their own copies of currentPath and usedPathSet.
  def getNextPath() = {
    var next: List[Int] = null
    var count = 0
    while (next == null) {
      val candidate = currentPath ::: List(count)
      if (!usedPathSet.contains(candidate.toList.toString)) {
        next = candidate
        usedPathSet += candidate.toList.toString
      }
      else
        count += 1
    }
    next
  }

  /*
  ensureTestRes method could be called by a different thread than the one that
  initially constructed the initial instance.
   */
  // Once the target leaf has been reached for an instance, targetLeafHasBeenReached
  // will be set to true. And because of that, the path of the next describe or it encountered will
  // be placed into nextTargetPath. If no other describe or it clause comes along, then nextTargetPath
  // will stay at None, and the while loop will stop.
  @volatile private var targetLeafHasBeenReached = false
  @volatile private var nextTargetPath: Option[List[Int]] = None
  @volatile private var testResultsRegistered = false
  def ensureTestResultsRegistered(callingInstance: Suite with OneInstancePerTest) {
    synchronized {
      val isAnInitialInstance = targetPath.isEmpty
      // Only register tests if this is an initial instance (and only if they haven't
      // already been registered).
      if (isAnInitialInstance  && !testResultsRegistered) {
        testResultsRegistered = true
        var currentInstance: Suite = callingInstance
        while (nextTargetPath.isDefined) {
          targetPath = Some(nextTargetPath.get)
          PathEngine.setEngine(thisEngine)
          currentPath = List.empty[Int]
          usedPathSet = Set.empty[String]
          targetLeafHasBeenReached = false
          nextTargetPath = None
          // testResultsRegistered = false 
          currentInstance = callingInstance.newInstance  
        }
      }
    }
  }
/*
 * (theSuite, report, tracker, testName, theTest, includeIcon) => MessageRecordingInformer2
 * 
 * 
 */
  def handleTest(handlingSuite: Suite, testText: String, testFun: () => Outcome, testRegistrationClosedResourceName: String, sourceFileName: String, methodName: String, stackDepth: Int, adjustment: Int, location: Option[Location], testTags: Tag*) {

    if (insideAPathTest) 
      throw new TestRegistrationClosedException(Resources(testRegistrationClosedResourceName), getStackDepthFun(sourceFileName, methodName, stackDepth + adjustment))
    
    insideAPathTest = true
    
    try {
      describeRegisteredNoTests = false
      val nextPath = getNextPath()
      if (isInTargetPath(nextPath, targetPath)) {
        // Default value of None indicates successful test
          //theTest.indentationLevel + 1
        val informerForThisTest =
          PathMessageRecordingInformer( // TODO: Put locations into path traits!
            (message, payload, wasConstructingThread, testWasPending, theSuite, report, tracker, testName, indentation, includeIcon, thread) =>
              createInfoProvided(theSuite, report, tracker, Some(testName), message, payload, indentation, None, wasConstructingThread, includeIcon)
          )

        val oldInformer = atomicInformer.getAndSet(informerForThisTest)

        val updaterForThisTest =
          PathMessageRecordingNotifier( // TODO: Put locations into path traits!
            (message, payload, wasConstructingThread, testWasPending, theSuite, report, tracker, testName, indentation, includeIcon, thread) =>
              createNoteProvided(theSuite, report, tracker, Some(testName), message, payload, indentation, None, wasConstructingThread, includeIcon)
          )

        val oldNotifier = atomicNotifier.getAndSet(updaterForThisTest)

        val alerterForThisTest =
          PathMessageRecordingAlerter( // TODO: Put locations into path traits!
            (message, payload, wasConstructingThread, testWasPending, theSuite, report, tracker, testName, indentation, includeIcon, thread) =>
              createAlertProvided(theSuite, report, tracker, Some(testName), message, payload, indentation, None, wasConstructingThread, includeIcon)
          )

        val oldAlerter = atomicAlerter.getAndSet(alerterForThisTest)

        val documenterForThisTest =
          PathMessageRecordingDocumenter( // TODO: Put locations into path traits!
            (message, wasConstructingThread, testWasPending, theSuite, report, tracker, testName, indentation, includeIcon, thread) =>
              createMarkupProvided(theSuite, report, tracker, Some(testName), message, indentation, None, wasConstructingThread, includeIcon)
          )

        val oldDocumenter = atomicDocumenter.getAndSet(documenterForThisTest)

        var durationOfRunningTest: Long = -1
        var startTime: Long = System.currentTimeMillis
        val outcome = 
          try { // TODO: Correctly record the time a test takes and report that
            // I think I need to replace the Informer with one that records the message and whether the
            // thread was this thread, and then...
            testFun()
            // If no exception, leave at None to indicate success
          }
          finally {
            durationOfRunningTest = System.currentTimeMillis - startTime
            val shouldBeInformerForThisTest = atomicInformer.getAndSet(oldInformer)
            if (shouldBeInformerForThisTest ne informerForThisTest)
              throw new ConcurrentModificationException(Resources("concurrentInformerMod", handlingSuite.getClass.getName))

            val shouldBeNotifierForThisTest = atomicNotifier.getAndSet(oldNotifier)
            if (shouldBeNotifierForThisTest ne updaterForThisTest)
              throw new ConcurrentModificationException(Resources("concurrentNotifierMod", handlingSuite.getClass.getName))

            val shouldBeAlerterForThisTest = atomicAlerter.getAndSet(oldAlerter)
            if (shouldBeAlerterForThisTest ne alerterForThisTest)
              throw new ConcurrentModificationException(Resources("concurrentAlerterMod", handlingSuite.getClass.getName))

            val shouldBeDocumenterForThisTest = atomicDocumenter.getAndSet(oldDocumenter)
            if (shouldBeDocumenterForThisTest ne documenterForThisTest)
              throw new ConcurrentModificationException(Resources("concurrentDocumenterMod", handlingSuite.getClass.getName))
          }

        // Here in the test function, replay those info calls. But I can't do this from different threads is the issue. Unless
        // I downcast to MessageRecordingInformer, and have another apply method on it that takes the true/false. Or override
        // runTestImpl and do something different. How about registering a different kind of test. EagerTest. Then it has
        // yes, that's how.
        val newTestFun = { () => outcome }
        // register with         informerForThisTest.fireRecordedMessages(testWasPending)

        registerTest(testText, newTestFun, testRegistrationClosedResourceName, sourceFileName, methodName, stackDepth + 1, adjustment, Some(durationOfRunningTest), location, Some(informerForThisTest), testTags: _*)
        targetLeafHasBeenReached = true
      }
      else if (targetLeafHasBeenReached && nextTargetPath.isEmpty) {
        nextTargetPath = Some(nextPath)
      }
    }
    finally {
      insideAPathTest = false
    }
  }

  def handleNestedBranch(description: String, childPrefix: Option[String], fun: => Unit, registrationClosedResource: String, sourceFileName: String, methodName: String, stackDepth: Int, adjustment: Int, location: Option[Location]) {

    if (insideAPathTest)
      throw new TestRegistrationClosedException(Resources(registrationClosedResource), getStackDepthFun(sourceFileName, methodName, stackDepth + adjustment))

    val nextPath = getNextPath()
    // val nextPathZero = if (nextPath.length > 0) nextPath(0) else -1
    // val nextPathOne = if (nextPath.length > 1) nextPath(1) else -1
    // val nextPathTwo = if (nextPath.length > 2) nextPath(2) else -1
    // val isDef = targetPath.isDefined
    // val isInTarget = if (isDef) isInTargetPath(nextPath, targetPath) else false
    // val theTarget = if (isDef) targetPath.get else List()
    // val targetPathZero = if (theTarget.length > 0) theTarget(0) else -1
    // val targetPathOne = if (theTarget.length > 1) theTarget(1) else -1
    // val targetPathTwo = if (theTarget.length > 2) theTarget(2) else -1
    if (targetLeafHasBeenReached && nextTargetPath.isEmpty) {
      nextTargetPath = Some(nextPath)
    }
    else if (isInTargetPath(nextPath, targetPath)) { 
      val oldCurrentPath = currentPath // I added !previousDescribeWasEmpty to get a sibling describe following an empty describe to get executed.
      currentPath = nextPath
      if (!registeredPathSet.contains(nextPath)) {
        // Set this to true before executing the describe. If it has a test, then handleTest will be invoked
        // and it will set previousDescribeWasEmpty back to false
        describeRegisteredNoTests = true
        registerNestedBranch(description, None, fun, "describeCannotAppearInsideAnIt", sourceFileName, methodName, stackDepth + 1, adjustment, location)
        registeredPathSet += nextPath
        if (describeRegisteredNoTests)
          targetLeafHasBeenReached = true
      }
      else {
        navigateToNestedBranch(nextPath, fun, "describeCannotAppearInsideAnIt", sourceFileName, methodName, stackDepth + 1, adjustment)
      }
      currentPath = oldCurrentPath
    }
  }

 def navigateToNestedBranch(path: List[Int], fun: => Unit, registrationClosedResource: String, sourceFile: String, methodName: String, stackDepth: Int, adjustment: Int) {

    val oldBundle = atomic.get
    val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    if (registrationClosed)
      throw new TestRegistrationClosedException(Resources(registrationClosedResource), getStackDepthFun(sourceFile, methodName, stackDepth + adjustment))

    // First look in current branch's subnodes for another branch
    def getBranch(b: Branch, path: List[Int]): Branch = {
      path match {
        case Nil => b
        case i :: tail =>
          val index = b.subNodes.length - 1 - i // They are in reverse order
          getBranch(b.subNodes(index).asInstanceOf[Branch], tail)
      }
    }
    
    val oldBranch = currentBranch
    val newBranch = getBranch(Trunk, path)
    // oldBranch.subNodes ::= newBranch

    // Update atomic, making the current branch to the new branch
    updateAtomic(oldBundle, Bundle(newBranch, testNamesList, testsMap, tagsMap, registrationClosed))

    fun // Execute the function

    { // Put the old branch back as the current branch
      val oldBundle = atomic.get
      val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      updateAtomic(oldBundle, Bundle(oldBranch, testNamesList, testsMap, tagsMap, registrationClosed))
    }
  }

  def runPathTestsImpl(
    theSuite: Suite,
    testName: Option[String],
    args: Args,
    info: Informer,
    includeIcon: Boolean,
    runTest: (String, Args) => Status
  ): Status = {
    import args._

     // All but one line of code copied from runImpl. Factor out duplication later...
    val stopRequested = stopper

    // Set the flag that indicates registration is closed (because run has now been invoked),
    // which will disallow any further invocations of "test" or "ignore" with
    // an RegistrationClosedException.    
    val oldBundle = atomic.get
    val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
    if (!registrationClosed)
      updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, true))

    val report = Suite.wrapReporterIfNecessary(theSuite, reporter)
    val newArgs = if (report eq reporter) args else args.copy(reporter = report)

    val informerForThisSuite =
      ConcurrentInformer(
        (message, payload, isConstructingThread, location) =>
          reportInfoProvided(theSuite, report, tracker, None, message, payload, 1, location, isConstructingThread)
      )

    atomicInformer.set(informerForThisSuite)

    val updaterForThisSuite =
      ConcurrentNotifier(
        (message, payload, isConstructingThread, location) =>
          reportNoteProvided(theSuite, report, tracker, None, message, payload, 1, location, isConstructingThread)
      )

    atomicNotifier.set(updaterForThisSuite)

    val alerterForThisSuite =
      ConcurrentAlerter(
        (message, payload, isConstructingThread, location) =>
          reportAlertProvided(theSuite, report, tracker, None, message, payload, 1, location, isConstructingThread)
      )

    atomicAlerter.set(alerterForThisSuite)

    val documenterForThisSuite =
      ConcurrentDocumenter(
        (message, payload, isConstructingThread, location) =>
          reportMarkupProvided(theSuite, report, tracker, None, message, 1, location, isConstructingThread)
      )

    atomicDocumenter.set(documenterForThisSuite)

    try {
     runTestsImpl(theSuite, testName, newArgs, info, true, runTest)
    }
    finally {
      val shouldBeInformerForThisSuite = atomicInformer.getAndSet(zombieInformer)
      if (shouldBeInformerForThisSuite ne informerForThisSuite)
        throw new ConcurrentModificationException(Resources("concurrentInformerMod", theSuite.getClass.getName))

      val shouldBeNotifierForThisSuite = atomicNotifier.getAndSet(zombieNotifier)
      if (shouldBeNotifierForThisSuite ne updaterForThisSuite)
        throw new ConcurrentModificationException(Resources("concurrentNotifierMod", theSuite.getClass.getName))

      val shouldBeAlerterForThisSuite = atomicAlerter.getAndSet(zombieAlerter)
      if (shouldBeAlerterForThisSuite ne alerterForThisSuite)
        throw new ConcurrentModificationException(Resources("concurrentAlerterMod", theSuite.getClass.getName))

      val shouldBeDocumenterForThisSuite = atomicDocumenter.getAndSet(zombieDocumenter)
      if (shouldBeDocumenterForThisSuite ne documenterForThisSuite)
        throw new ConcurrentModificationException(Resources("concurrentDocumenterMod", theSuite.getClass.getName))
    }
  }
   
  def handleIgnoredTest(testText: String, f: () => Outcome, testRegistrationClosedResourceName: String, sourceFileName: String, methodName: String, stackDepth: Int, adjustment: Int, location: Option[Location], testTags: Tag*) {

    if (insideAPathTest) 
      throw new TestRegistrationClosedException(Resources(testRegistrationClosedResourceName), getStackDepthFun(sourceFileName, methodName, stackDepth + adjustment))
    
    describeRegisteredNoTests = false
    val nextPath = getNextPath()
    if (isInTargetPath(nextPath, targetPath)) {
      super.registerIgnoredTest(testText, f, testRegistrationClosedResourceName, sourceFileName, methodName, stackDepth + 1, adjustment, location, testTags: _*)
      targetLeafHasBeenReached = true
    }
    else if (targetLeafHasBeenReached && nextTargetPath.isEmpty) {
      nextTargetPath = Some(nextPath)
    }
  }
}

private[scalatest] object PathEngine {
  
   private[this] val engine = new ThreadLocal[PathEngine]

   def setEngine(en: PathEngine) {
     if (engine.get != null)
       throw new IllegalStateException("Engine was already defined when setEngine was called")
     engine.set(en)
   }

   def getEngine(): PathEngine = {
     val en = engine.get
     engine.set(null)
     if (en == null) (new PathEngine("concurrentSpecMod", "Spec")) else en
   }
   
  /*
   * First time this is instantiated, targetPath will be None. In that case, execute the
   * first test, and each describe clause on the way to the first test (the all zeros path).
   */
  def isInTargetPath(currentPath: List[Int], targetPath: Option[List[Int]]): Boolean = {
    def allZeros(xs: List[Int]) = xs.count(_ == 0) == xs.length
    if (targetPath.isEmpty)
      allZeros(currentPath)
    else {
      if (currentPath.length < targetPath.get.length)
        targetPath.get.take(currentPath.length) == currentPath
      else if (currentPath.length > targetPath.get.length)
        (currentPath.take(targetPath.get.length) == targetPath.get) && (!currentPath.drop(targetPath.get.length).exists(_ != 0)) // TODO: deal with sibling describes
      else
        targetPath.get == currentPath
    }
  }
}
