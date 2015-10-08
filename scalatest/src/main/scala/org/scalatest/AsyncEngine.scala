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
import Suite.IgnoreTagName
import org.scalatest.Suite._
import org.scalatest.events.LineInFile
import org.scalatest.events.SeeStackDepthException
import scala.annotation.tailrec
import org.scalatest.Suite.checkChosenStyles
import org.scalatest.events.Event
import org.scalatest.events.Location
import collection.mutable.ListBuffer
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.TestPendingException
import org.scalatest.exceptions.TestRegistrationClosedException
import org.scalactic.Requirements._
import org.scalactic.exceptions.NullArgumentException

import scala.util.{Failure, Success}

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
      throw new ConcurrentModificationException(concurrentBundleModMessageFun)
  }

  private def checkTestOrIgnoreParamsForNull(testName: String, testTags: Tag*) {
    requireNonNull(testName)
    if (testTags.exists(_ == null))
      throw new NullArgumentException("a test tag was null")
  }

  def runTestImpl(
    theSuite: Suite,
    testName: String,
    args: Args,
    includeIcon: Boolean,
    invokeWithFixture: TestLeaf => AsyncOutcome
  ): Status = {

    requireNonNull(testName, args)
    
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

    val asyncOutcome: AsyncOutcome =
      try {
        invokeWithFixture(theTest)
      }
      catch {
        case ex: exceptions.TestCanceledException => PastOutcome(Canceled(ex)) // Probably don't need these anymore.
        case _: exceptions.TestPendingException => PastOutcome(Pending)
        case tfe: exceptions.TestFailedException => PastOutcome(Failed(tfe))
        case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => PastOutcome(Failed(ex))
      }

    asyncOutcome.onComplete { trial =>
      // println("###onComplete in the FORK!!")
      trial match {
        case Success(outcome) =>
          outcome match {

            case Succeeded =>
              val duration = System.currentTimeMillis - testStartTime
              val durationToReport = theTest.recordedDuration.getOrElse(duration)
              val recordEvents = messageRecorderForThisTest.recordedEvents(false, false) ++
                (if (theTest.recordedMessages.isDefined)
                  theTest.recordedMessages.get.recordedEvents(false, theSuite, report, tracker, testName, theTest.indentationLevel + 1, includeIcon)
                else
                  Vector.empty)
              reportTestSucceeded(theSuite, report, tracker, testName, theTest.testText, recordEvents, durationToReport, formatter, theSuite.rerunner, theTest.location)
              SucceededStatus

            case Pending =>
              val duration = System.currentTimeMillis - testStartTime
              // testWasPending = true so info's printed out in the finally clause show up yellow
              val recordEvents = messageRecorderForThisTest.recordedEvents(true, false) ++
                (if (theTest.recordedMessages.isDefined)
                  theTest.recordedMessages.get.recordedEvents(true, theSuite, report, tracker, testName, theTest.indentationLevel + 1, includeIcon)
                else
                  Vector.empty)
              reportTestPending(theSuite, report, tracker, testName, theTest.testText, recordEvents, duration, formatter, theTest.location)
              SucceededStatus

            case Canceled(e) =>
              val duration = System.currentTimeMillis - testStartTime
              // testWasCanceled = true so info's printed out in the finally clause show up yellow
              val recordEvents = messageRecorderForThisTest.recordedEvents(false, true) ++
                (if (theTest.recordedMessages.isDefined)
                  theTest.recordedMessages.get.recordedEvents(false, theSuite, report, tracker, testName, theTest.indentationLevel + 1, includeIcon)
                else
                  Vector.empty)
              reportTestCanceled(theSuite, report, e, testName, theTest.testText, recordEvents, theSuite.rerunner, tracker, duration, formatter, theTest.location)
              SucceededStatus

            case Failed(e) =>
              val duration = System.currentTimeMillis - testStartTime
              val durationToReport = theTest.recordedDuration.getOrElse(duration)
              val recordEvents = messageRecorderForThisTest.recordedEvents(false, false) ++
                (if (theTest.recordedMessages.isDefined)
                  theTest.recordedMessages.get.recordedEvents(false, theSuite, report, tracker, testName, theTest.indentationLevel + 1, includeIcon)
                else
                  Vector.empty)
              reportTestFailed(theSuite, report, e, testName, theTest.testText, recordEvents, theSuite.rerunner, tracker, durationToReport, formatter,  Some(SeeStackDepthException))
              FailedStatus
          }
        case Failure(ex) => throw ex
      }
    }
    asyncOutcome.toStatus
  }

  private def runTestsInBranch(
    theSuite: Suite,
    branch: Branch,
    args: Args,
    includeIcon: Boolean,
    oneAfterAnotherAsync: Boolean,
    runTest: (String, Args) => Status
  ): Status = {

    import args.stopper
    
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
        if (!stopper.stopRequested) {
          node match {
            case testLeaf @ TestLeaf(_, testName, testText, _, _, _, _) =>
              val (filterTest, ignoreTest) = args.filter(testName, theSuite.tags, theSuite.suiteId)
              if (!filterTest)
                if (ignoreTest) {
                  val testTextWithOptionalPrefix = prependChildPrefix(branch, testText)
                  val theTest = atomic.get.testsMap(testName)
                  reportTestIgnored(theSuite, args.reporter, args.tracker, testName, testTextWithOptionalPrefix, getIndentedTextForTest(testTextWithOptionalPrefix, testLeaf.indentationLevel, true), theTest.location)
                }
                else {
                  statusList += {
                    if (!oneAfterAnotherAsync || statusList.isEmpty) {
                      runTest(testName, args) // If oneAfterAnotherAsync, first time just go for it
                    }
                    else {
                      statusList.last thenRun runTest(testName, args)  // Only if oneAfterAnotherAsync, after first Status
                    }
                  }
                }

            case infoLeaf @ InfoLeaf(_, message, payload, location) =>
              reportInfoProvided(theSuite, args.reporter, args.tracker, None, message, payload, infoLeaf.indentationLevel, location, true, includeIcon)

            case noteLeaf @ NoteLeaf(_, message, payload, location) =>
              reportNoteProvided(theSuite, args.reporter, args.tracker, None, message, payload, noteLeaf.indentationLevel, location, true, includeIcon)

            case alertLeaf @ AlertLeaf(_, message, payload, location) =>
              reportAlertProvided(theSuite, args.reporter, args.tracker, None, message, payload, alertLeaf.indentationLevel, location, true, includeIcon)

            case markupLeaf @ MarkupLeaf(_, message, location) =>
              reportMarkupProvided(theSuite, args.reporter, args.tracker, None, message, markupLeaf.indentationLevel, location, true, includeIcon)

            case branch: Branch => statusList += runTestsInBranch(theSuite, branch, args, includeIcon, oneAfterAnotherAsync, runTest)
          }
        }
      }
    }
    new CompositeStatus(Set.empty ++ statusList)
  }

  def prependChildPrefix(branch: Branch, testText: String): String =
    branch match {
      case DescriptionBranch(_, _, Some(cp), _) => Resources.prefixSuffix(cp, testText)
      case _ => testText
    }

  def runTestsImpl(
    theSuite: Suite,
    testName: Option[String],
    args: Args,
    includeIcon: Boolean,
    oneAfterAnotherAsync: Boolean,
    runTest: (String, Args) => Status
  ): Status = {
    requireNonNull(testName, args)

    import args._

    if (theSuite.testNames.size > 0)
      checkChosenStyles(configMap, theSuite.styleName)

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
      case None => statusBuffer += runTestsInBranch(theSuite, Trunk, newArgs, includeIcon, oneAfterAnotherAsync, runTest)
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

    // Set the flag that indicates registration is closed (because run has now been invoked),
    // which will disallow any further invocations of "test" or "ignore" with
    // an RegistrationClosedException.    
    val oldBundle = atomic.get
    val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
    if (!registrationClosed)
      updateAtomic(oldBundle, Bundle(currentBranch, testNamesList, testsMap, tagsMap, true))

    val report = Suite.wrapReporterIfNecessary(theSuite, reporter)

    superRun(testName, args.copy(reporter = report))
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

  def registerNestedBranch(description: String, childPrefix: Option[String], fun: => Unit, registrationClosedMessageFun: => String, sourceFile: String, methodName: String, stackDepth: Int, adjustment: Int, location: Option[Location]) {

    val oldBundle = atomic.get
    val (currentBranch, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    if (registrationClosed)
      throw new TestRegistrationClosedException(registrationClosedMessageFun, getStackDepthFun(sourceFile, methodName, stackDepth + adjustment))

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
  def registerFlatBranch(description: String, registrationClosedMessageFun: => String, sourceFile: String, methodName: String, stackDepth: Int, adjustment: Int) {

    val oldBundle = atomic.get
    val (_, testNamesList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    if (registrationClosed)
      throw new TestRegistrationClosedException(registrationClosedMessageFun, getStackDepthFun(sourceFile, methodName, stackDepth + adjustment))

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
  def registerTest(testText: String, testFun: T, testRegistrationClosedMessageFun: => String, sourceFileName: String, methodName: String, stackDepth: Int, adjustment: Int, duration: Option[Long], location: Option[Location], informer: Option[PathMessageRecordingInformer], testTags: Tag*): String = { // returns testName

    checkRegisterTestParamsForNull(testText, testTags: _*)

    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(testRegistrationClosedMessageFun, getStackDepthFun(sourceFileName, methodName, stackDepth + adjustment))
//    throw new TestRegistrationClosedException(Resources.testCannotAppearInsideAnotherTest, getStackDepth(sourceFileName, "test"))

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

  def registerIgnoredTest(testText: String, f: T, testRegistrationClosedMessageFun: => String, sourceFileName: String, methodName: String, stackDepth: Int, adjustment: Int, location: Option[Location], testTags: Tag*) {

    checkRegisterTestParamsForNull(testText, testTags: _*)

// If this works delete this. I think we can rely on registerTest's check
//    if (atomic.get.registrationClosed)
//      throw new TestRegistrationClosedException(Resources.ignoreCannotAppearInsideATest, getStackDepth(sourceFileName, "ignore"))

    val testName = registerTest(testText, f, testRegistrationClosedMessageFun, sourceFileName, methodName, stackDepth + 1, adjustment, None, location, None) // Call test without passing the tags

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

  private def checkRegisterTestParamsForNull(testText: String, testTags: Tag*) {
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
  
  private[scalatest] def createTestDataFor(testName: String, theConfigMap: ConfigMap, theSuite: Suite) = 
    new TestData {
      val configMap = theConfigMap 
      val name = testName
      val scopes = testScopes(testName)
      val text = testText(testName)
      val tags = testTags(testName, theSuite)
    }
  
  private[scalatest] def testTags(testName: String, theSuite: Suite): Set[String] = {
    // SKIP-SCALATESTJS-START
    val suiteTags = for { 
      a <- theSuite.getClass.getAnnotations
      annotationClass = a.annotationType
      if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
    } yield annotationClass.getName
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val suiteTags = Set.empty[String]
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
    extends AsyncSuperEngine[() => AsyncOutcome](concurrentBundleModMessageFun, simpleClassName)

private[scalatest] class AsyncFixtureEngine[FixtureParam](concurrentBundleModMessageFun: => String, simpleClassName: String)
    extends AsyncSuperEngine[FixtureParam => AsyncOutcome](concurrentBundleModMessageFun, simpleClassName)

