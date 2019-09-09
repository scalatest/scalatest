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

import org.scalactic.{Resources => _, NameUtil, Requirements}
import org.scalactic.source
import Requirements.requireNonNull
import NameUtil.getSimpleNameOfAnObjectsClass
import org.scalatest.tools.Utils.wrapReporterIfNecessary
import scala.util.control.NonFatal
import org.scalatest.events.{
  TopOfClass,
  Formatter,
  Location,
  TopOfMethod,
  IndentedText,
  RunStarting,
  SuiteAborted,
  SuiteStarting,
  SeeStackDepthException,
  SuiteCompleted,
  RunCompleted,
  RunAborted,
  InfoProvided,
  NameInfo,
  MotionToSuppress,
  RecordableEvent,
  TestFailed,
  TestStarting,
  TestPending,
  TestCanceled,
  TestSucceeded,
  NoteProvided,
  AlertProvided,
  MarkupProvided,
  ScopeOpened,
  ScopeClosed,
  ScopePending,
  LineInFile,
  TestIgnored
}
import PureTests.formatterForSuiteStarting
import PureTests.formatterForSuiteAborted
import PureTests.formatterForSuiteCompleted
// SKIP-SCALATESTJS,NATIVE-START
import PureTests.getTopOfClass
import org.scalatest.tools.StandardOutReporter
import tools.SuiteDiscoveryHelper
// SKIP-SCALATESTJS,NATIVE-END

trait SuiteNameFunction {
  def apply(outermost: PureSuite): String
}

trait SuiteIdFunction {
  def apply(outermost: PureSuite): String
}

trait TestDataForFunction {
  def apply(outermost: PureSuite, testName: String, theConfigMap: ConfigMap): TestData
}

trait TestNamesFunction {
  def apply(outermost: PureSuite) : Set[String]
}

trait TagsFunction {
  def apply(outermost: PureSuite) : Map[String, Set[String]]
}

trait ExpectedTestCountFunction {
  def apply(outermost: PureSuite, filter: Filter): Int
}

trait RerunnerFunction {
  def apply(outermost: PureSuite) : Option[String]
}

trait PureSuite extends RunnableSuite { thisSuite =>

  final def run(testName: Option[String], args: Args): Status = runFun(this, testName, args)

  final def suiteName: String = suiteNameFun(this)

  final def suiteId: String = suiteIdFun(this)

  final def expectedTestCount(filter: Filter): Int = expectedTestCountFun(this, filter)
  
  final def rerunner: Option[String] = rerunnerFun(this)

  final def nestedSuites: collection.immutable.IndexedSeq[RunnableSuite] = nestedSuitesFun(this)

  def runFun(outermost: PureSuite, testName: Option[String], args: Args): Status

  val suiteNameFun: SuiteNameFunction

  val suiteIdFun: SuiteIdFunction

  val expectedTestCountFun: ExpectedTestCountFunction
  
  val rerunnerFun: RerunnerFunction

  def withBeforeAndAfterAll(
    beforeAll: => Unit,
    afterAll: => Unit,
    invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected: Boolean = false
  ): PureSuite = 
    new BeforeAndAfterAllWrapper(
      thisSuite,
      beforeAll,
      afterAll,
      invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected
    )

  def nestedSuitesFun(outermost: PureSuite): collection.immutable.IndexedSeq[PureSuite]

  def runNestedSuitesFun(outermost: PureSuite, args: Args): Status

  def runTestsFun(outermost: PureSuite, testName: Option[String], args: Args): Status

  def runTestFun(outermost: PureSuite, testName: String, args: Args): Status

  val testDataForFun: TestDataForFunction

  val testNamesFun: TestNamesFunction

  val tagsFun: TagsFunction

  final def execute(
    testName: String = null,
    configMap: ConfigMap = ConfigMap.empty,
    color: Boolean = true,
    durations: Boolean = false,
    shortstacks: Boolean = false,
    fullstacks: Boolean = false,
    stats: Boolean = false
  ): Unit = {
    requireNonNull(configMap)
    val SelectedTag = "Selected"
    val SelectedSet = Set(SelectedTag)
    val desiredTests: Set[String] =
      if (testName == null) Set.empty
      else {
        testNamesFun(thisSuite).filter { s =>
          s.indexOf(testName) >= 0 || NameTransformer.decode(s).indexOf(testName) >= 0
        }
      }
    if (testName != null && desiredTests.isEmpty)
      throw new IllegalArgumentException(Resources.testNotFound(testName))

    val dispatch = new DispatchReporter(List(new StandardOutReporter(durations, color, shortstacks, fullstacks, false, false, false, false, false, false, false)))
    val tracker = new Tracker
    val filter =
      if (testName == null) Filter()
      else {
        val taggedTests: Map[String, Set[String]] = desiredTests.map(_ -> SelectedSet).toMap
        Filter(
          tagsToInclude = Some(SelectedSet),
          excludeNestedSuites = true,
          dynaTags = DynaTags(Map.empty, Map(suiteId -> taggedTests))
        )
      }
    val runStartTime = System.currentTimeMillis
    if (stats)
      dispatch(RunStarting(tracker.nextOrdinal(), expectedTestCount(filter), configMap))

    val suiteStartTime = System.currentTimeMillis
    def dispatchSuiteAborted(e: Throwable): Unit = {
      val eMessage = e.getMessage
      val rawString = 
        if (eMessage != null && eMessage.length > 0)
          Resources.runOnSuiteException
        else
          Resources.runOnSuiteExceptionWithMessage(eMessage)
      val formatter = formatterForSuiteAborted(thisSuite, rawString)
      val duration = System.currentTimeMillis - suiteStartTime
      dispatch(SuiteAborted(tracker.nextOrdinal(), rawString, thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), Some(e), Some(duration), formatter, Some(SeeStackDepthException)))
    }

    try {

      val formatter = formatterForSuiteStarting(thisSuite)
      dispatch(SuiteStarting(tracker.nextOrdinal(), thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), formatter, Some(getTopOfClass(thisSuite))))

      val status =
        run(
          None,
          Args(dispatch,
          Stopper.default,
          filter,
          configMap,
          None,
          tracker,
          Set.empty)
        )
      status.waitUntilCompleted()
      val suiteCompletedFormatter = formatterForSuiteCompleted(thisSuite)
      val duration = System.currentTimeMillis - suiteStartTime
      dispatch(SuiteCompleted(tracker.nextOrdinal(), thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), Some(duration), suiteCompletedFormatter, Some(getTopOfClass(thisSuite))))
      if (stats) {
        val duration = System.currentTimeMillis - runStartTime
        dispatch(RunCompleted(tracker.nextOrdinal(), Some(duration)))
      }
    }
    catch {
      case e: InstantiationException =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources.cannotInstantiateSuite(e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
      case e: IllegalAccessException =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources.cannotInstantiateSuite(e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
      case e: NoClassDefFoundError =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources.cannotLoadClass(e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
      case e: Throwable =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources.bigProblems(e), Some(e), Some(System.currentTimeMillis - runStartTime)))
        if (!NonFatal(e))
          throw e
    }
    finally {
      dispatch.dispatchDisposeAndWaitUntilDone()
    }
  }
}

// Eventually, this hard codes lifecycle functions to no tests
trait PureSuites extends PureSuite

// Eventually, this hard codes lifecycle functions to no nested suites
trait PureTests extends PureSuite {

  final def nestedSuitesFun(outermost: PureSuite): collection.immutable.IndexedSeq[PureSuite] = Vector.empty

  final def runNestedSuitesFun(outermost: PureSuite, args: Args): Status = SucceededStatus
}

trait PureTestSuite extends PureTests { thisSuite =>

  protected trait NoArgTest extends (() => Outcome) with TestData {

    /**
     * Runs the body of the test, returning an <code>Outcome</code>.
     */
    def apply(): Outcome
  }

  // Keep this out of the public until there's a use case demonstrating its need
  private[scalatest] object NoArgTest {
    def apply(test: NoArgTest)(f: => Outcome): NoArgTest = {
      new NoArgTest {
        def apply(): Outcome = { f }
        val text: String = test.text
        val configMap: ConfigMap = test.configMap
        val scopes: collection.immutable.IndexedSeq[String] = test.scopes
        val name: String = test.name
        val tags: Set[String] = test.tags
        val pos: Option[source.Position] = test.pos
      }
    }
  }

  /**
   * Run the passed test function in the context of a fixture established by this method.
   *
   * <p>
   * This method should set up the fixture needed by the tests of the
   * current suite, invoke the test function, and if needed, perform any clean
   * up needed after the test completes. Because the <code>NoArgTest</code> function
   * passed to this method takes no parameters, preparing the fixture will require
   * side effects, such as reassigning instance <code>var</code>s in this <code>Suite</code> or initializing
   * a globally accessible external database. If you want to avoid reassigning instance <code>var</code>s
   * you can use <a href="FixtureSuite.html">FixtureSuite</a>.
   * </p>
   *
   * <p>
   * This trait's implementation of <code>runTest</code> invokes this method for each test, passing
   * in a <code>NoArgTest</code> whose <code>apply</code> method will execute the code of the test.
   * </p>
   *
   * <p>
   * This trait's implementation of this method simply invokes the passed <code>NoArgTest</code> function.
   * </p>
   *
   * @param test the no-arg test function to run with a fixture
   */
  val aroundEachFun: (PureTestSuite, NoArgTest) => Outcome = {
    (thisSuite: PureTestSuite, test: NoArgTest) => test()
  }
}

object PureTests {
  // SKIP-SCALATESTJS,NATIVE-START
  def getTopOfClass(theSuite: PureSuite) = TopOfClass(theSuite.getClass.getName)
  // SKIP-SCALATESTJS,NATIVE-END
  def formatterForSuiteCompleted(suite: PureSuite): Option[Formatter] =
      Some(MotionToSuppress)

  def formatterForSuiteAborted(suite: PureSuite, message: String): Option[Formatter] = {
    val actualSuiteName =
      suite match {
        // case DeferredAbortedSuite(suiteClassName, deferredThrowable) => suiteClassName
        case _ => suite.getClass.getName
      }
    Some(IndentedText(actualSuiteName, message, 0))
  }
  def formatterForSuiteStarting(suite: PureSuite): Option[Formatter] =
    // if ((suite.testNames.isEmpty) && (suite.nestedSuites.size > 0))
      // Some(MotionToSuppress)
    // else
      Some(IndentedText(suite.suiteName + ":", suite.suiteName, 0))
}

class PureFunSuite(tests: Test[() => Outcome]*) extends PureTestSuite { thisSuite =>
  
  final val suiteNameFun: SuiteNameFunction =
    new SuiteNameFunction {
      def apply(outermost: PureSuite): String = getSimpleNameOfAnObjectsClass(thisSuite)
    }

  final val suiteIdFun: SuiteIdFunction =
    new SuiteIdFunction {
      def apply(outermost: PureSuite): String = thisSuite.getClass.getName
    }

  final def runTestsFun(outermost: PureSuite, testName: Option[String], args: Args): Status = {
    for (t <- tests)
       runTestFun(thisSuite, t.testText, args)
    SucceededStatus
  }

  final def runTestFun(outermost: PureSuite, testName: String, args: Args): Status = {
    println("running " + testName)
    SucceededStatus
  }

  final val testDataForFun: TestDataForFunction =
    new TestDataForFunction {
      def apply(outermost: PureSuite, testName: String, theConfigMap: ConfigMap): TestData = throw new IllegalArgumentException
    }

  final val testNamesFun: TestNamesFunction =
    new TestNamesFunction {
      def apply(outermost: PureSuite) : Set[String] = tests.map(_.testText).toSet
    }

  final val tagsFun: TagsFunction =
    new TagsFunction {
      def apply(outermost: PureSuite) : Map[String, Set[String]] = Map.empty
    }

  final val expectedTestCountFun: ExpectedTestCountFunction = 
    new ExpectedTestCountFunction {
      def apply(outermost: PureSuite, filter: Filter): Int = tests.size
    }

  final def runFun(outermost: PureSuite, testName: Option[String], args: Args): Status = {

    requireNonNull(testName, args)

    import args._

    val originalThreadName = Thread.currentThread.getName
    try {
      Thread.currentThread.setName(SuiteHelpers.augmentedThreadName(originalThreadName, suiteName))

      val report = reporter // wrapReporterIfNecessary(thisSuite, reporter)
      val newArgs = args.copy(reporter = report)

      val testsStatus = runTestsFun(thisSuite, testName, newArgs)

      if (stopper.stopRequested) {
        val rawString = Resources.executeStopping
        report(InfoProvided(tracker.nextOrdinal(), rawString, Some(NameInfo(thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), testName))))
      }
      testsStatus
    }
    finally Thread.currentThread.setName(originalThreadName)
  }

  final val rerunnerFun: RerunnerFunction =
    new RerunnerFunction {
      def apply(outermost: PureSuite) : Option[String] = {
        val suiteClass = thisSuite.getClass
        // SKIP-SCALATESTJS,NATIVE-START
        val isAccessible = SuiteDiscoveryHelper.isAccessibleSuite(suiteClass)
        val hasWrapWithAnnotation = suiteClass.getAnnotation(classOf[WrapWith]) != null
        if (isAccessible || hasWrapWithAnnotation)
          Some(suiteClass.getName)
        else
          None
        // SKIP-SCALATESTJS,NATIVE-END
        //SCALATESTJS,NATIVE-ONLY Some(suiteClass.getName)
      }
    }
}

case class Test[T](
  testText: String,
  testFun: T,
  testRegistrationClosedMessageFun: () => String,
  sourceFileName: String,
  methodName: String,
  stackDepth: Int,
  adjustment: Int,
  duration: Option[Long],
  location: Option[Location],
  pos: Option[source.Position],
  informer: Option[PathMessageRecordingInformer],
  testTags: Tag*
)
object PureFunSuite {
  def test(testName: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Test[() => Outcome] = {
    // SKIP-SCALATESTJS-START
    val stackDepth = 4
    val stackDepthAdjustment = -2
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val stackDepth = 6
    //SCALATESTJS-ONLY val stackDepthAdjustment = -6
    Test[() => Outcome](testName, Transformer(() => testFun), () => Resources.testCannotAppearInsideAnotherTest, "FunSuiteLike.scala", "test", stackDepth, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
  }
}

import PureFunSuite.test
class MySuite extends PureFunSuite(
  test("one fish") { val x = 1; assert(x == 2) },
  test("two fish") { val x = 1; assert(x == 3) },
  test("red fish") { val x = 1; assert(x == 1) },
  test("blue fish") { val x = 1; assert(x == 4) }
)

class PureSuiteWrapper(decorated: PureSuite) extends PureSuite {

  override def runFun(outermost: PureSuite, testName: Option[String], args: Args): Status = decorated.runFun(outermost, testName, args)

  override val suiteNameFun: SuiteNameFunction = decorated.suiteNameFun

  override val suiteIdFun: SuiteIdFunction = decorated.suiteIdFun
    
  override val expectedTestCountFun: ExpectedTestCountFunction = decorated.expectedTestCountFun
  
  override val rerunnerFun: RerunnerFunction = decorated.rerunnerFun

  override def nestedSuitesFun(outermost: PureSuite): collection.immutable.IndexedSeq[PureSuite] = decorated.nestedSuitesFun(outermost)

  override def runNestedSuitesFun(outermost: PureSuite, args: Args): Status = decorated.runNestedSuitesFun(outermost, args)

  override def runTestsFun(outermost: PureSuite, testName: Option[String], args: Args): Status = decorated.runTestsFun(outermost, testName, args)

  override def runTestFun(outermost: PureSuite, testName: String, args: Args): Status = decorated.runTestFun(outermost, testName, args)

  override val testDataForFun: TestDataForFunction = decorated.testDataForFun

  override val testNamesFun: TestNamesFunction = decorated.testNamesFun

  override val tagsFun: TagsFunction = decorated.tagsFun
}

final class BeforeAndAfterAllWrapper(
  decorated: PureSuite,
  beforeAll: => Unit,
  afterAll: => Unit,
  invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected: Boolean = false
) extends PureSuiteWrapper(decorated) { thisSuite =>
 
  override def runFun(outermost: PureSuite, testName: Option[String], args: Args): Status = {
    val (runStatus, thrownException) =
      try {
        if (!args.runTestInNewInstance && (expectedTestCount(args.filter) > 0 || invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected))
          beforeAll
        (decorated.runFun(thisSuite, testName, args), None)
      }
      catch {
        case e: Exception => (FailedStatus, Some(e))
      }

    try {
      val statusToReturn =
        if (!args.runTestInNewInstance && (expectedTestCount(args.filter) > 0 || invokeBeforeAllAndAfterAllEvenIfNoTestsAreExpected)) {
          // runStatus may not be completed, call afterAll only after it is completed
          runStatus withAfterEffect {
            try {
             afterAll
            }
            catch {
              case laterException: Exception if !Suite.anExceptionThatShouldCauseAnAbort(laterException) && thrownException.isDefined =>
              // We will swallow the exception thrown from after if it is not test-aborting and exception was already thrown by before or test itself.
            }
          }
        }
        else runStatus
      thrownException match {
        case Some(e) => throw e
        case None =>
      }
      statusToReturn
    }
    catch {
      case laterException: Exception =>
        thrownException match { // If both before/run and after throw an exception, report the earlier exception
          case Some(earlierException) => throw earlierException
          case None => throw laterException
        }
    }
  }
}

/*
) withAroundEach (
  aroundEach = (t: NoArgTest) => t()
)

I think we use NoArgTest in the companion of PureTestSuite and PureAsyncTestSuite
and OneArgTest[F] in the companion of PureFixtureTestSuite and PureFixtureAsyncTestSuite

withFixtureAroundEach

Maybe I rename withFixture in TestSuite to aroundEach and
in FixtureTestSuite to fixtureAroundEach. I'd like them
to be the same names in classic and pure. That way
these are consistently named.

Oh, and PureFixtureTestSuite[F], not a type member, needs
to be a type parameter, so you can use it in the constructor.
Als means I could do some things with that function, 
such as contramap.

def fixtureAroundEach(t: OneArgTest): Outcome = {
}

In 4.0 I can rename those. And pull out RunnableSuite.
The introduction of these is 4.0.

nestedSuites should be a Seq of RunnableSuites.

can be called decorated and outermost. 

decorated is shorter than underlying:
underlying
decorated
*/


