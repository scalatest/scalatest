package org.scalatest.tools

import org.scalatools.testing.{Framework => SbtFramework, _}
import org.scalatest.tools.Runner.parsePropertiesArgsIntoMap
import org.scalatest.tools.Runner.parseCompoundArgIntoSet
import SuiteDiscoveryHelper._
import org.scalatest.Suite.formatterForSuiteStarting
import org.scalatest.Suite.formatterForSuiteCompleted
import org.scalatest.Suite.formatterForSuiteAborted
import org.scalatest.events.SuiteStarting
import org.scalatest.events.SuiteCompleted
import org.scalatest.events.SuiteAborted
import org.scalatest.events.SeeStackDepthException
import org.scalatest.events.TopOfClass
import org.scalatest._
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger

/**
 * Class that makes ScalaTest tests visible to sbt.
 *
 * <p>
 * To use ScalaTest from within sbt, simply add a line like this to your project file (for sbt 0.1.0 or higher):
 * </p>
 *
 * <pre class="stExamples">
 * libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"
 * </pre>
 *
 * <p>
 * The above line of code will work for any version of Scala 2.9 (for example, it works for Scala 2.9.0-1).
 * </p>
 *
 * <pre class="stExamples">
 * libraryDependencies += "org.scalatest" % "scalatest_2.8.1" % "1.5.1" % "test"
 * </pre>
 *
 * <p>
 * You can configure the output shown when running with sbt in four ways: 1) turn off color, 2) show
 * short stack traces, 3) full stack traces, and 4) show durations for everything. To do that
 * you need to add test options, like this:
 * </p>
 *
 * <pre class="stExamples">
 * override def testOptions = super.testOptions ++
 *   Seq(TestArgument(TestFrameworks.ScalaTest, "-oD"))
 * </pre>
 *
 * <p>
 * After the -o, place any combination of:
 * </p>
 *
 * <ul>
 * <li>D - show durations</li>
 * <li>S - show short stack traces</li>
 * <li>F - show full stack traces</li>
 * <li>W - without color</li>
 * </ul>
 *
 * <p>
 * For example, "-oDF" would show full stack traces and durations (the amount
 * of time spent in each test).
 * </p>
 *
 * @author Bill Venners
 * @author Josh Cough
 */
class ScalaTestFramework extends SbtFramework {
  
  /**
   * Returns <code>"ScalaTest"</code>, the human readable name for this test framework.
   */
  def name = "ScalaTest"

  /**
   * Returns an array containing fingerprint for ScalaTest's test, which are classes  
   * whose superclass name is <code>org.scalatest.Suite</code>
   * or is annotated with <code>org.scalatest.WrapWith</code>.
   */
  def tests =
    Array(
      new org.scalatools.testing.TestFingerprint {
        def superClassName = "org.scalatest.Suite"
        def isModule = false
      },
      new org.scalatools.testing.AnnotatedFingerprint {
        def annotationName = "org.scalatest.WrapWith"
        def isModule = false
      }
    )
    
  object RunConfig {

    private val reporter: AtomicReference[Option[DispatchReporter]] = new AtomicReference(None)
    private val reporterConfigs: AtomicReference[Option[ReporterConfigurations]] = new AtomicReference(None)
    private val useStdout, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted = new AtomicBoolean(false)
    private val presentReminder, presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests = new AtomicBoolean(false)
    private val filter: AtomicReference[Option[Filter]] = new AtomicReference(None)
    private val configMap: AtomicReference[Option[ConfigMap]] = new AtomicReference(None)
    private val resultHolder = new SuiteResultHolder()
    
    def getConfigurations(args: Array[String], loggers: Array[Logger], eventHandler: EventHandler, testLoader: ClassLoader) = 
      synchronized {
        if (reporterConfigs.get.isEmpty) {
          // Why are we getting rid of empty strings? Were empty strings coming in from sbt? -bv 11/09/2011
          val translator = new FriendlyParamsTranslator()
          val (propertiesArgsList, includesArgsList, excludesArgsList, repoArgsList, concurrentList, memberOnlyList, wildcardList, 
               suiteList, junitList, testngList) = translator.parsePropsAndTags(args.filter(!_.equals("")))
          configMap.getAndSet(Some(parsePropertiesArgsIntoMap(propertiesArgsList)))
          val tagsToInclude: Set[String] = parseCompoundArgIntoSet(includesArgsList, "-n")
          val tagsToExclude: Set[String] = parseCompoundArgIntoSet(excludesArgsList, "-l")
          filter.getAndSet(Some(org.scalatest.Filter(if (tagsToInclude.isEmpty) None else Some(tagsToInclude), tagsToExclude)))
          
          val fullReporterConfigurations = Runner.parseReporterArgsIntoConfigurations(repoArgsList)
          
          fullReporterConfigurations.standardOutReporterConfiguration match {
            case Some(stdoutConfig) =>
              val configSet = stdoutConfig.configSet
              useStdout.getAndSet(true)
              presentAllDurations.getAndSet(configSet.contains(PresentAllDurations))
              presentInColor.getAndSet(!configSet.contains(PresentWithoutColor))
              presentShortStackTraces.getAndSet(configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces))
              presentFullStackTraces.getAndSet(configSet.contains(PresentFullStackTraces))
              presentUnformatted.getAndSet(configSet.contains(PresentUnformatted))
              presentReminder.getAndSet(
                configSet.exists { ele =>
                  ele == PresentReminderWithoutStackTraces || ele == PresentReminderWithShortStackTraces || ele == PresentReminderWithFullStackTraces
                }
              )
              presentReminderWithShortStackTraces.getAndSet(configSet.contains(PresentReminderWithShortStackTraces) && !configSet.contains(PresentReminderWithFullStackTraces))
              presentReminderWithFullStackTraces.getAndSet(configSet.contains(PresentReminderWithFullStackTraces))
              presentReminderWithoutCanceledTests.getAndSet(configSet.contains(PresentReminderWithoutCanceledTests))
            case None => 
              useStdout.getAndSet(repoArgsList.isEmpty)  // If no reporters specified, just give them a default stdout reporter
              presentAllDurations.getAndSet(false)
              presentInColor.getAndSet(true)
              presentShortStackTraces.getAndSet(false)
              presentFullStackTraces.getAndSet(false)
              presentUnformatted.getAndSet(false)
              presentReminder.getAndSet(false)
              presentReminderWithShortStackTraces.getAndSet(false)
              presentReminderWithFullStackTraces.getAndSet(false)
              presentReminderWithoutCanceledTests.getAndSet(false)
          }
          
          fullReporterConfigurations.graphicReporterConfiguration match {
            case Some(g) => throw new IllegalArgumentException("Graphic reporter is not supported when runs in SBT.")
            case None => 
          }
          
          reporterConfigs.getAndSet(Some(fullReporterConfigurations.copy(standardOutReporterConfiguration = None)))
        }
        
        if (reporter.get.isEmpty || reporter.get.get.isDisposed) 
          reporter.getAndSet(Some(ReporterFactory.getDispatchReporter(reporterConfigs.get.get, None, None, testLoader, Some(resultHolder))))
          
        val dispatchReporter = 
          if (useStdout.get)
            ReporterFactory.getDispatchReporter(Seq(reporter.get.get, createSbtLogInfoReporter(loggers)), None, None, testLoader, Some(resultHolder))
          else
            reporter.get.get
          
        (dispatchReporter, filter.get.get, configMap.get.get)
      }
    
    private val atomicCount = new AtomicInteger(0)
  
    def increaseLatch() {
      atomicCount.incrementAndGet()
    }
  
    def decreaseLatch() {
      if (atomicCount.decrementAndGet() == 0) 
        reporter.get match {
          case Some(r) => r.dispatchDisposeAndWaitUntilDone()
          case None =>
        }
    }
    
    def createSbtLogInfoReporter(loggers: Array[Logger]) = {
      new SbtLogInfoReporter(
          loggers, 
          presentAllDurations.get,
          presentInColor.get,
          presentShortStackTraces.get,
          presentFullStackTraces.get, // If they say both S and F, F overrules
          presentUnformatted.get,
          presentReminder.get,
          presentReminderWithShortStackTraces.get,
          presentReminderWithFullStackTraces.get,
          presentReminderWithoutCanceledTests.get
        )
    }
  }

  /**
   * Returns an <code>org.scalatools.testing.Runner</code> that will load test classes via the passed <code>testLoader</code>
   * and direct output from running the tests to the passed array of <code>Logger</code>s.
   */
  def testRunner(testLoader: ClassLoader, loggers: Array[Logger]) = {
    new ScalaTestRunner(testLoader, loggers)
  }
  
  class SbtLogInfoReporter(
    loggers: Array[Logger],
    presentAllDurations: Boolean,
    presentInColor: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    presentUnformatted: Boolean,
    presentReminder: Boolean,
    presentReminderWithShortStackTraces: Boolean,
    presentReminderWithFullStackTraces: Boolean,
    presentReminderWithoutCanceledTests: Boolean
  ) extends StringReporter(
    presentAllDurations,
    presentInColor,
    presentShortStackTraces,
    presentFullStackTraces,
    presentUnformatted,
    presentReminder,
    presentReminderWithShortStackTraces,
    presentReminderWithFullStackTraces,
    presentReminderWithoutCanceledTests
  ) {

    protected def printPossiblyInColor(fragment: Fragment) {
      loggers.foreach { logger =>
        logger.info(fragment.toPossiblyColoredText(logger.ansiCodesSupported && presentInColor))
      }
    }

    def dispose() = ()
  }

  /**The test runner for ScalaTest suites. It is compiled in a second step after the rest of sbt.*/
  private[tools] class ScalaTestRunner(val testLoader: ClassLoader, val loggers: Array[Logger]) extends org.scalatools.testing.Runner2 {
    
    /* 
      test-only FredSuite -- -A -B -C -d  all things to right of == come in as a separate string in the array
 the other way is to set up the options and when I say test it always comes in that way

 new wqay, if one framework

testOptions in Test += Tests.Arguments("-d", "-g")

so each of those would come in as one separate string in the aray

testOptions in Test += Tests.Arguments(TestFrameworks.ScalaTest, "-d", "-g")

Remember:

maybe add a distributor like thing to run
maybe add some event things like pending, ignored as well skipped
maybe a call back for the summary

st look at wiki on xsbt

tasks & commands. commands have full control over everything.
tasks are more integrated, don't need to know as much.
write a sbt plugin to deploy the task.

Commands that should work:

-Ddbname=testdb -Dserver=192.168.1.188
Can't do a runpath
Can add more reporters. -g seems odd, but could be done, -o seems odd. Maybe it is a no-op. -e could work. -r for sure. -u for sure.
Ask Mark about -o. If there's some way to turn off his output, then that could mean -o. Or maybe -o is the default, which I think
it should be for runner anyway, and then if you say -g you don't get -o. Meaning I don't send the strings to log. yes, -o maybe
means log in the sbt case.

Reporters can be configured.

Tags to include and exclude: -n "CheckinTests FunctionalTests" -l "SlowTests NetworkTests"


     */
    def run(testClassName: String, fingerprint: Fingerprint, eventHandler: EventHandler, args: Array[String]) {
      val suiteClass = Class.forName(testClassName, true, testLoader)
       //println("sbt args: " + args.toList)
      if ((isAccessibleSuite(suiteClass) || isRunnable(suiteClass)) && isDiscoverableSuite(suiteClass)) {
        val (reporter, filter, configMap) = RunConfig.getConfigurations(args, loggers, eventHandler, testLoader)
        val report = new SbtReporter(eventHandler, Some(reporter))
        val tracker = new Tracker
        val suiteStartTime = System.currentTimeMillis

        val wrapWithAnnotation = suiteClass.getAnnotation(classOf[WrapWith])
        val suite = 
        if (wrapWithAnnotation == null)
          suiteClass.newInstance.asInstanceOf[Suite]
        else {
          val suiteClazz = wrapWithAnnotation.value
          val constructorList = suiteClazz.getDeclaredConstructors()
          val constructor = constructorList.find { c => 
              val types = c.getParameterTypes
              types.length == 1 && types(0) == classOf[java.lang.Class[_]]
            }
          constructor.get.newInstance(suiteClass).asInstanceOf[Suite]
        }

        val formatter = formatterForSuiteStarting(suite)

        RunConfig.increaseLatch()
        report(SuiteStarting(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suiteClass.getName), formatter, Some(TopOfClass(suiteClass.getName))))

        try {  // TODO: I had to pass Set.empty for chosen styles now. Fix this later.
          suite.run(None, Args(report, Stopper.default, filter, configMap, None, tracker, Set.empty))

          val formatter = formatterForSuiteCompleted(suite)

          val duration = System.currentTimeMillis - suiteStartTime

          report(SuiteCompleted(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suiteClass.getName), Some(duration), formatter, Some(TopOfClass(suiteClass.getName))))

        }
        catch {       
          case e: Exception => {

            // TODO: Could not get this from Resources. Got:
            // java.util.MissingResourceException: Can't find bundle for base name org.scalatest.ScalaTestBundle, locale en_US
            // TODO Chee Seng, I wonder why we couldn't access resources, and if that's still true. I'd rather get this stuff
            // from the resource file so we can later localize.
            val rawString = "Exception encountered when attempting to run a suite with class name: " + suiteClass.getName
            val formatter = formatterForSuiteAborted(suite, rawString)

            val duration = System.currentTimeMillis - suiteStartTime
            report(SuiteAborted(tracker.nextOrdinal(), rawString, suite.suiteName, suite.suiteId, Some(suiteClass.getName), Some(e), Some(duration), formatter, Some(SeeStackDepthException)))
          }
        }
        finally {
          RunConfig.decreaseLatch()
        }
      } // I think we should do nothing for non accessible, non runnable or non discoverable suite. 
      //else throw new IllegalArgumentException("Class is not an accessible org.scalatest.Suite: " + testClassName)
    }

    private val emptyClassArray = new Array[java.lang.Class[T] forSome {type T}](0)
    
    private class SbtReporter(eventHandler: EventHandler, report: Option[DispatchReporter]) extends Reporter {
      
      import org.scalatest.events._

      def fireEvent(tn: String, r: Result, e: Option[Throwable]) = {
        eventHandler.handle(
          new org.scalatools.testing.Event {
            def testName = tn
            def description = tn
            def result = r
            def error = e getOrElse null
          }
        )
      }
      
      override def apply(event: Event) {
        report match {
          case Some(report) => report(event)
          case None =>
        }
        event match {
          // the results of running an actual test
          case t: TestPending => fireEvent(t.testName, Result.Skipped, None)
          case t: TestFailed => fireEvent(t.testName, Result.Failure, t.throwable)
          case t: TestSucceeded => fireEvent(t.testName, Result.Success, None)
          case t: TestIgnored => fireEvent(t.testName, Result.Skipped, None)
          case t: TestCanceled => fireEvent(t.testName, Result.Skipped, None)
          case t: SuiteAborted => fireEvent("!!! Suite Aborted !!!", Result.Failure, t.throwable)
          case _ => 
        }
      }
      
      def dispose() {
        report match {
          case Some(report) => 
            report.dispatchDisposeAndWaitUntilDone()
          case None =>
        }
      }
    }
  }
}
