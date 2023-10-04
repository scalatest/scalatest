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
package org.scalatest.tools

import org.scalatest._
import ArgsParser._
import SuiteDiscoveryHelper._
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference

import org.scalatest.Suite.formatterForSuiteAborted
import org.scalatest.Suite.formatterForSuiteCompleted
import org.scalatest.Suite.formatterForSuiteStarting
import org.scalatest.Suite.getSuiteClassName
import org.scalatest.events.SeeStackDepthException
import org.scalatest.events.SuiteAborted
import org.scalatest.events.SuiteCompleted
import org.scalatest.events.SuiteStarting
import org.scalatest.events.TopOfClass
import org.scalatest.time.{Seconds, Span}
import org.scalatools.testing.{Framework => SbtFramework, _}
import org.scalatest.prop.Seed

/**
 * Class that makes ScalaTest tests visible to SBT (prior to version 0.13).
 *
 * <p>
 * To use ScalaTest in SBT, you should add ScalaTest as dependency in your SBT build file, the following shows an example
 * for using ScalaTest 3.3.0 with Scala 2.13.x project:
 * </p>
 *
 * <pre class="stHighlight">
 * "org.scalatest" %% "scalatest" % "3.3.0" % "test"
 * </pre>
 *
 * <p>
 * To pass argument to ScalaTest from SBT, you can use <code>testOptions</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * testOptions in Test += Tests.Argument("-u", "target/junit")  // Use JUnitXmlReporter
 * </pre>
 *
 * <p>
 * If you are using multiple testing frameworks, you can pass arguments specific to ScalaTest only:
 * </p>
 *
 * <pre class="stHighlight">
 * testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/junit") // Use JUnitXmlReporter
 * </pre>
 *
 * <h3>Supported arguments</h3>
 *
 * <p>
 * Integration in SBT 0.13 supports same argument format as [[org.scalatest.tools.Runner <code>Runner</code>]],
 * except the following arguments:
 * </p>
 *
 * <ul>
 *   <li><code>-R</code> -- runpath is not supported because test path and discovery is handled by SBT</li>
 *   <li><code>-s</code> -- suite is not supported because SBT's <code>test-only</code> serves the similar purpose</li>
 *   <li><code>-A</code> -- again is not supported because SBT's <code>test-quick</code> serves the similar purpose</li>
 *   <li><code>-j</code> -- junit is not supported because in SBT different test framework should be supported by its corresponding <code>Framework</code> implementation</li>
 *   <li><code>-b</code> -- testng is not supported because in SBT different test framework should be supported by its corresponding <code>Framework</code> implementation</li>
 *   <li><code>-P</code> -- concurrent/parallel is not supported because parallel execution is controlled by SBT.</li>
 *   <li><code>-q</code> is not supported because test discovery should be handled by SBT, and SBT's test-only or test filter serves the similar purpose</li>
 *   <li><code>-T</code> is not supported because correct ordering of text output is handled by SBT</li>
 *   <li><code>-g</code> is not supported because current Graphic Reporter implementation works differently than standard reporter</li>
 * </ul>
 *
 *
 * <em>
 * It is highly recommended to upgrade to SBT 0.13 to enjoy the best of ScalaTest 2.0 SBT integration.  Due to limitations
 * in old Framework API (prior to SBT 0.13), it is hard to support ScalaTest features in the most efficient way.  One example is
 * the nested suites, where in old Framework API they has to be executed sequentially, while new Framework API (included in SBT
 * 0.13) the concept of nested <a href="https://github.com/sbt/test-interface/blob/master/src/main/java/sbt/testing/Task.java"><code>Task</code></a>
 * has enabled parallel execution of ScalaTest's nested suites.
 * </em>
 *
 * @author Bill Venners
 * @author Josh Cough
 * @author Chee Seng
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
    
  private[scalatest] object RunConfig {

    val reporter: AtomicReference[Option[DispatchReporter]] = new AtomicReference(None)
    val reporterConfigs: AtomicReference[Option[ReporterConfigurations]] = new AtomicReference(None)
    val useStdout, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted = new AtomicBoolean(false)
    val presentReminder, presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests, presentFilePathname, presentJson = new AtomicBoolean(false)
    val filter: AtomicReference[Option[Filter]] = new AtomicReference(None)
    val configMap: AtomicReference[Option[ConfigMap]] = new AtomicReference(None)
    val membersOnly: AtomicReference[Option[List[String]]] = new AtomicReference(None)
    val wildcard: AtomicReference[Option[List[String]]] = new AtomicReference(None)
    val detectSlowpokes: AtomicBoolean = new AtomicBoolean(false)
    val slowpokeDetectionDelay: AtomicLong = new AtomicLong(60000)
    val slowpokeDetectionPeriod: AtomicLong = new AtomicLong(60000)
    val testSortingReporterTimeout: AtomicReference[Option[Span]] = new AtomicReference(None)
    val resultHolder = new SuiteResultHolder()
    
    def getConfigurations(args: Array[String], loggers: Array[Logger], eventHandler: EventHandler, testLoader: ClassLoader) = 
      synchronized {
        if (reporterConfigs.get.isEmpty) {
          val ParsedArgs(
            runpathArgs,
            reporterArgs,
            suiteArgs,
            againArgs,
            junitArgs,
            propertiesArgs,
            tagsToIncludeArgs,
            tagsToExcludeArgs,
            concurrentArgs,
            membersOnlyArgs,
            wildcardArgs,
            testNGArgs,
            suffixes, 
            spanScaleFactors, 
            testSortingReporterTimeouts,
            slowpokeArgs,
            seedArgs
          ) = parseArgs(args)
          
          if (!runpathArgs.isEmpty)
            throw new IllegalArgumentException("-R (runpath) is not supported when runs in SBT.")
               
          if (!suiteArgs.isEmpty)
            throw new IllegalArgumentException("-s (suite) is not supported when runs in SBT, please use SBT's test-only instead.")
          
          if (!againArgs.isEmpty)
            throw new IllegalArgumentException("-A is not supported when runs in SBT, please use SBT's test-quick instead.")
          
          if (!junitArgs.isEmpty)
            throw new IllegalArgumentException("-j (junit) is not supported when runs in SBT.")
          
          if (!testNGArgs.isEmpty)
            throw new IllegalArgumentException("-b (testng) is not supported when runs in SBT.")
          
          if (!concurrentArgs.isEmpty)
            throw new IllegalArgumentException("-P (concurrent) is not supported when runs in SBT, please use SBT parallel configuration instead.")
          
          if (!suffixes.isEmpty)
            throw new IllegalArgumentException("-q is not supported when runs in SBT, please use SBT's test-only or test filter instead.")

          testSortingReporterTimeout.getAndSet(Some(Span(parseDoubleArgument(testSortingReporterTimeouts, "-T", Suite.defaultTestSortingReporterTimeoutInSeconds), Seconds)))

          configMap.getAndSet(Some(parsePropertiesArgsIntoMap(propertiesArgs)))

          val tagsToInclude: Set[String] = parseCompoundArgIntoSet(tagsToIncludeArgs, "-n")
          val tagsToExclude: Set[String] = parseCompoundArgIntoSet(tagsToExcludeArgs, "-l")
          filter.getAndSet(Some(org.scalatest.Filter(if (tagsToInclude.isEmpty) None else Some(tagsToInclude), tagsToExclude)))
          membersOnly.getAndSet(Some(parseSuiteArgsIntoNameStrings(membersOnlyArgs, "-m")))
          wildcard.getAndSet(Some(parseSuiteArgsIntoNameStrings(wildcardArgs, "-w")))
          val slowpokeConfig: Option[SlowpokeConfig] = parseSlowpokeConfig(slowpokeArgs)
          //val (detectSlowpokes: Boolean, slowpokeDetectionDelay: Long, slowpokeDetectionPeriod: Long) =
          slowpokeConfig match {
            case Some(SlowpokeConfig(delayInMillis, periodInMillis)) => 
              detectSlowpokes.getAndSet(true)
              slowpokeDetectionDelay.getAndSet(delayInMillis)
              slowpokeDetectionPeriod.getAndSet(periodInMillis)
            case _ => 
              detectSlowpokes.getAndSet(false)
              slowpokeDetectionDelay.getAndSet(60000L)
              slowpokeDetectionPeriod.getAndSet(60000L)
          }

          val runnerCompanionClass = testLoader.loadClass("org.scalatest.tools.Runner$")
          val module = runnerCompanionClass.getField("MODULE$")
          val obj = module.get(runnerCompanionClass)
          val runnerInstance = obj.asInstanceOf[org.scalatest.tools.Runner.type]

          runnerInstance.spanScaleFactor = parseDoubleArgument(spanScaleFactors, "-F", 1.0)

          parseLongArgument(seedArgs, "-S") match {
            case Some(seed) => Seed.configuredRef.getAndSet(Some(seed))
            case None => // do nothing
          }
          
          val fullReporterConfigurations = parseReporterArgsIntoConfigurations(reporterArgs)
          val sbtNoFormat = java.lang.Boolean.getBoolean("sbt.log.noformat")
          
          fullReporterConfigurations.standardOutReporterConfiguration match {
            case Some(stdoutConfig) =>
              val configSet = stdoutConfig.configSet
              useStdout.getAndSet(true)
              presentAllDurations.getAndSet(configSet.contains(PresentAllDurations))
              presentInColor.getAndSet(!configSet.contains(PresentWithoutColor) && !sbtNoFormat)
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
              presentJson.getAndSet(configSet.contains(PresentJson))
            case None => 
              useStdout.getAndSet(reporterArgs.isEmpty)  // If no reporters specified, just give them a default stdout reporter
              presentAllDurations.getAndSet(false)
              presentInColor.getAndSet(!sbtNoFormat)
              presentShortStackTraces.getAndSet(false)
              presentFullStackTraces.getAndSet(false)
              presentUnformatted.getAndSet(false)
              presentReminder.getAndSet(false)
              presentReminderWithShortStackTraces.getAndSet(false)
              presentReminderWithFullStackTraces.getAndSet(false)
              presentReminderWithoutCanceledTests.getAndSet(false)
              presentJson.getAndSet(false)
          }
          
          fullReporterConfigurations.graphicReporterConfiguration match {
            case Some(g) => throw new IllegalArgumentException("Graphic reporter is not supported when runs in SBT.")
            case None => 
          }
          
          reporterConfigs.getAndSet(Some(fullReporterConfigurations.copy(standardOutReporterConfiguration = None)))
        }
        
        if (reporter.get.isEmpty || reporter.get.get.isDisposed) 
          reporter.getAndSet(Some(ReporterFactory.getDispatchReporter(reporterConfigs.get.get, None, None, testLoader, Some(resultHolder), detectSlowpokes.get, slowpokeDetectionDelay.get, slowpokeDetectionPeriod.get))) 
        
        val reporters =  
          if (useStdout.get)
            Vector(reporter.get.get, createSbtLogInfoReporter(loggers))
          else
            Vector(reporter.get.get)
            
        val dispatchReporter = new SbtDispatchReporter(reporters)
          
        (dispatchReporter, filter.get.get, configMap.get.get, membersOnly.get.get, wildcard.get.get, testSortingReporterTimeout.get.get)
      }
    
    private val atomicCount = new AtomicInteger(0)
  
    def increaseLatch(): Unit = {
      atomicCount.incrementAndGet()
    }
  
    def decreaseLatch(): Unit = {
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
          presentReminderWithoutCanceledTests.get,
          presentFilePathname.get,
          presentJson.get
        )
    }
  }

  /**
   * Returns an <code>org.scalatools.testing.Runner</code> that will load test classes via the passed <code>testLoader</code>
   * and direct output from running the tests to the passed array of <code>Logger</code>s.
   */
  def testRunner(testLoader: ClassLoader, loggers: Array[Logger]): ScalaTestRunner = {
    new ScalaTestRunner(testLoader, loggers)
  }
  
  private[scalatest] class SbtLogInfoReporter(
    loggers: Array[Logger],
    presentAllDurations: Boolean,
    presentInColor: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    presentUnformatted: Boolean,
    presentReminder: Boolean,
    presentReminderWithShortStackTraces: Boolean,
    presentReminderWithFullStackTraces: Boolean,
    presentReminderWithoutCanceledTests: Boolean,
    presentFilePathname: Boolean,
    presentJson: Boolean
  ) extends StringReporter(
    presentAllDurations,
    presentInColor,
    presentShortStackTraces,
    presentFullStackTraces,
    presentUnformatted,
    presentReminder,
    presentReminderWithShortStackTraces,
    presentReminderWithFullStackTraces,
    presentReminderWithoutCanceledTests,
    presentFilePathname,
    presentJson
  ) {

    protected def printPossiblyInColor(fragment: Fragment): Unit = {
      loggers.foreach { logger =>
        logger.info(fragment.toPossiblyColoredText(logger.ansiCodesSupported && presentInColor))
      }
    }

    protected def printNoColor(text: String): Unit = {
      loggers.foreach { logger =>
        logger.info(text)
      }
    }

    def dispose(): Unit = ()
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
    
    private def filterWildcard(paths: List[String], testClassName: String): Boolean = 
      paths.exists(testClassName.startsWith(_))
      
    private def filterMembersOnly(paths: List[String], testClassName: String): Boolean =
      paths.exists(path => testClassName.startsWith(path) && testClassName.substring(path.length ).lastIndexOf('.') <= 0)
      
    def run(testClassName: String, fingerprint: Fingerprint, eventHandler: EventHandler, args: Array[String]): Unit = {
      try {
        RunConfig.increaseLatch()
        val suiteClass = Class.forName(testClassName, true, testLoader)
        //println("sbt args: " + args.toList)
        if ((isAccessibleSuite(suiteClass) || isRunnable(suiteClass)) && isDiscoverableSuite(suiteClass)) {
          val (reporter, filter, configMap, membersOnly, wildcard, testSortingReporterTimeout) = RunConfig.getConfigurations(args, loggers, eventHandler, testLoader)
          
          if ((wildcard.isEmpty && membersOnly.isEmpty) || filterWildcard(wildcard, testClassName) || filterMembersOnly(membersOnly, testClassName)) {
          
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
            val suiteClassName = getSuiteClassName(suite)

            report(SuiteStarting(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suiteClassName), formatter, Some(TopOfClass(suiteClassName))))

            try {
              val status = suite.run(None, Args(report, Stopper.default, filter, configMap, None, tracker, false, None, None))

              val formatter = formatterForSuiteCompleted(suite)

              val duration = System.currentTimeMillis - suiteStartTime

              // Need to finish all before returning back to sbt.
              status.succeeds()

              status.unreportedException match {
                case Some(ue) =>
                  report(SuiteAborted(tracker.nextOrdinal(), ue.getMessage, suite.suiteName, suite.suiteId, Some(suiteClassName), Some(ue), Some(duration), formatter, Some(SeeStackDepthException)))

                case None =>
                  report(SuiteCompleted(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suiteClassName), Some(duration), formatter, Some(TopOfClass(suiteClassName))))
              }

            }
            catch {       
              case e: Exception => {

                // TODO: Could not get this from Resources. Got:
                // java.util.MissingResourceException: Can't find bundle for base name org.scalatest.ScalaTestBundle, locale en_US
                // TODO Chee Seng, I wonder why we couldn't access resources, and if that's still true. I'd rather get this stuff
                // from the resource file so we can later localize.
                val rawString = "Exception encountered when attempting to run a suite with class name: " + suiteClassName
                val formatter = formatterForSuiteAborted(suite, rawString)

                val duration = System.currentTimeMillis - suiteStartTime
                report(SuiteAborted(tracker.nextOrdinal(), rawString, suite.suiteName, suite.suiteId, Some(suiteClassName), Some(e), Some(duration), formatter, Some(SeeStackDepthException)))
              }
            }
          }
        } // I think we should do nothing for non accessible, non runnable or non discoverable suite. 
        //else throw new IllegalArgumentException("Class is not an accessible org.scalatest.Suite: " + testClassName)
      }
      catch {
        case t: Throwable => throw t // just rethrow it
      }
      finally {
        RunConfig.decreaseLatch()
      }
    }

    private val emptyClassArray = new Array[java.lang.Class[_]](0)
    
    private class SbtReporter(eventHandler: EventHandler, report: Option[Reporter]) extends Reporter {
      
      import org.scalatest.events._

      def fireEvent(tn: String, r: Result, e: Option[Throwable]): Unit = {
        eventHandler.handle(
          new org.scalatools.testing.Event {
            def testName = tn
            def description = tn
            def result = r
            def error = e getOrElse null
          }
        )
      }
      
      override def apply(event: Event): Unit = {
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
      
      def dispose(): Unit = {
        report match {
          case Some(report: DispatchReporter) => 
            report.dispatchDisposeAndWaitUntilDone()
          case _ =>
        }
      }
    }
  }
}
