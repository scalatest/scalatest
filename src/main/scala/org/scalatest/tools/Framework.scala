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

import sbt.testing.{Event => SbtEvent, Framework => SbtFramework, Status => SbtStatus, Runner => SbtRunner, _}
import org.scalatest._
import SuiteDiscoveryHelper._
import Suite.formatterForSuiteStarting
import Suite.formatterForSuiteCompleted
import Suite.formatterForSuiteAborted
import org.scalatest.events._
import Runner.parsePropertiesArgsIntoMap
import Runner.parseCompoundArgIntoSet
import Runner.SELECTED_TAG
import Runner.mergeMap
import Runner.parseSuiteArgsIntoNameStrings
import Runner.parseChosenStylesIntoChosenStyleSet
import Runner.parseArgs
import Runner.parseDoubleArgument
import Runner.parseSlowpokeConfig
import java.io.{StringWriter, PrintWriter}
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicInteger, AtomicBoolean}
import scala.collection.JavaConverters._
import StringReporter.fragmentsForEvent

/**
 * <p>
 * This class is ScalaTest's implementation of the new Framework API that is supported in sbt 0.13.
 * </p>
 *
 * <p>
 * To use ScalaTest in SBT, you should add ScalaTest as dependency in your SBT build file, the following shows an example
 * for using ScalaTest 2.0 with Scala 2.10.x project:
 * </p>
 *
 * <pre class="stHighlight">
 * "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
 * </pre>
 *
 * <p>
 * To pass argument to ScalaTest from SBT, you can use <code>testOptions</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * testOptions in Test += Tests.Argument("-h", "target/html")  // Use HtmlReporter
 * </pre>
 *
 * <p>
 * If you are using multiple testing frameworks, you can pass arguments specific to ScalaTest only:
 * </p>
 *
 * <pre class="stHighlight">
 * testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/html") // Use HtmlReporter
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
 *   <li><code>-p</code>, <code>-R</code> -- runpath is not supported because test path and discovery is handled by SBT</li>
 *   <li><code>-s</code> -- suite is not supported because SBT's <code>test-only</code> serves the similar purpose</li>
 *   <li><code>-A</code> -- again is not supported because SBT's <code>test-quick</code> serves the similar purpose</li>
 *   <li><code>-j</code> -- junit is not supported because in SBT different test framework should be supported by its corresponding <code>Framework</code> implementation</li>
 *   <li><code>-b</code> -- testng is not supported because in SBT different test framework should be supported by its corresponding <code>Framework</code> implementation</li>
 *   <li><code>-c</code>, <code>-P</code> -- concurrent/parallel is not supported because parallel execution is controlled by SBT.</li>
 *   <li><code>-q</code> is not supported because test discovery should be handled by SBT, and SBT's test-only or test filter serves the similar purpose</li>
 *   <li><code>-T</code> is not supported because correct ordering of text output is handled by SBT</li>
 *   <li><code>-g</code> is not supported because current Graphic Reporter implementation works differently than standard reporter</li>
 * </ul>
 *
 * <h3>New Features of New Framework API</h3>
 *
 * <p>
 * <a href="https://github.com/sbt/test-interface">New Framework API</a> supports a number of new features that ScalaTest has utilized to support a better testing
 * experience in SBT.  The followings are summary of new features supported by the new Framework API:
 * </p>
 *
 * <ul>
 *   <li>Specified behavior of single instance of <code>Runner</code> per project run (non-fork), and a new <code>done</code> method</li>
 *   <li>API to return nested tasks</li>
 *   <li>API to support test execution in <code>fork</code> mode</li>
 *   <li>Selector API to selectively run tests</li>
 *   <li>Added new <code>Ignored</code>, <code>Canceled</code> and <code>Pending</code> status</li>
 *   <li>Added SBT Tagging support</li>
 * </ul>
 *
 * <h3>Specified behavior of single instance of <code>Runner</code> per project run (non-fork), and a new <code>done</code> method</h3>
 *
 * <p>
 * In new Framework API, it is now a specified behavior that <code>Framework</code>'s <code>runner</code> method will be called
 * to get a <code>Runner</code> instance once per project run.  Arguments will be passed when calling <code>Framework</code>'s <code>runner</code>
 * and this gives ScalaTest a good place to perform setup tasks, such as initializing <code>Reporter</code>s.
 * </p>
 *
 * <p>
 * There's also a new <code>done</code> on <code>Runner</code> interface, which in turns provide a good spot for ScalaTest to perform
 * cleanup tasks, such as disposing the <code>Reporter</code>s.  [[org.scalatest.tools.HtmlReporter <code>HtmlReporter</code>]] depends
 * on this behavior to generate its <code>index.html</code>.  In addition, <code>done</code> can return framework-specific summary text
 * for SBT to render at the end of the project run, which allows ScalaTest to return its own summary text.
 * </p>
 *
 * <h3>API to return nested tasks</h3>
 *
 * <p>
 * In SBT version before 0.13, ScalaTest's nested suites are always executed sequentially regardless of <code>parallelExecution</code> value.
 * In new Framework API, a new concept of <a href="https://github.com/sbt/test-interface/blob/master/src/main/java/sbt/testing/Task.java"><code>Task</code></a>
 * is introduced, which its <code>execute</code> method can return more (nested) <code>Task</code>s for execution.  When <code>parallelExecution</code>
 * is set to <code>true</code> (the default), SBT will execute the nested tasks in parallel.
 * </p>
 *
 * <p>
 * Each <code>Suite</code> in ScalaTest now maps to a <code>Task</code> in SBT, and its nested suites are returned as nested <code>Task</code>s.
 * This enables parallel execution of nested suites in SBT.
 * </p>
 *
 * <h3>API to support test execution in <code>fork</code> mode</h3>
 *
 * <p>
 * Forking was added to SBT since version 0.12, you can find documentation for forking support in SBT at <a href="http://www.scala-sbt.org/0.13.0/docs/Detailed-Topics/Forking.html">Forking in SBT</a>.
 * </p>
 *
 * <p>
 * Although forking is already available in SBT since 0.12, there's no support in old Framework API, until it is added in new Framework API that is supported in
 * SBT 0.13.  With API provided with new Framework API, ScalaTest creates real <code>Reporter</code>s in the main process, and uses <code>SocketReporter</code>
 * in forked process to send events back to the main process, and get processed by real <code>Reporter</code>s at the main process.  All of this is transparent
 * to any custom <code>Reporter</code> implementation, as only one instance of the custom <code>Reporter</code> will be created to process the events, regardless
 * of whether the tests run in same or forked process.
 * </p>
 *
 * <h3>Selector API to selectively run tests</h3>
 *
 * <p>
 * New Framework API includes a set of comprehensive API to select tests for execution.  Though new Framework API supports fine-grained test selection, current
 * SBT's <code>test-only</code> and <code>test-quick</code> supports up to suite level selection only, or <code>SuiteSelector</code> as defined in new Framework API.
 * This <code>Framework</code> implementation already supports <code>SuiteSelector</code>, <code>NestedSuiteSelector</code>, <code>TestSelector</code> and
 * <code>NestedTestSelector</code>, which should work once future SBT version supports them.
 * </p>
 *
 * <h3>Added new <code>Ignored</code>, <code>Canceled</code> and <code>Pending</code> status</h3>
 *
 * <p>
 * Status <code>Ignored</code>, <code>Canceled</code> and <code>Pending</code> are added to new Framework API, and they match perfectly with ScalaTest's ignored
 * tests (now reported as <code>Ignored</code> instead of <code>Skipped</code>), as well as canceled and pending tests newly added in ScalaTest 2.0.
 * </p>
 *
 * <h3>Added SBT Tagging support</h3>
 *
 * <p>
 * SBT supports <a href="http://www.scala-sbt.org/release/docs/Detailed-Topics/Parallel-Execution.html#tagging-tasks">task tagging</a>, but has no support in old
 * Framework API for test frameworks to integrate it.  New Framework API supports it, and you can now use the following annotations to annotate your suite for SBT
 * built-in resource tags:
 * </p>
 *
 * <ul>
 *   <li>[[org.scalatest.tags.CPU <code>CPU</code>]]</li>
 *   <li>[[org.scalatest.tags.Disk <code>Disk</code>]]</li>
 *   <li>[[org.scalatest.tags.Network <code>Network</code>]]</li>
 * </ul>
 *
 * <p>
 * They will be mapped to corresponding resource tag <code>CPU</code>, <code>Disk</code> and <code>Network</code> in SBT.
 * </p>
 *
 * <p>
 * You can also define custom tag, which you'll need to write it as Java annotation:
 * </p>
 *
 * <pre class="stHighlight">
 * import java.lang.annotation.Target;
 * import java.lang.annotation.Retention;
 * import org.scalatest.TagAnnotation;
 *
 * @TagAnnotation("custom")
 * @Retention(RetentionPolicy.RUNTIME)
 * @Target({ElementType.TYPE})
 * public @interface Custom {}
 * </pre>
 *
 * <p>
 * which will be translated to <code>Tags.Tag("custom")</code> in SBT.
 * </p>
 *
 * @author Chee Seng
 */
class Framework extends SbtFramework {

  /**
   * Test framework name.
   *
   * @return <code>ScalaTest</code>
   */
  def name = "ScalaTest"
 
  private val resultHolder = new SuiteResultHolder()

  /**
   * An array of <code>Fingerprint</code></a>s that specify how to identify ScalaTest's test classes during
   * discovery.
   *
   * @return <code>SubclassFingerprint</code> for <code>org.scalatest.Suite</code> and <code>AnnotatedFingerprint</code> for <code>org.scalatest.WrapWith</code>
   *
   */
  def fingerprints =
    Array(
      new SubclassFingerprint {
        def superclassName = "org.scalatest.Suite"
        def isModule = false
        def requireNoArgConstructor = true
      }, 
      new AnnotatedFingerprint {
        def annotationName = "org.scalatest.WrapWith"
        def isModule = false
      })

  private class RecordingDistributor(
    taskDefinition: TaskDef, 
    rerunSuiteId: String,
    originalReporter: Reporter,
    args: Args,
    loader: ClassLoader,
    tagsToInclude: Set[String],
    tagsToExclude: Set[String],
    selectors: Array[Selector],
    explicitlySpecified: Boolean, 
    configMap: ConfigMap, 
    summaryCounter: SummaryCounter,
    useSbtLogInfoReporter: Boolean,
    presentAllDurations: Boolean,
    presentInColor: Boolean,
    presentShortStackTraces: Boolean, 
    presentFullStackTraces: Boolean,
    presentUnformatted: Boolean,
    presentReminder: Boolean,
    presentReminderWithShortStackTraces: Boolean,
    presentReminderWithFullStackTraces: Boolean,
    presentReminderWithoutCanceledTests: Boolean
  ) extends Distributor {

    private val taskQueue = new LinkedBlockingQueue[Task]()

    def apply(suite: Suite, tracker: Tracker) {
      apply(suite, args.copy(tracker = tracker))
    }

    def apply(suite: Suite, args: Args): Status = {
      if (suite == null)
        throw new NullPointerException("suite is null")
      if (args == null)
        throw new NullPointerException("args is null")
      val status = new ScalaTestStatefulStatus
      val nestedTask =
        new ScalaTestNestedTask(
          taskDefinition,
          rerunSuiteId,
          suite,
          loader,
          originalReporter,
          args.tracker,
          tagsToInclude,
          tagsToExclude,
          selectors,
          explicitlySpecified, 
          configMap,
          summaryCounter,
          status,
          useSbtLogInfoReporter, 
          presentAllDurations,
          presentInColor,
          presentShortStackTraces,
          presentFullStackTraces,
          presentUnformatted,
          presentReminder,
          presentReminderWithShortStackTraces,
          presentReminderWithFullStackTraces,
          presentReminderWithoutCanceledTests
        )
      taskQueue.put(nestedTask)
      status
    }
    
    def nestedTasks: Array[Task] = 
      taskQueue.asScala.toArray
  }
  
  private def createTaskDispatchReporter(
    reporter: Reporter,
    loggers: Array[Logger],
    loader: ClassLoader,
    useSbtLogInfoReporter: Boolean,
    presentAllDurations: Boolean,
    presentInColor: Boolean, 
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    presentUnformatted: Boolean,
    presentReminder: Boolean,
    presentReminderWithShortStackTraces: Boolean,
    presentReminderWithFullStackTraces: Boolean,
    presentReminderWithoutCanceledTests: Boolean, 
    summaryCounter: SummaryCounter
  ) = {
    val reporters = 
      if (useSbtLogInfoReporter) {
        val sbtLogInfoReporter = 
          new SbtLogInfoReporter(
            loggers, 
            presentAllDurations,
            presentInColor,
            presentShortStackTraces,
            presentFullStackTraces, // If they say both S and F, F overrules
            presentUnformatted,
            presentReminder,
            presentReminderWithShortStackTraces,
            presentReminderWithFullStackTraces,
            presentReminderWithoutCanceledTests, 
            summaryCounter
          )
        Vector(reporter, sbtLogInfoReporter)
      }
      else 
        Vector(reporter)
    new SbtDispatchReporter(reporters)
  }
      
  private def runSuite(
    taskDefinition: TaskDef,
    rerunSuiteId: String,
    suite: Suite,
    loader: ClassLoader,
    reporter: Reporter,
    tracker: Tracker,
    eventHandler: EventHandler, 
    tagsToInclude: Set[String],
    tagsToExclude: Set[String],
    selectors: Array[Selector],
    explicitlySpecified: Boolean, 
    configMap: ConfigMap,
    summaryCounter: SummaryCounter,
    statefulStatus: Option[ScalaTestStatefulStatus], 
    loggers: Array[Logger],
    useSbtLogInfoReporter: Boolean,
    presentAllDurations: Boolean,
    presentInColor: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean, 
    presentUnformatted: Boolean,
    presentReminder: Boolean,
    presentReminderWithShortStackTraces: Boolean,
    presentReminderWithFullStackTraces: Boolean,
    presentReminderWithoutCanceledTests: Boolean
  ): Array[Task] = {
    val suiteStartTime = System.currentTimeMillis
    val suiteClass = suite.getClass
    val report = new SbtReporter(rerunSuiteId, taskDefinition.fullyQualifiedName, taskDefinition.fingerprint, eventHandler, reporter, summaryCounter)
    val formatter = formatterForSuiteStarting(suite)
        
    val filter = 
      if ((selectors.length == 1 && selectors(0).isInstanceOf[SuiteSelector] && !explicitlySpecified))  // selectors will always at least have one SuiteSelector, according to javadoc of TaskDef
        Filter(if (tagsToInclude.isEmpty) None else Some(tagsToInclude), tagsToExclude)
      else {
        var suiteTags = Map[String, Set[String]]()
        var testTags = Map[String, Map[String, Set[String]]]()
        var hasTest = false
        var hasNested = false
            
        selectors.foreach { selector => 
          selector match {
            case suiteSelector: SuiteSelector => 
              suiteTags = mergeMap[String, Set[String]](List(suiteTags, Map(suite.suiteId -> Set(SELECTED_TAG)))) { _ ++ _ }
            case testSelector: TestSelector =>
              testTags = mergeMap[String, Map[String, Set[String]]](List(testTags, Map(suite.suiteId -> Map(testSelector.testName -> Set(SELECTED_TAG))))) { (testMap1, testMap2) => 
                mergeMap[String, Set[String]](List(testMap1, testMap2)) { _ ++ _}
              }
              hasTest = true
            case testWildcardSelector: TestWildcardSelector =>
              val filteredTestNames = suite.testNames.filter(_.contains(testWildcardSelector.testWildcard))
              val selectorTestTags = Map.empty ++ filteredTestNames.map(_ -> Set(SELECTED_TAG))
              testTags = mergeMap[String, Map[String, Set[String]]](List(testTags, Map(suite.suiteId -> selectorTestTags))) { (testMap1, testMap2) =>
                mergeMap[String, Set[String]](List(testMap1, testMap2)) { _ ++ _}
              }
              hasTest = true
            case nestedSuiteSelector: NestedSuiteSelector => 
              suiteTags = mergeMap[String, Set[String]](List(suiteTags, Map(nestedSuiteSelector.suiteId -> Set(SELECTED_TAG)))) { _ ++ _ }
              hasNested = true
            case nestedTestSelector: NestedTestSelector => 
              testTags = mergeMap[String, Map[String, Set[String]]](List(testTags, Map(nestedTestSelector.suiteId -> Map(nestedTestSelector.testName -> Set(SELECTED_TAG))))) { (testMap1, testMap2) => 
                mergeMap[String, Set[String]](List(testMap1, testMap2)) { _ ++ _}
              }
              hasNested = true
          }
        }
        // Only exclude nested suites when using -s XXX -t XXXX, same behaviour with Runner.
        val excludeNestedSuites = hasTest && !hasNested 
        Filter(if (tagsToInclude.isEmpty) Some(Set(SELECTED_TAG)) else Some(tagsToInclude + SELECTED_TAG), tagsToExclude, false, new DynaTags(suiteTags.toMap, testTags.toMap))
      }

    report(SuiteStarting(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suiteClass.getName), formatter, Some(TopOfClass(suiteClass.getName))))

    val args = Args(report, Stopper.default, filter, configMap, None, tracker, Set.empty)
    val distributor =
      new RecordingDistributor(
        taskDefinition, 
        rerunSuiteId,
        reporter,
        args,
        loader,
        tagsToInclude,
        tagsToExclude,
        selectors,
        explicitlySpecified, 
        configMap,
        summaryCounter,
        useSbtLogInfoReporter,
        presentAllDurations,
        presentInColor,
        presentShortStackTraces, 
        presentFullStackTraces,
        presentUnformatted,
        presentReminder,
        presentReminderWithShortStackTraces,
        presentReminderWithFullStackTraces,
        presentReminderWithoutCanceledTests
      )
    
    try {
      
      val status = suite.run(None, args.copy(distributor = Some(distributor)))
      val formatter = formatterForSuiteCompleted(suite)
      val duration = System.currentTimeMillis - suiteStartTime

      report(SuiteCompleted(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suiteClass.getName), Some(duration), formatter, Some(TopOfClass(suiteClass.getName))))
      
      statefulStatus match {
        case Some(s) => 
          s.setFailed()
        case None => // Do nothing
      }
    }
    catch {       
      case e: RuntimeException => {

        // TODO: Could not get this from Resources. Got:
        // java.util.MissingResourceException: Can't find bundle for base name org.scalatest.ScalaTestBundle, locale en_US
        // TODO Chee Seng, I wonder why we couldn't access resources, and if that's still true. I'd rather get this stuff
        // from the resource file so we can later localize.
        val rawString = "Exception encountered when attempting to run a suite with class name: " + suiteClass.getName
        val formatter = formatterForSuiteAborted(suite, rawString)

        val duration = System.currentTimeMillis - suiteStartTime
        report(SuiteAborted(tracker.nextOrdinal(), rawString, suite.suiteName, suite.suiteId, Some(suiteClass.getName), Some(e), Some(duration), formatter, Some(SeeStackDepthException)))
        
        statefulStatus match {
          case Some(s) => s.setFailed()
          case None => // Do nothing
        }
      }
    }
    finally {
      statefulStatus match {
        case Some(s) => s.setCompleted()
        case None => // Do nothing
      }
    }
    
    distributor.nestedTasks
  }
  
  private class ScalaTestNestedTask(
    taskDefinition: TaskDef, 
    rerunSuiteId: String,
    suite: Suite,
    loader: ClassLoader,
    reporter: Reporter,
    tracker: Tracker,
    tagsToInclude: Set[String],
    tagsToExclude: Set[String], 
    selectors: Array[Selector],
    explicitlySpecified: Boolean, 
    configMap: ConfigMap,
    summaryCounter: SummaryCounter,
    statefulStatus: ScalaTestStatefulStatus,
    useSbtLogInfoReporter: Boolean, 
    presentAllDurations: Boolean,
    presentInColor: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    presentUnformatted: Boolean,
    presentReminder: Boolean,
    presentReminderWithShortStackTraces: Boolean,
    presentReminderWithFullStackTraces: Boolean,
    presentReminderWithoutCanceledTests: Boolean
  ) extends Task {
    
    def tags = 
      for { 
        a <- suite.getClass.getDeclaredAnnotations
        annotationClass = a.annotationType
        if (annotationClass.isAnnotationPresent(classOf[TagAnnotation]) || annotationClass.isAssignableFrom(classOf[TagAnnotation])) 
      } yield {
        val value = 
          if (a.isInstanceOf[TagAnnotation])
            a.asInstanceOf[TagAnnotation].value
          else
            annotationClass.getAnnotation(classOf[TagAnnotation]).value
        if (value == "")
          annotationClass.getName
        else
          value
      }
    
    def execute(eventHandler: EventHandler, loggers: Array[Logger]) = {
      runSuite(
        taskDefinition,
        rerunSuiteId,
        suite,
        loader,
        reporter,
        tracker,
        eventHandler,
        tagsToInclude,
        tagsToExclude,
        selectors,
        explicitlySpecified, 
        configMap,
        summaryCounter,
        Some(statefulStatus), 
        loggers,
        useSbtLogInfoReporter,
        presentAllDurations,
        presentInColor,
        presentShortStackTraces,
        presentFullStackTraces,
        presentUnformatted,
        presentReminder,
        presentReminderWithShortStackTraces,
        presentReminderWithFullStackTraces,
        presentReminderWithoutCanceledTests
      )
    }
    
    def taskDef = taskDefinition
  }
      
  private class ScalaTestTask(
    taskDefinition: TaskDef, 
    loader: ClassLoader,
    reporter: Reporter,
    tracker: Tracker,
    tagsToInclude: Set[String], 
    tagsToExclude: Set[String],
    selectors: Array[Selector],
    explicitlySpecified: Boolean, 
    configMap: ConfigMap, 
    summaryCounter: SummaryCounter,
    useSbtLogInfoReporter: Boolean,
    presentAllDurations: Boolean,
    presentInColor: Boolean, 
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    presentUnformatted: Boolean,
    presentReminder: Boolean,
    presentReminderWithShortStackTraces: Boolean,
    presentReminderWithFullStackTraces: Boolean,
    presentReminderWithoutCanceledTests: Boolean
  ) extends Task {
    
    def loadSuiteClass = {
      try {
        Class.forName(taskDefinition.fullyQualifiedName, true, loader)
      }
      catch {
        case e: Exception => 
          throw new IllegalArgumentException("Unable to load class: " + taskDefinition.fullyQualifiedName)
      }
    }
    
    lazy val suiteClass = loadSuiteClass
    lazy val accessible = isAccessibleSuite(suiteClass)
    lazy val runnable = isRunnable(suiteClass)
    lazy val shouldDiscover = 
      taskDefinition.explicitlySpecified || ((accessible || runnable) && isDiscoverableSuite(suiteClass))
    
    def tags = 
      for { 
        a <- suiteClass.getDeclaredAnnotations
        annotationClass = a.annotationType
        if (annotationClass.isAnnotationPresent(classOf[TagAnnotation]) || annotationClass.isAssignableFrom(classOf[TagAnnotation])) 
      } yield {
        val value = 
          if (a.isInstanceOf[TagAnnotation])
            a.asInstanceOf[TagAnnotation].value
          else
            annotationClass.getAnnotation(classOf[TagAnnotation]).value
        if (value == "")
          annotationClass.getName
        else
          value
      }
    
    def execute(eventHandler: EventHandler, loggers: Array[Logger]) = {
      if (accessible || runnable) {
        val suite = 
          if (accessible)
            suiteClass.newInstance.asInstanceOf[Suite]
          else {
            val wrapWithAnnotation = suiteClass.getAnnotation(classOf[WrapWith])
            val suiteClazz = wrapWithAnnotation.value
            val constructorList = suiteClazz.getDeclaredConstructors()
            val constructor = constructorList.find { c => 
              val types = c.getParameterTypes
              types.length == 1 && types(0) == classOf[java.lang.Class[_]]
            }
            constructor.get.newInstance(suiteClass).asInstanceOf[Suite]
          }
        
        val taskReporter =
          createTaskDispatchReporter(
            reporter,
            loggers,
            loader,
            useSbtLogInfoReporter,
            presentAllDurations,
            presentInColor,
            presentShortStackTraces, 
            presentFullStackTraces,
            presentUnformatted,
            presentReminder,
            presentReminderWithShortStackTraces,
            presentReminderWithFullStackTraces,
            presentReminderWithoutCanceledTests, 
            summaryCounter
          )

        runSuite(
          taskDefinition,
          suite.suiteId,
          suite,
          loader,
          taskReporter,
          tracker,
          eventHandler,
          tagsToInclude,
          tagsToExclude,
          selectors,
          explicitlySpecified, 
          configMap,
          summaryCounter,
          None, 
          loggers,
          useSbtLogInfoReporter,
          presentAllDurations,
          presentInColor,
          presentShortStackTraces,
          presentFullStackTraces,
          presentUnformatted,
          presentReminder,
          presentReminderWithShortStackTraces,
          presentReminderWithFullStackTraces,
          presentReminderWithoutCanceledTests
        )
      }
       else 
         throw new IllegalArgumentException("Class " + taskDefinition.fullyQualifiedName + " is neither accessible accesible org.scalatest.Suite nor runnable.")
    }
    
    def taskDef = taskDefinition
  }
  
  private[tools] class SummaryCounter {
    val testsSucceededCount, testsFailedCount, testsIgnoredCount, testsPendingCount, testsCanceledCount, suitesCompletedCount, suitesAbortedCount, scopesPendingCount = new AtomicInteger
    val reminderEventsQueue = new LinkedBlockingQueue[ExceptionalEvent]
    
    def incrementTestsSucceededCount() { 
      testsSucceededCount.incrementAndGet() 
    }
    
    def incrementTestsFailedCount() {
      testsFailedCount.incrementAndGet()
    }
    
    def incrementTestsIgnoredCount() {
      testsIgnoredCount.incrementAndGet()
    }
    
    def incrementTestsPendingCount() {
      testsPendingCount.incrementAndGet()
    }
    
    def incrementTestsCanceledCount() {
      testsCanceledCount.incrementAndGet()
    }
    
    def incrementSuitesCompletedCount() {
      suitesCompletedCount.incrementAndGet()
    }
    
    def incrementSuitesAbortedCount() {
      suitesAbortedCount.incrementAndGet()
    }
    
    def incrementScopesPendingCount() {
      scopesPendingCount.incrementAndGet()
    }
    
    def recordReminderEvents(events: ExceptionalEvent) {
      reminderEventsQueue.put(events)
    }
  }
  
  private class SbtLogInfoReporter(
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
    summaryCounter: SummaryCounter
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
    
    override def apply(event: Event) {
      event match {
        case ee: ExceptionalEvent if presentReminder =>
          if (!presentReminderWithoutCanceledTests || event.isInstanceOf[TestFailed]) {
            summaryCounter.recordReminderEvents(ee)
          }
        case _ =>
      }
      fragmentsForEvent(
        event,
        presentUnformatted,
        presentAllDurations,
        presentShortStackTraces,
        presentFullStackTraces,
        presentReminder,
        presentReminderWithShortStackTraces,
        presentReminderWithFullStackTraces,
        presentReminderWithoutCanceledTests,
        reminderEventsBuf
      ) foreach printPossiblyInColor
    }

    def dispose() = ()
  }
  
  private[scalatest] class ScalaTestRunner(
    runArgs: Array[String],
    loader: ClassLoader,
    tagsToInclude: Set[String],
    tagsToExclude: Set[String],
    membersOnly: List[String], 
    wildcard: List[String], 
    configMap: ConfigMap, 
    repConfig: ReporterConfigurations,
    useSbtLogInfoReporter: Boolean,
    presentAllDurations: Boolean,
    presentInColor: Boolean, 
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    presentUnformatted: Boolean,
    presentReminder: Boolean,
    presentReminderWithShortStackTraces: Boolean,
    presentReminderWithFullStackTraces: Boolean,
    presentReminderWithoutCanceledTests: Boolean, 
    detectSlowpokes: Boolean,
    slowpokeDetectionDelay: Long,
    slowpokeDetectionPeriod: Long
  ) extends sbt.testing.Runner {  
    val isDone = new AtomicBoolean(false)
    val tracker = new Tracker
    val summaryCounter = new SummaryCounter
    val runStartTime = System.currentTimeMillis
    
    val dispatchReporter = ReporterFactory.getDispatchReporter(repConfig, None, None, loader, Some(resultHolder), detectSlowpokes, slowpokeDetectionDelay, slowpokeDetectionPeriod) 
    
    dispatchReporter(RunStarting(tracker.nextOrdinal(), 0, configMap))
    
    private def createTask(td: TaskDef): ScalaTestTask = 
      new ScalaTestTask(
          td, 
          loader,
          dispatchReporter,
          tracker,
          tagsToInclude,
          tagsToExclude,
          td.selectors,
          td.explicitlySpecified, 
          configMap,
          summaryCounter, 
          useSbtLogInfoReporter,
          presentAllDurations,
          presentInColor,
          presentShortStackTraces,
          presentFullStackTraces,
          presentUnformatted,
          presentReminder,
          presentReminderWithShortStackTraces,
          presentReminderWithFullStackTraces,
          presentReminderWithoutCanceledTests
        )
    
    private def filterWildcard(paths: List[String], taskDefs: Array[TaskDef]): Array[TaskDef] = 
      taskDefs.filter(td => paths.exists(td.fullyQualifiedName.startsWith(_)))
      
    private def filterMembersOnly(paths: List[String], taskDefs: Array[TaskDef]): Array[TaskDef] =
      taskDefs.filter { td =>
        paths.exists(path => td.fullyQualifiedName.startsWith(path) && td.fullyQualifiedName.substring(path.length).lastIndexOf('.') <= 0)
      }
      
    def tasks(taskDefs: Array[TaskDef]): Array[Task] = 
      for { 
        taskDef <- if (wildcard.isEmpty && membersOnly.isEmpty) taskDefs else (filterWildcard(wildcard, taskDefs) ++ filterMembersOnly(membersOnly, taskDefs)).distinct
        val task = createTask(taskDef)
        if task.shouldDiscover
      } yield task
    
    def done = {
      if (!isDone.getAndSet(true)) {
        val duration = System.currentTimeMillis - runStartTime
        val summary = new Summary(summaryCounter.testsSucceededCount.get, summaryCounter.testsFailedCount.get, summaryCounter.testsIgnoredCount.get, summaryCounter.testsPendingCount.get, 
                                  summaryCounter.testsCanceledCount.get, summaryCounter.suitesCompletedCount.get, summaryCounter.suitesAbortedCount.get, summaryCounter.scopesPendingCount.get)
        dispatchReporter(RunCompleted(tracker.nextOrdinal(), Some(duration), Some(summary)))
        dispatchReporter.dispatchDisposeAndWaitUntilDone()
        val fragments: Vector[Fragment] =
          StringReporter.summaryFragments(
            true,
            Some(duration),
            Some(summary),
            Vector.empty ++ summaryCounter.reminderEventsQueue.asScala,
            presentAllDurations,
            presentReminder,
            presentReminderWithShortStackTraces,
            presentReminderWithFullStackTraces,
            presentReminderWithoutCanceledTests
          ) 
        fragments.map(_.toPossiblyColoredText(presentInColor)).mkString("\n")
      }
      else
        throw new IllegalStateException("done method is called twice")
    }

    def args = runArgs

    def remoteArgs: Array[String] = {
      import java.net.{ServerSocket, InetAddress}
      import java.io.{ObjectInputStream, ObjectOutputStream}
      import org.scalatest.events._
      
      class Skeleton extends Runnable {
        
        val server = new ServerSocket(0)
        
        def run() {
          val socket = server.accept()
          val is = new ObjectInputStream(socket.getInputStream)

          try {
			(new React(is)).react()
          } 
          finally {
            is.close()	
            socket.close()
		  }
        }
        
        class React(is: ObjectInputStream) {
          @annotation.tailrec 
          final def react() { 
            val event = is.readObject
            event match {
              case e: TestStarting => 
                dispatchReporter(e) 
                react()
              case e: TestSucceeded => 
                dispatchReporter(e) 
                summaryCounter.incrementTestsSucceededCount()
                react()
              case e: TestFailed => 
                dispatchReporter(e) 
                summaryCounter.incrementTestsFailedCount()
                react()
              case e: TestIgnored => 
                dispatchReporter(e)
                summaryCounter.incrementTestsIgnoredCount()
                react()
              case e: TestPending => 
                dispatchReporter(e)
                summaryCounter.incrementTestsPendingCount()
                react()
              case e: TestCanceled => 
                dispatchReporter(e)
                summaryCounter.incrementTestsCanceledCount()
                react()
              case e: SuiteStarting => 
                dispatchReporter(e)
                react()
              case e: SuiteCompleted => 
                dispatchReporter(e)
                summaryCounter.incrementSuitesCompletedCount()
                react()
              case e: SuiteAborted => 
                dispatchReporter(e)
                summaryCounter.incrementSuitesAbortedCount()
                react()
              case e: ScopeOpened => dispatchReporter(e); react()
              case e: ScopeClosed => dispatchReporter(e); react()
              case e: ScopePending => 
                dispatchReporter(e)
                summaryCounter.incrementScopesPendingCount()
                react()
              case e: InfoProvided => dispatchReporter(e); react()
              case e: MarkupProvided => dispatchReporter(e); react()
              case e: AlertProvided => dispatchReporter(e); react()
              case e: UpdateProvided => dispatchReporter(e); react()
              case e: RunStarting => react() // just ignore test starting and continue
              case e: RunCompleted => // Sub-process completed, just let the thread terminate
              case e: RunStopped => dispatchReporter(e)
              case e: RunAborted => dispatchReporter(e)
	        }
          }
        }
        
        def host: String = server.getLocalSocketAddress.toString
        def port: Int = server.getLocalPort
      }
      
      val skeleton = new Skeleton()
      val thread = new Thread(skeleton)
      thread.start()
      Array(InetAddress.getLocalHost.getHostAddress, skeleton.port.toString)
    }
  }

  /**
   *
   * Initiates a ScalaTest run.
   *
   * @param args the ScalaTest arguments for the new run
   * @param remoteArgs the ScalaTest remote arguments for the run in a forked JVM
   * @param testClassLoader a class loader to use when loading test classes during the run
   * @return a <code>Runner</code> implementation representing the newly started run to run ScalaTest's tests.
   * @throws IllegalArgumentException when invalid or unsupported argument is passed
   */
  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): SbtRunner = {

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
      chosenStyles, 
      spanScaleFactors, 
      testSortingReporterTimeouts,
      slowpokeArgs
    ) = parseArgs(FriendlyParamsTranslator.translateArguments(args))
    
    if (!runpathArgs.isEmpty)
      throw new IllegalArgumentException("-p, -R (runpath) is not supported when runs in SBT.")
               
    if (!suiteArgs.isEmpty)
      throw new IllegalArgumentException("-s (suite) is not supported when runs in SBT, please use SBT's test-only instead.")
    
    if (!againArgs.isEmpty)
      throw new IllegalArgumentException("-A is not supported when runs in SBT, please use SBT's test-quick instead.")
    
    if (!junitArgs.isEmpty)
      throw new IllegalArgumentException("-j (junit) is not supported when runs in SBT.")
    
    if (!testNGArgs.isEmpty)
      throw new IllegalArgumentException("-b (testng) is not supported when runs in SBT.")
    
    if (!concurrentArgs.isEmpty)
      throw new IllegalArgumentException("-c, -P (concurrent) is not supported when runs in SBT, please use SBT parallel configuration instead.")
    
    if (!suffixes.isEmpty)
      throw new IllegalArgumentException("-q is not supported when runs in SBT, please use SBT's test-only or test filter instead.")
    
    if (!testSortingReporterTimeouts.isEmpty)
      throw new IllegalArgumentException("-T is not supported when runs in SBT.")
    
    val propertiesMap = parsePropertiesArgsIntoMap(propertiesArgs)
    val chosenStyleSet: Set[String] = parseChosenStylesIntoChosenStyleSet(chosenStyles, "-y")
    if (propertiesMap.isDefinedAt(Runner.CHOSEN_STYLES))
      throw new IllegalArgumentException("Property name '" + Runner.CHOSEN_STYLES + "' is used by ScalaTest, please choose other property name.")
    val configMap: ConfigMap = 
      if (chosenStyleSet.isEmpty)
        propertiesMap
      else
        propertiesMap + (Runner.CHOSEN_STYLES -> chosenStyleSet)
      
    val tagsToInclude: Set[String] = parseCompoundArgIntoSet(tagsToIncludeArgs, "-n")
    val tagsToExclude: Set[String] = parseCompoundArgIntoSet(tagsToExcludeArgs, "-l")
    val membersOnly: List[String] = parseSuiteArgsIntoNameStrings(membersOnlyArgs, "-m")
    val wildcard: List[String] = parseSuiteArgsIntoNameStrings(wildcardArgs, "-w")
    val slowpokeConfig: Option[SlowpokeConfig] = parseSlowpokeConfig(slowpokeArgs)
    val (detectSlowpokes: Boolean, slowpokeDetectionDelay: Long, slowpokeDetectionPeriod: Long) =
      slowpokeConfig match {
        case Some(SlowpokeConfig(delayInMillis, periodInMillis)) => (true, delayInMillis, periodInMillis)
        case _ => (false, 60000L, 60000L)
      }
    
    Runner.spanScaleFactor = parseDoubleArgument(spanScaleFactors, "-F", 1.0)
    
    val fullReporterConfigurations: ReporterConfigurations = 
      if (remoteArgs.isEmpty) {
        // Creating the normal/main runner, should create reporters as specified by args.
        // If no reporters specified, just give them a default stdout reporter
        Runner.parseReporterArgsIntoConfigurations(reporterArgs)
      }
      else {
        // Creating a sub-process runner, should just create stdout reporter and socket reporter
        Runner.parseReporterArgsIntoConfigurations("-K" :: remoteArgs(0) :: remoteArgs(1) :: Nil)
      }

    val sbtNoFormat = java.lang.Boolean.getBoolean("sbt.log.noformat")

    val (
      useStdout,
      presentAllDurations,
      presentInColor,
      presentShortStackTraces,
      presentFullStackTraces,
      presentUnformatted,
      presentReminder,
      presentReminderWithShortStackTraces,
      presentReminderWithFullStackTraces,
      presentReminderWithoutCanceledTests
    ) = 
      fullReporterConfigurations.standardOutReporterConfiguration match {
        case Some(stdoutConfig) =>
          val configSet = stdoutConfig.configSet
          (
            true, 
            configSet.contains(PresentAllDurations),
            !configSet.contains(PresentWithoutColor) && !sbtNoFormat,
            configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces),
            configSet.contains(PresentFullStackTraces), 
            configSet.contains(PresentUnformatted),
            configSet.exists { ele =>
              ele == PresentReminderWithoutStackTraces || ele == PresentReminderWithShortStackTraces || ele == PresentReminderWithFullStackTraces
            },
            configSet.contains(PresentReminderWithShortStackTraces) && !configSet.contains(PresentReminderWithFullStackTraces),
            configSet.contains(PresentReminderWithFullStackTraces),
            configSet.contains(PresentReminderWithoutCanceledTests)
          )
        case None => 
          (!remoteArgs.isEmpty || reporterArgs.isEmpty, false, !sbtNoFormat, false, false, false, false, false, false, false)
      }
    
    //val reporterConfigs = fullReporterConfigurations.copy(standardOutReporterConfiguration = None)
    // If there's a graphic reporter, we need to leave it out of
    // reporterSpecs, because we want to pass all reporterSpecs except
    // the graphic reporter's to the RunnerJFrame (because RunnerJFrame *is*
    // the graphic reporter).
    val reporterConfigs: ReporterConfigurations =
      fullReporterConfigurations.graphicReporterConfiguration match {
        case None => fullReporterConfigurations.copy(standardOutReporterConfiguration = None)
        case Some(grs) => {
          throw new IllegalArgumentException("Graphic reporter is not supported when runs in SBT.")
        }
      }
    
    new ScalaTestRunner(
      args,
      testClassLoader,
      tagsToInclude,
      tagsToExclude,
      membersOnly, 
      wildcard, 
      configMap,
      reporterConfigs,
      useStdout, 
      presentAllDurations,
      presentInColor,
      presentShortStackTraces,
      presentFullStackTraces,
      presentUnformatted,
      presentReminder,
      presentReminderWithShortStackTraces,
      presentReminderWithFullStackTraces,
      presentReminderWithoutCanceledTests, 
      detectSlowpokes,
      slowpokeDetectionDelay,
      slowpokeDetectionPeriod
    )
  }
  
  private case class ScalaTestSbtEvent(
      fullyQualifiedName: String, 
      fingerprint: Fingerprint, 
      selector: Selector, 
      status: SbtStatus, 
      throwable: OptionalThrowable, 
      duration: Long) extends SbtEvent
  
  private class SbtReporter(suiteId: String, fullyQualifiedName: String, fingerprint: Fingerprint, eventHandler: EventHandler, report: Reporter, summaryCounter: SummaryCounter) extends Reporter {
      
      import org.scalatest.events._
      
      private def getTestSelector(eventSuiteId: String, testName: String) = {
        if (suiteId == eventSuiteId)
          new TestSelector(testName)
        else
          new NestedTestSelector(eventSuiteId, testName)
      }
      
      private def getSuiteSelector(eventSuiteId: String) = {
        if (suiteId == eventSuiteId)
          new SuiteSelector
        else
          new NestedSuiteSelector(eventSuiteId)
      }
      
      private def getOptionalThrowable(throwable: Option[Throwable]): OptionalThrowable = 
        throwable match {
          case Some(t) => new OptionalThrowable(t)
          case None => new OptionalThrowable
        }
      
      override def apply(event: Event) {
        report(event)
        event match {
          // the results of running an actual test
          case t: TestPending => 
            summaryCounter.incrementTestsPendingCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getTestSelector(t.suiteId, t.testName), SbtStatus.Pending, new OptionalThrowable, t.duration.getOrElse(0)))
          case t: TestFailed => 
            summaryCounter.incrementTestsFailedCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getTestSelector(t.suiteId, t.testName), SbtStatus.Failure, getOptionalThrowable(t.throwable), t.duration.getOrElse(0)))
          case t: TestSucceeded => 
            summaryCounter.incrementTestsSucceededCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getTestSelector(t.suiteId, t.testName), SbtStatus.Success, new OptionalThrowable, t.duration.getOrElse(0)))
          case t: TestIgnored => 
            summaryCounter.incrementTestsIgnoredCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getTestSelector(t.suiteId, t.testName), SbtStatus.Ignored, new OptionalThrowable, -1))
          case t: TestCanceled =>
            summaryCounter.incrementTestsCanceledCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getTestSelector(t.suiteId, t.testName), SbtStatus.Canceled, new OptionalThrowable, t.duration.getOrElse(0)))
          case t: SuiteCompleted => 
            summaryCounter.incrementSuitesCompletedCount()
          case t: SuiteAborted => 
            summaryCounter.incrementSuitesAbortedCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, fingerprint, getSuiteSelector(t.suiteId), SbtStatus.Error, getOptionalThrowable(t.throwable), t.duration.getOrElse(0)))
          case t: ScopePending => 
            summaryCounter.incrementScopesPendingCount()
          case _ => 
        }
      }
    }
}
