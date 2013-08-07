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

import sbt.testing.{Event => SbtEvent, Framework => SbtFramework, Status => SbtStatus, _}
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
import java.io.{StringWriter, PrintWriter}
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicInteger, AtomicBoolean}
import scala.collection.JavaConverters._

/**
 * This class is ScalaTest's implementation of the new Framework API that will be supported in sbt 0.13. Since 0.13 is
 * not yet released, this can only be used with an early access version of sbt 0.13. We will document this class for the
 * next milestone release of ScalaTest, but you can use it with sbt 0.13 releases now if you like living on the edge.
 */
class Framework extends SbtFramework {

  /**
   * Test framework name.
   */
  def name = "ScalaTest"
 
  private val resultHolder = new SuiteResultHolder()

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

  class RecordingDistributor(
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
    presentReminderWithoutCanceledTests: Boolean
  ) = {
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
          presentReminderWithoutCanceledTests
        )
      ReporterFactory.getDispatchReporter(Seq(reporter, sbtLogInfoReporter), None, None, loader, Some(resultHolder), false, 0, 0) // TODO: Support Slowpoke detection from sbt
    }
    else 
      reporter
  }
      
  def runSuite(
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
      case e: Exception => {

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
  
  class ScalaTestNestedTask(
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
      
  class ScalaTestTask(
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
            presentReminderWithoutCanceledTests
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
  
  class ScalaTestRunner(
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
    presentReminderWithoutCanceledTests: Boolean
  ) extends sbt.testing.Runner {  
    val isDone = new AtomicBoolean(false)
    val tracker = new Tracker
    val summaryCounter = new SummaryCounter
    val runStartTime = System.currentTimeMillis
    
    val dispatchReporter = ReporterFactory.getDispatchReporter(repConfig, None, None, loader, Some(resultHolder), false, 0, 0) // TODO: Support slowpoke detection from sbt
    
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
            Vector.empty, // TODO: Need to get the failed / canceled events here
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

  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader) = {

    val translator = new FriendlyParamsTranslator()
    val (propertiesArgsList, includesArgsList, excludesArgsList, repoArgsList, concurrentList, memberOnlyList, wildcardList, 
               suiteList, junitList, testngList) = translator.parsePropsAndTags(args.filter(!_.equals("")))
               
    if (!suiteList.isEmpty)
      throw new IllegalArgumentException("-s (suite) is not supported when runs in SBT, please use SBT's test-only instead.")
    
    if (!junitList.isEmpty)
      throw new IllegalArgumentException("-j (junit) is not supported when runs in SBT.")
    
    if (!testngList.isEmpty)
      throw new IllegalArgumentException("-b (testng) is not supported when runs in SBT.")
               
    val configMap = parsePropertiesArgsIntoMap(propertiesArgsList)
    val tagsToInclude: Set[String] = parseCompoundArgIntoSet(includesArgsList, "-n")
    val tagsToExclude: Set[String] = parseCompoundArgIntoSet(excludesArgsList, "-l")
    val membersOnly: List[String] = parseSuiteArgsIntoNameStrings(memberOnlyList, "-m")
    val wildcard: List[String] = parseSuiteArgsIntoNameStrings(wildcardList, "-w")
    
    val fullReporterConfigurations: ReporterConfigurations = 
      if (remoteArgs.isEmpty) {
        // Creating the normal/main runner, should create reporters as specified by args.
        // If no reporters specified, just give them a default stdout reporter
        Runner.parseReporterArgsIntoConfigurations(repoArgsList)
      }
      else {
        // Creating a sub-process runner, should just create stdout reporter and socket reporter
        Runner.parseReporterArgsIntoConfigurations("-K" :: remoteArgs(0) :: remoteArgs(1) :: Nil)
      }
    
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
            !configSet.contains(PresentWithoutColor),
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
          (!remoteArgs.isEmpty || repoArgsList.isEmpty, false, true, false, false, false, false, false, false, false)
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
      presentReminderWithoutCanceledTests
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
