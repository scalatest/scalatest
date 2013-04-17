package org.scalatest.tools

import sbt.testing.{Event => SbtEvent, Framework => SbtFramework, Status => SbtStatus, _}
import org.scalatest._
import SuiteDiscoveryHelper._
import StringReporter.colorizeLinesIndividually
import Suite.formatterForSuiteStarting
import Suite.formatterForSuiteCompleted
import Suite.formatterForSuiteAborted
import org.scalatest.events._
import Runner.parsePropertiesArgsIntoMap
import Runner.parseCompoundArgIntoSet
import Runner.SELECTED_TAG
import Runner.mergeMap
import java.io.{StringWriter, PrintWriter}
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._

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
      
  class RecordingDistributor(fullyQualifiedName: String, rerunSuiteId: String, originalReporter: Reporter, args: Args, loader: ClassLoader, tagsToInclude: Set[String], tagsToExclude: Set[String], selectors: Array[Selector], configMap: ConfigMap, 
                             summaryCounter: SummaryCounter, useSbtLogInfoReporter: Boolean, presentAllDurations: Boolean, presentInColor: Boolean, presentShortStackTraces: Boolean, 
                             presentFullStackTraces: Boolean, presentUnformatted: Boolean) extends Distributor {
    
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
      val nestedTask = new ScalaTestNestedTask(fullyQualifiedName, rerunSuiteId, suite, loader, originalReporter, args.tracker, tagsToInclude, tagsToExclude, selectors, configMap, summaryCounter, status, useSbtLogInfoReporter, 
                                               presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted)
      taskQueue.put(nestedTask)
      status
    }
    
    def nestedTasks: Array[Task] = 
      taskQueue.asScala.toArray
  }
  
  private def createTaskDispatchReporter(reporter: Reporter, loggers: Array[Logger], loader: ClassLoader, useSbtLogInfoReporter: Boolean, presentAllDurations: Boolean, presentInColor: Boolean, 
                                         presentShortStackTraces: Boolean, presentFullStackTraces: Boolean, presentUnformatted: Boolean) = {
    if (useSbtLogInfoReporter) {
      val sbtLogInfoReporter = 
        new SbtLogInfoReporter(
          loggers, 
          presentAllDurations,
          presentInColor,
          presentShortStackTraces,
          presentFullStackTraces // If they say both S and F, F overrules
        )
      ReporterFactory.getDispatchReporter(Seq(reporter, sbtLogInfoReporter), None, None, loader, Some(resultHolder))
    }
    else 
      reporter
  }
      
  def runSuite(fullyQualifiedName: String, rerunSuiteId: String, suite: Suite, loader: ClassLoader, reporter: Reporter, tracker: Tracker, eventHandler: EventHandler, 
               tagsToInclude: Set[String], tagsToExclude: Set[String], selectors: Array[Selector], configMap: ConfigMap, summaryCounter: SummaryCounter, statefulStatus: Option[ScalaTestStatefulStatus], 
               loggers: Array[Logger], useSbtLogInfoReporter: Boolean, presentAllDurations: Boolean, presentInColor: Boolean, presentShortStackTraces: Boolean, presentFullStackTraces: Boolean, 
               presentUnformatted: Boolean): Array[Task] = {
    val suiteStartTime = System.currentTimeMillis
    val suiteClass = suite.getClass
    val report = new SbtReporter(rerunSuiteId, fullyQualifiedName, eventHandler, reporter, summaryCounter)
    val formatter = formatterForSuiteStarting(suite)
        
    val filter = 
      if (selectors.length == 0)
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
              testTags = mergeMap[String, Map[String, Set[String]]](List(testTags, Map(suite.suiteId -> Map(testSelector.getTestName() -> Set(SELECTED_TAG))))) { (testMap1, testMap2) => 
                mergeMap[String, Set[String]](List(testMap1, testMap2)) { _ ++ _}
              }
              hasTest = true
            case nestedSuiteSelector: NestedSuiteSelector => 
              suiteTags = mergeMap[String, Set[String]](List(suiteTags, Map(nestedSuiteSelector.getSuiteId -> Set(SELECTED_TAG)))) { _ ++ _ }
              hasNested = true
            case nestedTestSelector: NestedTestSelector => 
              testTags = mergeMap[String, Map[String, Set[String]]](List(testTags, Map(nestedTestSelector.getSuiteId -> Map(nestedTestSelector.getTestName -> Set(SELECTED_TAG))))) { (testMap1, testMap2) => 
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
    val distributor = new RecordingDistributor(fullyQualifiedName, rerunSuiteId, reporter, args, loader, tagsToInclude, tagsToExclude, selectors, configMap, summaryCounter, useSbtLogInfoReporter, presentAllDurations, presentInColor, presentShortStackTraces, 
                                               presentFullStackTraces, presentUnformatted)
    
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
  
  class ScalaTestNestedTask(fullyQualifiedName: String, rerunSuiteId: String, suite: Suite, loader: ClassLoader, reporter: Reporter, tracker: Tracker, tagsToInclude: Set[String], tagsToExclude: Set[String], 
                            selectors: Array[Selector], configMap: ConfigMap, summaryCounter: SummaryCounter, statefulStatus: ScalaTestStatefulStatus, useSbtLogInfoReporter: Boolean, 
                            presentAllDurations: Boolean, presentInColor: Boolean, presentShortStackTraces: Boolean, presentFullStackTraces: Boolean, presentUnformatted: Boolean) extends Task {
    
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
      runSuite(fullyQualifiedName, rerunSuiteId, suite, loader, reporter, tracker, eventHandler, tagsToInclude, tagsToExclude, selectors, configMap, summaryCounter, Some(statefulStatus), 
               loggers, useSbtLogInfoReporter, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted)
    }
  }
      
  class ScalaTestTask(fullyQualifiedName: String, loader: ClassLoader, reporter: Reporter, tracker: Tracker, tagsToInclude: Set[String], 
                      tagsToExclude: Set[String], explicitlySpecified: Boolean, selectors: Array[Selector], configMap: ConfigMap, 
                      summaryCounter: SummaryCounter, useSbtLogInfoReporter: Boolean, presentAllDurations: Boolean, presentInColor: Boolean, 
                      presentShortStackTraces: Boolean, presentFullStackTraces: Boolean, presentUnformatted: Boolean) extends Task {
    
    def loadSuiteClass = {
      try {
        Class.forName(fullyQualifiedName, true, loader)
      }
      catch {
        case e: Exception => 
          throw new IllegalArgumentException("Unable to load class: " + fullyQualifiedName)
      }
    }
    
    lazy val suiteClass = loadSuiteClass
    
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
      if (!explicitlySpecified && !isDiscoverableSuite(suiteClass))  // Do nothing if it is annotated with @DoNotDiscover and it is not explicitly specified.
        Array.empty[Task]
      else if (isAccessibleSuite(suiteClass) || isRunnable(suiteClass)) {
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
        
        val taskReporter = createTaskDispatchReporter(reporter, loggers, loader, useSbtLogInfoReporter, presentAllDurations, presentInColor, presentShortStackTraces, 
                                                      presentFullStackTraces, presentUnformatted)
        runSuite(fullyQualifiedName, suite.suiteId, suite, loader, taskReporter, tracker, eventHandler, tagsToInclude, tagsToExclude, selectors, configMap, summaryCounter, None, 
                 loggers, useSbtLogInfoReporter, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted)
      }
       else 
         throw new IllegalArgumentException("Class " + fullyQualifiedName + " is neither accessible accesible org.scalatest.Suite nor runnable.")
    }
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
  
  class SbtLogInfoReporter(loggers: Array[Logger], presentAllDurations: Boolean, presentInColor: Boolean, presentShortStackTraces: Boolean, presentFullStackTraces: Boolean) 
    extends StringReporter(presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, false) {
    
    protected def printPossiblyInColor(text: String, ansiColor: String) {
      loggers.foreach { logger =>
        logger.info(if (logger.ansiCodesSupported && presentInColor) colorizeLinesIndividually(text, ansiColor) else text)
      }
    }

    def dispose() = ()
  }
  
  class ScalaTestRunner(runArgs: Array[String], loader: ClassLoader, tagsToInclude: Set[String], tagsToExclude: Set[String], configMap: ConfigMap, 
                        repConfig: ReporterConfigurations, useSbtLogInfoReporter: Boolean, presentAllDurations: Boolean, presentInColor: Boolean, 
                        presentShortStackTraces: Boolean, presentFullStackTraces: Boolean, presentUnformatted: Boolean) 
                        extends sbt.testing.Runner {  
    var isDone = false
    val tracker = new Tracker
    val summaryCounter = new SummaryCounter
    val runStartTime = System.currentTimeMillis
    
    val dispatchReporter = ReporterFactory.getDispatchReporter(repConfig, None, None, loader, Some(resultHolder))
    
    dispatchReporter(RunStarting(tracker.nextOrdinal(), 0, configMap))
    
    def task(fullyQualifiedName: String, isModule: Boolean, explicitlySpecified: Boolean, selectors: Array[Selector]) = 
      new ScalaTestTask(fullyQualifiedName, loader, dispatchReporter, tracker, if (selectors.isEmpty) Set.empty else Set(SELECTED_TAG), Set.empty, explicitlySpecified, selectors, configMap, summaryCounter, 
                        useSbtLogInfoReporter, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted)
    
    def done = {
      if (!isDone) {
        val duration = System.currentTimeMillis - runStartTime
        val summary = new Summary(summaryCounter.testsSucceededCount.get, summaryCounter.testsFailedCount.get, summaryCounter.testsIgnoredCount.get, summaryCounter.testsPendingCount.get, 
                                  summaryCounter.testsCanceledCount.get, summaryCounter.suitesCompletedCount.get, summaryCounter.suitesAbortedCount.get, summaryCounter.scopesPendingCount.get)
        dispatchReporter(RunCompleted(tracker.nextOrdinal(), Some(duration), Some(summary)))
        dispatchReporter.dispatchDisposeAndWaitUntilDone()
        isDone = true
        val stringWriter = new StringWriter
        val printReporter = new PrintReporter(new PrintWriter(stringWriter), presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted) {}
        printReporter.makeFinalReport("runCompleted", Some(duration), Some(summary))
        stringWriter.flush() // just to make sure everything is flushed
        stringWriter.toString
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
    val configMap = parsePropertiesArgsIntoMap(propertiesArgsList)
    val tagsToInclude: Set[String] = parseCompoundArgIntoSet(includesArgsList, "-n")
    val tagsToExclude: Set[String] = parseCompoundArgIntoSet(excludesArgsList, "-l")
    
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
    
    val (useStdout, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted) = 
      fullReporterConfigurations.standardOutReporterConfiguration match {
        case Some(stdoutConfig) =>
          val configSet = stdoutConfig.configSet
          (
            true, 
            configSet.contains(PresentAllDurations),
            !configSet.contains(PresentWithoutColor),
            configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces),
            configSet.contains(PresentFullStackTraces), 
            configSet.contains(PresentUnformatted)
          )
        case None => 
          (!remoteArgs.isEmpty || repoArgsList.isEmpty, false, true, false, false, false)
      }
    
    val reporterConfigs = fullReporterConfigurations.copy(standardOutReporterConfiguration = None)
    
    new ScalaTestRunner(args, testClassLoader, tagsToInclude, tagsToExclude, configMap, reporterConfigs, useStdout, 
                        presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted)
  }
  
  private case class ScalaTestSbtEvent(
      fullyQualifiedName: String, 
      isModule: Boolean, 
      selector: Selector, 
      status: SbtStatus, 
      throwable: Throwable) extends SbtEvent
  
  private class SbtReporter(suiteId: String, fullyQualifiedName: String, eventHandler: EventHandler, report: Reporter, summaryCounter: SummaryCounter) extends Reporter {
      
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
      
      override def apply(event: Event) {
        report(event)
        event match {
          // the results of running an actual test
          case t: TestPending => 
            summaryCounter.incrementTestsPendingCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, false, getTestSelector(t.suiteId, t.testName), SbtStatus.Skipped, null))
          case t: TestFailed => 
            summaryCounter.incrementTestsFailedCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, false, getTestSelector(t.suiteId, t.testName), SbtStatus.Failure, t.throwable.getOrElse(null)))
          case t: TestSucceeded => 
            summaryCounter.incrementTestsSucceededCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, false, getTestSelector(t.suiteId, t.testName), SbtStatus.Success, null))
          case t: TestIgnored => 
            summaryCounter.incrementTestsIgnoredCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, false, getTestSelector(t.suiteId, t.testName), SbtStatus.Skipped, null))
          case t: TestCanceled =>
            summaryCounter.incrementTestsCanceledCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, false, getTestSelector(t.suiteId, t.testName), SbtStatus.Skipped, null))
          case t: SuiteCompleted => 
            summaryCounter.incrementSuitesCompletedCount()
          case t: SuiteAborted => 
            summaryCounter.incrementSuitesAbortedCount()
            eventHandler.handle(ScalaTestSbtEvent(fullyQualifiedName, false, getSuiteSelector(t.suiteId), SbtStatus.Error, t.throwable.getOrElse(null)))
          case t: ScopePending => 
            summaryCounter.incrementScopesPendingCount()
          case _ => 
        }
      }
    }
}