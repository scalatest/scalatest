package org.scalatest.tools
import org.scalatest.FunSuite
import sbt.testing._

class FrameworkSuite extends FunSuite {
  
  class TestEventHandler extends EventHandler {
    
    private var errorEvents = List[Event]()
    private var failureEvents = List[Event]()
    private var skippedEvents = List[Event]()
    private var successEvents = List[Event]()
    
    override def handle(event: Event): Unit = {
      event.status match {
        case Status.Success => successEvents ::= event
        case Status.Error => errorEvents ::= event
        case Status.Failure => failureEvents ::= event
        case Status.Skipped => skippedEvents ::= event
      }
    }
    
    def errorEventsReceived = errorEvents.reverse
    def failureEventsReceived = failureEvents.reverse
    def skippedEventsReceived = skippedEvents.reverse
    def successEventsReceived = successEvents.reverse
  }
  
  class TestLogger extends Logger {
    
    private var errorList = List[String]()
    private var warnList = List[String]()
    private var infoList = List[String]()
    private var debugList = List[String]()
    private var traceList = List[Throwable]()
    
    def ansiCodesSupported = false
    def error(msg: String) {
      errorList ::= msg
    }
    def warn(msg: String) {
      warnList ::= msg
    }
    def info(msg: String) {
      infoList ::= msg
    }
    def debug(msg: String) {
      debugList ::= msg
    }
    def trace(t: Throwable) {
      traceList ::= t
    }
    
    def errorReceived = errorList.reverse
    def warnReceived = warnList.reverse
    def infoReceived = infoList.reverse
    def debugReceived = debugList.reverse
    def traceReceived = traceList.reverse
  }

  test("framework name") {
    assert(new ScalaTestFramework().name === "ScalaTest")
  }
  
  test("fingerprints contains 2 test fingerprints, they are SubclassFingerprint for org.scalatest.Suite and AnnotatedFingerprint for org.scalatest.WrapWith") {
    val framework = new Framework
    val fingerprints = framework.fingerprints
    assert(fingerprints.size === 2)

    val testFingerprint =
      fingerprints(0).asInstanceOf[sbt.testing.SubclassFingerprint]
    assert(testFingerprint.isModule === false)
    assert(testFingerprint.superclassName === "org.scalatest.Suite")
    assert(testFingerprint.requireNoArgConstructor === true)
    
    val annotatedFingerprint = 
      fingerprints(1).asInstanceOf[sbt.testing.AnnotatedFingerprint]
    assert(annotatedFingerprint.isModule === false)
    assert(annotatedFingerprint.annotationName === "org.scalatest.WrapWith")
  }
  
  val testClassLoader = getClass.getClassLoader
  val subClassFingerprint = new sbt.testing.SubclassFingerprint {
                              def superclassName = "org.scalatest.Suite"
                              def isModule = false
                              def requireNoArgConstructor = true
                            }
  
  val framework = new Framework
  val subclassFingerprint = 
    new SubclassFingerprint {
      def superclassName = "org.scalatest.Suite"
      def isModule = false
      def requireNoArgConstructor = true
    }
  val annotatedFingerprint = 
    new AnnotatedFingerprint {
      def annotationName = "org.scalatest.WrapWith"
      def isModule = false
    }
  
  def assertSuiteSuccessEvent(event: Event, suiteClassName: String, testName: String) {
    assert(Status.Success === event.status)
    assert(suiteClassName === event.fullyQualifiedName)
    val selector = event.selector
    selector match {
      case testSelector: TestSelector => 
        assert(testName === testSelector.getTestName)
      case _ => 
        fail("Expected to get TestSelector, but got: " + selector.getClass.getName)
    }
  }
  
  def assertNestedSuiteSuccessEvent(event: Event, suiteClassName: String, suiteId:String, testName: String) {
    assert(Status.Success === event.status)
    assert(suiteClassName === event.fullyQualifiedName)
    val selector = event.selector
    selector match {
      case nestedTestSelector: NestedTestSelector => 
        assert(suiteId === nestedTestSelector.getSuiteId)
        assert(testName === nestedTestSelector.getTestName)
      case _ => 
        fail("Expected to get NestedTestSelector, but got: " + selector.getClass.getName)
    }
  }
  
  def assertSuiteFailureEvent(event: Event, suiteClassName: String, testName: String) {
    assert(Status.Failure === event.status)
    assert(suiteClassName === event.fullyQualifiedName)
    val selector = event.selector
    selector match {
      case testSelector: TestSelector => 
        assert(testName === testSelector.getTestName)
      case _ => 
        fail("Expected to get TestSelector, but got: " + selector.getClass.getName)
    }
  }
  
  def assertNestedSuiteFailureEvent(event: Event, suiteClassName: String, suiteId:String, testName: String) {
    assert(Status.Failure === event.status)
    assert(suiteClassName === event.fullyQualifiedName)
    val selector = event.selector
    selector match {
      case nestedTestSelector: NestedTestSelector => 
        assert(suiteId === nestedTestSelector.getSuiteId)
        assert(testName === nestedTestSelector.getTestName)
      case _ => 
        fail("Expected to get NestedTestSelector, but got: " + selector.getClass.getName)
    }
  }
  
  def assertSuiteErrorEvent(event: Event, suiteClassName: String) {
    assert(Status.Error === event.status)
    assert(suiteClassName === event.fullyQualifiedName)
    val selector = event.selector
    selector match {
      case suiteSelector: SuiteSelector => 
        // Nothing more to check, just make sure it is SuiteSelector.
      case _ => 
        fail("Expected to get TestSelector, but got: " + selector.getClass.getName)
    }
  }
  
  def assertNestedSuiteErrorEvent(event: Event, suiteClassName: String, suiteId:String) {
    assert(Status.Error === event.status)
    assert(suiteClassName === event.fullyQualifiedName)
    val selector = event.selector
    selector match {
      case nestedSuiteSelector: NestedSuiteSelector => 
        assert(suiteId === nestedSuiteSelector.getSuiteId)
      case _ => 
        fail("Expected to get NestedTestSelector, but got: " + selector.getClass.getName)
    }
  }
  
  def assertSuiteSkippedEvent(event: Event, suiteClassName: String, testName: String) {
    assert(Status.Skipped === event.status)
    assert(suiteClassName === event.fullyQualifiedName)
    val selector = event.selector
    selector match {
      case testSelector: TestSelector => 
        assert(testName === testSelector.getTestName)
      case _ => 
        fail("Expected to get TestSelector, but got: " + selector.getClass.getName)
    }
  }
  
  def assertNestedSuiteSkippedEvent(event: Event, suiteClassName: String, suiteId:String, testName: String) {
    assert(Status.Skipped === event.status)
    assert(suiteClassName === event.fullyQualifiedName)
    val selector = event.selector
    selector match {
      case nestedTestSelector: NestedTestSelector => 
        assert(suiteId === nestedTestSelector.getSuiteId)
        assert(testName === nestedTestSelector.getTestName)
      case _ => 
        fail("Expected to get NestedTestSelector, but got: " + selector.getClass.getName)
    }
  }
  
  test("ScalaTestRunner.task should return task that run whole suite when fullyQualifiedName = valid class name, explicitlySpecified = false and selectors = empty array") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    
    val task = runner.task("org.scalatest.tools.scalasbt.SampleSuite", subclassFingerprint, false, Array.empty)
    assert(task != null)
    task.execute(testEventHandler, Array(new TestLogger))
    val successEvents = testEventHandler.successEventsReceived
    assert(successEvents.length === 3)
    assertSuiteSuccessEvent(successEvents(0), "org.scalatest.tools.scalasbt.SampleSuite", "test 1")
    assertSuiteSuccessEvent(successEvents(1), "org.scalatest.tools.scalasbt.SampleSuite", "test 2")
    assertSuiteSuccessEvent(successEvents(2), "org.scalatest.tools.scalasbt.SampleSuite", "test 3")
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
  }
  
  test("ScalaTestRunner.task should return task that run whole suite when fullyQualifiedName = valid class name, explicitlySpecified = false and selectors = Array(SuiteSelector)") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    
    val task = runner.task("org.scalatest.tools.scalasbt.SampleSuite", subclassFingerprint, false, Array(new SuiteSelector))
    assert(task != null)
    task.execute(testEventHandler, Array(new TestLogger))
    val successEvents = testEventHandler.successEventsReceived
    assert(successEvents.length === 3)
    assertSuiteSuccessEvent(successEvents(0), "org.scalatest.tools.scalasbt.SampleSuite", "test 1")
    assertSuiteSuccessEvent(successEvents(1), "org.scalatest.tools.scalasbt.SampleSuite", "test 2")
    assertSuiteSuccessEvent(successEvents(2), "org.scalatest.tools.scalasbt.SampleSuite", "test 3")
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
  }
  
  test("ScalaTestRunner.task should return task that do nothing when fullyQualifiedName = valid class name, explicitlySpecified = false, selectors = empty array" +
  	   "and the suite class is marked as @DoNotDiscover") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    
    val task = runner.task("org.scalatest.tools.scalasbt.DoNotDiscoverSuite", subclassFingerprint, false, Array.empty)
    assert(task != null)
    task.execute(testEventHandler, Array(new TestLogger))
    assert(testEventHandler.successEventsReceived.length === 0)
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
  }
  
  test("ScalaTestRunner.task should return task that do nothing when fullyQualifiedName = valid class name, explicitlySpecified = false, selectors = Array(SuiteSelector)" +
  	   "and the suite class is marked as @DoNotDiscover") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    
    val task = runner.task("org.scalatest.tools.scalasbt.DoNotDiscoverSuite", subclassFingerprint, false, Array(new SuiteSelector))
    assert(task != null)
    task.execute(testEventHandler, Array(new TestLogger))
    assert(testEventHandler.successEventsReceived.length === 0)
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
  }
  
  test("When suite is neither subclass of org.scalatest.Suite or annotated with WrapWith, IllegalArgumentException will be thrown") {
    intercept[IllegalArgumentException] {
      val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
      val notASuiteTask = runner.task("org.scalatest.tools.scalasbt.NotASuite", subclassFingerprint, false, Array(new SuiteSelector))
      notASuiteTask.execute(new TestEventHandler, Array(new TestLogger))
    }
  }
  
  test("When an invalid suite class name is passed into to task(fullyQualifiedName: String, fingerprint: Fingerprint), IllegalArgumentException " +
  	   "will be thrown") {
    intercept[IllegalArgumentException] {
      val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
      val doesNotExistTask = runner.task("org.scalatest.tools.scalasbt.DoesNotExist", subclassFingerprint, false, Array(new SuiteSelector))
      doesNotExistTask.execute(new TestEventHandler, Array(new TestLogger))
    }
  }
  
  test("Nested suites will be included in tasks returned from task(fullyQualifiedName: String, fingerprint: Fingerprint)") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    
    val task = runner.task("org.scalatest.tools.scalasbt.SuiteWithNestedSuites", subclassFingerprint, false, Array(new SuiteSelector))
    assert(task != null)
    val nestedTasks = task.execute(testEventHandler, Array(new TestLogger))
    val successEvents = testEventHandler.successEventsReceived
    assert(successEvents.length === 3)
    assertSuiteSuccessEvent(successEvents(0), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "test 1")
    assertSuiteSuccessEvent(successEvents(1), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "test 2")
    assertSuiteSuccessEvent(successEvents(2), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "test 3")
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
    assert(nestedTasks.size === 2)
    
    val nestedTask1 = nestedTasks(0)
    val nestedTask1TestEventHandler = new TestEventHandler
    val nestedTask1NestedTasks = nestedTask1.execute(nestedTask1TestEventHandler, Array(new TestLogger))
    assert(nestedTask1NestedTasks.size === 0)
    val nestedTask1SuccessEvents = nestedTask1TestEventHandler.successEventsReceived
    assert(nestedTask1SuccessEvents.length === 3)
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(0), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 1", "nested 1 test 1")
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(1), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 1", "nested 1 test 2")
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(2), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 1", "nested 1 test 3")
    assert(nestedTask1TestEventHandler.errorEventsReceived.length === 0)
    assert(nestedTask1TestEventHandler.failureEventsReceived.length === 0)
    assert(nestedTask1TestEventHandler.skippedEventsReceived.length === 0)
    
    val nestedTask2 = nestedTasks(1)
    val nestedTask2TestEventHandler = new TestEventHandler
    val nestedTask2NestedTasks = nestedTask2.execute(nestedTask2TestEventHandler, Array(new TestLogger))
    assert(nestedTask2NestedTasks.size === 0)
    val nestedTask2SuccessEvents = nestedTask2TestEventHandler.successEventsReceived
    assert(nestedTask2SuccessEvents.length === 3)
    assertNestedSuiteSuccessEvent(nestedTask2SuccessEvents(0), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 2", "nested 2 test 1")
    assertNestedSuiteSuccessEvent(nestedTask2SuccessEvents(1), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 2", "nested 2 test 2")
    assertNestedSuiteSuccessEvent(nestedTask2SuccessEvents(2), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 2", "nested 2 test 3")
    assert(nestedTask2TestEventHandler.errorEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.failureEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.skippedEventsReceived.length === 0)
  }
  
  test("Ignore, pending, failed, canceled, suite aborted events should be translated and reported correctly for the suite and its nested suites") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    
    val task = runner.task("org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", subclassFingerprint, false, Array(new SuiteSelector))
    assert(task != null)
    val nestedTasks = task.execute(testEventHandler, Array(new TestLogger))
    assert(nestedTasks.size == 3)
    
    val successEvents = testEventHandler.successEventsReceived
    assert(successEvents.length === 1)
    assertSuiteSuccessEvent(successEvents(0), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "success")
    
    val failureEvents = testEventHandler.failureEventsReceived
    assert(failureEvents.length === 1)
    assertSuiteFailureEvent(failureEvents(0), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "failed")
    
    val errorEvents = testEventHandler.errorEventsReceived
    assert(errorEvents.length === 0)
    
    val skippedEvents = testEventHandler.skippedEventsReceived
    assert(skippedEvents.length === 3)
    assertSuiteSkippedEvent(skippedEvents(0), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "ignored")
    assertSuiteSkippedEvent(skippedEvents(1), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "pending")
    assertSuiteSkippedEvent(skippedEvents(2), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "canceled")
    
    val nestedTask1TestEventHandler = new TestEventHandler
    val nestedTask1 = nestedTasks(0)
    val nestedTask1NestedTask = nestedTask1.execute(nestedTask1TestEventHandler, Array(new TestLogger))
    
    val nestedTask1SuccessEvents = nestedTask1TestEventHandler.successEventsReceived
    assert(nestedTask1SuccessEvents.length === 1)
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(0), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "nested 1", "nested 1 success")
    
    val nestedTask1FailureEvents = nestedTask1TestEventHandler.failureEventsReceived
    assert(nestedTask1FailureEvents.length === 1)
    assertNestedSuiteFailureEvent(nestedTask1FailureEvents(0), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "nested 1", "nested 1 failed")
    
    val nestedTask1ErrorEvents = nestedTask1TestEventHandler.errorEventsReceived
    assert(nestedTask1ErrorEvents.length === 0)
    
    val nestedTask1SkippedEvents = nestedTask1TestEventHandler.skippedEventsReceived
    assert(nestedTask1SkippedEvents.length === 3)
    assertNestedSuiteSkippedEvent(nestedTask1SkippedEvents(0), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "nested 1", "nested 1 ignored")
    assertNestedSuiteSkippedEvent(nestedTask1SkippedEvents(1), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "nested 1", "nested 1 pending")
    assertNestedSuiteSkippedEvent(nestedTask1SkippedEvents(2), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "nested 1", "nested 1 canceled")
    
    val nestedTask2TestEventHandler = new TestEventHandler
    val nestedTask2 = nestedTasks(1)
    val nestedTask2NestedTask = nestedTask2.execute(nestedTask2TestEventHandler, Array(new TestLogger))
    
    val nestedTask2SuccessEvents = nestedTask2TestEventHandler.successEventsReceived
    assert(nestedTask2SuccessEvents.length === 1)
    assertNestedSuiteSuccessEvent(nestedTask2SuccessEvents(0), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "nested 2", "nested 2 success")
    
    val nestedTask2FailureEvents = nestedTask2TestEventHandler.failureEventsReceived
    assert(nestedTask2FailureEvents.length === 1)
    assertNestedSuiteFailureEvent(nestedTask2FailureEvents(0), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "nested 2", "nested 2 failed")
    
    val nestedTask2ErrorEvents = nestedTask2TestEventHandler.errorEventsReceived
    assert(nestedTask2ErrorEvents.length === 0)
    
    val nestedTask2SkippedEvents = nestedTask2TestEventHandler.skippedEventsReceived
    assert(nestedTask2SkippedEvents.length === 3)
    assertNestedSuiteSkippedEvent(nestedTask2SkippedEvents(0), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "nested 2", "nested 2 ignored")
    assertNestedSuiteSkippedEvent(nestedTask2SkippedEvents(1), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "nested 2", "nested 2 pending")
    assertNestedSuiteSkippedEvent(nestedTask2SkippedEvents(2), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "nested 2", "nested 2 canceled")
    
    val nestedTask3TestEventHandler = new TestEventHandler
    val nestedTask3 = nestedTasks(2)
    val nestedTask3NestedTask = nestedTask3.execute(nestedTask3TestEventHandler, Array(new TestLogger))
    
    val nestedTask3SuccessEvents = nestedTask3TestEventHandler.successEventsReceived
    assert(nestedTask3SuccessEvents.length === 0)
    
    val nestedTask3FailureEvents = nestedTask3TestEventHandler.failureEventsReceived
    assert(nestedTask3FailureEvents.length === 0)
    
    val nestedTask3ErrorEvents = nestedTask3TestEventHandler.errorEventsReceived
    assert(nestedTask3ErrorEvents.length === 1)
    assertNestedSuiteErrorEvent(nestedTask3ErrorEvents(0), "org.scalatest.tools.scalasbt.SuiteWithFailedSkippedTests", "nested 3")
    
    val nestedTask3SkippedEvents = nestedTask3TestEventHandler.skippedEventsReceived
    assert(nestedTask3SkippedEvents.length === 0)
  }
  
  test("SuiteSelector should select and run test(s) in selected suite") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    val task = runner.task("org.scalatest.tools.scalasbt.SampleSuite", subclassFingerprint, false, Array(new SuiteSelector()))
    task.execute(testEventHandler, Array(new TestLogger))
    val successEvents = testEventHandler.successEventsReceived
    assert(successEvents.length === 3)
    assertSuiteSuccessEvent(successEvents(0), "org.scalatest.tools.scalasbt.SampleSuite", "test 1")
    assertSuiteSuccessEvent(successEvents(1), "org.scalatest.tools.scalasbt.SampleSuite", "test 2")
    assertSuiteSuccessEvent(successEvents(2), "org.scalatest.tools.scalasbt.SampleSuite", "test 3")
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
  }
  
  test("SuiteSelector should select and run test(s) in selected suite when it is explicitly specified, even when the selected suite is annotated with @DoNotDiscover") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    val task = runner.task("org.scalatest.tools.scalasbt.DoNotDiscoverSuite", subclassFingerprint, true, Array(new SuiteSelector()))
    task.execute(testEventHandler, Array(new TestLogger))
    val successEvents = testEventHandler.successEventsReceived
    assert(successEvents.length === 3)
    assertSuiteSuccessEvent(successEvents(0), "org.scalatest.tools.scalasbt.DoNotDiscoverSuite", "test 1")
    assertSuiteSuccessEvent(successEvents(1), "org.scalatest.tools.scalasbt.DoNotDiscoverSuite", "test 2")
    assertSuiteSuccessEvent(successEvents(2), "org.scalatest.tools.scalasbt.DoNotDiscoverSuite", "test 3")
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
  }
  
  test("TestSelector should select and run selected test(s) in suite, excluding nested suites") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    val task = runner.task("org.scalatest.tools.scalasbt.SampleSuite", subclassFingerprint, false, Array(new TestSelector("test 1"), new TestSelector("test 3")))
    task.execute(testEventHandler, Array(new TestLogger))
    val successEvents = testEventHandler.successEventsReceived
    assert(successEvents.length === 2)
    assertSuiteSuccessEvent(successEvents(0), "org.scalatest.tools.scalasbt.SampleSuite", "test 1")
    assertSuiteSuccessEvent(successEvents(1), "org.scalatest.tools.scalasbt.SampleSuite", "test 3")
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
    
    val testEventHandler2 = new TestEventHandler
    val runner2 = framework.runner(Array.empty, Array.empty, testClassLoader)
    val task2 = runner2.task("org.scalatest.tools.scalasbt.SuiteWithNestedSuites", subclassFingerprint, false, Array(new TestSelector("test 2")))
    task2.execute(testEventHandler2, Array(new TestLogger))
    val successEvents2 = testEventHandler2.successEventsReceived
    assert(successEvents2.length === 1)
    assertSuiteSuccessEvent(successEvents2(0), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "test 2")
    assert(testEventHandler2.errorEventsReceived.length === 0)
    assert(testEventHandler2.failureEventsReceived.length === 0)
    assert(testEventHandler2.skippedEventsReceived.length === 0)
  }
  
  test("TestSelector should select and run selected test(s) in suite when it is explicitly specified, even when the suite is annotated with @DoNotDiscover") {    
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    val task = runner.task("org.scalatest.tools.scalasbt.DoNotDiscoverSuite", subclassFingerprint, true, Array(new TestSelector("test 1"), new TestSelector("test 3")))
    task.execute(testEventHandler, Array(new TestLogger))
    val successEvents = testEventHandler.successEventsReceived
    assert(successEvents.length === 2)
    assertSuiteSuccessEvent(successEvents(0), "org.scalatest.tools.scalasbt.DoNotDiscoverSuite", "test 1")
    assertSuiteSuccessEvent(successEvents(1), "org.scalatest.tools.scalasbt.DoNotDiscoverSuite", "test 3")
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
  }
  
  test("TestSelector should not select and run selected test(s) in suite when it is not explicitly specified and the suite is annotated with @DoNotDiscover") {    
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    val task = runner.task("org.scalatest.tools.scalasbt.DoNotDiscoverSuite", subclassFingerprint, false, Array(new TestSelector("test 1"), new TestSelector("test 3")))
    task.execute(testEventHandler, Array(new TestLogger))
    assert(testEventHandler.successEventsReceived.length === 0)
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
  }
  
  test("NestedSuiteSelector should select and run test(s) in selected nested suite when it is explicitly specified, even if the selected nested suite is annotated with @DoNotDiscover") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    
    val task = runner.task("org.scalatest.tools.scalasbt.SuiteWithNestedSuites", subclassFingerprint, true, Array(new NestedSuiteSelector("nested 1")))
    val nestedTasks = task.execute(testEventHandler, Array(new TestLogger))
    assert(nestedTasks.size === 2)
    
    val successEvents = testEventHandler.successEventsReceived
    assert(successEvents.length === 0)
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
    
    val nestedTask1 = nestedTasks(0)
    val nestedTask1TestEventHandler = new TestEventHandler
    val nestedTask1NestedTasks = nestedTask1.execute(nestedTask1TestEventHandler, Array(new TestLogger))
    assert(nestedTask1NestedTasks.size === 0)
    val nestedTask1SuccessEvents = nestedTask1TestEventHandler.successEventsReceived
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(0), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 1", "nested 1 test 1")
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(1), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 1", "nested 1 test 2")
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(2), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 1", "nested 1 test 3")
    assert(nestedTask1TestEventHandler.errorEventsReceived.length === 0)
    assert(nestedTask1TestEventHandler.failureEventsReceived.length === 0)
    assert(nestedTask1TestEventHandler.skippedEventsReceived.length === 0)
    
    val nestedTask2 = nestedTasks(1)
    val nestedTask2TestEventHandler = new TestEventHandler
    val nestedTask2NestedTasks = nestedTask2.execute(nestedTask2TestEventHandler, Array(new TestLogger))
    assert(nestedTask2NestedTasks.size === 0)
    assert(nestedTask2TestEventHandler.successEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.errorEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.failureEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.skippedEventsReceived.length === 0)
  }
  
  test("NestedSuiteSelector should select and run test(s) in selected nested suite when it is not explicitly specified, even if the selected nested suite is annotated with @DoNotDiscover") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    
    val task = runner.task("org.scalatest.tools.scalasbt.SuiteWithNestedSuites", subclassFingerprint, false, Array(new NestedSuiteSelector("nested 1")))
    val nestedTasks = task.execute(testEventHandler, Array(new TestLogger))
    assert(nestedTasks.size === 2)
    
    val successEvents = testEventHandler.successEventsReceived
    assert(successEvents.length === 0)
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
    
    val nestedTask1 = nestedTasks(0)
    val nestedTask1TestEventHandler = new TestEventHandler
    val nestedTask1NestedTasks = nestedTask1.execute(nestedTask1TestEventHandler, Array(new TestLogger))
    assert(nestedTask1NestedTasks.size === 0)
    val nestedTask1SuccessEvents = nestedTask1TestEventHandler.successEventsReceived
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(0), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 1", "nested 1 test 1")
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(1), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 1", "nested 1 test 2")
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(2), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 1", "nested 1 test 3")
    assert(nestedTask1TestEventHandler.errorEventsReceived.length === 0)
    assert(nestedTask1TestEventHandler.failureEventsReceived.length === 0)
    assert(nestedTask1TestEventHandler.skippedEventsReceived.length === 0)
    
    val nestedTask2 = nestedTasks(1)
    val nestedTask2TestEventHandler = new TestEventHandler
    val nestedTask2NestedTasks = nestedTask2.execute(nestedTask2TestEventHandler, Array(new TestLogger))
    assert(nestedTask2NestedTasks.size === 0)
    assert(nestedTask2TestEventHandler.successEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.errorEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.failureEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.skippedEventsReceived.length === 0)
  }
  
  test("NestedTestSelector should select and run selected test(s) in selected nested suite when it is explicitly specified, even if the selected nested suite is annotated with @DoNotDiscover") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    
    val task = runner.task("org.scalatest.tools.scalasbt.SuiteWithNestedSuites", subclassFingerprint, false, Array(new NestedTestSelector("nested 1", "nested 1 test 1"), new NestedTestSelector("nested 2", "nested 2 test 3")))
    val nestedTasks = task.execute(testEventHandler, Array(new TestLogger))
    assert(nestedTasks.size === 2)
    assert(testEventHandler.successEventsReceived.length === 0)
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
    
    val nestedTask1 = nestedTasks(0)
    val nestedTask1TestEventHandler = new TestEventHandler
    val nestedTask1NestedTasks = nestedTask1.execute(nestedTask1TestEventHandler, Array(new TestLogger))
    assert(nestedTask1NestedTasks.size === 0)
    val nestedTask1SuccessEvents = nestedTask1TestEventHandler.successEventsReceived
    assert(nestedTask1SuccessEvents.size === 1)
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(0), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 1", "nested 1 test 1")
    assert(nestedTask1TestEventHandler.errorEventsReceived.length === 0)
    assert(nestedTask1TestEventHandler.failureEventsReceived.length === 0)
    assert(nestedTask1TestEventHandler.skippedEventsReceived.length === 0)
    
    val nestedTask2 = nestedTasks(1)
    val nestedTask2TestEventHandler = new TestEventHandler
    val nestedTask2NestedTasks = nestedTask2.execute(nestedTask2TestEventHandler, Array(new TestLogger))
    assert(nestedTask2NestedTasks.size === 0)
    val nestedTask2SuccessEvents = nestedTask2TestEventHandler.successEventsReceived
    assert(nestedTask2SuccessEvents.size === 1)
    assertNestedSuiteSuccessEvent(nestedTask2SuccessEvents(0), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 2", "nested 2 test 3")
    assert(nestedTask2TestEventHandler.errorEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.failureEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.skippedEventsReceived.length === 0)
  }
  
  test("NestedTestSelector should select and run selected test(s) in selected nested suite when it is not explicitly specified, even if the selected nested suite is annotated with @DoNotDiscover") {
    val testEventHandler = new TestEventHandler
    val runner = framework.runner(Array.empty, Array.empty, testClassLoader)
    
    val task = runner.task("org.scalatest.tools.scalasbt.SuiteWithNestedSuites", subclassFingerprint, false, Array(new NestedTestSelector("nested 1", "nested 1 test 1"), new NestedTestSelector("nested 2", "nested 2 test 3")))
    val nestedTasks = task.execute(testEventHandler, Array(new TestLogger))
    assert(nestedTasks.size === 2)
    assert(testEventHandler.successEventsReceived.length === 0)
    assert(testEventHandler.errorEventsReceived.length === 0)
    assert(testEventHandler.failureEventsReceived.length === 0)
    assert(testEventHandler.skippedEventsReceived.length === 0)
    
    val nestedTask1 = nestedTasks(0)
    val nestedTask1TestEventHandler = new TestEventHandler
    val nestedTask1NestedTasks = nestedTask1.execute(nestedTask1TestEventHandler, Array(new TestLogger))
    assert(nestedTask1NestedTasks.size === 0)
    val nestedTask1SuccessEvents = nestedTask1TestEventHandler.successEventsReceived
    assert(nestedTask1SuccessEvents.size === 1)
    assertNestedSuiteSuccessEvent(nestedTask1SuccessEvents(0), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 1", "nested 1 test 1")
    assert(nestedTask1TestEventHandler.errorEventsReceived.length === 0)
    assert(nestedTask1TestEventHandler.failureEventsReceived.length === 0)
    assert(nestedTask1TestEventHandler.skippedEventsReceived.length === 0)
    
    val nestedTask2 = nestedTasks(1)
    val nestedTask2TestEventHandler = new TestEventHandler
    val nestedTask2NestedTasks = nestedTask2.execute(nestedTask2TestEventHandler, Array(new TestLogger))
    assert(nestedTask2NestedTasks.size === 0)
    val nestedTask2SuccessEvents = nestedTask2TestEventHandler.successEventsReceived
    assert(nestedTask2SuccessEvents.size === 1)
    assertNestedSuiteSuccessEvent(nestedTask2SuccessEvents(0), "org.scalatest.tools.scalasbt.SuiteWithNestedSuites", "nested 2", "nested 2 test 3")
    assert(nestedTask2TestEventHandler.errorEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.failureEventsReceived.length === 0)
    assert(nestedTask2TestEventHandler.skippedEventsReceived.length === 0)
  }
  
  test("ScalaTestRunner should return summary when 'done' is called, and throw IllegalStateException if 'done' method is called twice.") {
    val testLogger = new TestLogger
    val runner = framework.runner(Array("-oW"), Array.empty, testClassLoader)
    val task = runner.task("org.scalatest.tools.scalasbt.SampleSuite", subclassFingerprint, false, Array(new SuiteSelector()))
    task.execute(new TestEventHandler, Array(testLogger))
    val summaryText = runner.done.split("\n")
    assert(summaryText.size === 5)
    assert(summaryText(0).startsWith("Run completed in "))
    assert(summaryText(1) === "Total number of tests run: 3")
    assert(summaryText(2) === "Suites: completed 1, aborted 0")
    assert(summaryText(3) === "Tests: succeeded 3, failed 0, canceled 0, ignored 0, pending 0")
    assert(summaryText(4) === "All tests passed.")
    intercept[IllegalStateException] {
      runner.done()
    }
  }
  
  test("ScalaTest Task's tags method should return 'cpu' when suite class is annotated with @CPU") {
    val testLogger = new TestLogger
    val runner = framework.runner(Array("-oW"), Array.empty, testClassLoader)
    val task = runner.task("org.scalatest.tools.scalasbt.CPUTaggedSuite", subclassFingerprint, false, Array(new SuiteSelector))
    val taskTags = task.tags
    assert(taskTags.size === 1)
    assert(taskTags(0) === "cpu")
  }
  
  test("ScalaTest Task's tags method should return 'network' when suite class is annotated with @Network") {
    val testLogger = new TestLogger
    val runner = framework.runner(Array("-oW"), Array.empty, testClassLoader)
    val task = runner.task("org.scalatest.tools.scalasbt.NetworkTaggedSuite", subclassFingerprint, false, Array(new SuiteSelector))
    val taskTags = task.tags
    assert(taskTags.size === 1)
    assert(taskTags(0) === "network")
  }
  
  test("ScalaTest Task's tags method should return 'disk' when suite class is annotated with @Disk") {
    val testLogger = new TestLogger
    val runner = framework.runner(Array("-oW"), Array.empty, testClassLoader)
    val task = runner.task("org.scalatest.tools.scalasbt.DiskTaggedSuite", subclassFingerprint, false, Array(new SuiteSelector))
    val taskTags = task.tags
    assert(taskTags.size === 1)
    assert(taskTags(0) === "disk")
  }
  
  test("ScalaTest Task's tags method should return 'custom' when suite class is annotated with @TagAnnotation('custom')") {
    val testLogger = new TestLogger
    val runner = framework.runner(Array("-oW"), Array.empty, testClassLoader)
    val task = runner.task("org.scalatest.tools.scalasbt.CustomTaggedSuite", subclassFingerprint, false, Array(new SuiteSelector))
    val taskTags = task.tags
    assert(taskTags.size === 1)
    assert(taskTags(0) === "custom")
  }
}