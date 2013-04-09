package org.scalatest.tools

import org.scalatest.matchers.ShouldMatchers
import org.scalatest._
import events._
import events.SuiteCompleted
import events.SuiteStarting
import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest.Args
import org.scalatest.time.Span
import org.scalatest.time.Second
import org.scalatest.time.Seconds
import java.io.PrintStream
import java.io.ByteArrayOutputStream

class SuiteSortingReporterSpec extends FunSpec with ShouldMatchers with EventHelpers {

  def stringFor(event: Event): String = {
    event match {
      case e: ScopeOpened => e.message
      case e: ScopeClosed => e.message
      case e: TestStarting => e.testName
      case e: TestFailed => e.testName
      case e: TestSucceeded => e.testName
      case e: TestPending => e.testName
      case e: TestCanceled => e.testName
      case e: TestIgnored => e.testName
      case e: SuiteStarting => e.suiteId
      case e: SuiteCompleted => e.suiteId
      case e: SuiteAborted => e.suiteId
      case e: InfoProvided => e.message
      case _ => throw new RuntimeException("should never get here")
    }
  }

  def runSuites(reporter: Reporter, dss: Option[DistributedSuiteSorter]) {
    val spec1 = new RunInSpurtsSpec1
    val spec2 = new RunInSpurtsSpec2

    val tracker = new Tracker()

    reporter(SuiteStarting(tracker.nextOrdinal, spec1.suiteName, spec1.suiteId, Some(spec1.getClass.getName), None))

    spec1.run(None, Args(reporter, tracker = tracker, distributedSuiteSorter = dss))
    reporter(SuiteStarting(tracker.nextOrdinal, spec2.suiteName, spec2.suiteId, Some(spec2.getClass.getName), None))
    spec2.run(None, Args(reporter, tracker = tracker, distributedSuiteSorter = dss))
    spec1.batch = 1
    spec1.run(None, Args(reporter, tracker = tracker, distributedSuiteSorter = dss))
    reporter(SuiteCompleted(tracker.nextOrdinal, spec1.suiteName, spec1.suiteId, Some(spec1.getClass.getName), None))
    spec2.batch = 1
    spec2.run(None, Args(reporter, tracker = tracker, distributedSuiteSorter = dss))
    reporter(SuiteCompleted(tracker.nextOrdinal, spec2.suiteName, spec2.suiteId, Some(spec2.getClass.getName), None))
  }

  describe("The RunInSpurtsSpec classes") {
    they("should report half of their events each time they are run, so they can be used to simulate distributed suite testing") {

      // Not using SuiteSortingReporter here, to make sure that the events come out
      // in the order I expect, so that I'm sure they are actually being sorted when
      // they come out in a different order when using SuiteSortingReporter.
      val recordingReporter = new EventRecordingReporter()

      runSuites(recordingReporter, None)

      val recordedEvents = recordingReporter.eventsReceived
      // assert(false, recordedEvents.zipWithIndex.map { case (e, idx) => "check" + e.getClass.getSimpleName +
      //  "(recordedEvents(" + idx + "), \"" + stringFor(e) + "\")" }.mkString("\n"))
      checkSuiteStarting(recordedEvents(0), "org.scalatest.RunInSpurtsSpec1")
      checkScopeOpened(recordedEvents(1), "Thing 1")
      checkInfoProvided(recordedEvents(2), "In Before")
      checkTestStarting(recordedEvents(3), "Thing 1 do thing 1a")
      checkTestSucceeded(recordedEvents(4), "Thing 1 do thing 1a")
      checkInfoProvided(recordedEvents(5), "In After")
      checkInfoProvided(recordedEvents(6), "In Before")
      checkTestStarting(recordedEvents(7), "Thing 1 do thing 1b")
      checkTestSucceeded(recordedEvents(8), "Thing 1 do thing 1b")
      checkInfoProvided(recordedEvents(9), "In After")
      checkInfoProvided(recordedEvents(10), "In Before")
      checkTestStarting(recordedEvents(11), "Thing 1 do thing 1c")
      checkTestSucceeded(recordedEvents(12), "Thing 1 do thing 1c")
      checkInfoProvided(recordedEvents(13), "In After")
      checkScopeClosed(recordedEvents(14), "Thing 1")
      checkScopeOpened(recordedEvents(15), "Thing 2")
      checkInfoProvided(recordedEvents(16), "In Before")
      checkTestStarting(recordedEvents(17), "Thing 2 do thing 2a")
      checkTestSucceeded(recordedEvents(18), "Thing 2 do thing 2a")
      checkInfoProvided(recordedEvents(19), "In After")
      checkInfoProvided(recordedEvents(20), "In Before")
      checkTestStarting(recordedEvents(21), "Thing 2 do thing 2b")
      checkTestSucceeded(recordedEvents(22), "Thing 2 do thing 2b")
      checkInfoProvided(recordedEvents(23), "In After")
      checkInfoProvided(recordedEvents(24), "In Before")
      checkTestStarting(recordedEvents(25), "Thing 2 do thing 2c")
      checkTestSucceeded(recordedEvents(26), "Thing 2 do thing 2c")
      checkInfoProvided(recordedEvents(27), "In After")
      checkScopeClosed(recordedEvents(28), "Thing 2")
      checkSuiteStarting(recordedEvents(29), "org.scalatest.RunInSpurtsSpec2")
      checkScopeOpened(recordedEvents(30), "Thing 1")
      checkInfoProvided(recordedEvents(31), "In Before")
      checkTestStarting(recordedEvents(32), "Thing 1 do thing 1a")
      checkTestSucceeded(recordedEvents(33), "Thing 1 do thing 1a")
      checkInfoProvided(recordedEvents(34), "In After")
      checkInfoProvided(recordedEvents(35), "In Before")
      checkTestStarting(recordedEvents(36), "Thing 1 do thing 1b")
      checkTestSucceeded(recordedEvents(37), "Thing 1 do thing 1b")
      checkInfoProvided(recordedEvents(38), "In After")
      checkInfoProvided(recordedEvents(39), "In Before")
      checkTestStarting(recordedEvents(40), "Thing 1 do thing 1c")
      checkTestSucceeded(recordedEvents(41), "Thing 1 do thing 1c")
      checkInfoProvided(recordedEvents(42), "In After")
      checkScopeClosed(recordedEvents(43), "Thing 1")
      checkScopeOpened(recordedEvents(44), "Thing 2")
      checkInfoProvided(recordedEvents(45), "In Before")
      checkTestStarting(recordedEvents(46), "Thing 2 do thing 2a")
      checkTestSucceeded(recordedEvents(47), "Thing 2 do thing 2a")
      checkInfoProvided(recordedEvents(48), "In After")
      checkInfoProvided(recordedEvents(49), "In Before")
      checkTestStarting(recordedEvents(50), "Thing 2 do thing 2b")
      checkTestSucceeded(recordedEvents(51), "Thing 2 do thing 2b")
      checkInfoProvided(recordedEvents(52), "In After")
      checkInfoProvided(recordedEvents(53), "In Before")
      checkTestStarting(recordedEvents(54), "Thing 2 do thing 2c")
      checkTestSucceeded(recordedEvents(55), "Thing 2 do thing 2c")
      checkInfoProvided(recordedEvents(56), "In After")
      checkScopeClosed(recordedEvents(57), "Thing 2")
      checkScopeOpened(recordedEvents(58), "Thing 1")
      checkInfoProvided(recordedEvents(59), "In Before")
      checkTestStarting(recordedEvents(60), "Thing 1 do thing 1a")
      checkTestSucceeded(recordedEvents(61), "Thing 1 do thing 1a")
      checkInfoProvided(recordedEvents(62), "In After")
      checkInfoProvided(recordedEvents(63), "In Before")
      checkTestStarting(recordedEvents(64), "Thing 1 do thing 1b")
      checkTestSucceeded(recordedEvents(65), "Thing 1 do thing 1b")
      checkInfoProvided(recordedEvents(66), "In After")
      checkInfoProvided(recordedEvents(67), "In Before")
      checkTestStarting(recordedEvents(68), "Thing 1 do thing 1c")
      checkTestSucceeded(recordedEvents(69), "Thing 1 do thing 1c")
      checkInfoProvided(recordedEvents(70), "In After")
      checkScopeClosed(recordedEvents(71), "Thing 1")
      checkScopeOpened(recordedEvents(72), "Thing 2")
      checkInfoProvided(recordedEvents(73), "In Before")
      checkTestStarting(recordedEvents(74), "Thing 2 do thing 2a")
      checkTestSucceeded(recordedEvents(75), "Thing 2 do thing 2a")
      checkInfoProvided(recordedEvents(76), "In After")
      checkInfoProvided(recordedEvents(77), "In Before")
      checkTestStarting(recordedEvents(78), "Thing 2 do thing 2b")
      checkTestSucceeded(recordedEvents(79), "Thing 2 do thing 2b")
      checkInfoProvided(recordedEvents(80), "In After")
      checkInfoProvided(recordedEvents(81), "In Before")
      checkTestStarting(recordedEvents(82), "Thing 2 do thing 2c")
      checkTestSucceeded(recordedEvents(83), "Thing 2 do thing 2c")
      checkInfoProvided(recordedEvents(84), "In After")
      checkScopeClosed(recordedEvents(85), "Thing 2")
      checkSuiteCompleted(recordedEvents(86), "org.scalatest.RunInSpurtsSpec1")
      checkScopeOpened(recordedEvents(87), "Thing 1")
      checkInfoProvided(recordedEvents(88), "In Before")
      checkTestStarting(recordedEvents(89), "Thing 1 do thing 1a")
      checkTestSucceeded(recordedEvents(90), "Thing 1 do thing 1a")
      checkInfoProvided(recordedEvents(91), "In After")
      checkInfoProvided(recordedEvents(92), "In Before")
      checkTestStarting(recordedEvents(93), "Thing 1 do thing 1b")
      checkTestSucceeded(recordedEvents(94), "Thing 1 do thing 1b")
      checkInfoProvided(recordedEvents(95), "In After")
      checkInfoProvided(recordedEvents(96), "In Before")
      checkTestStarting(recordedEvents(97), "Thing 1 do thing 1c")
      checkTestSucceeded(recordedEvents(98), "Thing 1 do thing 1c")
      checkInfoProvided(recordedEvents(99), "In After")
      checkScopeClosed(recordedEvents(100), "Thing 1")
      checkScopeOpened(recordedEvents(101), "Thing 2")
      checkInfoProvided(recordedEvents(102), "In Before")
      checkTestStarting(recordedEvents(103), "Thing 2 do thing 2a")
      checkTestSucceeded(recordedEvents(104), "Thing 2 do thing 2a")
      checkInfoProvided(recordedEvents(105), "In After")
      checkInfoProvided(recordedEvents(106), "In Before")
      checkTestStarting(recordedEvents(107), "Thing 2 do thing 2b")
      checkTestSucceeded(recordedEvents(108), "Thing 2 do thing 2b")
      checkInfoProvided(recordedEvents(109), "In After")
      checkInfoProvided(recordedEvents(110), "In Before")
      checkTestStarting(recordedEvents(111), "Thing 2 do thing 2c")
      checkTestSucceeded(recordedEvents(112), "Thing 2 do thing 2c")
      checkInfoProvided(recordedEvents(113), "In After")
      checkScopeClosed(recordedEvents(114), "Thing 2")
      checkSuiteCompleted(recordedEvents(115), "org.scalatest.RunInSpurtsSpec2")
    }
  }

  describe("SuiteSortingReporter") {

    it("should sort events received intermixed from two suites executing concurrently.") {

      val recordingReporter = new EventRecordingReporter()

      val ssr = new SuiteSortingReporter(recordingReporter, Span(5, Seconds), new PrintStream(new ByteArrayOutputStream))

      runSuites(ssr, Some(ssr))

      val recordedEvents = recordingReporter.eventsReceived
      // assert(false, recordedEvents.zipWithIndex.map { case (e, idx) => "check" + e.getClass.getSimpleName +
      //  "(recordedEvents(" + idx + "), \"" + stringFor(e) + "\")" }.mkString("\n"))
      checkSuiteStarting(recordedEvents(0), "org.scalatest.RunInSpurtsSpec1")
      checkScopeOpened(recordedEvents(1), "Thing 1")
      checkInfoProvided(recordedEvents(2), "In Before")
      checkTestStarting(recordedEvents(3), "Thing 1 do thing 1a")
      checkTestSucceeded(recordedEvents(4), "Thing 1 do thing 1a")
      checkInfoProvided(recordedEvents(5), "In After")
      checkInfoProvided(recordedEvents(6), "In Before")
      checkTestStarting(recordedEvents(7), "Thing 1 do thing 1b")
      checkTestSucceeded(recordedEvents(8), "Thing 1 do thing 1b")
      checkInfoProvided(recordedEvents(9), "In After")
      checkInfoProvided(recordedEvents(10), "In Before")
      checkTestStarting(recordedEvents(11), "Thing 1 do thing 1c")
      checkTestSucceeded(recordedEvents(12), "Thing 1 do thing 1c")
      checkInfoProvided(recordedEvents(13), "In After")
      checkScopeClosed(recordedEvents(14), "Thing 1")
      checkScopeOpened(recordedEvents(15), "Thing 2")
      checkInfoProvided(recordedEvents(16), "In Before")
      checkTestStarting(recordedEvents(17), "Thing 2 do thing 2a")
      checkTestSucceeded(recordedEvents(18), "Thing 2 do thing 2a")
      checkInfoProvided(recordedEvents(19), "In After")
      checkInfoProvided(recordedEvents(20), "In Before")
      checkTestStarting(recordedEvents(21), "Thing 2 do thing 2b")
      checkTestSucceeded(recordedEvents(22), "Thing 2 do thing 2b")
      checkInfoProvided(recordedEvents(23), "In After")
      checkInfoProvided(recordedEvents(24), "In Before")
      checkTestStarting(recordedEvents(25), "Thing 2 do thing 2c")
      checkTestSucceeded(recordedEvents(26), "Thing 2 do thing 2c")
      checkInfoProvided(recordedEvents(27), "In After")
      checkScopeClosed(recordedEvents(28), "Thing 2")
      checkScopeOpened(recordedEvents(29), "Thing 1")
      checkInfoProvided(recordedEvents(30), "In Before")
      checkTestStarting(recordedEvents(31), "Thing 1 do thing 1a")
      checkTestSucceeded(recordedEvents(32), "Thing 1 do thing 1a")
      checkInfoProvided(recordedEvents(33), "In After")
      checkInfoProvided(recordedEvents(34), "In Before")
      checkTestStarting(recordedEvents(35), "Thing 1 do thing 1b")
      checkTestSucceeded(recordedEvents(36), "Thing 1 do thing 1b")
      checkInfoProvided(recordedEvents(37), "In After")
      checkInfoProvided(recordedEvents(38), "In Before")
      checkTestStarting(recordedEvents(39), "Thing 1 do thing 1c")
      checkTestSucceeded(recordedEvents(40), "Thing 1 do thing 1c")
      checkInfoProvided(recordedEvents(41), "In After")
      checkScopeClosed(recordedEvents(42), "Thing 1")
      checkScopeOpened(recordedEvents(43), "Thing 2")
      checkInfoProvided(recordedEvents(44), "In Before")
      checkTestStarting(recordedEvents(45), "Thing 2 do thing 2a")
      checkTestSucceeded(recordedEvents(46), "Thing 2 do thing 2a")
      checkInfoProvided(recordedEvents(47), "In After")
      checkInfoProvided(recordedEvents(48), "In Before")
      checkTestStarting(recordedEvents(49), "Thing 2 do thing 2b")
      checkTestSucceeded(recordedEvents(50), "Thing 2 do thing 2b")
      checkInfoProvided(recordedEvents(51), "In After")
      checkInfoProvided(recordedEvents(52), "In Before")
      checkTestStarting(recordedEvents(53), "Thing 2 do thing 2c")
      checkTestSucceeded(recordedEvents(54), "Thing 2 do thing 2c")
      checkInfoProvided(recordedEvents(55), "In After")
      checkScopeClosed(recordedEvents(56), "Thing 2")
      checkSuiteCompleted(recordedEvents(57), "org.scalatest.RunInSpurtsSpec1")
      checkSuiteStarting(recordedEvents(58), "org.scalatest.RunInSpurtsSpec2")
      checkScopeOpened(recordedEvents(59), "Thing 1")
      checkInfoProvided(recordedEvents(60), "In Before")
      checkTestStarting(recordedEvents(61), "Thing 1 do thing 1a")
      checkTestSucceeded(recordedEvents(62), "Thing 1 do thing 1a")
      checkInfoProvided(recordedEvents(63), "In After")
      checkInfoProvided(recordedEvents(64), "In Before")
      checkTestStarting(recordedEvents(65), "Thing 1 do thing 1b")
      checkTestSucceeded(recordedEvents(66), "Thing 1 do thing 1b")
      checkInfoProvided(recordedEvents(67), "In After")
      checkInfoProvided(recordedEvents(68), "In Before")
      checkTestStarting(recordedEvents(69), "Thing 1 do thing 1c")
      checkTestSucceeded(recordedEvents(70), "Thing 1 do thing 1c")
      checkInfoProvided(recordedEvents(71), "In After")
      checkScopeClosed(recordedEvents(72), "Thing 1")
      checkScopeOpened(recordedEvents(73), "Thing 2")
      checkInfoProvided(recordedEvents(74), "In Before")
      checkInfoProvided(recordedEvents(78), "In Before")
      checkTestStarting(recordedEvents(79), "Thing 2 do thing 2b")
      checkTestSucceeded(recordedEvents(80), "Thing 2 do thing 2b")
      checkInfoProvided(recordedEvents(81), "In After")
      checkInfoProvided(recordedEvents(82), "In Before")
      checkTestStarting(recordedEvents(83), "Thing 2 do thing 2c")
      checkTestSucceeded(recordedEvents(84), "Thing 2 do thing 2c")
      checkInfoProvided(recordedEvents(85), "In After")
      checkScopeClosed(recordedEvents(86), "Thing 2")
      checkScopeOpened(recordedEvents(87), "Thing 1")
      checkInfoProvided(recordedEvents(88), "In Before")
      checkTestStarting(recordedEvents(89), "Thing 1 do thing 1a")
      checkTestSucceeded(recordedEvents(90), "Thing 1 do thing 1a")
      checkInfoProvided(recordedEvents(91), "In After")
      checkInfoProvided(recordedEvents(92), "In Before")
      checkTestStarting(recordedEvents(93), "Thing 1 do thing 1b")
      checkTestSucceeded(recordedEvents(94), "Thing 1 do thing 1b")
      checkInfoProvided(recordedEvents(95), "In After")
      checkInfoProvided(recordedEvents(96), "In Before")
      checkTestStarting(recordedEvents(97), "Thing 1 do thing 1c")
      checkTestSucceeded(recordedEvents(98), "Thing 1 do thing 1c")
      checkInfoProvided(recordedEvents(99), "In After")
      checkScopeClosed(recordedEvents(100), "Thing 1")
      checkScopeOpened(recordedEvents(101), "Thing 2")
      checkInfoProvided(recordedEvents(102), "In Before")
      checkTestStarting(recordedEvents(103), "Thing 2 do thing 2a")
      checkTestSucceeded(recordedEvents(104), "Thing 2 do thing 2a")
      checkInfoProvided(recordedEvents(105), "In After")
      checkInfoProvided(recordedEvents(106), "In Before")
      checkTestStarting(recordedEvents(107), "Thing 2 do thing 2b")
      checkTestSucceeded(recordedEvents(108), "Thing 2 do thing 2b")
      checkInfoProvided(recordedEvents(109), "In After")
      checkInfoProvided(recordedEvents(110), "In Before")
      checkTestStarting(recordedEvents(111), "Thing 2 do thing 2c")
      checkTestSucceeded(recordedEvents(112), "Thing 2 do thing 2c")
      checkInfoProvided(recordedEvents(113), "In After")
      checkScopeClosed(recordedEvents(114), "Thing 2")
      checkSuiteCompleted(recordedEvents(115), "org.scalatest.RunInSpurtsSpec2")
    }
    
    it("should fire blocking suite's events when timeout, and just fire the missing event directly without waiting when received later.") {
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new SuiteSortingReporter(recordingReporter, Span(1, Second), new PrintStream(new ByteArrayOutputStream))
      
      val tracker = new Tracker()
      
      dispatch(SuiteStarting(tracker.nextOrdinal, "suite1", "suite1", Some("suite1 class name")))
      dispatch(SuiteStarting(tracker.nextOrdinal, "suite2", "suite2", Some("suite2 class name")))
      dispatch(TestStarting(tracker.nextOrdinal, "suite2", "suite2", Some("suite2 class name"), "Suite 2 Test", "Suite 2 Test"))
      dispatch(SuiteCompleted(tracker.nextOrdinal, "suite2", "suite2", Some("suite2 class name")))
      dispatch(TestStarting(tracker.nextOrdinal, "suite1", "suite1", Some("suite1 class name"), "Suite 1 Test", "Suite 1 Test"))
      dispatch(TestSucceeded(tracker.nextOrdinal, "suite1", "suite1", Some("suite1 class name"), "Suite 1 Test", "Suite 1 Test", Vector.empty))
      dispatch(TestSucceeded(tracker.nextOrdinal, "suite2", "suite2", Some("suite2 class name"), "Suite 2 Test", "Suite 2 Test", Vector.empty))
      
      Thread.sleep(1500) // Wait for the SuiteSortingReporter timeout, which is 1 second (set above)
      dispatch(SuiteCompleted(tracker.nextOrdinal, "suite1", "suite1", Some("suite1 class name"), None))
      
      val recordedEvents = recordingReporter.eventsReceived
      assert(recordedEvents.size === 8)
      checkSuiteStarting(recordedEvents(0), "suite1")
      checkTestStarting(recordedEvents(1), "Suite 1 Test")
      checkTestSucceeded(recordedEvents(2), "Suite 1 Test")
      // From here the SuiteCompleted from Suite 1 will not come until after timeout, after timeout, Suite 2 events will go out first.
      checkSuiteStarting(recordedEvents(3), "suite2")
      checkTestStarting(recordedEvents(4), "Suite 2 Test")
      checkTestSucceeded(recordedEvents(5), "Suite 2 Test")
      checkSuiteCompleted(recordedEvents(6), "suite2")
      // Now this guy finally arrive
      checkSuiteCompleted(recordedEvents(7), "suite1")
    }
  }
}