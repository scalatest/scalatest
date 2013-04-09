package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import org.scalatest.time.Span
import java.util.Timer
import java.util.TimerTask
import java.io.PrintStream

private[scalatest] class SuiteSortingReporter(dispatch: Reporter, sortingTimeout: Span, val out: PrintStream) extends CatchReporter with DistributedSuiteSorter {

  case class Slot(suiteId: String, doneEvent: Option[Event], includesDistributedTests: Boolean, testsCompleted: Boolean, ready: Boolean)

  @volatile private var slotListBuf = new ListBuffer[Slot]()
  private val slotMap = collection.mutable.HashMap[String, Slot]()
  // suiteEventMap is suite Id -> events for that suite (should be a Vector)
  private val suiteEventMap = collection.mutable.HashMap[String, Vector[Event]]()
  
  // Passed slot will always be the head of waitingBuffer
  class TimeoutTask(val slot: Slot) extends TimerTask {
    override def run() {
      timeout()
    }
  }
  
  private val timer = new Timer
  private var timeoutTask: Option[TimeoutTask] = None

  def doApply(event: Event) {
    synchronized {
      event match {
        case suiteStarting: SuiteStarting =>
          // if distributingTests is called (in case of the suite is ParallelTestExecution), the slot is already exists
          val slot = slotMap.get(suiteStarting.suiteId) match {
            case Some(s) => s
            case None => 
              val newSlot = Slot(suiteStarting.suiteId, None, false, false, false)
              slotMap.put(suiteStarting.suiteId, newSlot)
              newSlot
          }
          slotListBuf += slot
          // if it is the head, we should start the timer, because it is possible that this slot has no event coming later and it keeps blocking 
          // without the timer.
          if (slotListBuf.size == 1) 
            scheduleTimeoutTask()
          handleTestEvents(suiteStarting.suiteId, suiteStarting)

        case suiteCompleted: SuiteCompleted =>
          handleSuiteEvents(suiteCompleted.suiteId, suiteCompleted)
        case suiteAborted: SuiteAborted =>
          handleSuiteEvents(suiteAborted.suiteId, suiteAborted)
        case testStarting: TestStarting =>
          handleTestEvents(testStarting.suiteId, testStarting)
        case testIgnored: TestIgnored =>
          handleTestEvents(testIgnored.suiteId, testIgnored)
        case testSucceeded: TestSucceeded =>
          handleTestEvents(testSucceeded.suiteId, testSucceeded)
        case testFailed: TestFailed =>
          handleTestEvents(testFailed.suiteId, testFailed)
        case testPending: TestPending =>
          handleTestEvents(testPending.suiteId, testPending)
        case testCanceled: TestCanceled =>
          handleTestEvents(testCanceled.suiteId, testCanceled)
        case infoProvided: InfoProvided =>
          infoProvided.nameInfo match {
            case Some(nameInfo) =>
              handleTestEvents(nameInfo.suiteId, infoProvided)
            case None => // Under what condition will reach here?
              dispatch(infoProvided)
          }
        case markupProvided: MarkupProvided =>
          markupProvided.nameInfo match {
            case Some(nameInfo) =>
              handleTestEvents(nameInfo.suiteId, markupProvided)
            case None => // Under what condition will reach here?
              dispatch(markupProvided)
          }
        case scopeOpened: ScopeOpened =>
          handleTestEvents(scopeOpened.nameInfo.suiteId, scopeOpened)
        case scopeClosed: ScopeClosed =>
          handleTestEvents(scopeClosed.nameInfo.suiteId, scopeClosed)
        case scopePending: ScopePending => 
          handleTestEvents(scopePending.nameInfo.suiteId, scopePending)
        case _ =>
          dispatch(event)  // Just dispatch it if got unexpected event.
      }
      fireReadyEvents()
    }
  }

  // Handles just SuiteCompleted and SuiteAborted
  private def handleSuiteEvents(suiteId: String, event: Event) {
    val slot = slotMap(suiteId)
    val newSlot = slot.copy(doneEvent = Some(event), ready = (if (slot.includesDistributedTests) slot.testsCompleted else true))  // Assuming here that a done event hasn't already arrived
    slotMap.put(suiteId, newSlot)                     // Probably should fail on the second one
    val slotIdx = slotListBuf.indexOf(slot)
    if (slotIdx >= 0)                                 // In what case would it not be there?
      slotListBuf.update(slotIdx, newSlot)  // Why not fire ready events here? Oh, at end of apply
    else
      dispatch(event)  // could happens after timeout
  }
  // Handles SuiteStarting, TestStarting, TestIgnored, TestSucceeded, TestFailed, TestPending,
  // TestCanceled, InfoProvided, MarkupProvided, ScopeOpened, ScopeClosed, ScopePending
  private def handleTestEvents(suiteId: String, event: Event) {
    val slot = slotMap(suiteId)
    val slotIdx = slotListBuf.indexOf(slot)
    if (slotIdx >= 0) {
      // Only keep the events if the slot is still in slotListBuf
      suiteEventMap.get(suiteId) match { // Can probably use the transform or some such method
        case Some(eventList) =>
          suiteEventMap.put(suiteId, eventList :+ event)
        case None =>                                     // oldest events at front of vector
          suiteEventMap.put(suiteId, Vector(event))
      }
    }
    else
      dispatch(event)
  }

  // Only called within synchronized
  private def fireReadyEvents() {
    if (slotListBuf.size > 0) {
      val head = slotListBuf.head
      fireSuiteEvents(head.suiteId)
      if (head.ready) {
        for (doneEvent<- head.doneEvent)
          dispatch(doneEvent)
        slotListBuf = fireReadySuiteEvents(slotListBuf.tail)
        if (slotListBuf.size > 0) 
          scheduleTimeoutTask()
      }
    }
  }

  // suiteId must exist in the suiteEventMap
  @tailrec
  private def fireSuiteEvents(suiteId: String) {
    // Fire all of them and empty it out. The done event is stored elsewhere
    suiteEventMap.get(suiteId) match {
      case Some(eventList) =>
        if (eventList.length > 1) {
          val head = eventList.head
          suiteEventMap.put(suiteId, eventList.tail)
          dispatch(head)
          fireSuiteEvents(suiteId)
        }
        else if (eventList.length == 1) {
          val head = eventList.head
          suiteEventMap.put(suiteId, Vector.empty[Event])
          dispatch(head)
        }
      case None =>
      // Unable to get event vector from map, shouldn't happen
    }
  }

  private def fireReadySuiteEvents(remainingSlotList: ListBuffer[Slot]): ListBuffer[Slot] = {
    val (done, undone) = remainingSlotList.span(_.ready) // Grab all the done slots
    done.foreach {
      slot =>
        fireSuiteEvents(slot.suiteId)
        dispatch(slot.doneEvent.get)
    }
    undone
  }
  
  def completedTests(suiteId: String) {
    synchronized {
      val slot = slotMap(suiteId)
      val newSlot = slot.copy(testsCompleted = true, ready = slot.doneEvent.isDefined)
      slotMap.put(suiteId, newSlot)
      val slotIdx = slotListBuf.indexOf(slot)
      if (slotIdx >= 0)
        slotListBuf.update(slotIdx, newSlot)
      fireReadyEvents()
    }
  }

  // Will need a timeout. Hmm. Because can change it. Hmm. This is an issue. I wanted
  // suite's timeout to be 20% longer than the -T one. If an overridden sortingTimeout timeout is shorter, then
  // that's no prob. But if it is longer, then the suiteTimeout will timeout first. I think that's fine. I'll
  // just document that behavior.
  def distributingTests(suiteId: String) {
    synchronized {
      slotMap.get(suiteId) match {
        case Some(slot) => 
          val newSlot = slot.copy(includesDistributedTests = true)
          slotMap.put(suiteId, newSlot)
           val slotIdx = slotListBuf.indexOf(slot)
          if (slotIdx >= 0)
            slotListBuf.update(slotIdx, newSlot)
        case None =>
          slotMap.put(suiteId, Slot(suiteId, None, true, false, false))
      }
    }
  }

  override def doDispose() = {
    fireReadyEvents()
  }
  
  // Also happening inside synchronized block
  private def scheduleTimeoutTask() {
    val head = slotListBuf.head  // Assumes waitingBuffer is non-empty. Put a require there to make that obvious.
    timeoutTask match {
        case Some(task) => 
          if (head.suiteId != task.slot.suiteId) {
            task.cancel()
            timeoutTask = Some(new TimeoutTask(head)) // Replace the old with the new
            timer.schedule(timeoutTask.get, sortingTimeout.millisPart)
          }
        case None => 
          timeoutTask = Some(new TimeoutTask(head)) // Just create a new one
          timer.schedule(timeoutTask.get, sortingTimeout.millisPart)
      }
  }
  
  private def timeout() {
    synchronized {
      if (slotListBuf.size > 0) {
        val head = slotListBuf.head
        if (timeoutTask.get.slot.suiteId == head.suiteId) { // Probably a double check, or just in case there's race condition
          val newSlot = head.copy(ready = true) // Essentially, if time out, just say that one is ready. This test's events go out, and
          slotListBuf.update(0, newSlot)
        }
        fireReadyEvents()
      }
    }
  }
}
