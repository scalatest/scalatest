/*
 * Copyright 2001-2024 Artima, Inc.
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
import org.scalatest.events._
import Reporter.propagateDispose
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import org.scalatest.time.Span
import java.io.PrintStream

private[scalatest] class SuiteSortingReporter(dispatch: Reporter, val testSortingTimeout: Span, val out: PrintStream) extends CatchReporter with DistributedSuiteSorter {

  case class Slot(suiteId: String, doneEvent: Option[Event], includesDistributedTests: Boolean, testsCompleted: Boolean, ready: Boolean)

  @volatile private var slotListBuf = new ListBuffer[Slot]()
  private val slotMap = collection.mutable.HashMap[String, Slot]()
  // suiteEventMap is suite Id -> events for that suite (should be a Vector)
  private val suiteEventMap = collection.mutable.HashMap[String, Vector[Event]]()
  private val suiteReporterMap = collection.mutable.HashMap[String, Reporter]()

  def registerReporter(suiteId: String, reporter: Reporter): Unit =
    synchronized {
      suiteReporterMap += (suiteId -> reporter)
    }
  
  // Passed slot will always be the head of waitingBuffer
  class TimeoutTask(val slot: Slot) extends TimerTask {
    override def run(): Unit = {
      timeout()
    }
  }
  
  private var timeoutTask: Option[(TimeoutTask, Timer)] = None

  def doApply(event: Event): Unit = {
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
        case alertProvided: AlertProvided => // TODO: Maybe these should just fly out the door immediately. I think they should.
          alertProvided.nameInfo match {
            case Some(nameInfo) =>
              handleTestEvents(nameInfo.suiteId, alertProvided)
            case None => 
              dispatch(alertProvided)
          }
        case noteProvided: NoteProvided =>
          noteProvided.nameInfo match {
            case Some(nameInfo) =>
              handleTestEvents(nameInfo.suiteId, noteProvided)
            case None =>
              dispatch(noteProvided)
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

  private def dispatchToRegisteredSuiteReporter(suiteId: String, event: Event): Unit = {
    synchronized(suiteReporterMap.get(suiteId)) match {
      case Some(rep) => rep(event)
      case None =>
    }
  }

  // Handles just SuiteCompleted and SuiteAborted
  private def handleSuiteEvents(suiteId: String, event: Event): Unit = {
    val slot = slotMap(suiteId)
    val newSlot = slot.copy(doneEvent = Some(event), ready = (if (slot.includesDistributedTests) slot.testsCompleted else true))  // Assuming here that a done event hasn't already arrived
    slotMap.put(suiteId, newSlot)                     // Probably should fail on the second one
    val slotIdx = slotListBuf.indexOf(slot)
    if (slotIdx >= 0)                                 // In what case would it not be there?
      slotListBuf.update(slotIdx, newSlot)  // Why not fire ready events here? Oh, at end of apply
    else {
      dispatch(event) // could happens after timeout
      dispatchToRegisteredSuiteReporter(suiteId, event)
    }
  }
  // Handles SuiteStarting, TestStarting, TestIgnored, TestSucceeded, TestFailed, TestPending,
  // TestCanceled, InfoProvided, AlertProvided, NoteProvided, MarkupProvided, ScopeOpened, ScopeClosed, ScopePending
  private def handleTestEvents(suiteId: String, event: Event): Unit = {
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
    else {
      dispatch(event)
      dispatchToRegisteredSuiteReporter(suiteId, event)
    }
  }

  // Only called within synchronized
  private def fireReadyEvents(): Unit = {
    if (slotListBuf.size > 0) {
      val head = slotListBuf.head
      fireSuiteEvents(head.suiteId)
      if (head.ready) {
        for (doneEvent<- head.doneEvent) {
          dispatch(doneEvent)
          dispatchToRegisteredSuiteReporter(head.suiteId, doneEvent)
          // Only remove the suite reporter only if tests completed, don't remove if it is due to timeout.
          suiteReporterMap -= (head.suiteId)
        }
        slotListBuf = fireReadySuiteEvents(slotListBuf.tail)
        if (slotListBuf.size > 0) 
          scheduleTimeoutTask()
        else 
          cancelTimeoutTask()
      }
    }
  }

  // suiteId must exist in the suiteEventMap
  @tailrec
  private def fireSuiteEvents(suiteId: String): Unit = {
    // Fire all of them and empty it out. The done event is stored elsewhere
    suiteEventMap.get(suiteId) match {
      case Some(eventList) =>
        if (eventList.length > 1) {
          val head = eventList.head
          suiteEventMap.put(suiteId, eventList.tail)
          dispatch(head)
          dispatchToRegisteredSuiteReporter(suiteId, head)
          fireSuiteEvents(suiteId)
        }
        else if (eventList.length == 1) {
          val head = eventList.head
          suiteEventMap.put(suiteId, Vector.empty[Event])
          dispatch(head)
          dispatchToRegisteredSuiteReporter(suiteId, head)
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
        dispatchToRegisteredSuiteReporter(slot.suiteId, slot.doneEvent.get)
        suiteReporterMap -= (slot.suiteId)
    }
    undone
  }

  /**
    * This method is called after all tests in the suite completed execution, this implementation will update the slot state and fire all events accordingly.
    *
    * @param suiteId the <code>suiteId</code> for the suite that's completed its tests execution
    */
  def completedTests(suiteId: String): Unit = {
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

  /**
    * This internal method is called by DispatchReporter when a slow poke is triggered.
    *
    * @param event an <code>AlertProvided</code> event representing the slow poke.
    */
  private[scalatest] def slowpokeEvent(event: AlertProvided): Unit = {
    synchronized {
      synchronized(suiteReporterMap.values.headOption) match {
        case Some(rep) => rep(event)
        case None => // If no more active rep, well, probably fine then since this probably means that test is now completed.
      }
    }
  }

  // Will need a timeout. Hmm. Because can change it. Hmm. This is an issue. I wanted
  // suite's timeout to be 20% longer than the -T one. If an overridden testSortingTimeout timeout is shorter, then
  // that's no prob. But if it is longer, then the suiteTimeout will timeout first. I think that's fine. I'll
  // just document that behavior.
  /**
    * This method is called before first test in a suite is distributed to execute, this implementation will setup a new slot for the given suite.
    *
    * @param suiteId the <code>suiteId</code> for the suite that's starting to execute its tests
    */
  def distributingTests(suiteId: String): Unit = {
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

  /**
    * Dispose this reporter, will fire all pending events before disposing.
    */
  override def doDispose(): Unit = {
    fireReadyEvents()
  }
  
  // Also happening inside synchronized block
  private def scheduleTimeoutTask(): Unit = {
    val head = slotListBuf.head  // Assumes waitingBuffer is non-empty. Put a require there to make that obvious.
    timeoutTask match {
        case Some((oldTask, oldTimer)) => 
          if (head.suiteId != oldTask.slot.suiteId) {
            oldTask.cancel()
            oldTimer.cancel()
            val (task, timer) = (new TimeoutTask(head), new Timer)
            timeoutTask = Some((task, timer)) // Replace the old with the new
            timer.schedule(task, testSortingTimeout.millisPart)
          }
        case None => 
          val (task, timer) = (new TimeoutTask(head), new Timer)
          timeoutTask = Some((task, timer)) // Just create a new one
          timer.schedule(task, testSortingTimeout.millisPart)
      }
  }

  // Also happening inside synchronized block
  private def cancelTimeoutTask(): Unit = {
    timeoutTask match { // Waiting buffer is zero, so no timeout needed
      case Some((task, timer)) => 
        task.cancel()
        timer.cancel()
        timeoutTask = None
      case None =>
    }
  }
  
  private def timeout(): Unit = {
    synchronized {
      if (slotListBuf.size > 0) {
        val head = slotListBuf.head
        val (task, _) = timeoutTask.get
        if (task.slot.suiteId == head.suiteId) { // Probably a double check, or just in case there's race condition
          val newSlot = head.copy(ready = true) // Essentially, if time out, just say that one is ready. This test's events go out, and
          slotListBuf.update(0, newSlot)
        }
        fireReadyEvents()
      }
    }
  }
}
