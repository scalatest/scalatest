/*
 * Copyright 2001-2008 Artima, Inc.
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

import java.util.concurrent.CountDownLatch
import java.io.PrintStream
import org.scalatest.events._
import DispatchReporter.propagateDispose
import java.util.concurrent.LinkedBlockingQueue

/**
 * A <code>Reporter</code> that dispatches test results to other <code>Reporter</code>s.
 * Attempts to dispatch each method invocation to each contained <code>Reporter</code>,
 * even if some <code>Reporter</code> methods throw <code>Exception</code>s. Catches
 * <code>Exception</code>s thrown by <code>Reporter</code> methods and prints error
 * messages to the standard error stream.
 *
 * The primary constructor creates a new <code>DispatchReporter</code> with specified <code>Reporter</code>s list.
 * Each object in the <code>reporters</code> list must implement <code>Reporter</code>.
 *
 * @param reporters the initial <code>Reporter</code>s list for this
 * <code>DispatchReporter</code>
 * @throws NullPointerException if <code>reporters</code> is <code>null</code>.
 * @author Bill Venners
 */
private[scalatest] class DispatchReporter(val reporters: List[Reporter], val out: PrintStream) extends CatchReporter {

  private case object Dispose

  private val latch = new CountDownLatch(1)

  // Can be either Event or Dispose.type. Be nice to capture that in the type param.
  private val queue = new LinkedBlockingQueue[AnyRef]

  class Propagator extends Runnable {

    def run() {

      var alive = true // local variable. Only used by the Propagator's thread, so no need for synchronization
  
      class Counter {
        var testsSucceededCount = 0
        var testsFailedCount = 0
        var testsIgnoredCount = 0
        var testsCanceledCount = 0
        var testsPendingCount = 0
        var suitesCompletedCount = 0
        var suitesAbortedCount = 0
        var scopesPendingCount = 0
      }
  
      val counterMap = scala.collection.mutable.Map[Int, Counter]()
  
      def incrementCount(event: Event, f: (Counter) => Unit) {
        val runStamp = event.ordinal.runStamp
        if (counterMap.contains(runStamp)) {
          val counter = counterMap(runStamp)
          f(counter)
        }
        else {
          val counter = new Counter
          f(counter)
          counterMap(runStamp) = counter
        }
      }
  
      // If None, that means don't update the summary so forward the old event. If Some,
      // create a new event with everything the same except the old summary replaced by the new one
      def updatedSummary(oldSummary: Option[Summary], ordinal: Ordinal): Option[Summary] = {
        oldSummary match {
          case None if (counterMap.contains(ordinal.runStamp)) => {
              // Update the RunAborted so that it is the same except it has a new Some(Summary)
              val counter = counterMap(ordinal.runStamp)
              Some(
                Summary(
                  counter.testsSucceededCount,
                  counter.testsFailedCount,
                  counter.testsIgnoredCount,
                  counter.testsPendingCount,
                  counter.testsCanceledCount,
                  counter.suitesCompletedCount,
                  counter.suitesAbortedCount, 
                  counter.scopesPendingCount
                )
              )
            }
           case _ => None // Also pass the old None summary through if it isn't in the counterMap
        }
      }
  
      while (alive) {
        queue.take() match {
          case event: Event => 
            try {
              // The event will only actually be updated if it it is a RunCompleted/Aborted/Stopped event with None
              // as its summary and its runstamp has a counter entry. In that case, it will be given a Summary taken
              // from the counter. (And the counter will be removed from the counterMap.) These are counted here, because
              // they need to be counted on this side of any FilterReporters that may be in place. (In early versions of
              // ScalaTest, these were wrongly being counted by the reporters themselves, so if a FilterReporter filtered
              // out TestSucceeded events, then they just weren't being counted.
              val updatedEvent =
                event match {
  
                  case _: RunStarting => counterMap(event.ordinal.runStamp) = new Counter; event
  
                  case _: TestSucceeded => incrementCount(event, _.testsSucceededCount += 1); event
                  case _: TestFailed => incrementCount(event, _.testsFailedCount += 1); event
                  case _: TestIgnored => incrementCount(event, _.testsIgnoredCount += 1); event
                  case _: TestCanceled => incrementCount(event, _.testsCanceledCount += 1); event
                  case _: TestPending => incrementCount(event, _.testsPendingCount += 1); event
                  case _: SuiteCompleted => incrementCount(event, _.suitesCompletedCount += 1); event
                  case _: SuiteAborted => incrementCount(event, _.suitesAbortedCount += 1); event
                  case _: ScopePending => incrementCount(event, _.scopesPendingCount += 1); event
  
                  case oldRunCompleted @ RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
                    updatedSummary(summary, ordinal) match {
                      case None => oldRunCompleted
                      case newSummary @ Some(_) =>
                        counterMap.remove(ordinal.runStamp)
                        // Update the RunCompleted so that it is the same except it has a new Some(Summary)
                        RunCompleted(ordinal, duration, newSummary, formatter, location, payload, threadName, timeStamp)
                    }
        
                  case oldRunStopped @ RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
                    updatedSummary(summary, ordinal) match {
                      case None => oldRunStopped
                      case newSummary @ Some(_) =>
                        counterMap.remove(ordinal.runStamp)
                        // Update the RunStopped so that it is the same except it has a new Some(Summary)
                        RunStopped(ordinal, duration, newSummary, formatter, location, payload, threadName, timeStamp)
                    }
                  
                  case oldRunAborted @ RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 
                    updatedSummary(summary, ordinal) match {
                      case None => oldRunAborted
                      case newSummary @ Some(_) =>
                        counterMap.remove(ordinal.runStamp)
                        // Update the RunAborted so that it is the same except it has a new Some(Summary)
                        RunAborted(ordinal, message, throwable, duration, newSummary, formatter, location, payload, threadName, timeStamp)
                    }
                  
                  case _ => event
                }
              for (report <- reporters)
                report(updatedEvent)
            }
            catch {
              case e: Exception => 
                val stringToPrint = Resources("reporterThrew", event)
                out.println(stringToPrint)
                e.printStackTrace(out)
            }
          case Dispose =>
            try {
              for (reporter <- reporters)
                propagateDispose(reporter)
            }
            catch {
              case e: Exception =>
                val stringToPrint = Resources("reporterDisposeThrew")
                out.println(stringToPrint)
                e.printStackTrace(out)
            }
            finally {
              alive = false
              latch.countDown()
            }
        }
      }
    }
  }

  private val propagator = new Propagator
  (new Thread(propagator)).start()

  def this(reporters: List[Reporter]) = this(reporters, System.out)
  def this(reporter: Reporter) = this(List(reporter), System.out)

  // Invokes dispose on each Reporter in this DispatchReporter's reporters list.
  // This method puts an event in the queue that is being used to serialize
  // events, and at some time later the propagator's thread will attempts to invoke
  // dispose on each contained Reporter, even if some Reporter's dispose methods throw
  // Exceptions. This method catches any Exception thrown by
  // a dispose method and handles it by printing an error message to the
  // standard error stream. Once finished with that, the propagator's thread will return.
  //
  // This method will not return until the propagator's thread has exited.
  //
  def dispatchDisposeAndWaitUntilDone() {
    queue.put(Dispose)
    latch.await()
  }

  override def apply(event: Event) {
    queue.put(event)
  }
  
  def doApply(event: Event) {}
  
  def doDispose() {
    dispatchDisposeAndWaitUntilDone()
  }
  
  def isDisposed = latch.getCount == 0
}

// TODO: Not a real problem, but if a DispatchReporter ever got itself in
// its list of reporters, this would end up being an infinite loop. But
// That first part, a DispatchReporter getting itself in there would be the real
// bug.
private[scalatest] object DispatchReporter {

  def propagateDispose(reporter: Reporter) {
    reporter match {
      case dispatchReporter: DispatchReporter => dispatchReporter.dispatchDisposeAndWaitUntilDone()
      case resourcefulReporter: ResourcefulReporter => resourcefulReporter.dispose()
      case _ =>
    }
  }
}
