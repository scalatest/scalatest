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
package org.scalatest

import java.util.concurrent.atomic.AtomicReference
import MessageRecorder.RecordedMessageEventFun
import MessageRecorder.ConcurrentMessageFiringFun
import org.scalatest.events.Location
import org.scalatest.events.LineInFile
import org.scalatest.events.Event
import org.scalatest.events.RecordableEvent
import org.scalatest.events.NoteProvided
import org.scalatest.events.AlertProvided
import org.scalatest.events.NotificationEvent
import org.scalactic.Requirements._
import org.scalactic.source

/*
 This is used by Suite and test informers created as tests run, which therefore have
 populated defined NameInfos. These informers are returned by info in FunSuite and Spec,
 or passed to test methods that take an Informer in Suite, for example. If called by the
 thread that constructed them, which is the thread that was executing the suite and the tests
 inside the suite, then that NameInfo should be propagated. However, if a test starts other
 threads for a multi-threaded test, and those threads apply the Informer, then the NameInfo
 should *not* be propagated, because otherwise it could become very confusing to figure out
 what came from where in the report. Threads started by the test could outlast the thread
 that was running the test, for example. There will be a thread-name, so they can use that
 to figure out who sent what. And threads that call these informers will share a Tracker with
 the thread that was running the tests, so they should be ordered close together after
 sorting by Ordinal. But that's it. NameInfo only goes out when the thread running a test
 or suite applies the Informer.

 This in turn means that a reporter may get hit by multiple threads sending InfoProvided
 messages. If run with the Runner, that will be OK, because DispatchReporter will be in front
 serializing events with its actor. If run() is invoked directly on a suite instance, such as
 from the Scala interpreter, then it may not work. I think I may just say that when running
 from the interpreter, say with run(), you may get interleaved output. This would only happen
 when doing a multi-threaded test that starts threads that calls informer methods, likely a
 rare case. Also, in that case I think it is reasonable to say you may get interleaved output
 in the interpreter, so if you don't like that, use the Runner.
*/
private[scalatest] abstract class ThreadAwareness {

  private final val atomic = new AtomicReference[Thread](Thread.currentThread)

  def isConstructingThread: Boolean = {
    val constructingThread = atomic.get
    Thread.currentThread == constructingThread
  }
}

/*
private[scalatest] class ConcurrentMessageSender(fire: ConcurrentMessageFiringFun) extends ThreadAwareness {

/*
  def apply(message: String) {
    requireNonNull(message)
    fire(message, None, isConstructingThread, getLineInFile(Thread.currentThread.getStackTrace, 2)) // Fire the info provided event using the passed function
  }
*/
  
  def apply(message: String, payload: Option[Any] = None) {
    requireNonNull(message, payload)
    fire(message, payload, isConstructingThread, getLineInFile(Thread.currentThread.getStackTrace, 2))
  }
}
*/

private[scalatest] class ConcurrentInformer(fire: ConcurrentMessageFiringFun) extends ThreadAwareness with Informer {
  def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Provided = {
    requireNonNull(message, payload)
    fire(message, payload, isConstructingThread, Some(LineInFile(pos.lineNumber, pos.fileName)))
    Reported
  }
}

private[scalatest] object ConcurrentInformer {
  def apply(fire: (String, Option[Any], Boolean, Option[Location]) => Provided) = new ConcurrentInformer(fire)
}

private[scalatest] class ConcurrentNotifier(fire: ConcurrentMessageFiringFun) extends ThreadAwareness with Notifier {
  def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Provided = {
    requireNonNull(message, payload)
    fire(message, payload, isConstructingThread, Some(LineInFile(pos.lineNumber, pos.fileName)))
    Reported
  }
}

private[scalatest] object ConcurrentNotifier {
  def apply(fire: (String, Option[Any], Boolean, Option[Location]) => Provided) = new ConcurrentNotifier(fire)
}

private[scalatest] class ConcurrentAlerter(fire: ConcurrentMessageFiringFun) extends ThreadAwareness with Alerter {
  def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Provided = {
    requireNonNull(message, payload)
    fire(message, payload, isConstructingThread, Some(LineInFile(pos.lineNumber, pos.fileName)))
    Reported
  }
}

private[scalatest] object ConcurrentAlerter {
  def apply(fire: (String, Option[Any], Boolean, Option[Location]) => Provided) = new ConcurrentAlerter(fire)
}

private[scalatest] class ConcurrentDocumenter(fire: ConcurrentMessageFiringFun) extends ThreadAwareness with Documenter {
  def apply(text: String)(implicit pos: source.Position): Provided = {
    requireNonNull(text)
    fire(text, None, isConstructingThread, Some(LineInFile(pos.lineNumber, pos.fileName))) // Fire the info provided event using the passed function
    Reported
  }
}

private[scalatest] object ConcurrentDocumenter {
  def apply(fire: (String, Option[Any], Boolean, Option[Location]) => Provided) = new ConcurrentDocumenter(fire)
}

//
// Three params of function are the string message, a boolean indicating this was from the current thread, and
// the last one is an optional boolean that indicates the message is about a pending test, in which case it would
// be printed out in yellow.
//
// This kind of informer is only used during the execution of tests, to delay the printing out of info's fired
// during tests until after the test succeeded, failed, or pending gets sent out.
//
private[scalatest] class MessageRecorder(dispatch: Reporter) extends ThreadAwareness {

  private var messages = List[(String, Option[Any], RecordedMessageEventFun, Option[Location])]()

  // Should only be called by the thread that constructed this
  // ConcurrentInformer, because don't want to worry about synchronization here. Just send stuff from
  // other threads whenever they come in. So only call record after first checking isConstructingThread
  private def record(message: String, payload: Option[Any], eventFun: RecordedMessageEventFun, location: Option[Location]) {
    require(isConstructingThread)
    messages ::= (message, payload, eventFun, location)
  }

  // Returns them in order recorded
  private def recordedMessages: List[(String, Option[Any], RecordedMessageEventFun, Option[Location])] = messages.reverse

  def apply(message: String, payload: Option[Any], eventFun: RecordedMessageEventFun, location: Option[Location]) {
    requireNonNull(message, payload)
    if (isConstructingThread)
      record(message, payload, eventFun, location)
    else
      dispatch(eventFun(message, payload, false, false, false, location)) // Fire the info provided event using the passed function
  }

  def recordedEvents(testWasPending: Boolean, testWasCanceled: Boolean): collection.immutable.IndexedSeq[RecordableEvent] = {
    Vector.empty ++ recordedMessages.map { case (message, payload, eventFun, location) =>
      eventFun(message, payload, true, testWasPending, testWasCanceled, location)
    }
  }
}

private[scalatest] class MessageRecordingInformer(recorder: MessageRecorder, eventFun: RecordedMessageEventFun) extends Informer {
  def apply(message: String, payload: Option[Any])(implicit pos: source.Position): Provided = {
    // SKIP-SCALATESTJS-START
    val stackDepth = 2
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val stackDepth = 4
    recorder.apply(message, payload, eventFun, Some(LineInFile(pos.lineNumber, pos.fileName)))
    Recorded
  }
}

private[scalatest] object MessageRecordingInformer {
  def apply(recorder: MessageRecorder, eventFun: RecordedMessageEventFun) = new MessageRecordingInformer(recorder, eventFun)
}

private[scalatest] class MessageRecordingDocumenter(recorder: MessageRecorder, eventFun: RecordedMessageEventFun) extends Documenter {
  def apply(message: String)(implicit pos: source.Position): Provided = {
    recorder.apply(message, None, eventFun, Some(LineInFile(pos.lineNumber, pos.fileName)))
    Recorded
  }
}

private[scalatest] object MessageRecordingDocumenter {
  def apply(recorder: MessageRecorder, eventFun: RecordedMessageEventFun) = new MessageRecordingDocumenter(recorder, eventFun)
}

private[scalatest] object MessageRecorder {
  // Three params of function are the string message, a boolean indicating this was from the current
  // thread, two booleans that indicate the message is about a pending or canceled
  // test (in which case it would be printed out in yellow) and an optional location.
  type RecordedMessageEventFun = (String, Option[Any], Boolean, Boolean, Boolean, Option[Location]) => RecordableEvent 

  // First two params of function are the string message and a boolean indicating this was from the current thread, 
  // and an optional location.
  type ConcurrentMessageFiringFun = (String, Option[Any], Boolean, Option[Location]) => Provided
}

// For path traits, need a message recording informer that only later gets 
// (theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, theTest: TestLeaf, includeIcon: Boolean. thread: Thread)
private[scalatest] class PathMessageRecordingInformer(eventFun: (String, Option[Any], Boolean, Boolean, Suite, Reporter, Tracker, String, Int, Boolean, Thread) => RecordableEvent) extends ThreadAwareness with Informer {

  import scala.collection.mutable.SynchronizedBuffer
  import scala.collection.mutable.ArrayBuffer
  type Tup = (String, Option[Any], Thread, Boolean)
  private val messages = new ArrayBuffer[Tup] with SynchronizedBuffer[Tup]

  // Should only be called by the thread that constructed this
  // ConcurrentInformer, because don't want to worry about synchronization here. Just send stuff from
  // other threads whenever they come in. So only call record after first checking isConstructingThread
  // So now do have to worry about concurrency
  private def record(message: String, payload: Option[Any]) {
    messages += ((message, payload, Thread.currentThread, isConstructingThread))
  }

  def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Provided = {
    requireNonNull(message, payload)
    record(message, payload) // have to record all because of eager execution of tests in path traits
    Recorded
  }

  def recordedEvents(testWasPending: Boolean, theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, indentation: Int, includeIcon: Boolean): collection.immutable.IndexedSeq[RecordableEvent] = {
    Vector.empty ++ messages.map { case (message, payload, thread, wasConstructingThread) =>
      eventFun(message, payload, wasConstructingThread, testWasPending, theSuite, report, tracker, testName, indentation, includeIcon, thread)
    }
  }
}

private[scalatest] object PathMessageRecordingInformer {
  def apply(eventFun: (String, Option[Any], Boolean, Boolean, Suite, Reporter, Tracker, String, Int, Boolean, Thread) => RecordableEvent) = new PathMessageRecordingInformer(eventFun)
}

private[scalatest] class PathMessageRecordingNotifier(eventFun: (String, Option[Any], Boolean, Boolean, Suite, Reporter, Tracker, String, Int, Boolean, Thread) => NoteProvided) extends ThreadAwareness with Notifier {

  import scala.collection.mutable.SynchronizedBuffer
  import scala.collection.mutable.ArrayBuffer
  type Tup = (String, Option[Any], Thread, Boolean)
  private val messages = new ArrayBuffer[Tup] with SynchronizedBuffer[Tup]

  // Should only be called by the thread that constructed this
  // ConcurrentNotifier, because don't want to worry about synchronization here. Just send stuff from
  // other threads whenever they come in. So only call record after first checking isConstructingThread
  // So now do have to worry about concurrency
  private def record(message: String, payload: Option[Any]): Unit = {
    messages += ((message, payload, Thread.currentThread, isConstructingThread))
  }

  def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Provided = {
    requireNonNull(message, payload)
    record(message, payload) // have to record all because of eager execution of tests in path traits
    Recorded
  }

  def recordedEvents(testWasPending: Boolean, theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, indentation: Int, includeIcon: Boolean): collection.immutable.IndexedSeq[NotificationEvent] = {
    Vector.empty ++ messages.map { case (message, payload, thread, wasConstructingThread) =>
      eventFun(message, payload, wasConstructingThread, testWasPending, theSuite, report, tracker, testName, indentation, includeIcon, thread)
    }
  }
}

private[scalatest] object PathMessageRecordingNotifier {
  def apply(eventFun: (String, Option[Any], Boolean, Boolean, Suite, Reporter, Tracker, String, Int, Boolean, Thread) => NoteProvided) = new PathMessageRecordingNotifier(eventFun)
}

private[scalatest] class PathMessageRecordingAlerter(eventFun: (String, Option[Any], Boolean, Boolean, Suite, Reporter, Tracker, String, Int, Boolean, Thread) => AlertProvided) extends ThreadAwareness with Alerter {

  import scala.collection.mutable.SynchronizedBuffer
  import scala.collection.mutable.ArrayBuffer
  type Tup = (String, Option[Any], Thread, Boolean)
  private val messages = new ArrayBuffer[Tup] with SynchronizedBuffer[Tup]

  // Should only be called by the thread that constructed this
  // ConcurrentAlerter, because don't want to worry about synchronization here. Just send stuff from
  // other threads whenever they come in. So only call record after first checking isConstructingThread
  // So now do have to worry about concurrency
  private def record(message: String, payload: Option[Any]): Unit = {
    messages += ((message, payload, Thread.currentThread, isConstructingThread))
  }

  def apply(message: String, payload: Option[Any] = None)(implicit pos: source.Position): Provided = {
    requireNonNull(message, payload)
    record(message, payload) // have to record all because of eager execution of tests in path traits
    Recorded
  }

  def recordedEvents(testWasPending: Boolean, theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, indentation: Int, includeIcon: Boolean): collection.immutable.IndexedSeq[NotificationEvent] = {
    Vector.empty ++ messages.map { case (message, payload, thread, wasConstructingThread) =>
      eventFun(message, payload, wasConstructingThread, testWasPending, theSuite, report, tracker, testName, indentation, includeIcon, thread)
    }
  }
}

private[scalatest] object PathMessageRecordingAlerter {
  def apply(eventFun: (String, Option[Any], Boolean, Boolean, Suite, Reporter, Tracker, String, Int, Boolean, Thread) => AlertProvided) = new PathMessageRecordingAlerter(eventFun)
}

private[scalatest] class PathMessageRecordingDocumenter(eventFun: (String, Boolean, Boolean, Suite, Reporter, Tracker, String, Int, Boolean, Thread) => RecordableEvent) extends ThreadAwareness with Documenter {

  import scala.collection.mutable.SynchronizedBuffer
  import scala.collection.mutable.ArrayBuffer
  type Tup = (String, Thread, Boolean)
  private val messages = new ArrayBuffer[Tup] with SynchronizedBuffer[Tup]

  // Should only be called by the thread that constructed this
  // ConcurrentDocumenter, because don't want to worry about synchronization here. Just send stuff from
  // other threads whenever they come in. So only call record after first checking isConstructingThread
  // So now do have to worry about concurrency
  private def record(message: String): Unit = {
    messages += ((message, Thread.currentThread, isConstructingThread))
  }

  def apply(message: String)(implicit pos: source.Position): Provided = {
    requireNonNull(message)
    record(message) // have to record all because of eager execution of tests in path traits
    Recorded
  }

  def recordedEvents(testWasPending: Boolean, theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, indentation: Int, includeIcon: Boolean): collection.immutable.IndexedSeq[RecordableEvent] = {
    Vector.empty ++ messages.map { case (message, thread, wasConstructingThread) =>
      eventFun(message, wasConstructingThread, testWasPending, theSuite, report, tracker, testName, indentation, includeIcon, thread)
    }
  }
}

private[scalatest] object PathMessageRecordingDocumenter {
  def apply(eventFun: (String, Boolean, Boolean, Suite, Reporter, Tracker, String, Int, Boolean, Thread) => RecordableEvent) = new PathMessageRecordingDocumenter(eventFun)
}
