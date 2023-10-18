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

import org.scalatest.events._
import java.io.File
import org.scalatest.exceptions.StackDepthException
import scala.annotation.tailrec
import scala.collection.GenMap
import scala.collection.Iterable
import scala.collection.SortedMap
import scala.collection.SortedSet
import FailureMessages.decorateToStringValue
import org.scalactic.Prettifier
import org.scalactic.ArrayHelper.deep

object SharedHelpers extends Assertions with LineNumberHelper {

  object SilentReporter extends Reporter {
    def apply(event: Event): Unit = ()
  }

  object NoisyReporter extends Reporter {
    def apply(event: Event): Unit = { println(event) }
  }

  class TestDurationReporter extends Reporter {
    var testSucceededWasFiredAndHadADuration = false
    var testFailedWasFiredAndHadADuration = false
    override def apply(event: Event): Unit = {
      event match {
        case event: TestSucceeded => testSucceededWasFiredAndHadADuration = event.duration.isDefined
        case event: TestFailed => testFailedWasFiredAndHadADuration = event.duration.isDefined
        case _ =>
      }
    }
  }

  class SuiteDurationReporter extends Reporter {
    var suiteCompletedWasFiredAndHadADuration = false
    var suiteAbortedWasFiredAndHadADuration = false
    override def apply(event: Event): Unit = {
      event match {
        case event: SuiteCompleted => suiteCompletedWasFiredAndHadADuration = event.duration.isDefined
        case event: SuiteAborted => suiteAbortedWasFiredAndHadADuration = event.duration.isDefined
        case _ =>
      }
    }
  }

  class PendingReporter extends Reporter {
    var testPendingWasFired = false
    override def apply(event: Event): Unit = {
      event match {
        case _: TestPending => testPendingWasFired = true
        case _ =>
      }
    }
  }

  // This now needs to be thread safe, because I'm setting it in one thread
  // and asserting using it from a different thread in Async tests.
  class EventRecordingReporter extends Reporter {
    private var eventList: List[Event] = List()
    def eventsReceived = synchronized { eventList.reverse }
    def testSucceededEventsReceived: List[TestSucceeded] = {
      synchronized {
        eventsReceived filter {
          case event: TestSucceeded => true
          case _ => false
        } map {
          case event: TestSucceeded => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def testStartingEventsReceived: List[TestStarting] = {
      synchronized {
        eventsReceived filter {
          case event: TestStarting => true
          case _ => false
        } map {
          case event: TestStarting => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    // Why doesn't this work:
    // for (event: TestSucceeded <- eventsReceived) yield event
    def infoProvidedEventsReceived: List[InfoProvided] = {
      synchronized {
        eventsReceived filter {
          case event: InfoProvided => true
          case _ => false
        } map {
          case event: InfoProvided => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def noteProvidedEventsReceived: List[NoteProvided] = {
      synchronized {
        eventsReceived filter {
          case event: NoteProvided => true
          case _ => false
        } map {
          case event: NoteProvided => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def alertProvidedEventsReceived: List[AlertProvided] = {
      synchronized {
        eventsReceived filter {
          case event: AlertProvided => true
          case _ => false
        } map {
          case event: AlertProvided => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def markupProvidedEventsReceived: List[MarkupProvided] = {
      synchronized {
        eventsReceived filter {
          case event: MarkupProvided => true
          case _ => false
        } map {
          case event: MarkupProvided => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def scopeOpenedEventsReceived: List[ScopeOpened] = {
      synchronized {
        eventsReceived filter {
          case event: ScopeOpened => true
          case _ => false
        } map {
          case event: ScopeOpened => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def scopeClosedEventsReceived: List[ScopeClosed] = {
      synchronized {
        eventsReceived filter {
          case event: ScopeClosed => true
          case _ => false
        } map {
          case event: ScopeClosed => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def scopePendingEventsReceived: List[ScopePending] = {
      synchronized {
        eventsReceived filter {
          case event: ScopePending => true
          case _ => false
        } map {
          case event: ScopePending => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def testPendingEventsReceived: List[TestPending] = {
      synchronized {
        eventsReceived filter {
          case event: TestPending => true
          case _ => false
        } map {
          case event: TestPending => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def testCanceledEventsReceived: List[TestCanceled] = {
      synchronized {
        eventsReceived filter {
          case event: TestCanceled => true
          case _ => false
        } map {
          case event: TestCanceled => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def testFailedEventsReceived: List[TestFailed] = {
      synchronized {
        eventsReceived filter {
          case event: TestFailed => true
          case _ => false
        } map {
          case event: TestFailed => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def testIgnoredEventsReceived: List[TestIgnored] = {
      synchronized {
        eventsReceived filter {
          case event: TestIgnored => true
          case _ => false
        } map {
          case event: TestIgnored => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def suiteStartingEventsReceived: List[SuiteStarting] = {
      synchronized {
        eventsReceived filter {
          case event: SuiteStarting => true
          case _ => false
        } map {
          case event: SuiteStarting => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def suiteCompletedEventsReceived: List[SuiteCompleted] = {
      synchronized {
        eventsReceived filter {
          case event: SuiteCompleted => true
          case _ => false
        } map {
          case event: SuiteCompleted => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def suiteAbortedEventsReceived: List[SuiteAborted] = {
      synchronized {
        eventsReceived filter {
          case event: SuiteAborted => true
          case _ => false
        } map {
          case event: SuiteAborted => event
          case _ => throw new RuntimeException("should never happen")
        }
      }
    }
    def apply(event: Event): Unit = {
      synchronized {
        eventList ::= event
      }
    }
  }

  def getIndexesForTestInformerEventOrderTests(suite: Suite, testName: String, infoMsg: String): (Int, Int) = {
    val myRep = new EventRecordingReporter
    suite.run(None, Args(myRep))

    val indexedList = myRep.eventsReceived.zipWithIndex

    val testStartingOption = indexedList.find(_._1.isInstanceOf[TestStarting])
    val testSucceededOption = indexedList.find(_._1.isInstanceOf[TestSucceeded])

    assert(testStartingOption.isDefined, "TestStarting for Suite='" + suite.suiteId + "', testName='" + testName + "' not defined.")
    assert(testSucceededOption.isDefined, "TestSucceeded for Suite='" + suite.suiteId + "', testName='" + testName + "' not defined.")

    val testStartingIndex = testStartingOption.get._2
    val testSucceededIndex = testSucceededOption.get._2

    val testStarting = testStartingOption.get._1.asInstanceOf[TestStarting]
    val testSucceeded = testSucceededOption.get._1.asInstanceOf[TestSucceeded]

    val recordedEvents = testSucceeded.recordedEvents

    val infoProvidedOption = recordedEvents.find {
      case event: InfoProvided => event.message == infoMsg
      case _ => false
    }
    assert(infoProvidedOption.isDefined, "InfoProvided for Suite='" + suite.suiteId + "', testName='" + testName + "' not defined.")

    (testStartingIndex, testSucceededIndex)
  }

  def getIndexesForInformerEventOrderTests(suite: Suite, testName: String, infoMsg: String): (Int, Int, Int) = {

    val myRep = new EventRecordingReporter
    suite.run(None, Args(myRep))

    val indexedList = myRep.eventsReceived.zipWithIndex

    val testStartingOption = indexedList.find(_._1.isInstanceOf[TestStarting])
    val infoProvidedOption = indexedList.find {
      case (event: InfoProvided, index) => event.message == infoMsg
      case _ => false
    }
    val testSucceededOption = indexedList.find(_._1.isInstanceOf[TestSucceeded])

    assert(testStartingOption.isDefined, "TestStarting for Suite='" + suite.suiteId + "', testName='" + testName + "' not defined.")
    assert(infoProvidedOption.isDefined, "InfoProvided for Suite='" + suite.suiteId + "', testName='" + testName + "' not defined.")
    assert(testSucceededOption.isDefined, "TestSucceeded for Suite='" + suite.suiteId + "', testName='" + testName + "' not defined.")

    val testStartingIndex = testStartingOption.get._2
    val infoProvidedIndex = infoProvidedOption.get._2
    val testSucceededIndex = testSucceededOption.get._2

    val testStarting = testStartingOption.get._1.asInstanceOf[TestStarting]
    val infoProvided = infoProvidedOption.get._1.asInstanceOf[InfoProvided]
    val testSucceeded = testSucceededOption.get._1.asInstanceOf[TestSucceeded]

    assert(testStarting.testName === testName, "TestStarting.testName expected to be '" + testName + "', but got '" + testStarting.testName + "'.")
    assert(infoProvided.message === infoMsg, "InfoProvide.message expected to be '" + infoMsg + "', but got '" + infoProvided.message + "'.")
    assert(testSucceeded.testName === testName, "TestSucceeded.testName expected to be '" + testName + "', but got '" + testSucceeded.testName + "'.")

    (infoProvidedIndex, testStartingIndex, testSucceededIndex)
  }

  def getIndentedTextFromInfoProvided(suite: Suite): IndentedText = {

    val myRep = new EventRecordingReporter
    suite.run(None, Args(myRep))

    val infoProvidedOption = myRep.eventsReceived.find(_.isInstanceOf[InfoProvided])

    infoProvidedOption match {
      case Some(infoProvided: InfoProvided) =>
        infoProvided.formatter match {
          case Some(indentedText: IndentedText) => indentedText
          case _ => fail("An InfoProvided was received that didn't include an IndentedText formatter: " + infoProvided.formatter)
        }
      case _ => fail("No InfoProvided was received by the Reporter during the run.")
    }
  }

  def getIndentedTextFromTestInfoProvided(suite: Suite): IndentedText = {
    val myRep = new EventRecordingReporter
    suite.run(None, Args(myRep))
    val recordedEvents: Seq[Event] = myRep.eventsReceived.find { e =>
      e match {
        case testSucceeded: TestSucceeded =>
          true
        case testFailed: TestFailed =>
          true
        case testPending: TestPending =>
          true
        case testCanceled: TestCanceled =>
          true
        case _ =>
          false
      }
    } match {
      case Some(testCompleted) =>
        testCompleted match {
          case testSucceeded: TestSucceeded =>
            testSucceeded.recordedEvents
          case testFailed: TestFailed =>
            testFailed.recordedEvents
          case testPending: TestPending =>
            testPending.recordedEvents
          case testCanceled: TestCanceled =>
            testCanceled.recordedEvents
          case _ => throw new RuntimeException("should never get here")
        }
      case None =>
        fail("Test completed event is expected but not found.")
    }
    assert(recordedEvents.size === 1)
    recordedEvents(0) match {
      case ip: InfoProvided =>
        ip.formatter match {
          case Some(indentedText: IndentedText) => indentedText
          case _ => fail("An InfoProvided was received that didn't include an IndentedText formatter: " + ip.formatter)
        }
      case _ => fail("No InfoProvided was received by the Reporter during the run.")
    }
  }

  def ensureTestFailedEventReceived(suite: Suite, testName: String): Unit = {
    val reporter = new EventRecordingReporter
    suite.run(None, Args(reporter))
    val testFailedEvent = reporter.eventsReceived.find(_.isInstanceOf[TestFailed])
    assert(testFailedEvent.isDefined)
    assert(testFailedEvent.get.asInstanceOf[TestFailed].testName === testName)
  }

  def ensureTestFailedEventReceivedWithCorrectMessage(suite: Suite, testName: String, expectedMessage: String): Unit = {
    val reporter = new EventRecordingReporter
    suite.run(None, Args(reporter))
    val testFailedEvent = reporter.eventsReceived.find(_.isInstanceOf[TestFailed])
    assert(testFailedEvent.isDefined)
    assert(testFailedEvent.get.asInstanceOf[TestFailed].testName == testName)
    assert(testFailedEvent.get.asInstanceOf[TestFailed].message == expectedMessage)
  }

  class TestIgnoredTrackingReporter extends Reporter {
    var testIgnoredReceived = false
    var lastEvent: Option[TestIgnored] = None
    def apply(event: Event): Unit = {
      event match {
        case event: TestIgnored =>
          testIgnoredReceived = true
          lastEvent = Some(event)
        case _ =>
      }
    }
  }

  def getIndex[T](xs: Iterable[T], value: T): Int = {
    @tailrec
    def getIndexAcc[T](itr: Iterator[T], count: Int): Int = {
      if (itr.hasNext) {
        val next = itr.next
        if (next == value)
          count
        else
          getIndexAcc(itr, count + 1)
      }
      else
        -1
    }
    getIndexAcc(xs.toIterator, 0)
  }

  def getKeyIndex[K, V](xs: GenMap[K, V], value: K): Int = {
    @tailrec
    def getIndexAcc[K, V](itr: Iterator[(K, V)], count: Int): Int = {
      if (itr.hasNext) {
        val next = itr.next
        if (next._1 == value)
          count
        else
          getIndexAcc(itr, count + 1)
      }
      else
        -1
    }
    getIndexAcc(xs.toIterator, 0)
  }

  def getIndex(xs: java.util.Collection[_], value: Any): Int = {
    @tailrec
    def getIndexAcc(itr: java.util.Iterator[_], count: Int): Int = {
      if (itr.hasNext) {
        val next = itr.next
        if (next == value)
          count
        else
          getIndexAcc(itr, count + 1)
      }
      else
        -1
    }
    getIndexAcc(xs.iterator, 0)
  }

  def getIndex[K, V](xs: java.util.Map[K, V], value: java.util.Map.Entry[K, V]): Int = {
    @tailrec
    def getIndexAcc(itr: java.util.Iterator[java.util.Map.Entry[K, V]], count: Int): Int = {
      if (itr.hasNext) {
        val next = itr.next
        if (next == value)
          count
        else
          getIndexAcc(itr, count + 1)
      }
      else
        -1
    }
    getIndexAcc(xs.entrySet.iterator, 0)
  }

  def getKeyIndex[K, V](xs: java.util.Map[K, V], value: K): Int = {
    @tailrec
    def getIndexAcc[K, V](itr: java.util.Iterator[java.util.Map.Entry[K, V]], count: Int): Int = {
      if (itr.hasNext) {
        val next = itr.next
        if (next.getKey == value)
          count
        else
          getIndexAcc(itr, count + 1)
      }
      else
        -1
    }
    getIndexAcc(xs.entrySet.iterator, 0)
  }

  def getIndexes[T](xs: Iterable[T], values: Iterable[T]): Iterable[Int] = {
    @tailrec
    def getIndexesAcc[T](itr: Iterator[T], indexes: IndexedSeq[Int], count: Int): IndexedSeq[Int] = {
      if (itr.hasNext) {
        val next = itr.next
        if (values.exists(_ == next))
          getIndexesAcc(itr, indexes :+ count, count + 1)
        else
          getIndexesAcc(itr, indexes, count + 1)
      }
      else
        indexes
    }
    val itr = xs.toIterator
    getIndexesAcc(itr, IndexedSeq.empty, 0)
  }

  def getIndexesInJavaCol[T](xs: java.util.Collection[T], values: java.util.Collection[T]): Iterable[Int] = {
    import collection.JavaConverters._
    val javaValues = values.asScala
    @tailrec
    def getIndexesAcc[T](itr: java.util.Iterator[T], indexes: IndexedSeq[Int], count: Int): IndexedSeq[Int] = {
      if (itr.hasNext) {
        val next = itr.next
        if (javaValues.exists(_ == next))
          getIndexesAcc(itr, indexes :+ count, count + 1)
        else
          getIndexesAcc(itr, indexes, count + 1)
      }
      else
        indexes
    }
    val itr = xs.iterator
    getIndexesAcc(itr, IndexedSeq.empty, 0)
  }

  @tailrec
  final def getNext[T](itr: Iterator[T], predicate: T => Boolean): T = {
    val next = itr.next
    if (predicate(next))
      next
    else
      getNext(itr, predicate)
  }

  final def getNextInString(itr: Iterator[Char], predicate: Char => Boolean) =
    getNext[Char](itr, predicate)

  @tailrec
  final def getNextInJavaIterator[T](itr: java.util.Iterator[T], predicate: T => Boolean): T = {
    val next = itr.next
    if (predicate(next))
      next
    else
      getNextInJavaIterator(itr, predicate)
  }

  //final def getNextInJavaMap[K, V](map: java.util.Map[K, V], predicate: java.util.Map.Entry[K, V] => Boolean): java.util.Map.Entry[K, V] =
    //getNextInJavaIterator(map.entrySet.iterator, predicate)

  final def getNextInJavaMap[K, V](itr: java.util.Iterator[java.util.Map.Entry[K, V]], predicate: java.util.Map.Entry[K, V] => Boolean): java.util.Map.Entry[K, V] =
    getNextInJavaIterator(itr, predicate)

  def getFirst[T](col: Iterable[T], predicate: T => Boolean): T =
    getNext(col.toIterator, predicate)

  def getFirstInJavaCol[T](col: java.util.Collection[T], predicate: T => Boolean): T =
    getNextInJavaIterator(col.iterator, predicate)

  def getFirstInJavaMap[K, V](map: java.util.Map[K, V], predicate: java.util.Map.Entry[K, V] => Boolean): java.util.Map.Entry[K, V] =
    getNextInJavaIterator(map.entrySet.iterator, predicate)

  def getFirstInString(str: String, predicate: Char => Boolean): Char =
    getNext(str.toCharArray.iterator, predicate)

  @tailrec
  final def getNextNot[T](itr: Iterator[T], predicate: T => Boolean): T = {
    val next = itr.next
    if (!predicate(next))
      next
    else
      getNextNot(itr, predicate)
  }

  @tailrec
  final def getNextNotInJavaCol[T](itr: java.util.Iterator[T], predicate: T => Boolean): T = {
    val next = itr.next
    if (!predicate(next))
      next
    else
      getNextNotInJavaCol(itr, predicate)
  }

  def getFirstNot[T](col: Iterable[T], predicate: T => Boolean): T =
    getNextNot(col.toIterator, predicate)

  def getFirstEqual[T](col: Iterable[T], right: T): T =
    getFirst[T](col, _ == right)

  def getFirstNotEqual[T](col: Iterable[T], right: T): T =
    getFirst[T](col, _ != right)

  def getFirstEqual[K, V](col: java.util.Map[K, V], right: java.util.Map.Entry[K, V]): java.util.Map.Entry[K, V] =
    getFirstInJavaMap[K, V](col, (e: java.util.Map.Entry[K, V]) => e.getKey == right.getKey && e.getValue == right.getValue)

  def getFirstNotEqual[K, V](col: java.util.Map[K, V], right: java.util.Map.Entry[K, V]): java.util.Map.Entry[K, V] =
    getFirstInJavaMap[K, V](col, (e: java.util.Map.Entry[K, V]) =>  e.getKey != right.getKey || e.getValue != right.getValue)

  def getFirstMoreThanEqual(col: Iterable[Int], right: Int): Int =
    getFirst[Int](col, _ >= right)

  def getFirstLessThanEqual(col: Iterable[Int], right: Int): Int =
    getFirst[Int](col, _ <= right)

  def getFirstMoreThan(col: Iterable[Int], right: Int): Int =
    getFirst[Int](col, _ > right)

  def getFirstLessThan(col: Iterable[Int], right: Int): Int =
    getFirst[Int](col, _ < right)

  def getFirstIsEmpty(col: Iterable[String], right: String): String = // right is not used, but to be consistent to other so that easier for code generation
    getFirst[String](col, _.isEmpty)

  def getFirstIsNotEmpty(col: Iterable[String], right: String): String = // right is not used, but to be consistent to other so that easier for code generation
    getFirst[String](col, !_.isEmpty)

  def getFirstLengthEqual(col: Iterable[String], right: Int): String =
    getFirst[String](col, _.length == right)

  def getFirstLengthNotEqual(col: Iterable[String], right: Int): String =
    getFirst[String](col, _.length != right)

  def getFirstLengthNotEqualLength(col: Iterable[String], right: Int): String =
    getFirst[String](col, _.length != right)

  def getFirstSizeEqual(col: Iterable[String], right: Int): String =
    getFirst[String](col, _.size == right)

  def getFirstSizeNotEqual(col: Iterable[String], right: Int): String =
    getFirst[String](col, _.size != right)

  def getFirstRefEqual[T <: AnyRef](col: Iterable[T], right: T): T =
    getFirst[T](col, _ eq right)

  def getFirstNotRefEqual[T <: AnyRef](col: Iterable[T], right: T): T =
    getFirst[T](col, _ ne right)

  def getFirstStartsWith(col: Iterable[String], right: String): String =
    getFirst[String](col, _.startsWith(right))

  def getFirstNotStartsWith(col: Iterable[String], right: String): String =
    getFirst[String](col, !_.startsWith(right))

  def getFirstEndsWith(col: Iterable[String], right: String): String =
    getFirst[String](col, _.endsWith(right))

  def getFirstNotEndsWith(col: Iterable[String], right: String): String =
    getFirst[String](col, !_.endsWith(right))

  def getFirstInclude(col: Iterable[String], right: String): String =
    getFirst[String](col, _.indexOf(right) >= 0)

  def getFirstNotInclude(col: Iterable[String], right: String): String =
    getFirst[String](col, _.indexOf(right) < 0)

  def getFirstMatches(col: Iterable[String], right: String): String =
    getFirst[String](col, _.matches(right))

  def getFirstNotMatches(col: Iterable[String], right: String): String =
    getFirst[String](col, !_.matches(right))

  def getFirstNot[T](col: java.util.Collection[T], predicate: T => Boolean): T =
    getNextNotInJavaCol(col.iterator, predicate)

  def getFirstEqual[T](col: java.util.Collection[T], right: T): T =
    getFirstInJavaCol[T](col, _ == right)

  def getFirstNotEqual[T](col: java.util.Collection[T], right: T): T =
    getFirstInJavaCol[T](col, _ != right)

  def getFirstMoreThanEqual(col: java.util.Collection[Int], right: Int): Int =
    getFirstInJavaCol[Int](col, _ >= right)

  def getFirstLessThanEqual(col: java.util.Collection[Int], right: Int): Int =
    getFirstInJavaCol[Int](col, _ <= right)

  def getFirstMoreThan(col: java.util.Collection[Int], right: Int): Int =
    getFirstInJavaCol[Int](col, _ > right)

  def getFirstLessThan(col: java.util.Collection[Int], right: Int): Int =
    getFirstInJavaCol[Int](col, _ < right)

  def getFirstIsEmpty(col: java.util.Collection[String], right: String): String = // right is not used, but to be consistent to other so that easier for code generation
    getFirstInJavaCol[String](col, _.isEmpty)

  def getFirstIsNotEmpty(col: java.util.Collection[String], right: String): String = // right is not used, but to be consistent to other so that easier for code generation
    getFirstInJavaCol[String](col, !_.isEmpty)

  def getFirstLengthEqual(col: java.util.Collection[String], right: Int): String =
    getFirstInJavaCol[String](col, _.length == right)

  def getFirstLengthNotEqual(col: java.util.Collection[String], right: Int): String =
    getFirstInJavaCol[String](col, _.length != right)

  def getFirstLengthNotEqualLength(col: java.util.Collection[String], right: Int): String =
    getFirstInJavaCol[String](col, _.length != right)

  def getFirstSizeEqual(col: java.util.Collection[String], right: Int): String =
    getFirstInJavaCol[String](col, _.size == right)

  def getFirstSizeNotEqual(col: java.util.Collection[String], right: Int): String =
    getFirstInJavaCol[String](col, _.size != right)

  def getFirstRefEqual[T <: AnyRef](col: java.util.Collection[T], right: T): T =
    getFirstInJavaCol[T](col, _ eq right)

  def getFirstNotRefEqual[T <: AnyRef](col: java.util.Collection[T], right: T): T =
    getFirstInJavaCol[T](col, _ ne right)

  def getFirstStartsWith(col: java.util.Collection[String], right: String): String =
    getFirstInJavaCol[String](col, _.startsWith(right))

  def getFirstNotStartsWith(col: java.util.Collection[String], right: String): String =
    getFirstInJavaCol[String](col, !_.startsWith(right))

  def getFirstEndsWith(col: java.util.Collection[String], right: String): String =
    getFirstInJavaCol[String](col, _.endsWith(right))

  def getFirstNotEndsWith(col: java.util.Collection[String], right: String): String =
    getFirstInJavaCol[String](col, !_.endsWith(right))

  def getFirstInclude(col: java.util.Collection[String], right: String): String =
    getFirstInJavaCol[String](col, _.indexOf(right) >= 0)

  def getFirstNotInclude(col: java.util.Collection[String], right: String): String =
    getFirstInJavaCol[String](col, _.indexOf(right) < 0)

  def getFirstMatches(col: java.util.Collection[String], right: String): String =
    getFirstInJavaCol[String](col, _.matches(right))

  def getFirstNotMatches(col: java.util.Collection[String], right: String): String =
    getFirstInJavaCol[String](col, !_.matches(right))

  def getFirstSizeEqualIterable[T](col: Iterable[Iterable[T]], right: Int): Iterable[T] =
    getFirst[Iterable[T]](col, _.size == right)

  def getFirstSizeNotEqualIterable[T](col: Iterable[Iterable[T]], right: Int): Iterable[T] =
    getFirst[Iterable[T]](col, _.size != right)

  def getFirstSizeEqualIterableArray[T](col: Iterable[Array[T]], right: Int): Array[T] =
    getFirst[Array[T]](col, _.size == right)

  def getFirstSizeNotEqualIterableArray[T](col: Iterable[Array[T]], right: Int): Array[T] =
    getFirst[Array[T]](col, _.size != right)

  def getFirstIsEmpty[T](col: Iterable[Iterable[T]], right: T): Iterable[T] =
    getFirst[Iterable[T]](col, _.isEmpty)

  def getFirstNotIsEmpty[T](col: Iterable[Iterable[T]], right: T): Iterable[T] =
    getFirst[Iterable[T]](col, !_.isEmpty)

  def getFirstContainIterable[T](col: Iterable[Iterable[T]], right: T): Iterable[T] =
    getFirst[Iterable[T]](col, _.exists(_ == right))

  def getFirstNotContainIterable[T](col: Iterable[Iterable[T]], right: T): Iterable[T] =
    getFirst[Iterable[T]](col, !_.exists(_ == right))

  def getFirstContainIterableArray[T](col: Iterable[Array[T]], right: T): Array[T] =
    getFirst[Array[T]](col, _.exists(_ == right))

  def getFirstNotContainIterableArray[T](col: Iterable[Array[T]], right: T): Array[T] =
    getFirst[Array[T]](col, !_.exists(_ == right))

  def getFirstContainKey[K, V](col: Iterable[GenMap[K, V]], right: K): GenMap[K, V] =
    getFirst[GenMap[K, V]](col, _.exists(_._1 == right))

  def getFirstNotContainKey[K, V](col: Iterable[GenMap[K, V]], right: K): GenMap[K, V] =
    getFirst[GenMap[K, V]](col, !_.exists(_._1 == right))

  def getFirstContainValue[K, V](col: Iterable[GenMap[K, V]], right: V): GenMap[K, V] =
    getFirst[GenMap[K, V]](col, _.exists(_._2 == right))

  def getFirstNotContainValue[K, V](col: Iterable[GenMap[K, V]], right: V): GenMap[K, V] =
    getFirst[GenMap[K, V]](col, !_.exists(_._2 == right))

  import scala.language.higherKinds

  def getFirstJavaMapIsEmpty[K, V, JMAP[k, v] <: java.util.Map[_, _]](col: java.util.Collection[JMAP[K, V]], right: Int = 0): java.util.Map[K, V] = // right is not used, but to be consistent to other so that easier for code generation
    getFirstInJavaCol[java.util.Map[K, V]](col.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.isEmpty)

  def getFirstJavaMapNotIsEmpty[K, V, JMAP[k, v] <: java.util.Map[_, _]](col: java.util.Collection[JMAP[K, V]], right: Int = 0): java.util.Map[K, V] = // right is not used, but to be consistent to other so that easier for code generation
    getFirstInJavaCol[java.util.Map[K, V]](col.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], !_.isEmpty)

  def getFirstJavaMapContainKey[K, V, JMAP[k, v] <: java.util.Map[_, _]](col: java.util.Collection[JMAP[K, V]], right: K): java.util.Map[K, V] =
    getFirstInJavaCol[java.util.Map[K, V]](col.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.containsKey(right))

  def getFirstJavaMapNotContainKey[K, V, JMAP[k, v] <: java.util.Map[_, _]](col: java.util.Collection[JMAP[K, V]], right: K): java.util.Map[K, V] =
    getFirstInJavaCol[java.util.Map[K, V]](col.asInstanceOf[java.util.Collection[java.util.Map[K, V]]],  !_.containsKey(right))

  def getFirstJavaMapContainValue[K, V, JMAP[k, v] <: java.util.Map[_, _]](col: java.util.Collection[JMAP[K, V]], right: V): java.util.Map[K, V] =
    getFirstInJavaCol[java.util.Map[K, V]](col.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.containsValue(right))

  def getFirstJavaMapNotContainValue[K, V, JMAP[k, v] <: java.util.Map[_, _]](col: java.util.Collection[JMAP[K, V]], right: V): java.util.Map[K, V] =
    getFirstInJavaCol[java.util.Map[K, V]](col.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], !_.containsValue(right))

  def getFirstJavaMapSizeEqual[K, V, JMAP[k, v] <: java.util.Map[_, _]](col: java.util.Collection[JMAP[K, V]], right: Int): java.util.Map[K, V] =
    getFirstInJavaCol[java.util.Map[K, V]](col.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.size == right)

  def getFirstJavaMapSizeNotEqual[K, V, JMAP[k, v] <: java.util.Map[_, _]](col: java.util.Collection[JMAP[K, V]], right: Int): java.util.Map[K, V] =
    getFirstInJavaCol[java.util.Map[K, V]](col.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.size != right)

  def getFirstJavaColSizeEqual[T, C[t] <: java.util.Collection[_]](col: java.util.Collection[C[T]], right: Int): java.util.Collection[T] =
    getFirstInJavaCol[java.util.Collection[T]](col.asInstanceOf[java.util.Collection[java.util.Collection[T]]], _.size == right) // Safe cast, but ugly, can we do without it?

  def getFirstJavaColSizeNotEqual[T, C[t] <: java.util.Collection[_]](col: java.util.Collection[C[T]], right: Int): java.util.Collection[T] =
    getFirstInJavaCol[java.util.Collection[T]](col.asInstanceOf[java.util.Collection[java.util.Collection[T]]], _.size != right) // Safe cast, but ugly, can we do without it?

  def getFirstJavaColContain[T, C[t] <: java.util.Collection[_]](col: java.util.Collection[C[T]], right: T): java.util.Collection[T] =
    getFirstInJavaCol[java.util.Collection[T]](col.asInstanceOf[java.util.Collection[java.util.Collection[T]]], _.contains(right)) // Safe cast, but ugly, can we do without it?

  def getFirstJavaColNotContain[T, C[t] <: java.util.Collection[_]](col: java.util.Collection[C[T]], right: T): java.util.Collection[T] =
    getFirstInJavaCol[java.util.Collection[T]](col.asInstanceOf[java.util.Collection[java.util.Collection[T]]], !_.contains(right)) // Safe cast, but ugly, can we do without it?

  def getFirstJavaColIsEmpty[T, C[t] <: java.util.Collection[_]](col: java.util.Collection[C[T]], right: Int = 0): java.util.Collection[T] = // right is not used, but to be consistent to other so that easier for code generation
    getFirstInJavaCol[java.util.Collection[T]](col.asInstanceOf[java.util.Collection[java.util.Collection[T]]], _.isEmpty) // Safe cast, but ugly, can we do without it?

  def getFirstJavaColNotIsEmpty[T, C[t] <: java.util.Collection[_]](col: java.util.Collection[C[T]], right: Int = 0): java.util.Collection[T] = // right is not used, but to be consistent to other so that easier for code generation
    getFirstInJavaCol[java.util.Collection[T]](col.asInstanceOf[java.util.Collection[java.util.Collection[T]]], !_.isEmpty) // Safe cast, but ugly, can we do without it?

  def indexElement[T](itr: Iterator[T], xs: Iterable[T], errorFun: T => Boolean): Array[String] = {
    val element = getNext[T](itr, errorFun)
    val indexOrKey =
      xs match {
        case map: GenMap[_, _] => element.asInstanceOf[Tuple2[_, _]]._1
        case genTrv: Iterable[_] => getIndex(xs, element)
      }
    Array(indexOrKey.toString, decorateToStringValue(Prettifier.default, element))
  }

  def indexElementForJavaIterator[T](itr: java.util.Iterator[T], xs: java.util.Collection[T], errorFun: T => Boolean): Array[String] = {
    val element = getNextInJavaIterator[T](itr, errorFun)
    val indexOrKey =
      xs match {
        case map: java.util.Map[_, _] => element.asInstanceOf[java.util.Map.Entry[_, _]].getKey
        case genTrv: java.util.Collection[_] => getIndex(xs, element)
      }
    Array(indexOrKey.toString, decorateToStringValue(Prettifier.default, element))
  }

  def indexElementForJavaIterator[K, V](itr: java.util.Iterator[java.util.Map.Entry[K, V]], xs: java.util.Map[K, V], errorFun: java.util.Map.Entry[K, V] => Boolean): Array[String] = {
    val element = getNextInJavaIterator[java.util.Map.Entry[K, V]](itr, errorFun)
    val indexOrKey = element.asInstanceOf[java.util.Map.Entry[_, _]].getKey
    Array(indexOrKey.toString, decorateToStringValue(Prettifier.default, element))
  }

  def indexLengthElement[T](itr: Iterator[String], xs: Iterable[String], errorFun: String => Boolean): Array[String] = {
    val element = getNext[String](itr, errorFun)
    val indexOrKey =
      xs match {
        case map: GenMap[_, _] => element.asInstanceOf[Tuple2[_, _]]._1
        case genTrv: Iterable[_] => getIndex(xs, element)
      }
    Array(indexOrKey.toString, element.length.toString, (if (element != null && element.isInstanceOf[Array[_]]) deep(element.asInstanceOf[Array[T]]).toString else element.toString))
  }

  def indexLengthElement[T](itr: java.util.Iterator[String], xs: java.util.Collection[String], errorFun: String => Boolean): Array[String] = {
    val element = getNextInJavaIterator[String](itr, errorFun)
    val indexOrKey =
      xs match {
        case map: java.util.Map[_, _] => element.asInstanceOf[java.util.Map.Entry[_, _]].getKey
        case genTrv: java.util.Collection[_] => getIndex(xs, element)
      }
    Array(indexOrKey.toString, element.length.toString, (if (element != null && element.isInstanceOf[Array[_]]) deep(element.asInstanceOf[Array[T]]).toString else element.toString))
  }

  def indexElementLengthString[T](itr: Iterator[String], xs: Iterable[String], errorFun: String => Boolean): Array[String] = {
    val element = getNext[String](itr, errorFun)
    val indexOrKey =
      xs match {
        case map: GenMap[_, _] => element.asInstanceOf[Tuple2[_, _]]._1
        case genTrv: Iterable[_] => getIndex(xs, element)
      }
    Array(indexOrKey.toString, decorateToStringValue(Prettifier.default, element), element.length.toString)
  }

  def indexElementLengthString[T](itr: java.util.Iterator[String], xs: java.util.Collection[String], errorFun: String => Boolean): Array[String] = {
    val element = getNextInJavaIterator[String](itr, errorFun)
    val indexOrKey =
      xs match {
        case map: java.util.Map[_, _] => element.asInstanceOf[java.util.Map.Entry[_, _]].getKey
        case genTrv: java.util.Collection[_] => getIndex(xs, element)
      }
    Array(indexOrKey.toString, decorateToStringValue(Prettifier.default, element), element.length.toString)
  }

  def indexElementLengthIterable[T](itr: Iterator[Iterable[T]], xs: Iterable[Iterable[T]], errorFun: Iterable[T] => Boolean): Array[String] = {
    val element = getNext[Iterable[T]](itr, errorFun)
    val indexOrKey =
      xs match {
        case map: GenMap[_, _] => element.asInstanceOf[Tuple2[_, _]]._1
        case genTrv: Iterable[_] => getIndex(xs, element)
      }
    Array(indexOrKey.toString, decorateToStringValue(Prettifier.default, element), element.size.toString)
  }

  def indexElementLengthArray[T](itr: Iterator[Array[T]], xs: Iterable[Array[T]], errorFun: Array[T] => Boolean): Array[String] = {
    val element = getNext[Array[T]](itr, errorFun)
    val indexOrKey =
      xs match {
        case map: GenMap[_, _] => element.asInstanceOf[Tuple2[_, _]]._1
        case genTrv: Iterable[_] => getIndex(xs, element)
      }
    Array(indexOrKey.toString, decorateToStringValue(Prettifier.default, element), element.size.toString)
  }

  def indexElementLengthJavaCol[T, C[t] <: java.util.Collection[_]](itr: java.util.Iterator[C[T]], xs: java.util.Collection[C[T]], errorFun: java.util.Collection[T] => Boolean): Array[String] = {
    val element = getNextInJavaIterator[java.util.Collection[T]](itr.asInstanceOf[java.util.Iterator[java.util.Collection[T]]], errorFun)
    val indexOrKey =
      xs match {
        case map: java.util.Map[_, _] => element.asInstanceOf[java.util.Map.Entry[_, _]].getKey
        case genTrv: java.util.Collection[_] => getIndex(xs, element)
      }
    Array(indexOrKey.toString, decorateToStringValue(Prettifier.default, element), element.size.toString)
  }

  def indexElementLengthJavaMap[K, V, JMAP[k, v] <: java.util.Map[_, _]](itr: java.util.Iterator[JMAP[K, V]], xs: java.util.Collection[java.util.Map[K, V]], errorFun: java.util.Map[K, V] => Boolean): Array[String] = {
    val element = getNextInJavaIterator[java.util.Map[K, V]](itr.asInstanceOf[java.util.Iterator[java.util.Map[K, V]]], errorFun)
    val indexOrKey =
      xs match {
        case map: java.util.Map[_, _] => element.asInstanceOf[java.util.Map.Entry[_, _]].getKey
        case genTrv: java.util.Collection[_] => getIndex(xs, element)
      }
    Array(indexOrKey.toString, decorateToStringValue(Prettifier.default, element), element.size.toString)
  }

  def indexElementEqual[T](itr: Iterator[T], xs: Iterable[T], right: T): Array[String] =
    indexElement[T](itr, xs, _ == right)

  def indexElementNotEqual[T](itr: Iterator[T], xs: Iterable[T], right: T): Array[String] =
    indexElement[T](itr, xs, _ != right)

  def indexElementMoreThan(itr: Iterator[Int], xs: Iterable[Int], right: Int): Array[String] =
    indexElement[Int](itr, xs, _ > right)

  def indexElementMoreThanEqual(itr: Iterator[Int], xs: Iterable[Int], right: Int): Array[String] =
    indexElement[Int](itr, xs, _ >= right)

  def indexElementLessThan(itr: Iterator[Int], xs: Iterable[Int], right: Int): Array[String] =
    indexElement[Int](itr, xs, _ < right)

  def indexElementLessThanEqual(itr: Iterator[Int], xs: Iterable[Int], right: Int): Array[String] =
    indexElement[Int](itr, xs, _ <= right)

  def indexElementIsEmpty(itr: Iterator[String], xs: Iterable[String], right: String): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElement[String](itr, xs, _.isEmpty)

  def indexElementIsNotEmpty(itr: Iterator[String], xs: Iterable[String], right: String): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElement[String](itr, xs, !_.isEmpty)

  def indexElementLengthEqual(itr: Iterator[String], xs: Iterable[String], right: Int): Array[String] =
    indexElement[String](itr, xs, _.length == right)

  def indexElementLengthNotEqual(itr: Iterator[String], xs: Iterable[String], right: Int): Array[String] =
    indexElementLengthString[String](itr, xs, (e: String) =>  e.length != right)

  def indexElementSizeEqual(itr: Iterator[String], xs: Iterable[String], right: Int): Array[String] =
    indexElement[String](itr, xs, _.size == right)

  def indexElementSizeNotEqual(itr: Iterator[String], xs: Iterable[String], right: Int): Array[String] =
    indexElementLengthString[String](itr, xs, (e: String) => e.size != right)

  def indexElementLengthNotEqualLength(itr: Iterator[String], xs: Iterable[String], right: Int): Array[String] =
    indexLengthElement[String](itr, xs, (e: String) => e.length != right)

  def indexElementStartsWith(itr: Iterator[String], xs: Iterable[String], right: String): Array[String] =
    indexElement[String](itr, xs, _.startsWith(right))

  def indexElementNotStartsWith(itr: Iterator[String], xs: Iterable[String], right: String): Array[String] =
    indexElement[String](itr, xs, !_.startsWith(right))

  def indexElementEndsWith(itr: Iterator[String], xs: Iterable[String], right: String): Array[String] =
    indexElement[String](itr, xs, _.endsWith(right))

  def indexElementNotEndsWith(itr: Iterator[String], xs: Iterable[String], right: String): Array[String] =
    indexElement[String](itr, xs, !_.endsWith(right))

  def indexElementInclude(itr: Iterator[String], xs: Iterable[String], right: String): Array[String] =
    indexElement[String](itr, xs, _.indexOf(right) >= 0)

  def indexElementNotInclude(itr: Iterator[String], xs: Iterable[String], right: String): Array[String] =
    indexElement[String](itr, xs, _.indexOf(right) < 0)

  def indexElementMatches(itr: Iterator[String], xs: Iterable[String], right: String): Array[String] =
    indexElement[String](itr, xs, _.matches(right))

  def indexElementNotMatches(itr: Iterator[String], xs: Iterable[String], right: String): Array[String] =
    indexElement[String](itr, xs, !_.matches(right))

  //##################################

  // SKIP-SCALATESTJS,NATIVE-START
  def javaMapEntry[K, V](key: K, value: V): java.util.Map.Entry[K, V] = org.scalatest.Entry(key, value)
  // SKIP-SCALATESTJS,NATIVE-END

  def indexElementEqual[K, V](itr: java.util.Iterator[java.util.Map.Entry[K, V]], xs: java.util.Map[K, V], right: java.util.Map.Entry[K, V]): Array[String] =
    indexElementForJavaIterator[K, V](itr, xs, (e: java.util.Map.Entry[K, V]) => e.getKey == right.getKey && e.getValue == right.getValue)

  def indexElementNotEqual[K, V](itr: java.util.Iterator[java.util.Map.Entry[K, V]], xs: java.util.Map[K, V], right: java.util.Map.Entry[K, V]): Array[String] =
    indexElementForJavaIterator[K, V](itr, xs, (e: java.util.Map.Entry[K, V]) => e.getKey != right.getKey || e.getValue != right.getValue)

  def indexElementEqual[T](itr: java.util.Iterator[T], xs: java.util.Collection[T], right: T): Array[String] =
    indexElementForJavaIterator[T](itr, xs, _ == right)

  def indexElementNotEqual[T](itr: java.util.Iterator[T], xs: java.util.Collection[T], right: T): Array[String] =
    indexElementForJavaIterator[T](itr, xs, _ != right)

  def indexElementMoreThan(itr: java.util.Iterator[Int], xs: java.util.Collection[Int], right: Int): Array[String] =
    indexElementForJavaIterator[Int](itr, xs, _ > right)

  def indexElementMoreThanEqual(itr: java.util.Iterator[Int], xs: java.util.Collection[Int], right: Int): Array[String] =
    indexElementForJavaIterator[Int](itr, xs, _ >= right)

  def indexElementLessThan(itr: java.util.Iterator[Int], xs: java.util.Collection[Int], right: Int): Array[String] =
    indexElementForJavaIterator[Int](itr, xs, _ < right)

  def indexElementLessThanEqual(itr: java.util.Iterator[Int], xs: java.util.Collection[Int], right: Int): Array[String] =
    indexElementForJavaIterator[Int](itr, xs, _ <= right)

  def indexElementIsEmpty(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: String): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElementForJavaIterator[String](itr, xs, _.isEmpty)

  def indexElementIsNotEmpty(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: String): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElementForJavaIterator[String](itr, xs, !_.isEmpty)

  def indexElementLengthEqual(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: Int): Array[String] =
    indexElementForJavaIterator[String](itr, xs, _.length == right)

  def indexElementLengthNotEqual(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: Int): Array[String] =
    indexElementLengthString[String](itr, xs, (e: String) => e.length != right)

  def indexElementSizeEqual(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: Int): Array[String] =
    indexElementForJavaIterator[String](itr, xs, _.size == right)

  def indexElementSizeNotEqual(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: Int): Array[String] =
    indexElementLengthString[String](itr, xs, (e: String) => e.size != right)

  def indexElementLengthNotEqualLength(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: Int): Array[String] =
    indexLengthElement[String](itr, xs, (e: String) => e.length != right)

  def indexElementStartsWith(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: String): Array[String] =
    indexElementForJavaIterator[String](itr, xs, _.startsWith(right))

  def indexElementNotStartsWith(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: String): Array[String] =
    indexElementForJavaIterator[String](itr, xs, !_.startsWith(right))

  def indexElementEndsWith(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: String): Array[String] =
    indexElementForJavaIterator[String](itr, xs, _.endsWith(right))

  def indexElementNotEndsWith(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: String): Array[String] =
    indexElementForJavaIterator[String](itr, xs, !_.endsWith(right))

  def indexElementInclude(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: String): Array[String] =
    indexElementForJavaIterator[String](itr, xs, _.indexOf(right) >= 0)

  def indexElementNotInclude(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: String): Array[String] =
    indexElementForJavaIterator[String](itr, xs, _.indexOf(right) < 0)

  def indexElementMatches(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: String): Array[String] =
    indexElementForJavaIterator[String](itr, xs, _.matches(right))

  def indexElementNotMatches(itr: java.util.Iterator[String], xs: java.util.Collection[String], right: String): Array[String] =
    indexElementForJavaIterator[String](itr, xs, !_.matches(right))

  //##################################

  def indexElementSizeEqualIterable[T](itr: Iterator[Iterable[T]], xs: Iterable[Iterable[T]], right: Int): Array[String] =
    indexElement[Iterable[T]](itr, xs, _.size == right)

  def indexElementSizeNotEqualIterable[T](itr: Iterator[Iterable[T]], xs: Iterable[Iterable[T]], right: Int): Array[String] =
    indexElementLengthIterable[T](itr, xs, _.size != right)

  def indexElementSizeEqualIterableArray[T](itr: Iterator[Array[T]], xs: Iterable[Array[T]], right: Int): Array[T] =
    indexElement[Array[T]](itr, xs, _.size == right).asInstanceOf[Array[T]]

  def indexElementSizeNotEqualIterableArray[T](itr: Iterator[Array[T]], xs: Iterable[Array[T]], right: Int): Array[T] =
    indexElementLengthArray[T](itr, xs, _.size != right).asInstanceOf[Array[T]]

  def indexElementContainIterable[T](itr: Iterator[Iterable[T]], xs: Iterable[Iterable[T]], right: T): Array[String] =
    indexElement[Iterable[T]](itr, xs, _.exists(_ == right))

  def indexElementNotContainIterable[T](itr: Iterator[Iterable[T]], xs: Iterable[Iterable[T]], right: T): Array[String] =
    indexElement[Iterable[T]](itr, xs, !_.exists(_ == right))

  def indexElementContainIterableArray[T](itr: Iterator[Array[T]], xs: Iterable[Array[T]], right: T): Array[T] =
    indexElement[Array[T]](itr, xs, _.exists(_ == right)).asInstanceOf[Array[T]]

  def indexElementNotContainIterableArray[T](itr: Iterator[Array[T]], xs: Iterable[Array[T]], right: T): Array[T] =
    indexElement[Array[T]](itr, xs, !_.exists(_ == right)).asInstanceOf[Array[T]]

  def indexElementRefEqual[T <: AnyRef](itr: Iterator[T], xs: Iterable[T], right: T): Array[String] =
    indexElement[T](itr, xs, _ eq right)

  def indexElementNotRefEqual[T <: AnyRef](itr: Iterator[T], xs: Iterable[T], right: T): Array[String] =
    indexElement[T](itr, xs, _ ne right)

  def indexElementRefEqual[T <: AnyRef](itr: java.util.Iterator[T], xs: java.util.Collection[T], right: T): Array[String] =
    indexElementForJavaIterator[T](itr, xs, _ eq right)

  def indexElementNotRefEqual[T <: AnyRef](itr: java.util.Iterator[T], xs: java.util.Collection[T], right: T): Array[String] =
    indexElementForJavaIterator[T](itr, xs, _ ne right)

  def indexElementContainKey[K, V](itr: Iterator[GenMap[K, V]], xs: Iterable[GenMap[K, V]], right: K): Array[String] =
    indexElement[GenMap[K, V]](itr, xs, _.exists(_._1 == right))

  def indexElementNotContainKey[K, V](itr: Iterator[GenMap[K, V]], xs: Iterable[GenMap[K, V]], right: K): Array[String] =
    indexElement[GenMap[K, V]](itr, xs, !_.exists(_._1 == right))

  def indexElementContainValue[K, V](itr: Iterator[GenMap[K, V]], xs: Iterable[GenMap[K, V]], right: V): Array[String] =
    indexElement[GenMap[K, V]](itr, xs, _.exists(_._2 == right))

  def indexElementNotContainValue[K, V](itr: Iterator[GenMap[K, V]], xs: Iterable[GenMap[K, V]], right: V): Array[String] =
    indexElement[GenMap[K, V]](itr, xs, !_.exists(_._2 == right))

  def indexElementJavaMapIsEmpty[K, V, JMAP[k, v] <: java.util.Map[_, _]](itr: java.util.Iterator[JMAP[K, V]], xs: java.util.Collection[JMAP[K, V]], right: Int = 0): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElementForJavaIterator[java.util.Map[K, V]](itr.asInstanceOf[java.util.Iterator[java.util.Map[K, V]]], xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.isEmpty)

  def indexElementJavaMapNotIsEmpty[K, V, JMAP[k, v] <: java.util.Map[_, _]](itr: java.util.Iterator[JMAP[K, V]], xs: java.util.Collection[JMAP[K, V]], right: Int = 0): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElementForJavaIterator[java.util.Map[K, V]](itr.asInstanceOf[java.util.Iterator[java.util.Map[K, V]]], xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], !_.isEmpty)

  def indexElementJavaMapContainKey[K, V, JMAP[k, v] <: java.util.Map[_, _]](itr: java.util.Iterator[JMAP[K, V]], xs: java.util.Collection[JMAP[K, V]], right: K): Array[String] =
    indexElementForJavaIterator[java.util.Map[K, V]](itr.asInstanceOf[java.util.Iterator[java.util.Map[K, V]]], xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.containsKey(right))

  def indexElementJavaMapNotContainKey[K, V, JMAP[k, v] <: java.util.Map[_, _]](itr: java.util.Iterator[JMAP[K, V]], xs: java.util.Collection[JMAP[K, V]], right: K): Array[String] =
    indexElementForJavaIterator[java.util.Map[K, V]](itr.asInstanceOf[java.util.Iterator[java.util.Map[K, V]]], xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]],  !_.containsKey(right))

  def indexElementJavaMapContainValue[K, V, JMAP[k, v] <: java.util.Map[_, _]](itr: java.util.Iterator[JMAP[K, V]], xs: java.util.Collection[JMAP[K, V]], right: V): Array[String] =
    indexElementForJavaIterator[java.util.Map[K, V]](itr.asInstanceOf[java.util.Iterator[java.util.Map[K, V]]], xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.containsValue(right))

  def indexElementJavaMapNotContainValue[K, V, JMAP[k, v] <: java.util.Map[_, _]](itr: java.util.Iterator[JMAP[K, V]], xs: java.util.Collection[JMAP[K, V]], right: V): Array[String] =
    indexElementForJavaIterator[java.util.Map[K, V]](itr.asInstanceOf[java.util.Iterator[java.util.Map[K, V]]], xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], !_.containsValue(right))

  def indexElementJavaMapSizeEqual[K, V, JMAP[k, v] <: java.util.Map[_, _]](itr: java.util.Iterator[JMAP[K, V]], xs: java.util.Collection[JMAP[K, V]], right: Int): Array[String] =
    indexElementForJavaIterator[java.util.Map[K, V]](itr.asInstanceOf[java.util.Iterator[java.util.Map[K, V]]], xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.size == right)

  def indexElementJavaMapSizeNotEqual[K, V, JMAP[k, v] <: java.util.Map[_, _]](itr: java.util.Iterator[JMAP[K, V]], xs: java.util.Collection[JMAP[K, V]], right: Int): Array[String] =
    indexElementLengthJavaMap[K, V, java.util.Map](itr.asInstanceOf[java.util.Iterator[java.util.Map[K, V]]], xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.size != right)

  def indexElementJavaColSizeEqual[T, C[t] <: java.util.Collection[_]](itr: java.util.Iterator[C[T]], xs: java.util.Collection[C[T]], right: Int): Array[String] =
    indexElementForJavaIterator[java.util.Collection[T]](itr.asInstanceOf[java.util.Iterator[java.util.Collection[T]]], xs.asInstanceOf[java.util.Collection[java.util.Collection[T]]], _.size == right)

  def indexElementJavaColSizeNotEqual[T, C[t] <: java.util.Collection[_]](itr: java.util.Iterator[C[T]], xs: java.util.Collection[C[T]], right: Int): Array[String] =
    indexElementLengthJavaCol[T, java.util.Collection](itr.asInstanceOf[java.util.Iterator[java.util.Collection[T]]], xs.asInstanceOf[java.util.Collection[java.util.Collection[T]]], _.size != right)

  def indexElementJavaColContain[T, C[t] <: java.util.Collection[_]](itr: java.util.Iterator[C[T]], xs: java.util.Collection[C[T]], right: T): Array[String] =
    indexElementForJavaIterator[java.util.Collection[T]](itr.asInstanceOf[java.util.Iterator[java.util.Collection[T]]], xs.asInstanceOf[java.util.Collection[java.util.Collection[T]]], _.contains(right))

  def indexElementJavaColNotContain[T, C[t] <: java.util.Collection[_]](itr: java.util.Iterator[C[T]], xs: java.util.Collection[C[T]], right: T): Array[String] =
    indexElementForJavaIterator[java.util.Collection[T]](itr.asInstanceOf[java.util.Iterator[java.util.Collection[T]]], xs.asInstanceOf[java.util.Collection[java.util.Collection[T]]], !_.contains(right))

  def indexElementJavaColIsEmpty[T, C[t] <: java.util.Collection[_]](itr: java.util.Iterator[C[T]], xs: java.util.Collection[C[T]], right: Int = 0): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElementForJavaIterator[java.util.Collection[T]](itr.asInstanceOf[java.util.Iterator[java.util.Collection[T]]], xs.asInstanceOf[java.util.Collection[java.util.Collection[T]]], _.isEmpty)

  def indexElementJavaColNotIsEmpty[T, C[t] <: java.util.Collection[_]](itr: java.util.Iterator[C[T]], xs: java.util.Collection[C[T]], right: Int = 0): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElementForJavaIterator[java.util.Collection[T]](itr.asInstanceOf[java.util.Iterator[java.util.Collection[T]]], xs.asInstanceOf[java.util.Collection[java.util.Collection[T]]], !_.isEmpty)

  private def succeededIndexes[T](xs: Iterable[T], filterFun: T => Boolean): String = {
    xs match {
      case map: GenMap[_, _] =>
        val passedList = map.toList.filter(e => filterFun(e)).map(_._1).toList
        if (passedList.size > 1)
          "key " + passedList.dropRight(1).mkString(", ") + " and " + passedList.last
        else if (passedList.size == 1)
          "key " + passedList.last.toString
        else
          ""
      case _ =>
        val passedList = getIndexes(xs, xs.toList.filter(e => filterFun(e))).toList
        if (passedList.size > 1)
          "index " + passedList.dropRight(1).mkString(", ") + " and " + passedList.last
        else if (passedList.size == 1)
          "index " + passedList.last.toString
        else
          ""
    }
  }

  private def succeededIndexesInJavaCol[T](xs: java.util.Collection[T], filterFun: T => Boolean): String = {
    import collection.JavaConverters._
    val passedList = getIndexes(xs.asScala, xs.asScala.toList.filter(e => filterFun(e))).toList
    if (passedList.size > 1)
      "index " + passedList.dropRight(1).mkString(", ") + " and " + passedList.last
    else if (passedList.size == 1)
      "index " + passedList.last.toString
    else
      ""
  }

  // SKIP-SCALATESTJS,NATIVE-START
  private def succeededIndexesInJavaMap[K, V](xs: java.util.Map[K, V], filterFun: java.util.Map.Entry[K, V] => Boolean): String = {
    import collection.JavaConverters._
    val passedList = xs.asScala.toList.filter(e => filterFun(org.scalatest.Entry(e._1, e._2))).toList.map(_._1)
    if (passedList.size > 1)
      "key " + passedList.dropRight(1).mkString(", ") + " and " + passedList.last
    else if (passedList.size == 1)
      "key " + passedList.last.toString
    else
      ""
  }
  // SKIP-SCALATESTJS,NATIVE-END

  private def failEarlySucceededIndexes[T](xs: Iterable[T], filterFun: T => Boolean, maxSucceed: Int): String = {
    xs match {
      case map: GenMap[_, _] =>
        val passedList = map.toList.filter(e => filterFun(e)).take(maxSucceed).toList.map(_._1)
        if (passedList.size > 1)
          "key " + passedList.dropRight(1).mkString(", ") + " and " + passedList.last
        else if (passedList.size == 1)
          "key " + passedList.last.toString
        else
          ""
      case _ =>
        val passedList = getIndexes(xs, xs.toList.filter(e => filterFun(e))).take(maxSucceed).toList
        if (passedList.size > 1)
          "index " + passedList.dropRight(1).mkString(", ") + " and " + passedList.last
        else if (passedList.size == 1)
          "index " + passedList.last.toString
        else
          ""
    }
  }

  private def failEarlySucceededIndexesInJavaCol[T](xs: java.util.Collection[T], filterFun: T => Boolean, maxSucceed: Int): String = {
    import collection.JavaConverters._
    val passedList = getIndexes(xs.asScala, xs.asScala.toList.filter(e => filterFun(e))).take(maxSucceed).toList
    if (passedList.size > 1)
      "index " + passedList.dropRight(1).mkString(", ") + " and " + passedList.last
    else if (passedList.size == 1)
      "index " + passedList.last.toString
    else
      ""
  }

  // SKIP-SCALATESTJS,NATIVE-START
  private def failEarlySucceededIndexesInJavaMap[K, V](xs: java.util.Map[K, V], filterFun: java.util.Map.Entry[K, V] => Boolean, maxSucceed: Int): String = {
    import collection.JavaConverters._
    val passedList = xs.asScala.toList.filter(e => filterFun(org.scalatest.Entry(e._1, e._2))).take(maxSucceed).toList.map(_._1)
    if (passedList.size > 1)
      "key " + passedList.dropRight(1).mkString(", ") + " and " + passedList.last
    else if (passedList.size == 1)
      "key " + passedList.last.toString
    else
      ""
  }
  // SKIP-SCALATESTJS,NATIVE-END

  def succeededIndexesEqualBoolean[T](xs: Iterable[T], value: Boolean): String =
    succeededIndexes(xs, (e: T) => value)

  def succeededIndexesNotEqualBoolean[T](xs: Iterable[T], value: Boolean): String =
    succeededIndexes(xs, (e: T) => !value)

  def succeededIndexesEqual[T](xs: Iterable[T], value: T): String =
    succeededIndexes(xs, (e: T) => e == value)

  def succeededIndexesNotEqual[T](xs: Iterable[T], value: T): String =
    succeededIndexes(xs, (e: T) => e != value)

  // SKIP-SCALATESTJS,NATIVE-START
  def succeededIndexesEqual[K, V](xs: java.util.Map[K, V], value: java.util.Map.Entry[K, V]): String =
    succeededIndexesInJavaMap(xs, (e: java.util.Map.Entry[K, V]) => e.getKey == value.getKey && e.getValue == value.getValue)

  def succeededIndexesNotEqual[K, V](xs: java.util.Map[K, V], value: java.util.Map.Entry[K, V]): String =
    succeededIndexesInJavaMap(xs, (e: java.util.Map.Entry[K, V]) => e.getKey != value.getKey || e.getValue != value.getValue)
  // SKIP-SCALATESTJS,NATIVE-END

  def succeededIndexesLessThanEqual(xs: Iterable[Int], value: Int): String =
    succeededIndexes(xs, (e: Int) => e <= value)

  def succeededIndexesLessThan(xs: Iterable[Int], value: Int): String =
    succeededIndexes(xs, (e: Int) => e < value)

  def succeededIndexesMoreThanEqual(xs: Iterable[Int], value: Int): String =
    succeededIndexes(xs, (e: Int) => e >= value)

  def succeededIndexesMoreThan(xs: Iterable[Int], value: Int): String =
    succeededIndexes(xs, (e: Int) => e > value)

  def succeededIndexesIsEmpty(xs: Iterable[String], value: String): String =
    succeededIndexes(xs, (e: String) => e.isEmpty)

  def succeededIndexesIsNotEmpty(xs: Iterable[String], value: String): String =
    succeededIndexes(xs, (e: String) => !e.isEmpty)

  def succeededIndexesSizeEqual(xs: Iterable[String], value: Int): String =
    succeededIndexes(xs, (e: String) => e.size == value)

  def succeededIndexesSizeNotEqual(xs: Iterable[String], value: Int): String =
    succeededIndexes(xs, (e: String) => e.size != value)

  def succeededIndexesLengthEqual(xs: Iterable[String], value: Int): String =
    succeededIndexes(xs, (e: String) => e.length == value)

  def succeededIndexesLengthNotEqual(xs: Iterable[String], value: Int): String =
    succeededIndexes(xs, (e: String) => e.length != value)

  def succeededIndexesStartsWith(xs: Iterable[String], value: String): String =
    succeededIndexes(xs, (e: String) => e.startsWith(value))

  def succeededIndexesNotStartsWith(xs: Iterable[String], value: String): String =
    succeededIndexes(xs, (e: String) => !e.startsWith(value))

  def succeededIndexesEndsWith(xs: Iterable[String], value: String): String =
    succeededIndexes(xs, (e: String) => e.endsWith(value))

  def succeededIndexesNotEndsWith(xs: Iterable[String], value: String): String =
    succeededIndexes(xs, (e: String) => !e.endsWith(value))

  def succeededIndexesInclude(xs: Iterable[String], value: String): String =
    succeededIndexes(xs, (e: String) => e.indexOf(value) >= 0)

  def succeededIndexesNotInclude(xs: Iterable[String], value: String): String =
    succeededIndexes(xs, (e: String) => e.indexOf(value) < 0)

  def succeededIndexesSizeEqualIterable[T](xs: Iterable[Iterable[T]], value: Int): String =
    succeededIndexes(xs, (e: Iterable[T]) => e.size == value)

  def succeededIndexesSizeNotEqualIterable[T](xs: Iterable[Iterable[T]], value: Int): String =
    succeededIndexes(xs, (e: Iterable[T]) => e.size != value)

  def succeededIndexesSizeEqualIterableArray[T](xs: Iterable[Array[T]], value: Int): String =
    succeededIndexes(xs, (e: Array[T]) => e.size == value)

  def succeededIndexesSizeNotEqualIterableArray[T](xs: Iterable[Array[T]], value: Int): String =
    succeededIndexes(xs, (e: Array[T]) => e.size != value)

  def succeededIndexesMatches(xs: Iterable[String], value: String): String =
    succeededIndexes(xs, (e: String) => e.matches(value))

  def succeededIndexesNotMatches(xs: Iterable[String], value: String): String =
    succeededIndexes(xs, (e: String) => !e.matches(value))

  def succeededIndexesEqualBoolean[T](xs: java.util.Collection[T], value: Boolean): String =
    succeededIndexesInJavaCol(xs, (e: T) => value)

  def succeededIndexesNotEqualBoolean[T](xs: java.util.Collection[T], value: Boolean): String =
    succeededIndexesInJavaCol(xs, (e: T) => !value)

  def succeededIndexesEqual[T](xs: java.util.Collection[T], value: T): String =
    succeededIndexesInJavaCol(xs, (e: T) => e == value)

  def succeededIndexesNotEqual[T](xs: java.util.Collection[T], value: T): String =
    succeededIndexesInJavaCol(xs, (e: T) => e != value)

  def succeededIndexesLessThanEqual(xs: java.util.Collection[Int], value: Int): String =
    succeededIndexesInJavaCol(xs, (e: Int) => e <= value)

  def succeededIndexesLessThan(xs: java.util.Collection[Int], value: Int): String =
    succeededIndexesInJavaCol(xs, (e: Int) => e < value)

  def succeededIndexesMoreThanEqual(xs: java.util.Collection[Int], value: Int): String =
    succeededIndexesInJavaCol(xs, (e: Int) => e >= value)

  def succeededIndexesMoreThan(xs: java.util.Collection[Int], value: Int): String =
    succeededIndexesInJavaCol(xs, (e: Int) => e > value)

  def succeededIndexesIsEmpty(xs: java.util.Collection[String], value: String): String =
    succeededIndexesInJavaCol(xs, (e: String) => e.isEmpty)

  def succeededIndexesIsNotEmpty(xs: java.util.Collection[String], value: String): String =
    succeededIndexesInJavaCol(xs, (e: String) => !e.isEmpty)

  def succeededIndexesSizeEqual(xs: java.util.Collection[String], value: Int): String =
    succeededIndexesInJavaCol(xs, (e: String) => e.size == value)

  def succeededIndexesSizeNotEqual(xs: java.util.Collection[String], value: Int): String =
    succeededIndexesInJavaCol(xs, (e: String) => e.size != value)

  def succeededIndexesLengthEqual(xs: java.util.Collection[String], value: Int): String =
    succeededIndexesInJavaCol(xs, (e: String) => e.length == value)

  def succeededIndexesLengthNotEqual(xs: java.util.Collection[String], value: Int): String =
    succeededIndexesInJavaCol(xs, (e: String) => e.length != value)

  def succeededIndexesStartsWith(xs: java.util.Collection[String], value: String): String =
    succeededIndexesInJavaCol(xs, (e: String) => e.startsWith(value))

  def succeededIndexesNotStartsWith(xs: java.util.Collection[String], value: String): String =
    succeededIndexesInJavaCol(xs, (e: String) => !e.startsWith(value))

  def succeededIndexesEndsWith(xs: java.util.Collection[String], value: String): String =
    succeededIndexesInJavaCol(xs, (e: String) => e.endsWith(value))

  def succeededIndexesNotEndsWith(xs: java.util.Collection[String], value: String): String =
    succeededIndexesInJavaCol(xs, (e: String) => !e.endsWith(value))

  def succeededIndexesInclude(xs: java.util.Collection[String], value: String): String =
    succeededIndexesInJavaCol(xs, (e: String) => e.indexOf(value) >= 0)

  def succeededIndexesNotInclude(xs: java.util.Collection[String], value: String): String =
    succeededIndexesInJavaCol(xs, (e: String) => e.indexOf(value) < 0)

  def succeededIndexesMatches(xs: java.util.Collection[String], value: String): String =
    succeededIndexesInJavaCol(xs, (e: String) => e.matches(value))

  def succeededIndexesNotMatches(xs: java.util.Collection[String], value: String): String =
    succeededIndexesInJavaCol(xs, (e: String) => !e.matches(value))

  def succeededIndexesContainIterable[T](xs: Iterable[Iterable[T]], right: T): String =
    succeededIndexes[Iterable[T]](xs, _.exists(_ == right))

  def succeededIndexesNotContainIterable[T](xs: Iterable[Iterable[T]], right: T): String =
    succeededIndexes[Iterable[T]](xs, !_.exists(_ == right))

  def succeededIndexesContainIterableArray[T](xs: Iterable[Array[T]], right: T): String =
    succeededIndexes[Array[T]](xs, _.exists(_ == right))

  def succeededIndexesNotContainIterableArray[T](xs: Iterable[Array[T]], right: T): String =
    succeededIndexes[Array[T]](xs, !_.exists(_ == right))

  def succeededIndexesRefEqual[T <: AnyRef](xs: Iterable[T], value: T): String =
    succeededIndexes[T](xs, _ eq value)

  def succeededIndexesNotRefEqual[T <: AnyRef](xs: Iterable[T], value: T): String =
    succeededIndexes[T](xs, _ ne value)

  //#################################

  def succeededIndexesRefEqual[T <: AnyRef](xs: java.util.Collection[T], value: T): String =
    succeededIndexesInJavaCol[T](xs, _ eq value)

  def succeededIndexesNotRefEqual[T <: AnyRef](xs: java.util.Collection[T], value: T): String =
    succeededIndexesInJavaCol[T](xs, _ ne value)

  //#################################

  def succeededIndexesContainKey[K, V](xs: Iterable[GenMap[K, V]], right: K): String =
    succeededIndexes[GenMap[K, V]](xs, _.exists(_._1 == right))

  def succeededIndexesNotContainKey[K, V](xs: Iterable[GenMap[K, V]], right: K): String =
    succeededIndexes[GenMap[K, V]](xs, !_.exists(_._1 == right))

  def succeededIndexesContainValue[K, V](xs: Iterable[GenMap[K, V]], right: V): String =
    succeededIndexes[GenMap[K, V]](xs, _.exists(_._2 == right))

  def succeededIndexesNotContainValue[K, V](xs: Iterable[GenMap[K, V]], right: V): String =
    succeededIndexes[GenMap[K, V]](xs, !_.exists(_._2 == right))

  def succeededIndexesJavaMapIsEmpty[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: Int = 0): String = // right is not used, but to be consistent to other so that easier for code generation
    succeededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.isEmpty)

  def succeededIndexesJavaMapNotIsEmpty[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: Int = 0): String = // right is not used, but to be consistent to other so that easier for code generation
    succeededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], !_.isEmpty)

  def succeededIndexesJavaMapContainKey[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: K): String =
    succeededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.containsKey(right))

  def succeededIndexesJavaMapNotContainKey[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: K): String =
    succeededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]],  !_.containsKey(right))

  def succeededIndexesJavaMapContainValue[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: V): String =
    succeededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.containsValue(right))

  def succeededIndexesJavaMapNotContainValue[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: V): String =
    succeededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], !_.containsValue(right))

  def succeededIndexesJavaMapSizeEqual[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: Int): String =
    succeededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.size == right)

  def succeededIndexesJavaMapSizeNotEqual[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: Int): String =
    succeededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.size != right)

  def succeededIndexesJavaColSizeEqual[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: Int): String =
    succeededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], _.size == right)

  def succeededIndexesJavaColSizeNotEqual[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: Int): String =
    succeededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], _.size != right)

  def succeededIndexesJavaColContain[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: E): String =
    succeededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], _.contains(right))

  def succeededIndexesJavaColNotContain[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: E): String =
    succeededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], !_.contains(right))

  def succeededIndexesJavaColIsEmpty[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: Int = 0): String = // right is not used, but to be consistent to other so that easier for code generation
    succeededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], _.isEmpty)

  def succeededIndexesJavaColNotIsEmpty[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: Int = 0): String = // right is not used, but to be consistent to other so that easier for code generation
    succeededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], !_.isEmpty)

  def failEarlySucceededIndexesEqualBoolean[T](xs: Iterable[T], value: Boolean, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: T) => value, maxSucceed)

  def failEarlySucceededIndexesNotEqualBoolean[T](xs: Iterable[T], value: Boolean, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: T) => !value, maxSucceed)

  def failEarlySucceededIndexesEqual[T](xs: Iterable[T], value: T, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: T) => e == value, maxSucceed)

  def failEarlySucceededIndexesNotEqual[T](xs: Iterable[T], value: T, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: T) => e != value, maxSucceed)

  def failEarlySucceededIndexesLessThanEqual(xs: Iterable[Int], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: Int) => e <= value, maxSucceed)

  def failEarlySucceededIndexesLessThan(xs: Iterable[Int], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: Int) => e < value, maxSucceed)

  def failEarlySucceededIndexesMoreThanEqual(xs: Iterable[Int], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: Int) => e >= value, maxSucceed)

  def failEarlySucceededIndexesMoreThan(xs: Iterable[Int], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: Int) => e > value, maxSucceed)

  def failEarlySucceededIndexesIsEmpty(xs: Iterable[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => e.isEmpty, maxSucceed)

  def failEarlySucceededIndexesIsNotEmpty(xs: Iterable[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => !e.isEmpty, maxSucceed)

  def failEarlySucceededIndexesSizeEqual(xs: Iterable[String], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => e.size == value, maxSucceed)

  def failEarlySucceededIndexesSizeNotEqual(xs: Iterable[String], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => e.size != value, maxSucceed)

  def failEarlySucceededIndexesLengthEqual(xs: Iterable[String], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => e.length == value, maxSucceed)

  def failEarlySucceededIndexesLengthNotEqual(xs: Iterable[String], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => e.length != value, maxSucceed)

  def failEarlySucceededIndexesStartsWith(xs: Iterable[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => e.startsWith(value), maxSucceed)

  def failEarlySucceededIndexesNotStartsWith(xs: Iterable[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => !e.startsWith(value), maxSucceed)

  def failEarlySucceededIndexesEndsWith(xs: Iterable[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => e.endsWith(value), maxSucceed)

  def failEarlySucceededIndexesNotEndsWith(xs: Iterable[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => !e.endsWith(value), maxSucceed)

  def failEarlySucceededIndexesInclude(xs: Iterable[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => e.indexOf(value) >= 0, maxSucceed)

  def failEarlySucceededIndexesNotInclude(xs: Iterable[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => e.indexOf(value) < 0, maxSucceed)

  //################################################

  // SKIP-SCALATESTJS,NATIVE-START
  def failEarlySucceededIndexesEqualBoolean[T](xs: java.util.Collection[T], value: Boolean, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: T) => value, maxSucceed)

  def failEarlySucceededIndexesNotEqualBoolean[T](xs: java.util.Collection[T], value: Boolean, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: T) => !value, maxSucceed)

  def failEarlySucceededIndexesEqual[T](xs: java.util.Collection[T], value: T, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: T) => e == value, maxSucceed)

  def failEarlySucceededIndexesNotEqual[T](xs: java.util.Collection[T], value: T, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: T) => e != value, maxSucceed)

  def failEarlySucceededIndexesEqual[K, V](xs: java.util.Map[K, V], value: java.util.Map.Entry[K, V], maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaMap(xs, (e: java.util.Map.Entry[K, V]) => e.getKey == value.getKey && e.getValue == value.getValue, maxSucceed)

  def failEarlySucceededIndexesNotEqual[K, V](xs: java.util.Map[K, V], value: java.util.Map.Entry[K, V], maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaMap(xs, (e: java.util.Map.Entry[K, V]) => e.getKey != value.getKey || e.getValue != value.getValue, maxSucceed)

  def failEarlySucceededIndexesLessThanEqual(xs: java.util.Collection[Int], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: Int) => e <= value, maxSucceed)

  def failEarlySucceededIndexesLessThan(xs: java.util.Collection[Int], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: Int) => e < value, maxSucceed)

  def failEarlySucceededIndexesMoreThanEqual(xs: java.util.Collection[Int], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: Int) => e >= value, maxSucceed)

  def failEarlySucceededIndexesMoreThan(xs: java.util.Collection[Int], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: Int) => e > value, maxSucceed)

  def failEarlySucceededIndexesIsEmpty(xs: java.util.Collection[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => e.isEmpty, maxSucceed)

  def failEarlySucceededIndexesIsNotEmpty(xs: java.util.Collection[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => !e.isEmpty, maxSucceed)

  def failEarlySucceededIndexesSizeEqual(xs: java.util.Collection[String], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => e.size == value, maxSucceed)

  def failEarlySucceededIndexesSizeNotEqual(xs: java.util.Collection[String], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => e.size != value, maxSucceed)

  def failEarlySucceededIndexesLengthEqual(xs: java.util.Collection[String], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => e.length == value, maxSucceed)

  def failEarlySucceededIndexesLengthNotEqual(xs: java.util.Collection[String], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => e.length != value, maxSucceed)

  def failEarlySucceededIndexesStartsWith(xs: java.util.Collection[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => e.startsWith(value), maxSucceed)

  def failEarlySucceededIndexesNotStartsWith(xs: java.util.Collection[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => !e.startsWith(value), maxSucceed)

  def failEarlySucceededIndexesEndsWith(xs: java.util.Collection[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => e.endsWith(value), maxSucceed)

  def failEarlySucceededIndexesNotEndsWith(xs: java.util.Collection[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => !e.endsWith(value), maxSucceed)

  def failEarlySucceededIndexesInclude(xs: java.util.Collection[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => e.indexOf(value) >= 0, maxSucceed)

  def failEarlySucceededIndexesNotInclude(xs: java.util.Collection[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => e.indexOf(value) < 0, maxSucceed)

  // SKIP-SCALATESTJS,NATIVE-END

  //################################################

  def failEarlySucceededIndexesSizeEqualIterable[T](xs: Iterable[Iterable[T]], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes[Iterable[T]](xs, _.size == value, maxSucceed)

  def failEarlySucceededIndexesSizeNotEqualIterable[T](xs: Iterable[Iterable[T]], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes[Iterable[T]](xs, _.size != value, maxSucceed)

  def failEarlySucceededIndexesSizeEqualIterableArray[T](xs: Iterable[Array[T]], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: Array[T]) => e.size == value, maxSucceed)

  def failEarlySucceededIndexesSizeNotEqualIterableArray[T](xs: Iterable[Array[T]], value: Int, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: Array[T]) => e.size != value, maxSucceed)

  def failEarlySucceededIndexesMatches(xs: Iterable[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => e.matches(value), maxSucceed)

  def failEarlySucceededIndexesNotMatches(xs: Iterable[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexes(xs, (e: String) => !e.matches(value), maxSucceed)

  def failEarlySucceededIndexesMatches(xs: java.util.Collection[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => e.matches(value), maxSucceed)

  def failEarlySucceededIndexesNotMatches(xs: java.util.Collection[String], value: String, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol(xs, (e: String) => !e.matches(value), maxSucceed)

  def failEarlySucceededIndexesContainIterable[T](xs: Iterable[Iterable[T]], right: T, maxSucceed: Int): String =
    failEarlySucceededIndexes[Iterable[T]](xs, _.exists(_ == right), maxSucceed)

  def failEarlySucceededIndexesNotContainIterable[T](xs: Iterable[Iterable[T]], right: T, maxSucceed: Int): String =
    failEarlySucceededIndexes[Iterable[T]](xs, !_.exists(_ == right), maxSucceed)

  def failEarlySucceededIndexesContainIterableArray[T](xs: Iterable[Array[T]], right: T, maxSucceed: Int): String =
    failEarlySucceededIndexes[Array[T]](xs, _.exists(_ == right), maxSucceed)

  def failEarlySucceededIndexesNotContainIterableArray[T](xs: Iterable[Array[T]], right: T, maxSucceed: Int): String =
    failEarlySucceededIndexes[Array[T]](xs, !_.exists(_ == right), maxSucceed)

  def failEarlySucceededIndexesRefEqual[T <: AnyRef](xs: Iterable[T], value: T, maxSucceed: Int): String =
    failEarlySucceededIndexes[T](xs, _ eq value, maxSucceed)

  def failEarlySucceededIndexesNotRefEqual[T <: AnyRef](xs: Iterable[T], value: T, maxSucceed: Int): String =
    failEarlySucceededIndexes[T](xs, _ ne value, maxSucceed)

  def failEarlySucceededIndexesRefEqual[T <: AnyRef](xs: java.util.Collection[T], value: T, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[T](xs, _ eq value, maxSucceed)

  def failEarlySucceededIndexesNotRefEqual[T <: AnyRef](xs: java.util.Collection[T], value: T, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[T](xs, _ ne value, maxSucceed)

  def failEarlySucceededIndexesContainKey[K, V](xs: Iterable[GenMap[K, V]], right: K, maxSucceed: Int): String =
    failEarlySucceededIndexes[GenMap[K, V]](xs, _.exists(_._1 == right), maxSucceed)

  def failEarlySucceededIndexesNotContainKey[K, V](xs: Iterable[GenMap[K, V]], right: K, maxSucceed: Int): String =
    failEarlySucceededIndexes[GenMap[K, V]](xs, !_.exists(_._1 == right), maxSucceed)

  def failEarlySucceededIndexesContainValue[K, V](xs: Iterable[GenMap[K, V]], right: V, maxSucceed: Int): String =
    failEarlySucceededIndexes[GenMap[K, V]](xs, _.exists(_._2 == right), maxSucceed)

  def failEarlySucceededIndexesNotContainValue[K, V](xs: Iterable[GenMap[K, V]], right: V, maxSucceed: Int): String =
    failEarlySucceededIndexes[GenMap[K, V]](xs, !_.exists(_._2 == right), maxSucceed)

  def failEarlySucceededIndexesJavaMapIsEmpty[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: Int = 0, maxSucceed: Int): String = // right is not used, but to be consistent to other so that easier for code generation
    failEarlySucceededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.isEmpty, maxSucceed)

  def failEarlySucceededIndexesJavaMapNotIsEmpty[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: Int = 0, maxSucceed: Int): String = // right is not used, but to be consistent to other so that easier for code generation
    failEarlySucceededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], !_.isEmpty, maxSucceed)

  def failEarlySucceededIndexesJavaMapContainKey[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: K, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.containsKey(right), maxSucceed)

  def failEarlySucceededIndexesJavaMapNotContainKey[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: K, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]],  !_.containsKey(right), maxSucceed)

  def failEarlySucceededIndexesJavaMapContainValue[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: V, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.containsValue(right), maxSucceed)

  def failEarlySucceededIndexesJavaMapNotContainValue[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: V, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], !_.containsValue(right), maxSucceed)

  def failEarlySucceededIndexesJavaMapSizeEqual[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.size == right, maxSucceed)

  def failEarlySucceededIndexesJavaMapSizeNotEqual[K, V, JMAP[k, v] <: java.util.Map[_, _]](xs: java.util.Collection[JMAP[K, V]], right: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[java.util.Map[K, V]](xs.asInstanceOf[java.util.Collection[java.util.Map[K, V]]], _.size != right, maxSucceed)

  def failEarlySucceededIndexesJavaColSizeEqual[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], _.size == right, maxSucceed)

  def failEarlySucceededIndexesJavaColSizeNotEqual[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: Int, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], _.size != right, maxSucceed)

  def failEarlySucceededIndexesJavaColContain[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: E, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], _.contains(right), maxSucceed)

  def failEarlySucceededIndexesJavaColNotContain[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: E, maxSucceed: Int): String =
    failEarlySucceededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], !_.contains(right), maxSucceed)

  def failEarlySucceededIndexesJavaColIsEmpty[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: Int = 0, maxSucceed: Int): String = // right is not used, but to be consistent to other so that easier for code generation
    failEarlySucceededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], _.isEmpty, maxSucceed)

  def failEarlySucceededIndexesJavaColNotIsEmpty[E, C[e] <: java.util.Collection[_]](xs: java.util.Collection[C[E]], right: Int = 0, maxSucceed: Int): String = // right is not used, but to be consistent to other so that easier for code generation
    failEarlySucceededIndexesInJavaCol[java.util.Collection[E]](xs.asInstanceOf[java.util.Collection[java.util.Collection[E]]], !_.isEmpty, maxSucceed)

  private val TEMP_DIR_ATTEMPTS = 10000

  // This is based on createTempDir here (Apache License): http://code.google.com/p/guava-libraries/source/browse/guava/src/com/google/common/io/Files.java
  // java.nio.file.Files#createTempDirectory() exists in Java 7 should be preferred when we no longer support Java 5/6.
  def createTempDirectory(): File = {
    val baseDir = new File(System.getProperty("java.io.tmpdir"))
    val baseName = System.currentTimeMillis + "-"

    @tailrec
    def tryCreateTempDirectory(counter: Int): Option[File] = {
      val tempDir = new File(baseDir, baseName + counter)
      if (tempDir.mkdir())
        Some(tempDir)
      else if (counter < TEMP_DIR_ATTEMPTS)
        tryCreateTempDirectory(counter + 1)
      else
        None
    }

    tryCreateTempDirectory(0) match {
      case Some(tempDir) => tempDir
      case None =>
        throw new IllegalStateException(
            "Failed to create directory within " +
            TEMP_DIR_ATTEMPTS + " attempts (tried " +
            baseName + "0 to " + baseName +
            (TEMP_DIR_ATTEMPTS - 1) + ')');
    }
  }

  def javaSet[T](elements: T*): java.util.Set[T] = {
    val javaSet = new java.util.HashSet[T]()
    elements.foreach(javaSet.add(_))
    javaSet
  }

  def javaList[T](elements: T*): java.util.List[T] = {
    val javaList = new java.util.ArrayList[T]()
    elements.foreach(javaList.add(_))
    javaList
  }

  def javaMap[K, V](elements: Entry[K, V]*): java.util.LinkedHashMap[K, V] = {
    val m = new java.util.LinkedHashMap[K, V]
    elements.foreach(e => m.put(e.getKey, e.getValue))
    m
  }

  // This gives a comparator that compares based on the value in the passed in order map
  private def orderMapComparator[T](orderMap: Map[T, Int]): java.util.Comparator[T] =
    new java.util.Comparator[T] {
      def compare(x: T, y: T): Int = {
          // When both x and y is defined in order map, use its corresponding value to compare (which in usage below, is the index of the insertion order)
          if (orderMap.get(x).isDefined && orderMap.get(y).isDefined)
            orderMap(x) compare orderMap(y)
          else {
            // It can happens that the comparator is used by equal method to check if 2 element is equaled.
            // In the use-case below, orderMap only contains elements within the TreeSet/TreeMap itself,
            // but in equal method elements from other instances of TreeSet/TreeMap can be passed in to check
            // for equality.  So the below handles element of type Int and String, which is enough for our tests.
            // hashCode will be used for other types of objects, in future, special care for other types can be added
            // if necessary.
            // The relationship and behavior of comparator/ordering/equals is quite well defined in JavaDoc of java.lang.Comparable here:
            // http://docs.oracle.com/javase/6/docs/api/java/lang/Comparable.html
            x match {
              case xInt: Int =>
                y match {
                  case yInt: Int => xInt compare yInt
                  case _ => x.hashCode compare y.hashCode
                }
              case xStr: String =>
                y match {
                  case yStr: String => xStr compare yStr
                  case _ => x.hashCode compare y.hashCode
                }
              case _ => x.hashCode compare y.hashCode
            }
          }
        }
    }

  def sortedSet[T](elements: T*): SortedSet[T] = {
    val orderMap = Map.empty[T, Int] ++ elements.distinct.zipWithIndex
    val comparator = orderMapComparator(orderMap)
    implicit val ordering = new Ordering[T] {
      def compare(x: T, y: T): Int = comparator.compare(x, y)
    }
    SortedSet.empty[T] ++ elements
  }

  def sortedMap[K, V](elements: (K, V)*): SortedMap[K, V] = {
    val orderMap = Map.empty[K, Int] ++ elements.distinct.map(_._1).zipWithIndex
    val comparator = orderMapComparator(orderMap)
    implicit val ordering = new Ordering[K] {
      def compare(x: K, y: K): Int = comparator.compare(x, y)
    }
    SortedMap.empty[K, V] ++ elements.distinct
  }

  def javaSortedSet[T](elements: T*): java.util.SortedSet[T] = {
    val orderMap = Map.empty[T, Int] ++ elements.distinct.zipWithIndex
    val comparator = orderMapComparator(orderMap)
    val sortedSet = new java.util.TreeSet[T](comparator)
    elements.foreach(sortedSet.add(_))
    sortedSet
  }

  def javaSortedMap[K, V](elements: Entry[K, V]*): java.util.SortedMap[K, V] = {
    val orderMap = Map.empty[K, Int] ++ elements.distinct.map(_.getKey).zipWithIndex
    val comparator = orderMapComparator(orderMap)
    val sortedMap = new java.util.TreeMap[K, V](comparator)
    elements.distinct.foreach(e => sortedMap.put(e.getKey, e.getValue))
    sortedMap
  }

  // SKIP-SCALATESTJS,NATIVE-START
  def serializeRoundtrip[A](a: A): A = {
    val baos = new java.io.ByteArrayOutputStream
    val oos = new java.io.ObjectOutputStream(baos)
    oos.writeObject(a)
    oos.flush()
    val ois = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(baos.toByteArray))
    ois.readObject.asInstanceOf[A]
  }
  // SKIP-SCALATESTJS,NATIVE-END

  def checkMessageStackDepth(exception: StackDepthException, message: String, fileName: String, lineNumber: Int): Unit = {
    assert(exception.message === Some(message))
    assert(exception.failedCodeFileName === Some(fileName))
    assert(exception.failedCodeLineNumber === Some(lineNumber))
  }

  def prettifyAst(str: String): String = {
    import scala.annotation.tailrec

    def getUntilNextDoubleQuote(itr: BufferedIterator[Char], buf: StringBuilder = new StringBuilder): String = {
      if (itr.hasNext) {
        val next = itr.next
        buf.append(next)
        if (next != '\"')
          getUntilNextDoubleQuote(itr, buf)
        else
          buf.toString
      }
      else
        throw new IllegalStateException("Expecting closing \", but none of them found")
    }

    val brackets = Set('(', ')')
    @tailrec
    def getNextBracket(itr: BufferedIterator[Char], buf: StringBuilder = new StringBuilder): (Char, String) = {
      if (itr.hasNext) {
        if (brackets.contains(itr.head))
          (itr.head, buf.toString)
        else {
          val next = itr.next
          buf.append(next)
          if (next == '\"')
            buf.append(getUntilNextDoubleQuote(itr))
          getNextBracket(itr, buf)
        }
      }
      else
        throw new IllegalStateException("Expecting '(' or ')', but none of them found")
    }

    @tailrec
    def transform(itr: BufferedIterator[Char], openBracket: Int, builder: StringBuilder, multilineBracket: Boolean = false): Unit = {
      if (itr.hasNext) {
        val next = itr.next
        val (newOpenBracket, newMultilineBracket) =
          next match {
            case '(' =>
              val (nextBracket, textWithin) = getNextBracket(itr)
              if (nextBracket == '(') {
                builder.append("(\n")
                val newOpenBracket = openBracket + 1
                builder.append("  " * newOpenBracket)
                builder.append(textWithin)
                (newOpenBracket, true)
              }
              else {
                builder.append("(")
                builder.append(textWithin)
                (openBracket, false)
              }

            case ')' =>
              val newOpenBracket =
                if (multilineBracket) {
                  builder.append("\n")
                  val newOpenBracket =
                    if (openBracket > 0)
                      openBracket - 1
                    else
                      openBracket
                  builder.append("  " * newOpenBracket)
                  newOpenBracket
                }
                else
                  openBracket

              if (itr.hasNext && itr.head == ',') {
                itr.next
                builder.append("),\n")
                builder.append("  " * newOpenBracket)
              }
              else
                builder.append(")")

              if (itr.hasNext && itr.head == ' ')
                itr.next

              if (newOpenBracket == 0)
                builder.append("\n")

              (newOpenBracket, true)
            case '\n' =>
              builder.append("\n")
              builder.append("  " * openBracket)
              (openBracket, multilineBracket)
            case other => builder.append(other)
              (openBracket, multilineBracket)
          }
        transform(itr, newOpenBracket, builder, newMultilineBracket)
      }
    }
    val itr = str.toCharArray.iterator.buffered
    val builder = new StringBuilder
    transform(itr, 0, builder)
    builder.toString
  }
}

