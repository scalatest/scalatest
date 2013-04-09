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

import org.scalatest.events._
import java.util.concurrent.Executors
import java.io.File
import scala.annotation.tailrec
import scala.collection.GenTraversable
import scala.collection.GenMap

trait SharedHelpers extends Assertions {

  object SilentReporter extends Reporter {
    def apply(event: Event) = ()  
  }

  class TestDurationReporter extends Reporter {
    var testSucceededWasFiredAndHadADuration = false
    var testFailedWasFiredAndHadADuration = false
    override def apply(event: Event) {
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
    override def apply(event: Event) {
      event match {
        case event: SuiteCompleted => suiteCompletedWasFiredAndHadADuration = event.duration.isDefined
        case event: SuiteAborted => suiteAbortedWasFiredAndHadADuration = event.duration.isDefined
        case _ =>
      }
    }
  }

  class PendingReporter extends Reporter {
    var testPendingWasFired = false
    override def apply(event: Event) {
      event match {
        case _: TestPending => testPendingWasFired = true
        case _ =>
      }
    }
  }

  class EventRecordingReporter extends Reporter {
    private var eventList: List[Event] = List()
    def eventsReceived = eventList.reverse
    def testSucceededEventsReceived: List[TestSucceeded] = {
      eventsReceived filter {
        case event: TestSucceeded => true
        case _ => false
      } map {
        case event: TestSucceeded => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    def testStartingEventsReceived: List[TestStarting] = {
      eventsReceived filter {
        case event: TestStarting => true
        case _ => false
      } map {
        case event: TestStarting => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    // Why doesn't this work:
    // for (event: TestSucceeded <- eventsReceived) yield event
    def infoProvidedEventsReceived: List[InfoProvided] = {
      eventsReceived filter {
        case event: InfoProvided => true
        case _ => false
      } map {
        case event: InfoProvided => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    def markupProvidedEventsReceived: List[MarkupProvided] = {
      eventsReceived filter {
        case event: MarkupProvided => true
        case _ => false
      } map {
        case event: MarkupProvided => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    def scopeOpenedEventsReceived: List[ScopeOpened] = {
      eventsReceived filter {
        case event: ScopeOpened => true
        case _ => false
      } map {
        case event: ScopeOpened => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    def scopeClosedEventsReceived: List[ScopeClosed] = {
      eventsReceived filter {
        case event: ScopeClosed => true
        case _ => false
      } map {
        case event: ScopeClosed => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    def scopePendingEventsReceived: List[ScopePending] = {
      eventsReceived filter {
        case event: ScopePending => true
        case _ => false
      } map {
        case event: ScopePending => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    def testPendingEventsReceived: List[TestPending] = {
      eventsReceived filter {
        case event: TestPending => true
        case _ => false
      } map {
        case event: TestPending => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    def testCanceledEventsReceived: List[TestCanceled] = {
      eventsReceived filter {
        case event: TestCanceled => true
        case _ => false
      } map {
        case event: TestCanceled => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    def testFailedEventsReceived: List[TestFailed] = {
      eventsReceived filter {
        case event: TestFailed => true
        case _ => false
      } map {
        case event: TestFailed => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    def testIgnoredEventsReceived: List[TestIgnored] = {
      eventsReceived filter {
        case event: TestIgnored => true
        case _ => false
      } map {
        case event: TestIgnored => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    def suiteStartingEventsReceived: List[SuiteStarting] = {
      eventsReceived filter {
        case event: SuiteStarting => true
        case _ => false
      } map {
        case event: SuiteStarting => event
        case _ => throw new RuntimeException("should never happen")
      }
    }
    def apply(event: Event) {
      eventList ::= event
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

  def ensureTestFailedEventReceived(suite: Suite, testName: String) {
    val reporter = new EventRecordingReporter
    suite.run(None, Args(reporter))
    val testFailedEvent = reporter.eventsReceived.find(_.isInstanceOf[TestFailed])
    assert(testFailedEvent.isDefined)
    assert(testFailedEvent.get.asInstanceOf[TestFailed].testName === testName)
  }
  
  def thisLineNumber = {
    val st = Thread.currentThread.getStackTrace

    if (!st(2).getMethodName.contains("thisLineNumber"))
      st(2).getLineNumber
    else
      st(3).getLineNumber
  }

  class TestIgnoredTrackingReporter extends Reporter {
    var testIgnoredReceived = false
    var lastEvent: Option[TestIgnored] = None
    def apply(event: Event) {
      event match {
        case event: TestIgnored =>
          testIgnoredReceived = true
          lastEvent = Some(event)
        case _ =>
      }
    }
  }
  
  class TestConcurrentDistributor(poolSize: Int) extends tools.ConcurrentDistributor(Args(reporter = SilentReporter), Executors.newFixedThreadPool(poolSize)) {
     override def apply(suite: Suite, tracker: Tracker) {
       throw new UnsupportedOperationException("Please use apply with args.")
     }
  }
  
  def getIndex[T](xs: GenTraversable[T], value: T): Int = {
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

  def getIndexes[T](xs: GenTraversable[T], values: GenTraversable[T]): GenTraversable[Int] = {
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
  
  @tailrec
  final def getNext[T](itr: Iterator[T], predicate: T => Boolean): T = {
    val next = itr.next
    if (predicate(next))
      next
    else
      getNext(itr, predicate)
  }
  
  def getFirst[T](col: GenTraversable[T], predicate: T => Boolean): T = 
    getNext(col.toIterator, predicate)
  
  @tailrec
  final def getNextNot[T](itr: Iterator[T], predicate: T => Boolean): T = {
    val next = itr.next
    if (!predicate(next))
      next
    else
      getNextNot(itr, predicate)
  }
  
  def getFirstNot[T](col: GenTraversable[T], predicate: T => Boolean): T = 
    getNextNot(col.toIterator, predicate)
    
  def getFirstEqual[T](col: GenTraversable[T], right: T): T = 
    getFirst[T](col, _ == right)
    
  def getFirstNotEqual[T](col: GenTraversable[T], right: T): T = 
    getFirst[T](col, _ != right)
    
  def getFirstMoreThanEqual(col: GenTraversable[Int], right: Int): Int = 
    getFirst[Int](col, _ >= right)
    
  def getFirstLessThanEqual(col: GenTraversable[Int], right: Int): Int = 
    getFirst[Int](col, _ <= right)
    
  def getFirstMoreThan(col: GenTraversable[Int], right: Int): Int = 
    getFirst[Int](col, _ > right)
    
  def getFirstLessThan(col: GenTraversable[Int], right: Int): Int = 
    getFirst[Int](col, _ < right)
    
  def getFirstIsEmpty(col: GenTraversable[String], right: String = ""): String = // right is not used, but to be consistent to other so that easier for code generation
    getFirst[String](col, _.isEmpty)
    
  def getFirstIsNotEmpty(col: GenTraversable[String], right: String = ""): String = // right is not used, but to be consistent to other so that easier for code generation
    getFirst[String](col, !_.isEmpty)
    
  def getFirstLengthEqual(col: GenTraversable[String], right: Int): String = 
    getFirst[String](col, _.length == right)
    
  def getFirstLengthNotEqual(col: GenTraversable[String], right: Int): String = 
    getFirst[String](col, _.length != right)
    
  def getFirstLengthNotEqualLength(col: GenTraversable[String], right: Int): String = 
    getFirst[String](col, _.length != right)
    
  def getFirstSizeEqual(col: GenTraversable[String], right: Int): String = 
    getFirst[String](col, _.size == right)
    
  def getFirstSizeNotEqual(col: GenTraversable[String], right: Int): String = 
    getFirst[String](col, _.size != right)
    
  def getFirstRefEqual[T <: AnyRef](col: GenTraversable[T], right: T): T = 
    getFirst[T](col, _ eq right)
    
  def getFirstNotRefEqual[T <: AnyRef](col: GenTraversable[T], right: T): T = 
    getFirst[T](col, _ ne right)
    
  def getFirstStartsWith(col: GenTraversable[String], right: String): String = 
    getFirst[String](col, _.startsWith(right))
    
  def getFirstNotStartsWith(col: GenTraversable[String], right: String): String = 
    getFirst[String](col, !_.startsWith(right))
    
  def getFirstEndsWith(col: GenTraversable[String], right: String): String = 
    getFirst[String](col, _.endsWith(right))
    
  def getFirstNotEndsWith(col: GenTraversable[String], right: String): String = 
    getFirst[String](col, !_.endsWith(right))
    
  def getFirstInclude(col: GenTraversable[String], right: String): String = 
    getFirst[String](col, _.indexOf(right) >= 0)
    
  def getFirstNotInclude(col: GenTraversable[String], right: String): String = 
    getFirst[String](col, _.indexOf(right) < 0)
  
  def getFirstMatches(col: GenTraversable[String], right: String): String = 
    getFirst[String](col, _.matches(right))
    
  def getFirstNotMatches(col: GenTraversable[String], right: String): String = 
    getFirst[String](col, !_.matches(right))
    
  def getFirstSizeEqualGenTraversable[T](col: GenTraversable[GenTraversable[T]], right: Int): GenTraversable[T] = 
    getFirst[GenTraversable[T]](col, _.size == right)
    
  def getFirstSizeNotEqualGenTraversable[T](col: GenTraversable[GenTraversable[T]], right: Int): GenTraversable[T] = 
    getFirst[GenTraversable[T]](col, _.size != right)
    
  def getFirstSizeEqualGenTraversableArray[T](col: GenTraversable[Array[T]], right: Int): Array[T] = 
    getFirst[Array[T]](col, _.size == right)
    
  def getFirstSizeNotEqualGenTraversableArray[T](col: GenTraversable[Array[T]], right: Int): Array[T] = 
    getFirst[Array[T]](col, _.size != right)
    
  def getFirstIsEmpty[T](col: GenTraversable[GenTraversable[T]], right: T): GenTraversable[T] = 
    getFirst[GenTraversable[T]](col, _.isEmpty)
    
  def getFirstNotIsEmpty[T](col: GenTraversable[GenTraversable[T]], right: T): GenTraversable[T] = 
    getFirst[GenTraversable[T]](col, !_.isEmpty)
    
  def getFirstContainGenTraversable[T](col: GenTraversable[GenTraversable[T]], right: T): GenTraversable[T] = 
    getFirst[GenTraversable[T]](col, _.exists(_ == right))
    
  def getFirstNotContainGenTraversable[T](col: GenTraversable[GenTraversable[T]], right: T): GenTraversable[T] = 
    getFirst[GenTraversable[T]](col, !_.exists(_ == right))
    
  def getFirstContainGenTraversableArray[T](col: GenTraversable[Array[T]], right: T): Array[T] = 
    getFirst[Array[T]](col, _.exists(_ == right))
    
  def getFirstNotContainGenTraversableArray[T](col: GenTraversable[Array[T]], right: T): Array[T] = 
    getFirst[Array[T]](col, !_.exists(_ == right))
    
  def getFirstContainKey[K, V](col: GenTraversable[GenMap[K, V]], right: K): GenMap[K, V] = 
    getFirst[GenMap[K, V]](col, _.exists(_._1 == right))
    
  def getFirstNotContainKey[K, V](col: GenTraversable[GenMap[K, V]], right: K): GenMap[K, V] = 
    getFirst[GenMap[K, V]](col, !_.exists(_._1 == right))
    
  def getFirstContainValue[K, V](col: GenTraversable[GenMap[K, V]], right: V): GenMap[K, V] = 
    getFirst[GenMap[K, V]](col, _.exists(_._2 == right))
    
  def getFirstNotContainValue[K, V](col: GenTraversable[GenMap[K, V]], right: V): GenMap[K, V] = 
    getFirst[GenMap[K, V]](col, !_.exists(_._2 == right))
    
  def getFirstJavaMapIsEmpty[K, V](col: GenTraversable[java.util.Map[K, V]], right: Int = 0): java.util.Map[K, V] = // right is not used, but to be consistent to other so that easier for code generation
    getFirst[java.util.Map[K, V]](col, _.isEmpty)
    
  def getFirstJavaMapNotIsEmpty[K, V](col: GenTraversable[java.util.Map[K, V]], right: Int = 0): java.util.Map[K, V] = // right is not used, but to be consistent to other so that easier for code generation
    getFirst[java.util.Map[K, V]](col, !_.isEmpty)
    
  def getFirstJavaMapContainKey[K, V](col: GenTraversable[java.util.Map[K, V]], right: K): java.util.Map[K, V] = 
    getFirst[java.util.Map[K, V]](col, _.containsKey(right))
    
  def getFirstJavaMapNotContainKey[K, V](col: GenTraversable[java.util.Map[K, V]], right: K): java.util.Map[K, V] = 
    getFirst[java.util.Map[K, V]](col,  !_.containsKey(right))
    
  def getFirstJavaMapContainValue[K, V](col: GenTraversable[java.util.Map[K, V]], right: V): java.util.Map[K, V] = 
    getFirst[java.util.Map[K, V]](col, _.containsValue(right))
    
  def getFirstJavaMapNotContainValue[K, V](col: GenTraversable[java.util.Map[K, V]], right: V): java.util.Map[K, V] = 
    getFirst[java.util.Map[K, V]](col, !_.containsValue(right))
    
  def getFirstJavaMapSizeEqual[K, V](col: GenTraversable[java.util.Map[K, V]], right: Int): java.util.Map[K, V] = 
    getFirst[java.util.Map[K, V]](col, _.size == right)
    
  def getFirstJavaMapSizeNotEqual[K, V](col: GenTraversable[java.util.Map[K, V]], right: Int): java.util.Map[K, V] = 
    getFirst[java.util.Map[K, V]](col, _.size != right)
    
  def getFirstJavaColSizeEqual[T](col: GenTraversable[java.util.Collection[T]], right: Int): java.util.Collection[T] = 
    getFirst[java.util.Collection[T]](col, _.size == right)
    
  def getFirstJavaColSizeNotEqual[T](col: GenTraversable[java.util.Collection[T]], right: Int): java.util.Collection[T] = 
    getFirst[java.util.Collection[T]](col, _.size != right)
    
  def getFirstJavaColContain[T](col: GenTraversable[java.util.Collection[T]], right: T): java.util.Collection[T] = 
    getFirst[java.util.Collection[T]](col, _.contains(right))
    
  def getFirstJavaColNotContain[T](col: GenTraversable[java.util.Collection[T]], right: T): java.util.Collection[T] = 
    getFirst[java.util.Collection[T]](col, !_.contains(right))
    
  def getFirstJavaColIsEmpty[T](col: GenTraversable[java.util.Collection[T]], right: Int = 0): java.util.Collection[T] = // right is not used, but to be consistent to other so that easier for code generation
    getFirst[java.util.Collection[T]](col, _.isEmpty)
    
  def getFirstJavaColNotIsEmpty[T](col: GenTraversable[java.util.Collection[T]], right: Int = 0): java.util.Collection[T] = // right is not used, but to be consistent to other so that easier for code generation
    getFirst[java.util.Collection[T]](col, !_.isEmpty)
    
  def indexElement[T](itr: Iterator[T], xs: GenTraversable[T], errorFun: T => Boolean): Array[String] = { 
    val element = getNext[T](itr, errorFun) 
    val index = getIndex(xs, element) 
    Array(index.toString, (if (element != null && element.isInstanceOf[Array[_]]) element.asInstanceOf[Array[T]].deep.toString else element + "")) 
  } 
  
  def indexElementLength[T](itr: Iterator[String], xs: GenTraversable[String], errorFun: String => Boolean): Array[String] = { 
    val element = getNext[String](itr, errorFun)
    val index = getIndex(xs, element)
    Array(index.toString, element.length.toString, (if (element != null && element.isInstanceOf[Array[_]]) element.asInstanceOf[Array[T]].deep.toString else element.toString)) 
  }
  
  def indexElementEqual[T](itr: Iterator[T], xs: GenTraversable[T], right: T): Array[String] = 
    indexElement[T](itr, xs, _ == right)
    
  def indexElementNotEqual[T](itr: Iterator[T], xs: GenTraversable[T], right: T): Array[String] = 
    indexElement[T](itr, xs, _ != right)
    
  def indexElementMoreThan(itr: Iterator[Int], xs: GenTraversable[Int], right: Int): Array[String] = 
    indexElement[Int](itr, xs, _ > right)
  
  def indexElementMoreThanEqual(itr: Iterator[Int], xs: GenTraversable[Int], right: Int): Array[String] = 
    indexElement[Int](itr, xs, _ >= right)
    
  def indexElementLessThan(itr: Iterator[Int], xs: GenTraversable[Int], right: Int): Array[String] = 
    indexElement[Int](itr, xs, _ < right)
    
  def indexElementLessThanEqual(itr: Iterator[Int], xs: GenTraversable[Int], right: Int): Array[String] = 
    indexElement[Int](itr, xs, _ <= right)
     
  def indexElementIsEmpty(itr: Iterator[String], xs: GenTraversable[String], right: String = ""): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElement[String](itr, xs, _.isEmpty)
    
  def indexElementIsNotEmpty(itr: Iterator[String], xs: GenTraversable[String], right: String = ""): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElement[String](itr, xs, !_.isEmpty)
    
  def indexElementLengthEqual(itr: Iterator[String], xs: GenTraversable[String], right: Int): Array[String] = 
    indexElement[String](itr, xs, _.length == right)
    
  def indexElementLengthNotEqual(itr: Iterator[String], xs: GenTraversable[String], right: Int): Array[String] = 
    indexElement[String](itr, xs, _.length != right)
    
  def indexElementSizeEqual(itr: Iterator[String], xs: GenTraversable[String], right: Int): Array[String] = 
    indexElement[String](itr, xs, _.size == right)
    
  def indexElementSizeNotEqual(itr: Iterator[String], xs: GenTraversable[String], right: Int): Array[String] = 
    indexElement[String](itr, xs, _.size != right)
    
  def indexElementLengthNotEqualLength(itr: Iterator[String], xs: GenTraversable[String], right: Int): Array[String] = 
    indexElementLength[String](itr, xs, _.length != right)
    
  def indexElementStartsWith(itr: Iterator[String], xs: GenTraversable[String], right: String): Array[String] = 
    indexElement[String](itr, xs, _.startsWith(right))
    
  def indexElementNotStartsWith(itr: Iterator[String], xs: GenTraversable[String], right: String): Array[String] = 
    indexElement[String](itr, xs, !_.startsWith(right))
    
  def indexElementEndsWith(itr: Iterator[String], xs: GenTraversable[String], right: String): Array[String] = 
    indexElement[String](itr, xs, _.endsWith(right))
    
  def indexElementNotEndsWith(itr: Iterator[String], xs: GenTraversable[String], right: String): Array[String] = 
    indexElement[String](itr, xs, !_.endsWith(right))
    
  def indexElementInclude(itr: Iterator[String], xs: GenTraversable[String], right: String): Array[String] = 
    indexElement[String](itr, xs, _.indexOf(right) >= 0)
    
  def indexElementNotInclude(itr: Iterator[String], xs: GenTraversable[String], right: String): Array[String] = 
    indexElement[String](itr, xs, _.indexOf(right) < 0)
    
  def indexElementMatches(itr: Iterator[String], xs: GenTraversable[String], right: String): Array[String] = 
    indexElement[String](itr, xs, _.matches(right))
    
  def indexElementNotMatches(itr: Iterator[String], xs: GenTraversable[String], right: String): Array[String] = 
    indexElement[String](itr, xs, !_.matches(right))
    
  def indexElementSizeEqualGenTraversable[T](itr: Iterator[GenTraversable[T]], xs: GenTraversable[GenTraversable[T]], right: Int): Array[String] = 
    indexElement[GenTraversable[T]](itr, xs, _.size == right)
    
  def indexElementSizeNotEqualGenTraversable[T](itr: Iterator[GenTraversable[T]], xs: GenTraversable[GenTraversable[T]], right: Int): Array[String] = 
    indexElement[GenTraversable[T]](itr, xs, _.size != right)
    
  def indexElementSizeEqualGenTraversableArray[T](itr: Iterator[Array[T]], xs: GenTraversable[Array[T]], right: Int): Array[T] = 
    indexElement[Array[T]](itr, xs, _.size == right).asInstanceOf[Array[T]]
    
  def indexElementSizeNotEqualGenTraversableArray[T](itr: Iterator[Array[T]], xs: GenTraversable[Array[T]], right: Int): Array[T] = 
    indexElement[Array[T]](itr, xs, _.size != right).asInstanceOf[Array[T]]
  
  def indexElementContainGenTraversable[T](itr: Iterator[GenTraversable[T]], xs: GenTraversable[GenTraversable[T]], right: T): Array[String] = 
    indexElement[GenTraversable[T]](itr, xs, _.exists(_ == right))
    
  def indexElementNotContainGenTraversable[T](itr: Iterator[GenTraversable[T]], xs: GenTraversable[GenTraversable[T]], right: T): Array[String] = 
    indexElement[GenTraversable[T]](itr, xs, !_.exists(_ == right))
    
  def indexElementContainGenTraversableArray[T](itr: Iterator[Array[T]], xs: GenTraversable[Array[T]], right: T): Array[T] = 
    indexElement[Array[T]](itr, xs, _.exists(_ == right)).asInstanceOf[Array[T]]
    
  def indexElementNotContainGenTraversableArray[T](itr: Iterator[Array[T]], xs: GenTraversable[Array[T]], right: T): Array[T] = 
    indexElement[Array[T]](itr, xs, !_.exists(_ == right)).asInstanceOf[Array[T]]
  
  def indexElementRefEqual[T <: AnyRef](itr: Iterator[T], xs: GenTraversable[T], right: T): Array[String] = 
    indexElement[T](itr, xs, _ eq right)
    
  def indexElementNotRefEqual[T <: AnyRef](itr: Iterator[T], xs: GenTraversable[T], right: T): Array[String] = 
    indexElement[T](itr, xs, _ ne right)
    
  def indexElementContainKey[K, V](itr: Iterator[GenMap[K, V]], xs: GenTraversable[GenMap[K, V]], right: K): Array[String] = 
    indexElement[GenMap[K, V]](itr, xs, _.exists(_._1 == right))
    
  def indexElementNotContainKey[K, V](itr: Iterator[GenMap[K, V]], xs: GenTraversable[GenMap[K, V]], right: K): Array[String] = 
    indexElement[GenMap[K, V]](itr, xs, !_.exists(_._1 == right))
    
  def indexElementContainValue[K, V](itr: Iterator[GenMap[K, V]], xs: GenTraversable[GenMap[K, V]], right: V): Array[String] = 
    indexElement[GenMap[K, V]](itr, xs, _.exists(_._2 == right))
    
  def indexElementNotContainValue[K, V](itr: Iterator[GenMap[K, V]], xs: GenTraversable[GenMap[K, V]], right: V): Array[String] = 
    indexElement[GenMap[K, V]](itr, xs, !_.exists(_._2 == right))
    
  def indexElementJavaMapIsEmpty[K, V](itr: Iterator[java.util.Map[K, V]], xs: GenTraversable[java.util.Map[K, V]], right: Int = 0): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElement[java.util.Map[K, V]](itr, xs, _.isEmpty)
    
  def indexElementJavaMapNotIsEmpty[K, V](itr: Iterator[java.util.Map[K, V]], xs: GenTraversable[java.util.Map[K, V]], right: Int = 0): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElement[java.util.Map[K, V]](itr, xs, !_.isEmpty)
    
  def indexElementJavaMapContainKey[K, V](itr: Iterator[java.util.Map[K, V]], xs: GenTraversable[java.util.Map[K, V]], right: K): Array[String] = 
    indexElement[java.util.Map[K, V]](itr, xs, _.containsKey(right))
    
  def indexElementJavaMapNotContainKey[K, V](itr: Iterator[java.util.Map[K, V]], xs: GenTraversable[java.util.Map[K, V]], right: K): Array[String] = 
    indexElement[java.util.Map[K, V]](itr, xs,  !_.containsKey(right))
    
  def indexElementJavaMapContainValue[K, V](itr: Iterator[java.util.Map[K, V]], xs: GenTraversable[java.util.Map[K, V]], right: V): Array[String] = 
    indexElement[java.util.Map[K, V]](itr, xs, _.containsValue(right))
    
  def indexElementJavaMapNotContainValue[K, V](itr: Iterator[java.util.Map[K, V]], xs: GenTraversable[java.util.Map[K, V]], right: V): Array[String] = 
    indexElement[java.util.Map[K, V]](itr, xs, !_.containsValue(right))
    
  def indexElementJavaMapSizeEqual[K, V](itr: Iterator[java.util.Map[K, V]], xs: GenTraversable[java.util.Map[K, V]], right: Int): Array[String] = 
    indexElement[java.util.Map[K, V]](itr, xs, _.size == right)
    
  def indexElementJavaMapSizeNotEqual[K, V](itr: Iterator[java.util.Map[K, V]], xs: GenTraversable[java.util.Map[K, V]], right: Int): Array[String] = 
    indexElement[java.util.Map[K, V]](itr, xs, _.size != right)
    
  def indexElementJavaColSizeEqual[T](itr: Iterator[java.util.Collection[T]], xs: GenTraversable[java.util.Collection[T]], right: Int): Array[String] = 
    indexElement[java.util.Collection[T]](itr, xs, _.size == right)
    
  def indexElementJavaColSizeNotEqual[T](itr: Iterator[java.util.Collection[T]], xs: GenTraversable[java.util.Collection[T]], right: Int): Array[String] = 
    indexElement[java.util.Collection[T]](itr, xs, _.size != right)
    
  def indexElementJavaColContain[T](itr: Iterator[java.util.Collection[T]], xs: GenTraversable[java.util.Collection[T]], right: T): Array[String] = 
    indexElement[java.util.Collection[T]](itr, xs, _.contains(right))
    
  def indexElementJavaColNotContain[T](itr: Iterator[java.util.Collection[T]], xs: GenTraversable[java.util.Collection[T]], right: T): Array[String] = 
    indexElement[java.util.Collection[T]](itr, xs, !_.contains(right))
    
  def indexElementJavaColIsEmpty[T](itr: Iterator[java.util.Collection[T]], xs: GenTraversable[java.util.Collection[T]], right: Int = 0): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElement[java.util.Collection[T]](itr, xs, _.isEmpty)
    
  def indexElementJavaColNotIsEmpty[T](itr: Iterator[java.util.Collection[T]], xs: GenTraversable[java.util.Collection[T]], right: Int = 0): Array[String] = // right is not used, but to be consistent to other so that easier for code generation
    indexElement[java.util.Collection[T]](itr, xs, !_.isEmpty)
  
  private def succeededIndexes[T](xs: GenTraversable[T], filterFun: T => Boolean): String = {
    val passedList = getIndexes(xs, xs.toList.filter(e => filterFun(e))).toList
    if (passedList.size > 1) 
      "index " + passedList.dropRight(1).mkString(", ") + " and " + passedList.last
    else if (passedList.size == 1)
      "index " + passedList.last.toString
    else 
      ""
  }
  
  private def failEarlySucceededIndexes[T](xs: GenTraversable[T], filterFun: T => Boolean, maxSucceed: Int): String = {
    val itr = xs.toIterator
    val passedList = getIndexes(xs, xs.toList.filter(e => filterFun(e))).take(maxSucceed).toList
    if (passedList.size > 1)
      "index " + passedList.dropRight(1).mkString(", ") + " and " + passedList.last
    else if (passedList.size == 1)
      "index " + passedList.last.toString
    else
      ""
  }
  
  def succeededIndexesEqualBoolean[T](xs: GenTraversable[T], value: Boolean): String = 
    succeededIndexes(xs, (e: T) => value)
  
  def succeededIndexesNotEqualBoolean[T](xs: GenTraversable[T], value: Boolean): String = 
    succeededIndexes(xs, (e: T) => !value)
    
  def succeededIndexesEqual[T](xs: GenTraversable[T], value: T): String = 
    succeededIndexes(xs, (e: T) => e == value)
    
  def succeededIndexesNotEqual[T](xs: GenTraversable[T], value: T): String = 
    succeededIndexes(xs, (e: T) => e != value)
    
  def succeededIndexesLessThanEqual(xs: GenTraversable[Int], value: Int): String = 
    succeededIndexes(xs, (e: Int) => e <= value)
    
  def succeededIndexesLessThan(xs: GenTraversable[Int], value: Int): String = 
    succeededIndexes(xs, (e: Int) => e < value)
    
  def succeededIndexesMoreThanEqual(xs: GenTraversable[Int], value: Int): String = 
    succeededIndexes(xs, (e: Int) => e >= value)
    
  def succeededIndexesMoreThan(xs: GenTraversable[Int], value: Int): String = 
    succeededIndexes(xs, (e: Int) => e > value)
    
  def succeededIndexesIsEmpty(xs: GenTraversable[String], value: String): String = 
    succeededIndexes(xs, (e: String) => e.isEmpty)
  
  def succeededIndexesIsNotEmpty(xs: GenTraversable[String], value: String): String = 
    succeededIndexes(xs, (e: String) => !e.isEmpty)
    
  def succeededIndexesSizeEqual(xs: GenTraversable[String], value: Int): String = 
    succeededIndexes(xs, (e: String) => e.size == value)
    
  def succeededIndexesSizeNotEqual(xs: GenTraversable[String], value: Int): String = 
    succeededIndexes(xs, (e: String) => e.size != value)
    
  def succeededIndexesLengthEqual(xs: GenTraversable[String], value: Int): String = 
    succeededIndexes(xs, (e: String) => e.length == value)
    
  def succeededIndexesLengthNotEqual(xs: GenTraversable[String], value: Int): String = 
    succeededIndexes(xs, (e: String) => e.length != value)
    
  def succeededIndexesStartsWith(xs: GenTraversable[String], value: String): String = 
    succeededIndexes(xs, (e: String) => e.startsWith(value))
  
  def succeededIndexesNotStartsWith(xs: GenTraversable[String], value: String): String = 
    succeededIndexes(xs, (e: String) => !e.startsWith(value))
    
  def succeededIndexesEndsWith(xs: GenTraversable[String], value: String): String = 
    succeededIndexes(xs, (e: String) => e.endsWith(value))
  
  def succeededIndexesNotEndsWith(xs: GenTraversable[String], value: String): String = 
    succeededIndexes(xs, (e: String) => !e.endsWith(value))
    
  def succeededIndexesInclude(xs: GenTraversable[String], value: String): String = 
    succeededIndexes(xs, (e: String) => e.indexOf(value) >= 0)
  
  def succeededIndexesNotInclude(xs: GenTraversable[String], value: String): String = 
    succeededIndexes(xs, (e: String) => e.indexOf(value) < 0)
    
  def succeededIndexesSizeEqualGenTraversable[T](xs: GenTraversable[GenTraversable[T]], value: Int): String = 
    succeededIndexes(xs, (e: GenTraversable[T]) => e.size == value)
  
  def succeededIndexesSizeNotEqualGenTraversable[T](xs: GenTraversable[GenTraversable[T]], value: Int): String = 
    succeededIndexes(xs, (e: GenTraversable[T]) => e.size != value)
    
  def succeededIndexesSizeEqualGenTraversableArray[T](xs: GenTraversable[Array[T]], value: Int): String = 
    succeededIndexes(xs, (e: Array[T]) => e.size == value)
  
  def succeededIndexesSizeNotEqualGenTraversableArray[T](xs: GenTraversable[Array[T]], value: Int): String = 
    succeededIndexes(xs, (e: Array[T]) => e.size != value)
    
  def succeededIndexesMatches(xs: GenTraversable[String], value: String): String = 
    succeededIndexes(xs, (e: String) => e.matches(value))
  
  def succeededIndexesNotMatches(xs: GenTraversable[String], value: String): String = 
    succeededIndexes(xs, (e: String) => !e.matches(value))
    
  def succeededIndexesContainGenTraversable[T](xs: GenTraversable[GenTraversable[T]], right: T): String = 
    succeededIndexes[GenTraversable[T]](xs, _.exists(_ == right))
    
  def succeededIndexesNotContainGenTraversable[T](xs: GenTraversable[GenTraversable[T]], right: T): String = 
    succeededIndexes[GenTraversable[T]](xs, !_.exists(_ == right))
    
  def succeededIndexesContainGenTraversableArray[T](xs: GenTraversable[Array[T]], right: T): String = 
    succeededIndexes[Array[T]](xs, _.exists(_ == right))
    
  def succeededIndexesNotContainGenTraversableArray[T](xs: GenTraversable[Array[T]], right: T): String = 
    succeededIndexes[Array[T]](xs, !_.exists(_ == right))
    
  def succeededIndexesRefEqual[T <: AnyRef](xs: GenTraversable[T], value: T): String = 
    succeededIndexes[T](xs, _ eq value)
    
  def succeededIndexesNotRefEqual[T <: AnyRef](xs: GenTraversable[T], value: T): String = 
    succeededIndexes[T](xs, _ ne value)
    
  def succeededIndexesContainKey[K, V](xs: GenTraversable[GenMap[K, V]], right: K): String = 
    succeededIndexes[GenMap[K, V]](xs, _.exists(_._1 == right))
    
  def succeededIndexesNotContainKey[K, V](xs: GenTraversable[GenMap[K, V]], right: K): String = 
    succeededIndexes[GenMap[K, V]](xs, !_.exists(_._1 == right))
    
  def succeededIndexesContainValue[K, V](xs: GenTraversable[GenMap[K, V]], right: V): String = 
    succeededIndexes[GenMap[K, V]](xs, _.exists(_._2 == right))
    
  def succeededIndexesNotContainValue[K, V](xs: GenTraversable[GenMap[K, V]], right: V): String = 
    succeededIndexes[GenMap[K, V]](xs, !_.exists(_._2 == right))
    
  def succeededIndexesJavaMapIsEmpty[K, V](xs: GenTraversable[java.util.Map[K, V]], right: Int = 0): String = // right is not used, but to be consistent to other so that easier for code generation
    succeededIndexes[java.util.Map[K, V]](xs, _.isEmpty)
    
  def succeededIndexesJavaMapNotIsEmpty[K, V](xs: GenTraversable[java.util.Map[K, V]], right: Int = 0): String = // right is not used, but to be consistent to other so that easier for code generation
    succeededIndexes[java.util.Map[K, V]](xs, !_.isEmpty)
    
  def succeededIndexesJavaMapContainKey[K, V](xs: GenTraversable[java.util.Map[K, V]], right: K): String = 
    succeededIndexes[java.util.Map[K, V]](xs, _.containsKey(right))
    
  def succeededIndexesJavaMapNotContainKey[K, V](xs: GenTraversable[java.util.Map[K, V]], right: K): String = 
    succeededIndexes[java.util.Map[K, V]](xs,  !_.containsKey(right))
    
  def succeededIndexesJavaMapContainValue[K, V](xs: GenTraversable[java.util.Map[K, V]], right: V): String = 
    succeededIndexes[java.util.Map[K, V]](xs, _.containsValue(right))
    
  def succeededIndexesJavaMapNotContainValue[K, V](xs: GenTraversable[java.util.Map[K, V]], right: V): String = 
    succeededIndexes[java.util.Map[K, V]](xs, !_.containsValue(right))
    
  def succeededIndexesJavaMapSizeEqual[K, V](xs: GenTraversable[java.util.Map[K, V]], right: Int): String = 
    succeededIndexes[java.util.Map[K, V]](xs, _.size == right)
    
  def succeededIndexesJavaMapSizeNotEqual[K, V](xs: GenTraversable[java.util.Map[K, V]], right: Int): String = 
    succeededIndexes[java.util.Map[K, V]](xs, _.size != right)
    
  def succeededIndexesJavaColSizeEqual[T](xs: GenTraversable[java.util.Collection[T]], right: Int): String = 
    succeededIndexes[java.util.Collection[T]](xs, _.size == right)
    
  def succeededIndexesJavaColSizeNotEqual[T](xs: GenTraversable[java.util.Collection[T]], right: Int): String = 
    succeededIndexes[java.util.Collection[T]](xs, _.size != right)
    
  def succeededIndexesJavaColContain[T](xs: GenTraversable[java.util.Collection[T]], right: T): String = 
    succeededIndexes[java.util.Collection[T]](xs, _.contains(right))
    
  def succeededIndexesJavaColNotContain[T](xs: GenTraversable[java.util.Collection[T]], right: T): String = 
    succeededIndexes[java.util.Collection[T]](xs, !_.contains(right))
    
  def succeededIndexesJavaColIsEmpty[T](xs: GenTraversable[java.util.Collection[T]], right: Int = 0): String = // right is not used, but to be consistent to other so that easier for code generation
    succeededIndexes[java.util.Collection[T]](xs, _.isEmpty)
    
  def succeededIndexesJavaColNotIsEmpty[T](xs: GenTraversable[java.util.Collection[T]], right: Int = 0): String = // right is not used, but to be consistent to other so that easier for code generation
    succeededIndexes[java.util.Collection[T]](xs, !_.isEmpty)
    
  def failEarlySucceededIndexesEqualBoolean[T](xs: GenTraversable[T], value: Boolean, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: T) => value, maxSucceed)
  
  def failEarlySucceededIndexesNotEqualBoolean[T](xs: GenTraversable[T], value: Boolean, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: T) => !value, maxSucceed)
    
  def failEarlySucceededIndexesEqual[T](xs: GenTraversable[T], value: T, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: T) => e == value, maxSucceed)
    
  def failEarlySucceededIndexesNotEqual[T](xs: GenTraversable[T], value: T, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: T) => e != value, maxSucceed)
    
  def failEarlySucceededIndexesLessThanEqual(xs: GenTraversable[Int], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: Int) => e <= value, maxSucceed)
    
  def failEarlySucceededIndexesLessThan(xs: GenTraversable[Int], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: Int) => e < value, maxSucceed)
    
  def failEarlySucceededIndexesMoreThanEqual(xs: GenTraversable[Int], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: Int) => e >= value, maxSucceed)
    
  def failEarlySucceededIndexesMoreThan(xs: GenTraversable[Int], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: Int) => e > value, maxSucceed)
    
  def failEarlySucceededIndexesIsEmpty(xs: GenTraversable[String], value: String, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => e.isEmpty, maxSucceed)
  
  def failEarlySucceededIndexesIsNotEmpty(xs: GenTraversable[String], value: String, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => !e.isEmpty, maxSucceed)
    
  def failEarlySucceededIndexesSizeEqual(xs: GenTraversable[String], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => e.size == value, maxSucceed)
    
  def failEarlySucceededIndexesSizeNotEqual(xs: GenTraversable[String], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => e.size != value, maxSucceed)
    
  def failEarlySucceededIndexesLengthEqual(xs: GenTraversable[String], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => e.length == value, maxSucceed)
    
  def failEarlySucceededIndexesLengthNotEqual(xs: GenTraversable[String], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => e.length != value, maxSucceed)
    
  def failEarlySucceededIndexesStartsWith(xs: GenTraversable[String], value: String, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => e.startsWith(value), maxSucceed)
  
  def failEarlySucceededIndexesNotStartsWith(xs: GenTraversable[String], value: String, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => !e.startsWith(value), maxSucceed)
    
  def failEarlySucceededIndexesEndsWith(xs: GenTraversable[String], value: String, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => e.endsWith(value), maxSucceed)
  
  def failEarlySucceededIndexesNotEndsWith(xs: GenTraversable[String], value: String, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => !e.endsWith(value), maxSucceed)
    
  def failEarlySucceededIndexesInclude(xs: GenTraversable[String], value: String, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => e.indexOf(value) >= 0, maxSucceed)
  
  def failEarlySucceededIndexesNotInclude(xs: GenTraversable[String], value: String, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => e.indexOf(value) < 0, maxSucceed)
    
  def failEarlySucceededIndexesSizeEqualGenTraversable[T](xs: GenTraversable[GenTraversable[T]], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes[GenTraversable[T]](xs, _.size == value, maxSucceed)
  
  def failEarlySucceededIndexesSizeNotEqualGenTraversable[T](xs: GenTraversable[GenTraversable[T]], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes[GenTraversable[T]](xs, _.size != value, maxSucceed)
    
  def failEarlySucceededIndexesSizeEqualGenTraversableArray[T](xs: GenTraversable[Array[T]], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: Array[T]) => e.size == value, maxSucceed)
  
  def failEarlySucceededIndexesSizeNotEqualGenTraversableArray[T](xs: GenTraversable[Array[T]], value: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: Array[T]) => e.size != value, maxSucceed)
    
  def failEarlySucceededIndexesMatches(xs: GenTraversable[String], value: String, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => e.matches(value), maxSucceed)
  
  def failEarlySucceededIndexesNotMatches(xs: GenTraversable[String], value: String, maxSucceed: Int): String = 
    failEarlySucceededIndexes(xs, (e: String) => !e.matches(value), maxSucceed)
    
  def failEarlySucceededIndexesContainGenTraversable[T](xs: GenTraversable[GenTraversable[T]], right: T, maxSucceed: Int): String = 
    failEarlySucceededIndexes[GenTraversable[T]](xs, _.exists(_ == right), maxSucceed)
    
  def failEarlySucceededIndexesNotContainGenTraversable[T](xs: GenTraversable[GenTraversable[T]], right: T, maxSucceed: Int): String = 
    failEarlySucceededIndexes[GenTraversable[T]](xs, !_.exists(_ == right), maxSucceed)
    
  def failEarlySucceededIndexesContainGenTraversableArray[T](xs: GenTraversable[Array[T]], right: T, maxSucceed: Int): String = 
    failEarlySucceededIndexes[Array[T]](xs, _.exists(_ == right), maxSucceed)
    
  def failEarlySucceededIndexesNotContainGenTraversableArray[T](xs: GenTraversable[Array[T]], right: T, maxSucceed: Int): String = 
    failEarlySucceededIndexes[Array[T]](xs, !_.exists(_ == right), maxSucceed)
    
  def failEarlySucceededIndexesRefEqual[T <: AnyRef](xs: GenTraversable[T], value: T, maxSucceed: Int): String = 
    failEarlySucceededIndexes[T](xs, _ eq value, maxSucceed)
    
  def failEarlySucceededIndexesNotRefEqual[T <: AnyRef](xs: GenTraversable[T], value: T, maxSucceed: Int): String = 
    failEarlySucceededIndexes[T](xs, _ ne value, maxSucceed)
    
  def failEarlySucceededIndexesContainKey[K, V](xs: GenTraversable[GenMap[K, V]], right: K, maxSucceed: Int): String = 
    failEarlySucceededIndexes[GenMap[K, V]](xs, _.exists(_._1 == right), maxSucceed)
    
  def failEarlySucceededIndexesNotContainKey[K, V](xs: GenTraversable[GenMap[K, V]], right: K, maxSucceed: Int): String = 
    failEarlySucceededIndexes[GenMap[K, V]](xs, !_.exists(_._1 == right), maxSucceed)
    
  def failEarlySucceededIndexesContainValue[K, V](xs: GenTraversable[GenMap[K, V]], right: V, maxSucceed: Int): String = 
    failEarlySucceededIndexes[GenMap[K, V]](xs, _.exists(_._2 == right), maxSucceed)
    
  def failEarlySucceededIndexesNotContainValue[K, V](xs: GenTraversable[GenMap[K, V]], right: V, maxSucceed: Int): String = 
    failEarlySucceededIndexes[GenMap[K, V]](xs, !_.exists(_._2 == right), maxSucceed)
    
  def failEarlySucceededIndexesJavaMapIsEmpty[K, V](xs: GenTraversable[java.util.Map[K, V]], right: Int = 0, maxSucceed: Int): String = // right is not used, but to be consistent to other so that easier for code generation
    failEarlySucceededIndexes[java.util.Map[K, V]](xs, _.isEmpty, maxSucceed)
    
  def failEarlySucceededIndexesJavaMapNotIsEmpty[K, V](xs: GenTraversable[java.util.Map[K, V]], right: Int = 0, maxSucceed: Int): String = // right is not used, but to be consistent to other so that easier for code generation
    failEarlySucceededIndexes[java.util.Map[K, V]](xs, !_.isEmpty, maxSucceed)
    
  def failEarlySucceededIndexesJavaMapContainKey[K, V](xs: GenTraversable[java.util.Map[K, V]], right: K, maxSucceed: Int): String = 
    failEarlySucceededIndexes[java.util.Map[K, V]](xs, _.containsKey(right), maxSucceed)
    
  def failEarlySucceededIndexesJavaMapNotContainKey[K, V](xs: GenTraversable[java.util.Map[K, V]], right: K, maxSucceed: Int): String = 
    failEarlySucceededIndexes[java.util.Map[K, V]](xs,  !_.containsKey(right), maxSucceed)
    
  def failEarlySucceededIndexesJavaMapContainValue[K, V](xs: GenTraversable[java.util.Map[K, V]], right: V, maxSucceed: Int): String = 
    failEarlySucceededIndexes[java.util.Map[K, V]](xs, _.containsValue(right), maxSucceed)
    
  def failEarlySucceededIndexesJavaMapNotContainValue[K, V](xs: GenTraversable[java.util.Map[K, V]], right: V, maxSucceed: Int): String = 
    failEarlySucceededIndexes[java.util.Map[K, V]](xs, !_.containsValue(right), maxSucceed)
    
  def failEarlySucceededIndexesJavaMapSizeEqual[K, V](xs: GenTraversable[java.util.Map[K, V]], right: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes[java.util.Map[K, V]](xs, _.size == right, maxSucceed)
    
  def failEarlySucceededIndexesJavaMapSizeNotEqual[K, V](xs: GenTraversable[java.util.Map[K, V]], right: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes[java.util.Map[K, V]](xs, _.size != right, maxSucceed)
    
  def failEarlySucceededIndexesJavaColSizeEqual[T](xs: GenTraversable[java.util.Collection[T]], right: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes[java.util.Collection[T]](xs, _.size == right, maxSucceed)
    
  def failEarlySucceededIndexesJavaColSizeNotEqual[T](xs: GenTraversable[java.util.Collection[T]], right: Int, maxSucceed: Int): String = 
    failEarlySucceededIndexes[java.util.Collection[T]](xs, _.size != right, maxSucceed)
    
  def failEarlySucceededIndexesJavaColContain[T](xs: GenTraversable[java.util.Collection[T]], right: T, maxSucceed: Int): String = 
    failEarlySucceededIndexes[java.util.Collection[T]](xs, _.contains(right), maxSucceed)
    
  def failEarlySucceededIndexesJavaColNotContain[T](xs: GenTraversable[java.util.Collection[T]], right: T, maxSucceed: Int): String = 
    failEarlySucceededIndexes[java.util.Collection[T]](xs, !_.contains(right), maxSucceed)
    
  def failEarlySucceededIndexesJavaColIsEmpty[T](xs: GenTraversable[java.util.Collection[T]], right: Int = 0, maxSucceed: Int): String = // right is not used, but to be consistent to other so that easier for code generation
    failEarlySucceededIndexes[java.util.Collection[T]](xs, _.isEmpty, maxSucceed)
    
  def failEarlySucceededIndexesJavaColNotIsEmpty[T](xs: GenTraversable[java.util.Collection[T]], right: Int = 0, maxSucceed: Int): String = // right is not used, but to be consistent to other so that easier for code generation
    failEarlySucceededIndexes[java.util.Collection[T]](xs, !_.isEmpty, maxSucceed)
  
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
  
  def javaMap[K, V](elements: (K, V)*): java.util.Map[K, V] = {
    val javaMap = new java.util.LinkedHashMap[K, V]()
    elements.foreach(t => javaMap.put(t._1, t._2))
    javaMap
  }
}

// Selfless trait pattern
object SharedHelpers extends SharedHelpers

