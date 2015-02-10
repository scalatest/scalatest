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
import java.net.ServerSocket
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.xml.XML
import scala.xml.Elem
import org.xml.sax.SAXException
import java.net.Socket
import scala.collection.mutable.ListBuffer
import org.scalatest.events.SuiteStarting
import org.scalatest.events.Ordinal
import org.scalatest.events.TopOfClass
import java.util.Date
import org.scalatest.events.SuiteCompleted
import org.scalatest.events.SuiteAborted
import org.scalatest.events.RunStarting
import org.scalatest.events.RunCompleted
import org.scalatest.events.Summary
import org.scalatest.events.RunStopped
import org.scalatest.events.RunAborted
import org.scalatest.events.InfoProvided
import org.scalatest.events.NameInfo
import org.scalatest.events.LineInFile
import org.scalatest.events.MarkupProvided
import org.scalatest.events.TestFailed
import org.scalatest.events.TestCanceled
import org.scalatest.events.RecordableEvent
import org.scalatest.concurrent.Eventually
import org.scalatest.time.Span
import org.scalatest.time.Seconds
import org.scalatest._
import SharedHelpers._

class XmlSocketReporterSpec extends FunSpec with Eventually {
  
  class SocketEventRecorder(socket: ServerSocket) extends Runnable {
    @volatile
    var stopped: Boolean = false
    private var connection: Socket = null
    private var in: BufferedReader = null
    @volatile
    var ready: Boolean = false
    
    var testStartingList = new ListBuffer[Elem]
    def testStartingEvents = testStartingList.toArray
    
    var testSucceededList = new ListBuffer[Elem]
    def testSucceededEvents = testSucceededList.toArray
    
    var testFailedList = new ListBuffer[Elem]
    def testFailedEvents = testFailedList.toArray
    
    var testIgnoredList = new ListBuffer[Elem]
    def testIgnoredEvents = testIgnoredList.toArray
    
    var testPendingList = new ListBuffer[Elem]
    def testPendingEvents = testPendingList.toArray
    
    var testCanceledList = new ListBuffer[Elem]
    def testCanceledEvents = testCanceledList.toArray
    
    var scopeOpenedList = new ListBuffer[Elem]
    def scopeOpenedEvents = scopeOpenedList.toArray
    
    var scopeClosedList = new ListBuffer[Elem]
    def scopeClosedEvents = scopeClosedList.toArray
    
    var scopePendingList = new ListBuffer[Elem]
    def scopePendingEvents = scopePendingList.toArray
    
    var suiteStartingList = new ListBuffer[Elem]
    def suiteStartingEvents = suiteStartingList.toArray
    
    var suiteCompletedList = new ListBuffer[Elem]
    def suiteCompletedEvents = suiteCompletedList.toArray
    
    var suiteAbortedList = new ListBuffer[Elem]
    def suiteAbortedEvents = suiteAbortedList.toArray
    
    var runStartingList = new ListBuffer[Elem]
    def runStartingEvents = runStartingList.toArray
    
    var runCompletedList = new ListBuffer[Elem]
    def runCompletedEvents = runCompletedList.toArray
    
    var runStoppedList = new ListBuffer[Elem]
    def runStoppedEvents = runStoppedList.toArray
    
    var runAbortedList = new ListBuffer[Elem]
    def runAbortedEvents = runAbortedList.toArray
    
    var infoProvidedList = new ListBuffer[Elem]
    def infoProvidedEvents = infoProvidedList.toArray
    
    var markupProvidedList = new ListBuffer[Elem]
    def markupProvidedEvents = markupProvidedList.toArray
    
    def run() {
      try {
        connection = socket.accept
        val inputStream = connection.getInputStream
        in = new BufferedReader(new InputStreamReader(inputStream))
        while (!connection.isClosed && (!stopped || in.ready)) {
          var eventRawXml = ""
          var endingXml = ""
          var eventXml: Elem = null
          while (eventXml == null && !connection.isClosed && (!stopped || in.ready)) {
            val line = in.readLine
            if (line != null) {
              if (eventRawXml == "" && line.length > 0)
                endingXml = line.substring(0, 1) + "/" + line.substring(1)
              eventRawXml += line + "\n"
              if (line.trim == endingXml.trim) 
                eventXml = XML.loadString(eventRawXml)
              else if (!connection.isClosed && !in.ready)
                Thread.sleep(10)
            }
            else if (!in.ready)
                Thread.sleep(10)
          }
          if (eventXml != null) {
            eventXml.label match {
              case "TestStarting" => testStartingList += eventXml
              case "TestSucceeded" => testSucceededList += eventXml
              case "TestFailed" => testFailedList += eventXml
              case "TestIgnored" => testIgnoredList += eventXml
              case "TestPending" => testPendingList += eventXml
              case "TestCanceled" => testCanceledList += eventXml
              case "ScopeOpened" => scopeOpenedList += eventXml
              case "ScopeClosed" => scopeClosedList += eventXml
              case "ScopePending" => scopePendingList += eventXml
              case "SuiteStarting" => suiteStartingList += eventXml
              case "SuiteCompleted" => suiteCompletedList += eventXml
              case "SuiteAborted" => suiteAbortedList += eventXml
              case "RunStarting" => runStartingList += eventXml
              case "RunCompleted" => runCompletedList += eventXml
              case "RunStopped" => runStoppedList += eventXml
              case "RunAborted" => runAbortedList += eventXml
              case "InfoProvided" => infoProvidedList += eventXml
              case "MarkupProvided" => markupProvidedList += eventXml
            }
          }
          if (!connection.isClosed && !in.ready)
            Thread.sleep(10)
        }
      }
      finally {
        in.close()
        connection.close()
        ready = true
      }
    }
  }
  
  def checkStringOption(value: String, expectedValueOpt: Option[String]) {
    expectedValueOpt match {
      case Some(expectedValue) => 
        assert(value === expectedValue)
      case None =>
        assert(value === "")
    }
  }
  
  def checkLongOption(value: Long, expectedValueOpt: Option[Long]) {
    expectedValueOpt match {
      case Some(expectedValue) => 
        assert(value === expectedValue)
      case None =>
        assert(value === "")
    }
  }
  
  def checkScopeEvents(scopeOpened: Elem, message: String, suiteName: String, suiteId: String, suiteClassName: Option[String], fileName: String, lineNumber: Int) {
    assert((scopeOpened \ "message").text === message )
    assert((scopeOpened \ "nameInfo" \ "suiteName").text === suiteName )
    assert((scopeOpened \ "nameInfo" \ "suiteId").text === suiteId)
    checkStringOption((scopeOpened \ "nameInfo" \ "suiteClassName").text, suiteClassName)
    assert((scopeOpened \ "location" \ "LineInFile" \ "fileName").text === fileName)
    assert((scopeOpened \ "location" \ "LineInFile" \ "lineNumber").text === lineNumber.toString)
  }
  
  def checkTestStarting(testStarting:Elem, suiteName: String, suiteId: String, suiteClassName: Option[String], 
                        testName: String, testText: String, fileName: String, lineNumber: Int, 
                        rerunner: Option[String]) {
    assert((testStarting \ "suiteName").text === suiteName)
    assert((testStarting \ "suiteId").text === suiteId)
    checkStringOption((testStarting \ "suiteClassName").text, suiteClassName)
    assert((testStarting \ "testName").text === testName)
    assert((testStarting \ "testText").text === testText)
    assert((testStarting \ "location" \ "LineInFile" \ "fileName").text === fileName)
    assert((testStarting \ "location" \ "LineInFile" \ "lineNumber").text === lineNumber.toString)
    checkStringOption((testStarting \ "rerunner").text, rerunner)
  }
  
  def checkTestSucceeded(testSucceeded: Elem, suiteName: String, suiteId: String, suiteClassName: Option[String], 
                         testName: String, testText: String, fileName: String, lineNumber: Int, rerunner: Option[String] = None) {
    assert((testSucceeded \ "suiteName").text === suiteName)
    assert((testSucceeded \ "suiteId").text === suiteId)
    checkStringOption((testSucceeded \ "suiteClassName").text, suiteClassName)
    assert((testSucceeded \ "testName").text === testName)
    assert((testSucceeded \ "testText").text === testText)
    assert((testSucceeded \ "location" \ "LineInFile" \ "fileName").text === fileName)
    assert((testSucceeded \ "location" \ "LineInFile" \ "lineNumber").text === lineNumber.toString)
    checkStringOption((testSucceeded \ "rerunner").text, rerunner)
  }
  
  def checkTestFailed(testFailed: Elem, message: String, suiteName: String, suiteId: String, suiteClassName: Option[String], 
                      testName: String, testText: String, rerunner: Option[String]) {
    assert((testFailed \ "message").text === message)
    assert((testFailed \ "suiteName").text === suiteName)
    assert((testFailed \ "suiteId").text === suiteId)
    checkStringOption((testFailed \ "suiteClassName").text, suiteClassName)
    assert((testFailed \ "testName").text === testName)
    assert((testFailed \ "testText").text === testText)
    checkStringOption((testFailed \ "rerunner").text, rerunner)
  }
  
  def checkTestPending(testPending: Elem, suiteName: String, suiteId: String, suiteClassName: Option[String], 
                       testName: String, testText: String, fileName: String, lineNumber: Int, 
                       rerunner: Option[String]) {
    assert((testPending \ "suiteName").text === suiteName)
    assert((testPending \ "suiteId").text === suiteId)
    checkStringOption((testPending \ "suiteClassName").text, suiteClassName)
    assert((testPending \ "testName").text === testName)
    assert((testPending \ "testText").text === testText)
    assert((testPending \ "location" \ "LineInFile" \ "fileName").text === fileName)
    assert((testPending \ "location" \ "LineInFile" \ "lineNumber").text === lineNumber.toString)
    checkStringOption((testPending \ "rerunner").text, rerunner)
  }
  
  def checkTestIgnored(testIgnored: Elem, suiteName: String, suiteId: String, suiteClassName: Option[String], 
                       testName: String, testText: String, fileName: String, lineNumber: Int) {
    assert((testIgnored \ "suiteName").text === suiteName)
    assert((testIgnored \ "suiteId").text === suiteId)
    checkStringOption((testIgnored \ "suiteClassName").text, suiteClassName)
    assert((testIgnored \ "testName").text === testName)
    assert((testIgnored \ "testText").text === testText)
    assert((testIgnored \ "location" \ "LineInFile" \ "fileName").text === fileName)
    assert((testIgnored \ "location" \ "LineInFile" \ "lineNumber").text === lineNumber.toString)
  }
  
  def checkTestCanceled(testCanceled: Elem, message: String, suiteName: String, suiteId: String, suiteClassName: Option[String], 
                        testName: String, testText: String, fileName: String, lineNumber: Int) {
    assert((testCanceled \ "message").text === message)
    assert((testCanceled \ "suiteName").text === suiteName)
    assert((testCanceled \ "suiteId").text === suiteId)
    checkStringOption((testCanceled \ "suiteClassName").text, suiteClassName)
    assert((testCanceled \ "testName").text === testName)
    assert((testCanceled \ "testText").text === testText)
    assert((testCanceled \ "location" \ "LineInFile" \ "fileName").text === fileName)
    assert((testCanceled \ "location" \ "LineInFile" \ "lineNumber").text === lineNumber.toString)
  }
  
  def checkSuiteStarting(suiteStarting: Elem, suiteName: String, suiteId: String, suiteClassName: Option[String], 
                         topOfClassName: String, rerunner: Option[String] = None, threadName: String, timeStamp: Long) {
    assert((suiteStarting \ "suiteName").text === suiteName)
    assert((suiteStarting \ "suiteId").text === suiteId)
    checkStringOption((suiteStarting \ "suiteClassName").text, suiteClassName)
    assert((suiteStarting \ "location" \ "TopOfClass" \ "className").text === topOfClassName)
    checkStringOption((suiteStarting \ "rerunner").text, rerunner)
    assert((suiteStarting \ "threadName").text === threadName)
    assert((suiteStarting \ "timeStamp").text === timeStamp.toString)
  }
  
  def checkSuiteCompleted(suiteCompleted: Elem, suiteName: String, suiteId: String, suiteClassName: Option[String], 
                          duration: Option[Long], topOfClassName: String, rerunner: Option[String], threadName: String, timeStamp: Long) {
    assert((suiteCompleted \ "suiteName").text === suiteName)
    assert((suiteCompleted \ "suiteId").text === suiteId)
    checkStringOption((suiteCompleted \ "suiteClassName").text, suiteClassName)
    checkLongOption((suiteCompleted \ "duration").text.toLong, duration)
    assert((suiteCompleted \ "location" \ "TopOfClass" \ "className").text === topOfClassName)
    checkStringOption((suiteCompleted \ "rerunner").text, rerunner)
    assert((suiteCompleted \ "threadName").text === threadName)
    assert((suiteCompleted \ "timeStamp").text === timeStamp.toString)
  }
  
  def checkSuiteAborted(suiteAborted: Elem, message: String, suiteName: String, suiteId: String, suiteClassName: Option[String],  
                        duration: Option[Long], topOfClassName: String, rerunner: Option[String], threadName: String, timeStamp: Long) {
    assert((suiteAborted \ "message").text === message)
    assert((suiteAborted \ "suiteName").text === suiteName)
    assert((suiteAborted \ "suiteId").text === suiteId)
    checkStringOption((suiteAborted \ "suiteClassName").text, suiteClassName)
    checkLongOption((suiteAborted \ "duration").text.toLong, duration)
    assert((suiteAborted \ "location" \ "TopOfClass" \ "className").text === topOfClassName)
    checkStringOption((suiteAborted \ "rerunner").text, rerunner)
    assert((suiteAborted \ "threadName").text === threadName)
    assert((suiteAborted \ "timeStamp").text === timeStamp.toString)
    
  }

  describe("Socket Reporter") {
    
    it("should send TestStarting, TestSucceeded, TestFailed, TestIgnored, TestPending, " +
       "TestCanceled, ScopeOpened, ScopeClosed and ScopePending event using socket") {
      
      class TestSpec extends FunSpec {
        describe("A Feature") {
          it("should succeed") {}
          it("should failed") { assert(1 === 2) }
          ignore("should ignored") {}
          it("should pending") { pending }
          it("should canceled") { cancel("cancel on purpose") }
        }
        describe("A Pending Feature") {
          pending
        }
      }
      
      val socket = new ServerSocket(0)
      val eventRecorder = new SocketEventRecorder(socket)
      val eventRecorderThread = new Thread(eventRecorder)
      eventRecorderThread.start()
      val spec = new TestSpec()
      val rep = new XmlSocketReporter("localhost", socket.getLocalPort)
      spec.run(None, Args(rep))
      eventRecorder.stopped = true
      rep.dispose()
      eventually(timeout(Span(30, Seconds))) { assert(eventRecorder.ready) } // Wait until the receiver is ready
      
        
      assert(eventRecorder.scopeOpenedEvents.length === 2)
      checkScopeEvents(eventRecorder.scopeOpenedEvents(0), "A Feature", spec.suiteName, spec.suiteId, 
                     Some(spec.getClass.getName), "XmlSocketReporterSpec.scala", thisLineNumber - 26)
      checkScopeEvents(eventRecorder.scopeOpenedEvents(1), "A Pending Feature", spec.suiteName, spec.suiteId, 
                     Some(spec.getClass.getName), "XmlSocketReporterSpec.scala", thisLineNumber - 21)
      assert(eventRecorder.scopeClosedEvents.length === 1)
      checkScopeEvents(eventRecorder.scopeClosedEvents(0), "A Feature", spec.suiteName, spec.suiteId, 
                     Some(spec.getClass.getName), "XmlSocketReporterSpec.scala", thisLineNumber - 31)
      assert(eventRecorder.scopePendingEvents.length === 1)
      checkScopeEvents(eventRecorder.scopePendingEvents(0), "A Pending Feature", spec.suiteName, spec.suiteId, 
                     Some(spec.getClass.getName), "XmlSocketReporterSpec.scala", thisLineNumber - 27)
                     
      assert(eventRecorder.testStartingEvents.length === 4)
      checkTestStarting(eventRecorder.testStartingEvents(0), spec.suiteName, spec.suiteId, Some(spec.getClass.getName), 
                        "A Feature should succeed", "should succeed", "XmlSocketReporterSpec.scala", thisLineNumber - 37, 
                        None) // rerunner should be none, as the suite is an inner class.
      checkTestStarting(eventRecorder.testStartingEvents(1), spec.suiteName, spec.suiteId, Some(spec.getClass.getName), 
                        "A Feature should failed", "should failed", "XmlSocketReporterSpec.scala", thisLineNumber - 39, 
                        None)
      checkTestStarting(eventRecorder.testStartingEvents(2), spec.suiteName, spec.suiteId, Some(spec.getClass.getName), 
                        "A Feature should pending", "should pending", "XmlSocketReporterSpec.scala", thisLineNumber - 40, 
                        None)
      checkTestStarting(eventRecorder.testStartingEvents(3), spec.suiteName, spec.suiteId, Some(spec.getClass.getName), 
                        "A Feature should canceled", "should canceled", "XmlSocketReporterSpec.scala", thisLineNumber - 42, 
                        None)
      
      assert(eventRecorder.testSucceededEvents.length === 1)
      checkTestSucceeded(eventRecorder.testSucceededEvents(0), spec.suiteName, spec.suiteId, Some(spec.getClass.getName), 
                         "A Feature should succeed", "should succeed", "XmlSocketReporterSpec.scala", thisLineNumber - 51, None)
      
      assert(eventRecorder.testFailedEvents.length === 1)
      checkTestFailed(eventRecorder.testFailedEvents(0), "1 did not equal 2", spec.suiteName, spec.suiteId, Some(spec.getClass.getName), 
                      "A Feature should failed", "should failed", None)

      assert(eventRecorder.testPendingEvents.length === 1)
      checkTestPending(eventRecorder.testPendingEvents(0), spec.suiteName, spec.suiteId, Some(spec.getClass.getName), 
                       "A Feature should pending", "should pending", "XmlSocketReporterSpec.scala", thisLineNumber - 56, None)

      assert(eventRecorder.testIgnoredEvents.length === 1)
      checkTestIgnored(eventRecorder.testIgnoredEvents(0), spec.suiteName, spec.suiteId, Some(spec.getClass.getName), 
                       "A Feature should ignored", "should ignored", "XmlSocketReporterSpec.scala", thisLineNumber - 61)
      
      assert(eventRecorder.testCanceledEvents.length === 1)
      checkTestCanceled(eventRecorder.testCanceledEvents(0), "cancel on purpose", spec.suiteName, spec.suiteId, Some(spec.getClass.getName), 
                        "A Feature should canceled", "should canceled", "XmlSocketReporterSpec.scala", thisLineNumber - 63)        
    }
    
    it("should send SuiteStarting, SuiteCompleted, SuiteAborted event using socket") {
      val socket = new ServerSocket(0)
      val eventRecorder = new SocketEventRecorder(socket)
      val eventRecorderThread = new Thread(eventRecorder)
      eventRecorderThread.start()
      
      val timeStamp = (new Date).getTime
      
      val rep = new XmlSocketReporter("localhost", socket.getLocalPort)
      rep(SuiteStarting(new Ordinal(0), "a suite name", "a suite Id", Some("a class name"), None, Some(TopOfClass("a class name")), Some("a rerunner"),
                        None, Thread.currentThread.getName, timeStamp))
      rep(SuiteCompleted(new Ordinal(0), "a suite name", "a suite Id", Some("a class name"), Some(1000L), None, Some(TopOfClass("a class name")),
                         Some("a rerunner"), None, Thread.currentThread.getName, timeStamp))
      rep(SuiteStarting(new Ordinal(0), "another suite name", "another suite Id", Some("another class name"), None, Some(TopOfClass("another class name")), Some("another rerunner"),
                        None, Thread.currentThread.getName, timeStamp))
      rep(SuiteAborted(new Ordinal(0), "error message", "another suite name", "another suite Id", Some("another class name"), Some(new Throwable("purposely error")),
                       Some(1000L), None, Some(TopOfClass("another class name")), Some("another rerunner"), None, Thread.currentThread.getName, timeStamp))
      rep.dispose()
      eventRecorder.stopped = true
      eventually(timeout(Span(10, Seconds))) { assert(eventRecorder.ready) } // Wait until the receiver is ready
      
      
      assert(eventRecorder.suiteStartingEvents.length === 2)
      checkSuiteStarting(eventRecorder.suiteStartingEvents(0), "a suite name", "a suite Id", Some("a class name"), "a class name", Some("a rerunner"), Thread.currentThread.getName, timeStamp)
      checkSuiteStarting(eventRecorder.suiteStartingEvents(1), "another suite name", "another suite Id", Some("another class name"), "another class name", Some("another rerunner"), Thread.currentThread.getName, timeStamp)
      
      assert(eventRecorder.suiteCompletedEvents.length === 1)
      checkSuiteCompleted(eventRecorder.suiteCompletedEvents(0), "a suite name", "a suite Id", Some("a class name"), Some(1000L), "a class name", Some("a rerunner"), Thread.currentThread.getName, timeStamp)
      
      assert(eventRecorder.suiteAbortedEvents.length === 1)
      checkSuiteAborted(eventRecorder.suiteAbortedEvents(0), "error message", "another suite name", "another suite Id", Some("another class name"), Some(1000L), "another class name", Some("another rerunner"), 
                        Thread.currentThread.getName, timeStamp)
    }
    
    it("should send RunStarting, RunCompleted, RunStopped and RunAborted event using socket") {
      val socket = new ServerSocket(0)
      val eventRecorder = new SocketEventRecorder(socket)
      val eventRecorderThread = new Thread(eventRecorder)
      eventRecorderThread.start()
      
      val timeStamp = (new Date).getTime
      
      val rep = new XmlSocketReporter("localhost", socket.getLocalPort)
      rep(RunStarting(new Ordinal(0), 10, ConfigMap("key 1" -> "value 1"), None, None, None, Thread.currentThread.getName, timeStamp))
      rep(RunCompleted(new Ordinal(0), Some(1000L), Some(Summary(1, 2, 3, 4, 5, 6, 7, 8)), None, None, None, Thread.currentThread.getName, timeStamp))
      rep(RunStopped(new Ordinal(0), Some(1000L), Some(Summary(8, 9, 10, 11, 12, 13, 14, 15)), None, None, None, Thread.currentThread.getName, timeStamp))
      rep(RunAborted(new Ordinal(0), "Suite aborted", Some(new Throwable("error!")), Some(1000L), Some(Summary(15, 16, 17, 18, 19, 20, 21, 22)), None,
                     None, None, Thread.currentThread.getName, timeStamp))
      rep.dispose()
      eventRecorder.stopped = true
      eventually(timeout(Span(10, Seconds))) { assert(eventRecorder.ready) } // Wait until the receiver is ready
      
      
      assert(eventRecorder.runStartingEvents.length === 1)
      assert((eventRecorder.runStartingEvents(0) \ "testCount").text === "10")
      assert((eventRecorder.runStartingEvents(0) \ "configMap" \ "entry" \ "key").text === "key 1")
      assert((eventRecorder.runStartingEvents(0) \ "configMap" \ "entry" \ "value").text === "value 1")
      assert((eventRecorder.runStartingEvents(0) \ "threadName").text === Thread.currentThread.getName)
      assert((eventRecorder.runStartingEvents(0) \ "timeStamp").text === timeStamp.toString)
      
      assert(eventRecorder.runCompletedEvents.length === 1)
      assert((eventRecorder.runCompletedEvents(0) \ "duration").text.toLong === 1000L)
      assert((eventRecorder.runCompletedEvents(0) \ "summary" \ "testsSucceededCount").text.toInt === 1)
      assert((eventRecorder.runCompletedEvents(0) \ "summary" \ "testsFailedCount").text.toInt === 2)
      assert((eventRecorder.runCompletedEvents(0) \ "summary" \ "testsIgnoredCount").text.toInt === 3)
      assert((eventRecorder.runCompletedEvents(0) \ "summary" \ "testsPendingCount").text.toInt === 4)
      assert((eventRecorder.runCompletedEvents(0) \ "summary" \ "testsCanceledCount").text.toInt === 5)
      assert((eventRecorder.runCompletedEvents(0) \ "summary" \ "suitesCompletedCount").text.toInt === 6)
      assert((eventRecorder.runCompletedEvents(0) \ "summary" \ "suitesAbortedCount").text.toInt === 7)
      assert((eventRecorder.runCompletedEvents(0) \ "summary" \ "scopesPendingCount").text.toInt === 8)
      assert((eventRecorder.runCompletedEvents(0) \ "threadName").text === Thread.currentThread.getName)
      assert((eventRecorder.runCompletedEvents(0) \ "timeStamp").text === timeStamp.toString)
      
      assert(eventRecorder.runStoppedEvents.length === 1)
      assert((eventRecorder.runStoppedEvents(0) \ "duration").text.toLong === 1000L)
      assert((eventRecorder.runStoppedEvents(0) \ "summary" \ "testsSucceededCount").text.toInt === 8)
      assert((eventRecorder.runStoppedEvents(0) \ "summary" \ "testsFailedCount").text.toInt === 9)
      assert((eventRecorder.runStoppedEvents(0) \ "summary" \ "testsIgnoredCount").text.toInt === 10)
      assert((eventRecorder.runStoppedEvents(0) \ "summary" \ "testsPendingCount").text.toInt === 11)
      assert((eventRecorder.runStoppedEvents(0) \ "summary" \ "testsCanceledCount").text.toInt === 12)
      assert((eventRecorder.runStoppedEvents(0) \ "summary" \ "suitesCompletedCount").text.toInt === 13)
      assert((eventRecorder.runStoppedEvents(0) \ "summary" \ "suitesAbortedCount").text.toInt === 14)
      assert((eventRecorder.runStoppedEvents(0) \ "summary" \ "scopesPendingCount").text.toInt === 15)
      assert((eventRecorder.runStoppedEvents(0) \ "threadName").text === Thread.currentThread.getName)
      assert((eventRecorder.runStoppedEvents(0) \ "timeStamp").text === timeStamp.toString)
      
      assert(eventRecorder.runAbortedEvents.length === 1)
      assert((eventRecorder.runAbortedEvents(0) \ "message").text === "Suite aborted")
      assert((eventRecorder.runAbortedEvents(0) \ "duration").text.toLong === 1000L)
      assert((eventRecorder.runAbortedEvents(0) \ "summary" \ "testsSucceededCount").text.toInt === 15)
      assert((eventRecorder.runAbortedEvents(0) \ "summary" \ "testsFailedCount").text.toInt === 16)
      assert((eventRecorder.runAbortedEvents(0) \ "summary" \ "testsIgnoredCount").text.toInt === 17)
      assert((eventRecorder.runAbortedEvents(0) \ "summary" \ "testsPendingCount").text.toInt === 18)
      assert((eventRecorder.runAbortedEvents(0) \ "summary" \ "testsCanceledCount").text.toInt === 19)
      assert((eventRecorder.runAbortedEvents(0) \ "summary" \ "suitesCompletedCount").text.toInt === 20)
      assert((eventRecorder.runAbortedEvents(0) \ "summary" \ "suitesAbortedCount").text.toInt === 21)
      assert((eventRecorder.runAbortedEvents(0) \ "summary" \ "scopesPendingCount").text.toInt === 22)
      assert((eventRecorder.runAbortedEvents(0) \ "threadName").text === Thread.currentThread.getName)
      assert((eventRecorder.runAbortedEvents(0) \ "timeStamp").text === timeStamp.toString)
    }
    
    it("should send InfoProvided and MarkupProvided event using socket") {
      val socket = new ServerSocket(0)
      val eventRecorder = new SocketEventRecorder(socket)
      val eventRecorderThread = new Thread(eventRecorder)
      eventRecorderThread.start()
      
      val timeStamp = (new Date).getTime
      
      val rep = new XmlSocketReporter("localhost", socket.getLocalPort)
      rep(InfoProvided(new Ordinal(0), "some info", Some(NameInfo("a suite name", "a suite Id", Some("a suite class"), Some("a test name"))),
          None, None, Some(LineInFile(168, "XmlSocketReporterSpec.scala")), None, Thread.currentThread.getName, timeStamp))
      rep(MarkupProvided(new Ordinal(0), "some markup", Some(NameInfo("another suite name", "another suite Id", Some("another suite class"), Some("another test name"))),
          None, Some(LineInFile(188, "XmlSocketReporterSpec.scala")), None, Thread.currentThread.getName, timeStamp))
      rep.dispose()
      eventRecorder.stopped = true
      eventually(timeout(Span(10, Seconds))) { assert(eventRecorder.ready) } // Wait until the receiver is ready
        
      
      assert(eventRecorder.infoProvidedEvents.length === 1)
      assert((eventRecorder.infoProvidedEvents(0) \ "message").text === "some info")
      assert((eventRecorder.infoProvidedEvents(0) \ "nameInfo" \ "suiteName").text === "a suite name")
      assert((eventRecorder.infoProvidedEvents(0) \ "nameInfo" \ "suiteId").text === "a suite Id")
      assert((eventRecorder.infoProvidedEvents(0) \ "nameInfo" \ "suiteClassName").text === "a suite class")
      assert((eventRecorder.infoProvidedEvents(0) \ "nameInfo" \ "testName").text === "a test name")
      assert((eventRecorder.infoProvidedEvents(0) \ "location" \ "LineInFile" \ "lineNumber").text.toInt === 168)
      assert((eventRecorder.infoProvidedEvents(0) \ "location" \ "LineInFile" \ "fileName").text === "XmlSocketReporterSpec.scala")
      assert((eventRecorder.infoProvidedEvents(0) \ "threadName").text === Thread.currentThread.getName)
      assert((eventRecorder.infoProvidedEvents(0) \ "timeStamp").text === timeStamp.toString)
      
      assert(eventRecorder.markupProvidedEvents.length === 1)
      assert((eventRecorder.markupProvidedEvents(0) \ "text").text === "some markup")
      assert((eventRecorder.markupProvidedEvents(0) \ "nameInfo" \ "suiteName").text === "another suite name")
      assert((eventRecorder.markupProvidedEvents(0) \ "nameInfo" \ "suiteId").text === "another suite Id")
      assert((eventRecorder.markupProvidedEvents(0) \ "nameInfo" \ "suiteClassName").text === "another suite class")
      assert((eventRecorder.markupProvidedEvents(0) \ "nameInfo" \ "testName").text === "another test name")
      assert((eventRecorder.markupProvidedEvents(0) \ "location" \ "LineInFile" \ "lineNumber").text.toInt === 188)
      assert((eventRecorder.markupProvidedEvents(0) \ "location" \ "LineInFile" \ "fileName").text === "XmlSocketReporterSpec.scala")
      assert((eventRecorder.markupProvidedEvents(0) \ "threadName").text === Thread.currentThread.getName)
      assert((eventRecorder.markupProvidedEvents(0) \ "timeStamp").text === timeStamp.toString)
    }
    
    it("should send multilines indented error message correctly") {
      
      //forAll failed, because: 
        //at index 0, forAll failed, because: 
      val socket = new ServerSocket(0)
      val eventRecorder = new SocketEventRecorder(socket)
      val eventRecorderThread = new Thread(eventRecorder)
      eventRecorderThread.start()
      
      val timeStamp = (new Date).getTime
      
      val rep = new XmlSocketReporter("localhost", socket.getLocalPort)
      rep(TestFailed(new Ordinal(0), "test <function1> failed, because: \n  <function1> is buggy.", "TestSuite", "com.test.TestSuite", Some("com.test.TestSuite"),
                     "test 1", "test 1", collection.immutable.IndexedSeq.empty[RecordableEvent], Some(new RuntimeException("test <function1> failed, because: \n  <function1> is buggy."))))
      rep(TestCanceled(new Ordinal(0), "test <function1> canceled, because: \n  <function1> is buggy.", "TestSuite", "com.test.TestSuite", Some("com.test.TestSuite"),
                       "test 2", "test 2", collection.immutable.IndexedSeq.empty[RecordableEvent], Some(new RuntimeException("test <function1> canceled, because: \n  <function1> is buggy."))))
      rep(InfoProvided(new Ordinal(0), "some <function1> info, because: \n  <function1> is buggy.", Some(NameInfo("TestSuite", "com.test.TestSuite", Some("com.test.TestSuite"), Some("test 2"))),
          Some(new RuntimeException("some <function1> info, because: \n  <function1> is buggy.")), None, Some(LineInFile(168, "XmlSocketReporterSpec.scala")), None, Thread.currentThread.getName, timeStamp))
      eventRecorder.stopped = true
      eventually(timeout(Span(10, Seconds))) { assert(eventRecorder.ready) } // Wait until the receiver is ready             
      
      assert(eventRecorder.testFailedEvents.length === 1)
      val testFailed = eventRecorder.testFailedEvents(0)
      assert((testFailed \ "message").text === "test <function1> failed, because: \n  <function1> is buggy.") 
      assert((testFailed \ "throwable" \ "message").text === "test <function1> failed, because: \n  <function1> is buggy.")
      
      assert(eventRecorder.testCanceledEvents.length === 1)
      val testCanceled = eventRecorder.testCanceledEvents(0)
      assert((testCanceled \ "message").text === "test <function1> canceled, because: \n  <function1> is buggy.")
      assert((testCanceled \ "throwable" \ "message").text === "test <function1> canceled, because: \n  <function1> is buggy.")
      
      assert(eventRecorder.infoProvidedEvents.length === 1)
      val infoProvided = eventRecorder.infoProvidedEvents(0)
      assert((infoProvided \ "message").text === "some <function1> info, because: \n  <function1> is buggy.")
      assert((infoProvided \ "throwable" \ "message").text === "some <function1> info, because: \n  <function1> is buggy.")
    }
  }
}
