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

import org.scalatest._
import org.scalatest.events.Ordinal
import org.scalatest.events.SuiteStarting
import org.scalatest.events.SuiteAborted
import org.scalatest.events.SuiteCompleted
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import org.scalatest.events.TestIgnored
import org.scalatest.events.TestFailed
import org.scalatest.events.TestPending
import org.scalatest.events.TestCanceled
import org.scalatest.events.RecordableEvent
import org.scalatest.events.AlertProvided
import org.scalatest.events.NoteProvided
import org.scalatest.events.NameInfo

import java.io.File

class JUnitXmlReporterSuite extends FunSuite {

  val ord0 = new Ordinal(23)
  val ord0a = ord0.next
  val ord0b = ord0a.next
  
  val ord1 = new Ordinal(123)
  val ord1a = ord1.next
  val ord1b = ord1a.next
  val ord1c = ord1b.next
  val ord1d = ord1c.next

  val ord2 = new Ordinal(223)
  val ord2a = ord2.next
  val ord2b = ord2a.next
  val ord2c = ord2b.next
  val ord2d = ord2c.next
  val ord2e = ord2d.next
  val ord2f = ord2e.next
  val ord2g = ord2f.next
  val ord2h = ord2g.next
  val ord2i = ord2h.next
  val ord2j = ord2i.next
  val ord2k = ord2j.next
  val ord2l = ord2k.next
  val ord2m = ord2l.next
  val ord2n = ord2m.next
  val ord2o = ord2n.next

  val start1 =
    SuiteStarting(
      ord1a,
      "suite1",
      "suiteId1",
      None,
      None,
      None,
      None,
      None,
      "thread1",
      123123)

  val start2 =
    SuiteStarting(
      ord1b,
      "suite2",
      "suiteId2",
      None,
      None,
      None,
      None,
      None,
      "thread2",
      123223)

  val abort2 =
    SuiteAborted(
      ord1c,
      "aborted message",
      "suite2",
      "suiteId2",
      None,
      None,
      None,
      None,
      None,
      None)

  val complete1 =
    SuiteCompleted(
      ord1d,
      "suite1",
      "suiteId1",
      None,
      None,
      None,
      None,
      None,
      None,
      "thread1",
      123456)
      
  val start3 =
    SuiteStarting(
      ord2a,
      "suite3",
      "suiteId3",
      None,
      None,
      None,
      None,
      None,
      "thread1",
      123123)

  val startTest1 =
    TestStarting(
      ordinal = ord2d,
      suiteName = "suite3",
      suiteId = "suiteId3",
      suiteClassName = Some("Suite3Class"),
      testName = "a pass test",
      testText = "a pass test text")
      
  val endTest1 =
    TestSucceeded (
      ordinal = ord2g,
      suiteName = "suite3",
      suiteId = "suiteId3",
      suiteClassName = Some("Suite3Class"),
      testName = "a pass test",
      testText = "a pass test text",
      recordedEvents = Vector.empty[RecordableEvent])
      
  val ignoreTest1 =
    TestIgnored (
      ordinal = ord2h,
      suiteName = "suite3",
      suiteId = "suiteId3",
      suiteClassName = Some("Suite3Class"),
      testName = "an ignored test",
      testText = "an ignored test text")

  val startTest2 =
    TestStarting(
      ordinal = ord2i,
      suiteName = "suite3",
      suiteId = "suiteId3",
      suiteClassName = Some("Suite3Class"),
      testName = "a fail test",
      testText = "a fail test text")

  val failTest2 =
    TestFailed (
      ordinal = ord2j,
      message = "failTest2 message text",
      suiteName = "suite3",
      suiteId = "suiteId3",
      suiteClassName = Some("Suite3Class"),
      testName = "a fail test",
      testText = "a fail test text",
      recordedEvents = Vector.empty[RecordableEvent])

  val startTest3 =
    TestStarting(
      ordinal = ord2k,
      suiteName = "suite3",
      suiteId = "suiteId3",
      suiteClassName = Some("Suite3Class"),
      testName = "a pending test",
      testText = "a pending test text")

  val pendingTest3 =
    TestPending (
      ordinal = ord2l,
      suiteName = "suite3",
      suiteId = "suiteId3",
      suiteClassName = Some("Suite3Class"),
      testName = "a pending test",
      testText = "a pending test text",
      recordedEvents = Vector.empty[RecordableEvent])

  val startTest4 =
    TestStarting(
      ordinal = ord2m,
      suiteName = "suite3",
      suiteId = "suiteId3",
      suiteClassName = Some("Suite3Class"),
      testName = "a canceled test",
      testText = "a canceled test text")

  val canceledTest4 =
    TestCanceled (
      ordinal = ord2n,
      message = "bailed out",
      suiteName = "suite3",
      suiteId = "suiteId3",
      suiteClassName = Some("Suite3Class"),
      testName = "a canceled test",
      testText = "a canceled test text",
      recordedEvents = Vector.empty[RecordableEvent])

  val complete3 =
    SuiteCompleted(
      ord2o,
      "suite3",
      "suiteId3",
      None,
      None,
      None,
      None,
      None,
      None,
      "thread1",
      123456)
  
  val alertProvided1 = 
    AlertProvided(
      ord0a, 
      "This is an alert!", 
      None
    )
      
  val alertProvided2 = 
    AlertProvided(
      ord2c, 
      "This is an alert!", 
      Some(NameInfo("suite3", "suiteId3", None, None))
    )
    
  val alertProvided3 = 
    AlertProvided(
      ord2e, 
      "This is an alert!", 
      Some(NameInfo("suite3", "suiteId3", None, Some("a pass test")))
    )
  
  val noteProvided1 = 
    NoteProvided(
      ord0b, 
      "This is an update!", 
      None
    )
    
  val noteProvided2 = 
    NoteProvided(
      ord2d, 
      "This is an update!", 
      Some(NameInfo("suite3", "suiteId3", None, None))
    )
    
  val noteProvided3 = 
    NoteProvided(
      ord2f, 
      "This is an update!", 
      Some(NameInfo("suite3", "suiteId3", None, Some("a pass test")))
    )
      
  val reporter = new JUnitXmlReporter("target")

  /*test("SuiteAborted and SuiteCompleted are recognized as test terminators") {
    reporter(start1)
    reporter(start2)
    reporter(abort2)
    reporter(complete1)

    val file1 = new File("target/TEST-suiteId1.xml")
    val file2 = new File("target/TEST-suiteId2.xml")

    assert(file1.exists)
    assert(file2.exists)

    file1.delete
    file2.delete
  }

  test("test case gets reported") {
    reporter(start3)
    reporter(startTest1)
    reporter(endTest1)
    reporter(ignoreTest1)
    reporter(startTest2)
    reporter(failTest2)
    reporter(startTest3)
    reporter(pendingTest3)
    reporter(startTest4)
    reporter(canceledTest4)
    reporter(complete3)

    val loadnode = xml.XML.loadFile("target/TEST-suiteId3.xml")
    val testcases = loadnode \\ "testcase"

    val tcIgnored =
      testcases.find(tc => (tc \ "@name").toString == "an ignored test").get

    val tcFailed = 
      testcases.find(tc => (tc \ "@name").toString == "a fail test").get

    val tcPending = 
      testcases.find(tc => (tc \ "@name").toString == "a pending test").get

    val tcCanceled = 
      testcases.find(tc => (tc \ "@name").toString == "a canceled test").get

    assert(!(tcIgnored \ "skipped").isEmpty)
    assert(!(tcFailed \ "failure").isEmpty)
    assert(!(tcPending \ "skipped").isEmpty)
    assert(!(tcCanceled \ "skipped").isEmpty)
  }*/
  
  test("AlertProvided and NoteProvided should be ignored") {
    reporter(alertProvided1)
    reporter(noteProvided1)
    reporter(start3)
    reporter(alertProvided2)
    reporter(noteProvided2)
    reporter(startTest1)
    reporter(alertProvided3)
    reporter(noteProvided3)
    reporter(endTest1)
    reporter(ignoreTest1)
    reporter(startTest2)
    reporter(failTest2)
    reporter(startTest3)
    reporter(pendingTest3)
    reporter(startTest4)
    reporter(canceledTest4)
    reporter(complete3)

    val loadnode = xml.XML.loadFile("target/TEST-suiteId3.xml")
    val testcases = loadnode \\ "testcase"

    val tcIgnored =
      testcases.find(tc => (tc \ "@name").toString == "an ignored test").get

    val tcFailed = 
      testcases.find(tc => (tc \ "@name").toString == "a fail test").get

    val tcPending = 
      testcases.find(tc => (tc \ "@name").toString == "a pending test").get

    val tcCanceled = 
      testcases.find(tc => (tc \ "@name").toString == "a canceled test").get

    assert(!(tcIgnored \ "skipped").isEmpty)
    assert(!(tcFailed \ "failure").isEmpty)
    assert(!(tcPending \ "skipped").isEmpty)
    assert(!(tcCanceled \ "skipped").isEmpty)
  }

  /*test("testcase failure message xmlified properly"){
    //"" - not used parameters
    val bigFail = TestFailed(new Ordinal(0),
        "Unusually formed message: \n less:'<', amp:'&', double-quote:\"",
        "",
        "",
        None,
        "",
        "",
        null,
        Some(new Exception("Unusually formed exception: \n less:'<', more:'>' amp:'&', double-quote:\"")))

    val testsuite = reporter.Testsuite("TestSuite", 10L)
    testsuite.testcases += reporter.Testcase("TestCase", Some("someClass"), 1L)
    testsuite.testcases.foreach(tc => tc.failure = Some(bigFail))

    val rawXml:String = reporter.xmlify(testsuite)

    //correct xml, no exceptions are thrown
    val res= scala.xml.XML.loadString(rawXml)

    val message = (res \\ "failure" \ "@message").toString
    assert(message==="""Unusually formed message: &amp;#010; less:'&lt;', amp:'&amp;', double-quote:&quot;""","failure/@message is not as expected")
  }*/
}
