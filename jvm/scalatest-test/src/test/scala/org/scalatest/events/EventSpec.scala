/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalatest.events

import org.scalatest._
import SharedHelpers.EventRecordingReporter
import examples._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EventSpec extends AnyFunSpec {
/*
  describe("An TestStarting Event") {
    describe("(with different runStamps)") {
      it("should sort into order by runStamp") {
        check(
          (runStampA: Int, runStampB: Int, suiteStamp: List[Int], testStamp: Int, ordinal: Int) =>
            (runStampA != runStampB) ==> {
              val unsorted =
                List(
                  TestStarting("X - test", "X", Some("com.acme.X"), "test", runStampA, suiteStamp, testStamp, ordinal),
                  TestStarting("Y - test", "Y", Some("com.acme.Y"), "test", runStampB, suiteStamp, testStamp, ordinal)
                )
              val sorted = unsorted.sort(_ < _)
              sorted.head.runStamp < sorted.tail.head.runStamp
            }
        )
      }
    }

    describe("(with same runStamps but different suiteStamps of the same length)") {
      it("should sort into order by suiteStamp") {
        check(
          (runStamp: Int, suiteStampA: List[Int], suiteStampB: List[Int], testStamp: Int, ordinal: Int) =>
            (suiteStampA != suiteStampB && !suiteStampA.isEmpty && !suiteStampB.isEmpty) ==> {
              val length = if (suiteStampA.length > suiteStampB.length) suiteStampB.length else suiteStampA.length
              val properLengthA = if (suiteStampA.length != length) suiteStampA take length else suiteStampA
              val properLengthB = if (suiteStampB.length != length) suiteStampB take length else suiteStampB
              val unequalA = if (properLengthA == properLengthB) (properLengthA.head + 1) :: properLengthB.tail else properLengthA
              val unsorted =
                List(
                  TestStarting("X - test", "X", Some("com.acme.X"), "test", runStamp, unequalA, testStamp, ordinal),
                  TestStarting("Y - test", "Y", Some("com.acme.Y"), "test", runStamp, properLengthB, testStamp, ordinal)
                )
              val sorted = unsorted.sort(_ < _)
              val first = sorted.head.suiteStamp.get.reverse
              val second = sorted.tail.head.suiteStamp.get.reverse
              val zipped = first zip second
              val unequalPair = zipped find (pair => pair._1 != pair._2)
              unequalPair match {
                case Some((firstElement, secondElement)) => firstElement < secondElement
                case None => fail() // should never happen
              }
            }
        )
      }
    }

    describe("(with same runStamps, suiteStamps, but different ordinals)") {
      it("should sort into order by ordinal") {
        check(
          (runStamp: Int, suiteStamp: List[Int], testStamp: Int, ordinalA: Int, ordinalB: Int) =>
            (ordinalA != ordinalB) ==> {
              val length = if (suiteStampA.length > suiteStampB.length) suiteStampB.length else suiteStampA.length
              val properLengthA = if (suiteStampA.length != length) suiteStampA take length else suiteStampA
              val properLengthB = if (suiteStampB.length != length) suiteStampB take length else suiteStampB
              val unequalA = if (properLengthA == properLengthB) (properLengthA.head + 1) :: properLengthB.tail else properLengthA
              val unsorted =
                List(
                  TestStarting("X - test", "X", Some("com.acme.X"), "test", runStamp, unequalA, testStamp, ordinal),
                  TestStarting("Y - test", "Y", Some("com.acme.Y"), "test", runStamp, properLengthB, testStamp, ordinal)
                )
              val sorted = unsorted.sort(_ < _)
              val first = sorted.head.suiteStamp.get.reverse
              val second = sorted.tail.head.suiteStamp.get.reverse
              val zipped = first zip second
              val unequalPair = zipped find (pair => pair._1 != pair._2)
              unequalPair match {
                case Some((firstElement, secondElement)) => firstElement < secondElement
                case None => fail() // should never happen
              }
            }
        )
      }
    }
  }
*/

// SKIP-SCALATESTNATIVE-START

  /*def getField[T](json: Json, key: String)(implicit d: Decoder[T]): T =
    json.hcursor.downField(key).as[T] match {
      case Right(t) => t
      case _ => fail(s"failed to decode $key from: $json")
    }*/

  import JSON._

  describe("TestStaring event") {

    it("should return correct JSON in its toJson method") {
      val event = TestStarting(new Ordinal(0), "testSuiteName", "testSuiteId", Some("testSuiteClassName"), "test name", "test text", Some(MotionToSuppress), Some(LineInFile(159, "File.scala", None)), Some("rerunner name"))
      val jsObj = parseJson(event.toJson)
      assert((jsObj \ "eventType").get == JsStr("TestStarting"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "suiteName").get == JsStr("testSuiteName"))
      assert((jsObj \ "suiteId").get == JsStr("testSuiteId"))
      assert((jsObj \ "suiteClassName").get == JsStr("testSuiteClassName"))
      assert((jsObj \ "testName").get == JsStr("test name"))
      assert((jsObj \ "testText").get == JsStr("test text"))

      val formatterObj = (jsObj \ "formatter").get
      assert((formatterObj \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationObj = (jsObj \ "location").get
      assert((locationObj \ "locationType").get == JsStr("LineInFile"))
      assert((locationObj \ "lineNumber").get == JsNum(159))
      assert((locationObj \ "fileName").get == JsStr("File.scala"))
      assert((locationObj \ "filePathname").get == JsNULL)

      assert((jsObj \ "rerunner").get == JsStr("rerunner name"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("TestSucceeded event") {

    it("should return correct JSON in its toJson method") {
      val event =
        TestSucceeded(
          new Ordinal(0),
          "testSuiteName",
          "testSuiteId",
          Some("testSuiteClassName"),
          "test name",
          "test text",
          Vector(
            InfoProvided(
              new Ordinal(1),
              "success",
              Some(NameInfo("testSuiteName", "testSuiteId", Some("testSuiteClassName"), Some("test name"))),
              Some(new RuntimeException("testing")),
              Some(MotionToSuppress),
              Some(LineInFile(123, "Test.scala", None))
            )
          ),
          Some(777),
          Some(IndentedText("formatted text", "raw text", 2)),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala"))),
          Some("rerunner name"),
          None
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("TestSucceeded"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "suiteName").get == JsStr("testSuiteName"))
      assert((jsObj \ "suiteId").get == JsStr("testSuiteId"))
      assert((jsObj \ "suiteClassName").get == JsStr("testSuiteClassName"))
      assert((jsObj \ "testName").get == JsStr("test name"))
      assert((jsObj \ "testText").get == JsStr("test text"))

      (jsObj \ "recordedEvents").get match {
        case recordedEvents: JsArr =>
          assert(recordedEvents.value.length == 1)
          val infoProvidedJson = recordedEvents.value(0)
          assert((infoProvidedJson \ "eventType").get == JsStr("InfoProvided"))
          assert((infoProvidedJson \ "ordinal").get == JsNum(1))
          assert((infoProvidedJson \ "message").get == JsStr("success"))

          val ipNameInfoJson = (infoProvidedJson \ "nameInfo").get
          assert((ipNameInfoJson \ "suiteName").get == JsStr("testSuiteName"))
          assert((ipNameInfoJson \ "suiteId").get == JsStr("testSuiteId"))
          assert((ipNameInfoJson \ "suiteClassName").get == JsStr("testSuiteClassName"))
          assert((ipNameInfoJson \ "testName").get == JsStr("test name"))

          val ipThrowableJson = (infoProvidedJson \ "throwable").get
          assert((ipThrowableJson \ "className").get == JsStr("java.lang.RuntimeException"))
          assert((ipThrowableJson \ "message").get == JsStr("testing"))

          val ipFormatterJson = (infoProvidedJson \ "formatter").get
          assert((ipFormatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

          val ipLocationJson = (infoProvidedJson \ "location").get
          assert((ipLocationJson \ "lineNumber").get == JsNum(123))
          assert((ipLocationJson \ "fileName").get == JsStr("Test.scala"))
          assert((ipLocationJson \ "filePathname").get == JsNULL)

        case other => fail("Expected JsArray but got: " + other)
      }

      assert((jsObj \ "duration").get == JsNum(777))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("IndentedText"))
      assert((formatterJson \ "formattedText").get == JsStr("formatted text"))
      assert((formatterJson \ "rawText").get == JsStr("raw text"))
      assert((formatterJson \ "indentationLevel").get == JsNum(2))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "rerunner").get == JsStr("rerunner name"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("A TestFailed event") {

    it("should return correct JSON in its toJson method") {
      val event =
        TestFailed(
          new Ordinal(0),
          "error message",
          "testSuiteName",
          "testSuiteId",
          Some("testSuiteClassName"),
          "test name",
          "test text",
          Vector(
            MarkupProvided(
              new Ordinal(1),
              "<b>success</b>",
              Some(NameInfo("testSuiteName", "testSuiteId", Some("testSuiteClassName"), Some("test name"))),
              Some(IndentedText("formatted text", "raw text", 2)),
              Some(LineInFile(456, "Test.scala", None))
            )
          ),
          Vector.empty,
          Some(new RuntimeException("fail message here")),
          Some(555),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala"))),
          Some("rerunner name"),
          None
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("TestFailed"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "suiteName").get == JsStr("testSuiteName"))
      assert((jsObj \ "suiteId").get == JsStr("testSuiteId"))
      assert((jsObj \ "suiteClassName").get == JsStr("testSuiteClassName"))
      assert((jsObj \ "testName").get == JsStr("test name"))
      assert((jsObj \ "testText").get == JsStr("test text"))

      assert((jsObj \ "message").get == JsStr("error message"))

      (jsObj \ "recordedEvents").get match {
        case recordedEvents: JsArr =>
          assert(recordedEvents.value.length == 1)
          val markupProvidedJson = recordedEvents.value(0)
          assert((markupProvidedJson \ "eventType").get == JsStr("MarkupProvided"))
          assert((markupProvidedJson \ "ordinal").get == JsNum(1))
          assert((markupProvidedJson \ "text").get == JsStr("<b>success</b>"))

          val mpNameInfoJson = (markupProvidedJson \ "nameInfo").get
          assert((mpNameInfoJson \ "suiteName").get == JsStr("testSuiteName"))
          assert((mpNameInfoJson \ "suiteId").get == JsStr("testSuiteId"))
          assert((mpNameInfoJson \ "suiteClassName").get == JsStr("testSuiteClassName"))
          assert((mpNameInfoJson \ "testName").get == JsStr("test name"))

          val mpFormatterJson = (markupProvidedJson \ "formatter").get
          assert((mpFormatterJson \ "formatterType").get == JsStr("IndentedText"))
          assert((mpFormatterJson \ "formattedText").get == JsStr("formatted text"))
          assert((mpFormatterJson \ "rawText").get == JsStr("raw text"))
          assert((mpFormatterJson \ "indentationLevel").get == JsNum(2))

          val mpLocationJson = (markupProvidedJson \ "location").get
          assert((mpLocationJson \ "locationType").get == JsStr("LineInFile"))
          assert((mpLocationJson \ "lineNumber").get == JsNum(456))
          assert((mpLocationJson \ "fileName").get == JsStr("Test.scala"))
          assert((mpLocationJson \ "filePathname").get == JsNULL)

        case other => fail("Expected JsArray but got: " + other)
      }

      val throwableJson = (jsObj \ "throwable").get
      assert((throwableJson \ "className").get == JsStr("java.lang.RuntimeException"))
      assert((throwableJson \ "message").get == JsStr("fail message here"))

      assert((jsObj \ "duration").get == JsNum(555))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "rerunner").get == JsStr("rerunner name"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }

    it("should carry correct differences value when used with s1 shouldEqual s2 syntax") {
      class ExampleSpec extends AnyFunSuite with Matchers {
        test("test") {
          "s1" shouldEqual "s2"
        }
      }
      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      suite.run(None, Args(rep))
      val failedEvents = rep.testFailedEventsReceived
      assert(failedEvents.length == 1)
      val analysis = failedEvents(0).analysis
      assert(analysis.length == 1)
      assert(analysis(0) == "\"s[1]\" -> \"s[2]\"")
    }

    it("should carry correct differences value when used with s1 should equal s2 syntax") {
      class ExampleSpec extends AnyFunSuite with Matchers {
        test("test") {
          "s1" should equal ("s2")
        }
      }
      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      suite.run(None, Args(rep))
      val failedEvents = rep.testFailedEventsReceived
      assert(failedEvents.length == 1)
      val analysis = failedEvents(0).analysis
      assert(analysis.length == 1)
      assert(analysis(0) == "\"s[1]\" -> \"s[2]\"")
    }

    it("should carry correct differences value when used with all(s1) shouldEqual s2 syntax") {
      class ExampleSpec extends AnyFunSuite with Matchers {
        test("test") {
          all(List("s1")) shouldEqual "s2"
        }
      }
      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      suite.run(None, Args(rep))
      val failedEvents = rep.testFailedEventsReceived
      assert(failedEvents.length == 1)
      val analysis = failedEvents(0).analysis
      assert(analysis.length == 1)
      assert(analysis(0) == "\"s[1]\" -> \"s[2]\"")
    }

    it("should carry correct differences value when used with all(s1) should equal s2 syntax") {
      class ExampleSpec extends AnyFunSuite with Matchers {
        test("test") {
          all(List("s1")) should equal ("s2")
        }
      }
      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      suite.run(None, Args(rep))
      val failedEvents = rep.testFailedEventsReceived
      assert(failedEvents.length == 1)
      val analysis = failedEvents(0).analysis
      assert(analysis.length == 1)
      assert(analysis(0) == "\"s[1]\" -> \"s[2]\"")
    }

  }

  describe("TestIgnored event") {

    it("should return correct JSON in its toJson method") {
      val event =
        TestIgnored(
          new Ordinal(0),
          "testSuiteName",
          "testSuiteId",
          Some("testSuiteClassName"),
          "test name",
          "test text",
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("TestIgnored"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "suiteName").get == JsStr("testSuiteName"))
      assert((jsObj \ "suiteId").get == JsStr("testSuiteId"))
      assert((jsObj \ "suiteClassName").get == JsStr("testSuiteClassName"))
      assert((jsObj \ "testName").get == JsStr("test name"))
      assert((jsObj \ "testText").get == JsStr("test text"))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("A TestCanceled event") {
    it("should carry suite class name as its rerunner when it is fired from top level suite") {
      val rep = new EventRecordingReporter
      val suite = new ExampleCancelSpec
      suite.run(None, Args(rep))
      val canceledEvents = rep.testCanceledEventsReceived
      assert(canceledEvents.length === 1)
      assert(canceledEvents(0).rerunner === Some(suite.getClass.getName))
    }

    it("should carry top level suite class name as its rerunner when it is fired from nested suites") {
      val rep = new EventRecordingReporter
      val suite = new ExampleCancelInNestedSuite
      suite.run(None, Args(rep))
      val canceledEvents = rep.testCanceledEventsReceived
      assert(canceledEvents.length === 1)
      assert(canceledEvents(0).rerunner === Some(classOf[ExampleCancelSpec].getName))
    }

    it("should return correct JSON in its toJson method") {
      val event =
        TestCanceled(
          new Ordinal(0),
          "error message",
          "testSuiteName",
          "testSuiteId",
          Some("testSuiteClassName"),
          "test name",
          "test text",
          Vector(
            MarkupProvided(
              new Ordinal(1),
              "<b>cancel</b>",
              Some(NameInfo("testSuiteName", "testSuiteId", Some("testSuiteClassName"), Some("test name"))),
              Some(IndentedText("formatted text", "raw text", 2)),
              Some(LineInFile(456, "Test.scala", None))
            )
          ),
          Some(new RuntimeException("cancel message here")),
          Some(555),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala"))),
          Some("rerunner name"),
          None
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("TestCanceled"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "suiteName").get == JsStr("testSuiteName"))
      assert((jsObj \ "suiteId").get == JsStr("testSuiteId"))
      assert((jsObj \ "suiteClassName").get == JsStr("testSuiteClassName"))
      assert((jsObj \ "testName").get == JsStr("test name"))
      assert((jsObj \ "testText").get == JsStr("test text"))

      assert((jsObj \ "message").get == JsStr("error message"))

      (jsObj \ "recordedEvents").get match {
        case recordedEvents: JsArr =>
          assert(recordedEvents.value.length == 1)
          val markupProvidedJson = recordedEvents.value(0)
          assert((markupProvidedJson \ "eventType").get == JsStr("MarkupProvided"))
          assert((markupProvidedJson \ "ordinal").get == JsNum(1))
          assert((markupProvidedJson \ "text").get == JsStr("<b>cancel</b>"))

          val mpNameInfoJson = (markupProvidedJson \ "nameInfo").get
          assert((mpNameInfoJson \ "suiteName").get == JsStr("testSuiteName"))
          assert((mpNameInfoJson \ "suiteId").get == JsStr("testSuiteId"))
          assert((mpNameInfoJson \ "suiteClassName").get == JsStr("testSuiteClassName"))
          assert((mpNameInfoJson \ "testName").get == JsStr("test name"))

          val mpFormatterJson = (markupProvidedJson \ "formatter").get
          assert((mpFormatterJson \ "formatterType").get == JsStr("IndentedText"))
          assert((mpFormatterJson \ "formattedText").get == JsStr("formatted text"))
          assert((mpFormatterJson \ "rawText").get == JsStr("raw text"))
          assert((mpFormatterJson \ "indentationLevel").get == JsNum(2))

          val mpLocationJson = (markupProvidedJson \ "location").get
          assert((mpLocationJson \ "locationType").get == JsStr("LineInFile"))
          assert((mpLocationJson \ "lineNumber").get == JsNum(456))
          assert((mpLocationJson \ "fileName").get == JsStr("Test.scala"))
          assert((mpLocationJson \ "filePathname").get == JsNULL)

        case other => fail("Expected JsArray but got: " + other)
      }

      val throwableJson = (jsObj \ "throwable").get
      assert((throwableJson \ "className").get == JsStr("java.lang.RuntimeException"))
      assert((throwableJson \ "message").get == JsStr("cancel message here"))

      assert((jsObj \ "duration").get == JsNum(555))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "rerunner").get == JsStr("rerunner name"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("TestPending event") {

    it("should return correct JSON in its toJson method") {
      val event =
        TestPending(
          new Ordinal(0),
          "testSuiteName",
          "testSuiteId",
          Some("testSuiteClassName"),
          "test name",
          "test text",
          Vector(
            MarkupProvided(
              new Ordinal(1),
              "<b>success</b>",
              Some(NameInfo("testSuiteName", "testSuiteId", Some("testSuiteClassName"), Some("test name"))),
              Some(IndentedText("formatted text", "raw text", 2)),
              Some(LineInFile(456, "Test.scala", None))
            )
          ),
          Some(555),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("TestPending"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "suiteName").get == JsStr("testSuiteName"))
      assert((jsObj \ "suiteId").get == JsStr("testSuiteId"))
      assert((jsObj \ "suiteClassName").get == JsStr("testSuiteClassName"))
      assert((jsObj \ "testName").get == JsStr("test name"))
      assert((jsObj \ "testText").get == JsStr("test text"))

      (jsObj \ "recordedEvents").get match {
        case recordedEvents: JsArr =>
          assert(recordedEvents.value.length == 1)
          val markupProvidedJson = recordedEvents.value(0)
          assert((markupProvidedJson \ "eventType").get == JsStr("MarkupProvided"))
          assert((markupProvidedJson \ "ordinal").get == JsNum(1))
          assert((markupProvidedJson \ "text").get == JsStr("<b>success</b>"))

          val mpNameInfoJson = (markupProvidedJson \ "nameInfo").get
          assert((mpNameInfoJson \ "suiteName").get == JsStr("testSuiteName"))
          assert((mpNameInfoJson \ "suiteId").get == JsStr("testSuiteId"))
          assert((mpNameInfoJson \ "suiteClassName").get == JsStr("testSuiteClassName"))
          assert((mpNameInfoJson \ "testName").get == JsStr("test name"))

          val mpFormatterJson = (markupProvidedJson \ "formatter").get
          assert((mpFormatterJson \ "formatterType").get == JsStr("IndentedText"))
          assert((mpFormatterJson \ "formattedText").get == JsStr("formatted text"))
          assert((mpFormatterJson \ "rawText").get == JsStr("raw text"))
          assert((mpFormatterJson \ "indentationLevel").get == JsNum(2))

          val mpLocationJson = (markupProvidedJson \ "location").get
          assert((mpLocationJson \ "locationType").get == JsStr("LineInFile"))
          assert((mpLocationJson \ "lineNumber").get == JsNum(456))
          assert((mpLocationJson \ "fileName").get == JsStr("Test.scala"))
          assert((mpLocationJson \ "filePathname").get == JsNULL)

        case other => fail("Expected JsArray but got: " + other)
      }

      assert((jsObj \ "duration").get == JsNum(555))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("SuiteStarting event") {

    it("should return correct JSON in its toJson method") {
      val event =
        SuiteStarting(
          new Ordinal(0),
          "testSuiteName",
          "testSuiteId",
          Some("testSuiteClassName"),
          Some(MotionToSuppress),
          Some(TopOfClass("com.test.TestClassName")),
          Some("rerunner name")
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("SuiteStarting"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "suiteName").get == JsStr("testSuiteName"))
      assert((jsObj \ "suiteId").get == JsStr("testSuiteId"))
      assert((jsObj \ "suiteClassName").get == JsStr("testSuiteClassName"))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("TopOfClass"))
      assert((locationJson \ "className").get == JsStr("com.test.TestClassName"))

      assert((jsObj \ "rerunner").get == JsStr("rerunner name"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("SuiteCompleted event") {

    it("should return correct JSON in its toJson method") {
      val event =
        SuiteCompleted(
          new Ordinal(0),
          "testSuiteName",
          "testSuiteId",
          Some("testSuiteClassName"),
          Some(555),
          Some(MotionToSuppress),
          Some(TopOfClass("com.test.TestClassName")),
          Some("rerunner name")
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("SuiteCompleted"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "suiteName").get == JsStr("testSuiteName"))
      assert((jsObj \ "suiteId").get == JsStr("testSuiteId"))
      assert((jsObj \ "suiteClassName").get == JsStr("testSuiteClassName"))

      assert((jsObj \ "duration").get == JsNum(555))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("TopOfClass"))
      assert((locationJson \ "className").get == JsStr("com.test.TestClassName"))

      assert((jsObj \ "rerunner").get == JsStr("rerunner name"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("SuiteAborted event") {

    it("should return correct JSON in its toJson method") {
      val event =
        SuiteAborted(
          new Ordinal(0),
          "boom!",
          "testSuiteName",
          "testSuiteId",
          Some("testSuiteClassName"),
          Some(new RuntimeException("oops!")),
          Some(555),
          Some(MotionToSuppress),
          Some(TopOfClass("com.test.TestClassName")),
          Some("rerunner name")
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("SuiteAborted"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "suiteName").get == JsStr("testSuiteName"))
      assert((jsObj \ "suiteId").get == JsStr("testSuiteId"))
      assert((jsObj \ "suiteClassName").get == JsStr("testSuiteClassName"))

      assert((jsObj \ "message").get == JsStr("boom!"))

      val throwableJson = (jsObj \ "throwable").get
      assert((throwableJson \ "className").get == JsStr("java.lang.RuntimeException"))
      assert((throwableJson \ "message").get == JsStr("oops!"))

      assert((jsObj \ "duration").get == JsNum(555))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("TopOfClass"))
      assert((locationJson \ "className").get == JsStr("com.test.TestClassName"))

      assert((jsObj \ "rerunner").get == JsStr("rerunner name"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("RunStarting event") {

    it("should return correct JSON in its toJson method") {
      val event =
        RunStarting(
          new Ordinal(0),
          123,
          ConfigMap("k1" -> "value 1", "k2" -> "value 2", "k3" -> "value 3"),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("RunStarting"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "testCount").get == JsNum(123))

      val configMapJson = (jsObj \ "configMap").get
      assert((configMapJson \ "k1").get == JsStr("value 1"))
      assert((configMapJson \ "k2").get == JsStr("value 2"))
      assert((configMapJson \ "k3").get == JsStr("value 3"))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("RunCompleted event") {

    it("should return correct JSON in its toJson method") {
      val event =
        RunCompleted(
          new Ordinal(0),
          Some(123),
          Some(Summary(1, 2, 3, 4, 5, 6, 7, 8)),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("RunCompleted"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "duration").get == JsNum(123))

      val summaryJson = (jsObj \ "summary").get
      assert((summaryJson \ "testsSucceededCount").get == JsNum(1))
      assert((summaryJson \ "testsFailedCount").get == JsNum(2))
      assert((summaryJson \ "testsIgnoredCount").get == JsNum(3))
      assert((summaryJson \ "testsPendingCount").get == JsNum(4))
      assert((summaryJson \ "testsCanceledCount").get == JsNum(5))
      assert((summaryJson \ "suitesCompletedCount").get == JsNum(6))
      assert((summaryJson \ "suitesAbortedCount").get == JsNum(7))
      assert((summaryJson \ "scopesPendingCount").get == JsNum(8))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("RunStopped event") {

    it("should return correct JSON in its toJson method") {
      val event =
        RunStopped(
          new Ordinal(0),
          Some(123),
          Some(Summary(1, 2, 3, 4, 5, 6, 7, 8)),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("RunStopped"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "duration").get == JsNum(123))

      val summaryJson = (jsObj \ "summary").get
      assert((summaryJson \ "testsSucceededCount").get == JsNum(1))
      assert((summaryJson \ "testsFailedCount").get == JsNum(2))
      assert((summaryJson \ "testsIgnoredCount").get == JsNum(3))
      assert((summaryJson \ "testsPendingCount").get == JsNum(4))
      assert((summaryJson \ "testsCanceledCount").get == JsNum(5))
      assert((summaryJson \ "suitesCompletedCount").get == JsNum(6))
      assert((summaryJson \ "suitesAbortedCount").get == JsNum(7))
      assert((summaryJson \ "scopesPendingCount").get == JsNum(8))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("RunAborted event") {

    it("should return correct JSON in its toJson method") {
      val event =
        RunAborted(
          new Ordinal(0),
          "error message",
          Some(new RuntimeException("oops!")),
          Some(123),
          Some(Summary(1, 2, 3, 4, 5, 6, 7, 8)),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("RunAborted"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "message").get == JsStr("error message"))

      val throwableJson = (jsObj \ "throwable").get
      assert((throwableJson \ "className").get == JsStr("java.lang.RuntimeException"))
      assert((throwableJson \ "message").get == JsStr("oops!"))

      assert((jsObj \ "duration").get == JsNum(123))

      val summaryJson = (jsObj \ "summary").get
      assert((summaryJson \ "testsSucceededCount").get == JsNum(1))
      assert((summaryJson \ "testsFailedCount").get == JsNum(2))
      assert((summaryJson \ "testsIgnoredCount").get == JsNum(3))
      assert((summaryJson \ "testsPendingCount").get == JsNum(4))
      assert((summaryJson \ "testsCanceledCount").get == JsNum(5))
      assert((summaryJson \ "suitesCompletedCount").get == JsNum(6))
      assert((summaryJson \ "suitesAbortedCount").get == JsNum(7))
      assert((summaryJson \ "scopesPendingCount").get == JsNum(8))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("InfoProvided event") {

    it("should return correct JSON in its toJson method") {
      val event =
        InfoProvided(
          new Ordinal(0),
          "info message",
          Some(NameInfo("suite name", "suite id", Some("SuiteClassName"), None)),
          Some(new RuntimeException("oops!")),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("InfoProvided"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "message").get == JsStr("info message"))

      val throwableJson = (jsObj \ "throwable").get
      assert((throwableJson \ "className").get == JsStr("java.lang.RuntimeException"))
      assert((throwableJson \ "message").get == JsStr("oops!"))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("AlertProvided event") {

    it("should return correct JSON in its toJson method") {
      val event =
        AlertProvided(
          new Ordinal(0),
          "alert message",
          Some(NameInfo("suite name", "suite id", Some("SuiteClassName"), None)),
          Some(new RuntimeException("oops!")),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("AlertProvided"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "message").get == JsStr("alert message"))

      val throwableJson = (jsObj \ "throwable").get
      assert((throwableJson \ "className").get == JsStr("java.lang.RuntimeException"))
      assert((throwableJson \ "message").get == JsStr("oops!"))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("NoteProvided event") {

    it("should return correct JSON in its toJson method") {
      val event =
        NoteProvided(
          new Ordinal(0),
          "note message",
          Some(NameInfo("suite name", "suite id", Some("SuiteClassName"), None)),
          Some(new RuntimeException("oops!")),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("NoteProvided"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "message").get == JsStr("note message"))

      val throwableJson = (jsObj \ "throwable").get
      assert((throwableJson \ "className").get == JsStr("java.lang.RuntimeException"))
      assert((throwableJson \ "message").get == JsStr("oops!"))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("MarkupProvided event") {

    it("should return correct JSON in its toJson method") {
      val event =
        MarkupProvided(
          new Ordinal(0),
          "markup message",
          Some(NameInfo("suite name", "suite id", Some("SuiteClassName"), None)),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("MarkupProvided"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \"text").get == JsStr("markup message"))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("ScopeOpened event") {

    it("should return correct JSON in its toJson method") {
      val event =
        ScopeOpened(
          new Ordinal(0),
          "open the scope!",
          NameInfo("suite name", "suite id", Some("SuiteClassName"), None),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("ScopeOpened"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "message").get == JsStr("open the scope!"))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("ScopeClosed event") {

    it("should return correct JSON in its toJson method") {
      val event =
        ScopeClosed(
          new Ordinal(0),
          "close the scope!",
          NameInfo("suite name", "suite id", Some("SuiteClassName"), None),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("ScopeClosed"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "message").get == JsStr("close the scope!"))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("ScopePending event") {

    it("should return correct JSON in its toJson method") {
      val event =
        ScopePending(
          new Ordinal(0),
          "this scope is pending..",
          NameInfo("suite name", "suite id", Some("SuiteClassName"), None),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala")))
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("ScopePending"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "message").get == JsStr("this scope is pending.."))

      val formatterJson = (jsObj \ "formatter").get
      assert((formatterJson \ "formatterType").get == JsStr("MotionToSuppress"))

      val locationJson = (jsObj \ "location").get
      assert((locationJson \ "locationType").get == JsStr("LineInFile"))
      assert((locationJson \ "lineNumber").get == JsNum(456))
      assert((locationJson \ "fileName").get == JsStr("Test.scala"))
      assert((locationJson \ "filePathname").get == JsStr("path/Test.scala"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("DiscoveryStarting event") {

    it("should return correct JSON in its toJson method") {
      val event =
        DiscoveryStarting(
          new Ordinal(0),
          ConfigMap("k1" -> "value 1", "k2" -> "value 2", "k3" -> "value 3")
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("DiscoveryStarting"))
      assert((jsObj \ "ordinal").get == JsNum(0))

      val configMapJson = (jsObj \ "configMap").get
      assert((configMapJson \ "k1").get == JsStr("value 1"))
      assert((configMapJson \ "k2").get == JsStr("value 2"))
      assert((configMapJson \ "k3").get == JsStr("value 3"))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }

  describe("DiscoveryCompleted event") {

    it("should return correct JSON in its toJson method") {
      val event =
        DiscoveryCompleted(
          new Ordinal(0),
          Some(123)
        )
      val jsonText = event.toJson
      val jsObj = parseJson(jsonText)

      assert((jsObj \ "eventType").get == JsStr("DiscoveryCompleted"))
      assert((jsObj \ "ordinal").get == JsNum(0))
      assert((jsObj \ "duration").get == JsNum(123))

      assert((jsObj \ "threadName").get.value.toString.nonEmpty)
      assert((jsObj \ "timeStamp").get.value.toString.nonEmpty)
    }
  }
// SKIP-SCALATESTNATIVE-END
}
