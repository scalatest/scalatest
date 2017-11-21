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
package org.scalatest.events

import org.scalatest._
import SharedHelpers.EventRecordingReporter
import org.scalatest.prop.Checkers
import examples._

class EventSpec extends FunSpec with Checkers {
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

  import spray.json._
  import DefaultJsonProtocol._

  /*def getField[T](json: Json, key: String)(implicit d: Decoder[T]): T =
    json.hcursor.downField(key).as[T] match {
      case Right(t) => t
      case _ => fail(s"failed to decode $key from: $json")
    }*/

  describe("TestStaring event") {

    it("should return correct JSON in its toJson method") {
      val event = TestStarting(new Ordinal(0), "testSuiteName", "testSuiteId", Some("testSuiteClassName"), "test name", "test text", Some(MotionToSuppress), Some(LineInFile(159, "File.scala", None)), Some("rerunner name"))
      val jsonText = event.toJson
      jsonText.parseJson match {
        case jsObj: JsObject =>
          assert(jsObj.fields("eventType") == JsString("TestStarting"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("suiteName") == JsString("testSuiteName"))
          assert(jsObj.fields("suiteId") == JsString("testSuiteId"))
          assert(jsObj.fields("suiteClassName") == JsString("testSuiteClassName"))
          assert(jsObj.fields("testName") == JsString("test name"))
          assert(jsObj.fields("testText") == JsString("test text"))

          val formatterObj = jsObj.fields("formatter").asJsObject
          assert(formatterObj.fields("formatterType") == JsString("MotionToSuppress"))

          val locationObj = jsObj.fields("location").asJsObject
          assert(locationObj.fields("locationType") == JsString("LineInFile"))
          assert(locationObj.fields("lineNumber") == JsNumber(159))
          assert(locationObj.fields("fileName") == JsString("File.scala"))
          assert(locationObj.fields("filePathname") == JsNull)

          assert(jsObj.fields("rerunner") == JsString("rerunner name"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>
          assert(jsObj.fields("eventType") == JsString("TestSucceeded"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("suiteName") == JsString("testSuiteName"))
          assert(jsObj.fields("suiteId") == JsString("testSuiteId"))
          assert(jsObj.fields("suiteClassName") == JsString("testSuiteClassName"))
          assert(jsObj.fields("testName") == JsString("test name"))
          assert(jsObj.fields("testText") == JsString("test text"))

          jsObj.fields("recordedEvents") match {
            case recordedEvents: JsArray =>
              assert(recordedEvents.elements.length == 1)
              val infoProvidedJson = recordedEvents.elements(0).asJsObject
              assert(infoProvidedJson.fields("eventType") == JsString("InfoProvided"))
              assert(infoProvidedJson.fields("ordinal") == JsNumber(1))
              assert(infoProvidedJson.fields("message") == JsString("success"))

              val ipNameInfoJson = infoProvidedJson.fields("nameInfo").asJsObject
              assert(ipNameInfoJson.fields("suiteName") == JsString("testSuiteName"))
              assert(ipNameInfoJson.fields("suiteId") == JsString("testSuiteId"))
              assert(ipNameInfoJson.fields("suiteClassName") == JsString("testSuiteClassName"))
              assert(ipNameInfoJson.fields("testName") == JsString("test name"))

              val ipThrowableJson = infoProvidedJson.fields("throwable").asJsObject
              assert(ipThrowableJson.fields("className") == JsString("java.lang.RuntimeException"))
              assert(ipThrowableJson.fields("message") == JsString("testing"))

              val ipFormatterJson = infoProvidedJson.fields("formatter").asJsObject
              assert(ipFormatterJson.fields("formatterType") == JsString("MotionToSuppress"))

              val ipLocationJson = infoProvidedJson.fields("location").asJsObject
              assert(ipLocationJson.fields("lineNumber") == JsNumber(123))
              assert(ipLocationJson.fields("fileName") == JsString("Test.scala"))
              assert(ipLocationJson.fields("filePathname") == JsNull)

            case other => fail("Expected JsArray but got: " + other)
          }

          assert(jsObj.fields("duration") == JsNumber(777))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("IndentedText"))
          assert(formatterJson.fields("formattedText") == JsString("formatted text"))
          assert(formatterJson.fields("rawText") == JsString("raw text"))
          assert(formatterJson.fields("indentationLevel") == JsNumber(2))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("rerunner") == JsString("rerunner name"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
    }
  }

  describe("TestFailed event") {

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
          Some(new RuntimeException("fail message here")),
          Some(555),
          Some(MotionToSuppress),
          Some(LineInFile(456, "Test.scala", Some("path/Test.scala"))),
          Some("rerunner name"),
          None
        )
      val jsonText = event.toJson
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("TestFailed"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("suiteName") == JsString("testSuiteName"))
          assert(jsObj.fields("suiteId") == JsString("testSuiteId"))
          assert(jsObj.fields("suiteClassName") == JsString("testSuiteClassName"))
          assert(jsObj.fields("testName") == JsString("test name"))
          assert(jsObj.fields("testText") == JsString("test text"))

          assert(jsObj.fields("message") == JsString("error message"))

          jsObj.fields("recordedEvents") match {
            case recordedEvents: JsArray =>
              assert(recordedEvents.elements.length == 1)
              val markupProvidedJson = recordedEvents.elements(0).asJsObject
              assert(markupProvidedJson.fields("eventType") == JsString("MarkupProvided"))
              assert(markupProvidedJson.fields("ordinal") == JsNumber(1))
              assert(markupProvidedJson.fields("text") == JsString("<b>success</b>"))

              val mpNameInfoJson = markupProvidedJson.fields("nameInfo").asJsObject
              assert(mpNameInfoJson.fields("suiteName") == JsString("testSuiteName"))
              assert(mpNameInfoJson.fields("suiteId") == JsString("testSuiteId"))
              assert(mpNameInfoJson.fields("suiteClassName") == JsString("testSuiteClassName"))
              assert(mpNameInfoJson.fields("testName") == JsString("test name"))

              val mpFormatterJson = markupProvidedJson.fields("formatter").asJsObject
              assert(mpFormatterJson.fields("formatterType") == JsString("IndentedText"))
              assert(mpFormatterJson.fields("formattedText") == JsString("formatted text"))
              assert(mpFormatterJson.fields("rawText") == JsString("raw text"))
              assert(mpFormatterJson.fields("indentationLevel") == JsNumber(2))

              val mpLocationJson = markupProvidedJson.fields("location").asJsObject
              assert(mpLocationJson.fields("locationType") == JsString("LineInFile"))
              assert(mpLocationJson.fields("lineNumber") == JsNumber(456))
              assert(mpLocationJson.fields("fileName") == JsString("Test.scala"))
              assert(mpLocationJson.fields("filePathname") == JsNull)

            case other => fail("Expected JsArray but got: " + other)
          }

          val throwableJson = jsObj.fields("throwable").asJsObject
          assert(throwableJson.fields("className") == JsString("java.lang.RuntimeException"))
          assert(throwableJson.fields("message") == JsString("fail message here"))

          assert(jsObj.fields("duration") == JsNumber(555))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("rerunner") == JsString("rerunner name"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("TestIgnored"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("suiteName") == JsString("testSuiteName"))
          assert(jsObj.fields("suiteId") == JsString("testSuiteId"))
          assert(jsObj.fields("suiteClassName") == JsString("testSuiteClassName"))
          assert(jsObj.fields("testName") == JsString("test name"))
          assert(jsObj.fields("testText") == JsString("test text"))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("TestCanceled"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("suiteName") == JsString("testSuiteName"))
          assert(jsObj.fields("suiteId") == JsString("testSuiteId"))
          assert(jsObj.fields("suiteClassName") == JsString("testSuiteClassName"))
          assert(jsObj.fields("testName") == JsString("test name"))
          assert(jsObj.fields("testText") == JsString("test text"))

          assert(jsObj.fields("message") == JsString("error message"))

          jsObj.fields("recordedEvents") match {
            case recordedEvents: JsArray =>
              assert(recordedEvents.elements.length == 1)
              val markupProvidedJson = recordedEvents.elements(0).asJsObject
              assert(markupProvidedJson.fields("eventType") == JsString("MarkupProvided"))
              assert(markupProvidedJson.fields("ordinal") == JsNumber(1))
              assert(markupProvidedJson.fields("text") == JsString("<b>cancel</b>"))

              val mpNameInfoJson = markupProvidedJson.fields("nameInfo").asJsObject
              assert(mpNameInfoJson.fields("suiteName") == JsString("testSuiteName"))
              assert(mpNameInfoJson.fields("suiteId") == JsString("testSuiteId"))
              assert(mpNameInfoJson.fields("suiteClassName") == JsString("testSuiteClassName"))
              assert(mpNameInfoJson.fields("testName") == JsString("test name"))

              val mpFormatterJson = markupProvidedJson.fields("formatter").asJsObject
              assert(mpFormatterJson.fields("formatterType") == JsString("IndentedText"))
              assert(mpFormatterJson.fields("formattedText") == JsString("formatted text"))
              assert(mpFormatterJson.fields("rawText") == JsString("raw text"))
              assert(mpFormatterJson.fields("indentationLevel") == JsNumber(2))

              val mpLocationJson = markupProvidedJson.fields("location").asJsObject
              assert(mpLocationJson.fields("locationType") == JsString("LineInFile"))
              assert(mpLocationJson.fields("lineNumber") == JsNumber(456))
              assert(mpLocationJson.fields("fileName") == JsString("Test.scala"))
              assert(mpLocationJson.fields("filePathname") == JsNull)

            case other => fail("Expected JsArray but got: " + other)
          }

          val throwableJson = jsObj.fields("throwable").asJsObject
          assert(throwableJson.fields("className") == JsString("java.lang.RuntimeException"))
          assert(throwableJson.fields("message") == JsString("cancel message here"))

          assert(jsObj.fields("duration") == JsNumber(555))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("rerunner") == JsString("rerunner name"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>
          assert(jsObj.fields("eventType") == JsString("TestPending"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("suiteName") == JsString("testSuiteName"))
          assert(jsObj.fields("suiteId") == JsString("testSuiteId"))
          assert(jsObj.fields("suiteClassName") == JsString("testSuiteClassName"))
          assert(jsObj.fields("testName") == JsString("test name"))
          assert(jsObj.fields("testText") == JsString("test text"))

          jsObj.fields("recordedEvents") match {
            case recordedEvents: JsArray =>
              assert(recordedEvents.elements.length == 1)
              val markupProvidedJson = recordedEvents.elements(0).asJsObject
              assert(markupProvidedJson.fields("eventType") == JsString("MarkupProvided"))
              assert(markupProvidedJson.fields("ordinal") == JsNumber(1))
              assert(markupProvidedJson.fields("text") == JsString("<b>success</b>"))

              val mpNameInfoJson = markupProvidedJson.fields("nameInfo").asJsObject
              assert(mpNameInfoJson.fields("suiteName") == JsString("testSuiteName"))
              assert(mpNameInfoJson.fields("suiteId") == JsString("testSuiteId"))
              assert(mpNameInfoJson.fields("suiteClassName") == JsString("testSuiteClassName"))
              assert(mpNameInfoJson.fields("testName") == JsString("test name"))

              val mpFormatterJson = markupProvidedJson.fields("formatter").asJsObject
              assert(mpFormatterJson.fields("formatterType") == JsString("IndentedText"))
              assert(mpFormatterJson.fields("formattedText") == JsString("formatted text"))
              assert(mpFormatterJson.fields("rawText") == JsString("raw text"))
              assert(mpFormatterJson.fields("indentationLevel") == JsNumber(2))

              val mpLocationJson = markupProvidedJson.fields("location").asJsObject
              assert(mpLocationJson.fields("locationType") == JsString("LineInFile"))
              assert(mpLocationJson.fields("lineNumber") == JsNumber(456))
              assert(mpLocationJson.fields("fileName") == JsString("Test.scala"))
              assert(mpLocationJson.fields("filePathname") == JsNull)

            case other => fail("Expected JsArray but got: " + other)
          }

          assert(jsObj.fields("duration") == JsNumber(555))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("SuiteStarting"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("suiteName") == JsString("testSuiteName"))
          assert(jsObj.fields("suiteId") == JsString("testSuiteId"))
          assert(jsObj.fields("suiteClassName") == JsString("testSuiteClassName"))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("TopOfClass"))
          assert(locationJson.fields("className") == JsString("com.test.TestClassName"))

          assert(jsObj.fields("rerunner") == JsString("rerunner name"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("SuiteCompleted"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("suiteName") == JsString("testSuiteName"))
          assert(jsObj.fields("suiteId") == JsString("testSuiteId"))
          assert(jsObj.fields("suiteClassName") == JsString("testSuiteClassName"))

          assert(jsObj.fields("duration") == JsNumber(555))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("TopOfClass"))
          assert(locationJson.fields("className") == JsString("com.test.TestClassName"))

          assert(jsObj.fields("rerunner") == JsString("rerunner name"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("SuiteAborted"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("suiteName") == JsString("testSuiteName"))
          assert(jsObj.fields("suiteId") == JsString("testSuiteId"))
          assert(jsObj.fields("suiteClassName") == JsString("testSuiteClassName"))

          assert(jsObj.fields("message") == JsString("boom!"))

          val throwableJson = jsObj.fields("throwable").asJsObject
          assert(throwableJson.fields("className") == JsString("java.lang.RuntimeException"))
          assert(throwableJson.fields("message") == JsString("oops!"))

          assert(jsObj.fields("duration") == JsNumber(555))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("TopOfClass"))
          assert(locationJson.fields("className") == JsString("com.test.TestClassName"))

          assert(jsObj.fields("rerunner") == JsString("rerunner name"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("RunStarting"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("testCount") == JsNumber(123))

          val configMapJson = jsObj.fields("configMap").asJsObject
          assert(configMapJson.fields("k1") == JsString("value 1"))
          assert(configMapJson.fields("k2") == JsString("value 2"))
          assert(configMapJson.fields("k3") == JsString("value 3"))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("RunCompleted"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("duration") == JsNumber(123))

          val summaryJson = jsObj.fields("summary").asJsObject
          assert(summaryJson.fields("testsSucceededCount") == JsNumber(1))
          assert(summaryJson.fields("testsFailedCount") == JsNumber(2))
          assert(summaryJson.fields("testsIgnoredCount") == JsNumber(3))
          assert(summaryJson.fields("testsPendingCount") == JsNumber(4))
          assert(summaryJson.fields("testsCanceledCount") == JsNumber(5))
          assert(summaryJson.fields("suitesCompletedCount") == JsNumber(6))
          assert(summaryJson.fields("suitesAbortedCount") == JsNumber(7))
          assert(summaryJson.fields("scopesPendingCount") == JsNumber(8))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("RunStopped"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("duration") == JsNumber(123))

          val summaryJson = jsObj.fields("summary").asJsObject
          assert(summaryJson.fields("testsSucceededCount") == JsNumber(1))
          assert(summaryJson.fields("testsFailedCount") == JsNumber(2))
          assert(summaryJson.fields("testsIgnoredCount") == JsNumber(3))
          assert(summaryJson.fields("testsPendingCount") == JsNumber(4))
          assert(summaryJson.fields("testsCanceledCount") == JsNumber(5))
          assert(summaryJson.fields("suitesCompletedCount") == JsNumber(6))
          assert(summaryJson.fields("suitesAbortedCount") == JsNumber(7))
          assert(summaryJson.fields("scopesPendingCount") == JsNumber(8))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("RunAborted"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("message") == JsString("error message"))

          val throwableJson = jsObj.fields("throwable").asJsObject
          assert(throwableJson.fields("className") == JsString("java.lang.RuntimeException"))
          assert(throwableJson.fields("message") == JsString("oops!"))

          assert(jsObj.fields("duration") == JsNumber(123))

          val summaryJson = jsObj.fields("summary").asJsObject
          assert(summaryJson.fields("testsSucceededCount") == JsNumber(1))
          assert(summaryJson.fields("testsFailedCount") == JsNumber(2))
          assert(summaryJson.fields("testsIgnoredCount") == JsNumber(3))
          assert(summaryJson.fields("testsPendingCount") == JsNumber(4))
          assert(summaryJson.fields("testsCanceledCount") == JsNumber(5))
          assert(summaryJson.fields("suitesCompletedCount") == JsNumber(6))
          assert(summaryJson.fields("suitesAbortedCount") == JsNumber(7))
          assert(summaryJson.fields("scopesPendingCount") == JsNumber(8))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("InfoProvided"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("message") == JsString("info message"))

          val throwableJson = jsObj.fields("throwable").asJsObject
          assert(throwableJson.fields("className") == JsString("java.lang.RuntimeException"))
          assert(throwableJson.fields("message") == JsString("oops!"))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("AlertProvided"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("message") == JsString("alert message"))

          val throwableJson = jsObj.fields("throwable").asJsObject
          assert(throwableJson.fields("className") == JsString("java.lang.RuntimeException"))
          assert(throwableJson.fields("message") == JsString("oops!"))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>
          assert(jsObj.fields("eventType") == JsString("NoteProvided"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("message") == JsString("note message"))

          val throwableJson = jsObj.fields("throwable").asJsObject
          assert(throwableJson.fields("className") == JsString("java.lang.RuntimeException"))
          assert(throwableJson.fields("message") == JsString("oops!"))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>
          assert(jsObj.fields("eventType") == JsString("MarkupProvided"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("text") == JsString("markup message"))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("ScopeOpened"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("message") == JsString("open the scope!"))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("ScopeClosed"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("message") == JsString("close the scope!"))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("ScopePending"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("message") == JsString("this scope is pending.."))

          val formatterJson = jsObj.fields("formatter").asJsObject
          assert(formatterJson.fields("formatterType") == JsString("MotionToSuppress"))

          val locationJson = jsObj.fields("location").asJsObject
          assert(locationJson.fields("locationType") == JsString("LineInFile"))
          assert(locationJson.fields("lineNumber") == JsNumber(456))
          assert(locationJson.fields("fileName") == JsString("Test.scala"))
          assert(locationJson.fields("filePathname") == JsString("path/Test.scala"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("DiscoveryStarting"))
          assert(jsObj.fields("ordinal") == JsNumber(0))

          val configMapJson = jsObj.fields("configMap").asJsObject
          assert(configMapJson.fields("k1") == JsString("value 1"))
          assert(configMapJson.fields("k2") == JsString("value 2"))
          assert(configMapJson.fields("k3") == JsString("value 3"))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
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
      jsonText.parseJson match {
        case jsObj: JsObject =>

          assert(jsObj.fields("eventType") == JsString("DiscoveryCompleted"))
          assert(jsObj.fields("ordinal") == JsNumber(0))
          assert(jsObj.fields("duration") == JsNumber(123))

          assert(jsObj.fields("threadName").toString.nonEmpty)
          assert(jsObj.fields("timeStamp").toString.nonEmpty)

        case _ =>  fail("Unable to parse JSON: " + jsonText + " into a JsObject.")
      }
    }
  }
}
