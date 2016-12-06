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

  import io.circe._
  import io.circe.parser._

  def getField[T](json: Json, key: String)(implicit d: Decoder[T]): T =
    json.hcursor.downField(key).as[T] match {
      case Right(t) => t
      case _ => fail(s"failed to decode $key from: $json")
    }

  describe("TestStaring event") {

    it("should return correct JSON in its toJson method") {
      val event = TestStarting(new Ordinal(0), "testSuiteName", "testSuiteId", Some("testSuiteClassName"), "test name", "test text", Some(MotionToSuppress), Some(LineInFile(159, "File.scala", None)), Some("rerunner name"))
      val jsonText = event.toJson
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "TestStarting")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "suiteName") == "testSuiteName")
          assert(getField[String](json, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](json, "suiteClassName") == Some("testSuiteClassName"))
          assert(getField[String](json, "testName") == "test name")
          assert(getField[String](json, "testText") == "test text")

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 159)
          assert(getField[String](locationJson, "fileName") == "File.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == None)

          assert(getField[Option[String]](json, "rerunner") == Some("rerunner name"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "TestSucceeded")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "suiteName") == "testSuiteName")
          assert(getField[String](json, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](json, "suiteClassName") == Some("testSuiteClassName"))
          assert(getField[String](json, "testName") == "test name")
          assert(getField[String](json, "testText") == "test text")

          val recordedEvents = getField[Vector[Json]](json, "recordedEvents")
          assert(recordedEvents.length == 1)
          val infoProvidedJson = recordedEvents(0)
          assert(getField[String](infoProvidedJson, "eventType") == "InfoProvided")
          assert(getField[Int](infoProvidedJson, "ordinal") == 1)
          assert(getField[String](infoProvidedJson, "message") == "success")

          val ipNameInfoJson = getField[Json](infoProvidedJson, "nameInfo")
          assert(getField[String](ipNameInfoJson, "suiteName") == "testSuiteName")
          assert(getField[String](ipNameInfoJson, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](ipNameInfoJson, "suiteClassName") == Some("testSuiteClassName"))
          assert(getField[Option[String]](ipNameInfoJson, "testName") == Some("test name"))

          val ipThrowableJson = getField[Json](infoProvidedJson, "throwable")
          assert(getField[String](ipThrowableJson, "className") == "java.lang.RuntimeException")
          assert(getField[String](ipThrowableJson, "message") == "testing")

          val ipFormatterJson = getField[Json](infoProvidedJson, "formatter")
          assert(getField[String](ipFormatterJson, "formatterType") == "MotionToSuppress")

          val ipLocationJson = getField[Json](infoProvidedJson, "location")
          assert(getField[Int](ipLocationJson, "lineNumber") == 123)
          assert(getField[String](ipLocationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](ipLocationJson, "filePathname") == None)

          assert(getField[Option[Int]](json, "duration") == Some(777))

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "IndentedText")
          assert(getField[String](formatterJson, "formattedText") == "formatted text")
          assert(getField[String](formatterJson, "rawText") == "raw text")
          assert(getField[Int](formatterJson, "indentationLevel") == 2)

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "rerunner") == "rerunner name")

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
      }
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "TestFailed")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "suiteName") == "testSuiteName")
          assert(getField[String](json, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](json, "suiteClassName") == Some("testSuiteClassName"))
          assert(getField[String](json, "testName") == "test name")
          assert(getField[String](json, "testText") == "test text")

          assert(getField[String](json, "message") == "error message")

          val recordedEvents = getField[Vector[Json]](json, "recordedEvents")
          assert(recordedEvents.length == 1)
          val markupProvidedJson = recordedEvents(0)
          assert(getField[String](markupProvidedJson, "eventType") == "MarkupProvided")
          assert(getField[Int](markupProvidedJson, "ordinal") == 1)
          assert(getField[String](markupProvidedJson, "text") == "<b>success</b>")

          val mpNameInfoJson = getField[Json](markupProvidedJson, "nameInfo")
          assert(getField[String](mpNameInfoJson, "suiteName") == "testSuiteName")
          assert(getField[String](mpNameInfoJson, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](mpNameInfoJson, "suiteClassName") == Some("testSuiteClassName"))
          assert(getField[Option[String]](mpNameInfoJson, "testName") == Some("test name"))

          val throwableJson = getField[Json](json, "throwable")
          assert(getField[String](throwableJson, "className") == "java.lang.RuntimeException")
          assert(getField[String](throwableJson, "message") == "fail message here")

          val mpFormatterJson = getField[Json](markupProvidedJson, "formatter")
          assert(getField[String](mpFormatterJson, "formatterType") == "IndentedText")
          assert(getField[String](mpFormatterJson, "formattedText") == "formatted text")
          assert(getField[String](mpFormatterJson, "rawText") == "raw text")
          assert(getField[Int](mpFormatterJson, "indentationLevel") == 2)

          val mpLocationJson = getField[Json](markupProvidedJson, "location")
          assert(getField[String](mpLocationJson, "locationType") == "LineInFile")
          assert(getField[Int](mpLocationJson, "lineNumber") == 456)
          assert(getField[String](mpLocationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](mpLocationJson, "filePathname") == None)

          assert(getField[Option[Int]](json, "duration") == Some(555))

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "rerunner") == "rerunner name")

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
      }
    }

    it("should carry correct differences value when used with s1 shouldEqual s2 syntax") {
      class ExampleSpec extends FunSuite with Matchers {
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
      class ExampleSpec extends FunSuite with Matchers {
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
      class ExampleSpec extends FunSuite with Matchers {
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
      class ExampleSpec extends FunSuite with Matchers {
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "TestIgnored")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "suiteName") == "testSuiteName")
          assert(getField[String](json, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](json, "suiteClassName") == Some("testSuiteClassName"))
          assert(getField[String](json, "testName") == "test name")
          assert(getField[String](json, "testText") == "test text")

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "TestCanceled")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "suiteName") == "testSuiteName")
          assert(getField[String](json, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](json, "suiteClassName") == Some("testSuiteClassName"))
          assert(getField[String](json, "testName") == "test name")
          assert(getField[String](json, "testText") == "test text")

          assert(getField[String](json, "message") == "error message")

          val recordedEvents = getField[Vector[Json]](json, "recordedEvents")
          assert(recordedEvents.length == 1)
          val markupProvidedJson = recordedEvents(0)
          assert(getField[String](markupProvidedJson, "eventType") == "MarkupProvided")
          assert(getField[Int](markupProvidedJson, "ordinal") == 1)
          assert(getField[String](markupProvidedJson, "text") == "<b>cancel</b>")

          val mpNameInfoJson = getField[Json](markupProvidedJson, "nameInfo")
          assert(getField[String](mpNameInfoJson, "suiteName") == "testSuiteName")
          assert(getField[String](mpNameInfoJson, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](mpNameInfoJson, "suiteClassName") == Some("testSuiteClassName"))
          assert(getField[Option[String]](mpNameInfoJson, "testName") == Some("test name"))

          val throwableJson = getField[Json](json, "throwable")
          assert(getField[String](throwableJson, "className") == "java.lang.RuntimeException")
          assert(getField[String](throwableJson, "message") == "cancel message here")

          val mpFormatterJson = getField[Json](markupProvidedJson, "formatter")
          assert(getField[String](mpFormatterJson, "formatterType") == "IndentedText")
          assert(getField[String](mpFormatterJson, "formattedText") == "formatted text")
          assert(getField[String](mpFormatterJson, "rawText") == "raw text")
          assert(getField[Int](mpFormatterJson, "indentationLevel") == 2)

          val mpLocationJson = getField[Json](markupProvidedJson, "location")
          assert(getField[String](mpLocationJson, "locationType") == "LineInFile")
          assert(getField[Int](mpLocationJson, "lineNumber") == 456)
          assert(getField[String](mpLocationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](mpLocationJson, "filePathname") == None)

          assert(getField[Option[Int]](json, "duration") == Some(555))

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "rerunner") == "rerunner name")

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "TestPending")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "suiteName") == "testSuiteName")
          assert(getField[String](json, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](json, "suiteClassName") == Some("testSuiteClassName"))
          assert(getField[String](json, "testName") == "test name")
          assert(getField[String](json, "testText") == "test text")

          val recordedEvents = getField[Vector[Json]](json, "recordedEvents")
          assert(recordedEvents.length == 1)
          val markupProvidedJson = recordedEvents(0)
          assert(getField[String](markupProvidedJson, "eventType") == "MarkupProvided")
          assert(getField[Int](markupProvidedJson, "ordinal") == 1)
          assert(getField[String](markupProvidedJson, "text") == "<b>success</b>")

          val mpNameInfoJson = getField[Json](markupProvidedJson, "nameInfo")
          assert(getField[String](mpNameInfoJson, "suiteName") == "testSuiteName")
          assert(getField[String](mpNameInfoJson, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](mpNameInfoJson, "suiteClassName") == Some("testSuiteClassName"))
          assert(getField[Option[String]](mpNameInfoJson, "testName") == Some("test name"))

          val mpFormatterJson = getField[Json](markupProvidedJson, "formatter")
          assert(getField[String](mpFormatterJson, "formatterType") == "IndentedText")
          assert(getField[String](mpFormatterJson, "formattedText") == "formatted text")
          assert(getField[String](mpFormatterJson, "rawText") == "raw text")
          assert(getField[Int](mpFormatterJson, "indentationLevel") == 2)

          val mpLocationJson = getField[Json](markupProvidedJson, "location")
          assert(getField[Int](mpLocationJson, "lineNumber") == 456)
          assert(getField[String](mpLocationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](mpLocationJson, "filePathname") == None)

          assert(getField[Option[Int]](json, "duration") == Some(555))

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val loationJson = getField[Json](json, "location")
          assert(getField[Int](loationJson, "lineNumber") == 456)
          assert(getField[String](loationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](loationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "SuiteStarting")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "suiteName") == "testSuiteName")
          assert(getField[String](json, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](json, "suiteClassName") == Some("testSuiteClassName"))

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "TopOfClass")
          assert(getField[String](locationJson, "className") == "com.test.TestClassName")

          assert(getField[String](json, "rerunner") == "rerunner name")

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "SuiteCompleted")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "suiteName") == "testSuiteName")
          assert(getField[String](json, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](json, "suiteClassName") == Some("testSuiteClassName"))

          assert(getField[Option[Int]](json, "duration") == Some(555))

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "TopOfClass")
          assert(getField[String](locationJson, "className") == "com.test.TestClassName")

          assert(getField[String](json, "rerunner") == "rerunner name")

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "SuiteAborted")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "suiteName") == "testSuiteName")
          assert(getField[String](json, "suiteId") == "testSuiteId")
          assert(getField[Option[String]](json, "suiteClassName") == Some("testSuiteClassName"))

          assert(getField[String](json, "message") == "boom!")

          val throwableJson = getField[Json](json, "throwable")
          assert(getField[String](throwableJson, "className") == "java.lang.RuntimeException")
          assert(getField[String](throwableJson, "message") == "oops!")

          assert(getField[Option[Int]](json, "duration") == Some(555))

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "TopOfClass")
          assert(getField[String](locationJson, "className") == "com.test.TestClassName")

          assert(getField[String](json, "rerunner") == "rerunner name")

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "RunStarting")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[Int](json, "testCount") == 123)

          val configMapJson = getField[Json](json, "configMap")
          assert(getField[String](configMapJson, "k1") == "value 1")
          assert(getField[String](configMapJson, "k2") == "value 2")
          assert(getField[String](configMapJson, "k3") == "value 3")

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "RunCompleted")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[Option[Int]](json, "duration") == Some(123))

          val summaryJson = getField[Json](json, "summary")
          assert(getField[Int](summaryJson, "testsSucceededCount") == 1)
          assert(getField[Int](summaryJson, "testsFailedCount") == 2)
          assert(getField[Int](summaryJson, "testsIgnoredCount") == 3)
          assert(getField[Int](summaryJson, "testsPendingCount") == 4)
          assert(getField[Int](summaryJson, "testsCanceledCount") == 5)
          assert(getField[Int](summaryJson, "suitesCompletedCount") == 6)
          assert(getField[Int](summaryJson, "suitesAbortedCount") == 7)
          assert(getField[Int](summaryJson, "scopesPendingCount") == 8)

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "RunStopped")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[Option[Int]](json, "duration") == Some(123))

          val summaryJson = getField[Json](json, "summary")
          assert(getField[Int](summaryJson, "testsSucceededCount") == 1)
          assert(getField[Int](summaryJson, "testsFailedCount") == 2)
          assert(getField[Int](summaryJson, "testsIgnoredCount") == 3)
          assert(getField[Int](summaryJson, "testsPendingCount") == 4)
          assert(getField[Int](summaryJson, "testsCanceledCount") == 5)
          assert(getField[Int](summaryJson, "suitesCompletedCount") == 6)
          assert(getField[Int](summaryJson, "suitesAbortedCount") == 7)
          assert(getField[Int](summaryJson, "scopesPendingCount") == 8)

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "RunAborted")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "message") == "error message")

          val throwableJson = getField[Json](json, "throwable")
          assert(getField[String](throwableJson, "className") == "java.lang.RuntimeException")
          assert(getField[String](throwableJson, "message") == "oops!")

          assert(getField[Option[Int]](json, "duration") == Some(123))

          val summaryJson = getField[Json](json, "summary")
          assert(getField[Int](summaryJson, "testsSucceededCount") == 1)
          assert(getField[Int](summaryJson, "testsFailedCount") == 2)
          assert(getField[Int](summaryJson, "testsIgnoredCount") == 3)
          assert(getField[Int](summaryJson, "testsPendingCount") == 4)
          assert(getField[Int](summaryJson, "testsCanceledCount") == 5)
          assert(getField[Int](summaryJson, "suitesCompletedCount") == 6)
          assert(getField[Int](summaryJson, "suitesAbortedCount") == 7)
          assert(getField[Int](summaryJson, "scopesPendingCount") == 8)

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "InfoProvided")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "message") == "info message")

          val throwableJson = getField[Json](json, "throwable")
          assert(getField[String](throwableJson, "className") == "java.lang.RuntimeException")
          assert(getField[String](throwableJson, "message") == "oops!")

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "AlertProvided")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "message") == "alert message")

          val throwableJson = getField[Json](json, "throwable")
          assert(getField[String](throwableJson, "className") == "java.lang.RuntimeException")
          assert(getField[String](throwableJson, "message") == "oops!")

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "NoteProvided")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "message") == "note message")

          val throwableJson = getField[Json](json, "throwable")
          assert(getField[String](throwableJson, "className") == "java.lang.RuntimeException")
          assert(getField[String](throwableJson, "message") == "oops!")

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "MarkupProvided")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "text") == "markup message")

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "ScopeOpened")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "message") == "open the scope!")

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "ScopeClosed")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "message") == "close the scope!")

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "ScopePending")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[String](json, "message") == "this scope is pending..")

          val formatterJson = getField[Json](json, "formatter")
          assert(getField[String](formatterJson, "formatterType") == "MotionToSuppress")

          val locationJson = getField[Json](json, "location")
          assert(getField[String](locationJson, "locationType") == "LineInFile")
          assert(getField[Int](locationJson, "lineNumber") == 456)
          assert(getField[String](locationJson, "fileName") == "Test.scala")
          assert(getField[Option[String]](locationJson, "filePathname") == Some("path/Test.scala"))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "DiscoveryStarting")
          assert(getField[Int](json, "ordinal") == 0)

          val configMapJson = getField[Json](json, "configMap")
          assert(getField[String](configMapJson, "k1") == "value 1")
          assert(getField[String](configMapJson, "k2") == "value 2")
          assert(getField[String](configMapJson, "k3") == "value 3")

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
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
      parse(jsonText) match {
        case Right(json) =>
          assert(getField[String](json, "eventType") == "DiscoveryCompleted")
          assert(getField[Int](json, "ordinal") == 0)
          assert(getField[Option[Int]](json, "duration") == Some(123))

          assert(getField[String](json, "threadName").nonEmpty)
          assert(getField[Long](json, "timeStamp") > 0L)

        case Left(failure) => fail("Unable to parse JSON: " + jsonText + ", failure: " + failure.message)
      }
    }
  }
}
