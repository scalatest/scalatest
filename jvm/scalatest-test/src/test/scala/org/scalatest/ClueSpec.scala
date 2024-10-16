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
package org.scalatest

import exceptions.{GeneratorDrivenPropertyCheckFailedException, TableDrivenPropertyCheckFailedException, TestFailedDueToTimeoutException, TestCanceledException}

// SKIP-SCALATESTJS,NATIVE-START
import org.scalatestplus.junit.JUnitTestFailedError
// SKIP-SCALATESTJS,NATIVE-END
import prop.TableDrivenPropertyChecks
import TableDrivenPropertyChecks._
import org.scalatest.exceptions.ModifiableMessage
import org.scalatest.exceptions.StackDepth
import SharedHelpers.EventRecordingReporter
import org.scalactic.exceptions.NullArgumentException
import org.scalactic.source
import org.scalactic.Prettifier
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TestFailedException
import prop.TableFor1
import time.{Second, Span}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Fact._

class ClueSpec extends AnyFlatSpec with Matchers {

  def examples: TableFor1[Throwable with ModifiableMessage[_ <: StackDepth]] =
    Table(
      "exception",
      new TestFailedException((_: StackDepthException) => Some("message"), None, source.Position.here),
      // SKIP-SCALATESTJS,NATIVE-START
      new JUnitTestFailedError("message", 3),
      // SKIP-SCALATESTJS,NATIVE-END
      new TestFailedDueToTimeoutException((_: StackDepthException) => Some("message"), None, Left(source.Position.here), None, Span(1, Second)),
      new TableDrivenPropertyCheckFailedException((_: StackDepthException) => "message", None, source.Position.here, None, "undecMsg", List.empty, List.empty, 3),
      new GeneratorDrivenPropertyCheckFailedException((_: StackDepthException) => "message", None, source.Position.here, None, "undecMsg", List.empty, Option(List.empty), List.empty)
   )

  // TOTEST: clue object with toString. clue object with null toString. all-whitespace clue string
  "The modifyMessage method" should "return the an exception with an equal message option if passed a function that returns the same option passed to it" in {
    forAll (examples) { e =>
      e.modifyMessage(opt => opt) should equal (e)
    }
  }

  it should "return the new exception with the clue string prepended, separated by a space char if passed a function that does that" in {
    forAll (examples) { e =>
      val clue = "clue"
      val fun: (Option[String] => Option[String]) =
        opt => opt match {
          case Some(msg) => Some(clue + " " + msg)
          case None => Some(clue)
        }
      e.modifyMessage(fun).message.get should be ("clue message")
    }
  }

  // ******* withClue tests *******

  "The withClue construct" should "allow any non-ModifiableMessage exception to pass through" in {
    val iae = new IllegalArgumentException
    val caught = intercept[IllegalArgumentException] {
      withClue("howdy") {
        throw iae 
      }
    }
    caught should be theSameInstanceAs (iae)
  }

  it should "given an empty clue string, rethrow the same TFE exception" in {
    forAll (examples) { e =>
      val caught = intercept[Throwable] {
        withClue("") {
          throw e
        }
      }
      caught should be theSameInstanceAs (e)
    }
  }

  it should "given an all-whitespace clue string, should throw a new TFE with the white space prepended to the old message" in {
    forAll (examples) { e =>
      val white = "    "
      val caught = intercept[Throwable with StackDepth] {
        withClue(white) {
          throw e
        }
      }
      caught should not be theSameInstanceAs (e)
      caught.message should equal (Some(white + "message"))
      caught.getClass should be theSameInstanceAs (e.getClass)
    }
  }

  it should "given a non-empty clue string with no trailing white space, throw a new instance of the caught TFE exception that has all fields the same except a prepended clue string followed by an extra space" in {
    forAll (examples) { e =>
      val caught = intercept[Throwable with StackDepth] {
        withClue("clue") {
          throw e
        }
      }
      caught should not be theSameInstanceAs (e)
      caught.message should equal (Some("clue message"))
      caught.getClass should be theSameInstanceAs (e.getClass)
    }
  }

  it should "given a non-empty clue string with a trailing space, throw a new instance of the caught TFE exception that has all fields the same except a prepended clue string (followed by no extra space)" in {
    forAll (examples) { e =>
      val caught = intercept[Throwable with StackDepth] {
        withClue("clue ") { // has a trailing space
          throw e
        }
      }
      caught should not be theSameInstanceAs (e)
      caught.message should equal (Some("clue message"))
      caught.getClass should be theSameInstanceAs (e.getClass)
    }
  }

  it should "given a non-empty clue string with a end of line, throw a new instance of the caught TFE exception that has all fields the same except a prepended clue string (followed by no extra space)" in {
    forAll (examples) { e =>
      val caught = intercept[Throwable with StackDepth] {
        withClue("clue\n") { // has a end of line character
          throw e
        }
      }
      caught should not be theSameInstanceAs (e)
      caught.message should equal (Some("clue\nmessage"))
      caught.getClass should be theSameInstanceAs (e.getClass)
    }
  }

  // ***** tests with objects other than String *****

  it should "given an object with a non-empty clue string with no trailing white space, throw a new instance of the caught TFE exception that has all fields the same except a prepended clue string followed by an extra space" in {
    forAll (examples) { e =>
      val list = List(1, 2, 3)
      val caught = intercept[Throwable with StackDepth] {
        withClue(list) {
          throw e
        }
      }
      caught should not be theSameInstanceAs (e)
      caught.message should equal (Some("List(1, 2, 3) message"))
      caught.getClass should be theSameInstanceAs (e.getClass)
    }
  }

  it should "pass the last value back" in {
    val result = withClue("hi") { 3 }
    result should equal (3)
  }

  it should "throw NPE if a null clue object is passed" in {
    forAll (examples) { e =>
      assertThrows[NullArgumentException] {
        withClue (null) {
          throw e
        }
      }
    }
  }
  
  it should "infer the type of the result of the passed in function" in {
    val result: Int = withClue("hi") { 22 }
    assert(result === 22)
  }
  
  it should "be able to accept by-name payload" in {
    val result: String = withClue(() => 128) { "hello" }
    assert(result === "hello")
  }

  it should "work when used in withFixture" in {
    forAll(examples) { e => 
      val a = 
        new org.scalatest.funspec.FixtureAnyFunSpec {
          type FixtureParam = String
        
          override def withFixture(test: OneArgTest) = {
            withClue("a clue") {
              test("something")
            }
          }
        
          it("should do something") { p => 
            throw e
          }
        }
      val rep = new EventRecordingReporter()
      a.run(None, Args(rep))
      rep.testFailedEventsReceived.length should be (1)
      rep.testFailedEventsReceived(0).message should be ("a clue message")
    }
  }

  it should "return Failed that contains TestFailedException and with prepended clue" in {
    val failed = Failed(new TestFailedException((_: StackDepthException) => Some("message"), None, source.Position.here))
    val result = withClue("a clue") { failed }
    result shouldBe a [Failed]
    result.exception shouldBe a [TestFailedException]
    result.exception.getMessage shouldBe "a clue message"
  }

  it should "return original Failed that contains the RuntimeException and without prepended clue" in {
    val failed = Failed(new RuntimeException("message"))
    val result = withClue("a clue") { failed }
    result should be theSameInstanceAs failed
    result.exception.getMessage shouldBe "message"
  }

  it should "return Canceled that contains TestCanceledException and with prepended clue" in {
    val canceled = Canceled(new TestCanceledException("message", 3))
    val result = withClue("a clue") { canceled }
    result shouldBe a [Canceled]
    result.exception shouldBe a [TestCanceledException]
    result.exception.getMessage shouldBe "a clue message"
  }

  it should "return original Canceled that contains the RuntimeException and without prepended clue" in {
    val re = new RuntimeException("message")
    val canceled = Canceled(re)
    val result = withClue("a clue") { canceled }
    result.exception.getCause should be theSameInstanceAs re
    result.exception.getMessage shouldBe "a clue message"
  }

  it should "return original Pending" in {
    val pending = Pending
    val result = withClue("a clue") { pending }
    result should be theSameInstanceAs pending
  }

  it should "return original Succeeded" in {
    val succeeded = Succeeded
    val result = withClue("a clue") { succeeded }
    result should be theSameInstanceAs succeeded
  }

  it should "return Yes prepended clue" in {
    val fact = Yes("message", Prettifier.default)
    val result = withClue("a clue") { fact }
    result shouldBe a [Fact]
    result.factMessage shouldBe "a clue message"
  }

  it should "return No prepended clue" in {
    val fact = No("message", Prettifier.default)
    val result = withClue("a clue") { fact }
    result shouldBe a [Fact]
    result.factMessage shouldBe "a clue message"
  }

  it should "return Unary_! prepended clue" in {
    val fact = !No("message", Prettifier.default)
    val result = withClue("a clue") { fact }
    result shouldBe a [Fact]
    result.factMessage shouldBe "a clue message"
  }

  it should "return Binary_| prepended clue" in {
    val fact = Yes("message 1", Prettifier.default) | No("message 2", Prettifier.default)
    val result = withClue("a clue") { fact }
    result shouldBe a [Fact]
    result.factMessage shouldBe "a clue message 1, and message 2"

    val fact2 = Yes("message 1", Prettifier.default) | No("message 2", Prettifier.default)
    val result2 = withClue("a clue") { fact2 }
    result2 shouldBe a [Fact]
    result2.factMessage shouldBe "a clue message 1, and message 2"
  }

  it should "return Binary_|| prepended clue" in {
    val fact = No("message 1", Prettifier.default) || Yes("message 2", Prettifier.default)
    val result = withClue("a clue") { fact }
    result shouldBe a [Fact]
    result.factMessage shouldBe "a clue message 1, and message 2"

    val fact2 = Yes("message 1", Prettifier.default) || No("message 2", Prettifier.default)
    val result2 = withClue("a clue") { fact2 }
    result2 shouldBe a [Fact]
    result2.factMessage shouldBe "a clue message 1"
  }

  it should "return Binary_& prepended clue" in {
    val fact = Yes("message 1", Prettifier.default) & No("message 2", Prettifier.default)
    val result = withClue("a clue") { fact }
    result shouldBe a [Fact]
    result.factMessage shouldBe "a clue message 1, but message 2"

    val fact2 = No("message 1", Prettifier.default) & Yes("message 2", Prettifier.default)
    val result2 = withClue("a clue") { fact2 }
    result2 shouldBe a [Fact]
    result2.factMessage shouldBe "a clue message 1, and message 2"
  }

  it should "return Binary_&& prepended clue" in {
    val fact = Yes("message 1", Prettifier.default) && No("message 2", Prettifier.default)
    val result = withClue("a clue") { fact }
    result shouldBe a [Fact]
    result.factMessage shouldBe "a clue message 1, but message 2"

    val fact2 = No("message 1", Prettifier.default) && Yes("message 2", Prettifier.default)
    val result2 = withClue("a clue") { fact2 }
    result2 shouldBe a [Fact]
    result2.factMessage shouldBe "a clue message 1"
  }

  it should "return Implies prepended clue" in {
    val fact = Yes("message 1", Prettifier.default) implies No("message 2", Prettifier.default)
    val result = withClue("a clue") { fact }
    result shouldBe a [Fact]
    result.factMessage shouldBe "a clue message 1, but message 2"
  }

  it should "return IsEqvTo prepended clue" in {
    val fact = Yes("message 1", Prettifier.default) isEqvTo No("message 2", Prettifier.default)
    val result = withClue("a clue") { fact }
    result shouldBe a [Fact]
    result.factMessage shouldBe "a clue message 1, and message 2"
  }

  // SKIP-SCALATESTJS,NATIVE-START
  it should "throw Serializable TestFailedDueToTimeoutException thrown from withClue wrapping a failing eventually" in {

    import org.scalatest.concurrent.Eventually._
    import org.scalatest.exceptions.TestFailedDueToTimeoutException
    import SharedHelpers.serializeRoundtrip

    val result = intercept[TestFailedDueToTimeoutException] {
      withClue("a clue") {
        eventually {
          "a" should equal ("b")
        }
      }
    }

    serializeRoundtrip(result)
  }
  // SKIP-SCALATESTJS,NATIVE-END
}

