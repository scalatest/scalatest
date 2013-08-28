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

import exceptions.{GeneratorDrivenPropertyCheckFailedException, TableDrivenPropertyCheckFailedException, TestFailedDueToTimeoutException, TestFailedException, StackDepth, TestCanceledException}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitTestFailedError
import prop.{TableDrivenPropertyChecks, TableFor1}
import time.{Span, Second}
import SharedHelpers.EventRecordingReporter

// TODO: Test with imported AppendedClues
class AppendedCluesSpec extends FlatSpec with ShouldMatchers with AppendedClues with TableDrivenPropertyChecks  with SeveredStackTraces {

  def examples: TableFor1[Throwable with ModifiableMessage[_ <: StackDepth]] =
    Table(
      "exception",
      new TestFailedException("message", 3),
      new JUnitTestFailedError("message", 3),
      new TestFailedDueToTimeoutException(e => Some("message"), None, e => 3, None, Span(1, Second)),
      new TableDrivenPropertyCheckFailedException(e => "message", None, e => 3, None, "undecMsg", List.empty, List.empty, 3),
      new GeneratorDrivenPropertyCheckFailedException(e => "message", None, e => 3, None, "undecMsg", List.empty, Option(List.empty), List.empty)
    )


  def failWith(e: Throwable) { throw e }

  it should "return the new exception with the clue string appended, separated by a space char if passed a function that does that" in {
    forAll (examples) { e =>
      val clue = "clue"
      val fun: (Option[String] => Option[String]) =
        opt => opt match {
          case Some(msg) => Some(msg + " " + clue)
          case None => Some(clue)
        }
      e.modifyMessage(fun).message.get should be("message clue")
    }
  }

  "The withClue construct" should "allow any non-ModifiableMessage exception to pass through" in {
    val iae = new IllegalArgumentException
    val caught = intercept[IllegalArgumentException] {
      { failWith(iae) } withClue "howdy"
    }
    caught should be theSameInstanceAs (iae)
  }

  it should "given an empty clue string, rethrow the same exception" in {
    forAll (examples) { e =>
      val caught = intercept[Throwable] {
        {
          failWith(e)
        } withClue ""
      }
      caught should be theSameInstanceAs (e)
    }
  }

  it should "given a non-string clue object with an empty clue string, rethrow the same exception" in {
    forAll (examples) { e =>
      class VeryUnlikely {
        override def toString = ""
      }
      val caught = intercept[Throwable] {
        {
          failWith(e)
        } withClue (new VeryUnlikely)
      }
      caught should be theSameInstanceAs (e)
    }
  }

  it should "given an all-whitespace clue string, should throw a new exception with the white space appended to the old message" in {
    forAll (examples) { e =>
      val white = "    "
      val caught = intercept[StackDepth] {
        {
          failWith(e)
        } withClue white
      }
      caught should not be theSameInstanceAs(e)
      caught.message should be('defined)
      caught.message.get should equal("message" + white)
    }
  }

  it should "given a non-empty clue string with no leading white space, throw a new instance of the caught exception that has all fields the same except an appended clue string preceded by an extra space" in {
    forAll (examples) { e =>
      val caught = intercept[StackDepth] {
        {
          failWith(e)
        } withClue "clue"
      }
      caught should not be theSameInstanceAs(e)
      caught.message should be('defined)
      caught.message.get should equal("message clue")
    }
  }

  it should "given a non-empty clue string with a leading space, throw a new instance of the caught exception that has all fields the same except an appended clue string (preceded by no extra space)" in {
    forAll (examples) { e =>
      val caught = intercept[StackDepth] {
        {
          failWith(e)
        } withClue " clue" // has a leading space
      }
      caught should not be theSameInstanceAs(e)
      caught.message should be('defined)
      caught.message.get should equal("message clue")
    }
  }

  it should "given a non-empty clue string preceded by an end of line, throw a new instance of the caught exception that has all fields the same except an appended clue string (preceded by no extra space)" in {
    forAll (examples) { e =>
      val caught = intercept[StackDepth] {
        {
          failWith(e)
        } withClue "\nclue" // has an end of line character
      }
      caught should not be theSameInstanceAs(e)
      caught.message should be('defined)
      caught.message.get should equal("message\nclue")
    }
  }

  it should "given a non-empty clue string preceded by a period (.), throw a new instance of the caught TFE exception that has all fields the same except an appended clue string (preceded by no extra space)" in {
    forAll (examples) { e =>
      val caught = intercept[StackDepth] {
        {
          failWith(e)
        } withClue ". clue" // has an end of line character
      }
      caught should not be theSameInstanceAs(e)
      caught.message should be('defined)
      caught.message.get should equal("message. clue")
    }
  }

  it should "given a non-empty clue string preceded by a comma (,), throw a new instance of the caught TFE exception that has all fields the same except an appended clue string (preceded by no extra space)" in {
    forAll (examples) { e =>
      val caught = intercept[StackDepth] {
        {
          failWith(e)
        } withClue ", clue" // has an end of line character
      }
      caught should not be theSameInstanceAs(e)
      caught.message should be('defined)
      caught.message.get should equal("message, clue")
    }
  }

  it should "given a non-empty clue string preceded by a semicolon (;), throw a new instance of the caught TFE exception that has all fields the same except an appended clue string (preceded by no extra space)" in {
    forAll (examples) { e =>
      val caught = intercept[StackDepth] {
        {
          failWith(e)
        } withClue ", clue" // has an end of line character
      }
      caught should not be theSameInstanceAs(e)
      caught.message should be('defined)
      caught.message.get should equal("message, clue")
    }
  }

  it should "given an object with a non-empty clue string with no leading white space, throw a new instance of the caught TFE exception that has all fields the same except a appended clue string preceded by an extra space" in {
    forAll (examples) { e =>
      val list = List(1, 2, 3)
      val caught = intercept[StackDepth] {
        {
          failWith(e)
        } withClue (list)
      }
      caught should not be theSameInstanceAs(e)
      caught.message should be('defined)
      caught.message.get should equal("message List(1, 2, 3)")
    }
  }

  it should "pass the last value back" in {
    val result = 3 withClue("hi")
    result should equal (3)
  }

  it should "throw NPE if a null clue object is passed" in {
    forAll (examples) { e =>
      intercept[NullPointerException] {
        {
          failWith(e)
        } withClue (null)
      }
    }
  }
  
  it should "infer the type of the result of the passed in function" in {
    val result: Int = { 22 } withClue ("hi")
    assert(result === 22)
  }
  
  it should "be able to accept by-name payload" in {
    val result: String = { "hello" } withClue (() => 128)
    assert(result === "hello")
  }
  
  it should "work when used in withFixture" in {
    forAll(examples) { e => 
      val a = 
        new org.scalatest.fixture.FunSpec {
          type FixtureParam = String
        
          override def withFixture(test: OneArgTest) = {
            test("something") withClue("a clue")
          }
        
          it("should do something") { p => 
            throw e
          }
        }
      val rep = new EventRecordingReporter()
      a.run(None, Args(rep))
      rep.testFailedEventsReceived.length should be (1)
      rep.testFailedEventsReceived(0).message should be ("message a clue")
    }
  }
  
  it should "return Failed that contains TestFailedException and with appended clue" in {
    val failed = Failed(new TestFailedException("message", 3))
    val result = { failed } withClue("a clue")
    result shouldBe a [Failed]
    result.exception shouldBe a [TestFailedException]
    result.exception.getMessage shouldBe "message a clue"
  }
  
  it should "return original Failed that contains the RuntimeException and without appended clue" in {
    val failed = Failed(new RuntimeException("message"))
    val result = { failed } withClue("a clue") 
    result should be theSameInstanceAs failed
    result.exception.getMessage shouldBe "message"
  }
  
  it should "return Canceled that contains TestCanceledException and with appended clue" in {
    val canceled = Canceled(new TestCanceledException("message", 3))
    val result = { canceled } withClue("a clue")
    result shouldBe a [Canceled]
    result.exception shouldBe a [TestCanceledException]
    result.exception.getMessage shouldBe "message a clue"
  }
  
  it should "return original Canceled that contains the RuntimeException and without appended clue" in {
    val re = new RuntimeException("message")
    val canceled = Canceled(re)
    val result = { canceled } withClue("a clue")
    result.exception.getCause should be theSameInstanceAs re
    result.exception.getMessage shouldBe "message a clue"
  }

  it should "return original Pending" in {
    val pending = Pending
    val result = { pending } withClue("a clue")
    result should be theSameInstanceAs pending
  }
  
  it should "return original Succeeded" in {
    val succeeded = Succeeded
    val result = { succeeded } withClue("a clue")
    result should be theSameInstanceAs succeeded
  }
}

