/*
 * Copyright 2001-2011 Artima, Inc.
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

import exceptions.{GeneratorDrivenPropertyCheckFailedException, TableDrivenPropertyCheckFailedException, TestFailedDueToTimeoutException}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitTestFailedError
import prop.TableDrivenPropertyChecks
import prop.TableFor1
import time.{Second, Span}

/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.ModifiableMessage
*/

class ClueSpec extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks with SeveredStackTraces {

  def examples: TableFor1[Throwable with ModifiableMessage[_ <: StackDepth]] =
    Table(
      "exception",
      new TestFailedException("message", 3),
      new JUnitTestFailedError("message", 3),
      new TestFailedDueToTimeoutException(e => Some("message"), None, e => 3, None, Span(1, Second)),
      new TableDrivenPropertyCheckFailedException(e => "message", None, e => 3, None, "undecMsg", List.empty, List.empty, 3),
      new GeneratorDrivenPropertyCheckFailedException(e => "message", None, e => 3, None, "undecMsg", List.empty, Option(List.empty), List.empty)
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
      caught.message should be ('defined)
      caught.message.get should equal (white + "message")
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
      caught.message should be ('defined)
      caught.message.get should equal ("clue message")
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
      caught.message should be ('defined)
      caught.message.get should equal ("clue message")
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
      caught.message should be ('defined)
      caught.message.get should equal ("clue\nmessage")
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
      caught.message should be ('defined)
      caught.message.get should equal ("List(1, 2, 3) message")
      caught.getClass should be theSameInstanceAs (e.getClass)
    }
  }

  it should "pass the last value back" in {
    val result = withClue("hi") { 3 }
    result should equal (3)
  }

  it should "throw NPE if a null clue object is passed" in {
    forAll (examples) { e =>
      intercept[NullPointerException] {
        withClue (null) {
          throw e
        }
      }
    }
  }
}

