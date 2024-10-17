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
package org.scalatest.matchers

import org.scalatest._
import org.scalactic.PrettyMethods
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MatcherProducersSpec extends AnyFunSpec with Matchers {

  val f = be > (_: Int)
  val g = (_: String).toInt
  describe("A Matcher (without MatcherProducers)") {
    it("can be composed via compose ... andThen ... compose") {
      // (f compose g)(x) === f(g(x))
      val beAsIntsGreaterThan = (f compose g) andThen (_ compose g)
      "8" should beAsIntsGreaterThan ("7")
      val tfe = the [TestFailedException] thrownBy {
        "7" should beAsIntsGreaterThan ("8")
      }
      tfe.message should be (Some(Resources.wasNotGreaterThan("7", "8")))
    }
  }
  describe("The MatcherProducers trait") {
    import MatcherProducers._
    it("should enable compose ... andThen ... compose behavior via composeTwice") {
      // (f compose g)(x) === f(g(x))
      val beAsIntsGreaterThan = f composeTwice g
      "8" should beAsIntsGreaterThan ("7")
      val tfe = the [TestFailedException] thrownBy {
        "7" should beAsIntsGreaterThan ("8")
      }
      tfe.message should be (Some(Resources.wasNotGreaterThan("7", "8")))
    }
    it("should enable failure messages to be modified via mapResult") {
      val beAsIntsGreaterThan = f composeTwice g mapResult { mr => MatchResult(mr.matches, mr.failureMessage.toUpperCase, mr.negatedFailureMessage.toUpperCase) }
      "8" should beAsIntsGreaterThan ("7")
      val tfe = the [TestFailedException] thrownBy {
        "7" should beAsIntsGreaterThan ("8")
      }
      tfe.message should be (Some(Resources.wasNotGreaterThan("7", "8").toUpperCase))
    }
    it("should be able to modify failure message args via mapResult") {
      val beAsIntsGreaterThan = f composeTwice g mapResult { mr =>
        mr.copy(
          failureMessageArgs = mr.failureMessageArgs.map((LazyArg(_) { _.toString + ".toInt"})),
          negatedFailureMessageArgs = mr.negatedFailureMessageArgs.map((LazyArg(_) { _.toString + ".toInt"})),
          midSentenceFailureMessageArgs = mr.midSentenceFailureMessageArgs.map((LazyArg(_) { _.toString + ".toInt"})),
          midSentenceNegatedFailureMessageArgs = mr.midSentenceNegatedFailureMessageArgs.map((LazyArg(_) { _.toString + ".toInt"}))
        )
      }
      "8" should beAsIntsGreaterThan ("7")
      val tfe = the [TestFailedException] thrownBy {
        "7" should beAsIntsGreaterThan ("8")
      }
      tfe.message should be (Some(Resources.wasNotGreaterThan("7.toInt", "8.toInt")))
    }
    it("should be able to modify failure message args via mapArgs") {
      val beAsIntsGreaterThan = f composeTwice g mapArgs { _.toString + ".toInt" }
      "8" should beAsIntsGreaterThan ("7")
      val tfe = the [TestFailedException] thrownBy {
        "7" should beAsIntsGreaterThan ("8")
      }
      tfe.message should be (Some(Resources.wasNotGreaterThan("7.toInt", "8.toInt")))
    }
  }
}

