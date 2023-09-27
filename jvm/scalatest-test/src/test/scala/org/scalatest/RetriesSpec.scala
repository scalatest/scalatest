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

import time.Span
import time.SpanSugar._
import Retries._
import org.scalactic.{Resources => _, _}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class RetriesSpec extends AnyFunSpec {

  import scala.language.postfixOps

  describe("The Retries trait") {
    
    describe("offers a withRetryOnFailure method that") {

      it("should return Succeeded on Succeeded") {
        var executionCount = 0
        val outcome =
          withRetryOnFailure {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      it("should return Pending on Pending") {
        var executionCount = 0
        val outcome =
          withRetryOnFailure {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      it("should return Canceled on Canceled") {
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetryOnFailure {
            executionCount += 1
            canceled
          }
        outcome should be theSameInstanceAs canceled
        executionCount shouldBe 1
      }
      it("should return first Failed if fails twice") {
        var executionCount = 0
        val firstFailed = Failed()
        val secondFailed = Failed()
        val outcome =
          withRetryOnFailure {
            executionCount += 1
            if (executionCount == 1) firstFailed else secondFailed
          }
        outcome should be theSameInstanceAs firstFailed 
        executionCount shouldBe 2
      }
      it("should return Failed if fails first then gives Pending") { // unlikely case
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetryOnFailure {
            executionCount += 1
            if (executionCount == 1) failed else Pending
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Failed if fails first then gives Canceled") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetryOnFailure {
            executionCount += 1
            if (executionCount == 1) failed else Canceled()
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Canceled if fails first then succeeds") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetryOnFailure {
            executionCount += 1
            if (executionCount == 1) failed else Succeeded
          }
        outcome shouldBe a [Canceled]
        executionCount shouldBe 2
        outcome match {
          case Canceled(ex) =>
            ex.getMessage should be (Resources.testFlickered)
            ex.getCause should be (failed.exception)
          case _ => fail()
        }
      }
    }
    describe("offers a withRetryOnFailure(delay) method that") {
      
      it("should return Succeeded on Succeeded") {
        var executionCount = 0
        val outcome =
          withRetryOnFailure(delay = 1 millisecond) {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      it("should return Pending on Pending") {
        var executionCount = 0
        val outcome =
          withRetryOnFailure(delay = 1 millisecond) {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      it("should return Canceled on Canceled") {
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetryOnFailure(delay = 1 millisecond) {
            executionCount += 1
            canceled
          }
        outcome should be theSameInstanceAs canceled
        executionCount shouldBe 1
      }
      it("should return first Failed if fails twice") {
        var executionCount = 0
        val firstFailed = Failed()
        val secondFailed = Failed()
        val outcome =
          withRetryOnFailure(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) firstFailed else secondFailed
          }
        outcome should be theSameInstanceAs firstFailed 
        executionCount shouldBe 2
      }
      it("should return Failed if fails first then gives Pending") { // unlikely case
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetryOnFailure(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) failed else Pending
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Failed if fails first then gives Canceled") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetryOnFailure(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) failed else Canceled()
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Canceled if fails first then succeeds") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetryOnFailure(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) failed else Succeeded
          }
        outcome shouldBe a [Canceled]
        executionCount shouldBe 2
        outcome match {
          case Canceled(ex) =>
            ex.getMessage should be (Resources.testFlickered)
            ex.getCause should be (failed.exception)
          case _ => fail()
        }
      }
    }
    describe("offers a withRetryOnCancel method that") {

      it("should return Succeeded on Succeeded") {
        var executionCount = 0
        val outcome =
          withRetryOnCancel {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      it("should return Pending on Pending") {
        var executionCount = 0
        val outcome =
          withRetryOnCancel {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      it("should return Failed on Failed") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetryOnCancel {
            executionCount += 1
            failed
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 1
      }
      it("should return first Canceled if cancels twice") {
        var executionCount = 0
        val firstCanceled = Canceled()
        val secondCanceled = Canceled()
        val outcome =
          withRetryOnCancel {
            executionCount += 1
            if (executionCount == 1) firstCanceled else secondCanceled
          }
        outcome should be theSameInstanceAs firstCanceled 
        executionCount shouldBe 2
      }
      it("should return Canceled if cancels first then gives Pending") { // unlikely case
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetryOnCancel {
            executionCount += 1
            if (executionCount == 1) canceled else Pending
          }
        outcome should be theSameInstanceAs canceled
        executionCount shouldBe 2
      }
      it("should return Failed if cancels first then fails") {
        var executionCount = 0
        val canceled = Canceled()
        val failed = Failed()
        val outcome =
          withRetryOnCancel {
            executionCount += 1
            if (executionCount == 1) canceled else failed
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Succeeded if cancels first then succeeds") {
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetryOnCancel {
            executionCount += 1
            if (executionCount == 1) canceled else Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 2
      }
    }
    describe("offers a withRetryOnCancel(delay) method that") {

      it("should return Succeeded on Succeeded") {
        var executionCount = 0
        val outcome =
          withRetryOnCancel(delay = 1 millisecond) {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      it("should return Pending on Pending") {
        var executionCount = 0
        val outcome =
          withRetryOnCancel(delay = 1 millisecond) {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      it("should return Failed on Failed") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetryOnCancel(delay = 1 millisecond) {
            executionCount += 1
            failed
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 1
      }
      it("should return first Canceled if cancels twice") {
        var executionCount = 0
        val firstCanceled = Canceled()
        val secondCanceled = Canceled()
        val outcome =
          withRetryOnCancel(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) firstCanceled else secondCanceled
          }
        outcome should be theSameInstanceAs firstCanceled 
        executionCount shouldBe 2
      }
      it("should return Canceled if cancels first then gives Pending") { // unlikely case
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetryOnCancel(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) canceled else Pending
          }
        outcome should be theSameInstanceAs canceled
        executionCount shouldBe 2
      }
      it("should return Failed if cancels first then fails") {
        var executionCount = 0
        val canceled = Canceled()
        val failed = Failed()
        val outcome =
          withRetryOnCancel(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) canceled else failed
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Succeeded if cancels first then succeeds") {
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetryOnCancel(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) canceled else Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 2
      }
    }
    describe("offers a withRetry method that") {
      it("should return Succeeded on Succeeded") {
        var executionCount = 0
        val outcome =
          withRetry {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      it("should return Pending on Pending") {
        var executionCount = 0
        val outcome =
          withRetry {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      it("should return first Failed if fails twice") {
        var executionCount = 0
        val firstFailed = Failed()
        val secondFailed = Failed()
        val outcome =
          withRetry {
            executionCount += 1
            if (executionCount == 1) firstFailed else secondFailed
          }
        outcome should be theSameInstanceAs firstFailed 
        executionCount shouldBe 2
      }
      it("should return Failed if fails first then gives Pending") { // unlikely case
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetry {
            executionCount += 1
            if (executionCount == 1) failed else Pending
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Failed if fails first then gives Canceled") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetry {
            executionCount += 1
            if (executionCount == 1) failed else Canceled()
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Canceled if fails first then succeeds") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetry {
            executionCount += 1
            if (executionCount == 1) failed else Succeeded
          }
        outcome shouldBe a [Canceled]
        executionCount shouldBe 2
        outcome match {
          case Canceled(ex) =>
            ex.getMessage should be (Resources.testFlickered)
            ex.getCause should be (failed.exception)
          case _ => fail()
        }
      }
      it("should return first Canceled if cancels twice") {
        var executionCount = 0
        val firstCanceled = Canceled()
        val secondCanceled = Canceled()
        val outcome =
          withRetry {
            executionCount += 1
            if (executionCount == 1) firstCanceled else secondCanceled
          }
        outcome should be theSameInstanceAs firstCanceled 
        executionCount shouldBe 2
      }
      it("should return Canceled if cancels first then gives Pending") { // unlikely case
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetry {
            executionCount += 1
            if (executionCount == 1) canceled else Pending
          }
        outcome should be theSameInstanceAs canceled
        executionCount shouldBe 2
      }
      it("should return Failed if cancels first then fails") {
        var executionCount = 0
        val canceled = Canceled()
        val failed = Failed()
        val outcome =
          withRetry {
            executionCount += 1
            if (executionCount == 1) canceled else failed
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Succeeded if cancels first then succeeds") {
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetry {
            executionCount += 1
            if (executionCount == 1) canceled else Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 2
      }
    }
    describe("offers a withRetry(delay) method that") {
      it("should return Succeeded on Succeeded") {
        var executionCount = 0
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      it("should return Pending on Pending") {
        var executionCount = 0
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      it("should return first Failed if fails twice") {
        var executionCount = 0
        val firstFailed = Failed()
        val secondFailed = Failed()
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) firstFailed else secondFailed
          }
        outcome should be theSameInstanceAs firstFailed 
        executionCount shouldBe 2
      }
      it("should return Failed if fails first then gives Pending") { // unlikely case
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) failed else Pending
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Failed if fails first then gives Canceled") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) failed else Canceled()
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Canceled if fails first then succeeds") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) failed else Succeeded
          }
        outcome shouldBe a [Canceled]
        executionCount shouldBe 2
        outcome match {
          case Canceled(ex) =>
            ex.getMessage should be (Resources.testFlickered)
            ex.getCause should be (failed.exception)
          case _ => fail()
        }
      }
      it("should return first Canceled if cancels twice") {
        var executionCount = 0
        val firstCanceled = Canceled()
        val secondCanceled = Canceled()
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) firstCanceled else secondCanceled
          }
        outcome should be theSameInstanceAs firstCanceled 
        executionCount shouldBe 2
      }
      it("should return Canceled if cancels first then gives Pending") { // unlikely case
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) canceled else Pending
          }
        outcome should be theSameInstanceAs canceled
        executionCount shouldBe 2
      }
      it("should return Failed if cancels first then fails") {
        var executionCount = 0
        val canceled = Canceled()
        val failed = Failed()
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) canceled else failed
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 2
      }
      it("should return Succeeded if cancels first then succeeds") {
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            if (executionCount == 1) canceled else Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 2
      }
    }
    describe("offers a withRetries(delay, maxRetries) method that") {
      it("should return Succeeded on Succeeded") {
        var executionCount = 0
        val outcome =
          withRetries(delay = 1 millisecond, 3) {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      it("should return Pending on Pending") {
        var executionCount = 0
        val outcome =
          withRetries(delay = 1 millisecond, 3) {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      it("should return first Failed if fails 3 times") {
        var executionCount = 0
        val firstFailed = Failed()
        val secondFailed = Failed()
        val thirdFailed = Failed()
        val outcome =
          withRetries(delay = 1 millisecond, 3) {
            executionCount += 1
            if (executionCount == 1) 
              firstFailed 
            else if (executionCount == 2) 
              secondFailed
            else
              thirdFailed  
          }
        outcome should be theSameInstanceAs firstFailed 
        executionCount shouldBe 4 // 4 because the original call + 3 retries.
      }
      it("should return Failed if fails first then gives Pending") { // unlikely case
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetries(delay = 1 millisecond, 3) {
            executionCount += 1
            if (executionCount < 3) failed else Pending
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 3 // 3 because the original call + 2 retries until it gets Pending.
      }
      it("should return Failed if fails first then gives Canceled") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetries(delay = 1 millisecond, 3) {
            executionCount += 1
            if (executionCount < 3) failed else Canceled()
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 4 // 4 because the original call + 4 retries until it gets 2x Canceled.
      }
      it("should return Canceled if fails first then succeeds") {
        var executionCount = 0
        val failed = Failed()
        val outcome =
          withRetries(delay = 1 millisecond, 3) {
            executionCount += 1
            if (executionCount < 3) failed else Succeeded
          }
        outcome shouldBe a [Canceled]
        executionCount shouldBe 3 // 3 because the original call + 2 retries until it gets Canceled.
        outcome match {
          case Canceled(ex) =>
            ex.getMessage should be (Resources.testFlickered)
            ex.getCause should be (failed.exception)
          case _ => fail()
        }
      }
      it("should return first Canceled if cancels continuously") {
        var executionCount = 0
        val firstCanceled = Canceled()
        val secondCanceled = Canceled()
        val outcome =
          withRetries(delay = 1 millisecond, 3) {
            executionCount += 1
            if (executionCount == 1) firstCanceled else secondCanceled
          }
        outcome should be theSameInstanceAs firstCanceled 
        executionCount shouldBe 4 // 4 because the original call + 4 retries until it gets 2x Canceled.
      }
      it("should return Canceled if cancels first then gives Pending") { // unlikely case
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetries(delay = 1 millisecond, 3) {
            executionCount += 1
            if (executionCount < 3) canceled else Pending
          }
        outcome should be theSameInstanceAs canceled
        executionCount shouldBe 3 // 3 because the original call + 2 retries until it gets Pending.
      }
      it("should return Failed if cancels first then fails") {
        var executionCount = 0
        val canceled = Canceled()
        val failed = Failed()
        val outcome =
          withRetries(delay = 1 millisecond, 3) {
            executionCount += 1
            if (executionCount < 3) canceled else failed
          }
        outcome should be theSameInstanceAs failed
        executionCount shouldBe 4 // 4 because the original call + 4 retries until it gets failed.
      }
      it("should return Succeeded if cancels first then succeeds") {
        var executionCount = 0
        val canceled = Canceled()
        val outcome =
          withRetries(delay = 1 millisecond, 3) {
            executionCount += 1
            if (executionCount < 4) canceled else Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 4 // 4 because the original call + 4 retries until it gets succeed.
      }
    }
    describe("offers an isRetryable method that") {
      it("should indicate given a TestData whether a test is tagged with org.scalatest.tags.Retryable") {
        val yes = new TestData {
          val configMap: ConfigMap = ConfigMap.empty
          val name: String = "i am retryable"
          val scopes: collection.immutable.IndexedSeq[String] = Vector.empty
          val text: String = name
          val tags: Set[String] = Set("org.scalatest.tags.Retryable")
          val pos: Option[source.Position] = Some(source.Position.here)
        }
        val no = new TestData {
          val configMap: ConfigMap = ConfigMap.empty
          val name: String = "i am not retryable"
          val scopes: collection.immutable.IndexedSeq[String] = Vector.empty
          val text: String = name
          val tags: Set[String] = Set("NotMe")
          val pos = Some(source.Position.here)
        }

        isRetryable(yes) shouldBe true
        isRetryable(no) shouldBe false
      }
    }
  }
}

