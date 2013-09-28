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

import matchers._
import SharedHelpers._
import time.Span
import time.SpanSugar._

class RetriesSpec extends Spec with Matchers with Retries {

  object `The Retries trait` {
    
    object `offers a withRetryOnFailure method that` {

      def `should return Succeeded on Succeeded` {
        var executionCount = 0
        val outcome =
          withRetryOnFailure {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      def `should return Pending on Pending` {
        var executionCount = 0
        val outcome =
          withRetryOnFailure {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      def `should return Canceled on Canceled` {
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
      def `should return first Failed if fails twice` {
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
      def `should return Failed if fails first then gives Pending` { // unlikely case
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
      def `should return Failed if fails first then gives Canceled` {
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
      def `should return Canceled if fails first then succeeds` {
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
            ex.getMessage should be (Resources("testFlickered"))
            ex.getCause should be (failed.exception)
          case _ => fail()
        }
      }
    }
    object `offers a withRetryOnFailure(delay) method that` {
      
      def `should return Succeeded on Succeeded` {
        var executionCount = 0
        val outcome =
          withRetryOnFailure(delay = 1 millisecond) {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      def `should return Pending on Pending` {
        var executionCount = 0
        val outcome =
          withRetryOnFailure(delay = 1 millisecond) {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      def `should return Canceled on Canceled` {
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
      def `should return first Failed if fails twice` {
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
      def `should return Failed if fails first then gives Pending` { // unlikely case
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
      def `should return Failed if fails first then gives Canceled` {
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
      def `should return Canceled if fails first then succeeds` {
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
            ex.getMessage should be (Resources("testFlickered"))
            ex.getCause should be (failed.exception)
          case _ => fail()
        }
      }
    }
    object `offers a withRetryOnCancel method that` {

      def `should return Succeeded on Succeeded` {
        var executionCount = 0
        val outcome =
          withRetryOnCancel {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      def `should return Pending on Pending` {
        var executionCount = 0
        val outcome =
          withRetryOnCancel {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      def `should return Failed on Failed` {
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
      def `should return first Canceled if cancels twice` {
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
      def `should return Canceled if cancels first then gives Pending` { // unlikely case
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
      def `should return Failed if cancels first then fails` {
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
      def `should return Succeeded if cancels first then succeeds` {
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
    object `offers a withRetryOnCancel(delay) method that` {

      def `should return Succeeded on Succeeded` {
        var executionCount = 0
        val outcome =
          withRetryOnCancel(delay = 1 millisecond) {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      def `should return Pending on Pending` {
        var executionCount = 0
        val outcome =
          withRetryOnCancel(delay = 1 millisecond) {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      def `should return Failed on Failed` {
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
      def `should return first Canceled if cancels twice` {
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
      def `should return Canceled if cancels first then gives Pending` { // unlikely case
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
      def `should return Failed if cancels first then fails` {
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
      def `should return Succeeded if cancels first then succeeds` {
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
    object `offers a withRetry method that` {
      def `should return Succeeded on Succeeded` {
        var executionCount = 0
        val outcome =
          withRetry {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      def `should return Pending on Pending` {
        var executionCount = 0
        val outcome =
          withRetry {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      def `should return first Failed if fails twice` {
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
      def `should return Failed if fails first then gives Pending` { // unlikely case
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
      def `should return Failed if fails first then gives Canceled` {
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
      def `should return Canceled if fails first then succeeds` {
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
            ex.getMessage should be (Resources("testFlickered"))
            ex.getCause should be (failed.exception)
          case _ => fail()
        }
      }
      def `should return first Canceled if cancels twice` {
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
      def `should return Canceled if cancels first then gives Pending` { // unlikely case
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
      def `should return Failed if cancels first then fails` {
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
      def `should return Succeeded if cancels first then succeeds` {
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
    object `offers a withRetry(delay) method that` {
      def `should return Succeeded on Succeeded` {
        var executionCount = 0
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            Succeeded
          }
        outcome shouldBe Succeeded
        executionCount shouldBe 1
      }
      def `should return Pending on Pending` {
        var executionCount = 0
        val outcome =
          withRetry(delay = 1 millisecond) {
            executionCount += 1
            Pending
          }
        outcome shouldBe Pending
        executionCount shouldBe 1
      }
      def `should return first Failed if fails twice` {
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
      def `should return Failed if fails first then gives Pending` { // unlikely case
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
      def `should return Failed if fails first then gives Canceled` {
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
      def `should return Canceled if fails first then succeeds` {
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
            ex.getMessage should be (Resources("testFlickered"))
            ex.getCause should be (failed.exception)
          case _ => fail()
        }
      }
      def `should return first Canceled if cancels twice` {
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
      def `should return Canceled if cancels first then gives Pending` { // unlikely case
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
      def `should return Failed if cancels first then fails` {
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
      def `should return Succeeded if cancels first then succeeds` {
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
    object `offers an isRetryable method that` {
      def `should indicate given a TestData whether a test is tagged with org.scalatest.tags.Retryable` {
        val yes = new TestData {
          val configMap: ConfigMap = ConfigMap.empty
          val name: String = "i am retryable"
          val scopes: collection.immutable.IndexedSeq[String] = Vector.empty
          val text: String = name
          val tags: Set[String] = Set("org.scalatest.tags.Retryable")
        }
        val no = new TestData {
          val configMap: ConfigMap = ConfigMap.empty
          val name: String = "i am not retryable"
          val scopes: collection.immutable.IndexedSeq[String] = Vector.empty
          val text: String = name
          val tags: Set[String] = Set("NotMe")
        }

        isRetryable(yes) shouldBe true
        isRetryable(no) shouldBe false
      }
    }
  }
}

