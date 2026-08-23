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
package org.scalatest.concurrent

import org.scalactic.source
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Milliseconds, Span}

import scala.concurrent.Promise

class FuturesPositionSpec extends AnyFunSpec with Matchers with ScalaFutures {

  def neverFuture(): scala.concurrent.Future[Int] = Promise[Int]().future

  def doWhenReady()(using pos: source.Position): Unit = {
    whenReady(neverFuture()) { _ => () }
  }

  def doWhenReadyWithTimeout()(using pos: source.Position): Unit = {
    whenReady(neverFuture(), timeout(Span(30, Millis))) { _ => () }
  }

  def doWhenReadyWithInterval()(using pos: source.Position): Unit = {
    whenReady(neverFuture(), interval(Span(5, Milliseconds))) { _ => () }
  }

  def doWhenReadyWithTimeoutAndInterval()(using pos: source.Position): Unit = {
    whenReady(neverFuture(), timeout(Span(30, Millis)), interval(Span(5, Milliseconds))) { _ => () }
  }

  describe("whenReady") {
    it("should use the Position available in the enclosing scope when reporting location for whenReady") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 55)
      val e =
        try {
          doWhenReady()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(55))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for whenReady with timeout") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 70)
      val e =
        try {
          doWhenReadyWithTimeout()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(70))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for whenReady with interval") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 85)
      val e =
        try {
          doWhenReadyWithInterval()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(85))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for whenReady with timeout and interval") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 100)
      val e =
        try {
          doWhenReadyWithTimeoutAndInterval()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(100))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }
  }
}
