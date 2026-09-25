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
package org.scalactic

import org.scalatest._

import scala.util.{Failure, Success, Try}
import scala.util.control.ControlThrowable

class UsingSpec extends funspec.AnyFunSpec with NonImplicitAssertions {

  class CloseThrowingResource(ex: => Throwable) extends AutoCloseable {
    var closed = false
    def close(): Unit = {
      closed = true
      if (ex != null) throw ex
    }
  }

  private def suppressionOutcome(bodyEx: Throwable, closeEx: Throwable): Throwable = {
    val resource = new CloseThrowingResource(closeEx)
    try {
      Using.resource(resource) { _ => throw bodyEx }
      throw new AssertionError("expected an exception to be thrown")
    }
    catch {
      case t: Throwable => t
    }
  }

  describe("Using.apply") {

    it("should perform an operation and return the result in a Success") {
      val resource = new CloseThrowingResource(null)
      val result = Using(resource) { _ => 42 }
      assert(result == Success(42))
      assert(resource.closed)
    }

    it("should return a Failure if the operation throws") {
      val result = Using(new CloseThrowingResource(null)) { _ => throw new RuntimeException("op") }
      assert(result.isInstanceOf[Failure[_]])
    }

    it("should throw a NullPointerException if the resource is null") {
      val resource: CloseThrowingResource = null
      val result = Using(resource) { r => r }
      assert(result.isInstanceOf[Failure[_]])
    }
  }

  describe("Using.resource") {

    it("should release the resource when the operation succeeds") {
      val resource = new CloseThrowingResource(null)
      val result = Using.resource(resource) { r => r.closed; 3 }
      assert(result == 3)
      assert(resource.closed)
    }

    it("should release the resource even when the operation throws") {
      val resource = new CloseThrowingResource(null)
      val thrown = intercept[RuntimeException] {
        Using.resource(resource) { _ => throw new RuntimeException("body") }
      }
      assert(thrown.getMessage == "body")
      assert(resource.closed)
    }

    it("should throw a NullPointerException if the resource is null") {
      val resource: CloseThrowingResource = null
      intercept[NullPointerException] {
        Using.resource(resource) { r => r }
      }
    }

    it("should propagate the release exception when the operation succeeds but release throws") {
      val resource = new CloseThrowingResource(new RuntimeException("close"))
      val thrown = intercept[RuntimeException] {
        Using.resource(resource) { _ => 3 }
      }
      assert(thrown.getMessage == "close")
    }

    it("should combine operation and release exceptions via suppression") {
      val thrown = suppressionOutcome(new RuntimeException("body"), new RuntimeException("close"))
      assert(thrown.getMessage == "body")
    }
  }

  describe("Using.resources") {

    it("should manage two resources, releasing them in reverse order") {
      val r1 = new CloseThrowingResource(null)
      val r2 = new CloseThrowingResource(null)
      val result = Using.resources(r1, r2) { (a, b) => (a, b) }
      assert(result._1 eq r1)
      assert(result._2 eq r2)
      assert(r2.closed)
      assert(r1.closed)
    }

    it("should manage three resources") {
      val r1 = new CloseThrowingResource(null)
      val r2 = new CloseThrowingResource(null)
      val r3 = new CloseThrowingResource(null)
      val result = Using.resources(r1, r2, r3) { (a, b, c) => (a, b, c) }
      assert(result._1 eq r1)
      assert(result._2 eq r2)
      assert(result._3 eq r3)
    }

    it("should manage four resources") {
      val r1 = new CloseThrowingResource(null)
      val r2 = new CloseThrowingResource(null)
      val r3 = new CloseThrowingResource(null)
      val r4 = new CloseThrowingResource(null)
      val result = Using.resources(r1, r2, r3, r4) { (a, b, c, d) => (a, b, c, d) }
      assert(result._1 eq r1)
      assert(result._2 eq r2)
      assert(result._3 eq r3)
      assert(result._4 eq r4)
    }
  }

  describe("Using.Manager") {

    it("should manage resources and close them on success") {
      val r1 = new CloseThrowingResource(null)
      val r2 = new CloseThrowingResource(null)
      val result = Using.Manager { mgr =>
        val a = mgr(r1)
        mgr.acquire(r2)
        (a, r2)
      }
      assert(result.isInstanceOf[Success[_]])
      assert(r2.closed)
      assert(r1.closed)
    }

    it("should return a Failure, closing resources, when the operation throws") {
      val r1 = new CloseThrowingResource(null)
      val result = Using.Manager { mgr =>
        mgr.acquire(r1)
        throw new RuntimeException("op")
      }
      assert(result.isInstanceOf[Failure[_]])
      assert(r1.closed)
    }

    it("should throw a NullPointerException if a null resource is acquired") {
      val thrown = Using.Manager { mgr =>
        mgr.acquire[CloseThrowingResource](null)
      }
      assert(thrown.isInstanceOf[Failure[_]])
    }

    it("should throw an IllegalStateException if used after being closed") {
      val manager = new Using.Manager
      manager.close()
      val thrown = intercept[IllegalStateException] {
        manager.acquire(new CloseThrowingResource(null))
      }
      assert(thrown.getMessage == "Manager has already been closed")
    }

    it("should throw an IllegalStateException if closed twice") {
      val manager = new Using.Manager
      manager.close()
      intercept[IllegalStateException] {
        manager.close()
      }
    }

    it("should combine two release exceptions via suppression when they both throw") {
      val r1 = new CloseThrowingResource(new RuntimeException("c1"))
      val r2 = new CloseThrowingResource(new RuntimeException("c2"))
      val thrown = Using.Manager { mgr =>
        mgr.acquire(r1)
        mgr.acquire(r2)
      }.failed.get
      assert(thrown != null)
    }
  }

  describe("preferential suppression") {

    it("should prefer a higher-severity secondary exception over a non-fatal primary") {
      val thrown = suppressionOutcome(new RuntimeException("normal"), new InternalError("oome"))
      assert(thrown.isInstanceOf[InternalError])
    }

    it("should prefer a non-fatal primary over a lower-severity secondary") {
      val thrown = suppressionOutcome(new InternalError("oome"), new RuntimeException("normal"))
      assert(thrown.isInstanceOf[InternalError])
    }

    it("should rank a LinkageError above a non-fatal exception") {
      val thrown = suppressionOutcome(new RuntimeException("normal"), new LinkageError("link"))
      assert(thrown.isInstanceOf[LinkageError])
    }

    it("should rank an InterruptedException above a non-fatal exception") {
      val thrown = suppressionOutcome(new RuntimeException("normal"), new InterruptedException("interrupt"))
      assert(thrown.isInstanceOf[InterruptedException])
    }

    it("should rank a ControlThrowable above a non-fatal exception") {
      val thrown = suppressionOutcome(new RuntimeException("normal"), new ControlThrowable {})
      assert(thrown.isInstanceOf[ControlThrowable])
    }

    it("should keep the first of two exceptions of equal severity") {
      val body = new RuntimeException("body")
      val close = new RuntimeException("close")
      val thrown = suppressionOutcome(body, close)
      assert(thrown eq body)
    }
  }

  describe("UsingCompat") {

    it("should expose the Releasable type and value") {
      val releasable = UsingCompat.Releasable
      val res = new CloseThrowingResource(new RuntimeException("oops"))
      val thrown = intercept[RuntimeException] {
        implicitly[UsingCompat.Releasable[CloseThrowingResource]].release(res)
      }
      assert(thrown.getMessage == "oops")
      assert(releasable != null)
    }
  }
}
