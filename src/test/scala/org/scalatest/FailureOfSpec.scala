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

/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
*/

class FailureOfSpec extends FunSpec with FailureOf {

  describe("The failureOf method") {
    it("should return None if no exception is thrown") {
      val result = failureOf { assert(1 + 1 === 2) }
      assert(result === None)
    }
    it("should return the exception if TestFailedException is thrown") {
      val result = failureOf { assert(1 + 1 === 3) }
      assert(result.isDefined)
      assert(result.get.isInstanceOf[TestFailedException])
    }
    it("if UnknownError is thrown, should complete abruptly with that exception") {
      intercept[UnknownError] {
        failureOf { throw new UnknownError }
      }
    }
  }
}
