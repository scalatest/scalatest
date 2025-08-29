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

class NormMethodsSpec extends funspec.AnyFunSpec with StringNormalizations with NormMethods {

  describe("trait NormMethods") {
    it("should provide a .norm method for any type T for which an implicit Normalization[T] is available") {

      { // Try with lowerCased
        implicit val strNorm: Normalization[String] = lowerCased
        assert("Hello".norm === "hello")
        assert("hello".norm === "hello")
        assert("".norm === "")
        assert("HELLO".norm === "hello")
      }

      { // Try with trimmed
        implicit val strNorm: Normalization[String] = trimmed
        assert("hello".norm === "hello")
        assert(" hello".norm === "hello")
        assert("hello ".norm === "hello")
        assert("\nhello".norm === "hello")
        assert("hello\n".norm === "hello")
        assert("\n".norm === "")
        assert("  ".norm === "")
        assert("".norm === "")
      }

      { // Try with upperCased
        implicit val strNorm: Normalization[String] = upperCased
        assert("Hello".norm === "HELLO")
        assert("hello".norm === "HELLO")
        assert("".norm === "")
        assert("HELLO".norm === "HELLO")
      }

      { // Try with a type other than String
        implicit val intNorm: Normalization[Int] =
          new Normalization[Int] {
            def normalized(i: Int) = i.abs
          }
        assert((3).norm === 3)
        assert((-3).norm === 3)
      }
    }
  }
}

