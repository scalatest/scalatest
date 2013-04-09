/*
 * Copyright 2001-2008 Artima, Inc.
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

class PrivateMethodTesterSpec extends FunSpec with PrivateMethodTester {

  describe("The PrivateMethodTester trait") {

    it("should work if the private method takes a String") {

      class Modest {
        private def secret(s: String) = s + " sesame!"
      }
      val secret = PrivateMethod[String]('secret)
      assert(((new Modest) invokePrivate secret("open")) === "open sesame!")
    }

    it("should work if the private method takes an Int") {

      class Modest {
        private def secret(i: Int) = i + 42
      }
      val secret = PrivateMethod[String]('secret)
      assert(((new Modest) invokePrivate secret(1)) === 43)
    }

    it("should work if the private method takes a Boolean") {

      class Modest {
        private def secret(b: Boolean) = !b
      }
      val secret = PrivateMethod[String]('secret)
      assert(((new Modest) invokePrivate secret(true)) === false)
    }

    it("should work if the private method takes a Float") {

      class Modest {
        private def secret(f: Float) = f
      }
      val secret = PrivateMethod[String]('secret)
      assert(((new Modest) invokePrivate secret(1.0f)) === 1.0f)
    }

    it("should work if the private method takes a Double") {

      class Modest {
        private def secret(d: Double) = d
      }
      val secret = PrivateMethod[String]('secret)
      assert(((new Modest) invokePrivate secret(1.0d)) === 1.0d)
    }

    it("should work if the private method takes a Char") {

      class Modest {
        private def secret(c: Char) = c + 1
      }
      val secret = PrivateMethod[String]('secret)
      assert(((new Modest) invokePrivate secret('a')) === 'b')
    }

    it("should work if the private method takes a Short") {

      class Modest {
        private def secret(i: Short) = i + 1
      }
      val secret = PrivateMethod[String]('secret)
      assert(((new Modest) invokePrivate secret(8.asInstanceOf[Short])) === 9)
    }

    it("should work if the private method takes a Byte") {

      class Modest {
        private def secret(i: Byte) = i + 1
      }
      val secret = PrivateMethod[String]('secret)
      assert(((new Modest) invokePrivate secret(8.asInstanceOf[Byte])) === 9)
    }

    it("should work if the private method takes a Long") {

      class Modest {
        private def secret(i: Long) = i + 1
      }
      val secret = PrivateMethod[String]('secret)
      assert(((new Modest) invokePrivate secret(8l)) === 9l)
    }

    it("should work if the private method takes a combination of primitive and reference types") {

      class Modest {
        private def secret(i: Long, s: String, b: Boolean) = i + 1
      }
      val secret = PrivateMethod[String]('secret)
      assert(((new Modest) invokePrivate secret(8l, "hi", false)) === 9l)
    }
  }
}

