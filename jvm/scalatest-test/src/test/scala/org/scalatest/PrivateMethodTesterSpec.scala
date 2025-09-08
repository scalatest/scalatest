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
package org.scalatest

import PrivateMethodTester._
import org.scalatest.funspec.AnyFunSpec

class PrivateMethodTesterSpec extends AnyFunSpec {

  describe("The PrivateMethodTester trait") {

    describe("using old PrivateMethod unsafe invocation") {

      it("should work if the private method takes a String") {

        class Modest {
          private def secret(s: String) = s + " sesame!"
        }
        val secret = PrivateMethod[String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open")) === "open sesame!")
      }

      it("should work if the private method takes an Int") {

        class Modest {
          private def secret(i: Int) = i + 42
        }
        val secret = PrivateMethod[Int](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(1)) === 43)
      }

      it("should work if the private method takes a Boolean") {

        class Modest {
          private def secret(b: Boolean) = !b
        }
        val secret = PrivateMethod[Boolean](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(true)) === false)
      }

      it("should work if the private method takes a Float") {

        class Modest {
          private def secret(f: Float) = f
        }
        val secret = PrivateMethod[Float](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(1.0f)) === 1.0f)
      }

      it("should work if the private method takes a Double") {

        class Modest {
          private def secret(d: Double) = d
        }
        val secret = PrivateMethod[Double](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(1.0d)) === 1.0d)
      }

      it("should work if the private method takes a Char") {

        class Modest {
          private def secret(c: Char) = c + 1
        }
        val secret = PrivateMethod[Int](Symbol("secret"))
        assert(((new Modest) invokePrivate secret('a')) === 'b')
      }

      it("should work if the private method takes a Short") {

        class Modest {
          private def secret(i: Short) = i + 1
        }
        val secret = PrivateMethod[Int](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(8.asInstanceOf[Short])) === 9)
      }

      it("should work if the private method takes a Byte") {

        class Modest {
          private def secret(i: Byte) = i + 1
        }
        val secret = PrivateMethod[Int](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(8.asInstanceOf[Byte])) === 9)
      }

      it("should work if the private method takes a Long") {

        class Modest {
          private def secret(i: Long) = i + 1
        }
        val secret = PrivateMethod[Long](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(8l)) === 9l)
      }

      it("should work if the private method takes a combination of primitive and reference types") {

        class Modest {
          private def secret(i: Long, s: String, b: Boolean) = i + 1
        }
        val secret = PrivateMethod[Long](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(8l, "hi", false)) === 9l)
      }

      it("should work if the private method is passed a null") {
        class Modest {
          private def secret(s: String) = Option(s).getOrElse("open") + " sesame!"
        }
        val secret = PrivateMethod[String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(null)) === "open sesame!")
      }

    }

    describe("using new PrivateMethodX safe invocation") {

      it("should work if the private method takes a String") {

        class Modest {
          private def secret(s: String) = s + " sesame!"
        }
        val secret = PrivateMethod1[String, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open")) === "open sesame!")
      }

      it("should work if the private method takes an Int") {

        class Modest {
          private def secret(i: Int) = i + 42
        }
        val secret = PrivateMethod1[Int, Int](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(1)) === 43)
      }

      it("should work if the private method takes a Boolean") {

        class Modest {
          private def secret(b: Boolean) = !b
        }
        val secret = PrivateMethod1[Boolean, Boolean](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(true)) === false)
      }

      it("should work if the private method takes a Float") {

        class Modest {
          private def secret(f: Float) = f
        }
        val secret = PrivateMethod1[Float, Float](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(1.0f)) === 1.0f)
      }

      it("should work if the private method takes a Double") {

        class Modest {
          private def secret(d: Double) = d
        }
        val secret = PrivateMethod1[Double, Double](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(1.0d)) === 1.0d)
      }

      it("should work if the private method takes a Char") {

        class Modest {
          private def secret(c: Char) = c + 1
        }
        val secret = PrivateMethod1[Char, Int](Symbol("secret"))
        assert(((new Modest) invokePrivate secret('a')) === 'b')
      }

      it("should work if the private method takes a Short") {

        class Modest {
          private def secret(i: Short) = i + 1
        }
        val secret = PrivateMethod1[Short, Int](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(8.asInstanceOf[Short])) === 9)
      }

      it("should work if the private method takes a Byte") {

        class Modest {
          private def secret(i: Byte) = i + 1
        }
        val secret = PrivateMethod1[Byte, Int](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(8.asInstanceOf[Byte])) === 9)
      }

      it("should work if the private method takes a Long") {

        class Modest {
          private def secret(i: Long) = i + 1
        }
        val secret = PrivateMethod1[Long, Long](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(8l)) === 9l)
      }

      it("should work if the private method takes a combination of primitive and reference types") {

        class Modest {
          private def secret(i: Long, s: String, b: Boolean) = i + 1
        }
        val secret = PrivateMethod3[Long, String, Boolean, Long](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(8l, "hi", false)) === 9l)
      }

      it("should work if the private method is passed a null") {
        class Modest {
          private def secret(s: String) = Option(s).getOrElse("open") + " sesame!"
        }
        val secret = PrivateMethod1[String, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret(null)) === "open sesame!")
      }

      it("should work if the private method takes 4 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String): String = a1 + " sesame!"
        }
        val secret = PrivateMethod4[String, Int, Float, String, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test")) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, 123)""")
      }

      it("should work if the private method takes 5 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int): String = a1 + " sesame!"
        }
        val secret = PrivateMethod5[String, Int, Float, String, Int, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", "123")""")
      }

      it("should work if the private method takes 6 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float): String = a1 + " sesame!"
        }
        val secret = PrivateMethod6[String, Int, Float, String, Int, Float, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, "5.0f")""")
      }

      it("should work if the private method takes 7 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String): String = a1 + " sesame!"
        }
        val secret = PrivateMethod7[String, Int, Float, String, Int, Float, String, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2")) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, 123)""")
      }

      it("should work if the private method takes 8 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int): String = a1 + " sesame!"
        }
        val secret = PrivateMethod8[String, Int, Float, String, Int, Float, String, Int, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", "456")""")
      }

      it("should work if the private method takes 9 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float): String = a1 + " sesame!"
        }
        val secret = PrivateMethod9[String, Int, Float, String, Int, Float, String, Int, Float, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, "6.0f")""")
      }

      it("should work if the private method takes 10 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String): String = a1 + " sesame!"
        }
        val secret = PrivateMethod10[String, Int, Float, String, Int, Float, String, Int, Float, String, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3")) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, 3)""")
      }

      it("should work if the private method takes 11 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String, 
                            a11: Int): String = a1 + " sesame!"
        }
        val secret = PrivateMethod11[String, Int, Float, String, Int, Float, String, Int, Float, String, Int, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", "789")""")
      }

      it("should work if the private method takes 12 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String, 
                            a11: Int, a12: Float): String = a1 + " sesame!"
        }
        val secret = PrivateMethod12[String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, 7.0f)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, "7.0f")""")
      }

      it("should work if the private method takes 13 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String, 
                            a11: Int, a12: Float, a13: String): String = a1 + " sesame!"
        }
        val secret = PrivateMethod13[String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, 7.0f, "test4")) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, "7.0f", 4)""")
      }

      it("should work if the private method takes 14 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String, 
                            a11: Int, a12: Float, a13: String, a14: Int): String = a1 + " sesame!"
        }
        val secret = PrivateMethod14[String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, 7.0f, "test4", 123)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, "7.0f", "test4", "123")""")
      }

      it("should work if the private method takes 15 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String, 
                            a11: Int, a12: Float, a13: String, a14: Int, a15: Float): String = a1 + " sesame!"
        }
        val secret = PrivateMethod15[String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, 7.0f, "test4", 123, 8.0f)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, "7.0f", "test4", 123, "8.0f")""")
      }

      it("should work if the private method takes 16 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String, 
                            a11: Int, a12: Float, a13: String, a14: Int, a15: Float, a16: String): String = a1 + " sesame!"
        }
        val secret = PrivateMethod16[String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, 7.0f, "test4", 123, 8.0f, "test5")) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, "7.0f", "test4", 123, 8.0f, 5)""")
      }

      it("should work if the private method takes 17 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String, 
                            a11: Int, a12: Float, a13: String, a14: Int, a15: Float, a16: String, a17: Int): String = a1 + " sesame!"
        }
        val secret = PrivateMethod17[String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, 7.0f, "test4", 123, 8.0f, "test5", 456)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, "7.0f", "test4", 123, 8.0f, "test5", "456")""")
      }

      it("should work if the private method takes 18 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String, 
                            a11: Int, a12: Float, a13: String, a14: Int, a15: Float, a16: String, a17: Int, a18: Float): String = a1 + " sesame!"
        }
        val secret = PrivateMethod18[String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, 7.0f, "test4", 123, 8.0f, "test5", 456, 9.0f)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, "7.0f", "test4", 123, 8.0f, "test5", 456, "9.0f")""")
      }

      it("should work if the private method takes 19 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String, 
                            a11: Int, a12: Float, a13: String, a14: Int, a15: Float, a16: String, a17: Int, a18: Float, a19: String): String = a1 + " sesame!"
        }
        val secret = PrivateMethod19[String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, 7.0f, "test4", 123, 8.0f, "test5", 456, 9.0f, "test6")) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, "7.0f", "test4", 123, 8.0f, "test5", 456, 9.0f, 6)""")
      }

      it("should work if the private method takes 20 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String, 
                            a11: Int, a12: Float, a13: String, a14: Int, a15: Float, a16: String, a17: Int, a18: Float, a19: String, 
                            a20: Int): String = a1 + " sesame!"
        }
        val secret = PrivateMethod20[String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, 7.0f, "test4", 123, 8.0f, "test5", 456, 9.0f, "test6", 789)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, "7.0f", "test4", 123, 8.0f, "test5", 456, 9.0f, "test6", "789")""")
      }

      it("should work if the private method takes 21 arguments and returns a String") {

        class Modest {
          private def secret(a1: String, a2: Int, a3: Float, a4: String, a5: Int, a6: Float, a7: String, a8: Int, a9: Float, a10: String, 
                            a11: Int, a12: Float, a13: String, a14: Int, a15: Float, a16: String, a17: Int, a18: Float, a19: String, 
                            a20: Int, a21: Float): String = a1 + " sesame!"
        }
        val secret = PrivateMethod21[String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String, Int, Float, String](Symbol("secret"))
        assert(((new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, 7.0f, "test4", 123, 8.0f, "test5", 456, 9.0f, "test6", 789, 10.0f)) === "open sesame!")
        assertDoesNotCompile("""(new Modest) invokePrivate secret("open", 2, 3.0f, "test", 123, 5.0f, "test2", 456, 6.0f, "test3", 789, "7.0f", "test4", 123, 8.0f, "test5", 456, 9.0f, "test6", 789, "10.0f")""")
      }

    }

    
  }
}

