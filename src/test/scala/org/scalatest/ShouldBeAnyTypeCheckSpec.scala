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

import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import org.scalactic.Equality
import org.scalactic.TypeCheckedTripleEquals
import Matchers._

class ShouldBeAnyTypeCheckSpec extends Spec with TypeCheckedTripleEquals {

  object `the should be syntax should give a type error for suspicious types` {
    object `for values` {
      def `at the basic level` {
        """"hi" should be ("hi")""" should compile
        """"hi" shouldBe "hi"""" should compile
        """"hi" should not be ("ho")""" should compile
        """"hi" shouldNot be ("ho")""" should compile
  
        """1 should be ("hi")""" shouldNot typeCheck
        """1 shouldBe "hi"""" shouldNot typeCheck
        """1 should not be ("ho")""" shouldNot typeCheck
        """1 shouldNot be ("ho")""" shouldNot typeCheck
  
        """"hi" should be (1)""" shouldNot typeCheck
        """"hi" shouldBe 1""" shouldNot typeCheck
        """"hi" should not be (1)""" shouldNot typeCheck
        """"hi" shouldNot be (1)""" shouldNot typeCheck
      }
      def `when used solo in logical expressions` {
  
        "1 should (be (1) and be (2 - 1))" should compile
        "1 should (be (1) or be (2 - 1))" should compile
  
        "1 should (not be (1) and be (2 - 1))" should compile
        "1 should (not be (1) or be (2 - 1))" should compile
  
        "1 should (be (1) and not be (2 - 1))" should compile
        "1 should (be (1) or not be (2 - 1))" should compile
  
        "1 should (not be (1) and not be (2 - 1))" should compile
        "1 should (not be (1) or not be (2 - 1))" should compile
  
        """1 should (be (1) and be ("X"))""" shouldNot typeCheck
        """1 should (be (1) or be ("X"))""" shouldNot typeCheck
  
        """1 should (not be (1) and be ("X"))""" shouldNot typeCheck
        """1 should (not be (1) or be ("X"))""" shouldNot typeCheck
  
        """1 should (be (1) and not be ("X"))""" shouldNot typeCheck
        """1 should (be (1) or not be ("X"))""" shouldNot typeCheck
  
        """1 should (not be (1) and not be ("X"))""" shouldNot typeCheck
        """1 should (not be (1) or not be ("X"))""" shouldNot typeCheck
  
        """1 should (be ("X") and be (2 - 1))""" shouldNot typeCheck
        """1 should (be ("X") or be (2 - 1))""" shouldNot typeCheck
  
        """1 should (not be ("X") and be (2 - 1))""" shouldNot typeCheck
        """1 should (not be ("X") or be (2 - 1))""" shouldNot typeCheck
  
        """1 should (be ("X") and not be (2 - 1))""" shouldNot typeCheck
        """1 should (be ("X") or not be (2 - 1))""" shouldNot typeCheck
  
        """1 should (not be ("X") and not be (2 - 1))""" shouldNot typeCheck
        """1 should (not be ("X") or not be (2 - 1))""" shouldNot typeCheck
  
        """1 should (be ("X") and be ("Y"))""" shouldNot typeCheck
        """1 should (be ("X") or be ("Y"))""" shouldNot typeCheck
  
        """1 should (not be ("X") and be ("Y"))""" shouldNot typeCheck
        """1 should (not be ("X") or be ("Y"))""" shouldNot typeCheck
  
        """1 should (be ("X") and not be ("Y"))""" shouldNot typeCheck
        """1 should (be ("X") or not be ("Y"))""" shouldNot typeCheck
  
        """1 should (not be ("X") and not be ("Y"))""" shouldNot typeCheck
        """1 should (not be ("X") or not be ("Y"))""" shouldNot typeCheck
      }
      def `when used in larger logical expressions` {
  
        "List(1) should (have length (1) and be (List(1, 2)))" should compile
        "List(1) should (have length (1) or be (List(1, 2)))" should compile
  
        "List(1) should (not have length (1) and be (List(1, 2)))" should compile
        "List(1) should (not have length (1) or be (List(1, 2)))" should compile
  
        "List(1) should (have length (1) and not be (List(1, 2)))" should compile
        "List(1) should (have length (1) or not be (List(1, 2)))" should compile
  
        "List(1) should (not have length (1) and not be (List(1, 2)))" should compile
        "List(1) should (not have length (1) or not be (List(1, 2)))" should compile
  
        "List(1) should (be (Vector(1)) and have length (1))" should compile
        "List(1) should (be (Vector(1)) or have length (1))" should compile
  
        "List(1) should (not be (Vector(1)) and have length (1))" should compile
        "List(1) should (not be (Vector(1)) or have length (1))" should compile
  
        "List(1) should (be (Vector(1)) and not have length (1))" should compile
        "List(1) should (be (Vector(1)) or not have length (1))" should compile
  
        "List(1) should (not be (Vector(1)) and not have length (1))" should compile
        "List(1) should (not be (Vector(1)) or not have length (1))" should compile
  
        """List(1) should (have length (1) and be ("X"))""" shouldNot typeCheck
        """List(1) should (have length (1) or be ("X"))""" shouldNot typeCheck
  
        """List(1) should (not have length (1) and be ("X"))""" shouldNot typeCheck
        """List(1) should (not have length (1) or be ("X"))""" shouldNot typeCheck
  
        """List(1) should (have length (1) and not be ("X"))""" shouldNot typeCheck
        """List(1) should (have length (1) or not be ("X"))""" shouldNot typeCheck
  
        """List(1) should (not have length (1) and not be ("X"))""" shouldNot typeCheck
        """List(1) should (not have length (1) or not be ("X"))""" shouldNot typeCheck
  
        """List(1) should (be ("X") and have length (1))""" shouldNot typeCheck
        """List(1) should (be ("X") or have length (1))""" shouldNot typeCheck
  
        """List(1) should (not be ("X") and have length (1))""" shouldNot typeCheck
        """List(1) should (not be ("X") or have length (1))""" shouldNot typeCheck
  
        """List(1) should (be ("X") and not have length (1))""" shouldNot typeCheck
        """List(1) should (be ("X") or not have length (1))""" shouldNot typeCheck
  
        """List(1) should (not be ("X") and not have length (1))""" shouldNot typeCheck
        """List(1) should (not be ("X") or not have length (1))""" shouldNot typeCheck
      }
      def `when used in even larger multi-part logical expressions` {
  
          "1 should (be (1) and be (1) and be (1) and be (1))" should compile
          "1 should (be (1) and be (1) or be (1) and be (1) or be (1))" should compile
          """1 should (
              be (1) and
              be (1) or
              be (1) and
              be (1) or
              be (1)
          )""" should compile
  
          "1 should (be (1) and be (1) and be >= (1) and be (1))" should compile
          "1 should (be (1) and be (1) or be >= (1) and be (1) or be (1))" should compile
          """1 should (
              be (1) and
              be (1) or
              be >= (1) and
              be (1) or
              be (1)
          )""" should compile
  
          "1 should (be (1) and be >= (1) and be (1) and be >= (1))" should compile
          "1 should (be (1) and be >= (1) or be (1) and be >= (1) or be (1))" should compile
          """1 should (
              be (1) and
              be >= (1) or
              be (1) and
              be >= (1) or
              be (1)
          )""" should compile
  
          """1 should (be (1) and be ("hi") and be (1) and be (1))""" shouldNot typeCheck
          """1 should (be (1) and be (1) or be (1) and be ("hi") or be (1))""" shouldNot typeCheck
          """1 should (
              be (1) and
              be (1) or
              be (1) and
              be (1) or
              be ("hi")
          )""" shouldNot typeCheck
  
          """1 should (be ("hi") and be (1) and be >= (1) and be (1))""" shouldNot typeCheck
          """1 should (be (1) and be (1) or be >= (1) and be ("hi") or be (1))""" shouldNot typeCheck
          """1 should (
              be (1) and
              be ("hi") or
              be >= (1) and
              be (1) or
              be (1)
          )""" shouldNot typeCheck
  
          """1 should (be ("hi") and be >= (1) and be (new java.util.Date) and be >= (1))""" shouldNot typeCheck
          """1 should (be (1) and be >= (1) or be (1) and be >= (1) or be ("hi"))""" shouldNot typeCheck
          """1 should (
              be (1) and
              be >= (1) or
              be (new java.util.Date) and
              be >= (1) or
              be (1)
          )""" shouldNot typeCheck
      }
      def `when a wrongly typed explcit beity is provided` {
        import org.scalactic.Explicitly._
        import org.scalactic.StringNormalizations._
        implicit val strEq = after being lowerCased
        """"hi" should be ("Hi")""" should compile
        """"hi" shouldNot be ("Hi") (decided by defaultEquality[String])""" should compile
        """"hi" shouldNot be ("Hi") (defaultEquality[String])""" should compile
        """1 shouldNot be ("Hi") (defaultEquality[Int])""" shouldNot typeCheck
        """1 should be ("Hi") (defaultEquality[Int])""" shouldNot typeCheck
      }
    }
    object `for collections` {
      def `at the basic level` {
        """all (List("hi")) should be ("hi")""" should compile
        """all (List("hi")) shouldBe "hi"""" should compile
        """all (List("hi")) should not be ("ho")""" should compile
        """all (List("hi")) shouldNot be ("ho")""" should compile
  
        """all (List(1)) should be ("hi")""" shouldNot typeCheck
        """all (List(1)) shouldBe "hi"""" shouldNot typeCheck
        """all (List(1)) should not be ("ho")""" shouldNot typeCheck
        """all (List(1)) shouldNot be ("ho")""" shouldNot typeCheck
  
        """all (List("hi")) should be (1)""" shouldNot typeCheck
        """all (List("hi")) shouldBe 1""" shouldNot typeCheck
        """all (List("hi")) should not be (1)""" shouldNot typeCheck
        """all (List("hi")) shouldNot be (1)""" shouldNot typeCheck
      }
      def `when used solo in logical expressions` {
  
        "all (List(1)) should (be (1) and be (2 - 1))" should compile
        "all (List(1)) should (be (1) or be (2 - 1))" should compile
  
        "all (List(1)) should (not be (1) and be (2 - 1))" should compile
        "all (List(1)) should (not be (1) or be (2 - 1))" should compile
  
        "all (List(1)) should (be (1) and not be (2 - 1))" should compile
        "all (List(1)) should (be (1) or not be (2 - 1))" should compile
  
        "all (List(1)) should (not be (1) and not be (2 - 1))" should compile
        "all (List(1)) should (not be (1) or not be (2 - 1))" should compile
  
        """all (List(1)) should (be (1) and be ("X"))""" shouldNot typeCheck
        """all (List(1)) should (be (1) or be ("X"))""" shouldNot typeCheck
  
        """all (List(1)) should (not be (1) and be ("X"))""" shouldNot typeCheck
        """all (List(1)) should (not be (1) or be ("X"))""" shouldNot typeCheck
  
        """all (List(1)) should (be (1) and not be ("X"))""" shouldNot typeCheck
        """all (List(1)) should (be (1) or not be ("X"))""" shouldNot typeCheck
  
        """all (List(1)) should (not be (1) and not be ("X"))""" shouldNot typeCheck
        """all (List(1)) should (not be (1) or not be ("X"))""" shouldNot typeCheck
  
        """all (List(1)) should (be ("X") and be (2 - 1))""" shouldNot typeCheck
        """all (List(1)) should (be ("X") or be (2 - 1))""" shouldNot typeCheck
  
        """all (List(1)) should (not be ("X") and be (2 - 1))""" shouldNot typeCheck
        """all (List(1)) should (not be ("X") or be (2 - 1))""" shouldNot typeCheck
  
        """all (List(1)) should (be ("X") and not be (2 - 1))""" shouldNot typeCheck
        """all (List(1)) should (be ("X") or not be (2 - 1))""" shouldNot typeCheck
  
        """all (List(1)) should (not be ("X") and not be (2 - 1))""" shouldNot typeCheck
        """all (List(1)) should (not be ("X") or not be (2 - 1))""" shouldNot typeCheck
  
        """all (List(1)) should (be ("X") and be ("Y"))""" shouldNot typeCheck
        """all (List(1)) should (be ("X") or be ("Y"))""" shouldNot typeCheck
  
        """all (List(1)) should (not be ("X") and be ("Y"))""" shouldNot typeCheck
        """all (List(1)) should (not be ("X") or be ("Y"))""" shouldNot typeCheck
  
        """all (List(1)) should (be ("X") and not be ("Y"))""" shouldNot typeCheck
        """all (List(1)) should (be ("X") or not be ("Y"))""" shouldNot typeCheck
  
        """all (List(1)) should (not be ("X") and not be ("Y"))""" shouldNot typeCheck
        """all (List(1)) should (not be ("X") or not be ("Y"))""" shouldNot typeCheck
      }
      def `when used in larger logical expressions` {
  
        "all (List(List(1))) should (have length (1) and be (List(1, 2)))" should compile
        "all (List(List(1)) )should (have length (1) or be (List(1, 2)))" should compile
  
        "all (List(List(1))) should (not have length (1) and be (List(1, 2)))" should compile
        "all (List(List(1))) should (not have length (1) or be (List(1, 2)))" should compile
  
        "all (List(List(1))) should (have length (1) and not be (List(1, 2)))" should compile
        "all (List(List(1))) should (have length (1) or not be (List(1, 2)))" should compile
  
        "all (List(List(1))) should (not have length (1) and not be (List(1, 2)))" should compile
        "all (List(List(1))) should (not have length (1) or not be (List(1, 2)))" should compile
  
        "all (List(List(1))) should (be (Vector(1)) and have length (1))" should compile
        "all (List(List(1))) should (be (Vector(1)) or have length (1))" should compile
  
        "all (List(List(1))) should (not be (Vector(1)) and have length (1))" should compile
        "all (List(List(1))) should (not be (Vector(1)) or have length (1))" should compile
  
        "all (List(List(1))) should (be (Vector(1)) and not have length (1))" should compile
        "all (List(List(1))) should (be (Vector(1)) or not have length (1))" should compile
  
        "all (List(List(1))) should (not be (Vector(1)) and not have length (1))" should compile
        "all (List(List(1))) should (not be (Vector(1)) or not have length (1))" should compile
  
        """all (List(List(1))) should (have length (1) and be ("X"))""" shouldNot typeCheck
        """all (List(List(1))) should (have length (1) or be ("X"))""" shouldNot typeCheck
  
        """all (List(List(1))) should (not have length (1) and be ("X"))""" shouldNot typeCheck
        """all (List(List(1))) should (not have length (1) or be ("X"))""" shouldNot typeCheck
  
        """all (List(List(1))) should (have length (1) and not be ("X"))""" shouldNot typeCheck
        """all (List(List(1))) should (have length (1) or not be ("X"))""" shouldNot typeCheck
  
        """all (List(List(1))) should (not have length (1) and not be ("X"))""" shouldNot typeCheck
        """all (List(List(1))) should (not have length (1) or not be ("X"))""" shouldNot typeCheck
  
        """all (List(List(1))) should (be ("X") and have length (1))""" shouldNot typeCheck
        """all (List(List(1))) should (be ("X") or have length (1))""" shouldNot typeCheck
  
        """all (List(List(1))) should (not be ("X") and have length (1))""" shouldNot typeCheck
        """all (List(List(1))) should (not be ("X") or have length (1))""" shouldNot typeCheck
  
        """all (List(List(1))) should (be ("X") and not have length (1))""" shouldNot typeCheck
        """all (List(List(1))) should (be ("X") or not have length (1))""" shouldNot typeCheck
  
        """all (List(List(1))) should (not be ("X") and not have length (1))""" shouldNot typeCheck
        """all (List(List(1))) should (not be ("X") or not have length (1))""" shouldNot typeCheck
      }
      def `when used in even larger multi-part logical expressions` {
  
          "1 should (be (1) and be (1) and be (1) and be (1))" should compile
          "1 should (be (1) and be (1) or be (1) and be (1) or be (1))" should compile
          """1 should (
              be (1) and
              be (1) or
              be (1) and
              be (1) or
              be (1)
          )""" should compile
  
          "all (List(1)) should (be (1) and be (1) and be >= (1) and be (1))" should compile
          "all (List(1)) should (be (1) and be (1) or be >= (1) and be (1) or be (1))" should compile
          """all (List(1)) should (
              be (1) and
              be (1) or
              be >= (1) and
              be (1) or
              be (1)
          )""" should compile
  
          "all (List(1)) should (be (1) and be >= (1) and be (1) and be >= (1))" should compile
          "all (List(1)) should (be (1) and be >= (1) or be (1) and be >= (1) or be (1))" should compile
          """all (List(1)) should (
              be (1) and
              be >= (1) or
              be (1) and
              be >= (1) or
              be (1)
          )""" should compile
  
          """all (List(1)) should (be (1) and be ("hi") and be (1) and be (1))""" shouldNot typeCheck
          """all (List(1)) should (be (1) and be (1) or be (1) and be ("hi") or be (1))""" shouldNot typeCheck
          """all (List(1)) should (
              be (1) and
              be (1) or
              be (1) and
              be (1) or
              be ("hi")
          )""" shouldNot typeCheck
  
          """all (List(1)) should (be ("hi") and be (1) and be >= (1) and be (1))""" shouldNot typeCheck
          """all (List(1)) should (be (1) and be (1) or be >= (1) and be ("hi") or be (1))""" shouldNot typeCheck
          """all (List(1)) should (
              be (1) and
              be ("hi") or
              be >= (1) and
              be (1) or
              be (1)
          )""" shouldNot typeCheck
  
          """all (List(1)) should (be ("hi") and be >= (1) and be (new java.util.Date) and be >= (1))""" shouldNot typeCheck
          """all (List(1)) should (be (1) and be >= (1) or be (1) and be >= (1) or be ("hi"))""" shouldNot typeCheck
          """all (List(1)) should (
              be (1) and
              be >= (1) or
              be (new java.util.Date) and
              be >= (1) or
              be (1)
          )""" shouldNot typeCheck
      }
    }
  }
}

