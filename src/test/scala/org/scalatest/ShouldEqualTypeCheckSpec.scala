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
import org.scalactic.CheckedEquality
import Matchers._

class ShouldEqualTypeCheckSpec extends Spec with CheckedEquality {

  object `the should equal syntax should give a type error for suspicious types` {
    object `for values` {
      def `at the basic level` {
        """"hi" should equal ("hi")""" should compile
        """"hi" shouldEqual "hi"""" should compile
        """"hi" should not equal ("ho")""" should compile
        """"hi" shouldNot equal ("ho")""" should compile
  
        """1 should equal ("hi")""" shouldNot typeCheck
        """1 shouldEqual "hi"""" shouldNot typeCheck
        """1 should not equal ("ho")""" shouldNot typeCheck
        """1 shouldNot equal ("ho")""" shouldNot typeCheck
  
        """"hi" should equal (1)""" shouldNot typeCheck
        """"hi" shouldEqual 1""" shouldNot typeCheck
        """"hi" should not equal (1)""" shouldNot typeCheck
        """"hi" shouldNot equal (1)""" shouldNot typeCheck
      }
      def `when used solo in logical expressions` {
  
        "1 should (equal (1) and equal (2 - 1))" should compile
        "1 should (equal (1) or equal (2 - 1))" should compile
  
        "1 should (not equal (1) and equal (2 - 1))" should compile
        "1 should (not equal (1) or equal (2 - 1))" should compile
  
        "1 should (equal (1) and not equal (2 - 1))" should compile
        "1 should (equal (1) or not equal (2 - 1))" should compile
  
        "1 should (not equal (1) and not equal (2 - 1))" should compile
        "1 should (not equal (1) or not equal (2 - 1))" should compile
  
        """1 should (equal (1) and equal ("X"))""" shouldNot typeCheck
        """1 should (equal (1) or equal ("X"))""" shouldNot typeCheck
  
        """1 should (not equal (1) and equal ("X"))""" shouldNot typeCheck
        """1 should (not equal (1) or equal ("X"))""" shouldNot typeCheck
  
        """1 should (equal (1) and not equal ("X"))""" shouldNot typeCheck
        """1 should (equal (1) or not equal ("X"))""" shouldNot typeCheck
  
        """1 should (not equal (1) and not equal ("X"))""" shouldNot typeCheck
        """1 should (not equal (1) or not equal ("X"))""" shouldNot typeCheck
  
        """1 should (equal ("X") and equal (2 - 1))""" shouldNot typeCheck
        """1 should (equal ("X") or equal (2 - 1))""" shouldNot typeCheck
  
        """1 should (not equal ("X") and equal (2 - 1))""" shouldNot typeCheck
        """1 should (not equal ("X") or equal (2 - 1))""" shouldNot typeCheck
  
        """1 should (equal ("X") and not equal (2 - 1))""" shouldNot typeCheck
        """1 should (equal ("X") or not equal (2 - 1))""" shouldNot typeCheck
  
        """1 should (not equal ("X") and not equal (2 - 1))""" shouldNot typeCheck
        """1 should (not equal ("X") or not equal (2 - 1))""" shouldNot typeCheck
  
        """1 should (equal ("X") and equal ("Y"))""" shouldNot typeCheck
        """1 should (equal ("X") or equal ("Y"))""" shouldNot typeCheck
  
        """1 should (not equal ("X") and equal ("Y"))""" shouldNot typeCheck
        """1 should (not equal ("X") or equal ("Y"))""" shouldNot typeCheck
  
        """1 should (equal ("X") and not equal ("Y"))""" shouldNot typeCheck
        """1 should (equal ("X") or not equal ("Y"))""" shouldNot typeCheck
  
        """1 should (not equal ("X") and not equal ("Y"))""" shouldNot typeCheck
        """1 should (not equal ("X") or not equal ("Y"))""" shouldNot typeCheck
      }
      def `when used in larger logical expressions` {
  
        "List(1) should (have length (1) and equal (List(1, 2)))" should compile
        "List(1) should (have length (1) or equal (List(1, 2)))" should compile
  
        "List(1) should (not have length (1) and equal (List(1, 2)))" should compile
        "List(1) should (not have length (1) or equal (List(1, 2)))" should compile
  
        "List(1) should (have length (1) and not equal (List(1, 2)))" should compile
        "List(1) should (have length (1) or not equal (List(1, 2)))" should compile
  
        "List(1) should (not have length (1) and not equal (List(1, 2)))" should compile
        "List(1) should (not have length (1) or not equal (List(1, 2)))" should compile
  
        "List(1) should (equal (Vector(1)) and have length (1))" should compile
        "List(1) should (equal (Vector(1)) or have length (1))" should compile
  
        "List(1) should (not equal (Vector(1)) and have length (1))" should compile
        "List(1) should (not equal (Vector(1)) or have length (1))" should compile
  
        "List(1) should (equal (Vector(1)) and not have length (1))" should compile
        "List(1) should (equal (Vector(1)) or not have length (1))" should compile
  
        "List(1) should (not equal (Vector(1)) and not have length (1))" should compile
        "List(1) should (not equal (Vector(1)) or not have length (1))" should compile
  
        """List(1) should (have length (1) and equal ("X"))""" shouldNot typeCheck
        """List(1) should (have length (1) or equal ("X"))""" shouldNot typeCheck
  
        """List(1) should (not have length (1) and equal ("X"))""" shouldNot typeCheck
        """List(1) should (not have length (1) or equal ("X"))""" shouldNot typeCheck
  
        """List(1) should (have length (1) and not equal ("X"))""" shouldNot typeCheck
        """List(1) should (have length (1) or not equal ("X"))""" shouldNot typeCheck
  
        """List(1) should (not have length (1) and not equal ("X"))""" shouldNot typeCheck
        """List(1) should (not have length (1) or not equal ("X"))""" shouldNot typeCheck
  
        """List(1) should (equal ("X") and have length (1))""" shouldNot typeCheck
        """List(1) should (equal ("X") or have length (1))""" shouldNot typeCheck
  
        """List(1) should (not equal ("X") and have length (1))""" shouldNot typeCheck
        """List(1) should (not equal ("X") or have length (1))""" shouldNot typeCheck
  
        """List(1) should (equal ("X") and not have length (1))""" shouldNot typeCheck
        """List(1) should (equal ("X") or not have length (1))""" shouldNot typeCheck
  
        """List(1) should (not equal ("X") and not have length (1))""" shouldNot typeCheck
        """List(1) should (not equal ("X") or not have length (1))""" shouldNot typeCheck
      }
      def `when used in even larger multi-part logical expressions` {
  
          "1 should (equal (1) and equal (1) and equal (1) and equal (1))" should compile
          "1 should (equal (1) and equal (1) or equal (1) and equal (1) or equal (1))" should compile
          """1 should (
              equal (1) and
              equal (1) or
              equal (1) and
              equal (1) or
              equal (1)
          )""" should compile
  
          "1 should (equal (1) and equal (1) and be >= (1) and equal (1))" should compile
          "1 should (equal (1) and equal (1) or be >= (1) and equal (1) or equal (1))" should compile
          """1 should (
              equal (1) and
              equal (1) or
              be >= (1) and
              equal (1) or
              equal (1)
          )""" should compile
  
          "1 should (equal (1) and be >= (1) and equal (1) and be >= (1))" should compile
          "1 should (equal (1) and be >= (1) or equal (1) and be >= (1) or equal (1))" should compile
          """1 should (
              equal (1) and
              be >= (1) or
              equal (1) and
              be >= (1) or
              equal (1)
          )""" should compile
  
          """1 should (equal (1) and equal ("hi") and equal (1) and equal (1))""" shouldNot typeCheck
          """1 should (equal (1) and equal (1) or equal (1) and equal ("hi") or equal (1))""" shouldNot typeCheck
          """1 should (
              equal (1) and
              equal (1) or
              equal (1) and
              equal (1) or
              equal ("hi")
          )""" shouldNot typeCheck
  
          """1 should (equal ("hi") and equal (1) and be >= (1) and equal (1))""" shouldNot typeCheck
          """1 should (equal (1) and equal (1) or be >= (1) and equal ("hi") or equal (1))""" shouldNot typeCheck
          """1 should (
              equal (1) and
              equal ("hi") or
              be >= (1) and
              equal (1) or
              equal (1)
          )""" shouldNot typeCheck
  
          """1 should (equal ("hi") and be >= (1) and equal (new java.util.Date) and be >= (1))""" shouldNot typeCheck
          """1 should (equal (1) and be >= (1) or equal (1) and be >= (1) or equal ("hi"))""" shouldNot typeCheck
          """1 should (
              equal (1) and
              be >= (1) or
              equal (new java.util.Date) and
              be >= (1) or
              equal (1)
          )""" shouldNot typeCheck
      }
      def `when a wrongly typed explcit equality is provided` {
        import org.scalactic.Explicitly._
        import org.scalactic.StringNormalizations._
        implicit val strEq = after being lowerCased
        """"hi" should equal ("Hi")""" should compile
        """"hi" shouldNot equal ("Hi") (decided by defaultEquality[String])""" should compile
        """"hi" shouldNot equal ("Hi") (defaultEquality[String])""" should compile
        """1 shouldNot equal ("Hi") (defaultEquality[Int])""" shouldNot typeCheck
        """1 should equal ("Hi") (defaultEquality[Int])""" shouldNot typeCheck
      }
    }
    object `for collections` {
      def `at the basic level` {
        """all (List("hi")) should equal ("hi")""" should compile
        """all (List("hi")) shouldEqual "hi"""" should compile
        """all (List("hi")) should not equal ("ho")""" should compile
        """all (List("hi")) shouldNot equal ("ho")""" should compile
  
        """all (List(1)) should equal ("hi")""" shouldNot typeCheck
        """all (List(1)) shouldEqual "hi"""" shouldNot typeCheck
        """all (List(1)) should not equal ("ho")""" shouldNot typeCheck
        """all (List(1)) shouldNot equal ("ho")""" shouldNot typeCheck
  
        """all (List("hi")) should equal (1)""" shouldNot typeCheck
        """all (List("hi")) shouldEqual 1""" shouldNot typeCheck
        """all (List("hi")) should not equal (1)""" shouldNot typeCheck
        """all (List("hi")) shouldNot equal (1)""" shouldNot typeCheck
      }
      def `when used solo in logical expressions` {
  
        "all (List(1)) should (equal (1) and equal (2 - 1))" should compile
        "all (List(1)) should (equal (1) or equal (2 - 1))" should compile
  
        "all (List(1)) should (not equal (1) and equal (2 - 1))" should compile
        "all (List(1)) should (not equal (1) or equal (2 - 1))" should compile
  
        "all (List(1)) should (equal (1) and not equal (2 - 1))" should compile
        "all (List(1)) should (equal (1) or not equal (2 - 1))" should compile
  
        "all (List(1)) should (not equal (1) and not equal (2 - 1))" should compile
        "all (List(1)) should (not equal (1) or not equal (2 - 1))" should compile
  
        """all (List(1)) should (equal (1) and equal ("X"))""" shouldNot typeCheck
        """all (List(1)) should (equal (1) or equal ("X"))""" shouldNot typeCheck
  
        """all (List(1)) should (not equal (1) and equal ("X"))""" shouldNot typeCheck
        """all (List(1)) should (not equal (1) or equal ("X"))""" shouldNot typeCheck
  
        """all (List(1)) should (equal (1) and not equal ("X"))""" shouldNot typeCheck
        """all (List(1)) should (equal (1) or not equal ("X"))""" shouldNot typeCheck
  
        """all (List(1)) should (not equal (1) and not equal ("X"))""" shouldNot typeCheck
        """all (List(1)) should (not equal (1) or not equal ("X"))""" shouldNot typeCheck
  
        """all (List(1)) should (equal ("X") and equal (2 - 1))""" shouldNot typeCheck
        """all (List(1)) should (equal ("X") or equal (2 - 1))""" shouldNot typeCheck
  
        """all (List(1)) should (not equal ("X") and equal (2 - 1))""" shouldNot typeCheck
        """all (List(1)) should (not equal ("X") or equal (2 - 1))""" shouldNot typeCheck
  
        """all (List(1)) should (equal ("X") and not equal (2 - 1))""" shouldNot typeCheck
        """all (List(1)) should (equal ("X") or not equal (2 - 1))""" shouldNot typeCheck
  
        """all (List(1)) should (not equal ("X") and not equal (2 - 1))""" shouldNot typeCheck
        """all (List(1)) should (not equal ("X") or not equal (2 - 1))""" shouldNot typeCheck
  
        """all (List(1)) should (equal ("X") and equal ("Y"))""" shouldNot typeCheck
        """all (List(1)) should (equal ("X") or equal ("Y"))""" shouldNot typeCheck
  
        """all (List(1)) should (not equal ("X") and equal ("Y"))""" shouldNot typeCheck
        """all (List(1)) should (not equal ("X") or equal ("Y"))""" shouldNot typeCheck
  
        """all (List(1)) should (equal ("X") and not equal ("Y"))""" shouldNot typeCheck
        """all (List(1)) should (equal ("X") or not equal ("Y"))""" shouldNot typeCheck
  
        """all (List(1)) should (not equal ("X") and not equal ("Y"))""" shouldNot typeCheck
        """all (List(1)) should (not equal ("X") or not equal ("Y"))""" shouldNot typeCheck
      }
      def `when used in larger logical expressions` {
  
        "all (List(List(1))) should (have length (1) and equal (List(1, 2)))" should compile
        "all (List(List(1)) )should (have length (1) or equal (List(1, 2)))" should compile
  
        "all (List(List(1))) should (not have length (1) and equal (List(1, 2)))" should compile
        "all (List(List(1))) should (not have length (1) or equal (List(1, 2)))" should compile
  
        "all (List(List(1))) should (have length (1) and not equal (List(1, 2)))" should compile
        "all (List(List(1))) should (have length (1) or not equal (List(1, 2)))" should compile
  
        "all (List(List(1))) should (not have length (1) and not equal (List(1, 2)))" should compile
        "all (List(List(1))) should (not have length (1) or not equal (List(1, 2)))" should compile
  
        "all (List(List(1))) should (equal (Vector(1)) and have length (1))" should compile
        "all (List(List(1))) should (equal (Vector(1)) or have length (1))" should compile
  
        "all (List(List(1))) should (not equal (Vector(1)) and have length (1))" should compile
        "all (List(List(1))) should (not equal (Vector(1)) or have length (1))" should compile
  
        "all (List(List(1))) should (equal (Vector(1)) and not have length (1))" should compile
        "all (List(List(1))) should (equal (Vector(1)) or not have length (1))" should compile
  
        "all (List(List(1))) should (not equal (Vector(1)) and not have length (1))" should compile
        "all (List(List(1))) should (not equal (Vector(1)) or not have length (1))" should compile
  
        """all (List(List(1))) should (have length (1) and equal ("X"))""" shouldNot typeCheck
        """all (List(List(1))) should (have length (1) or equal ("X"))""" shouldNot typeCheck
  
        """all (List(List(1))) should (not have length (1) and equal ("X"))""" shouldNot typeCheck
        """all (List(List(1))) should (not have length (1) or equal ("X"))""" shouldNot typeCheck
  
        """all (List(List(1))) should (have length (1) and not equal ("X"))""" shouldNot typeCheck
        """all (List(List(1))) should (have length (1) or not equal ("X"))""" shouldNot typeCheck
  
        """all (List(List(1))) should (not have length (1) and not equal ("X"))""" shouldNot typeCheck
        """all (List(List(1))) should (not have length (1) or not equal ("X"))""" shouldNot typeCheck
  
        """all (List(List(1))) should (equal ("X") and have length (1))""" shouldNot typeCheck
        """all (List(List(1))) should (equal ("X") or have length (1))""" shouldNot typeCheck
  
        """all (List(List(1))) should (not equal ("X") and have length (1))""" shouldNot typeCheck
        """all (List(List(1))) should (not equal ("X") or have length (1))""" shouldNot typeCheck
  
        """all (List(List(1))) should (equal ("X") and not have length (1))""" shouldNot typeCheck
        """all (List(List(1))) should (equal ("X") or not have length (1))""" shouldNot typeCheck
  
        """all (List(List(1))) should (not equal ("X") and not have length (1))""" shouldNot typeCheck
        """all (List(List(1))) should (not equal ("X") or not have length (1))""" shouldNot typeCheck
      }
      def `when used in even larger multi-part logical expressions` {
  
          "1 should (equal (1) and equal (1) and equal (1) and equal (1))" should compile
          "1 should (equal (1) and equal (1) or equal (1) and equal (1) or equal (1))" should compile
          """1 should (
              equal (1) and
              equal (1) or
              equal (1) and
              equal (1) or
              equal (1)
          )""" should compile
  
          "all (List(1)) should (equal (1) and equal (1) and be >= (1) and equal (1))" should compile
          "all (List(1)) should (equal (1) and equal (1) or be >= (1) and equal (1) or equal (1))" should compile
          """all (List(1)) should (
              equal (1) and
              equal (1) or
              be >= (1) and
              equal (1) or
              equal (1)
          )""" should compile
  
          "all (List(1)) should (equal (1) and be >= (1) and equal (1) and be >= (1))" should compile
          "all (List(1)) should (equal (1) and be >= (1) or equal (1) and be >= (1) or equal (1))" should compile
          """all (List(1)) should (
              equal (1) and
              be >= (1) or
              equal (1) and
              be >= (1) or
              equal (1)
          )""" should compile
  
          """all (List(1)) should (equal (1) and equal ("hi") and equal (1) and equal (1))""" shouldNot typeCheck
          """all (List(1)) should (equal (1) and equal (1) or equal (1) and equal ("hi") or equal (1))""" shouldNot typeCheck
          """all (List(1)) should (
              equal (1) and
              equal (1) or
              equal (1) and
              equal (1) or
              equal ("hi")
          )""" shouldNot typeCheck
  
          """all (List(1)) should (equal ("hi") and equal (1) and be >= (1) and equal (1))""" shouldNot typeCheck
          """all (List(1)) should (equal (1) and equal (1) or be >= (1) and equal ("hi") or equal (1))""" shouldNot typeCheck
          """all (List(1)) should (
              equal (1) and
              equal ("hi") or
              be >= (1) and
              equal (1) or
              equal (1)
          )""" shouldNot typeCheck
  
          """all (List(1)) should (equal ("hi") and be >= (1) and equal (new java.util.Date) and be >= (1))""" shouldNot typeCheck
          """all (List(1)) should (equal (1) and be >= (1) or equal (1) and be >= (1) or equal ("hi"))""" shouldNot typeCheck
          """all (List(1)) should (
              equal (1) and
              be >= (1) or
              equal (new java.util.Date) and
              be >= (1) or
              equal (1)
          )""" shouldNot typeCheck
      }
    }
  }
}

