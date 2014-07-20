/*
 * Copyright 2001-2014 Artima, Inc.
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

import java.text._
import org.scalatest._
import java.util.Date
import scala.collection.mutable

class InnerConstraintsSpec extends Spec with Matchers with TypeCheckedTripleEquals {
  
  object `Inner constraints should enable equality comparisons` {
    def `on Seqs and Arrays` {
      List(1, 2, 3) shouldEqual Vector(1L, 2L, 3L)
      List(1, 2, 3) shouldEqual List(1L, 2L, 3L)
      Vector(1, 2, 3) shouldEqual List(1L, 2L, 3L)
      List(1, 2, 3) shouldEqual Array(1L, 2L, 3L)
      Array(1, 2, 3) shouldEqual Array(1L, 2L, 3L)
      Array(1, 2, 3) shouldEqual List(1L, 2L, 3L)
    }
    def `on nested Seqs` {
      Vector(List(1, 2, 3)) shouldEqual List(Vector(1L, 2L, 3L))
      List(List(1, 2, 3)) shouldEqual List(List(1L, 2L, 3L))
      List(Vector(1, 2, 3)) shouldEqual Vector(List(1L, 2L, 3L))
    }
    def `on Sets` {
      Set(1, 2, 3) shouldEqual mutable.HashSet(1L, 2L, 3L)
      Set(1, 2, 3) shouldEqual Set(1L, 2L, 3L)
      mutable.HashSet(1, 2, 3) shouldEqual Set(1L, 2L, 3L)
    }
    def `on nested Sets` {
      mutable.HashSet(Set(1, 2, 3)) shouldEqual Set(mutable.HashSet(1L, 2L, 3L))
      Set(Set(1, 2, 3)) shouldEqual Set(Set(1L, 2L, 3L))
      Set(mutable.HashSet(1, 2, 3)) shouldEqual mutable.HashSet(Set(1L, 2L, 3L))
    }
    def `on Maps` {
      Map("1" -> 1, "2" -> 2, "3" -> 3) shouldEqual mutable.HashMap("1" -> 1L, "2" -> 2L, "3" -> 3L)
      Map("1" -> 1, "2" -> 2, "3" -> 3) shouldEqual Map("1" -> 1L, "2" -> 2L, "3" -> 3L)
      mutable.HashMap("1" -> 1, "2" -> 2, "3" -> 3) shouldEqual Map("1" -> 1L, "2" -> 2L, "3" -> 3L)
    }
    def `on nested Maps` {
      mutable.HashMap(0 -> Map("1" -> 1, "2" -> 2, "3" -> 3)) shouldEqual Map(0 -> mutable.HashMap("1" -> 1L, "2" -> 2L, "3" -> 3L))
      Map(0 -> Map("1" -> 1, "2" -> 2, "3" -> 3)) shouldEqual Map(0 -> Map("1" -> 1L, "2" -> 2L, "3" -> 3L))
      Map(0 -> mutable.HashMap("1" -> 1, "2" -> 2, "3" -> 3)) shouldEqual mutable.HashMap(0 -> Map("1" -> 1L, "2" -> 2L, "3" -> 3L))
    }
    def `on Every` {
      One(1) shouldEqual One(1L)
      Many(1, 2) shouldEqual Many(1L, 2L)

      Every(1) shouldEqual One(1L)
      Every(1, 2) shouldEqual Many(1L, 2L)

      One(1) shouldEqual Every(1L)
      Many(1, 2) shouldEqual Every(1L, 2L)

      Every(1) shouldEqual Every(1L)
      Every(1, 2) shouldEqual Every(1L, 2L)

      // But if a One and Many are compared, that can never be equal, so it should not be allowed
      """One(1) === Many(1, 2)""" shouldNot typeCheck
      """Many(1) === One(1, 2)""" shouldNot typeCheck
    }
    def `on nested Every` {
      List(One(1)) shouldEqual Vector(One(1L))
      List(Many(1, 2)) shouldEqual Vector(Many(1L, 2L))

      List(Every(1)) shouldEqual Vector(One(1L))
      List(Every(1, 2)) shouldEqual Vector(Many(1L, 2L))

      List(One(1)) shouldEqual Vector(Every(1L))
      List(Many(1, 2)) shouldEqual Vector(Every(1L, 2L))

      List(Every(1)) shouldEqual Vector(Every(1L))
      List(Every(1, 2)) shouldEqual Vector(Every(1L, 2L))

      // But if a One and Many are compared, that can never be equal, so it should not be allowed
      """List(One(1)) === Vector(Many(1, 2))""" shouldNot typeCheck
      """List(Many(1)) === Vector(One(1, 2))""" shouldNot typeCheck
    }
    def `on Or` {

      // Both sides Good
      (Good(1): Good[Int, Int]) shouldEqual (Good(1L): Good[Long, Int])
      (Good(1): Good[Int, Int]) shouldEqual (Good(1): Good[Int, Long])
      (Good(1L): Good[Long, Int]) shouldEqual (Good(1): Good[Int, Int])
      (Good(1): Good[Int, Long]) shouldEqual (Good(1): Good[Int, Int])
      // Given both sides are Good, it shouldn't matter if the Bad type has no constraint
      (Good(1): Good[Int, Long]) shouldEqual (Good(1): Good[Int, String])
      (Good(1L): Good[Long, Int]) shouldEqual (Good(1): Good[Int, String])
      // But if both sides are Good but without a constraint, it should not compile, even
      // if the Bad type has a constraint.
      """(Good(1L): Good[Long, Int]) shouldEqual (Good("one"): Good[String, Int])""" shouldNot typeCheck

      // Left side Good, right side Or
      (Good(1): Good[Int, Int]) shouldEqual (Good(1L): Long Or Int)
      (Good(1): Good[Int, Int]) shouldEqual (Good(1): Int Or Long)
      (Good(1L): Good[Long, Int]) shouldEqual (Good(1): Int Or Int)
      (Good(1): Good[Int, Long]) shouldEqual (Good(1): Int Or Int)
      // Given left side is Good, it shouldn't matter if the Bad type has no constraint
      (Good(1): Good[Int, Long]) shouldEqual (Good(1): Or[Int, String])
      (Good(1L): Good[Long, Int]) shouldEqual (Good(1): Or[Int, String])
      // But if left side is Good but without a constraint between left and right Good types, it should not compile, even
      // if the Bad type has a constraint.
      """(Good(1L): Good[Long, Int]) shouldEqual (Good("one"): Good[String, Int])""" shouldNot typeCheck

      // Right side Good, left side Or
      (Good(1): Int Or Int) shouldEqual (Good(1L): Good[Long, Int])
      (Good(1): Int Or Int) shouldEqual (Good(1): Good[Int, Long])
      (Good(1L): Long Or Int) shouldEqual (Good(1): Good[Int, Int])
      (Good(1): Int Or Long) shouldEqual (Good(1): Good[Int, Int])
      // Given right side is Good, it shouldn't matter if the Bad type has no constraint
      (Good(1): Or[Int, Long]) shouldEqual (Good(1): Good[Int, String])
      (Good(1L): Or[Long, Int]) shouldEqual (Good(1): Good[Int, String])
      // But if right side is Good but without a constraint between left and right Good types, it should not compile, even
      // if the Bad type has a constraint.
      """(Good(1L): Or[Long, Int]) shouldEqual (Good("one"): Good[String, Int])""" shouldNot typeCheck

      // Both sides Bad
      (Bad(1): Bad[Int, Int]) shouldEqual (Bad(1): Bad[Long, Int])
      (Bad(1): Bad[Int, Int]) shouldEqual (Bad(1L): Bad[Int, Long])
      (Bad(1): Bad[Long, Int]) shouldEqual (Bad(1): Bad[Int, Int])
      (Bad(1L): Bad[Int, Long]) shouldEqual (Bad(1): Bad[Int, Int])
      // Given both sides are Bad, it shouldn't matter if the Good type has no constraint
      (Bad(1): Bad[Long, Int]) shouldEqual (Bad(1): Bad[String, Int])
      (Bad(1L): Bad[Int, Long]) shouldEqual (Bad(1): Bad[String, Int])
      // But if both sides are Bad but without a constraint, it should not compile, even
      // if the Good type has a constraint.
      """(Bad(1L): Bad[Int, Long]) shouldEqual (Bad("one"): Bad[Int, String])""" shouldNot typeCheck

      // Left side Bad, right side Or
      (Bad(1): Bad[Int, Int]) shouldEqual (Bad(1): Long Or Int)
      (Bad(1): Bad[Int, Int]) shouldEqual (Bad(1L): Int Or Long)
      (Bad(1): Bad[Long, Int]) shouldEqual (Bad(1): Int Or Int)
      (Bad(1L): Bad[Int, Long]) shouldEqual (Bad(1): Int Or Int)
      // Given left side is Bad, it shouldn't matter if the Good type has no constraint
      (Bad(1): Bad[Long, Int]) shouldEqual (Bad(1): Or[String, Int])
      (Bad(1L): Bad[Int, Long]) shouldEqual (Bad(1): Or[String, Int])
      // But if left side is Bad but without a constraint between left and right Bad types, it should not compile, even
      // if the Good type has a constraint.
      """(Bad(1L): Bad[Int, Long]) shouldEqual (Bad("one"): Or[Int, String])""" shouldNot typeCheck

      // Right side Bad, left side Or
      (Bad(1): Int Or Int) shouldEqual (Bad(1): Bad[Long, Int])
      (Bad(1): Int Or Int) shouldEqual (Bad(1L): Bad[Int, Long])
      (Bad(1): Long Or Int) shouldEqual (Bad(1): Bad[Int, Int])
      (Bad(1L): Int Or Long) shouldEqual (Bad(1): Bad[Int, Int])
      // Given right side is Bad, it shouldn't matter if the Good type has no constraint
      (Bad(1): Or[Long, Int]) shouldEqual (Bad(1): Bad[String, Int])
      (Bad(1L): Or[Int, Long]) shouldEqual (Bad(1): Bad[String, Int])
      // But if right side is Bad but without a constraint between left and right Bad types, it should not compile, even
      // if the Good type has a constraint.
      """(Bad(1L): Or[Int, Long]) shouldEqual (Bad("one"): Bad[Int, String])""" shouldNot typeCheck

      // Both sides Or
      (Good(1): Int Or Int) shouldEqual (Good(1L): Long Or Int)
      (Good(1): Int Or Int) shouldEqual (Good(1): Int Or Long)
      (Good(1L): Long Or Int) shouldEqual (Good(1): Int Or Int)
      (Good(1): Int Or Long) shouldEqual (Good(1): Int Or Int)
      (Bad(1): Int Or Int) shouldEqual (Bad(1): Long Or Int)
      (Bad(1): Int Or Int) shouldEqual (Bad(1L): Int Or Long)
      (Bad(1): Long Or Int) shouldEqual (Bad(1): Int Or Int)
      (Bad(1L): Int Or Long) shouldEqual (Bad(1): Int Or Int)
      // So long as an equality constraint exists for one the Good or Bad side of type Or,
      // the comparison will be allowed. This is because it may be true. At the
      // end of the day, a Good[Int].orBad[String] can equal a Good[Int].orBad[java.util.Date]
      // 
      // scala> Good(1).orBad[String] == Good(1L).orBad[java.util.Date]
      // res0: Boolean = true
      // 
      // Similarly, a Good[Int].orBad[String] can equal a Good[java.util.Date].orBad[String]
      // scala> Good[Int].orBad("hi") == Good[java.util.Date].orBad("hi")
      // res1: Boolean = true
      (Good(1).orBad[String]: Int Or String) shouldEqual (Good(1L).orBad[Date]: Long Or Date)
      (Good[Int].orBad("hi"): Int Or String) shouldEqual (Good[Date].orBad("hi"): Date Or String)
      // The only way an equality comparison of two Ors will not be allowed to compile, therefore, is if
      // no constraint exists between either the Good or Bad types:
      """(Good(1): Int Or String) shouldEqual (Good("one"): String Or Int)""" shouldNot typeCheck
    }
    def `on Nested Or` {

      // Both sides Good
      (List(Good(1)): List[Good[Int, Int]]) shouldEqual (List(Good(1L)): List[Good[Long, Int]])
      (List(Good(1)): List[Good[Int, Int]]) shouldEqual (List(Good(1)): List[Good[Int, Long]])
      (List(Good(1L)): List[Good[Long, Int]]) shouldEqual (List(Good(1)): List[Good[Int, Int]])
      (List(Good(1)): List[Good[Int, Long]]) shouldEqual (List(Good(1)): List[Good[Int, Int]])
      // Given both sides are Good, it shouldn't matter if the Bad type has no constraint
      (List(Good(1)): List[Good[Int, Long]]) shouldEqual (List(Good(1)): List[Good[Int, String]])
      (List(Good(1L)): List[Good[Long, Int]]) shouldEqual (List(Good(1)): List[Good[Int, String]])
      // But if both sides are Good but without a constraint, it should not compile, even
      // if the Bad type has a constraint.
      """(List(Good(1L)): List[Good[Long, Int]]) shouldEqual (List(Good("one")): List[Good[String, Int]])""" shouldNot typeCheck

      // Left side Good, right side Or
      (List(Good(1)): List[Good[Int, Int]]) shouldEqual (List(Good(1L)): List[Long Or Int])
      (List(Good(1)): List[Good[Int, Int]]) shouldEqual (List(Good(1)): List[Int Or Long])
      (List(Good(1L)): List[Good[Long, Int]]) shouldEqual (List(Good(1)): List[Int Or Int])
      (List(Good(1)): List[Good[Int, Long]]) shouldEqual (List(Good(1)): List[Int Or Int])
      // Given left side is Good, it shouldn't matter if the Bad type has no constraint
      (List(Good(1)): List[Good[Int, Long]]) shouldEqual (List(Good(1)): List[Or[Int, String]])
      (List(Good(1L)): List[Good[Long, Int]]) shouldEqual (List(Good(1)): List[Or[Int, String]])
      // But if left side is Good but without a constraint between left and right Good types, it should not compile, even
      // if the Bad type has a constraint.
      """List((Good(1L)): List[Good[Long, Int]]) shouldEqual (List(Good("one")): List[Good[String, Int]])""" shouldNot typeCheck

      // Right side Good, left side Or
      (List(Good(1)): List[Int Or Int]) shouldEqual (List(Good(1L)): List[Good[Long, Int]])
      (List(Good(1)): List[Int Or Int]) shouldEqual (List(Good(1)): List[Good[Int, Long]])
      (List(Good(1L)): List[Long Or Int]) shouldEqual (List(Good(1)): List[Good[Int, Int]])
      (List(Good(1)): List[Int Or Long]) shouldEqual (List(Good(1)): List[Good[Int, Int]])
      // Given right side is Good, it shouldn't matter if the Bad type has no constraint
      (List(Good(1)): List[Or[Int, Long]]) shouldEqual (List(Good(1)): List[Good[Int, String]])
      (List(Good(1L)): List[Or[Long, Int]]) shouldEqual (List(Good(1)): List[Good[Int, String]])
      // But if right side is Good but without a constraint between left and right Good types, it should not compile, even
      // if the Bad type has a constraint.
      """(List(Good(1L)): List[Or[Long, Int]]) shouldEqual (List(Good("one")): List[Good[String, Int]])""" shouldNot typeCheck

      // Both sides Bad
      (List(Bad(1)): List[Bad[Int, Int]]) shouldEqual (List(Bad(1)): List[Bad[Long, Int]])
      (List(Bad(1)): List[Bad[Int, Int]]) shouldEqual (List(Bad(1L)): List[Bad[Int, Long]])
      (List(Bad(1)): List[Bad[Long, Int]]) shouldEqual (List(Bad(1)): List[Bad[Int, Int]])
      (List(Bad(1L)): List[Bad[Int, Long]]) shouldEqual (List(Bad(1)): List[Bad[Int, Int]])
      // Given both sides are Bad, it shouldn't matter if the Good type has no constraint
      (List(Bad(1)): List[Bad[Long, Int]]) shouldEqual (List(Bad(1)): List[Bad[String, Int]])
      (List(Bad(1L)): List[Bad[Int, Long]]) shouldEqual (List(Bad(1)): List[Bad[String, Int]])
      // But if both sides are Bad but without a constraint, it should not compile, even
      // if the Good type has a constraint.
      """(List(Bad(1L)): List[Bad[Int, Long]]) shouldEqual (List(Bad("one")): List[Bad[Int, String]])""" shouldNot typeCheck

      // Left side Bad, right side Or
      (List(Bad(1)): List[Bad[Int, Int]]) shouldEqual (List(Bad(1)): List[Long Or Int])
      (List(Bad(1)): List[Bad[Int, Int]]) shouldEqual (List(Bad(1L)): List[Int Or Long])
      (List(Bad(1)): List[Bad[Long, Int]]) shouldEqual (List(Bad(1)): List[Int Or Int])
      (List(Bad(1L)): List[Bad[Int, Long]]) shouldEqual (List(Bad(1)): List[Int Or Int])
      // Given left side is Bad, it shouldn't matter if the Good type has no constraint
      (List(Bad(1)): List[Bad[Long, Int]]) shouldEqual (List(Bad(1)): List[Or[String, Int]])
      (List(Bad(1L)): List[Bad[Int, Long]]) shouldEqual (List(Bad(1)): List[Or[String, Int]])
      // But if left side is Bad but without a constraint between left and right Bad types, it should not compile, even
      // if the Good type has a constraint.
      """(List(Bad(1L)): List[Bad[Int, Long]]) shouldEqual (List(Bad("one")): List[Or[Int, String]])""" shouldNot typeCheck

      // Right side Bad, left side Or
      (List(Bad(1)): List[Int Or Int]) shouldEqual (List(Bad(1)): List[Bad[Long, Int]])
      (List(Bad(1)): List[Int Or Int]) shouldEqual (List(Bad(1L)): List[Bad[Int, Long]])
      (List(Bad(1)): List[Long Or Int]) shouldEqual (List(Bad(1)): List[Bad[Int, Int]])
      (List(Bad(1L)): List[Int Or Long]) shouldEqual (List(Bad(1)): List[Bad[Int, Int]])
      // Given right side is Bad, it shouldn't matter if the Good type has no constraint
      (List(Bad(1)): List[Or[Long, Int]]) shouldEqual (List(Bad(1)): List[Bad[String, Int]])
      (List(Bad(1L)): List[Or[Int, Long]]) shouldEqual (List(Bad(1)): List[Bad[String, Int]])
      // But if right side is Bad but without a constraint between left and right Bad types, it should not compile, even
      // if the Good type has a constraint.
      """(List(Bad(1L)): List[Or[Int, Long]]) shouldEqual (List(Bad("one")): List[Bad[Int, String]])""" shouldNot typeCheck

      // Both sides Or
      (List(Good(1)): List[Int Or Int]) shouldEqual (List(Good(1L)): List[Long Or Int])
      (List(Good(1)): List[Int Or Int]) shouldEqual (List(Good(1)): List[Int Or Long])
      (List(Good(1L)): List[Long Or Int]) shouldEqual (List(Good(1)): List[Int Or Int])
      (List(Good(1)): List[Int Or Long]) shouldEqual (List(Good(1)): List[Int Or Int])
      (List(Bad(1)): List[Int Or Int]) shouldEqual (List(Bad(1)): List[Long Or Int])
      (List(Bad(1)): List[Int Or Int]) shouldEqual (List(Bad(1L)): List[Int Or Long])
      (List(Bad(1)): List[Long Or Int]) shouldEqual (List(Bad(1)): List[Int Or Int])
      (List(Bad(1L)): List[Int Or Long]) shouldEqual (List(Bad(1)): List[Int Or Int])
      // So long as an equality constraint exists for one the Good or Bad side of type Or,
      // the comparison will be allowed. This is because it may be true. At the
      // end of the day, a Good[Int].orBad[String] can equal a Good[Int].orBad[java.util.Date]
      // 
      // scala> Good(1).orBad[String] == Good(1L).orBad[java.util.Date]
      // res0: Boolean = true
      // 
      // Similarly, a Good[Int].orBad[String] can equal a Good[java.util.Date].orBad[String]
      // scala> Good[Int].orBad("hi") == Good[java.util.Date].orBad("hi"])
      // res1: Boolean = true
      (List(Good(1).orBad[String]): List[Int Or String]) shouldEqual (List(Good(1L).orBad[Date]): List[Long Or Date])
      (List(Good[Int].orBad("hi")): List[Int Or String]) shouldEqual (List(Good[Date].orBad("hi")): List[Date Or String])
      // The only way an equality comparison of two Ors will not be allowed to compile, therefore, is if
      // no constraint exists between either the Good or Bad types:
      """(List(Good(1)): List[Int Or String]) shouldEqual (List(Good("one")): List[String Or Int])""" shouldNot typeCheck
    }
  }
}

