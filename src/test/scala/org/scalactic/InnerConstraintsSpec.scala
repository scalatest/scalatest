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

//
// Going to need to deal with Array more specially at the nested level. Would need to take the Array
// Equality for the nested one. I think I could do this in general: have special implicits when the
// contained type is Array, for any and all containers. I think that would fix List[Array[T]] too.
//

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
      (Good(1): Good[Int, Long]) shouldEqual (Good(1): Int Or String)
      (Good(1L): Good[Long, Int]) shouldEqual (Good(1): Int Or String)
      // But if left side is Good but without a constraint between left and right Good types, it should not compile, even
      // if the Bad type has a constraint.
      """(Good(1L): Good[Long, Int]) shouldEqual (Good("one"): Good[String, Int])""" shouldNot typeCheck

      // Right side Good, left side Or
      (Good(1): Int Or Int) shouldEqual (Good(1L): Good[Long, Int])
      (Good(1): Int Or Int) shouldEqual (Good(1): Good[Int, Long])
      (Good(1L): Long Or Int) shouldEqual (Good(1): Good[Int, Int])
      (Good(1): Int Or Long) shouldEqual (Good(1): Good[Int, Int])
      // Given right side is Good, it shouldn't matter if the Bad type has no constraint
      (Good(1): Int Or Long) shouldEqual (Good(1): Good[Int, String])
      (Good(1L): Long Or Int) shouldEqual (Good(1): Good[Int, String])
      // But if right side is Good but without a constraint between left and right Good types, it should not compile, even
      // if the Bad type has a constraint.
      """(Good(1L): Long Or Int) shouldEqual (Good("one"): Good[String, Int])""" shouldNot typeCheck

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
      (Bad(1): Bad[Long, Int]) shouldEqual (Bad(1): String Or Int)
      (Bad(1L): Bad[Int, Long]) shouldEqual (Bad(1): String Or Int)
      // But if left side is Bad but without a constraint between left and right Bad types, it should not compile, even
      // if the Good type has a constraint.
      """(Bad(1L): Bad[Int, Long]) shouldEqual (Bad("one"): Int Or String)""" shouldNot typeCheck

      // Right side Bad, left side Or
      (Bad(1): Int Or Int) shouldEqual (Bad(1): Bad[Long, Int])
      (Bad(1): Int Or Int) shouldEqual (Bad(1L): Bad[Int, Long])
      (Bad(1): Long Or Int) shouldEqual (Bad(1): Bad[Int, Int])
      (Bad(1L): Int Or Long) shouldEqual (Bad(1): Bad[Int, Int])
      // Given right side is Bad, it shouldn't matter if the Good type has no constraint
      (Bad(1): Long Or Int) shouldEqual (Bad(1): Bad[String, Int])
      (Bad(1L): Int Or Long) shouldEqual (Bad(1): Bad[String, Int])
      // But if right side is Bad but without a constraint between left and right Bad types, it should not compile, even
      // if the Good type has a constraint.
      """(Bad(1L): Int Or Long) shouldEqual (Bad("one"): Bad[Int, String])""" shouldNot typeCheck

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

      // Much ado about Nothing
      // Both sides Good
      Good(1) shouldEqual Good(1L)
      Good(1) shouldEqual Good(1)
      Good(1L) shouldEqual Good(1)
      Good(1) shouldEqual Good(1)
      // Left side Good, right side Or
      Good(1) shouldEqual Good(1L).asOr
      Good(1) shouldEqual Good(1).asOr
      Good(1L) shouldEqual Good(1).asOr
      // Right side Good, left side Or
      Good(1).asOr shouldEqual Good(1L)
      Good(1).asOr shouldEqual Good(1)
      Good(1L).asOr shouldEqual Good(1)
      // Both sides Bad
      Bad(1) shouldEqual Bad(1)
      Bad(1) shouldEqual Bad(1L)
      Bad(1L) shouldEqual Bad(1)
      // Left side Bad, right side Or
      Bad(1) shouldEqual Bad(1).asOr
      Bad(1) shouldEqual Bad(1L).asOr
      Bad(1L) shouldEqual Bad(1).asOr
      // Right side Bad, left side Or
      Bad(1).asOr shouldEqual Bad(1)
      Bad(1).asOr shouldEqual Bad(1L)
      Bad(1L).asOr shouldEqual Bad(1)
      // Both sides Or
      Good(1).asOr shouldEqual Good(1L).asOr
      Good(1).asOr shouldEqual Good(1).asOr
      Good(1L).asOr shouldEqual Good(1).asOr
      Bad(1).asOr shouldEqual Bad(1).asOr
      Bad(1).asOr shouldEqual Bad(1L).asOr
      Bad(1L).asOr shouldEqual Bad(1).asOr
    }
    object `on Nested Or` {

      def `with List (which is covariant)` {
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
        (List(Good(1)): List[Good[Int, Long]]) shouldEqual (List(Good(1)): List[Int Or String])
        (List(Good(1L)): List[Good[Long, Int]]) shouldEqual (List(Good(1)): List[Int Or String])
        // But if left side is Good but without a constraint between left and right Good types, it should not compile, even
        // if the Bad type has a constraint.
        """List((Good(1L)): List[Good[Long, Int]]) shouldEqual (List(Good("one")): List[Good[String, Int]])""" shouldNot typeCheck
  
        // Right side Good, left side Or
        (List(Good(1)): List[Int Or Int]) shouldEqual (List(Good(1L)): List[Good[Long, Int]])
        (List(Good(1)): List[Int Or Int]) shouldEqual (List(Good(1)): List[Good[Int, Long]])
        (List(Good(1L)): List[Long Or Int]) shouldEqual (List(Good(1)): List[Good[Int, Int]])
        (List(Good(1)): List[Int Or Long]) shouldEqual (List(Good(1)): List[Good[Int, Int]])
        // Given right side is Good, it shouldn't matter if the Bad type has no constraint
        (List(Good(1)): List[Int Or Long]) shouldEqual (List(Good(1)): List[Good[Int, String]])
        (List(Good(1L)): List[Long Or Int]) shouldEqual (List(Good(1)): List[Good[Int, String]])
        // But if right side is Good but without a constraint between left and right Good types, it should not compile, even
        // if the Bad type has a constraint.
        """(List(Good(1L)): List[Long Or Int]) shouldEqual (List(Good("one")): List[Good[String, Int]])""" shouldNot typeCheck
  
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
        (List(Bad(1)): List[Bad[Long, Int]]) shouldEqual (List(Bad(1)): List[String Or Int])
        (List(Bad(1L)): List[Bad[Int, Long]]) shouldEqual (List(Bad(1)): List[String Or Int])
        // But if left side is Bad but without a constraint between left and right Bad types, it should not compile, even
        // if the Good type has a constraint.
        """(List(Bad(1L)): List[Bad[Int, Long]]) shouldEqual (List(Bad("one")): List[Int Or String])""" shouldNot typeCheck
  
        // Right side Bad, left side Or
        (List(Bad(1)): List[Int Or Int]) shouldEqual (List(Bad(1)): List[Bad[Long, Int]])
        (List(Bad(1)): List[Int Or Int]) shouldEqual (List(Bad(1L)): List[Bad[Int, Long]])
        (List(Bad(1)): List[Long Or Int]) shouldEqual (List(Bad(1)): List[Bad[Int, Int]])
        (List(Bad(1L)): List[Int Or Long]) shouldEqual (List(Bad(1)): List[Bad[Int, Int]])
        // Given right side is Bad, it shouldn't matter if the Good type has no constraint
        (List(Bad(1)): List[Long Or Int]) shouldEqual (List(Bad(1)): List[Bad[String, Int]])
        (List(Bad(1L)): List[Int Or Long]) shouldEqual (List(Bad(1)): List[Bad[String, Int]])
        // But if right side is Bad but without a constraint between left and right Bad types, it should not compile, even
        // if the Good type has a constraint.
        """(List(Bad(1L)): List[Int Or Long]) shouldEqual (List(Bad("one")): List[Bad[Int, String]])""" shouldNot typeCheck
  
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
  
        // Much ado about Nothing
        // Both sides List[Good]
        List(Good(1)) shouldEqual List(Good(1L))
        List(Good(1)) shouldEqual List(Good(1))
        List(Good(1L)) shouldEqual List(Good(1))
        List(Good(1)) shouldEqual List(Good(1))
        // Left side List[Good], right side List[Or]
        List(Good(1)) shouldEqual List(Good(1L).asOr)
        List(Good(1)) shouldEqual List(Good(1).asOr)
        List(Good(1L)) shouldEqual List(Good(1).asOr)
        // Right side List[Good], left side List[Or]
        List(Good(1).asOr) shouldEqual List(Good(1L))
        List(Good(1).asOr) shouldEqual List(Good(1))
        List(Good(1L).asOr) shouldEqual List(Good(1))
        // Both sides List[Bad]
        List(Bad(1)) shouldEqual List(Bad(1))
        List(Bad(1)) shouldEqual List(Bad(1L))
        List(Bad(1L)) shouldEqual List(Bad(1))
        // Left side List[Bad], right side List[Or]
        List(Bad(1)) shouldEqual List(Bad(1).asOr)
        List(Bad(1)) shouldEqual List(Bad(1L).asOr)
        List(Bad(1L)) shouldEqual List(Bad(1).asOr)
        // Right side List[Bad], left side List[Or]
        List(Bad(1).asOr) shouldEqual List(Bad(1))
        List(Bad(1).asOr) shouldEqual List(Bad(1L))
        List(Bad(1L).asOr) shouldEqual List(Bad(1))
        // Both sides List[Or]
        List(Good(1).asOr) shouldEqual List(Good(1L).asOr)
        List(Good(1).asOr) shouldEqual List(Good(1).asOr)
        List(Good(1L).asOr) shouldEqual List(Good(1).asOr)
        List(Bad(1).asOr) shouldEqual List(Bad(1).asOr)
        List(Bad(1).asOr) shouldEqual List(Bad(1L).asOr)
        List(Bad(1L).asOr) shouldEqual List(Bad(1).asOr)
      }

      def `with Set (which is invariant)` {
        // Both sides Good
        (Set(Good(1)): Set[Good[Int, Int]]) shouldEqual (Set(Good(1L)): Set[Good[Long, Int]])
        (Set(Good(1)): Set[Good[Int, Int]]) shouldEqual (Set(Good(1)): Set[Good[Int, Long]])
        (Set(Good(1L)): Set[Good[Long, Int]]) shouldEqual (Set(Good(1)): Set[Good[Int, Int]])
        (Set(Good(1)): Set[Good[Int, Long]]) shouldEqual (Set(Good(1)): Set[Good[Int, Int]])
        // Given both sides are Good, it shouldn't matter if the Bad type has no constraint
        (Set(Good(1)): Set[Good[Int, Long]]) shouldEqual (Set(Good(1)): Set[Good[Int, String]])
        (Set(Good(1L)): Set[Good[Long, Int]]) shouldEqual (Set(Good(1)): Set[Good[Int, String]])
        // But if both sides are Good but without a constraint, it should not compile, even
        // if the Bad type has a constraint.
        """(Set(Good(1L)): Set[Good[Long, Int]]) shouldEqual (Set(Good("one")): Set[Good[String, Int]])""" shouldNot typeCheck
  
        // Left side Good, right side Or
        (Set(Good(1)): Set[Good[Int, Int]]) shouldEqual (Set(Good(1L)): Set[Long Or Int])
        (Set(Good(1)): Set[Good[Int, Int]]) shouldEqual (Set(Good(1)): Set[Int Or Long])
        (Set(Good(1L)): Set[Good[Long, Int]]) shouldEqual (Set(Good(1)): Set[Int Or Int])
        (Set(Good(1)): Set[Good[Int, Long]]) shouldEqual (Set(Good(1)): Set[Int Or Int])
        // Given left side is Good, it shouldn't matter if the Bad type has no constraint
        (Set(Good(1)): Set[Good[Int, Long]]) shouldEqual (Set(Good(1)): Set[Int Or String])
        (Set(Good(1L)): Set[Good[Long, Int]]) shouldEqual (Set(Good(1)): Set[Int Or String])
        // But if left side is Good but without a constraint between left and right Good types, it should not compile, even
        // if the Bad type has a constraint.
        """Set((Good(1L)): Set[Good[Long, Int]]) shouldEqual (Set(Good("one")): Set[Good[String, Int]])""" shouldNot typeCheck
  
        // Right side Good, left side Or
        (Set(Good(1)): Set[Int Or Int]) shouldEqual (Set(Good(1L)): Set[Good[Long, Int]])
        (Set(Good(1)): Set[Int Or Int]) shouldEqual (Set(Good(1)): Set[Good[Int, Long]])
        (Set(Good(1L)): Set[Long Or Int]) shouldEqual (Set(Good(1)): Set[Good[Int, Int]])
        (Set(Good(1)): Set[Int Or Long]) shouldEqual (Set(Good(1)): Set[Good[Int, Int]])
        // Given right side is Good, it shouldn't matter if the Bad type has no constraint
        (Set(Good(1)): Set[Int Or Long]) shouldEqual (Set(Good(1)): Set[Good[Int, String]])
        (Set(Good(1L)): Set[Long Or Int]) shouldEqual (Set(Good(1)): Set[Good[Int, String]])
        // But if right side is Good but without a constraint between left and right Good types, it should not compile, even
        // if the Bad type has a constraint.
        """(Set(Good(1L)): Set[Long Or Int]) shouldEqual (Set(Good("one")): Set[Good[String, Int]])""" shouldNot typeCheck
  
        // Both sides Bad
        (Set(Bad(1)): Set[Bad[Int, Int]]) shouldEqual (Set(Bad(1)): Set[Bad[Long, Int]])
        (Set(Bad(1)): Set[Bad[Int, Int]]) shouldEqual (Set(Bad(1L)): Set[Bad[Int, Long]])
        (Set(Bad(1)): Set[Bad[Long, Int]]) shouldEqual (Set(Bad(1)): Set[Bad[Int, Int]])
        (Set(Bad(1L)): Set[Bad[Int, Long]]) shouldEqual (Set(Bad(1)): Set[Bad[Int, Int]])
        // Given both sides are Bad, it shouldn't matter if the Good type has no constraint
        (Set(Bad(1)): Set[Bad[Long, Int]]) shouldEqual (Set(Bad(1)): Set[Bad[String, Int]])
        (Set(Bad(1L)): Set[Bad[Int, Long]]) shouldEqual (Set(Bad(1)): Set[Bad[String, Int]])
        // But if both sides are Bad but without a constraint, it should not compile, even
        // if the Good type has a constraint.
        """(Set(Bad(1L)): Set[Bad[Int, Long]]) shouldEqual (Set(Bad("one")): Set[Bad[Int, String]])""" shouldNot typeCheck
  
        // Left side Bad, right side Or
        (Set(Bad(1)): Set[Bad[Int, Int]]) shouldEqual (Set(Bad(1)): Set[Long Or Int])
        (Set(Bad(1)): Set[Bad[Int, Int]]) shouldEqual (Set(Bad(1L)): Set[Int Or Long])
        (Set(Bad(1)): Set[Bad[Long, Int]]) shouldEqual (Set(Bad(1)): Set[Int Or Int])
        (Set(Bad(1L)): Set[Bad[Int, Long]]) shouldEqual (Set(Bad(1)): Set[Int Or Int])
        // Given left side is Bad, it shouldn't matter if the Good type has no constraint
        (Set(Bad(1)): Set[Bad[Long, Int]]) shouldEqual (Set(Bad(1)): Set[String Or Int])
        (Set(Bad(1L)): Set[Bad[Int, Long]]) shouldEqual (Set(Bad(1)): Set[String Or Int])
        // But if left side is Bad but without a constraint between left and right Bad types, it should not compile, even
        // if the Good type has a constraint.
        """(Set(Bad(1L)): Set[Bad[Int, Long]]) shouldEqual (Set(Bad("one")): Set[Int Or String])""" shouldNot typeCheck
  
        // Right side Bad, left side Or
        (Set(Bad(1)): Set[Int Or Int]) shouldEqual (Set(Bad(1)): Set[Bad[Long, Int]])
        (Set(Bad(1)): Set[Int Or Int]) shouldEqual (Set(Bad(1L)): Set[Bad[Int, Long]])
        (Set(Bad(1)): Set[Long Or Int]) shouldEqual (Set(Bad(1)): Set[Bad[Int, Int]])
        (Set(Bad(1L)): Set[Int Or Long]) shouldEqual (Set(Bad(1)): Set[Bad[Int, Int]])
        // Given right side is Bad, it shouldn't matter if the Good type has no constraint
        (Set(Bad(1)): Set[Long Or Int]) shouldEqual (Set(Bad(1)): Set[Bad[String, Int]])
        (Set(Bad(1L)): Set[Int Or Long]) shouldEqual (Set(Bad(1)): Set[Bad[String, Int]])
        // But if right side is Bad but without a constraint between left and right Bad types, it should not compile, even
        // if the Good type has a constraint.
        """(Set(Bad(1L)): Set[Int Or Long]) shouldEqual (Set(Bad("one")): Set[Bad[Int, String]])""" shouldNot typeCheck
  
        // Both sides Or
        (Set(Good(1)): Set[Int Or Int]) shouldEqual (Set(Good(1L)): Set[Long Or Int])
        (Set(Good(1)): Set[Int Or Int]) shouldEqual (Set(Good(1)): Set[Int Or Long])
        (Set(Good(1L)): Set[Long Or Int]) shouldEqual (Set(Good(1)): Set[Int Or Int])
        (Set(Good(1)): Set[Int Or Long]) shouldEqual (Set(Good(1)): Set[Int Or Int])
        (Set(Bad(1)): Set[Int Or Int]) shouldEqual (Set(Bad(1)): Set[Long Or Int])
        (Set(Bad(1)): Set[Int Or Int]) shouldEqual (Set(Bad(1L)): Set[Int Or Long])
        (Set(Bad(1)): Set[Long Or Int]) shouldEqual (Set(Bad(1)): Set[Int Or Int])
        (Set(Bad(1L)): Set[Int Or Long]) shouldEqual (Set(Bad(1)): Set[Int Or Int])
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
        (Set(Good(1).orBad[String]): Set[Int Or String]) shouldEqual (Set(Good(1L).orBad[Date]): Set[Long Or Date])
        (Set(Good[Int].orBad("hi")): Set[Int Or String]) shouldEqual (Set(Good[Date].orBad("hi")): Set[Date Or String])
        // The only way an equality comparison of two Ors will not be allowed to compile, therefore, is if
        // no constraint exists between either the Good or Bad types:
        """(Set(Good(1)): Set[Int Or String]) shouldEqual (Set(Good("one")): Set[String Or Int])""" shouldNot typeCheck
  
        // Much ado about Nothing
        // Both sides Set[Good]
        Set(Good(1)) shouldEqual Set(Good(1L))
        Set(Good(1)) shouldEqual Set(Good(1))
        Set(Good(1L)) shouldEqual Set(Good(1))
        Set(Good(1)) shouldEqual Set(Good(1))
        // Left side Set[Good], right side Set[Or]
        Set(Good(1)) shouldEqual Set(Good(1L).asOr)
        Set(Good(1)) shouldEqual Set(Good(1).asOr)
        Set(Good(1L)) shouldEqual Set(Good(1).asOr)
        // Right side Set[Good], left side Set[Or]
        Set(Good(1).asOr) shouldEqual Set(Good(1L))
        Set(Good(1).asOr) shouldEqual Set(Good(1))
        Set(Good(1L).asOr) shouldEqual Set(Good(1))
        // Both sides Set[Bad]
        Set(Bad(1)) shouldEqual Set(Bad(1))
        Set(Bad(1)) shouldEqual Set(Bad(1L))
        Set(Bad(1L)) shouldEqual Set(Bad(1))
        // Left side Set[Bad], right side Set[Or]
        Set(Bad(1)) shouldEqual Set(Bad(1).asOr)
        Set(Bad(1)) shouldEqual Set(Bad(1L).asOr)
        Set(Bad(1L)) shouldEqual Set(Bad(1).asOr)
        // Right side Set[Bad], left side Set[Or]
        Set(Bad(1).asOr) shouldEqual Set(Bad(1))
        Set(Bad(1).asOr) shouldEqual Set(Bad(1L))
        Set(Bad(1L).asOr) shouldEqual Set(Bad(1))
        // Both sides Set[Or]
        Set(Good(1).asOr) shouldEqual Set(Good(1L).asOr)
        Set(Good(1).asOr) shouldEqual Set(Good(1).asOr)
        Set(Good(1L).asOr) shouldEqual Set(Good(1).asOr)
        Set(Bad(1).asOr) shouldEqual Set(Bad(1).asOr)
        Set(Bad(1).asOr) shouldEqual Set(Bad(1L).asOr)
        Set(Bad(1L).asOr) shouldEqual Set(Bad(1).asOr)
      }
    }

    def `on Either` {

      // Both sides Left
      (Left(1): Left[Int, Int]) shouldEqual (Left(1L): Left[Long, Int])
      (Left(1): Left[Int, Int]) shouldEqual (Left(1): Left[Int, Long])
      (Left(1L): Left[Long, Int]) shouldEqual (Left(1): Left[Int, Int])
      (Left(1): Left[Int, Long]) shouldEqual (Left(1): Left[Int, Int])
      // Given both sides are Left, it shouldn't matter if the Right type has no constraint
      (Left(1): Left[Int, Long]) shouldEqual (Left(1): Left[Int, String])
      (Left(1L): Left[Long, Int]) shouldEqual (Left(1): Left[Int, String])
      // But if both sides are Left but without a constraint, it should not compile, even
      // if the Right type has a constraint.
      """(Left(1L): Left[Long, Int]) shouldEqual (Left("one"): Left[String, Int])""" shouldNot typeCheck

      // Left side Left, right side Either
      (Left(1): Left[Int, Int]) shouldEqual (Left(1L): Either[Long, Int])
      (Left(1): Left[Int, Int]) shouldEqual (Left(1): Either[Int, Long])
      (Left(1L): Left[Long, Int]) shouldEqual (Left(1): Either[Int, Int])
      (Left(1): Left[Int, Long]) shouldEqual (Left(1): Either[Int, Int])
      // Given left side is Left, it shouldn't matter if the Right type has no constraint
      (Left(1): Left[Int, Long]) shouldEqual (Left(1): Either[Int, String])
      (Left(1L): Left[Long, Int]) shouldEqual (Left(1): Either[Int, String])
      // But if left side is Left but without a constraint between left and right Left types, it should not compile, even
      // if the Right type has a constraint.
      """(Left(1L): Left[Long, Int]) shouldEqual (Left("one"): Left[String, Int])""" shouldNot typeCheck

      // Right side Left, left side Either
      (Left(1): Either[Int, Int]) shouldEqual (Left(1L): Left[Long, Int])
      (Left(1): Either[Int, Int]) shouldEqual (Left(1): Left[Int, Long])
      (Left(1L): Either[Long, Int]) shouldEqual (Left(1): Left[Int, Int])
      (Left(1): Either[Int, Long]) shouldEqual (Left(1): Left[Int, Int])
      // Given right side is Left, it shouldn't matter if the Right type has no constraint
      (Left(1): Either[Int, Long]) shouldEqual (Left(1): Left[Int, String])
      (Left(1L): Either[Long, Int]) shouldEqual (Left(1): Left[Int, String])
      // But if right side is Left but without a constraint between left and right Left types, it should not compile, even
      // if the Right type has a constraint.
      """(Left(1L): Either[Long, Int]) shouldEqual (Left("one"): Left[String, Int])""" shouldNot typeCheck

      // Both sides Right
      (Right(1): Right[Int, Int]) shouldEqual (Right(1): Right[Long, Int])
      (Right(1): Right[Int, Int]) shouldEqual (Right(1L): Right[Int, Long])
      (Right(1): Right[Long, Int]) shouldEqual (Right(1): Right[Int, Int])
      (Right(1L): Right[Int, Long]) shouldEqual (Right(1): Right[Int, Int])
      // Given both sides are Right, it shouldn't matter if the Left type has no constraint
      (Right(1): Right[Long, Int]) shouldEqual (Right(1): Right[String, Int])
      (Right(1L): Right[Int, Long]) shouldEqual (Right(1): Right[String, Int])
      // But if both sides are Right but without a constraint, it should not compile, even
      // if the Left type has a constraint.
      """(Right(1L): Right[Int, Long]) shouldEqual (Right("one"): Right[Int, String])""" shouldNot typeCheck

      // Left side Right, right side Either
      (Right(1): Right[Int, Int]) shouldEqual (Right(1): Either[Long, Int])
      (Right(1): Right[Int, Int]) shouldEqual (Right(1L): Either[Int, Long])
      (Right(1): Right[Long, Int]) shouldEqual (Right(1): Either[Int, Int])
      (Right(1L): Right[Int, Long]) shouldEqual (Right(1): Either[Int, Int])
      // Given left side is Right, it shouldn't matter if the Left type has no constraint
      (Right(1): Right[Long, Int]) shouldEqual (Right(1): Either[String, Int])
      (Right(1L): Right[Int, Long]) shouldEqual (Right(1): Either[String, Int])
      // But if left side is Right but without a constraint between left and right Right types, it should not compile, even
      // if the Left type has a constraint.
      """(Right(1L): Right[Int, Long]) shouldEqual (Right("one"): Either[Int, String])""" shouldNot typeCheck

      // Right side Right, left side Either
      (Right(1): Either[Int, Int]) shouldEqual (Right(1): Right[Long, Int])
      (Right(1): Either[Int, Int]) shouldEqual (Right(1L): Right[Int, Long])
      (Right(1): Either[Long, Int]) shouldEqual (Right(1): Right[Int, Int])
      (Right(1L): Either[Int, Long]) shouldEqual (Right(1): Right[Int, Int])
      // Given right side is Right, it shouldn't matter if the Left type has no constraint
      (Right(1): Either[Long, Int]) shouldEqual (Right(1): Right[String, Int])
      (Right(1L): Either[Int, Long]) shouldEqual (Right(1): Right[String, Int])
      // But if right side is Right but without a constraint between left and right Right types, it should not compile, even
      // if the Left type has a constraint.
      """(Right(1L): Either[Int, Long]) shouldEqual (Right("one"): Right[Int, String])""" shouldNot typeCheck

      // Both sides Either
      (Left(1): Either[Int, Int]) shouldEqual (Left(1L): Either[Long, Int])
      (Left(1): Either[Int, Int]) shouldEqual (Left(1): Either[Int, Long])
      (Left(1L): Either[Long, Int]) shouldEqual (Left(1): Either[Int, Int])
      (Left(1): Either[Int, Long]) shouldEqual (Left(1): Either[Int, Int])
      (Right(1): Either[Int, Int]) shouldEqual (Right(1): Either[Long, Int])
      (Right(1): Either[Int, Int]) shouldEqual (Right(1L): Either[Int, Long])
      (Right(1): Either[Long, Int]) shouldEqual (Right(1): Either[Int, Int])
      (Right(1L): Either[Int, Long]) shouldEqual (Right(1): Either[Int, Int])
      // So long as an equality constraint exists for one the Left or Right side of type Either,
      // the comparison will be allowed. This is because it may be true. At the
      // end of the day, a Left[Int, String] can equal a Left[Int, java.util.Date]
      // 
      // scala> Left[Int, String](1) == Left[Long, java.util.Date](1L)
      // res0: Boolean = true
      // 
      // Similarly, a Right[Int, String] can equal a Right[java.util.Date, String]
      // scala> Right[Int, String]("hi") == Right[java.util.Date, String]("hi")
      // res1: Boolean = true
      (Left[Int, String](1): Either[Int, String]) shouldEqual (Left[Long, Date](1L): Either[Long, Date])
      (Right[Int, String]("hi"): Either[Int, String]) shouldEqual (Right[Date, String]("hi"): Either[Date, String])
      // The only way an equality comparison of two Eithers will not be allowed to compile, therefore, is if
      // no constraint exists between either the Left or Right types:
      """(Left[Int, String](1): Either[Int, String]) shouldEqual (Left[String, Int]("one"): Either[String, Int])""" shouldNot typeCheck
    }
    def `on Nested Either` {

      // Both sides Left
      (List(Left(1)): List[Left[Int, Int]]) shouldEqual (List(Left(1L)): List[Left[Long, Int]])
      (List(Left(1)): List[Left[Int, Int]]) shouldEqual (List(Left(1)): List[Left[Int, Long]])
      (List(Left(1L)): List[Left[Long, Int]]) shouldEqual (List(Left(1)): List[Left[Int, Int]])
      (List(Left(1)): List[Left[Int, Long]]) shouldEqual (List(Left(1)): List[Left[Int, Int]])
      // Given both sides are Left, it shouldn't matter if the Right type has no constraint
      (List(Left(1)): List[Left[Int, Long]]) shouldEqual (List(Left(1)): List[Left[Int, String]])
      (List(Left(1L)): List[Left[Long, Int]]) shouldEqual (List(Left(1)): List[Left[Int, String]])
      // But if both sides are Left but without a constraint, it should not compile, even
      // if the Right type has a constraint.
      """(List(Left(1L)): List[Left[Long, Int]]) shouldEqual (List(Left("one")): List[Left[String, Int]])""" shouldNot typeCheck

      // Left side Left, right side Either
      (List(Left(1)): List[Left[Int, Int]]) shouldEqual (List(Left(1L)): List[Either[Long, Int]])
      (List(Left(1)): List[Left[Int, Int]]) shouldEqual (List(Left(1)): List[Either[Int, Long]])
      (List(Left(1L)): List[Left[Long, Int]]) shouldEqual (List(Left(1)): List[Either[Int, Int]])
      (List(Left(1)): List[Left[Int, Long]]) shouldEqual (List(Left(1)): List[Either[Int, Int]])
      // Given left side is Left, it shouldn't matter if the Right type has no constraint
      (List(Left(1)): List[Left[Int, Long]]) shouldEqual (List(Left(1)): List[Either[Int, String]])
      (List(Left(1L)): List[Left[Long, Int]]) shouldEqual (List(Left(1)): List[Either[Int, String]])
      // But if left side is Left but without a constraint between left and right Left types, it should not compile, even
      // if the Right type has a constraint.
      """List((Left(1L)): List[Left[Long, Int]]) shouldEqual (List(Left("one")): List[Left[String, Int]])""" shouldNot typeCheck

      // Right side Left, left side Either
      (List(Left(1)): List[Either[Int, Int]]) shouldEqual (List(Left(1L)): List[Left[Long, Int]])
      (List(Left(1)): List[Either[Int, Int]]) shouldEqual (List(Left(1)): List[Left[Int, Long]])
      (List(Left(1L)): List[Either[Long, Int]]) shouldEqual (List(Left(1)): List[Left[Int, Int]])
      (List(Left(1)): List[Either[Int, Long]]) shouldEqual (List(Left(1)): List[Left[Int, Int]])
      // Given right side is Left, it shouldn't matter if the Right type has no constraint
      (List(Left(1)): List[Either[Int, Long]]) shouldEqual (List(Left(1)): List[Left[Int, String]])
      (List(Left(1L)): List[Either[Long, Int]]) shouldEqual (List(Left(1)): List[Left[Int, String]])
      // But if right side is Left but without a constraint between left and right Left types, it should not compile, even
      // if the Right type has a constraint.
      """(List(Left(1L)): List[Either[Long, Int]]) shouldEqual (List(Left("one")): List[Left[String, Int]])""" shouldNot typeCheck

      // Both sides Right
      (List(Right(1)): List[Right[Int, Int]]) shouldEqual (List(Right(1)): List[Right[Long, Int]])
      (List(Right(1)): List[Right[Int, Int]]) shouldEqual (List(Right(1L)): List[Right[Int, Long]])
      (List(Right(1)): List[Right[Long, Int]]) shouldEqual (List(Right(1)): List[Right[Int, Int]])
      (List(Right(1L)): List[Right[Int, Long]]) shouldEqual (List(Right(1)): List[Right[Int, Int]])
      // Given both sides are Right, it shouldn't matter if the Left type has no constraint
      (List(Right(1)): List[Right[Long, Int]]) shouldEqual (List(Right(1)): List[Right[String, Int]])
      (List(Right(1L)): List[Right[Int, Long]]) shouldEqual (List(Right(1)): List[Right[String, Int]])
      // But if both sides are Right but without a constraint, it should not compile, even
      // if the Left type has a constraint.
      """(List(Right(1L)): List[Right[Int, Long]]) shouldEqual (List(Right("one")): List[Right[Int, String]])""" shouldNot typeCheck

      // Left side Right, right side Either
      (List(Right(1)): List[Right[Int, Int]]) shouldEqual (List(Right(1)): List[Either[Long, Int]])
      (List(Right(1)): List[Right[Int, Int]]) shouldEqual (List(Right(1L)): List[Either[Int, Long]])
      (List(Right(1)): List[Right[Long, Int]]) shouldEqual (List(Right(1)): List[Either[Int, Int]])
      (List(Right(1L)): List[Right[Int, Long]]) shouldEqual (List(Right(1)): List[Either[Int, Int]])
      // Given left side is Right, it shouldn't matter if the Left type has no constraint
      (List(Right(1)): List[Right[Long, Int]]) shouldEqual (List(Right(1)): List[Either[String, Int]])
      (List(Right(1L)): List[Right[Int, Long]]) shouldEqual (List(Right(1)): List[Either[String, Int]])
      // But if left side is Right but without a constraint between left and right Right types, it should not compile, even
      // if the Left type has a constraint.
      """(List(Right(1L)): List[Right[Int, Long]]) shouldEqual (List(Right("one")): List[Either[Int, String]])""" shouldNot typeCheck

      // Right side Right, left side Either
      (List(Right(1)): List[Either[Int, Int]]) shouldEqual (List(Right(1)): List[Right[Long, Int]])
      (List(Right(1)): List[Either[Int, Int]]) shouldEqual (List(Right(1L)): List[Right[Int, Long]])
      (List(Right(1)): List[Either[Long, Int]]) shouldEqual (List(Right(1)): List[Right[Int, Int]])
      (List(Right(1L)): List[Either[Int, Long]]) shouldEqual (List(Right(1)): List[Right[Int, Int]])
      // Given right side is Right, it shouldn't matter if the Left type has no constraint
      (List(Right(1)): List[Either[Long, Int]]) shouldEqual (List(Right(1)): List[Right[String, Int]])
      (List(Right(1L)): List[Either[Int, Long]]) shouldEqual (List(Right(1)): List[Right[String, Int]])
      // But if right side is Right but without a constraint between left and right Right types, it should not compile, even
      // if the Left type has a constraint.
      """(List(Right(1L)): List[Either[Int, Long]]) shouldEqual (List(Right("one")): List[Right[Int, String]])""" shouldNot typeCheck

      // Both sides Either
      (List(Left(1)): List[Either[Int, Int]]) shouldEqual (List(Left(1L)): List[Either[Long, Int]])
      (List(Left(1)): List[Either[Int, Int]]) shouldEqual (List(Left(1)): List[Either[Int, Long]])
      (List(Left(1L)): List[Either[Long, Int]]) shouldEqual (List(Left(1)): List[Either[Int, Int]])
      (List(Left(1)): List[Either[Int, Long]]) shouldEqual (List(Left(1)): List[Either[Int, Int]])
      (List(Right(1)): List[Either[Int, Int]]) shouldEqual (List(Right(1)): List[Either[Long, Int]])
      (List(Right(1)): List[Either[Int, Int]]) shouldEqual (List(Right(1L)): List[Either[Int, Long]])
      (List(Right(1)): List[Either[Long, Int]]) shouldEqual (List(Right(1)): List[Either[Int, Int]])
      (List(Right(1L)): List[Either[Int, Long]]) shouldEqual (List(Right(1)): List[Either[Int, Int]])
      // So long as an equality constraint exists for one the Left or Right side of type Either,
      // the comparison will be allowed. This is because it may be true. At the
      // end of the day, a Left[Int, String] can equal a Left[Int, java.util.Date]
      // 
      // scala> Left[Int, String](1) == Left[Long, java.util.Date](1L)
      // res0: Boolean = true
      // 
      // Similarly, a Right[Int, String] can equal a Right[java.util.Date, String]
      // scala> Right[Int, String]("hi") == Right[java.util.Date, String]("hi")
      // res1: Boolean = true
      (List(Left[Int, String](1)): List[Either[Int, String]]) shouldEqual (List(Left[Long, Date](1L)): List[Either[Long, Date]])
      (List(Right[Int, String]("hi")): List[Either[Int, String]]) shouldEqual (List(Right[Date, String]("hi")): List[Either[Date, String]])
      // The only way an equality comparison of two Eithers will not be allowed to compile, therefore, is if
      // no constraint exists between either the Left or Right types:
      """(List(Left(1)): List[Either[Int, String]]) shouldEqual (List(Left("one")): List[Either[String, Int]])""" shouldNot typeCheck
    }
  }
}

