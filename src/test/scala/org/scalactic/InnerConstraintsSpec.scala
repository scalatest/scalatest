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

class InnerConstraintsSpec extends Spec with Matchers with TypeCheckedTripleEquals {
  
  object `Inner constraints should enable equality comparisons` {
    def `on Every` {
      One(1) shouldEqual One(1L)
      Many(1, 2) shouldEqual Many(1L, 2L)

      Every(1) shouldEqual One(1L)
      Every(1, 2) shouldEqual Many(1L, 2L)

      One(1) shouldEqual Every(1L)
      Many(1, 2) shouldEqual Every(1L, 2L)

      Every(1) shouldEqual Every(1L)
      Every(1, 2) shouldEqual Every(1L, 2L)
    }
    def `on Or` {

      // Both sides Good
      (Good(1): Good[Int, Int]) shouldEqual (Good(1L): Good[Long, Int])
      (Good(1): Good[Int, Int]) shouldEqual (Good(1): Good[Int, Long])
      (Good(1L): Good[Long, Int]) shouldEqual (Good(1): Good[Int, Int])
      (Good(1): Good[Int, Long]) shouldEqual (Good(1): Good[Int, Int])
      // Given both sides are Good, it shouldn't matter if the bad type has no constraint
      (Good(1): Good[Int, Long]) shouldEqual (Good(1): Good[Int, String])
      (Good(1L): Good[Long, Int]) shouldEqual (Good(1): Good[Int, String])
      // But if there's a ...

      // Left side Good, right side Or
      (Good(1): Good[Int, Int]) shouldEqual (Good(1L): Long Or Int)
      (Good(1): Good[Int, Int]) shouldEqual (Good(1): Int Or Long)
      (Good(1L): Good[Long, Int]) shouldEqual (Good(1): Int Or Int)
      (Good(1): Good[Int, Long]) shouldEqual (Good(1): Int Or Int)
      // If left side is Good, right side must be an Or with a valid Good type.
      // Therefore, if right side is an Or with mismatched Good type but valid Bad type, it should not type check
      """(Good(1L): Good[Long, Int]) shouldEqual (Good("hi"): String Or Int)""" shouldNot typeCheck

      // Right side Good, left side Or
      (Good(1): Int Or Int) shouldEqual (Good(1L): Good[Long, Int])
      (Good(1): Int Or Int) shouldEqual (Good(1): Good[Int, Long])
      (Good(1L): Long Or Int) shouldEqual (Good(1): Good[Int, Int])
      (Good(1): Int Or Long) shouldEqual (Good(1): Good[Int, Int])

      // Both sides Bad
      (Bad(1): Bad[Int, Int]) shouldEqual (Bad(1): Bad[Long, Int])
      (Bad(1): Bad[Int, Int]) shouldEqual (Bad(1L): Bad[Int, Long])
      (Bad(1): Bad[Long, Int]) shouldEqual (Bad(1): Bad[Int, Int])
      (Bad(1L): Bad[Int, Long]) shouldEqual (Bad(1): Bad[Int, Int])

      // Left side Bad, right side Or
      (Bad(1): Bad[Int, Int]) shouldEqual (Bad(1): Long Or Int)
      (Bad(1): Bad[Int, Int]) shouldEqual (Bad(1L): Int Or Long)
      (Bad(1): Bad[Long, Int]) shouldEqual (Bad(1): Int Or Int)
      (Bad(1L): Bad[Int, Long]) shouldEqual (Bad(1): Int Or Int)

      // Right side Bad, left side Or
      (Bad(1): Int Or Int) shouldEqual (Bad(1): Bad[Long, Int])
      (Bad(1): Int Or Int) shouldEqual (Bad(1L): Bad[Int, Long])
      (Bad(1): Long Or Int) shouldEqual (Bad(1): Bad[Int, Int])
      (Bad(1L): Int Or Long) shouldEqual (Bad(1): Bad[Int, Int])

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
    }
/*
   If left side is Good, right side must be an Or with a valid Good type.
   If left side is Bad, right side must be an Or with a valid Bad type.
   If left side is Or, and right side is Bad, left side must have valid Bad type.
   If left side is Or, and right side is Good, left side must have valid Good type.
*/
  }
}

