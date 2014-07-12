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

import org.scalatest.exceptions.TestFailedException
import FailureMessages._
import Matchers._
import org.scalactic.TypeCheckedTripleEquals

class ShouldContainElementTypeCheckSpec extends Spec with TypeCheckedTripleEquals {

  def `should give a compiler error` {
    """List(1, 2, 3) should contain ("hi")""" shouldNot typeCheck
    // """List(1, 2, 3) should contain oneOf ("hi", "ho")""" shouldNot typeCheck
  }

  // Checking for a specific size
  object `The 'contain (<element>)' syntax` {

    object `should give a type error if the types are not compatible` {

      def `on Array` {

        """Array(1, 2) should contain ("2")""" shouldNot typeCheck
        """Array(1, 2) should (contain ("2"))""" shouldNot typeCheck

        """Array(1, 2) should not { contain ("3") }""" shouldNot typeCheck
        """Array(1, 2) should not contain ("3")""" shouldNot typeCheck

        """Array(1, 2) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """Array(1, 2) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """Array(1, 2) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """Array(1, 2) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """Array(1, 2) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """Array(1, 2) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """Array(1, 2) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """Array(1, 2) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """Array(1, 2) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """Array(1, 2) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """Array(1, 2) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """Array(1, 2) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """Array(1, 2) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """Array(1, 2) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """Array(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """Array(1, 2) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """Array(1, 2) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """Array(1, 2) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """Array(1, 2) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """Array(1, 2) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """Array(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """Array(1, 2) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """Array(1, 2) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """Array(1, 2) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.Set` {

        """Set(1, 2) should contain ("2")""" shouldNot typeCheck
        """Set(1, 2) should (contain ("2"))""" shouldNot typeCheck

        """Set(1, 2) should not { contain ("3") }""" shouldNot typeCheck
        """Set(1, 2) should not contain ("3")""" shouldNot typeCheck

        """Set(1, 2) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """Set(1, 2) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """Set(1, 2) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """Set(1, 2) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """Set(1, 2) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """Set(1, 2) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """Set(1, 2) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """Set(1, 2) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """Set(1, 2) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """Set(1, 2) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """Set(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """Set(1, 2) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """Set(1, 2) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """Set(1, 2) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """Set(1, 2) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """Set(1, 2) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """Set(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """Set(1, 2) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """Set(1, 2) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """Set(1, 2) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.Set` {

        import scala.collection.mutable

        """mutable.Set(1, 2) should contain ("2")""" shouldNot typeCheck
        """mutable.Set(1, 2) should (contain ("2"))""" shouldNot typeCheck

        """mutable.Set(1, 2) should not { contain ("3") }""" shouldNot typeCheck
        """mutable.Set(1, 2) should not contain ("3")""" shouldNot typeCheck

        """mutable.Set(1, 2) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """mutable.Set(1, 2) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """mutable.Set(1, 2) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """mutable.Set(1, 2) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """mutable.Set(1, 2) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """mutable.Set(1, 2) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """mutable.Set(1, 2) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """mutable.Set(1, 2) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on scala.collection.Set` {

        val set: scala.collection.Set[Int] = Set(1, 2)

        """set should contain ("2")""" shouldNot typeCheck
        """set should (contain ("2"))""" shouldNot typeCheck

        """set should not { contain ("3") }""" shouldNot typeCheck
        """set should not contain ("3")""" shouldNot typeCheck

        """set should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """set should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """set should (contain ("2") and contain (1))""" shouldNot typeCheck
        """set should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """set should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """set should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """set should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """set should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """set should (contain ("77") or contain (2))""" shouldNot typeCheck
        """set should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """set should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """set should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """set should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """set should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """set should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """set should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """set should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """set should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """set should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """set should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """set should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """set should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """set should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """set should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.HashSet` {

        import scala.collection.immutable.HashSet

        """Set(1, 2) should contain ("2")""" shouldNot typeCheck
        """Set(1, 2) should (contain ("2"))""" shouldNot typeCheck

        """Set(1, 2) should not { contain ("3") }""" shouldNot typeCheck
        """Set(1, 2) should not contain ("3")""" shouldNot typeCheck

        """Set(1, 2) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """Set(1, 2) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """Set(1, 2) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """Set(1, 2) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """Set(1, 2) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """Set(1, 2) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """Set(1, 2) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """Set(1, 2) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """Set(1, 2) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """Set(1, 2) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """Set(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """Set(1, 2) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """Set(1, 2) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """Set(1, 2) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """Set(1, 2) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """Set(1, 2) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """Set(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """Set(1, 2) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """Set(1, 2) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """Set(1, 2) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.HashSet` {

        import scala.collection.mutable

        """mutable.HashSet(1, 2) should contain ("2")""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (contain ("2"))""" shouldNot typeCheck

        """mutable.HashSet(1, 2) should not { contain ("3") }""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should not contain ("3")""" shouldNot typeCheck

        """mutable.HashSet(1, 2) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """mutable.HashSet(1, 2) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """mutable.HashSet(1, 2) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """mutable.HashSet(1, 2) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on List` {

        """List(1, 2) should contain ("2")""" shouldNot typeCheck
        """List(1, 2) should (contain ("2"))""" shouldNot typeCheck

        """List(1, 2) should not { contain ("3") }""" shouldNot typeCheck
        """List(1, 2) should not contain ("3")""" shouldNot typeCheck

        """List(1, 2) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """List(1, 2) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """List(1, 2) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """List(1, 2) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """List(1, 2) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """List(1, 2) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """List(1, 2) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """List(1, 2) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """List(1, 2) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """List(1, 2) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """List(1, 2) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """List(1, 2) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """List(1, 2) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """List(1, 2) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """List(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """List(1, 2) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """List(1, 2) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """List(1, 2) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """List(1, 2) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """List(1, 2) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """List(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """List(1, 2) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """List(1, 2) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """List(1, 2) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on Vector` {

        """Vector(1, 2) should contain ("2")""" shouldNot typeCheck
        """Vector(1, 2) should (contain ("2"))""" shouldNot typeCheck

        """Vector(1, 2) should not { contain ("3") }""" shouldNot typeCheck
        """Vector(1, 2) should not contain ("3")""" shouldNot typeCheck

        """Vector(1, 2) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """Vector(1, 2) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """Vector(1, 2) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """Vector(1, 2) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """Vector(1, 2) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """Vector(1, 2) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """Vector(1, 2) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """Vector(1, 2) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """Vector(1, 2) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """Vector(1, 2) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """Vector(1, 2) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """Vector(1, 2) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """Vector(1, 2) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """Vector(1, 2) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """Vector(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """Vector(1, 2) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """Vector(1, 2) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """Vector(1, 2) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """Vector(1, 2) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """Vector(1, 2) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """Vector(1, 2) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """Vector(1, 2) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """Vector(1, 2) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """Vector(1, 2) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on java.util.List` {

        val javaList: java.util.List[Int] = new java.util.ArrayList
        javaList.add(1)
        javaList.add(2)
      
        """javaList should contain ("2")""" shouldNot typeCheck
        """javaList should (contain ("2"))""" shouldNot typeCheck

        """javaList should not { contain ("3") }""" shouldNot typeCheck
        """javaList should not contain ("3")""" shouldNot typeCheck

        """javaList should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """javaList should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """javaList should (contain ("2") and contain (1))""" shouldNot typeCheck
        """javaList should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """javaList should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """javaList should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """javaList should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """javaList should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """javaList should (contain ("77") or contain (2))""" shouldNot typeCheck
        """javaList should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """javaList should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """javaList should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """javaList should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """javaList should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """javaList should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """javaList should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """javaList should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """javaList should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """javaList should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """javaList should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """javaList should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """javaList should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """javaList should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """javaList should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
    }
  }
/*
    object `on scala.collection.immutable.Map ` {

      def `should do nothing if map contains specified element` {
        Map("one" -> 1, "two" -> 2) should contain ("two" -> 2)
        Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2))
        Map(1 -> "one", 2 -> "two") should contain (2 -> "two")
      }

      def `should do nothing if map does not contain the specified element and used with not` {
        Map("one" -> 1, "two" -> 2) should not { contain ("three" -> 3) }
        Map("one" -> 1, "two" -> 2) should not contain ("three" -> 3)
        Map("one" -> 1, "two" -> 2) should (not contain ("three" -> 3))
      }

      def `should do nothing when map contains specified element and used in a logical-and expression` {
        Map("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> 1)) }
        Map("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> 1))
      }

      def `should do nothing when map contains specified element and used in a logical-or expression` {
        Map("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        Map("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        Map("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      def `should do nothing when map does not contain the specified element and used in a logical-and expression with not` {
        Map("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        Map("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        Map("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      def `should do nothing when map does not contain the specified element and used in a logical-or expression with not` {
        Map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        Map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      def `should throw TestFailedException if map does not contain the specified element` {
        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should contain ("three" -> 3)
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3)")
      }

      def `should throw TestFailedException if contains the specified element when used with not` {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2))
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should not (contain ("two" -> 2))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should not contain ("two" -> 2)
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      def `should throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")
      }

      def `should throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")
      }

      def `should throw an TestFailedException when map contains specified element and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      def `should throw an TestFailedException when map contains specified element and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      def `should work on parallel form` {
        Map("one" -> 1, "two" -> 2).par should contain ("two" -> 2)
      }
    }

    object `on scala.collection.mutable.Map ` {

      import scala.collection.mutable

      def `should do nothing if map contains specified element` {
        mutable.Map("one" -> 1, "two" -> 2) should contain ("two" -> 2)
        mutable.Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2))
        mutable.Map(1 -> "one", 2 -> "two") should contain (2 -> "two")
      }

      def `should do nothing if map does not contain the specified element and used with not` {
        mutable.Map("one" -> 1, "two" -> 2) should not { contain ("three" -> 3) }
        mutable.Map("one" -> 1, "two" -> 2) should not contain ("three" -> 3)
        mutable.Map("one" -> 1, "two" -> 2) should (not contain ("three" -> 3))
      }

      def `should do nothing when map contains specified element and used in a logical-and expression` {
        mutable.Map("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> 1)) }
        mutable.Map("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        mutable.Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> 1))
      }

      def `should do nothing when map contains specified element and used in a logical-or expression` {
        mutable.Map("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        mutable.Map("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        mutable.Map("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      def `should do nothing when map does not contain the specified element and used in a logical-and expression with not` {
        mutable.Map("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        mutable.Map("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        mutable.Map("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      def `should do nothing when map does not contain the specified element and used in a logical-or expression with not` {
        mutable.Map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        mutable.Map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        mutable.Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      def `should throw TestFailedException if map does not contain the specified element` {
        val map = mutable.Map("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {(
          map should contain ("three" -> 3)
        )}
        assert(caught1.getMessage === decorateToStringValue(map) + " did not contain element (three,3)")
      }

      def `should throw TestFailedException if contains the specified element when used with not` {
        val map1 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {(
          map1 should (not contain ("two" -> 2))
        )}
        assert(caught1.getMessage === decorateToStringValue(map1) + " contained element (two,2)")
        
        val map2 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {(
          map2 should not (contain ("two" -> 2))
        )}
        assert(caught2.getMessage === decorateToStringValue(map2) + " contained element (two,2)")

        val map3 = mutable.Map("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
           map3 should not contain ("two" -> 2)
        }
        assert(caught3.getMessage === decorateToStringValue(map3) + " contained element (two,2)")
      }

      def `should throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression` {
        val map1 = mutable.Map("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === decorateToStringValue(map1) + " did not contain element (five,5)")

        val map2 = mutable.Map("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(map2) + " did not contain element (five,5)")

        val map3 = mutable.Map("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(map3) + " did not contain element (five,5)")
      }

      def `should throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression` {
        val map1 = mutable.Map("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage === decorateToStringValue(map1) + " did not contain element (fifty five,55), and " + decorateToStringValue(map1) + " did not contain element (twenty two,22)")

        val map2 = mutable.Map("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === decorateToStringValue(map2) + " did not contain element (fifty five,55), and " + decorateToStringValue(map2) + " did not contain element (twenty two,22)")
        
        val map3 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {(
          map3 should (contain ("fifty five" -> 55) or contain ("twenty two" -> 22)))
        }
        assert(caught3.getMessage === decorateToStringValue(map3) + " did not contain element (fifty five,55), and " + decorateToStringValue(map3) + " did not contain element (twenty two,22)")
      }

      def `should throw an TestFailedException when map contains specified element and used in a logical-and expression with not` {
        val map1 = mutable.Map("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === decorateToStringValue(map1) + " did not contain element (three,3), but " + decorateToStringValue(map1) + " contained element (two,2)")

        val map2 = mutable.Map("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(map2) + " did not contain element (three,3), but " + decorateToStringValue(map2) + " contained element (two,2)")

        val map3 = mutable.Map("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(map3) + " did not contain element (three,3), but " + decorateToStringValue(map3) + " contained element (two,2)")
      }

      def `should throw an TestFailedException when map contains specified element and used in a logical-or expression with not` {
        val map1 = mutable.Map("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === decorateToStringValue(map1) + " contained element (two,2), and " + decorateToStringValue(map1) + " contained element (two,2)")
        
        val map2 = mutable.Map("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(map2) + " contained element (two,2), and " + decorateToStringValue(map2) + " contained element (two,2)")
        
        val map3 = mutable.Map("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(map3) + " contained element (two,2), and " + decorateToStringValue(map3) + " contained element (two,2)")
      }

      def `should work on parallel form` {
        mutable.Map("one" -> 1, "two" -> 2).par should contain ("two" -> 2)
      }
    }

    object `on scala.collection.Map ` {

      val map: scala.collection.Map[String, Int] = Map("one" -> 1, "two" -> 2)

      def `should do nothing if map contains specified element` {
        map should contain ("two" -> 2)
        map should (contain ("two" -> 2))
        map should contain ("two" -> 2)
      }

      def `should do nothing if map does not contain the specified element and used with not` {
        map should not { contain ("three" -> 3) }
        map should not contain ("three" -> 3)
        map should (not contain ("three" -> 3))
      }

      def `should do nothing when map contains specified element and used in a logical-and expression` {
        map should { contain ("two" -> 2) and (contain ("one" -> 1)) }
        map should ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        map should (contain ("two" -> 2) and contain ("one" -> 1))
      }

      def `should do nothing when map contains specified element and used in a logical-or expression` {
        map should { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        map should ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        map should (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      def `should do nothing when map does not contain the specified element and used in a logical-and expression with not` {
        map should { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        map should ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        map should (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      def `should do nothing when map does not contain the specified element and used in a logical-or expression with not` {
        map should { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        map should ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        map should (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      def `should throw TestFailedException if map does not contain the specified element` {
        val caught1 = intercept[TestFailedException] {
          map should contain ("three" -> 3)
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3)")
      }

      def `should throw TestFailedException if contains the specified element when used with not` {

        val caught1 = intercept[TestFailedException] {
          map should (not contain ("two" -> 2))
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          map should not (contain ("two" -> 2))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          map should not contain ("two" -> 2)
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      def `should throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          map should { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")

        val caught2 = intercept[TestFailedException] {
          map should ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")

        val caught3 = intercept[TestFailedException] {
          map should (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")
      }

      def `should throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          map should { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")

        val caught2 = intercept[TestFailedException] {
          map should ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")

        val caught3 = intercept[TestFailedException] {
          map should (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")
      }

      def `should throw an TestFailedException when map contains specified element and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          map should { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          map should ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          map should (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      def `should throw an TestFailedException when map contains specified element and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          map should { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          map should ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          map should (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      def `should work on parallel form` {
        map.par should contain ("two" -> 2)
      }
    }

    object `on scala.collection.immutable.HashMap ` {

      import scala.collection.immutable.HashMap

      def `should do nothing if map contains specified element` {
        HashMap("one" -> 1, "two" -> 2) should contain ("two" -> 2)
        HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2))
        HashMap(1 -> "one", 2 -> "two") should contain (2 -> "two")
      }

      def `should do nothing if map does not contain the specified element and used with not` {
        HashMap("one" -> 1, "two" -> 2) should not { contain ("three" -> 3) }
        HashMap("one" -> 1, "two" -> 2) should not contain ("three" -> 3)
        HashMap("one" -> 1, "two" -> 2) should (not contain ("three" -> 3))
      }

      def `should do nothing when map contains specified element and used in a logical-and expression` {
        HashMap("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> 1)) }
        HashMap("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> 1))
      }

      def `should do nothing when map contains specified element and used in a logical-or expression` {
        HashMap("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        HashMap("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        HashMap("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      def `should do nothing when map does not contain the specified element and used in a logical-and expression with not` {
        HashMap("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        HashMap("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        HashMap("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      def `should do nothing when map does not contain the specified element and used in a logical-or expression with not` {
        HashMap("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        HashMap("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      def `should throw TestFailedException if map does not contain the specified element` {
        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should contain ("three" -> 3)
        }
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.endsWith(") did not contain element (three,3)"))
      }

      def `should throw TestFailedException if contains the specified element when used with not` {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2))
        }
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.endsWith(") contained element (two,2)"))


        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should not (contain ("two" -> 2))
        }
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.endsWith(" contained element (two,2)"))


        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should not contain ("two" -> 2)
        }
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.endsWith(" contained element (two,2)"))
      }

      def `should throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.endsWith(" did not contain element (five,5)"))

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.endsWith(" did not contain element (five,5)"))

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.endsWith(" did not contain element (five,5)"))
      }

      def `should throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.contains(") did not contain element (fifty five,55), and Map("))
        assert(caught1.getMessage.endsWith(") did not contain element (twenty two,22)"))

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        //assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.contains(") did not contain element (fifty five,55), and Map("))
        assert(caught1.getMessage.endsWith(") did not contain element (twenty two,22)"))

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        //assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.contains(") did not contain element (fifty five,55), and Map("))
        assert(caught1.getMessage.endsWith(") did not contain element (twenty two,22)"))
      }

      def `should throw an TestFailedException when map contains specified element and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        //assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.contains(") did not contain element (three,3), but Map("))
        assert(caught1.getMessage.endsWith(") contained element (two,2)"))

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        //assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.contains(") did not contain element (three,3), but Map("))
        assert(caught1.getMessage.endsWith(") contained element (two,2)"))

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        //assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.contains(") did not contain element (three,3), but Map("))
        assert(caught1.getMessage.endsWith(") contained element (two,2)"))
      }

      def `should throw an TestFailedException when map contains specified element and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        //assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.contains(" contained element (two,2), and Map("))
        assert(caught1.getMessage.endsWith(") contained element (two,2)"))

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        //assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.contains(" contained element (two,2), and Map("))
        assert(caught1.getMessage.endsWith(") contained element (two,2)"))

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        //assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")
        assert(caught1.getMessage.startsWith("Map("))
        assert(caught1.getMessage.contains(" contained element (two,2), and Map("))
        assert(caught1.getMessage.endsWith(") contained element (two,2)"))
      }

      def `should work on parallel form` {
        HashMap("one" -> 1, "two" -> 2).par should contain ("two" -> 2)
      }
    }

    object `on scala.collection.mutable.HashMap ` {

      import scala.collection.mutable

      def `should do nothing if map contains specified element` {
        mutable.HashMap("one" -> 1, "two" -> 2) should contain ("two" -> 2)
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2))
        mutable.HashMap(1 -> "one", 2 -> "two") should contain (2 -> "two")
      }

      def `should do nothing if map does not contain the specified element and used with not` {
        mutable.HashMap("one" -> 1, "two" -> 2) should not { contain ("three" -> 3) }
        mutable.HashMap("one" -> 1, "two" -> 2) should not contain ("three" -> 3)
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain ("three" -> 3))
      }

      def `should do nothing when map contains specified element and used in a logical-and expression` {
        mutable.HashMap("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> 1)) }
        mutable.HashMap("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> 1))
      }

      def `should do nothing when map contains specified element and used in a logical-or expression` {
        mutable.HashMap("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        mutable.HashMap("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      def `should do nothing when map does not contain the specified element and used in a logical-and expression with not` {
        mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      def `should do nothing when map does not contain the specified element and used in a logical-or expression with not` {
        mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      def `should throw TestFailedException if map does not contain the specified element` {
        val map = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map should contain ("three" -> 3)
        }
        assert(caught1.getMessage === decorateToStringValue(map) + " did not contain element (three,3)")
      }

      def `should throw TestFailedException if contains the specified element when used with not` {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {(
          map1 should (not contain ("two" -> 2)))
        }
        assert(caught1.getMessage === decorateToStringValue(map1) + " contained element (two,2)")
        
        val map2 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {(
          map2 should not (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(map2) + " contained element (two,2)")
        
        val map3 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {(
          map3 should not contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(map3) + " contained element (two,2)")
      }

      def `should throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression` {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === decorateToStringValue(map1) + " did not contain element (five,5)")
        
        val map2 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {(
          map2 should ((contain ("five" -> 5)) and (contain ("two" -> 2))))
        }
        assert(caught2.getMessage === decorateToStringValue(map2) + " did not contain element (five,5)")
        
        val map3 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {(
          map3 should (contain ("five" -> 5) and contain ("two" -> 2)))
        }
        assert(caught3.getMessage === decorateToStringValue(map3) + " did not contain element (five,5)")
      }

      def `should throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression` {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {(
          map1 should { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        )}
        assert(caught1.getMessage === decorateToStringValue(map1) + " did not contain element (fifty five,55), and " + decorateToStringValue(map1) + " did not contain element (twenty two,22)")

        val map2 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === decorateToStringValue(map2) + " did not contain element (fifty five,55), and " + decorateToStringValue(map2) + " did not contain element (twenty two,22)")

        val map3 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage === decorateToStringValue(map3) + " did not contain element (fifty five,55), and " + decorateToStringValue(map3) + " did not contain element (twenty two,22)")
      }

      def `should throw an TestFailedException when map contains specified element and used in a logical-and expression with not` {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === decorateToStringValue(map1) + " did not contain element (three,3), but " + decorateToStringValue(map1) + " contained element (two,2)")

        val map2 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(map2) + " did not contain element (three,3), but " + decorateToStringValue(map2) + " contained element (two,2)")

        val map3 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(map3) + " did not contain element (three,3), but " + decorateToStringValue(map3) + " contained element (two,2)")
      }

      def `should throw an TestFailedException when map contains specified element and used in a logical-or expression with not` {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === decorateToStringValue(map1) + " contained element (two,2), and " + decorateToStringValue(map1) + " contained element (two,2)")

        val map2 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(map2) + " contained element (two,2), and " + decorateToStringValue(map2) + " contained element (two,2)")

        val map3 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(map3) + " contained element (two,2), and " + decorateToStringValue(map3) + " contained element (two,2)")
      }

      def `should work on parallel form` {
        mutable.HashMap("one" -> 1, "two" -> 2).par should contain ("two" -> 2)
      }
    }

    object `on java.util.Set` {

      val javaSet: java.util.Set[Int] = new java.util.HashSet
      javaSet.add(1)
      javaSet.add(2)

      def `should do nothing if list contains the specified element` {
        javaSet should contain (2)
        javaSet should (contain (2))
      }

      def `should do nothing if list does not contain the element and used with should not` {
        javaSet should (not contain (3))
        javaSet should not { contain (3) }
        javaSet should not contain (3)
      }

      def `should do nothing when list contains the specified element and used in a logical-and expression` {
        javaSet should { contain (2) and (contain (1)) }
        javaSet should ((contain (2)) and (contain (1)))
        javaSet should (contain (2) and contain (1))
       }

      def `should do nothing when list contains the specified element and used in a logical-or expression` {
        javaSet should { contain (77) or (contain (2)) }
        javaSet should ((contain (77)) or (contain (2)))
        javaSet should (contain (77) or contain (2))
      }

      def `should do nothing when list doesn't contain the specified element and used in a logical-and expression with not` {
        javaSet should { not { contain (5) } and not { contain (3) }}
        javaSet should ((not contain (5)) and (not contain (3)))
        javaSet should (not contain (5) and not contain (3))
      }

      def `should do nothing when list doesn't contain the specified element and used in a logical-or expression with not` {
        javaSet should { not { contain (1) } or not { contain (3) }}
        javaSet should ((not contain (1)) or (not contain (3)))
        javaSet should (not contain (3) or not contain (2))
      }

      def `should throw TestFailedException if list does not contain the specified element` {
        val caught = intercept[TestFailedException] {
          javaSet should contain (3)
        }
        assert(caught.getMessage endsWith "] did not contain element 3")
      }

      def `should throw TestFailedException if list contains the specified element, when used with not` {

        val caught1 = intercept[TestFailedException] {
          javaSet should not contain (2)
        }
        assert(caught1.getMessage endsWith "] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaSet should not (contain (2))
        }
        assert(caught2.getMessage endsWith "] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaSet should (not contain (2))
        }
        assert(caught3.getMessage endsWith "] contained element 2")
      }

      def `should throw a TestFailedException when list doesn't contain the specified element and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          javaSet should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage endsWith "] did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          javaSet should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage endsWith "] did not contain element 5")
      }

      def `should throw a TestFailedException when list doesn't contain the specified element and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          javaSet should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage endsWith "] did not contain element 22")
        assert(caught1.getMessage.indexOf("] did not contain element 55, and [") != -1)

        val caught2 = intercept[TestFailedException] {
          javaSet should (contain (55) or contain (22))
        }
        assert(caught1.getMessage endsWith "] did not contain element 22")
        assert(caught1.getMessage.indexOf("] did not contain element 55, and [") != -1)
        // assert(caught2.getMessage === "[2, 1] did not contain element 55, and [2, 1] did not contain element 22")
      }

      def `should throw a TestFailedException when list contains the specified element and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          javaSet should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] did not contain element 3, but [") != -1)
        // assert(caught1.getMessage === "[2, 1] did not contain element 3, but [2, 1] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaSet should ((not contain (3)) and (not contain (2)))
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] did not contain element 3, but [") != -1)
        // assert(caught2.getMessage === "[2, 1] did not contain element 3, but [2, 1] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaSet should (not contain (3) and not contain (2))
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] did not contain element 3, but [") != -1)
        // assert(caught3.getMessage === "[2, 1] did not contain element 3, but [2, 1] contained element 2")
      }

      def `should throw a TestFailedException when list contains the specified element and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          javaSet should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] contained element 2, and [") != -1)
        // assert(caught1.getMessage === "[2, 1] contained element 2, and [2, 1] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaSet should ((not contain (2)) or (not contain (2)))
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] contained element 2, and [") != -1)
        // assert(caught2.getMessage === "[2, 1] contained element 2, and [2, 1] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaSet should (not contain (2) or not contain (2))
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] contained element 2, and [") != -1)
        // assert(caught3.getMessage === "[2, 1] contained element 2, and [2, 1] contained element 2")
      }
    }

/*
    I'm just not going to support this for now. Let them do whatever, and when someone
    comes back with a good suggestion, then I can consider adding it.

    object `on java.util.Map ` {

      val javaMap: java.util.Map[String, Int] = new java.util.HashMap
      javaMap.put("one",1)
      javaMap.put("two", 2)

      import java.util.Map.Entry

      def `should do nothing if list contains the specified element` {
        javaMap.entrySet should contain ("one" -> 1)
        javaMap.entrySet should (contain ("two" -> 2))
      }

      def `should do nothing if list does not contain the element and used with should not` {
        javaMap should (not contain (3))
        javaMap should not { contain (3) }
        javaMap should not contain (3)
      }

      def `should do nothing when list contains the specified element and used in a logical-and expression` {
        javaMap should { contain (2) and (contain (1)) }
        javaMap should ((contain (2)) and (contain (1)))
        javaMap should (contain (2) and contain (1))
       }

      def `should do nothing when list contains the specified element and used in a logical-or expression` {
        javaMap should { contain (77) or (contain (2)) }
        javaMap should ((contain (77)) or (contain (2)))
        javaMap should (contain (77) or contain (2))
      }

      def `should do nothing when list doesn't contain the specified element and used in a logical-and expression with not` {
        javaMap should { not { contain (5) } and not { contain (3) }}
        javaMap should ((not contain (5)) and (not contain (3)))
        javaMap should (not contain (5) and not contain (3))
      }

      def `should do nothing when list doesn't contain the specified element and used in a logical-or expression with not` {
        javaMap should { not { contain (1) } or not { contain (3) }}
        javaMap should ((not contain (1)) or (not contain (3)))
        javaMap should (not contain (3) or not contain (2))
      }

      def `should throw TestFailedException if list does not contain the specified element` {
        val caught = intercept[TestFailedException] {
          javaMap should contain (3)
        }
        assert(caught.getMessage === "{one=1, two=2} did not contain element 3")
      }

      def `should throw TestFailedException if list contains the specified element, when used with not` {

        val caught1 = intercept[TestFailedException] {
          javaMap should not contain (2)
        }
        assert(caught1.getMessage === "{one=1, two=2} contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaMap should not (contain (2))
        }
        assert(caught2.getMessage === "{one=1, two=2} contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaMap should (not contain (2))
        }
        assert(caught3.getMessage === "{one=1, two=2} contained element 2")
      }

      def `should throw a TestFailedException when list doesn't contain the specified element and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          javaMap should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "{one=1, two=2} did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          javaMap should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "{one=1, two=2} did not contain element 5")
      }

      def `should throw a TestFailedException when list doesn't contain the specified element and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          javaMap should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "{one=1, two=2} did not contain element 55, and {one=1, two=2} did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          javaMap should (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "{one=1, two=2} did not contain element 55, and {one=1, two=2} did not contain element 22")
      }

      def `should throw a TestFailedException when list contains the specified element and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          javaMap should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "{one=1, two=2} did not contain element 3, but {one=1, two=2} contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "{one=1, two=2} did not contain element 3, but {one=1, two=2} contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaMap should (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "{one=1, two=2} did not contain element 3, but {one=1, two=2} contained element 2")
      }

      def `should throw a TestFailedException when list contains the specified element and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          javaMap should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "{one=1, two=2} contained element 2, and {one=1, two=2} contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "{one=1, two=2} contained element 2, and {one=1, two=2} contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaMap should (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "{one=1, two=2} contained element 2, and {one=1, two=2} contained element 2")
      }
    }
*/
  }
*/
}
