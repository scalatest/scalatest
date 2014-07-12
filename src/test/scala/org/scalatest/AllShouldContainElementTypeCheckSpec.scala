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

class AllShouldContainElementTypeCheckSpec extends Spec with TypeCheckedTripleEquals {

  // Checking for a specific size
  object `The 'contain (<element>)' syntax` {

    object `should give a type error if the types are not compatible` {

      def `on Array` {

        """all (Array(Array(1, 2))) should contain ("2")""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should (contain ("2"))""" shouldNot typeCheck

        """all (Array(Array(1, 2))) should not { contain ("3") }""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should not contain ("3")""" shouldNot typeCheck

        """all (Array(Array(1, 2))) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """all (Array(Array(1, 2))) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """all (Array(Array(1, 2))) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """all (Array(Array(1, 2))) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """all (Array(Array(1, 2))) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.Set` {

        """atLeast (1, Set(Set(1, 2))) should contain ("2")""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should (contain ("2"))""" shouldNot typeCheck

        """atLeast (1, Set(Set(1, 2))) should not { contain ("3") }""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should not contain ("3")""" shouldNot typeCheck

        """atLeast (1, Set(Set(1, 2))) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """atLeast (1, Set(Set(1, 2))) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """atLeast (1, Set(Set(1, 2))) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """atLeast (1, Set(Set(1, 2))) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """atLeast (1, Set(Set(1, 2))) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
/*
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
      def `on scala.collection.immutable.Map ` {

        """Map("one" -> 1, "two" -> 2) should contain (2 -> 2)""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain (2 -> 2))""" shouldNot typeCheck
        """Map(1 -> "one", 2 -> "two") should contain ("two" -> "two")""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should contain ("two" -> "two")""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain ("two" -> "two"))""" shouldNot typeCheck
        """Map(1 -> "one", 2 -> "two") should contain (2 -> 2)""" shouldNot typeCheck

        """Map("one" -> 1, "two" -> 2) should not { contain (3 -> 3) }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should not contain (3 -> 3)""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain (3 -> 3))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should not { contain ("three" -> "three") }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should not contain ("three" -> "three")""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain ("three" -> "three"))""" shouldNot typeCheck

        """Map("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain (1 -> 1)) }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain (1 -> 1)))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain (1 -> 1))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> "one")) }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> "one")))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> "one"))""" shouldNot typeCheck

        """Map("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain (1 -> 1)) }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain (1 -> 1)))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain (1 -> 1))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> "one")) }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> "one")))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> "one"))""" shouldNot typeCheck

        """Map("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain (3 -> 3) }}""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain (3 -> 3)))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain (3 -> 3))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> "three")))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> "three"))""" shouldNot typeCheck

        """Map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain (3 -> 3) }}""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain (3 -> 3)))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain (3 -> 3))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> "three")))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.Map ` {

        import scala.collection.mutable

        """mutable.Map("one" -> 1, "two" -> 2) should contain (2 -> 2)""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain (2 -> 2))""" shouldNot typeCheck
        """mutable.Map(1 -> "one", 2 -> "two") should contain ("two" -> "two")""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should contain ("two" -> "two")""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain ("two" -> "two"))""" shouldNot typeCheck
        """mutable.Map(1 -> "one", 2 -> "two") should contain (2 -> 2)""" shouldNot typeCheck

        """mutable.Map("one" -> 1, "two" -> 2) should not { contain (3 -> 3) }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should not contain (3 -> 3)""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain (3 -> 3))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should not { contain ("three" -> "three") }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should not contain ("three" -> "three")""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain ("three" -> "three"))""" shouldNot typeCheck

        """mutable.Map("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain (1 -> 1)) }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain (1 -> 1)))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain (1 -> 1))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> "one")) }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> "one")))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> "one"))""" shouldNot typeCheck

        """mutable.Map("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain (1 -> 1)) }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain (1 -> 1)))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain (1 -> 1))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> "one")) }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> "one")))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> "one"))""" shouldNot typeCheck

        """mutable.Map("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain (3 -> 3) }}""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain (3 -> 3)))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain (3 -> 3))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> "three")))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> "three"))""" shouldNot typeCheck

        """mutable.Map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain (3 -> 3) }}""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain (3 -> 3)))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain (3 -> 3))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> "three")))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.Map ` {

        val map: scala.collection.Map[String, Int] = Map("one" -> 1, "two" -> 2)

        """map("one" -> 1, "two" -> 2) should contain (2 -> 2)""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain (2 -> 2))""" shouldNot typeCheck
        """map(1 -> "one", 2 -> "two") should contain ("two" -> "two")""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should contain ("two" -> "two")""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain ("two" -> "two"))""" shouldNot typeCheck
        """map(1 -> "one", 2 -> "two") should contain (2 -> 2)""" shouldNot typeCheck

        """map("one" -> 1, "two" -> 2) should not { contain (3 -> 3) }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should not contain (3 -> 3)""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain (3 -> 3))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should not { contain ("three" -> "three") }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should not contain ("three" -> "three")""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain ("three" -> "three"))""" shouldNot typeCheck

        """map("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain (1 -> 1)) }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain (1 -> 1)))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain (1 -> 1))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> "one")) }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> "one")))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> "one"))""" shouldNot typeCheck

        """map("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain (1 -> 1)) }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain (1 -> 1)))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain (1 -> 1))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> "one")) }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> "one")))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> "one"))""" shouldNot typeCheck

        """map("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain (3 -> 3) }}""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain (3 -> 3)))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain (3 -> 3))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> "three")))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> "three"))""" shouldNot typeCheck

        """map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain (3 -> 3) }}""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain (3 -> 3)))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain (3 -> 3))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> "three")))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.HashMap ` {

        import scala.collection.immutable.HashMap

        """HashMap("one" -> 1, "two" -> 2) should contain (2 -> 2)""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain (2 -> 2))""" shouldNot typeCheck
        """HashMap(1 -> "one", 2 -> "two") should contain ("two" -> "two")""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should contain ("two" -> "two")""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> "two"))""" shouldNot typeCheck
        """HashMap(1 -> "one", 2 -> "two") should contain (2 -> 2)""" shouldNot typeCheck

        """HashMap("one" -> 1, "two" -> 2) should not { contain (3 -> 3) }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should not contain (3 -> 3)""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain (3 -> 3))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should not { contain ("three" -> "three") }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should not contain ("three" -> "three")""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain ("three" -> "three"))""" shouldNot typeCheck

        """HashMap("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain (1 -> 1)) }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain (1 -> 1)))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain (1 -> 1))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> "one")) }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> "one")))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> "one"))""" shouldNot typeCheck

        """HashMap("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain (1 -> 1)) }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain (1 -> 1)))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain (1 -> 1))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> "one")) }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> "one")))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> "one"))""" shouldNot typeCheck

        """HashMap("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain (3 -> 3) }}""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain (3 -> 3)))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain (3 -> 3))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> "three")))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> "three"))""" shouldNot typeCheck

        """HashMap("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain (3 -> 3) }}""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain (3 -> 3)))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain (3 -> 3))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> "three")))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.HashMap ` {

        import scala.collection.mutable

        """mutable.HashMap("one" -> 1, "two" -> 2) should contain (2 -> 2)""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain (2 -> 2))""" shouldNot typeCheck
        """mutable.HashMap(1 -> "one", 2 -> "two") should contain ("two" -> "two")""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should contain ("two" -> "two")""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> "two"))""" shouldNot typeCheck
        """mutable.HashMap(1 -> "one", 2 -> "two") should contain (2 -> 2)""" shouldNot typeCheck

        """mutable.HashMap("one" -> 1, "two" -> 2) should not { contain (3 -> 3) }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should not contain (3 -> 3)""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain (3 -> 3))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should not { contain ("three" -> "three") }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should not contain ("three" -> "three")""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain ("three" -> "three"))""" shouldNot typeCheck

        """mutable.HashMap("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain (1 -> 1)) }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain (1 -> 1)))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain (1 -> 1))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> "one")) }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> "one")))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> "one"))""" shouldNot typeCheck

        """mutable.HashMap("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain (1 -> 1)) }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain (1 -> 1)))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain (1 -> 1))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> "one")) }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> "one")))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> "one"))""" shouldNot typeCheck

        """mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain (3 -> 3) }}""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain (3 -> 3)))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain (3 -> 3))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> "three")))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> "three"))""" shouldNot typeCheck

        """mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain (3 -> 3) }}""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain (3 -> 3)))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain (3 -> 3))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> "three")))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> "three"))""" shouldNot typeCheck
      }
      def `on java.util.Set` {

        val javaSet: java.util.Set[Int] = new java.util.HashSet
        javaSet.add(1)
        javaSet.add(2)

        """javaSet should contain ("2")""" shouldNot typeCheck
        """javaSet should (contain ("2"))""" shouldNot typeCheck

        """javaSet should not { contain ("3") }""" shouldNot typeCheck
        """javaSet should not contain ("3")""" shouldNot typeCheck

        """javaSet should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """javaSet should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """javaSet should (contain ("2") and contain (1))""" shouldNot typeCheck
        """javaSet should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """javaSet should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """javaSet should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """javaSet should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """javaSet should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """javaSet should (contain ("77") or contain (2))""" shouldNot typeCheck
        """javaSet should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """javaSet should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """javaSet should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """javaSet should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """javaSet should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """javaSet should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """javaSet should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """javaSet should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """javaSet should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """javaSet should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """javaSet should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """javaSet should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """javaSet should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """javaSet should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """javaSet should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on java.util.Map` {

        val javaMap: java.util.Map[String, Int] = new java.util.HashMap
        javaMap.put("one",1)
        javaMap.put("two", 2)

        """javaMap should contain (Entry(2, 2))""" shouldNot typeCheck
        """javaMap should (contain (Entry(2, 2)))""" shouldNot typeCheck
        """javaMap should contain (Entry("two", "two"))""" shouldNot typeCheck
        """javaMap should contain (Entry("two", "two"))""" shouldNot typeCheck
        """javaMap should (contain (Entry("two", "two")))""" shouldNot typeCheck
        """javaMap should contain (Entry(2, 2))""" shouldNot typeCheck

        """javaMap should not { contain (Entry(3, 3)) }""" shouldNot typeCheck
        """javaMap should not contain (Entry(3, 3))""" shouldNot typeCheck
        """javaMap should (not contain (Entry(3, 3)))""" shouldNot typeCheck
        """javaMap should not { contain (Entry("three", "three")) }""" shouldNot typeCheck
        """javaMap should not contain (Entry("three", "three"))""" shouldNot typeCheck
        """javaMap should (not contain (Entry("three", "three")))""" shouldNot typeCheck

        """javaMap should { contain (Entry(Entry("two", 2))) and (contain (Entry(1, 1))) }""" shouldNot typeCheck
        """javaMap should ((contain (Entry(Entry("two", 2)))) and (contain (Entry(1, 1))))""" shouldNot typeCheck
        """javaMap should (contain (Entry(Entry("two", 2))) and contain (Entry(1, 1)))""" shouldNot typeCheck
        """javaMap should { contain (Entry(Entry("two", 2))) and (contain (Entry("one", "one"))) }""" shouldNot typeCheck
        """javaMap should ((contain (Entry(Entry("two", 2)))) and (contain (Entry("one", "one"))))""" shouldNot typeCheck
        """javaMap should (contain (Entry(Entry("two", 2))) and contain (Entry("one", "one")))""" shouldNot typeCheck

        """javaMap should { contain (Entry("cat", 77)) or (contain (Entry(1, 1))) }""" shouldNot typeCheck
        """javaMap should ((contain (Entry("cat", 77))) or (contain (Entry(1, 1))))""" shouldNot typeCheck
        """javaMap should (contain (Entry("cat", 77)) or contain (Entry(1, 1)))""" shouldNot typeCheck
        """javaMap should { contain (Entry("cat", 77)) or (contain (Entry("one", "one"))) }""" shouldNot typeCheck
        """javaMap should ((contain (Entry("cat", 77))) or (contain (Entry("one", "one"))))""" shouldNot typeCheck
        """javaMap should (contain (Entry("cat", 77)) or contain (Entry("one", "one")))""" shouldNot typeCheck

        """javaMap should { not { contain (Entry("five", 5)) } and not { contain (Entry(3, 3)) }}""" shouldNot typeCheck
        """javaMap should ((not contain (Entry("five", 5))) and (not contain (Entry(3, 3))))""" shouldNot typeCheck
        """javaMap should (not contain (Entry("five", 5)) and not contain (Entry(3, 3)))""" shouldNot typeCheck
        """javaMap should { not { contain (Entry("five", 5)) } and not { contain (Entry("three", "three")) }}""" shouldNot typeCheck
        """javaMap should ((not contain (Entry("five", 5))) and (not contain (Entry("three", "three"))))""" shouldNot typeCheck
        """javaMap should (not contain (Entry("five", 5)) and not contain (Entry("three", "three")))""" shouldNot typeCheck

        """javaMap should { not { contain (Entry(Entry("two", 2))) } or not { contain (Entry(3, 3)) }}""" shouldNot typeCheck
        """javaMap should ((not contain (Entry(Entry("two", 2)))) or (not contain (Entry(3, 3))))""" shouldNot typeCheck
        """javaMap should (not contain (Entry(Entry("two", 2))) or not contain (Entry(3, 3)))""" shouldNot typeCheck
        """javaMap should { not { contain (Entry(Entry("two", 2))) } or not { contain (Entry("three", "three")) }}""" shouldNot typeCheck
        """javaMap should ((not contain (Entry(Entry("two", 2)))) or (not contain (Entry("three", "three"))))""" shouldNot typeCheck
        """javaMap should (not contain (Entry(Entry("two", 2))) or not contain (Entry("three", "three")))""" shouldNot typeCheck
      }
*/
    }
  }
}
