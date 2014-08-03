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
import org.scalactic.CheckedEquality

class AllShouldContainElementTypeCheckSpec extends Spec with CheckedEquality {

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
      def `on scala.collection.mutable.Set` {

        import scala.collection.mutable

        """atMost (1, mutable.Set(mutable.Set(1, 2))) should contain ("2")""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should (contain ("2"))""" shouldNot typeCheck

        """atMost (1, mutable.Set(mutable.Set(1, 2))) should not { contain ("3") }""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should not contain ("3")""" shouldNot typeCheck

        """atMost (1, mutable.Set(mutable.Set(1, 2))) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """atMost (1, mutable.Set(mutable.Set(1, 2))) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """atMost (1, mutable.Set(mutable.Set(1, 2))) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """atMost (1, mutable.Set(mutable.Set(1, 2))) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """atMost (1, mutable.Set(mutable.Set(1, 2))) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on scala.collection.Set` {

        val set: scala.collection.Set[Int] = Set(1, 2)

        """between (1, 3, Vector(set)) should contain ("2")""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should (contain ("2"))""" shouldNot typeCheck

        """between (1, 3, Vector(set)) should not { contain ("3") }""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should not contain ("3")""" shouldNot typeCheck

        """between (1, 3, Vector(set)) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """between (1, 3, Vector(set)) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """between (1, 3, Vector(set)) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """between (1, 3, Vector(set)) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """between (1, 3, Vector(set)) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.HashSet` {

        import scala.collection.immutable.HashSet

        """exactly (1, List(HashSet(1, 2))) should contain ("2")""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should (contain ("2"))""" shouldNot typeCheck

        """exactly (1, List(HashSet(1, 2))) should not { contain ("3") }""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should not contain ("3")""" shouldNot typeCheck

        """exactly (1, List(HashSet(1, 2))) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """exactly (1, List(HashSet(1, 2))) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """exactly (1, List(HashSet(1, 2))) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """exactly (1, List(HashSet(1, 2))) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """exactly (1, List(HashSet(1, 2))) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.HashSet` {

        import scala.collection.mutable

        """every (mutable.HashSet(mutable.HashSet(1, 2))) should contain ("2")""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should (contain ("2"))""" shouldNot typeCheck

        """every (mutable.HashSet(mutable.HashSet(1, 2))) should not { contain ("3") }""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should not contain ("3")""" shouldNot typeCheck

        """every (mutable.HashSet(mutable.HashSet(1, 2))) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """every (mutable.HashSet(mutable.HashSet(1, 2))) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """every (mutable.HashSet(mutable.HashSet(1, 2))) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """every (mutable.HashSet(mutable.HashSet(1, 2))) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """every (mutable.HashSet(mutable.HashSet(1, 2))) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on List` {

        """all (List(List(1, 2))) should contain ("2")""" shouldNot typeCheck
        """all (List(List(1, 2))) should (contain ("2"))""" shouldNot typeCheck

        """all (List(List(1, 2))) should not { contain ("3") }""" shouldNot typeCheck
        """all (List(List(1, 2))) should not contain ("3")""" shouldNot typeCheck

        """all (List(List(1, 2))) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """all (List(List(1, 2))) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """all (List(List(1, 2))) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """all (List(List(1, 2))) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """all (List(List(1, 2))) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """all (List(List(1, 2))) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """all (List(List(1, 2))) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """all (List(List(1, 2))) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on Vector` {

        """every (Vector(Vector(1, 2))) should contain ("2")""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should (contain ("2"))""" shouldNot typeCheck

        """every (Vector(Vector(1, 2))) should not { contain ("3") }""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should not contain ("3")""" shouldNot typeCheck

        """every (Vector(Vector(1, 2))) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """every (Vector(Vector(1, 2))) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """every (Vector(Vector(1, 2))) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """every (Vector(Vector(1, 2))) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """every (Vector(Vector(1, 2))) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on java.util.List` {

        val javaList: java.util.List[Int] = new java.util.ArrayList
        javaList.add(1)
        javaList.add(2)
        val javaListOfJavaList: java.util.List[java.util.List[Int]] = new java.util.ArrayList
        javaListOfJavaList.add(javaList)
      
        """all (javaListOfJavaList) should contain ("2")""" shouldNot typeCheck
        """all (javaListOfJavaList) should (contain ("2"))""" shouldNot typeCheck

        """all (javaListOfJavaList) should not { contain ("3") }""" shouldNot typeCheck
        """all (javaListOfJavaList) should not contain ("3")""" shouldNot typeCheck

        """all (javaListOfJavaList) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """all (javaListOfJavaList) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """all (javaListOfJavaList) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """all (javaListOfJavaList) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """all (javaListOfJavaList) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """all (javaListOfJavaList) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """all (javaListOfJavaList) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """all (javaListOfJavaList) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """all (javaListOfJavaList) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """all (javaListOfJavaList) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """all (javaListOfJavaList) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """all (javaListOfJavaList) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """all (javaListOfJavaList) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """all (javaListOfJavaList) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """all (javaListOfJavaList) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """all (javaListOfJavaList) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """all (javaListOfJavaList) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """all (javaListOfJavaList) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """all (javaListOfJavaList) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """all (javaListOfJavaList) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """all (javaListOfJavaList) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """all (javaListOfJavaList) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """all (javaListOfJavaList) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """all (javaListOfJavaList) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.Map ` {

        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should contain (2 -> 2)""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (contain (2 -> 2))""" shouldNot typeCheck
        """atLeast (1, Array(Map(1 -> "one", 2 -> "two"))) should contain ("two" -> "two")""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should contain ("two" -> "two")""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (contain ("two" -> "two"))""" shouldNot typeCheck
        """atLeast (1, Array(Map(1 -> "one", 2 -> "two"))) should contain (2 -> 2)""" shouldNot typeCheck

        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should not { contain (3 -> 3) }""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should not contain (3 -> 3)""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (not contain (3 -> 3))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should not { contain ("three" -> "three") }""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should not contain ("three" -> "three")""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (not contain ("three" -> "three"))""" shouldNot typeCheck

        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should { contain ("two" -> 2) and (contain (1 -> 1)) }""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should ((contain ("two" -> 2)) and (contain (1 -> 1)))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (contain ("two" -> 2) and contain (1 -> 1))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should { contain ("two" -> 2) and (contain ("one" -> "one")) }""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should ((contain ("two" -> 2)) and (contain ("one" -> "one")))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (contain ("two" -> 2) and contain ("one" -> "one"))""" shouldNot typeCheck

        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should { contain ("cat" -> 77) or (contain (1 -> 1)) }""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should ((contain ("cat" -> 77)) or (contain (1 -> 1)))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (contain ("cat" -> 77) or contain (1 -> 1))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should { contain ("cat" -> 77) or (contain ("one" -> "one")) }""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should ((contain ("cat" -> 77)) or (contain ("one" -> "one")))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (contain ("cat" -> 77) or contain ("one" -> "one"))""" shouldNot typeCheck

        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should { not { contain ("five" -> 5) } and not { contain (3 -> 3) }}""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should ((not contain ("five" -> 5)) and (not contain (3 -> 3)))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (not contain ("five" -> 5) and not contain (3 -> 3))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should { not { contain ("five" -> 5) } and not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should ((not contain ("five" -> 5)) and (not contain ("three" -> "three")))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (not contain ("five" -> 5) and not contain ("three" -> "three"))""" shouldNot typeCheck

        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should { not { contain ("two" -> 2) } or not { contain (3 -> 3) }}""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should ((not contain ("two" -> 2)) or (not contain (3 -> 3)))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (not contain ("two" -> 2) or not contain (3 -> 3))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should { not { contain ("two" -> 2) } or not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should ((not contain ("two" -> 2)) or (not contain ("three" -> "three")))""" shouldNot typeCheck
        """atLeast (1, Array(Map("one" -> 1, "two" -> 2))) should (not contain ("two" -> 2) or not contain ("three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.Map ` {

        import scala.collection.mutable

        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should contain (2 -> 2)""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (contain (2 -> 2))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map(1 -> "one", 2 -> "two"))) should contain ("two" -> "two")""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should contain ("two" -> "two")""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (contain ("two" -> "two"))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map(1 -> "one", 2 -> "two"))) should contain (2 -> 2)""" shouldNot typeCheck

        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should not { contain (3 -> 3) }""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should not contain (3 -> 3)""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain (3 -> 3))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should not { contain ("three" -> "three") }""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should not contain ("three" -> "three")""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain ("three" -> "three"))""" shouldNot typeCheck

        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should { contain ("two" -> 2) and (contain (1 -> 1)) }""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should ((contain ("two" -> 2)) and (contain (1 -> 1)))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (contain ("two" -> 2) and contain (1 -> 1))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should { contain ("two" -> 2) and (contain ("one" -> "one")) }""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should ((contain ("two" -> 2)) and (contain ("one" -> "one")))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (contain ("two" -> 2) and contain ("one" -> "one"))""" shouldNot typeCheck

        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should { contain ("cat" -> 77) or (contain (1 -> 1)) }""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should ((contain ("cat" -> 77)) or (contain (1 -> 1)))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (contain ("cat" -> 77) or contain (1 -> 1))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should { contain ("cat" -> 77) or (contain ("one" -> "one")) }""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should ((contain ("cat" -> 77)) or (contain ("one" -> "one")))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (contain ("cat" -> 77) or contain ("one" -> "one"))""" shouldNot typeCheck

        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should { not { contain ("five" -> 5) } and not { contain (3 -> 3) }}""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should ((not contain ("five" -> 5)) and (not contain (3 -> 3)))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain ("five" -> 5) and not contain (3 -> 3))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should { not { contain ("five" -> 5) } and not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should ((not contain ("five" -> 5)) and (not contain ("three" -> "three")))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain ("five" -> 5) and not contain ("three" -> "three"))""" shouldNot typeCheck

        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should { not { contain ("two" -> 2) } or not { contain (3 -> 3) }}""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should ((not contain ("two" -> 2)) or (not contain (3 -> 3)))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain ("two" -> 2) or not contain (3 -> 3))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should { not { contain ("two" -> 2) } or not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should ((not contain ("two" -> 2)) or (not contain ("three" -> "three")))""" shouldNot typeCheck
        """atMost(1, List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain ("two" -> 2) or not contain ("three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.Map ` {

        val map: scala.collection.Map[String, Int] = Map("one" -> 1, "two" -> 2)

        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should contain (2 -> 2)""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (contain (2 -> 2))""" shouldNot typeCheck
        """between (1, 2, List(map(1 -> "one", 2 -> "two"))) should contain ("two" -> "two")""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should contain ("two" -> "two")""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (contain ("two" -> "two"))""" shouldNot typeCheck
        """between (1, 2, List(map(1 -> "one", 2 -> "two"))) should contain (2 -> 2)""" shouldNot typeCheck

        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should not { contain (3 -> 3) }""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should not contain (3 -> 3)""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (not contain (3 -> 3))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should not { contain ("three" -> "three") }""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should not contain ("three" -> "three")""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (not contain ("three" -> "three"))""" shouldNot typeCheck

        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should { contain ("two" -> 2) and (contain (1 -> 1)) }""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should ((contain ("two" -> 2)) and (contain (1 -> 1)))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (contain ("two" -> 2) and contain (1 -> 1))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should { contain ("two" -> 2) and (contain ("one" -> "one")) }""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should ((contain ("two" -> 2)) and (contain ("one" -> "one")))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (contain ("two" -> 2) and contain ("one" -> "one"))""" shouldNot typeCheck

        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should { contain ("cat" -> 77) or (contain (1 -> 1)) }""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should ((contain ("cat" -> 77)) or (contain (1 -> 1)))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (contain ("cat" -> 77) or contain (1 -> 1))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should { contain ("cat" -> 77) or (contain ("one" -> "one")) }""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should ((contain ("cat" -> 77)) or (contain ("one" -> "one")))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (contain ("cat" -> 77) or contain ("one" -> "one"))""" shouldNot typeCheck

        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should { not { contain ("five" -> 5) } and not { contain (3 -> 3) }}""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should ((not contain ("five" -> 5)) and (not contain (3 -> 3)))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (not contain ("five" -> 5) and not contain (3 -> 3))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should { not { contain ("five" -> 5) } and not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should ((not contain ("five" -> 5)) and (not contain ("three" -> "three")))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (not contain ("five" -> 5) and not contain ("three" -> "three"))""" shouldNot typeCheck

        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should { not { contain ("two" -> 2) } or not { contain (3 -> 3) }}""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should ((not contain ("two" -> 2)) or (not contain (3 -> 3)))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (not contain ("two" -> 2) or not contain (3 -> 3))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should { not { contain ("two" -> 2) } or not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should ((not contain ("two" -> 2)) or (not contain ("three" -> "three")))""" shouldNot typeCheck
        """between (1, 2, List(map("one" -> 1, "two" -> 2))) should (not contain ("two" -> 2) or not contain ("three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.HashMap ` {

        import scala.collection.immutable.HashMap

        """no (Set(HashMap("one" -> 1, "two" -> 2))) should contain (2 -> 2)""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (contain (2 -> 2))""" shouldNot typeCheck
        """no (Set(HashMap(1 -> "one", 2 -> "two"))) should contain ("two" -> "two")""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should contain ("two" -> "two")""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (contain ("two" -> "two"))""" shouldNot typeCheck
        """no (Set(HashMap(1 -> "one", 2 -> "two"))) should contain (2 -> 2)""" shouldNot typeCheck

        """no (Set(HashMap("one" -> 1, "two" -> 2))) should not { contain (3 -> 3) }""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should not contain (3 -> 3)""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (not contain (3 -> 3))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should not { contain ("three" -> "three") }""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should not contain ("three" -> "three")""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (not contain ("three" -> "three"))""" shouldNot typeCheck

        """no (Set(HashMap("one" -> 1, "two" -> 2))) should { contain ("two" -> 2) and (contain (1 -> 1)) }""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should ((contain ("two" -> 2)) and (contain (1 -> 1)))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (contain ("two" -> 2) and contain (1 -> 1))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should { contain ("two" -> 2) and (contain ("one" -> "one")) }""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should ((contain ("two" -> 2)) and (contain ("one" -> "one")))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (contain ("two" -> 2) and contain ("one" -> "one"))""" shouldNot typeCheck

        """no (Set(HashMap("one" -> 1, "two" -> 2))) should { contain ("cat" -> 77) or (contain (1 -> 1)) }""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should ((contain ("cat" -> 77)) or (contain (1 -> 1)))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (contain ("cat" -> 77) or contain (1 -> 1))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should { contain ("cat" -> 77) or (contain ("one" -> "one")) }""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should ((contain ("cat" -> 77)) or (contain ("one" -> "one")))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (contain ("cat" -> 77) or contain ("one" -> "one"))""" shouldNot typeCheck

        """no (Set(HashMap("one" -> 1, "two" -> 2))) should { not { contain ("five" -> 5) } and not { contain (3 -> 3) }}""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should ((not contain ("five" -> 5)) and (not contain (3 -> 3)))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (not contain ("five" -> 5) and not contain (3 -> 3))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should { not { contain ("five" -> 5) } and not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should ((not contain ("five" -> 5)) and (not contain ("three" -> "three")))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (not contain ("five" -> 5) and not contain ("three" -> "three"))""" shouldNot typeCheck

        """no (Set(HashMap("one" -> 1, "two" -> 2))) should { not { contain ("two" -> 2) } or not { contain (3 -> 3) }}""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should ((not contain ("two" -> 2)) or (not contain (3 -> 3)))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (not contain ("two" -> 2) or not contain (3 -> 3))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should { not { contain ("two" -> 2) } or not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should ((not contain ("two" -> 2)) or (not contain ("three" -> "three")))""" shouldNot typeCheck
        """no (Set(HashMap("one" -> 1, "two" -> 2))) should (not contain ("two" -> 2) or not contain ("three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.HashMap ` {

        import scala.collection.mutable

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should contain (2 -> 2)""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain (2 -> 2))""" shouldNot typeCheck
        """all (List(mutable.HashMap(1 -> "one", 2 -> "two"))) should contain ("two" -> "two")""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should contain ("two" -> "two")""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain ("two" -> "two"))""" shouldNot typeCheck
        """all (List(mutable.HashMap(1 -> "one", 2 -> "two"))) should contain (2 -> 2)""" shouldNot typeCheck

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should not { contain (3 -> 3) }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should not contain (3 -> 3)""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain (3 -> 3))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should not { contain ("three" -> "three") }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should not contain ("three" -> "three")""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain ("three" -> "three"))""" shouldNot typeCheck

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { contain ("two" -> 2) and (contain (1 -> 1)) }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((contain ("two" -> 2)) and (contain (1 -> 1)))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain ("two" -> 2) and contain (1 -> 1))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { contain ("two" -> 2) and (contain ("one" -> "one")) }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((contain ("two" -> 2)) and (contain ("one" -> "one")))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain ("two" -> 2) and contain ("one" -> "one"))""" shouldNot typeCheck

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { contain ("cat" -> 77) or (contain (1 -> 1)) }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((contain ("cat" -> 77)) or (contain (1 -> 1)))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain ("cat" -> 77) or contain (1 -> 1))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { contain ("cat" -> 77) or (contain ("one" -> "one")) }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((contain ("cat" -> 77)) or (contain ("one" -> "one")))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain ("cat" -> 77) or contain ("one" -> "one"))""" shouldNot typeCheck

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { not { contain ("five" -> 5) } and not { contain (3 -> 3) }}""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((not contain ("five" -> 5)) and (not contain (3 -> 3)))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain ("five" -> 5) and not contain (3 -> 3))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { not { contain ("five" -> 5) } and not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((not contain ("five" -> 5)) and (not contain ("three" -> "three")))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain ("five" -> 5) and not contain ("three" -> "three"))""" shouldNot typeCheck

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { not { contain ("two" -> 2) } or not { contain (3 -> 3) }}""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((not contain ("two" -> 2)) or (not contain (3 -> 3)))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain ("two" -> 2) or not contain (3 -> 3))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { not { contain ("two" -> 2) } or not { contain ("three" -> "three") }}""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((not contain ("two" -> 2)) or (not contain ("three" -> "three")))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain ("two" -> 2) or not contain ("three" -> "three"))""" shouldNot typeCheck
      }
      def `on java.util.Set` {

        val javaSet: java.util.Set[Int] = new java.util.HashSet
        javaSet.add(1)
        javaSet.add(2)
        val javaSetOfJavaSet: java.util.Set[java.util.Set[Int]] = new java.util.HashSet
        javaSetOfJavaSet.add(javaSet)

        """every (javaSetOfJavaSet) should contain ("2")""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should (contain ("2"))""" shouldNot typeCheck

        """every (javaSetOfJavaSet) should not { contain ("3") }""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should not contain ("3")""" shouldNot typeCheck

        """every (javaSetOfJavaSet) should { contain ("2") and (contain (1)) }""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should ((contain ("2")) and (contain (1)))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should (contain ("2") and contain (1))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should { contain (2) and (contain ("1")) }""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should ((contain (2)) and (contain ("1")))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should (contain (2) and contain ("1"))""" shouldNot typeCheck

        """every (javaSetOfJavaSet) should { contain ("77") or (contain (2)) }""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should ((contain ("77")) or (contain (2)))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should (contain ("77") or contain (2))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should { contain (77) or (contain ("2")) }""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should ((contain (77)) or (contain ("2")))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should (contain (77) or contain ("2"))""" shouldNot typeCheck

        """every (javaSetOfJavaSet) should { not { contain ("5") } and not { contain (3) }}""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should ((not contain ("5")) and (not contain (3)))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should { not { contain (5) } and not { contain ("3") }}""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should ((not contain (5)) and (not contain ("3")))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should { not { contain (1) } or not { contain ("3") }}""" shouldNot typeCheck

        """every (javaSetOfJavaSet) should ((not contain ("1")) or (not contain (3)))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should (not contain ("3") or not contain (2))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should (not contain ("5") and not contain (3))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should ((not contain (1)) or (not contain ("3")))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should (not contain (3) or not contain ("2"))""" shouldNot typeCheck
        """every (javaSetOfJavaSet) should (not contain (5) and not contain ("3"))""" shouldNot typeCheck
      }
      def `on java.util.Map` {

        val javaMap: java.util.Map[String, Int] = new java.util.HashMap
        javaMap.put("one",1)
        javaMap.put("two", 2)

        """all (Vector(javaMap)) should contain (Entry(2, 2))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (contain (Entry(2, 2)))""" shouldNot typeCheck
        """all (Vector(javaMap)) should contain (Entry("two", "two"))""" shouldNot typeCheck
        """all (Vector(javaMap)) should contain (Entry("two", "two"))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (contain (Entry("two", "two")))""" shouldNot typeCheck
        """all (Vector(javaMap)) should contain (Entry(2, 2))""" shouldNot typeCheck

        """all (Vector(javaMap)) should not { contain (Entry(3, 3)) }""" shouldNot typeCheck
        """all (Vector(javaMap)) should not contain (Entry(3, 3))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (not contain (Entry(3, 3)))""" shouldNot typeCheck
        """all (Vector(javaMap)) should not { contain (Entry("three", "three")) }""" shouldNot typeCheck
        """all (Vector(javaMap)) should not contain (Entry("three", "three"))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (not contain (Entry("three", "three")))""" shouldNot typeCheck

        """all (Vector(javaMap)) should { contain (Entry(Entry("two", 2))) and (contain (Entry(1, 1))) }""" shouldNot typeCheck
        """all (Vector(javaMap)) should ((contain (Entry(Entry("two", 2)))) and (contain (Entry(1, 1))))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (contain (Entry(Entry("two", 2))) and contain (Entry(1, 1)))""" shouldNot typeCheck
        """all (Vector(javaMap)) should { contain (Entry(Entry("two", 2))) and (contain (Entry("one", "one"))) }""" shouldNot typeCheck
        """all (Vector(javaMap)) should ((contain (Entry(Entry("two", 2)))) and (contain (Entry("one", "one"))))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (contain (Entry(Entry("two", 2))) and contain (Entry("one", "one")))""" shouldNot typeCheck

        """all (Vector(javaMap)) should { contain (Entry("cat", 77)) or (contain (Entry(1, 1))) }""" shouldNot typeCheck
        """all (Vector(javaMap)) should ((contain (Entry("cat", 77))) or (contain (Entry(1, 1))))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (contain (Entry("cat", 77)) or contain (Entry(1, 1)))""" shouldNot typeCheck
        """all (Vector(javaMap)) should { contain (Entry("cat", 77)) or (contain (Entry("one", "one"))) }""" shouldNot typeCheck
        """all (Vector(javaMap)) should ((contain (Entry("cat", 77))) or (contain (Entry("one", "one"))))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (contain (Entry("cat", 77)) or contain (Entry("one", "one")))""" shouldNot typeCheck

        """all (Vector(javaMap)) should { not { contain (Entry("five", 5)) } and not { contain (Entry(3, 3)) }}""" shouldNot typeCheck
        """all (Vector(javaMap)) should ((not contain (Entry("five", 5))) and (not contain (Entry(3, 3))))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (not contain (Entry("five", 5)) and not contain (Entry(3, 3)))""" shouldNot typeCheck
        """all (Vector(javaMap)) should { not { contain (Entry("five", 5)) } and not { contain (Entry("three", "three")) }}""" shouldNot typeCheck
        """all (Vector(javaMap)) should ((not contain (Entry("five", 5))) and (not contain (Entry("three", "three"))))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (not contain (Entry("five", 5)) and not contain (Entry("three", "three")))""" shouldNot typeCheck

        """all (Vector(javaMap)) should { not { contain (Entry(Entry("two", 2))) } or not { contain (Entry(3, 3)) }}""" shouldNot typeCheck
        """all (Vector(javaMap)) should ((not contain (Entry(Entry("two", 2)))) or (not contain (Entry(3, 3))))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (not contain (Entry(Entry("two", 2))) or not contain (Entry(3, 3)))""" shouldNot typeCheck
        """all (Vector(javaMap)) should { not { contain (Entry(Entry("two", 2))) } or not { contain (Entry("three", "three")) }}""" shouldNot typeCheck
        """all (Vector(javaMap)) should ((not contain (Entry(Entry("two", 2)))) or (not contain (Entry("three", "three"))))""" shouldNot typeCheck
        """all (Vector(javaMap)) should (not contain (Entry(Entry("two", 2))) or not contain (Entry("three", "three")))""" shouldNot typeCheck
      }
    }
  }
}
