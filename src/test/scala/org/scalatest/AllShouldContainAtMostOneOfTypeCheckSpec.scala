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

class AllShouldContainAtMostOneOfTypeCheckSpec extends Spec with TypeCheckedTripleEquals {

  // Checking for a specific size
  object `The 'contain atMostOneOf (1, <element>)' syntax` {

    object `should give a type error if the types are not compatible` {

      def `on Array` {

        """all (List(Array(1, 2))) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """all (List(Array(1, 2))) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(Array(1, 2))) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """all (List(Array(1, 2))) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """all (List(Array(1, 2))) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """all (List(Array(1, 2))) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """all (List(Array(1, 2))) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """all (List(Array(1, 2))) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """all (List(Array(1, 2))) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """all (List(Array(1, 2))) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(Array(1, 2))) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """all (List(Array(1, 2))) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """all (List(Array(1, 2))) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """all (List(Array(1, 2))) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """all (List(Array(1, 2))) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
/*
      def `on scala.collection.immutable.Set` {

        """all (List(Set(1, 2))) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """all (List(Set(1, 2))) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(Set(1, 2))) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """all (List(Set(1, 2))) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """all (List(Set(1, 2))) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """all (List(Set(1, 2))) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """all (List(Set(1, 2))) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """all (List(Set(1, 2))) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """all (List(Set(1, 2))) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """all (List(Set(1, 2))) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(Set(1, 2))) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """all (List(Set(1, 2))) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """all (List(Set(1, 2))) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """all (List(Set(1, 2))) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """all (List(Set(1, 2))) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.Set` {

        import scala.collection.mutable

        """all (List(mutable.Set(1, 2))) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(mutable.Set(1, 2))) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """all (List(mutable.Set(1, 2))) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """all (List(mutable.Set(1, 2))) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(mutable.Set(1, 2))) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """all (List(mutable.Set(1, 2))) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """all (List(mutable.Set(1, 2))) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      def `on scala.collection.Set` {

        val set: scala.collection.Set[Int] = Set(1, 2)

        """all (List(set)) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """all (List(set)) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(set)) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """all (List(set)) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """all (List(set)) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """all (List(set)) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """all (List(set)) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """all (List(set)) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """all (List(set)) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """all (List(set)) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """all (List(set)) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """all (List(set)) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """all (List(set)) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(set)) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """all (List(set)) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """all (List(set)) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(set)) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """all (List(set)) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(set)) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(set)) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """all (List(set)) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(set)) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """all (List(set)) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(set)) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(set)) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(set)) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(set)) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """all (List(set)) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.HashSet` {

        import scala.collection.immutable.HashSet

        """all (List(HashSet(1, 2))) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(HashSet(1, 2))) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """all (List(HashSet(1, 2))) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """all (List(HashSet(1, 2))) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(HashSet(1, 2))) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """all (List(HashSet(1, 2))) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """all (List(HashSet(1, 2))) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.HashSet` {

        import scala.collection.mutable

        """all (List(mutable.HashSet(1, 2))) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(mutable.HashSet(1, 2))) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """all (List(mutable.HashSet(1, 2))) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """all (List(mutable.HashSet(1, 2))) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(mutable.HashSet(1, 2))) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """all (List(mutable.HashSet(1, 2))) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """all (List(mutable.HashSet(1, 2))) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      def `on List` {

        """all (List(List(1, 2))) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """all (List(List(1, 2))) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(List(1, 2))) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """all (List(List(1, 2))) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """all (List(List(1, 2))) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """all (List(List(1, 2))) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """all (List(List(1, 2))) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(List(1, 2))) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(List(1, 2))) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(List(1, 2))) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(List(1, 2))) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """all (List(List(1, 2))) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(List(1, 2))) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """all (List(List(1, 2))) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      def `on Vector` {

        """all (List(Vector(1, 2))) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(Vector(1, 2))) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """all (List(Vector(1, 2))) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """all (List(Vector(1, 2))) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(Vector(1, 2))) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """all (List(Vector(1, 2))) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """all (List(Vector(1, 2))) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      def `on java.util.List` {

        val javaList: java.util.List[Int] = new java.util.ArrayList
        javaList.add(1)
        javaList.add(2)
      
        """all (List(javaList)) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """all (List(javaList)) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(javaList)) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """all (List(javaList)) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """all (List(javaList)) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """all (List(javaList)) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """all (List(javaList)) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """all (List(javaList)) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """all (List(javaList)) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """all (List(javaList)) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """all (List(javaList)) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """all (List(javaList)) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """all (List(javaList)) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(javaList)) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """all (List(javaList)) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """all (List(javaList)) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(javaList)) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """all (List(javaList)) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(javaList)) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(javaList)) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """all (List(javaList)) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(javaList)) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """all (List(javaList)) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(javaList)) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(javaList)) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(javaList)) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(javaList)) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """all (List(javaList)) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.Map ` {

        """all (List(Map("one" -> 1, "two" -> 2))) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf (1 -> 1, 2 -> 2))""" shouldNot typeCheck
        """all (List(Map(1 -> "one", 2 -> "two"))) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> "1", "two" -> "two"))""" shouldNot typeCheck
        """all (List(Map(1 -> "one", 2 -> "two"))) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck

        """all (List(Map("one" -> 1, "two" -> 2))) should not { contain atMostOneOf (1 -> 1, 3 -> 3) }""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should not contain atMostOneOf (1 -> 1, 3 -> 3)""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should not { contain atMostOneOf ("1" -> "1", "three" -> "three") }""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should not contain atMostOneOf ("1" -> "1", "three" -> "three")""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """all (List(Map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """all (List(Map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """all (List(Map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """all (List(Map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """all (List(Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.Map ` {

        import scala.collection.mutable

        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf (1 -> 1, 2 -> 2))""" shouldNot typeCheck
        """all (List(mutable.Map(1 -> "one", 2 -> "two"))) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> "1", "two" -> "two"))""" shouldNot typeCheck
        """all (List(mutable.Map(1 -> "one", 2 -> "two"))) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck

        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should not { contain atMostOneOf (1 -> 1, 3 -> 3) }""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should not contain atMostOneOf (1 -> 1, 3 -> 3)""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should not { contain atMostOneOf ("1" -> "1", "three" -> "three") }""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should not contain atMostOneOf ("1" -> "1", "three" -> "three")""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1", "cat" -> 77) or (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1", "cat" -> 77)) or (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1", "cat" -> 77) or contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """all (List(mutable.Map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.Map ` {

        val map: scala.collection.Map[String, Int] = Map("one" -> 1, "two" -> 2)

        """all (List(map("one" -> 1, "two" -> 2))) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (contain atMostOneOf (1 -> 1, 2 -> 2))""" shouldNot typeCheck
        """all (List(map(1 -> "one", 2 -> "two"))) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> "1", "two" -> "two"))""" shouldNot typeCheck
        """all (List(map(1 -> "one", 2 -> "two"))) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck

        """all (List(map("one" -> 1, "two" -> 2))) should not { contain atMostOneOf (1 -> 1, 3 -> 3) }""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should not contain atMostOneOf (1 -> 1, 3 -> 3)""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should not { contain atMostOneOf ("1" -> "1", "three" -> "three") }""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should not contain atMostOneOf ("1" -> "1", "three" -> "three")""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """all (List(map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """all (List(map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """all (List(map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """all (List(map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """all (List(map("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.HashMap ` {

        import scala.collection.immutable.HashMap

        """all (List(HashMap("one" -> 1, "two" -> 2))) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf (1 -> 1, 2 -> 2))""" shouldNot typeCheck
        """all (List(HashMap(1 -> "one", 2 -> "two"))) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> "1", "two" -> "two"))""" shouldNot typeCheck
        """all (List(HashMap(1 -> "one", 2 -> "two"))) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck

        """all (List(HashMap("one" -> 1, "two" -> 2))) should not { contain atMostOneOf (1 -> 1, 3 -> 3) }""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should not contain atMostOneOf (1 -> 1, 3 -> 3)""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should not { contain atMostOneOf ("1" -> "1", "three" -> "three") }""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should not contain atMostOneOf ("1" -> "1", "three" -> "three")""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """all (List(HashMap("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """all (List(HashMap("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """all (List(HashMap("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """all (List(HashMap("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """all (List(HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.HashMap ` {

        import scala.collection.mutable

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf (1 -> 1, 2 -> 2))""" shouldNot typeCheck
        """all (List(mutable.HashMap(1 -> "one", 2 -> "two"))) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> "1", "two" -> "two"))""" shouldNot typeCheck
        """all (List(mutable.HashMap(1 -> "one", 2 -> "two"))) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should not { contain atMostOneOf (1 -> 1, 3 -> 3) }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should not contain atMostOneOf (1 -> 1, 3 -> 3)""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should not { contain atMostOneOf ("1" -> "1", "three" -> "three") }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should not contain atMostOneOf ("1" -> "1", "three" -> "three")""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1", "two" -> 2) and (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1", "two" -> 2)) and (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1", "two" -> 2) and contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1", "two" -> 2) and (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1", "two" -> 2)) and (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1", "two" -> 2) and contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """all (List(mutable.HashMap("one" -> 1, "two" -> 2))) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck
      }
      def `on java.util.Set` {

        val javaSet: java.util.Set[Int] = new java.util.HashSet
        javaSet.add(1)
        javaSet.add(2)

        """all (List(javaSet)) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """all (List(javaSet)) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(javaSet)) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """all (List(javaSet)) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """all (List(javaSet)) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """all (List(javaSet)) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """all (List(javaSet)) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """all (List(javaSet)) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """all (List(javaSet)) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """all (List(javaSet)) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """all (List(javaSet)) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """all (List(javaSet)) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """all (List(javaSet)) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(javaSet)) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """all (List(javaSet)) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """all (List(javaSet)) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """all (List(javaSet)) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """all (List(javaSet)) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(javaSet)) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(javaSet)) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """all (List(javaSet)) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(javaSet)) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """all (List(javaSet)) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """all (List(javaSet)) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """all (List(javaSet)) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """all (List(javaSet)) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """all (List(javaSet)) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """all (List(javaSet)) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      def `on java.util.Map` {

        val javaMap: java.util.Map[String, Int] = new java.util.HashMap
        javaMap.put("one",1)
        javaMap.put("two", 2)

        """all (List(javaMap)) should contain atMostOneOf (Entry(3, 3), Entry(2, 2))""" shouldNot typeCheck
        """all (List(javaMap)) should (contain atMostOneOf (Entry(3, 3), Entry(2, 2)))""" shouldNot typeCheck
        """all (List(javaMap)) should contain atMostOneOf (Entry(3, 3), Entry("two", "two"))""" shouldNot typeCheck
        """all (List(javaMap)) should contain atMostOneOf (Entry(3, 3), Entry("two", "two"))""" shouldNot typeCheck
        """all (List(javaMap)) should (contain atMostOneOf (Entry(3, 3), Entry("two", "two")))""" shouldNot typeCheck
        """all (List(javaMap)) should contain atMostOneOf (Entry(3, 3), Entry(2, 2))""" shouldNot typeCheck

        """all (List(javaMap)) should not { contain atMostOneOf (Entry(1, 1), Entry(3, 3)) }""" shouldNot typeCheck
        """all (List(javaMap)) should not contain atMostOneOf (Entry(1, 1), Entry(3, 3))""" shouldNot typeCheck
        """all (List(javaMap)) should (not contain atMostOneOf (Entry(1, 1), Entry(3, 3)))""" shouldNot typeCheck
        """all (List(javaMap)) should not { contain atMostOneOf (Entry(1, 1), Entry("three", "three")) }""" shouldNot typeCheck
        """all (List(javaMap)) should not contain atMostOneOf (Entry(1, 1), Entry("three", "three"))""" shouldNot typeCheck
        """all (List(javaMap)) should (not contain atMostOneOf (Entry(1, 1), Entry("three", "three")))""" shouldNot typeCheck

        """all (List(javaMap)) should { contain atMostOneOf (Entry("one", 1), Entry("two", 2)) and (contain atMostOneOf (Entry(2, 2), Entry(1, 1))) }""" shouldNot typeCheck
        """all (List(javaMap)) should ((contain atMostOneOf (Entry("one", 1), Entry("two", 2))) and (contain atMostOneOf (Entry(2, 2), Entry(1, 1))))""" shouldNot typeCheck
        """all (List(javaMap)) should (contain atMostOneOf (Entry("one", 1), Entry("two", 2)) and contain atMostOneOf (Entry(2, 2), Entry(1, 1)))""" shouldNot typeCheck
        """all (List(javaMap)) should { contain atMostOneOf (Entry("one", 1), Entry("two", 2)) and (contain atMostOneOf (Entry("two", "two"), Entry("one", "one"))) }""" shouldNot typeCheck
        """all (List(javaMap)) should ((contain atMostOneOf (Entry("one", 1), Entry("two", 2))) and (contain atMostOneOf (Entry("two", "two"), Entry("one", "one"))))""" shouldNot typeCheck
        """all (List(javaMap)) should (contain atMostOneOf (Entry("one", 1), Entry("two", 2)) and contain atMostOneOf (Entry("two", "two"), Entry("one", "one")))""" shouldNot typeCheck

        """all (List(javaMap)) should { contain atMostOneOf (Entry("dog", 99), Entry("cat", 77)) or (contain atMostOneOf (Entry(2, 2), Entry(1, 1))) }""" shouldNot typeCheck
        """all (List(javaMap)) should ((contain atMostOneOf (Entry("dog", 99), Entry("cat", 77))) or (contain atMostOneOf (Entry(2, 2), Entry(1, 1))))""" shouldNot typeCheck
        """all (List(javaMap)) should (contain atMostOneOf (Entry("dog", 99), Entry("cat", 77)) or contain atMostOneOf (Entry(2, 2), Entry(1, 1)))""" shouldNot typeCheck
        """all (List(javaMap)) should { contain atMostOneOf (Entry("dog", 99), Entry("cat", 77)) or (contain atMostOneOf (Entry("two", "two"), Entry("one", "one"))) }""" shouldNot typeCheck
        """all (List(javaMap)) should ((contain atMostOneOf (Entry("dog", 99), Entry("cat", 77))) or (contain atMostOneOf (Entry("two", "two"), Entry("one", "one"))))""" shouldNot typeCheck
        """all (List(javaMap)) should (contain atMostOneOf (Entry("dog", 99), Entry("cat", 77)) or contain atMostOneOf (Entry("two", "two"), Entry("one", "one")))""" shouldNot typeCheck

        """all (List(javaMap)) should { not { contain atMostOneOf (Entry("one", 1), Entry("five", 5)) } and not { contain atMostOneOf (Entry(1, 1), Entry(3, 3)) }}""" shouldNot typeCheck
        """all (List(javaMap)) should ((not contain atMostOneOf (Entry("one", 1), Entry("five", 5))) and (not contain atMostOneOf (Entry(1, 1), Entry(3, 3))))""" shouldNot typeCheck
        """all (List(javaMap)) should (not contain atMostOneOf (Entry("one", 1), Entry("five", 5)) and not contain atMostOneOf (Entry(1, 1), Entry(3, 3)))""" shouldNot typeCheck
        """all (List(javaMap)) should { not { contain atMostOneOf (Entry("one", 1), Entry("five", 5)) } and not { contain atMostOneOf (Entry("one", "one"), Entry("three", "three")) }}""" shouldNot typeCheck
        """all (List(javaMap)) should ((not contain atMostOneOf (Entry("one", 1), Entry("five", 5))) and (not contain atMostOneOf (Entry("one", "one"), Entry("three", "three"))))""" shouldNot typeCheck
        """all (List(javaMap)) should (not contain atMostOneOf (Entry("one", 1), Entry("five", 5)) and not contain atMostOneOf (Entry("one", "one"), Entry("three", "three")))""" shouldNot typeCheck

        """all (List(javaMap)) should { not { contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2))) } or not { contain atMostOneOf (Entry(1, 1), Entry(3, 3)) }}""" shouldNot typeCheck
        """all (List(javaMap)) should ((not contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2)))) or (not contain atMostOneOf (Entry(1, 1), Entry(3, 3))))""" shouldNot typeCheck
        """all (List(javaMap)) should (not contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2))) or not contain atMostOneOf (Entry(1, 1), Entry(3, 3)))""" shouldNot typeCheck
        """all (List(javaMap)) should { not { contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2))) } or not { contain atMostOneOf (Entry("one", "one"), Entry("three", "three")) }}""" shouldNot typeCheck
        """all (List(javaMap)) should ((not contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2)))) or (not contain atMostOneOf (Entry("one", "one"), Entry("three", "three"))))""" shouldNot typeCheck
        """all (List(javaMap)) should (not contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2))) or not contain atMostOneOf (Entry("one", "one"), Entry("three", "three")))""" shouldNot typeCheck
      }
*/
    }
  }
}
