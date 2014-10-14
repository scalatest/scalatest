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

class ShouldContainAtMostOneOfTypeCheckSpec extends FunSpec with CheckedEquality {

  // Checking for a specific size
  describe("The 'contain atMostOneOf (1, <element>)' syntax") {

    describe("should give a type error if the types are not compatible") {

      it("on Array") {

        """Array(1, 2) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """Array(1, 2) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """Array(1, 2) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """Array(1, 2) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """Array(1, 2) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """Array(1, 2) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """Array(1, 2) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """Array(1, 2) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """Array(1, 2) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """Array(1, 2) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """Array(1, 2) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """Array(1, 2) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """Array(1, 2) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """Array(1, 2) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """Array(1, 2) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """Array(1, 2) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """Array(1, 2) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """Array(1, 2) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """Array(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """Array(1, 2) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """Array(1, 2) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """Array(1, 2) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """Array(1, 2) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """Array(1, 2) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """Array(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """Array(1, 2) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """Array(1, 2) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """Array(1, 2) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
/*
      it("on scala.collection.immutable.Set") {

        """Set(1, 2) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """Set(1, 2) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """Set(1, 2) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """Set(1, 2) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """Set(1, 2) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """Set(1, 2) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """Set(1, 2) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """Set(1, 2) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """Set(1, 2) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """Set(1, 2) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """Set(1, 2) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """Set(1, 2) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """Set(1, 2) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """Set(1, 2) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """Set(1, 2) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """Set(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """Set(1, 2) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """Set(1, 2) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """Set(1, 2) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """Set(1, 2) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """Set(1, 2) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """Set(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """Set(1, 2) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """Set(1, 2) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """Set(1, 2) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      it("on scala.collection.mutable.Set") {

        import scala.collection.mutable

        """mutable.Set(1, 2) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """mutable.Set(1, 2) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """mutable.Set(1, 2) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """mutable.Set(1, 2) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """mutable.Set(1, 2) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """mutable.Set(1, 2) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """mutable.Set(1, 2) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """mutable.Set(1, 2) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """mutable.Set(1, 2) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """mutable.Set(1, 2) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """mutable.Set(1, 2) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """mutable.Set(1, 2) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """mutable.Set(1, 2) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """mutable.Set(1, 2) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      it("on scala.collection.Set") {

        val set: scala.collection.Set[Int] = Set(1, 2)

        """set should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """set should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """set should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """set should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """set should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """set should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """set should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """set should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """set should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """set should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """set should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """set should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """set should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """set should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """set should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """set should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """set should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """set should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """set should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """set should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """set should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """set should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """set should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """set should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """set should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """set should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """set should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """set should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      it("on scala.collection.immutable.HashSet") {

        import scala.collection.immutable.HashSet

        """HashSet(1, 2) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """HashSet(1, 2) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """HashSet(1, 2) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """HashSet(1, 2) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """HashSet(1, 2) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """HashSet(1, 2) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """HashSet(1, 2) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """HashSet(1, 2) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """HashSet(1, 2) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """HashSet(1, 2) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """HashSet(1, 2) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """HashSet(1, 2) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """HashSet(1, 2) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """HashSet(1, 2) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """HashSet(1, 2) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """HashSet(1, 2) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """HashSet(1, 2) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """HashSet(1, 2) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """HashSet(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """HashSet(1, 2) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """HashSet(1, 2) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """HashSet(1, 2) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """HashSet(1, 2) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """HashSet(1, 2) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """HashSet(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """HashSet(1, 2) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """HashSet(1, 2) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """HashSet(1, 2) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      it("on scala.collection.mutable.HashSet") {

        import scala.collection.mutable

        """mutable.HashSet(1, 2) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """mutable.HashSet(1, 2) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """mutable.HashSet(1, 2) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """mutable.HashSet(1, 2) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """mutable.HashSet(1, 2) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """mutable.HashSet(1, 2) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """mutable.HashSet(1, 2) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      it("on List") {

        """List(1, 2) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """List(1, 2) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """List(1, 2) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """List(1, 2) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """List(1, 2) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """List(1, 2) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """List(1, 2) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """List(1, 2) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """List(1, 2) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """List(1, 2) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """List(1, 2) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """List(1, 2) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """List(1, 2) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """List(1, 2) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """List(1, 2) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """List(1, 2) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """List(1, 2) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """List(1, 2) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """List(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """List(1, 2) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """List(1, 2) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """List(1, 2) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """List(1, 2) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """List(1, 2) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """List(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """List(1, 2) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """List(1, 2) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """List(1, 2) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      it("on Vector") {

        """Vector(1, 2) should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """Vector(1, 2) should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """Vector(1, 2) should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """Vector(1, 2) should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """Vector(1, 2) should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """Vector(1, 2) should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """Vector(1, 2) should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """Vector(1, 2) should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """Vector(1, 2) should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """Vector(1, 2) should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """Vector(1, 2) should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """Vector(1, 2) should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """Vector(1, 2) should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """Vector(1, 2) should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """Vector(1, 2) should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """Vector(1, 2) should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """Vector(1, 2) should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """Vector(1, 2) should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """Vector(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """Vector(1, 2) should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """Vector(1, 2) should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """Vector(1, 2) should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """Vector(1, 2) should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """Vector(1, 2) should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """Vector(1, 2) should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """Vector(1, 2) should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """Vector(1, 2) should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """Vector(1, 2) should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      it("on java.util.List") {

        val javaList: java.util.List[Int] = new java.util.ArrayList
        javaList.add(1)
        javaList.add(2)
      
        """javaList should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """javaList should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """javaList should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """javaList should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """javaList should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """javaList should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """javaList should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """javaList should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """javaList should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """javaList should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """javaList should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """javaList should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """javaList should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """javaList should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """javaList should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """javaList should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """javaList should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """javaList should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """javaList should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """javaList should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """javaList should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """javaList should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """javaList should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """javaList should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """javaList should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """javaList should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """javaList should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """javaList should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      it("on scala.collection.immutable.Map ") {

        """Map("one" -> 1, "two" -> 2) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain atMostOneOf (1 -> 1, 2 -> 2))""" shouldNot typeCheck
        """Map(1 -> "one", 2 -> "two") should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> "1", "two" -> "two"))""" shouldNot typeCheck
        """Map(1 -> "one", 2 -> "two") should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck

        """Map("one" -> 1, "two" -> 2) should not { contain atMostOneOf (1 -> 1, 3 -> 3) }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should not contain atMostOneOf (1 -> 1, 3 -> 3)""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should not { contain atMostOneOf ("1" -> "1", "three" -> "three") }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should not contain atMostOneOf ("1" -> "1", "three" -> "three")""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """Map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """Map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """Map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """Map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck
      }
      it("on scala.collection.mutable.Map ") {

        import scala.collection.mutable

        """mutable.Map("one" -> 1, "two" -> 2) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain atMostOneOf (1 -> 1, 2 -> 2))""" shouldNot typeCheck
        """mutable.Map(1 -> "one", 2 -> "two") should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> "1", "two" -> "two"))""" shouldNot typeCheck
        """mutable.Map(1 -> "one", 2 -> "two") should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck

        """mutable.Map("one" -> 1, "two" -> 2) should not { contain atMostOneOf (1 -> 1, 3 -> 3) }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should not contain atMostOneOf (1 -> 1, 3 -> 3)""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should not { contain atMostOneOf ("1" -> "1", "three" -> "three") }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should not contain atMostOneOf ("1" -> "1", "three" -> "three")""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """mutable.Map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """mutable.Map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1", "cat" -> 77) or (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1", "cat" -> 77)) or (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1", "cat" -> 77) or contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """mutable.Map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """mutable.Map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """mutable.Map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck
      }
      it("on scala.collection.Map ") {

        val map: scala.collection.Map[String, Int] = Map("one" -> 1, "two" -> 2)

        """map("one" -> 1, "two" -> 2) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain atMostOneOf (1 -> 1, 2 -> 2))""" shouldNot typeCheck
        """map(1 -> "one", 2 -> "two") should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> "1", "two" -> "two"))""" shouldNot typeCheck
        """map(1 -> "one", 2 -> "two") should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck

        """map("one" -> 1, "two" -> 2) should not { contain atMostOneOf (1 -> 1, 3 -> 3) }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should not contain atMostOneOf (1 -> 1, 3 -> 3)""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should not { contain atMostOneOf ("1" -> "1", "three" -> "three") }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should not contain atMostOneOf ("1" -> "1", "three" -> "three")""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """map("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck
      }
      it("on scala.collection.immutable.HashMap ") {

        import scala.collection.immutable.HashMap

        """HashMap("one" -> 1, "two" -> 2) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf (1 -> 1, 2 -> 2))""" shouldNot typeCheck
        """HashMap(1 -> "one", 2 -> "two") should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> "1", "two" -> "two"))""" shouldNot typeCheck
        """HashMap(1 -> "one", 2 -> "two") should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck

        """HashMap("one" -> 1, "two" -> 2) should not { contain atMostOneOf (1 -> 1, 3 -> 3) }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should not contain atMostOneOf (1 -> 1, 3 -> 3)""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should not { contain atMostOneOf ("1" -> "1", "three" -> "three") }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should not contain atMostOneOf ("1" -> "1", "three" -> "three")""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """HashMap("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "two" -> 2) and (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "two" -> 2)) and (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "two" -> 2) and contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """HashMap("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """HashMap("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """HashMap("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck
      }
      it("on scala.collection.mutable.HashMap ") {

        import scala.collection.mutable

        """mutable.HashMap("one" -> 1, "two" -> 2) should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf (1 -> 1, 2 -> 2))""" shouldNot typeCheck
        """mutable.HashMap(1 -> "one", 2 -> "two") should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should contain atMostOneOf ("1" -> "1", "two" -> "two")""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> "1", "two" -> "two"))""" shouldNot typeCheck
        """mutable.HashMap(1 -> "one", 2 -> "two") should contain atMostOneOf (1 -> 1, 2 -> 2)""" shouldNot typeCheck

        """mutable.HashMap("one" -> 1, "two" -> 2) should not { contain atMostOneOf (1 -> 1, 3 -> 3) }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should not contain atMostOneOf (1 -> 1, 3 -> 3)""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should not { contain atMostOneOf ("1" -> "1", "three" -> "three") }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should not contain atMostOneOf ("1" -> "1", "three" -> "three")""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """mutable.HashMap("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1", "two" -> 2) and (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1", "two" -> 2)) and (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1", "two" -> 2) and contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1", "two" -> 2) and (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1", "two" -> 2)) and (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1", "two" -> 2) and contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """mutable.HashMap("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf (1 -> 1, 1 -> 1)) }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf (1 -> 1, 1 -> 1)))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf (1 -> 1, 1 -> 1))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should { contain atMostOneOf ("1" -> 1, "cat" -> 77) or (contain atMostOneOf ("1" -> "1", "one" -> "one")) }""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((contain atMostOneOf ("1" -> 1, "cat" -> 77)) or (contain atMostOneOf ("1" -> "1", "one" -> "one")))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (contain atMostOneOf ("1" -> 1, "cat" -> 77) or contain atMostOneOf ("1" -> "1", "one" -> "one"))""" shouldNot typeCheck

        """mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "five" -> 5) } and not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "five" -> 5)) and (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "five" -> 5) and not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck

        """mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf (1 -> 1, 3 -> 3) }}""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf (1 -> 1, 3 -> 3)))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf (1 -> 1, 3 -> 3))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain atMostOneOf ("1" -> 1, "two" -> 2) } or not { contain atMostOneOf ("1" -> "1", "three" -> "three") }}""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain atMostOneOf ("1" -> 1, "two" -> 2)) or (not contain atMostOneOf ("1" -> "1", "three" -> "three")))""" shouldNot typeCheck
        """mutable.HashMap("one" -> 1, "two" -> 2) should (not contain atMostOneOf ("1" -> 1, "two" -> 2) or not contain atMostOneOf ("1" -> "1", "three" -> "three"))""" shouldNot typeCheck
      }
      it("on java.util.Set") {

        val javaSet: java.util.Set[Int] = new java.util.HashSet
        javaSet.add(1)
        javaSet.add(2)

        """javaSet should contain atMostOneOf ("1", "2")""" shouldNot typeCheck
        """javaSet should (contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """javaSet should not { contain atMostOneOf ("1", "3") }""" shouldNot typeCheck
        """javaSet should not contain atMostOneOf ("1", "3")""" shouldNot typeCheck

        """javaSet should { contain atMostOneOf ("1", "2") and (contain atMostOneOf (1, 1)) }""" shouldNot typeCheck
        """javaSet should ((contain atMostOneOf ("1", "2")) and (contain atMostOneOf (1, 1)))""" shouldNot typeCheck
        """javaSet should (contain atMostOneOf ("1", "2") and contain atMostOneOf (1, 1))""" shouldNot typeCheck
        """javaSet should { contain atMostOneOf (1, 2) and (contain atMostOneOf ("1", "1")) }""" shouldNot typeCheck
        """javaSet should ((contain atMostOneOf (1, 2)) and (contain atMostOneOf ("1", "1")))""" shouldNot typeCheck
        """javaSet should (contain atMostOneOf (1, 2) and contain atMostOneOf ("1", "1"))""" shouldNot typeCheck

        """javaSet should { contain atMostOneOf ("1", "77") or (contain atMostOneOf (1, 2)) }""" shouldNot typeCheck
        """javaSet should ((contain atMostOneOf ("1", "77")) or (contain atMostOneOf (1, 2)))""" shouldNot typeCheck
        """javaSet should (contain atMostOneOf ("1", "77") or contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """javaSet should { contain atMostOneOf (1, 77) or (contain atMostOneOf ("1", "2")) }""" shouldNot typeCheck
        """javaSet should ((contain atMostOneOf (1, 77)) or (contain atMostOneOf ("1", "2")))""" shouldNot typeCheck
        """javaSet should (contain atMostOneOf (1, 77) or contain atMostOneOf ("1", "2"))""" shouldNot typeCheck

        """javaSet should { not { contain atMostOneOf ("1", "5") } and not { contain atMostOneOf (1, 3) }}""" shouldNot typeCheck
        """javaSet should ((not contain atMostOneOf ("1", "5")) and (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """javaSet should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """javaSet should { not { contain atMostOneOf (1, 5) } and not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck
        """javaSet should ((not contain atMostOneOf (1, 5)) and (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """javaSet should { not { contain atMostOneOf (1, 1) } or not { contain atMostOneOf ("1", "3") }}""" shouldNot typeCheck

        """javaSet should ((not contain atMostOneOf ("1", "1")) or (not contain atMostOneOf (1, 3)))""" shouldNot typeCheck
        """javaSet should (not contain atMostOneOf ("1", "3") or not contain atMostOneOf (1, 2))""" shouldNot typeCheck
        """javaSet should (not contain atMostOneOf ("1", "5") and not contain atMostOneOf (1, 3))""" shouldNot typeCheck
        """javaSet should ((not contain atMostOneOf (1, 1)) or (not contain atMostOneOf ("1", "3")))""" shouldNot typeCheck
        """javaSet should (not contain atMostOneOf (1, 3) or not contain atMostOneOf ("1", "2"))""" shouldNot typeCheck
        """javaSet should (not contain atMostOneOf (1, 5) and not contain atMostOneOf ("1", "3"))""" shouldNot typeCheck
      }
      it("on java.util.Map") {

        val javaMap: java.util.Map[String, Int] = new java.util.HashMap
        javaMap.put("one",1)
        javaMap.put("two", 2)

        """javaMap should contain atMostOneOf (Entry(3, 3), Entry(2, 2))""" shouldNot typeCheck
        """javaMap should (contain atMostOneOf (Entry(3, 3), Entry(2, 2)))""" shouldNot typeCheck
        """javaMap should contain atMostOneOf (Entry(3, 3), Entry("two", "two"))""" shouldNot typeCheck
        """javaMap should contain atMostOneOf (Entry(3, 3), Entry("two", "two"))""" shouldNot typeCheck
        """javaMap should (contain atMostOneOf (Entry(3, 3), Entry("two", "two")))""" shouldNot typeCheck
        """javaMap should contain atMostOneOf (Entry(3, 3), Entry(2, 2))""" shouldNot typeCheck

        """javaMap should not { contain atMostOneOf (Entry(1, 1), Entry(3, 3)) }""" shouldNot typeCheck
        """javaMap should not contain atMostOneOf (Entry(1, 1), Entry(3, 3))""" shouldNot typeCheck
        """javaMap should (not contain atMostOneOf (Entry(1, 1), Entry(3, 3)))""" shouldNot typeCheck
        """javaMap should not { contain atMostOneOf (Entry(1, 1), Entry("three", "three")) }""" shouldNot typeCheck
        """javaMap should not contain atMostOneOf (Entry(1, 1), Entry("three", "three"))""" shouldNot typeCheck
        """javaMap should (not contain atMostOneOf (Entry(1, 1), Entry("three", "three")))""" shouldNot typeCheck

        """javaMap should { contain atMostOneOf (Entry("one", 1), Entry("two", 2)) and (contain atMostOneOf (Entry(2, 2), Entry(1, 1))) }""" shouldNot typeCheck
        """javaMap should ((contain atMostOneOf (Entry("one", 1), Entry("two", 2))) and (contain atMostOneOf (Entry(2, 2), Entry(1, 1))))""" shouldNot typeCheck
        """javaMap should (contain atMostOneOf (Entry("one", 1), Entry("two", 2)) and contain atMostOneOf (Entry(2, 2), Entry(1, 1)))""" shouldNot typeCheck
        """javaMap should { contain atMostOneOf (Entry("one", 1), Entry("two", 2)) and (contain atMostOneOf (Entry("two", "two"), Entry("one", "one"))) }""" shouldNot typeCheck
        """javaMap should ((contain atMostOneOf (Entry("one", 1), Entry("two", 2))) and (contain atMostOneOf (Entry("two", "two"), Entry("one", "one"))))""" shouldNot typeCheck
        """javaMap should (contain atMostOneOf (Entry("one", 1), Entry("two", 2)) and contain atMostOneOf (Entry("two", "two"), Entry("one", "one")))""" shouldNot typeCheck

        """javaMap should { contain atMostOneOf (Entry("dog", 99), Entry("cat", 77)) or (contain atMostOneOf (Entry(2, 2), Entry(1, 1))) }""" shouldNot typeCheck
        """javaMap should ((contain atMostOneOf (Entry("dog", 99), Entry("cat", 77))) or (contain atMostOneOf (Entry(2, 2), Entry(1, 1))))""" shouldNot typeCheck
        """javaMap should (contain atMostOneOf (Entry("dog", 99), Entry("cat", 77)) or contain atMostOneOf (Entry(2, 2), Entry(1, 1)))""" shouldNot typeCheck
        """javaMap should { contain atMostOneOf (Entry("dog", 99), Entry("cat", 77)) or (contain atMostOneOf (Entry("two", "two"), Entry("one", "one"))) }""" shouldNot typeCheck
        """javaMap should ((contain atMostOneOf (Entry("dog", 99), Entry("cat", 77))) or (contain atMostOneOf (Entry("two", "two"), Entry("one", "one"))))""" shouldNot typeCheck
        """javaMap should (contain atMostOneOf (Entry("dog", 99), Entry("cat", 77)) or contain atMostOneOf (Entry("two", "two"), Entry("one", "one")))""" shouldNot typeCheck

        """javaMap should { not { contain atMostOneOf (Entry("one", 1), Entry("five", 5)) } and not { contain atMostOneOf (Entry(1, 1), Entry(3, 3)) }}""" shouldNot typeCheck
        """javaMap should ((not contain atMostOneOf (Entry("one", 1), Entry("five", 5))) and (not contain atMostOneOf (Entry(1, 1), Entry(3, 3))))""" shouldNot typeCheck
        """javaMap should (not contain atMostOneOf (Entry("one", 1), Entry("five", 5)) and not contain atMostOneOf (Entry(1, 1), Entry(3, 3)))""" shouldNot typeCheck
        """javaMap should { not { contain atMostOneOf (Entry("one", 1), Entry("five", 5)) } and not { contain atMostOneOf (Entry("one", "one"), Entry("three", "three")) }}""" shouldNot typeCheck
        """javaMap should ((not contain atMostOneOf (Entry("one", 1), Entry("five", 5))) and (not contain atMostOneOf (Entry("one", "one"), Entry("three", "three"))))""" shouldNot typeCheck
        """javaMap should (not contain atMostOneOf (Entry("one", 1), Entry("five", 5)) and not contain atMostOneOf (Entry("one", "one"), Entry("three", "three")))""" shouldNot typeCheck

        """javaMap should { not { contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2))) } or not { contain atMostOneOf (Entry(1, 1), Entry(3, 3)) }}""" shouldNot typeCheck
        """javaMap should ((not contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2)))) or (not contain atMostOneOf (Entry(1, 1), Entry(3, 3))))""" shouldNot typeCheck
        """javaMap should (not contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2))) or not contain atMostOneOf (Entry(1, 1), Entry(3, 3)))""" shouldNot typeCheck
        """javaMap should { not { contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2))) } or not { contain atMostOneOf (Entry("one", "one"), Entry("three", "three")) }}""" shouldNot typeCheck
        """javaMap should ((not contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2)))) or (not contain atMostOneOf (Entry("one", "one"), Entry("three", "three"))))""" shouldNot typeCheck
        """javaMap should (not contain atMostOneOf (Entry("one", 1), Entry(Entry("two", 2))) or not contain atMostOneOf (Entry("one", "one"), Entry("three", "three")))""" shouldNot typeCheck
      }
*/
    }
  }
}
