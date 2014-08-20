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

import org.scalatest._

class IsInUnderEnabledEqualitySpec extends Spec with Matchers with EnabledEquality {

  object `The IsIn and IsNotIn syntax should` {
    def `allow type checked containership tests` {
      def `on Array` {

        (1 isIn Array(1, 2, 3)) shouldBe true
        (5 isIn Array(1, 2, 3)) shouldBe false
        (1 isNotIn Array(1, 2, 3)) shouldBe false
        (5 isNotIn Array(1, 2, 3)) shouldBe true
        """"1" isIn Array(1, 2, 3)""" shouldNot typeCheck
        """"1" isNotIn Array(1, 2, 3)""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.Set` {

        (1 isIn Set(1, 2, 3)) shouldBe true
        (5 isIn Set(1, 2, 3)) shouldBe false
        (1 isNotIn Set(1, 2, 3)) shouldBe false
        (5 isNotIn Set(1, 2, 3)) shouldBe true
        """"1" isIn Set(1, 2, 3)""" shouldNot typeCheck
        """"1" isNotIn Set(1, 2, 3)""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.Set` {

        import scala.collection.mutable

        (1 isIn mutable.Set(1, 2, 3)) shouldBe true
        (5 isIn mutable.Set(1, 2, 3)) shouldBe false
        (1 isNotIn mutable.Set(1, 2, 3)) shouldBe false
        (5 isNotIn mutable.Set(1, 2, 3)) shouldBe true
        """"1" isIn mutable.Set(1, 2, 3)""" shouldNot typeCheck
        """"1" isNotIn mutable.Set(1, 2, 3)""" shouldNot typeCheck
      }
      def `on scala.collection.Set` {

        val set: scala.collection.Set[Int] = Set(1, 2)

        (1 isIn set) shouldBe true
        (5 isIn set) shouldBe false
        (1 isNotIn set) shouldBe false
        (5 isNotIn set) shouldBe true
        """"1" isIn set""" shouldNot typeCheck
        """"1" isNotIn set""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.HashSet` {

        import scala.collection.immutable.HashSet

        """HashSet(1, 2) should contain ("2")""" shouldNot typeCheck
        (1 isIn HashSet(1, 2, 3)) shouldBe true
        (5 isIn HashSet(1, 2, 3)) shouldBe false
        (1 isNotIn HashSet(1, 2, 3)) shouldBe false
        (5 isNotIn HashSet(1, 2, 3)) shouldBe true
        """"1" isIn HashSet(1, 2, 3)""" shouldNot typeCheck
        """"1" isNotIn HashSet(1, 2, 3)""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.HashSet` {

        import scala.collection.mutable

        (1 isIn mutable.HashSet(1, 2, 3)) shouldBe true
        (5 isIn mutable.HashSet(1, 2, 3)) shouldBe false
        (1 isNotIn mutable.HashSet(1, 2, 3)) shouldBe false
        (5 isNotIn mutable.HashSet(1, 2, 3)) shouldBe true
        """"1" isIn mutable.HashSet(1, 2, 3)""" shouldNot typeCheck
        """"1" isNotIn mutable.HashSet(1, 2, 3)""" shouldNot typeCheck
      }
      def `on List` {

        (1 isIn List(1, 2, 3)) shouldBe true
        (5 isIn List(1, 2, 3)) shouldBe false
        (1 isNotIn List(1, 2, 3)) shouldBe false
        (5 isNotIn List(1, 2, 3)) shouldBe true
        """"1" isIn List(1, 2, 3)""" shouldNot typeCheck
        """"1" isNotIn List(1, 2, 3)""" shouldNot typeCheck
      }
      def `on Vector` {

        (1 isIn Vector(1, 2, 3)) shouldBe true
        (5 isIn Vector(1, 2, 3)) shouldBe false
        (1 isNotIn Vector(1, 2, 3)) shouldBe false
        (5 isNotIn Vector(1, 2, 3)) shouldBe true
        """"1" isIn Vector(1, 2, 3)""" shouldNot typeCheck
        """"1" isNotIn Vector(1, 2, 3)""" shouldNot typeCheck
      }
      def `on java.util.List` {

        val javaList: java.util.List[Int] = new java.util.ArrayList
        javaList.add(1)
        javaList.add(2)
      
        (1 isIn javaList) shouldBe true
        (5 isIn javaList) shouldBe false
        (1 isNotIn javaList) shouldBe false
        (5 isNotIn javaList) shouldBe true
        """"1" isIn javaList""" shouldNot typeCheck
        """"1" isNotIn javaList""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.Map ` {

        ("one" -> 1 isIn Map("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe true
        ("five" -> 5 isIn Map("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe false
        ("one" -> 1 isNotIn Map("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe false
        ("five" -> 5 isNotIn Map("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe true
        """"1" isIn Map("one" -> 1, "two" -> 2, "three" -> 3)""" shouldNot typeCheck
        """"1" isNotIn Map("one" -> 1, "two" -> 2, "three" -> 3)""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.Map ` {

        import scala.collection.mutable

        ("one" -> 1 isIn mutable.Map("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe true
        ("five" -> 5 isIn mutable.Map("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe false
        ("one" -> 1 isNotIn mutable.Map("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe false
        ("five" -> 5 isNotIn mutable.Map("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe true
        """"1" isIn mutable.Map("one" -> 1, "two" -> 2, "three" -> 3)""" shouldNot typeCheck
        """"1" isNotIn mutable.Map("one" -> 1, "two" -> 2, "three" -> 3)""" shouldNot typeCheck
      }
      def `on scala.collection.Map ` {

        val map: scala.collection.Map[String, Int] = Map("one" -> 1, "two" -> 2)

        ("one" -> 1 isIn map) shouldBe true
        ("five" -> 5 isIn map) shouldBe false
        ("one" -> 1 isNotIn map) shouldBe false
        ("five" -> 5 isNotIn map) shouldBe true
        """"1" isIn map""" shouldNot typeCheck
        """"1" isNotIn map""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.HashMap ` {

        import scala.collection.immutable.HashMap

        ("one" -> 1 isIn HashMap("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe true
        ("five" -> 5 isIn HashMap("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe false
        ("one" -> 1 isNotIn HashMap("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe false
        ("five" -> 5 isNotIn HashMap("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe true
        """"1" isIn HashMap("one" -> 1, "two" -> 2, "three" -> 3)""" shouldNot typeCheck
        """"1" isNotIn HashMap("one" -> 1, "two" -> 2, "three" -> 3)""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.HashMap ` {

        import scala.collection.mutable

        ("one" -> 1 isIn mutable.HashMap("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe true
        ("five" -> 5 isIn mutable.HashMap("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe false
        ("one" -> 1 isNotIn mutable.HashMap("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe false
        ("five" -> 5 isNotIn mutable.HashMap("one" -> 1, "two" -> 2, "three" -> 3)) shouldBe true
        """"1" isIn mutable.HashMap("one" -> 1, "two" -> 2, "three" -> 3)""" shouldNot typeCheck
        """"1" isNotIn mutable.HashMap("one" -> 1, "two" -> 2, "three" -> 3)""" shouldNot typeCheck
      }
      def `on java.util.Set` {

        val javaSet: java.util.Set[Int] = new java.util.HashSet
        javaSet.add(1)
        javaSet.add(2)

        (1 isIn javaSet) shouldBe true
        (5 isIn javaSet) shouldBe false
        (1 isNotIn javaSet) shouldBe false
        (5 isNotIn javaSet) shouldBe true
        """"1" isIn javaSet""" shouldNot typeCheck
        """"1" isNotIn javaSet""" shouldNot typeCheck
      }
      def `on java.util.Map` {

        val javaMap: java.util.Map[String, Int] = new java.util.HashMap
        javaMap.put("one",1)
        javaMap.put("two", 2)

        (Entry("one", 1) isIn javaMap) shouldBe true
        (Entry("five", 5) isIn javaMap) shouldBe false
        (Entry("two", 2) isNotIn javaMap) shouldBe false
        (Entry("five", 5) isNotIn javaMap) shouldBe true
        """"1" isIn javaMap""" shouldNot typeCheck
        """"1" isNotIn javaMap""" shouldNot typeCheck
      }
    }
  }
}
