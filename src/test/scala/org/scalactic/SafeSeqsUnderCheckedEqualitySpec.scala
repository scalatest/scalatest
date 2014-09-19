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

class SafeSeqsUnderCheckedEqualitySpec extends Spec with Matchers with CheckedEquality with SafeSeqs {

  object `The sContains syntax should` {
    def `allow type checked containership tests` {
      def `on Array` {

        (Array(1, 2, 3) sContains 1) shouldBe true
        (Array(1, 2, 3) sContains 5) shouldBe false
        """Array(1, 2, 3) sContains "1"""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.Set` {

        (Set(1, 2, 3) sContains 1) shouldBe true
        (Set(1, 2, 3) sContains 5) shouldBe false
        """Set(1, 2, 3) sContains "1"""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.Set` {

        import scala.collection.mutable

        (mutable.Set(1, 2, 3) sContains 1) shouldBe true
        (mutable.Set(1, 2, 3) sContains 5) shouldBe false
        """mutable.Set(1, 2, 3) sContains "1"""" shouldNot typeCheck
      }
      def `on scala.collection.Set` {

        val set: scala.collection.Set[Int] = Set(1, 2)

        (set sContains 1) shouldBe true
        (set sContains 5) shouldBe false
        """set sContains "1"""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.HashSet` {

        import scala.collection.immutable.HashSet

        """HashSet(1, 2) should contain ("2")""" shouldNot typeCheck
        (HashSet(1, 2, 3) sContains 1) shouldBe true
        (HashSet(1, 2, 3) sContains 5) shouldBe false
        """HashSet(1, 2, 3) sContains "1"""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.HashSet` {

        import scala.collection.mutable

        (mutable.HashSet(1, 2, 3) sContains 1) shouldBe true
        (mutable.HashSet(1, 2, 3) sContains 5) shouldBe false
        """mutable.HashSet(1, 2, 3) sContains "1"""" shouldNot typeCheck
      }
      def `on List` {

        (List(1, 2, 3) sContains 1) shouldBe true
        (List(1, 2, 3) sContains 5) shouldBe false
        """List(1, 2, 3) sContains "1"""" shouldNot typeCheck
      }
      def `on Vector` {

        (Vector(1, 2, 3) sContains 1) shouldBe true
        (Vector(1, 2, 3) sContains 5) shouldBe false
        """Vector(1, 2, 3) sContains "1"""" shouldNot typeCheck
      }
      def `on java.util.List` {

        val javaList: java.util.List[Int] = new java.util.ArrayList
        javaList.add(1)
        javaList.add(2)
      
        (javaList sContains 1) shouldBe true
        (javaList sContains 5) shouldBe false
        """javaList sContains "1"""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.Map ` {

        (Map("one" -> 1, "two" -> 2, "three" -> 3) sContains "one" -> 1) shouldBe true
        (Map("one" -> 1, "two" -> 2, "three" -> 3) sContains "five" -> 5) shouldBe false
        """Map("one" -> 1, "two" -> 2, "three" -> 3) sContains "1"""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.Map ` {

        import scala.collection.mutable

        (mutable.Map("one" -> 1, "two" -> 2, "three" -> 3) sContains "one" -> 1) shouldBe true
        (mutable.Map("one" -> 1, "two" -> 2, "three" -> 3) sContains "five" -> 5) shouldBe false
        """mutable.Map("one" -> 1, "two" -> 2, "three" -> 3) sContains "1"""" shouldNot typeCheck
      }
      def `on scala.collection.Map ` {

        val map: scala.collection.Map[String, Int] = Map("one" -> 1, "two" -> 2)

        (map sContains "one" -> 1) shouldBe true
        (map sContains "five" -> 5) shouldBe false
        """map sContains "1"""" shouldNot typeCheck
      }
      def `on scala.collection.immutable.HashMap ` {

        import scala.collection.immutable.HashMap

        (HashMap("one" -> 1, "two" -> 2, "three" -> 3) sContains "one" -> 1) shouldBe true
        (HashMap("one" -> 1, "two" -> 2, "three" -> 3) sContains "five" -> 5) shouldBe false
        """HashMap("one" -> 1, "two" -> 2, "three" -> 3) sContains "1"""" shouldNot typeCheck
      }
      def `on scala.collection.mutable.HashMap ` {

        import scala.collection.mutable

        (mutable.HashMap("one" -> 1, "two" -> 2, "three" -> 3) sContains "one" -> 1) shouldBe true
        (mutable.HashMap("one" -> 1, "two" -> 2, "three" -> 3) sContains "five" -> 5) shouldBe false
        """mutable.HashMap("one" -> 1, "two" -> 2, "three" -> 3) sContains "1"""" shouldNot typeCheck
      }
      def `on java.util.Set` {

        val javaSet: java.util.Set[Int] = new java.util.HashSet
        javaSet.add(1)
        javaSet.add(2)

        (javaSet sContains 1) shouldBe true
        (javaSet sContains 5) shouldBe false
        """javaSet sContains "1"""" shouldNot typeCheck
      }
      def `on java.util.Map` {

        val javaMap: java.util.Map[String, Int] = new java.util.HashMap
        javaMap.put("one",1)
        javaMap.put("two", 2)

        (javaMap sContains Entry("one", 1)) shouldBe true
        (javaMap sContains Entry("five", 5)) shouldBe false
        """javaMap sContains "1"""" shouldNot typeCheck
      }
    }
  }
}
