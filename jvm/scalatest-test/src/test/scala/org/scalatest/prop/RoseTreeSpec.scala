/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalatest.prop

import org.scalactic.anyvals._
import org.scalatest.exceptions.TestFailedException
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RoseTreeSpec extends AnyFunSpec with Matchers {
  describe("A RoseTree") {
    it("should offer a toString that gives the value only") {
      val irt = new RoseTree[Int] {
        val value: Int = 42;

        def shrinks(rnd: Randomizer) = (List.empty, rnd)
      }
      irt.toString shouldBe "RoseTree(42)"
    }
    it("should offer a map and flatMap method") {

      import RoseTreeSpec._

      val rt = intRoseTree(10)
      rt.value shouldBe 10
      rt.map(n => n.toString + "!").value shouldBe "10!"

      val rt2 = charRoseTree('e')
      rt.flatMap(n => rt2.map(c => (n.toString + "!", (c - 1).toChar))).value shouldBe (("10!", 'd'))
    }
    it("should offer a depthFirstShrinks method") {
      import RoseTreeSpec._

      val rt = intRoseTree(72)
      rt.value shouldBe 72

      val (shrinks, _) = rt.depthFirstShrinks(_ < 12, Randomizer.default)
      shrinks should have length 1
      shrinks(0).value shouldBe 12
    }
  }
  describe("A Rose") {
    it("should have a toString that gives the value") {
      Rose(42).toString shouldBe "Rose(42)"
    }
  }
}

object RoseTreeSpec {
  def intRoseTree(i: Int): RoseTree[Int] =
    new RoseTree[Int] {
      val value: Int = i

      def shrinks(rnd: Randomizer): (List[RoseTree[Int]], Randomizer) = {
        val roseTrees = if (value > 0) (0 to value - 1).toList.reverse.map(x => intRoseTree(x)) else List.empty
        (roseTrees, rnd)
      }
    }

  def charRoseTree(c: Char): RoseTree[Char] =
    new RoseTree[Char] {
      val value: Char = c
      def shrinks(rnd: Randomizer): (List[RoseTree[Char]], Randomizer) = {
        val userFriendlyChars = "abcdefghikjlmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        if (userFriendlyChars.indexOf(c) >= 0) (List.empty, rnd)
        else (userFriendlyChars.toList.map(c => Rose(c)), rnd)
      }
    }

  def unfold[a](rt: RoseTree[a], indent: String = ""): Unit = {
    println(s"$indent ${rt.value}")
    val (roseTrees, rnd2) = rt.shrinks(Randomizer.default)
    roseTrees.foreach(t => unfold(t, s"$indent  "))
  }
}

