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
import org.scalactic.ColCompatHelper._

class RoseTreeSpec extends AnyFunSpec with Matchers {
  describe("A RoseTree") {
    it("should offer a toString that gives the value only") {
      val irt = new RoseTree[Int] {
        val value: Int = 42;

        def shrinks = LazyListOrStream.empty
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
    it("should offer a shrinkSearch method") {
      import RoseTreeSpec._

      val rt = intRoseTree(72)
      rt.value shouldBe 72

      val (shrinks, _) = rt.shrinkSearch(i => (i < 12, None))
      shrinks should have length 1
      shrinks(0).value shouldBe 12
    }

    case class StatefulInt(value: Int) {
      var processed = false
    }

    class StatefulRoseTree(i: StatefulInt) extends RoseTree[StatefulInt] {
      def processed: Boolean = i.processed
      lazy val shrinksRoseTrees: LazyListOrStream[StatefulRoseTree] = {
        if (value.value == 0)
          LazyListOrStream.empty
        else {
          val half: Int = value.value / 2
          val minusOne = if (value.value > 0) value.value - 1 else value.value + 1
          LazyListOrStream(new StatefulRoseTree(new StatefulInt(half)), new StatefulRoseTree(new StatefulInt(minusOne)))
        }
      }
      val value: StatefulInt = i

      def shrinks: LazyListOrStream[RoseTree[StatefulInt]] = shrinksRoseTrees
    }

    case class StatefulBoolean(value: Boolean) {
      var processed = false
    }

    class StatefulBooleanRoseTree(b: StatefulBoolean) extends RoseTree[StatefulBoolean] {
      def processed: Boolean = b.processed
      lazy val shrinksRoseTrees: LazyListOrStream[StatefulBooleanRoseTree] = {
        if (value.value == false)
          LazyListOrStream.empty
        else 
          LazyListOrStream(new StatefulBooleanRoseTree(StatefulBoolean(false)))
      }
      val value: StatefulBoolean = b

      def shrinks: LazyListOrStream[RoseTree[StatefulBoolean]] = shrinksRoseTrees
    }

    it("should offer a shrinkSearch method that follows the 'depth-first' algo") {
      
      val rt = new StatefulRoseTree(StatefulInt(72))
      rt.value.value shouldBe 72

      def processFun(i: StatefulInt): (Boolean, Option[String]) = {
        i.processed = true
        (i.value < 12, None)
      }

      val (rtRes, _) = processFun(rt.value)
      rtRes shouldBe false
      
      val (shrinks, _) = rt.shrinkSearch(processFun)
      shrinks should have length 1
      shrinks(0).value.value shouldBe 12

      /*
         72 // This one fails, we'll shrink next level of depth first
            36 // This one fails, we'll shrink next level of depth first
               18  
                  9 // This one does not fail, so we won't shrink, will try its sibling instead
                  17
                     8 // This one does not fail, so we won't shrink, will try its sibling instead
                     16
                        8 // This one won't be processed as it is processed before     
                        15
                           7
                           14
                              7 // This one won't be processed as it is processed before  
                              13
                                 6
                                 12
                                    6  // This one won't be processed as it is processed before
                                    11
               35 // We won't touch this at all as its previous sibling is failing
            71 // We won't touch this at all as its previous sibling is failing
      */

      rt.processed shouldBe true

      val lvl2Node36 = rt.shrinksRoseTrees(0)
      val lvl2Node71 = rt.shrinksRoseTrees(1)

      lvl2Node36.processed shouldBe true
      lvl2Node36.value.value shouldBe 36
      val (lvl2Node36Res, _) = processFun(lvl2Node36.value)
      lvl2Node36Res shouldBe false
      lvl2Node71.processed shouldBe false
      lvl2Node71.value.value shouldBe 71

      val lvl3Node18 = lvl2Node36.shrinksRoseTrees(0)
      val lvl3Node35 = lvl2Node36.shrinksRoseTrees(1)

      lvl3Node18.processed shouldBe true
      lvl3Node18.value.value shouldBe 18
      val (lvl3Node18Res, _) = processFun(lvl3Node18.value)
      lvl3Node18Res shouldBe false
      lvl3Node35.processed shouldBe false
      lvl3Node35.value.value shouldBe 35

      val lvl4Node9 = lvl3Node18.shrinksRoseTrees(0)
      val lvl4Node17 = lvl3Node18.shrinksRoseTrees(1)

      lvl4Node9.processed shouldBe true
      lvl4Node9.value.value shouldBe 9
      lvl4Node17.processed shouldBe true
      lvl4Node17.value.value shouldBe 17
      val (lvl4Node17Res, _) = processFun(lvl4Node17.value)
      lvl4Node17Res shouldBe false

      val lvl5Node8 = lvl4Node17.shrinksRoseTrees(0)
      val lvl5Node16 = lvl4Node17.shrinksRoseTrees(1)

      lvl5Node8.processed shouldBe true
      lvl5Node8.value.value shouldBe 8
      lvl5Node16.processed shouldBe true
      lvl5Node16.value.value shouldBe 16
      val (lvl5Node16Res, _) = processFun(lvl5Node16.value)
      lvl5Node16Res shouldBe false

      val lvl6Node8 = lvl5Node16.shrinksRoseTrees(0)
      val lvl6Node15 = lvl5Node16.shrinksRoseTrees(1)

      lvl6Node8.processed shouldBe true  // This should be processed even though 8 has been processed before.
      lvl6Node8.value.value shouldBe 8
      lvl6Node15.processed shouldBe true
      lvl6Node15.value.value shouldBe 15
      val (lvl6Node15Res, _) = processFun(lvl6Node15.value)
      lvl6Node15Res shouldBe false

      val lvl7Node7 = lvl6Node15.shrinksRoseTrees(0)
      val lvl7Node14 = lvl6Node15.shrinksRoseTrees(1)

      lvl7Node7.processed shouldBe true
      lvl7Node7.value.value shouldBe 7
      lvl7Node14.processed shouldBe true
      lvl7Node14.value.value shouldBe 14
      val (lvl7Node14Res, _) = processFun(lvl7Node14.value)
      lvl7Node14Res shouldBe false

      val lvl8Node7 = lvl7Node14.shrinksRoseTrees(0)
      val lvl8Node13 = lvl7Node14.shrinksRoseTrees(1)

      lvl8Node7.processed shouldBe true  // This should be processed even though 8 has been processed before.
      lvl8Node7.value.value shouldBe 7
      lvl8Node13.processed shouldBe true
      lvl8Node13.value.value shouldBe 13
      val (lvl8Node13Res, _) = processFun(lvl8Node13.value)
      lvl8Node13Res shouldBe false

      val lvl9Node6 = lvl8Node13.shrinksRoseTrees(0)
      val lvl9Node12 = lvl8Node13.shrinksRoseTrees(1)

      lvl9Node6.processed shouldBe true
      lvl9Node6.value.value shouldBe 6
      lvl9Node12.processed shouldBe true
      lvl9Node12.value.value shouldBe 12
      val (lvl9Node12Res, _) = processFun(lvl9Node12.value)
      lvl9Node12Res shouldBe false

      val lvl10Node6 = lvl9Node12.shrinksRoseTrees(0)
      val lvl10Node11 = lvl9Node12.shrinksRoseTrees(1)

      lvl10Node6.processed shouldBe true  // This should be processed even though 8 has been processed before.
      lvl10Node6.value.value shouldBe 6
      lvl10Node11.processed shouldBe true
      lvl10Node11.value.value shouldBe 11
      val (lvl10Node11Res, _) = processFun(lvl10Node11.value)
      lvl10Node11Res shouldBe true
    }
  }
  describe("A Rose") {
    it("should have a toString that gives the value") {
      Rose(42).toString shouldBe "Rose(42)"
    }
  }
  describe("RoseTree companion object") {
    it("should offer a map2 function that combines 2 RoseTree") {
      import RoseTreeSpec._

      val rtOfInt = intRoseTree(2)
      val rtOfBoolean = booleanRoseTree(true)
      val rtOfIntBoolean = RoseTree.map2(rtOfInt, rtOfBoolean) { case (i, b) => (i, b) }
      val shrinks = rtOfIntBoolean.shrinks
      shrinks.length shouldBe 3
      shrinks(0).value shouldBe (1, true)
      shrinks(1).value shouldBe (0, true)
      shrinks(2).value shouldBe (2, false)
    }
  }
}

object RoseTreeSpec {
  def intRoseTree(i: Int): RoseTree[Int] =
    new RoseTree[Int] {
      val value: Int = i

      def shrinks: LazyListOrStream[RoseTree[Int]] = {
        val roseTrees: LazyListOrStream[RoseTree[Int]] = toLazyListOrStream(if (value > 0) (0 to value - 1).reverse.map(x => intRoseTree(x)) else LazyListOrStream.empty)
        roseTrees
      }
    }

  def charRoseTree(c: Char): RoseTree[Char] =
    new RoseTree[Char] {
      val value: Char = c
      def shrinks: LazyListOrStream[RoseTree[Char]] = {
        val userFriendlyChars = "abcdefghikjlmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        if (userFriendlyChars.indexOf(c) >= 0) LazyListOrStream.empty
        else toLazyListOrStream(userFriendlyChars).map(c => Rose(c))
      }
    }

  def booleanRoseTree(i: Boolean): RoseTree[Boolean] =
    new RoseTree[Boolean] {
      val value: Boolean = i

      def shrinks: LazyListOrStream[RoseTree[Boolean]] = 
        if (i) LazyListOrStream(booleanRoseTree(false)) else LazyListOrStream.empty
    }  

  def unfold[a](rt: RoseTree[a], indent: String = ""): Unit = {
    println(s"$indent ${rt.value}")
    val roseTrees = rt.shrinks
    roseTrees.foreach(t => unfold(t, s"$indent  "))
  }
}

