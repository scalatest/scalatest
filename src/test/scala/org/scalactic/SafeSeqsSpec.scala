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

class SafeSeqsSpec extends Spec with Matchers with SafeSeqs {

  abstract class Fruit
  case class Apple(name: String) extends Fruit
  case class Orange(name: String) extends Fruit
  val mac = Apple("Mcintosh")
  val navel = Orange("Navel")

  object `The safeContains syntax should` {
    object `allow type checked containership tests` {
      def `on Array` {

        (Array(1, 2, 3) safeContains 1) shouldBe true
        (Array(1, 2, 3) safeContains 5) shouldBe false
        """Array(1, 2, 3) safeContains "1"""" shouldNot typeCheck

        Array(mac, navel) safeContains mac
      }
      def `on List` {

        (List(1, 2, 3) safeContains 1) shouldBe true
        (List(1, 2, 3) safeContains 5) shouldBe false
        """List(1, 2, 3) safeContains "1"""" shouldNot typeCheck

        List(mac, navel) safeContains mac
      }
      def `on Vector` {

        (Vector(1, 2, 3) safeContains 1) shouldBe true
        (Vector(1, 2, 3) safeContains 5) shouldBe false
        """Vector(1, 2, 3) safeContains "1"""" shouldNot typeCheck

        Vector(mac, navel) safeContains mac
      }
      def `on ListBuffer` {

        import scala.collection.mutable.ListBuffer

        (ListBuffer(1, 2, 3) safeContains 1) shouldBe true
        (ListBuffer(1, 2, 3) safeContains 5) shouldBe false
        """ListBuffer(1, 2, 3) safeContains "1"""" shouldNot typeCheck

        ListBuffer(mac, navel) safeContains mac
      }
    }
    object `allow type checked indexOf` {
      def `on Array` {

        Array(1, 2, 3).safeIndexOf(1) shouldBe 0
        Array(1, 2, 3).safeIndexOf(2) shouldBe 1
        Array(1, 2, 3).safeIndexOf(3) shouldBe 2
        Array(1, 2, 3).safeIndexOf(1, 0) shouldBe 0
        Array(1, 2, 3).safeIndexOf(1, 1) shouldBe -1
        Array(1, 2, 3).safeIndexOf(5) shouldBe -1
        """Array(1, 2, 3).safeIndexOf("1")""" shouldNot typeCheck

        Array(mac, navel).safeIndexOf(mac) shouldBe 0
        Array(mac, navel).safeIndexOf(navel) shouldBe 1
        Array(mac, navel).safeIndexOf(mac, 1) shouldBe -1
        Array(mac, navel).safeIndexOf(navel, 1) shouldBe 1
      }
      def `on List` {

        List(1, 2, 3).safeIndexOf(1) shouldBe 0
        List(1, 2, 3).safeIndexOf(2) shouldBe 1
        List(1, 2, 3).safeIndexOf(3) shouldBe 2
        List(1, 2, 3).safeIndexOf(1, 0) shouldBe 0
        List(1, 2, 3).safeIndexOf(1, 1) shouldBe -1
        List(1, 2, 3).safeIndexOf(5) shouldBe -1
        """List(1, 2, 3).safeIndexOf("1")""" shouldNot typeCheck

        List(mac, navel).safeIndexOf(mac) shouldBe 0
        List(mac, navel).safeIndexOf(navel) shouldBe 1
        List(mac, navel).safeIndexOf(mac, 1) shouldBe -1
        List(mac, navel).safeIndexOf(navel, 1) shouldBe 1
      }
      def `on Vector` {

        Vector(1, 2, 3).safeIndexOf(1) shouldBe 0
        Vector(1, 2, 3).safeIndexOf(2) shouldBe 1
        Vector(1, 2, 3).safeIndexOf(3) shouldBe 2
        Vector(1, 2, 3).safeIndexOf(1, 0) shouldBe 0
        Vector(1, 2, 3).safeIndexOf(1, 1) shouldBe -1
        Vector(1, 2, 3).safeIndexOf(5) shouldBe -1
        """Vector(1, 2, 3).safeIndexOf("1")""" shouldNot typeCheck

        Vector(mac, navel).safeIndexOf(mac) shouldBe 0
        Vector(mac, navel).safeIndexOf(navel) shouldBe 1
        Vector(mac, navel).safeIndexOf(mac, 1) shouldBe -1
        Vector(mac, navel).safeIndexOf(navel, 1) shouldBe 1
      }
      def `on ListBuffer` {

        import scala.collection.mutable.ListBuffer

        ListBuffer(1, 2, 3).safeIndexOf(1) shouldBe 0
        ListBuffer(1, 2, 3).safeIndexOf(2) shouldBe 1
        ListBuffer(1, 2, 3).safeIndexOf(3) shouldBe 2
        ListBuffer(1, 2, 3).safeIndexOf(1, 0) shouldBe 0
        ListBuffer(1, 2, 3).safeIndexOf(1, 1) shouldBe -1
        ListBuffer(1, 2, 3).safeIndexOf(5) shouldBe -1
        """ListBuffer(1, 2, 3).safeIndexOf("1")""" shouldNot typeCheck

        ListBuffer(mac, navel).safeIndexOf(mac) shouldBe 0
        ListBuffer(mac, navel).safeIndexOf(navel) shouldBe 1
        ListBuffer(mac, navel).safeIndexOf(mac, 1) shouldBe -1
        ListBuffer(mac, navel).safeIndexOf(navel, 1) shouldBe 1
      }
    }

    object `allow type checked lastIndexOf` {
      def `on Array` {

        Array(1, 2, 3).safeLastIndexOf(1) shouldBe 0
        Array(1, 2, 3).safeLastIndexOf(2) shouldBe 1
        Array(1, 2, 3).safeLastIndexOf(3) shouldBe 2
        Array(1, 2, 3).safeLastIndexOf(1, 0) shouldBe 0
        Array(1, 2, 3).safeLastIndexOf(3, 1) shouldBe -1
        Array(1, 2, 3).safeLastIndexOf(5) shouldBe -1
        """Array(1, 2, 3).safeLastIndexOf("1")""" shouldNot typeCheck

        Array(mac, navel).safeLastIndexOf(mac) shouldBe 0
        Array(mac, navel).safeLastIndexOf(navel) shouldBe 1
        Array(mac, navel).safeLastIndexOf(mac, 1) shouldBe 0
        Array(mac, navel).safeLastIndexOf(navel, 0) shouldBe -1
      }
      def `on List` {

        List(1, 2, 3).safeLastIndexOf(1) shouldBe 0
        List(1, 2, 3).safeLastIndexOf(2) shouldBe 1
        List(1, 2, 3).safeLastIndexOf(3) shouldBe 2
        List(1, 2, 3).safeLastIndexOf(1, 0) shouldBe 0
        List(1, 2, 3).safeLastIndexOf(3, 1) shouldBe -1
        List(1, 2, 3).safeLastIndexOf(5) shouldBe -1
        """List(1, 2, 3).safeLastIndexOf("1")""" shouldNot typeCheck

        List(mac, navel).safeLastIndexOf(mac) shouldBe 0
        List(mac, navel).safeLastIndexOf(navel) shouldBe 1
        List(mac, navel).safeLastIndexOf(mac, 1) shouldBe 0
        List(mac, navel).safeLastIndexOf(navel, 0) shouldBe -1
      }
      def `on Vector` {

        Vector(1, 2, 3).safeLastIndexOf(1) shouldBe 0
        Vector(1, 2, 3).safeLastIndexOf(2) shouldBe 1
        Vector(1, 2, 3).safeLastIndexOf(3) shouldBe 2
        Vector(1, 2, 3).safeLastIndexOf(1, 0) shouldBe 0
        Vector(1, 2, 3).safeLastIndexOf(3, 1) shouldBe -1
        Vector(1, 2, 3).safeLastIndexOf(5) shouldBe -1
        """Vector(1, 2, 3).safeLastIndexOf("1")""" shouldNot typeCheck

        Vector(mac, navel).safeLastIndexOf(mac) shouldBe 0
        Vector(mac, navel).safeLastIndexOf(navel) shouldBe 1
        Vector(mac, navel).safeLastIndexOf(mac, 1) shouldBe 0
        Vector(mac, navel).safeLastIndexOf(navel, 0) shouldBe -1
      }
      def `on ListBuffer` {

        import scala.collection.mutable.ListBuffer

        ListBuffer(1, 2, 3).safeLastIndexOf(1) shouldBe 0
        ListBuffer(1, 2, 3).safeLastIndexOf(2) shouldBe 1
        ListBuffer(1, 2, 3).safeLastIndexOf(3) shouldBe 2
        ListBuffer(1, 2, 3).safeLastIndexOf(1, 0) shouldBe 0
        ListBuffer(1, 2, 3).safeLastIndexOf(3, 1) shouldBe -1
        ListBuffer(1, 2, 3).safeLastIndexOf(5) shouldBe -1
        """ListBuffer(1, 2, 3).safeLastIndexOf("1")""" shouldNot typeCheck

        ListBuffer(mac, navel).safeLastIndexOf(mac) shouldBe 0
        ListBuffer(mac, navel).safeLastIndexOf(navel) shouldBe 1
        ListBuffer(mac, navel).safeLastIndexOf(mac, 1) shouldBe 0
        ListBuffer(mac, navel).safeLastIndexOf(navel, 0) shouldBe -1
      }
    }

    object `allow type checked indexOfSlice` {
      def `on Array` {

        Array(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3)) shouldBe 1
        Array(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3), 3) shouldBe -1
        Array(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3, 5), 3) shouldBe -1
        Array(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3, 5)) shouldBe -1
        Array(1, 2, 3, 4, 5).safeIndexOfSlice(List(5)) shouldBe 4
        Array(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
        Array(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
        Array(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe -1
        Array(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe 0

        """Array(1, 2, 3, 4, 5).safeIndexOfSlice(List("2", "3")) shouldBe 1""" shouldNot typeCheck

        Array("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3")) shouldBe 1
        Array("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3"), 3) shouldBe -1
        Array("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3", "5"), 3) shouldBe -1
        Array("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3", "5")) shouldBe -1
        Array("1", "2", "3", "4", "5").safeIndexOfSlice(List("5")) shouldBe 4
        Array("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5")) shouldBe 0
        Array("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), 0) shouldBe 0
        Array("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), 1) shouldBe -1
        Array("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), -1) shouldBe 0

        """Array("1", "2", "3", "4", "5").safeIndexOfSlice(List(2, 3)) shouldBe 1""" shouldNot typeCheck
      }
      def `on List` {

        List(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3)) shouldBe 1
        List(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3), 3) shouldBe -1
        List(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3, 5), 3) shouldBe -1
        List(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3, 5)) shouldBe -1
        List(1, 2, 3, 4, 5).safeIndexOfSlice(List(5)) shouldBe 4
        List(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
        List(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
        List(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe -1
        List(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe 0

        """List(1, 2, 3, 4, 5).safeIndexOfSlice(List("2", "3")) shouldBe 1""" shouldNot typeCheck

        List("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3")) shouldBe 1
        List("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3"), 3) shouldBe -1
        List("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3", "5"), 3) shouldBe -1
        List("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3", "5")) shouldBe -1
        List("1", "2", "3", "4", "5").safeIndexOfSlice(List("5")) shouldBe 4
        List("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5")) shouldBe 0
        List("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), 0) shouldBe 0
        List("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), 1) shouldBe -1
        List("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), -1) shouldBe 0

        """List("1", "2", "3", "4", "5").safeIndexOfSlice(List(2, 3)) shouldBe 1""" shouldNot typeCheck
      }
      def `on Vector` {

        Vector(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3)) shouldBe 1
        Vector(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3), 3) shouldBe -1
        Vector(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3, 5), 3) shouldBe -1
        Vector(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3, 5)) shouldBe -1
        Vector(1, 2, 3, 4, 5).safeIndexOfSlice(List(5)) shouldBe 4
        Vector(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
        Vector(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
        Vector(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe -1
        Vector(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe 0

        """Vector(1, 2, 3, 4, 5).safeIndexOfSlice(List("2", "3")) shouldBe 1""" shouldNot typeCheck

        Vector("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3")) shouldBe 1
        Vector("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3"), 3) shouldBe -1
        Vector("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3", "5"), 3) shouldBe -1
        Vector("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3", "5")) shouldBe -1
        Vector("1", "2", "3", "4", "5").safeIndexOfSlice(List("5")) shouldBe 4
        Vector("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5")) shouldBe 0
        Vector("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), 0) shouldBe 0
        Vector("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), 1) shouldBe -1
        Vector("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), -1) shouldBe 0

        """Vector("1", "2", "3", "4", "5").safeIndexOfSlice(List(2, 3)) shouldBe 1""" shouldNot typeCheck
      }
      def `on ListBuffer` {

        import scala.collection.mutable.ListBuffer

        ListBuffer(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3)) shouldBe 1
        ListBuffer(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3), 3) shouldBe -1
        ListBuffer(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3, 5), 3) shouldBe -1
        ListBuffer(1, 2, 3, 4, 5).safeIndexOfSlice(List(2, 3, 5)) shouldBe -1
        ListBuffer(1, 2, 3, 4, 5).safeIndexOfSlice(List(5)) shouldBe 4
        ListBuffer(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
        ListBuffer(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
        ListBuffer(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe -1
        ListBuffer(1, 2, 3, 4, 5).safeIndexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe 0

        """ListBuffer(1, 2, 3, 4, 5).safeIndexOfSlice(List("2", "3")) shouldBe 1""" shouldNot typeCheck

        ListBuffer("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3")) shouldBe 1
        ListBuffer("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3"), 3) shouldBe -1
        ListBuffer("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3", "5"), 3) shouldBe -1
        ListBuffer("1", "2", "3", "4", "5").safeIndexOfSlice(List("2", "3", "5")) shouldBe -1
        ListBuffer("1", "2", "3", "4", "5").safeIndexOfSlice(List("5")) shouldBe 4
        ListBuffer("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5")) shouldBe 0
        ListBuffer("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), 0) shouldBe 0
        ListBuffer("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), 1) shouldBe -1
        ListBuffer("1", "2", "3", "4", "5").safeIndexOfSlice(List("1", "2", "3", "4", "5"), -1) shouldBe 0

        """ListBuffer("1", "2", "3", "4", "5").safeIndexOfSlice(List(2, 3)) shouldBe 1""" shouldNot typeCheck
      }
    }

    object `allow type checked lastIndexOfSlice` {
      def `on Array` {

        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3)) shouldBe 1
        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3), 3) shouldBe 1
        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3, 5), 3) shouldBe -1
        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3, 5)) shouldBe -1
        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(5)) shouldBe 4
        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe 0
        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe -1
        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty) shouldBe 5
        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty, 6) shouldBe 5
        Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty, 4) shouldBe 4

        """Array(1, 2, 3, 4, 5).safeLastIndexOfSlice(List("2", "3")) shouldBe 1""" shouldNot typeCheck

        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3")) shouldBe 1
        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3"), 3) shouldBe 1
        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3", "5"), 3) shouldBe -1
        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3", "5")) shouldBe -1
        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("5")) shouldBe 4
        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5")) shouldBe 0
        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), 0) shouldBe 0
        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), 1) shouldBe 0
        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), -1) shouldBe -1
        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty) shouldBe 5
        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty, 6) shouldBe 5
        Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty, 4) shouldBe 4

        """Array("1", "2", "3", "4", "5").safeLastIndexOfSlice(List(2, 3)) shouldBe 1""" shouldNot typeCheck
      }
      def `on List` {

        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3)) shouldBe 1
        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3), 3) shouldBe 1
        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3, 5), 3) shouldBe -1
        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3, 5)) shouldBe -1
        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(5)) shouldBe 4
        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe 0
        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe -1
        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty[Int]) shouldBe 5
        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty[Int], 6) shouldBe 5
        List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty[Int], 4) shouldBe 4

        """List(1, 2, 3, 4, 5).safeLastIndexOfSlice(List("2", "3")) shouldBe 1""" shouldNot typeCheck

        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3")) shouldBe 1
        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3"), 3) shouldBe 1
        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3", "5"), 3) shouldBe -1
        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3", "5")) shouldBe -1
        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("5")) shouldBe 4
        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5")) shouldBe 0
        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), 0) shouldBe 0
        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), 1) shouldBe 0
        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), -1) shouldBe -1
        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty[String]) shouldBe 5
        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty[String], 6) shouldBe 5
        List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty[String], 4) shouldBe 4

        """List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List(2, 3)) shouldBe 1""" shouldNot typeCheck
      }
      def `on Vector` {

        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3)) shouldBe 1
        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3), 3) shouldBe 1
        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3, 5), 3) shouldBe -1
        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3, 5)) shouldBe -1
        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(5)) shouldBe 4
        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe 0
        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe -1
        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty[Int]) shouldBe 5
        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty[Int], 6) shouldBe 5
        Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty[Int], 4) shouldBe 4

        """Vector(1, 2, 3, 4, 5).safeLastIndexOfSlice(List("2", "3")) shouldBe 1""" shouldNot typeCheck

        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3")) shouldBe 1
        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3"), 3) shouldBe 1
        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3", "5"), 3) shouldBe -1
        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3", "5")) shouldBe -1
        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("5")) shouldBe 4
        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5")) shouldBe 0
        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), 0) shouldBe 0
        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), 1) shouldBe 0
        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), -1) shouldBe -1
        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty[String]) shouldBe 5
        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty[String], 6) shouldBe 5
        Vector("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty[String], 4) shouldBe 4

        """List("1", "2", "3", "4", "5").safeLastIndexOfSlice(List(2, 3)) shouldBe 1""" shouldNot typeCheck
      }
      def `on ListBuffer` {

        import scala.collection.mutable.ListBuffer

        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3)) shouldBe 1
        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3), 3) shouldBe 1
        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3, 5), 3) shouldBe -1
        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(2, 3, 5)) shouldBe -1
        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(5)) shouldBe 4
        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5)) shouldBe 0
        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), 0) shouldBe 0
        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), 1) shouldBe 0
        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List(1, 2, 3, 4, 5), -1) shouldBe -1
        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty) shouldBe 5
        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty, 6) shouldBe 5
        ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List.empty, 4) shouldBe 4

        """ListBuffer(1, 2, 3, 4, 5).safeLastIndexOfSlice(List("2", "3")) shouldBe 1""" shouldNot typeCheck

        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3")) shouldBe 1
        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3"), 3) shouldBe 1
        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3", "5"), 3) shouldBe -1
        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("2", "3", "5")) shouldBe -1
        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("5")) shouldBe 4
        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5")) shouldBe 0
        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), 0) shouldBe 0
        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), 1) shouldBe 0
        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List("1", "2", "3", "4", "5"), -1) shouldBe -1
        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty) shouldBe 5
        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty, 6) shouldBe 5
        ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List.empty, 4) shouldBe 4

        """ListBuffer("1", "2", "3", "4", "5").safeLastIndexOfSlice(List(2, 3)) shouldBe 1""" shouldNot typeCheck
      }
    }
  }
}

