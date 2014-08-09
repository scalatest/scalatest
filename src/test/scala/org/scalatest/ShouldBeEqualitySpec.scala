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

import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import org.scalactic.Equality
import org.scalactic.TripleEquals
import Matchers._

class ShouldBeEqualitySpec extends Spec {

  object `the should be syntax should use the appropriate Equality type class` {
    def `for Any` {
      () should be (())
      () shouldBe ()
      () should not be (7)
      implicit val e = new Equality[Unit] {
        def areEqual(a: Unit, b: Any): Boolean = a != b
      }
      () should not be (())
      () should be (7)
      () shouldBe 7
    }
    def `for String` {
      "hi" should be ("hi")
      "hi" shouldBe "hi"
      "hi" should not be ("ho")
      implicit val e = new Equality[String] {
        def areEqual(a: String, b: Any): Boolean = a != b
      }
      "hi" should not be ("hi")
      "hi" should be ("ho")
      "hi" shouldBe "ho"
    }
    def `for Numeric` {
      3 should be (3)
      3 shouldBe 3
      3 should not be (4)
      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      3 should not be (3)
      3 should be (4)
      3 shouldBe 4
    }
    object `for Map` {
      def `with default beity` {
        Map("I" -> 1, "II" -> 2) should be (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) shouldBe Map("I" -> 1, "II" -> 2)
        Map("I" -> 1, "II" -> 2) should not be (Map("one" -> 1, "two" -> 2))
        implicit val e = new Equality[GenMap[String, Int]] {
          def areEqual(a: GenMap[String, Int], b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should be (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) shouldBe Map("I" -> 1, "II" -> 2)
        Map("I" -> 1, "II" -> 2) should not be (Map("one" -> 1, "two" -> 2))
      }
      def `with inferred GenMap beity` {
        implicit def travEq[T <: GenMap[String, Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should not be (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should be (Map("one" -> 1, "two" -> 2))
        Map("I" -> 1, "II" -> 2) shouldBe Map("one" -> 1, "two" -> 2)
      }
      def `with specific Map beity` {
        implicit val e = new Equality[Map[String, Int]] {
          def areEqual(a: Map[String, Int], b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should not be (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should be (Map("one" -> 1, "two" -> 2))
        Map("I" -> 1, "II" -> 2) shouldBe Map("one" -> 1, "two" -> 2)
      }
      def `with both GenMap and specific Map beity, though I don't know why this compiles` {
        implicit val e = new Equality[GenMap[String, Int]] {
          def areEqual(a: GenMap[String, Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Map[String, Int]] { // Should pick the most specific one
          def areEqual(a: Map[String, Int], b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should not be (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should be (Map("one" -> 1, "two" -> 2))
        Map("I" -> 1, "II" -> 2) shouldBe Map("one" -> 1, "two" -> 2)
      }
      def `with both inferred GenMap and specific Map beity` {
        implicit def travEq[T <: GenMap[String, Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[Map[String, Int]] { // Should pick the most specific one
          def areEqual(a: Map[String, Int], b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should not be (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should be (Map("one" -> 1, "two" -> 2))
        Map("I" -> 1, "II" -> 2) shouldBe Map("one" -> 1, "two" -> 2)
      }
    }
    object `for mutable.Map` {
      def `with default beity` {
        mutable.Map("I" -> 1, "II" -> 2) should be (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) shouldBe mutable.Map("I" -> 1, "II" -> 2)
        mutable.Map("I" -> 1, "II" -> 2) should not be (mutable.Map("one" -> 1, "two" -> 2))
        implicit val e = new Equality[GenMap[String, Int]] {
          def areEqual(a: GenMap[String, Int], b: Any): Boolean = a != b
        }
        mutable.Map("I" -> 1, "II" -> 2) should be (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) shouldBe mutable.Map("I" -> 1, "II" -> 2)
        mutable.Map("I" -> 1, "II" -> 2) should not be (mutable.Map("one" -> 1, "two" -> 2))
      }
      def `with inferred GenMap beity` {
        implicit def travEq[T <: GenMap[String, Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        mutable.Map("I" -> 1, "II" -> 2) should not be (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) should be (mutable.Map("one" -> 1, "two" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) shouldBe mutable.Map("one" -> 1, "two" -> 2)
      }
      def `with specific mutable.Map beity` {
        implicit val e = new Equality[mutable.Map[String, Int]] {
          def areEqual(a: mutable.Map[String, Int], b: Any): Boolean = a != b
        }
        mutable.Map("I" -> 1, "II" -> 2) should not be (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) should be (mutable.Map("one" -> 1, "two" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) shouldBe mutable.Map("one" -> 1, "two" -> 2)
      }
      def `with both GenMap and specific mutable.Map beity, though I don't know why this compiles` {
        implicit val e = new Equality[GenMap[String, Int]] {
          def areEqual(a: GenMap[String, Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[mutable.Map[String, Int]] { // Should pick the most specific one
          def areEqual(a: mutable.Map[String, Int], b: Any): Boolean = a != b
        }
        mutable.Map("I" -> 1, "II" -> 2) should not be (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) should be (mutable.Map("one" -> 1, "two" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) shouldBe mutable.Map("one" -> 1, "two" -> 2)
      }
      def `with both inferred GenMap and specific mutable.Map beity` {
        implicit def travEq[T <: GenMap[String, Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[mutable.Map[String, Int]] { // Should pick the most specific one
          def areEqual(a: mutable.Map[String, Int], b: Any): Boolean = a != b
        }
        mutable.Map("I" -> 1, "II" -> 2) should not be (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) should be (mutable.Map("one" -> 1, "two" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) shouldBe mutable.Map("one" -> 1, "two" -> 2)
      }
    }
    def `for AnyRef` {
      case class Person(name: String)
      Person("Joe") should be (Person("Joe"))
      Person("Joe") shouldBe Person("Joe")
      Person("Joe") should not be (Person("Sally"))
      implicit val e = new Equality[Person] {
        def areEqual(a: Person, b: Any): Boolean = a != b
      }
      Person("Joe") should not be (Person("Joe"))
      Person("Joe") should be (Person("Sally"))
      Person("Joe") shouldBe Person("Sally")
    }
    object `for Traversable` {
      def `with default beity` {
        Set(1, 2, 3) should be (Set(1, 2, 3))
        Set(1, 2, 3) shouldBe Set(1, 2, 3)
        Set(1, 2, 3) should not be (Set(1, 2, 4))

        implicit val e = new Equality[GenTraversable[Int]] {
          def areEqual(a: GenTraversable[Int], b: Any): Boolean = a != b
        }

        Set(1, 2, 3) should be (Set(1, 2, 3))
        Set(1, 2, 3) shouldBe Set(1, 2, 3)
        Set(1, 2, 3) should not be (Set(1, 2, 4))
      }
      def `with inferred GenTraversable beity` {
        implicit def travEq[T <: GenTraversable[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should not be (Set(1, 2, 3))
        Set(1, 2, 3) should be (Set(1, 2, 4))
        Set(1, 2, 3) shouldBe Set(1, 2, 4)
      }
      def `with specific Traversable beity` {
        implicit val e = new Equality[Set[Int]] {
          def areEqual(a: Set[Int], b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should not be (Set(1, 2, 3))
        Set(1, 2, 3) should be (Set(1, 2, 4))
        Set(1, 2, 3) shouldBe Set(1, 2, 4)
      }
      def `with both GenTraversable and specific Traversable beity` {
        implicit val e = new Equality[GenTraversable[Int]] {
          def areEqual(a: GenTraversable[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Set[Int]] { // Should pick the most specific one
          def areEqual(a: Set[Int], b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should not be (Set(1, 2, 3))
        Set(1, 2, 3) should be (Set(1, 2, 4))
        Set(1, 2, 3) shouldBe Set(1, 2, 4)
      }
      def `with both inferred GenTraversable and specific Traversable beity` {
        implicit def travEq[T <: GenTraversable[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[Set[Int]] { // Should pick the most specific one
          def areEqual(a: Set[Int], b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should not be (Set(1, 2, 3))
        Set(1, 2, 3) shouldBe Set(1, 2, 4)
      }
    }
    object `for mutable.Traversable` {
      def `with default beity` {
        mutable.Set(1, 2, 3) should be (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) shouldBe mutable.Set(1, 2, 3)
        mutable.Set(1, 2, 3) should not be (mutable.Set(1, 2, 4))

        implicit val e = new Equality[GenTraversable[Int]] {
          def areEqual(a: GenTraversable[Int], b: Any): Boolean = a != b
        }

        mutable.Set(1, 2, 3) should be (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) shouldBe mutable.Set(1, 2, 3)
        mutable.Set(1, 2, 3) should not be (mutable.Set(1, 2, 4))
      }
      def `with inferred GenTraversable beity` {
        implicit def travEq[T <: GenTraversable[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        mutable.Set(1, 2, 3) should not be (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) should be (mutable.Set(1, 2, 4))
        mutable.Set(1, 2, 3) shouldBe mutable.Set(1, 2, 4)
      }
      def `with specific mutable.Traversable beity` {
        implicit val e = new Equality[mutable.Set[Int]] {
          def areEqual(a: mutable.Set[Int], b: Any): Boolean = a != b
        }
        mutable.Set(1, 2, 3) should not be (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) should be (mutable.Set(1, 2, 4))
        mutable.Set(1, 2, 3) shouldBe mutable.Set(1, 2, 4)
      }
      def `with both GenTraversable and specific Traversable beity` {
        implicit val e = new Equality[GenTraversable[Int]] {
          def areEqual(a: GenTraversable[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[mutable.Set[Int]] { // Should pick the most specific one
          def areEqual(a: mutable.Set[Int], b: Any): Boolean = a != b
        }
        mutable.Set(1, 2, 3) should not be (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) should be (mutable.Set(1, 2, 4))
        mutable.Set(1, 2, 3) shouldBe mutable.Set(1, 2, 4)
      }
      def `with both inferred GenTraversable and specific Traversable beity` {
        implicit def travEq[T <: GenTraversable[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[mutable.Set[Int]] { // Should pick the most specific one
          def areEqual(a: mutable.Set[Int], b: Any): Boolean = a != b
        }
        mutable.Set(1, 2, 3) should not be (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) should be (mutable.Set(1, 2, 4))
        mutable.Set(1, 2, 3) shouldBe mutable.Set(1, 2, 4)
      }
    }
    object `for Java Collection` {

      val javaSet123: java.util.Set[Int] = new java.util.HashSet
      javaSet123.add(1)
      javaSet123.add(2)
      javaSet123.add(3)

      val javaSet124: java.util.Set[Int] = new java.util.HashSet
      javaSet124.add(1)
      javaSet124.add(2)
      javaSet124.add(4)

      def `with default beity` {
        javaSet123 should be (javaSet123)
        javaSet123 shouldBe javaSet123
        javaSet123 should not be (javaSet124)
        implicit val e = new Equality[java.util.Collection[Int]] {
          def areEqual(a: java.util.Collection[Int], b: Any): Boolean = a != b
        }
        javaSet123 should be (javaSet123)
        javaSet123 shouldBe javaSet123
        javaSet123 should not be (javaSet124)
      }

      def `with inferred Collection beity` {
        implicit def travEq[T <: java.util.Collection[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        javaSet123 should not be (javaSet123)
        javaSet123 should be (javaSet124)
        javaSet123 shouldBe javaSet124
      }

      def `with specific Collection beity` {
        implicit val e = new Equality[java.util.Set[Int]] {
          def areEqual(a: java.util.Set[Int], b: Any): Boolean = a != b
        }
        javaSet123 should not be (javaSet123)
        javaSet123 should be (javaSet124)
        javaSet123 shouldBe javaSet124
      }

      def `with both Collection and specific Collection beity` {
        implicit val e = new Equality[java.util.Collection[Int]] {
          def areEqual(a: java.util.Collection[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[java.util.Set[Int]] { // Should pick the most specific one
          def areEqual(a: java.util.Set[Int], b: Any): Boolean = a != b
        }
        javaSet123 should not be (javaSet123)
        javaSet123 should be (javaSet124)
        javaSet123 shouldBe javaSet124
      }
      def `with both inferred Collection and specific Collection beity` {
        implicit def travEq[T <: java.util.Collection[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[java.util.Set[Int]] { // Should pick the most specific one
          def areEqual(a: java.util.Set[Int], b: Any): Boolean = a != b
        }
        javaSet123 should not be (javaSet123)
        javaSet123 should be (javaSet124)
        javaSet123 shouldBe javaSet124
      }
    }

    object `for Java Map` {

      val javaMap123: java.util.HashMap[String, Int] = new java.util.HashMap
      javaMap123.put("one",1)
      javaMap123.put("two", 2)
      javaMap123.put("three", 3)

      val javaMap124: java.util.HashMap[String, Int] = new java.util.HashMap
      javaMap124.put("one",1)
      javaMap124.put("two", 2)
      javaMap124.put("four", 4)

      def `with default beity` {
        javaMap123 should be (javaMap123)
        javaMap123 shouldBe javaMap123
        javaMap123 should not be (javaMap124)
        implicit val e = new Equality[java.util.Map[String, Int]] {
          def areEqual(a: java.util.Map[String, Int], b: Any): Boolean = a != b
        }
        javaMap123 should be (javaMap123)
        javaMap123 shouldBe javaMap123
        javaMap123 should not be (javaMap124)
      }

      def `with inferred Map beity` {
        implicit def travEq[T <: java.util.Map[String, Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        javaMap123 should not be (javaMap123)
        javaMap123 should be (javaMap124)
        javaMap123 shouldBe javaMap124
      }

      def `with specific HashMap beity` {
        implicit val e = new Equality[java.util.HashMap[String, Int]] {
          def areEqual(a: java.util.HashMap[String, Int], b: Any): Boolean = a != b
        }
        javaMap123 should not be (javaMap123)
        javaMap123 should be (javaMap124)
        javaMap123 shouldBe javaMap124
      }

      def `with both Map and specific HashMap beity` {
        implicit val e = new Equality[java.util.Map[String, Int]] {
          def areEqual(a: java.util.Map[String, Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[java.util.HashMap[String, Int]] { // Should pick this because it is an exact match
          def areEqual(a: java.util.HashMap[String, Int], b: Any): Boolean = a != b
        }
        javaMap123 should not be (javaMap123)
        javaMap123 should be (javaMap124)
        javaMap123 shouldBe javaMap124
      }
      def `with both inferred Map and specific HashMap beity` {
        implicit def travEq[T <: java.util.Map[String, Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[java.util.HashMap[String, Int]] { // Should pick the most specific one
          def areEqual(a: java.util.HashMap[String, Int], b: Any): Boolean = a != b
        }
        javaMap123 should not be (javaMap123)
        javaMap123 should be (javaMap124)
        javaMap123 shouldBe javaMap124
      }
    }

    object `for Seq` {
      def `with default beity` {
        Vector(1, 2, 3) should be (Vector(1, 2, 3))
        Vector(1, 2, 3) shouldBe Vector(1, 2, 3)
        Vector(1, 2, 3) should not be (Vector(1, 2, 4))
      }
      def `with inferred GenSeq beity` {
        implicit def travEq[T <: GenSeq[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should not be (Vector(1, 2, 3))
        Vector(1, 2, 3) should be (Vector(1, 2, 4))
        Vector(1, 2, 3) shouldBe Vector(1, 2, 4)
      }
      def `with specific Seq beity` {
        implicit val e = new Equality[Vector[Int]] {
          def areEqual(a: Vector[Int], b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should not be (Vector(1, 2, 3))
        Vector(1, 2, 3) should be (Vector(1, 2, 4))
        Vector(1, 2, 3) shouldBe Vector(1, 2, 4)
      }
      def `with both GenSeq and specific Seq beity` {
        implicit val e = new Equality[GenSeq[Int]] {
          def areEqual(a: GenSeq[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Vector[Int]] { // Should pick the exact one
          def areEqual(a: Vector[Int], b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should not be (Vector(1, 2, 3))
        Vector(1, 2, 3) should be (Vector(1, 2, 4))
        Vector(1, 2, 3) shouldBe Vector(1, 2, 4)
      }
      def `with both inferred GenSeq and specific Seq beity` {
        implicit def travEq[T <: GenSeq[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Vector[Int]] { // Should pick the exact one
          def areEqual(a: Vector[Int], b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should not be (Vector(1, 2, 3))
        Vector(1, 2, 3) should be (Vector(1, 2, 4))
        Vector(1, 2, 3) shouldBe Vector(1, 2, 4)
      }
    }
    object `for mutable.Seq` {
      def `with default beity` {
        ListBuffer(1, 2, 3) should be (ListBuffer(1, 2, 3))
        ListBuffer(1, 2, 3) shouldBe ListBuffer(1, 2, 3)
        ListBuffer(1, 2, 3) should not be (ListBuffer(1, 2, 4))
      }
      def `with inferred GenSeq beity` {
        implicit def travEq[T <: GenSeq[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        ListBuffer(1, 2, 3) should not be (ListBuffer(1, 2, 3))
        ListBuffer(1, 2, 3) should be (ListBuffer(1, 2, 4))
        ListBuffer(1, 2, 3) shouldBe ListBuffer(1, 2, 4)
      }
      def `with specific Seq beity` {
        implicit val e = new Equality[ListBuffer[Int]] {
          def areEqual(a: ListBuffer[Int], b: Any): Boolean = a != b
        }
        ListBuffer(1, 2, 3) should not be (ListBuffer(1, 2, 3))
        ListBuffer(1, 2, 3) should be (ListBuffer(1, 2, 4))
        ListBuffer(1, 2, 3) shouldBe ListBuffer(1, 2, 4)
      }
      def `with both GenSeq and specific Seq beity` {
        implicit val e = new Equality[GenSeq[Int]] {
          def areEqual(a: GenSeq[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[ListBuffer[Int]] { // Should pick the exact one
          def areEqual(a: ListBuffer[Int], b: Any): Boolean = a != b
        }
        ListBuffer(1, 2, 3) should not be (ListBuffer(1, 2, 3))
        ListBuffer(1, 2, 3) should be (ListBuffer(1, 2, 4))
        ListBuffer(1, 2, 3) shouldBe ListBuffer(1, 2, 4)
      }
      def `with both inferred GenSeq and specific Seq beity` {
        implicit def travEq[T <: GenSeq[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[ListBuffer[Int]] { // Should pick the exact one
          def areEqual(a: ListBuffer[Int], b: Any): Boolean = a != b
        }
        ListBuffer(1, 2, 3) should not be (ListBuffer(1, 2, 3))
        ListBuffer(1, 2, 3) shouldBe ListBuffer(1, 2, 4)
      }
    }
    def `for Array` {
      Array(1, 2, 3) should be (Array(1, 2, 3))
      Array(1, 2, 3) shouldBe Array(1, 2, 3)
      Array(1, 2, 3) should not be (Array(1, 2, 4))
      implicit val e = new Equality[Array[Int]] {
        def areEqual(a: Array[Int], b: Any): Boolean = a.deep != b.asInstanceOf[Array[Int]].deep
      }
      Array(1, 2, 3) should not be (Array(1, 2, 3))
      Array(1, 2, 3) should be (Array(1, 2, 4))
      Array(1, 2, 3) shouldBe Array(1, 2, 4)
    }
    object `for Java List` {

      val javaList123: java.util.List[Int] = new java.util.ArrayList
      javaList123.add(1)
      javaList123.add(2)
      javaList123.add(3)

      val javaList124: java.util.List[Int] = new java.util.ArrayList
      javaList124.add(1)
      javaList124.add(2)
      javaList124.add(4)
      
      def `with default beity` {
        javaList123 should be (javaList123)
        javaList123 shouldBe javaList123
        javaList123 should not be (javaList124)
        implicit val e = new Equality[java.util.Collection[Int]] {
          def areEqual(a: java.util.Collection[Int], b: Any): Boolean = a != b
        }
        javaList123 should be (javaList123)
        javaList123 shouldBe javaList123
        javaList123 should not be (javaList124)
      }
      def `with inferred java.util.Collection beity` {
        implicit def travEq[T <: java.util.Collection[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        javaList123 should not be (javaList123)
        javaList123 should be (javaList124)
        javaList123 shouldBe javaList124
      }
      def `with specific java.util.List beity` {
        implicit val e = new Equality[java.util.List[Int]] {
          def areEqual(a: java.util.List[Int], b: Any): Boolean = a != b
        }
        javaList123 should not be (javaList123)
        javaList123 should be (javaList124)
        javaList123 shouldBe javaList124
      }
      def `with both java.util.Collection and java.util.List beity` {
        implicit val e = new Equality[java.util.Collection[Int]] {
          def areEqual(a: java.util.Collection[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[java.util.List[Int]] { // Should pick the exact one
          def areEqual(a: java.util.List[Int], b: Any): Boolean = a != b
        }
        javaList123 should not be (javaList123)
        javaList123 should be (javaList124)
        javaList123 shouldBe javaList124
      }
      def `with both inferred java.util.List and specific java.util.List beity` {
        implicit def travEq[T <: java.util.List[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[java.util.List[Int]] { // Should pick the exact one
          def areEqual(a: java.util.List[Int], b: Any): Boolean = a != b
        }
        javaList123 should not be (javaList123)
        javaList123 should be (javaList124)
        javaList123 shouldBe javaList124
      }
    }
  }
}

