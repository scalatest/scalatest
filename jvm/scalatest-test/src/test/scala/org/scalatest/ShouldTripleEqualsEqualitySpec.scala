/*
 * Copyright 2001-2024 Artima, Inc.
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
import org.scalactic.ColCompatHelper.{Iterable, IterableOnce}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import org.scalactic.Equality

import org.scalactic.ArrayHelper.deep
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldTripleEqualsEqualitySpec extends AnyFunSpec with NonImplicitAssertions {

  describe("the should === operator should use the appropriate Equality type class") {
    it("for Any") {
      () should === (())
      () should !== (7)

      {
        implicit val e = new Equality[Unit] {
          def areEqual(a: Unit, b: Any): Boolean = a != b
        }
        () should !== (())
        () should === (7)
      }
    }
    it("for String") {
      "hi" should === ("hi")
      "hi" should !== ("ho")

      {
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        "hi" should !== ("hi")
        "hi" should === ("ho")
      }
    }
    it("for Numeric") {
      3 should === (3)
      3 should !== (4)

      {
        implicit val e = new Equality[Int] {
          def areEqual(a: Int, b: Any): Boolean = a != b
        }
        3 should !== (3)
        3 should === (4)
      }
    }
    describe("for Map") {
      it("with default equality") {
        Map("I" -> 1, "II" -> 2) should === (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should !== (Map("one" -> 1, "two" -> 2))
        implicit val e = new Equality[GenMap[String, Int]] {
          def areEqual(a: GenMap[String, Int], b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should === (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should !== (Map("one" -> 1, "two" -> 2))
      }
      it("with inferred GenMap equality") {
        implicit def travEq[T <: GenMap[String, Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should !== (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should === (Map("one" -> 1, "two" -> 2))
      }
      it("with specific Map equality") {
        implicit val e = new Equality[Map[String, Int]] {
          def areEqual(a: Map[String, Int], b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should !== (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should === (Map("one" -> 1, "two" -> 2))
      }
      it("with both GenMap and specific Map equality, though I don't know why this compiles") {
        implicit val e = new Equality[GenMap[String, Int]] {
          def areEqual(a: GenMap[String, Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Map[String, Int]] { // Should pick the most specific one
          def areEqual(a: Map[String, Int], b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should !== (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should === (Map("one" -> 1, "two" -> 2))
      }
      it("with both inferred GenMap and specific Map equality") {
        implicit def travEq[T <: GenMap[String, Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[Map[String, Int]] { // Should pick the most specific one
          def areEqual(a: Map[String, Int], b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should !== (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should === (Map("one" -> 1, "two" -> 2))
      }
    }
    describe("for mutable.Map") {
      it("with default equality") {
        mutable.Map("I" -> 1, "II" -> 2) should === (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) should !== (mutable.Map("one" -> 1, "two" -> 2))
        implicit val e = new Equality[GenMap[String, Int]] {
          def areEqual(a: GenMap[String, Int], b: Any): Boolean = a != b
        }
        mutable.Map("I" -> 1, "II" -> 2) should === (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) should !== (mutable.Map("one" -> 1, "two" -> 2))
      }
      it("with inferred GenMap equality") {
        implicit def travEq[T <: GenMap[String, Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        mutable.Map("I" -> 1, "II" -> 2) should !== (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) should === (mutable.Map("one" -> 1, "two" -> 2))
      }
      it("with specific mutable.Map equality") {
        implicit val e = new Equality[mutable.Map[String, Int]] {
          def areEqual(a: mutable.Map[String, Int], b: Any): Boolean = a != b
        }
        mutable.Map("I" -> 1, "II" -> 2) should !== (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) should === (mutable.Map("one" -> 1, "two" -> 2))
      }
      it("with both GenMap and specific mutable.Map equality, though I don't know why this compiles") {
        implicit val e = new Equality[GenMap[String, Int]] {
          def areEqual(a: GenMap[String, Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[mutable.Map[String, Int]] { // Should pick the most specific one
          def areEqual(a: mutable.Map[String, Int], b: Any): Boolean = a != b
        }
        mutable.Map("I" -> 1, "II" -> 2) should !== (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) should === (mutable.Map("one" -> 1, "two" -> 2))
      }
      it("with both inferred GenMap and specific mutable.Map equality") {
        implicit def travEq[T <: GenMap[String, Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[mutable.Map[String, Int]] { // Should pick the most specific one
          def areEqual(a: mutable.Map[String, Int], b: Any): Boolean = a != b
        }
        mutable.Map("I" -> 1, "II" -> 2) should !== (mutable.Map("I" -> 1, "II" -> 2))
        mutable.Map("I" -> 1, "II" -> 2) should === (mutable.Map("one" -> 1, "two" -> 2))
      }
    }
    it("for AnyRef") {
      case class Person(name: String)
      Person("Joe") should === (Person("Joe"))
      Person("Joe") should !== (Person("Sally"))

      {
        implicit val e = new Equality[Person] {
          def areEqual(a: Person, b: Any): Boolean = a != b
        }
        Person("Joe") should !== (Person("Joe"))
        Person("Joe") should === (Person("Sally"))
      }
    }
    describe("for Traversable") {
      it("with default equality") {
        Set(1, 2, 3) should === (Set(1, 2, 3))
        Set(1, 2, 3) should !== (Set(1, 2, 4))

        implicit val e = new Equality[Iterable[Int]] {
          def areEqual(a: Iterable[Int], b: Any): Boolean = a != b
        }

        Set(1, 2, 3) should === (Set(1, 2, 3))
        Set(1, 2, 3) should !== (Set(1, 2, 4))
      }
      it("with inferred Iterable equality") {
        implicit def travEq[T <: Iterable[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should !== (Set(1, 2, 3))
        Set(1, 2, 3) should === (Set(1, 2, 4))
      }
      it("with specific Traversable equality") {
        implicit val e = new Equality[Set[Int]] {
          def areEqual(a: Set[Int], b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should !== (Set(1, 2, 3))
        Set(1, 2, 3) should === (Set(1, 2, 4))
      }
      it("with both Iterable and specific Traversable equality") {
        implicit val e = new Equality[Iterable[Int]] {
          def areEqual(a: Iterable[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Set[Int]] { // Should pick the most specific one
          def areEqual(a: Set[Int], b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should !== (Set(1, 2, 3))
        Set(1, 2, 3) should === (Set(1, 2, 4))
      }
      it("with both inferred Iterable and specific Traversable equality") {
        implicit def travEq[T <: Iterable[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[Set[Int]] { // Should pick the most specific one
          def areEqual(a: Set[Int], b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should !== (Set(1, 2, 3))
        Set(1, 2, 3) should === (Set(1, 2, 4))
      }
    }
    describe("for mutable.Traversable") {
      it("with default equality") {
        mutable.Set(1, 2, 3) should === (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) should !== (mutable.Set(1, 2, 4))

        implicit val e = new Equality[Iterable[Int]] {
          def areEqual(a: Iterable[Int], b: Any): Boolean = a != b
        }

        mutable.Set(1, 2, 3) should === (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) should !== (mutable.Set(1, 2, 4))
      }
      it("with inferred Iterable equality") {
        implicit def travEq[T <: Iterable[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        mutable.Set(1, 2, 3) should !== (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) should === (mutable.Set(1, 2, 4))
      }
      it("with specific mutable.Traversable equality") {
        implicit val e = new Equality[mutable.Set[Int]] {
          def areEqual(a: mutable.Set[Int], b: Any): Boolean = a != b
        }
        mutable.Set(1, 2, 3) should !== (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) should === (mutable.Set(1, 2, 4))
      }
      it("with both Iterable and specific Traversable equality") {
        implicit val e = new Equality[Iterable[Int]] {
          def areEqual(a: Iterable[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[mutable.Set[Int]] { // Should pick the most specific one
          def areEqual(a: mutable.Set[Int], b: Any): Boolean = a != b
        }
        mutable.Set(1, 2, 3) should !== (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) should === (mutable.Set(1, 2, 4))
      }
      it("with both inferred Iterable and specific Traversable equality") {
        implicit def travEq[T <: Iterable[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[mutable.Set[Int]] { // Should pick the most specific one
          def areEqual(a: mutable.Set[Int], b: Any): Boolean = a != b
        }
        mutable.Set(1, 2, 3) should !== (mutable.Set(1, 2, 3))
        mutable.Set(1, 2, 3) should === (mutable.Set(1, 2, 4))
      }
    }
    // SKIP-SCALATESTJS,NATIVE-START
    describe("for Java Collection") {

      val javaSet123: java.util.Set[Int] = new java.util.HashSet
      javaSet123.add(1)
      javaSet123.add(2)
      javaSet123.add(3)

      val javaSet124: java.util.Set[Int] = new java.util.HashSet
      javaSet124.add(1)
      javaSet124.add(2)
      javaSet124.add(4)

      it("with default equality") {
        javaSet123 should === (javaSet123)
        javaSet123 should !== (javaSet124)
        implicit val e = new Equality[java.util.Collection[Int]] {
          def areEqual(a: java.util.Collection[Int], b: Any): Boolean = a != b
        }
        javaSet123 should === (javaSet123)
        javaSet123 should !== (javaSet124)
      }

      it("with inferred Collection equality") {
        // implicit val e = new Equality[Iterable[Int]] { ... does not and should not compile
        implicit def travEq[T <: java.util.Collection[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        javaSet123 should !== (javaSet123)
        javaSet123 should === (javaSet124)
      }

      it("with specific Collection equality") {
        implicit val e = new Equality[java.util.Set[Int]] {
          def areEqual(a: java.util.Set[Int], b: Any): Boolean = a != b
        }
        javaSet123 should !== (javaSet123)
        javaSet123 should === (javaSet124)
      }

      it("with both Collection and specific Collection equality") {
        implicit val e = new Equality[java.util.Collection[Int]] {
          def areEqual(a: java.util.Collection[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[java.util.Set[Int]] { // Should pick the most specific one
          def areEqual(a: java.util.Set[Int], b: Any): Boolean = a != b
        }
        javaSet123 should !== (javaSet123)
        javaSet123 should === (javaSet124)
      }
      it("with both inferred Collection and specific Collection equality") {
        implicit def travEq[T <: java.util.Collection[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[java.util.Set[Int]] { // Should pick the most specific one
          def areEqual(a: java.util.Set[Int], b: Any): Boolean = a != b
        }
        javaSet123 should !== (javaSet123)
        javaSet123 should === (javaSet124)
      }
    }

    describe("for Java Map") {

      val javaMap123: java.util.HashMap[String, Int] = new java.util.HashMap
      javaMap123.put("one",1)
      javaMap123.put("two", 2)
      javaMap123.put("three", 3)

      val javaMap124: java.util.HashMap[String, Int] = new java.util.HashMap
      javaMap124.put("one",1)
      javaMap124.put("two", 2)
      javaMap124.put("four", 4)

      it("with default equality") {
        javaMap123 should === (javaMap123)
        javaMap123 should !== (javaMap124)
        implicit val e = new Equality[java.util.Map[String, Int]] {
          def areEqual(a: java.util.Map[String, Int], b: Any): Boolean = a != b
        }
        javaMap123 should === (javaMap123)
        javaMap123 should !== (javaMap124)
      }

      it("with inferred Map equality") {
        implicit def travEq[T <: java.util.Map[String, Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        javaMap123 should !== (javaMap123)
        javaMap123 should === (javaMap124)
      }

      it("with specific HashMap equality") {
        implicit val e = new Equality[java.util.HashMap[String, Int]] {
          def areEqual(a: java.util.HashMap[String, Int], b: Any): Boolean = a != b
        }
        javaMap123 should !== (javaMap123)
        javaMap123 should === (javaMap124)
      }

      it("with both Map and specific HashMap equality") {
        implicit val e = new Equality[java.util.Map[String, Int]] {
          def areEqual(a: java.util.Map[String, Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[java.util.HashMap[String, Int]] { // Should pick this because it is an exact match
          def areEqual(a: java.util.HashMap[String, Int], b: Any): Boolean = a != b
        }
        javaMap123 should !== (javaMap123)
        javaMap123 should === (javaMap124)
      }
      it("with both inferred Map and specific HashMap equality") {
        implicit def travEq[T <: java.util.Map[String, Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[java.util.HashMap[String, Int]] { // Should pick the most specific one
          def areEqual(a: java.util.HashMap[String, Int], b: Any): Boolean = a != b
        }
        javaMap123 should !== (javaMap123)
        javaMap123 should === (javaMap124)
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END

    describe("for Seq") {
      it("with default equality") {
        Vector(1, 2, 3) should === (Vector(1, 2, 3))
        Vector(1, 2, 3) should !== (Vector(1, 2, 4))
      }
      it("with inferred GenSeq equality") {
        implicit def travEq[T <: GenSeq[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should !== (Vector(1, 2, 3))
        Vector(1, 2, 3) should === (Vector(1, 2, 4))
      }
      it("with specific Seq equality") {
        implicit val e = new Equality[Vector[Int]] {
          def areEqual(a: Vector[Int], b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should !== (Vector(1, 2, 3))
        Vector(1, 2, 3) should === (Vector(1, 2, 4))
      }
      it("with both GenSeq and specific Seq equality") {
        implicit val e = new Equality[GenSeq[Int]] {
          def areEqual(a: GenSeq[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Vector[Int]] { // Should pick the exact one
          def areEqual(a: Vector[Int], b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should !== (Vector(1, 2, 3))
        Vector(1, 2, 3) should === (Vector(1, 2, 4))
      }
      it("with both inferred GenSeq and specific Seq equality") {
        implicit def travEq[T <: GenSeq[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Vector[Int]] { // Should pick the exact one
          def areEqual(a: Vector[Int], b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should !== (Vector(1, 2, 3))
        Vector(1, 2, 3) should === (Vector(1, 2, 4))
      }
    }
    describe("for mutable.Seq") {
      it("with default equality") {
        ListBuffer(1, 2, 3) should === (ListBuffer(1, 2, 3))
        ListBuffer(1, 2, 3) should !== (ListBuffer(1, 2, 4))
      }
      it("with inferred GenSeq equality") {
        implicit def travEq[T <: GenSeq[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        ListBuffer(1, 2, 3) should !== (ListBuffer(1, 2, 3))
        ListBuffer(1, 2, 3) should === (ListBuffer(1, 2, 4))
      }
      it("with specific Seq equality") {
        implicit val e = new Equality[ListBuffer[Int]] {
          def areEqual(a: ListBuffer[Int], b: Any): Boolean = a != b
        }
        ListBuffer(1, 2, 3) should !== (ListBuffer(1, 2, 3))
        ListBuffer(1, 2, 3) should === (ListBuffer(1, 2, 4))
      }
      it("with both GenSeq and specific Seq equality") {
        implicit val e = new Equality[GenSeq[Int]] {
          def areEqual(a: GenSeq[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[ListBuffer[Int]] { // Should pick the exact one
          def areEqual(a: ListBuffer[Int], b: Any): Boolean = a != b
        }
        ListBuffer(1, 2, 3) should !== (ListBuffer(1, 2, 3))
        ListBuffer(1, 2, 3) should === (ListBuffer(1, 2, 4))
      }
      it("with both inferred GenSeq and specific Seq equality") {
        implicit def travEq[T <: GenSeq[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[ListBuffer[Int]] { // Should pick the exact one
          def areEqual(a: ListBuffer[Int], b: Any): Boolean = a != b
        }
        ListBuffer(1, 2, 3) should !== (ListBuffer(1, 2, 3))
        ListBuffer(1, 2, 3) should === (ListBuffer(1, 2, 4))
      }
    }
    it("for Array") {
      Array(1, 2, 3) should === (Array(1, 2, 3))
      Array(1, 2, 3) should !== (Array(1, 2, 4))

      {
        implicit val e = new Equality[Array[Int]] {
          def areEqual(a: Array[Int], b: Any): Boolean = deep(a) != deep(b.asInstanceOf[Array[Int]])
        }
        Array(1, 2, 3) should !== (Array(1, 2, 3))
        Array(1, 2, 3) should === (Array(1, 2, 4))
      }
    }
    // SKIP-SCALATESTJS,NATIVE-START
    describe("for Java List") {

      val javaList123: java.util.List[Int] = new java.util.ArrayList
      javaList123.add(1)
      javaList123.add(2)
      javaList123.add(3)

      val javaList124: java.util.List[Int] = new java.util.ArrayList
      javaList124.add(1)
      javaList124.add(2)
      javaList124.add(4)
      
      it("with default equality") {
        javaList123 should === (javaList123)
        javaList123 should !== (javaList124)
        implicit val e = new Equality[java.util.Collection[Int]] {
          def areEqual(a: java.util.Collection[Int], b: Any): Boolean = a != b
        }
        javaList123 should === (javaList123)
        javaList123 should !== (javaList124)
      }
      it("with inferred java.util.Collection equality") {
        implicit def travEq[T <: java.util.Collection[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        javaList123 should !== (javaList123)
        javaList123 should === (javaList124)
      }
      it("with specific java.util.List equality") {
        implicit val e = new Equality[java.util.List[Int]] {
          def areEqual(a: java.util.List[Int], b: Any): Boolean = a != b
        }
        javaList123 should !== (javaList123)
        javaList123 should === (javaList124)
      }
      it("with both java.util.Collection and java.util.List equality") {
        implicit val e = new Equality[java.util.Collection[Int]] {
          def areEqual(a: java.util.Collection[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[java.util.List[Int]] { // Should pick the exact one
          def areEqual(a: java.util.List[Int], b: Any): Boolean = a != b
        }
        javaList123 should !== (javaList123)
        javaList123 should === (javaList124)
      }
      it("with both inferred java.util.List and specific java.util.List equality") {
        implicit def travEq[T <: java.util.List[Int]]: Equality[T] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[java.util.List[Int]] { // Should pick the exact one
          def areEqual(a: java.util.List[Int], b: Any): Boolean = a != b
        }
        javaList123 should !== (javaList123)
        javaList123 should === (javaList124)
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }
}

