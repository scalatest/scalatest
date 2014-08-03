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
package org.scalactic

import org.scalatest._
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce

class FreshConversionCheckedSeqEqualityConstraintsSpec extends Spec with NonImplicitAssertions with CheckedEquality {

  // TODO: Need to explicitly enable the implicit conversion

  case class Super(size: Int)
  class Sub(sz: Int) extends Super(sz)

  val super1: Super = new Super(1)
  val sub1: Sub = new Sub(1)
  val super2: Super = new Super(2)
  val sub2: Sub = new Sub(2)
  val nullSuper: Super = null

  case class Fruit(name: String)
  class Apple extends Fruit("apple")
  class Orange extends Fruit("orange")

  implicit class IntWrapper(val value: Int) {
    override def equals(o: Any): Boolean =
      o match {
        case that: IntWrapper => this.value == that.value
        case _ => false
      }
    override def hashCode: Int = value.hashCode
  }

  object `the SeqEqualityConstraints trait` {

    def `should allow any Seq to be compared with any other Seq, so long as the element types of the two Seq's have an InnerConstraint` {
      assert(Vector(1, 2, 3) === List(1, 2, 3))
      assert(Vector(1, 2, 3) === List(1L, 2L, 3L))
      assert(Vector(1L, 2L, 3L) === List(1, 2, 3))

      // Test for something convertible
      assertTypeError("Vector(new IntWrapper(1), new IntWrapper(2), new IntWrapper(3)) === List(1, 2, 3)")
      assertTypeError("Vector(1, 2, 3) === List(new IntWrapper(1), new IntWrapper(2), new IntWrapper(3))")

      assert(Vector(new Apple, new Apple) === List(new Fruit("apple"), new Fruit("apple")))
      assert(List(new Fruit("apple"), new Fruit("apple")) === Vector(new Apple, new Apple))
      assertTypeError("Vector(new Apple, new Apple) === List(new Orange, new Orange)")
      assertTypeError("List(new Orange, new Orange) === Vector(new Apple, new Apple)")
    }

    def `should allow an Array to be compared with any other Seq, so long as the element types of the two objects have an InnerConstraint` {
      assert(Array(1, 2, 3) === List(1, 2, 3))
      assert(Array(1, 2, 3) === List(1L, 2L, 3L))
      assert(Array(1L, 2L, 3L) === List(1, 2, 3))

      // Test for something convertible
      assertTypeError("Array(new IntWrapper(1), new IntWrapper(2), new IntWrapper(3)) === List(1, 2, 3)")
      assertTypeError("Array(1, 2, 3) === List(new IntWrapper(1), new IntWrapper(2), new IntWrapper(3))")

      assert(Array(new Apple, new Apple) === List(new Fruit("apple"), new Fruit("apple")))
      assert(Array(new Fruit("apple"), new Fruit("apple")) === Vector(new Apple, new Apple))
      assertTypeError("Array(new Apple, new Apple) === List(new Orange, new Orange)")
      assertTypeError("Array(new Orange, new Orange) === Vector(new Apple, new Apple)")
    }

    def `should allow any Seq to be compared with an Array, so long as the element types of the two objects have an InnerConstraint` {
      assert(Vector(1, 2, 3) === Array(1, 2, 3))
      assert(Vector(1, 2, 3) === Array(1L, 2L, 3L))
      assert(Vector(1L, 2L, 3L) === Array(1, 2, 3))

      // Test for something convertible
      assertTypeError("Vector(new IntWrapper(1), new IntWrapper(2), new IntWrapper(3)) === Array(1, 2, 3)")
      assertTypeError("Vector(1, 2, 3) === Array(new IntWrapper(1), new IntWrapper(2), new IntWrapper(3))")

      assert(Vector(new Apple, new Apple) === Array(new Fruit("apple"), new Fruit("apple")))
      assert(List(new Fruit("apple"), new Fruit("apple")) === Array(new Apple, new Apple))
      assertTypeError("Vector(new Apple, new Apple) === Array(new Orange, new Orange)")
      assertTypeError("List(new Orange, new Orange) === Array(new Apple, new Apple)")
    }
  }
}

