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
package org.scalautils

import org.scalatest._
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce
import scala.collection.{mutable, immutable}

class ConversionCheckedTraversableEqualityConstraintsSpec extends Spec with NonImplicitAssertions with ConversionCheckedTripleEquals with TraversableEqualityConstraints {

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

  object `the TraversableEqualityConstraints trait` {

    // Actually, I wonder if we shouldn't use something put into scopye by either TypeChecked or ConversionChecked
    def `should allow any Seq to be compared with any other Seq, so long as the element types of the two Seq's are in a subtype/supertype relationship` {
      assert(Vector(1, 2, 3) === List(1, 2, 3))
      assert(Vector(1, 2, 3) === List(1L, 2L, 3L))
      assert(Vector(new Apple, new Apple) === List(new Fruit("apple"), new Fruit("apple")))
      assert(List(new Fruit("apple"), new Fruit("apple")) === Vector(new Apple, new Apple))
      // assert(Vector(new Apple, new Apple) === List(new Orange, new Orange)) // does not compile last time I checked
      // assert(List(new Orange, new Orange) === Vector(new Apple, new Apple)) // does not compile last time I checked
    }

    def `should allow any Set to be compared with any other Set, so long as the element types of the two Sets adhere to the equality constraint in force for those types` {
      assert(mutable.HashSet(1, 2, 3) === immutable.HashSet(1, 2, 3))
      assert(mutable.HashSet(1, 2, 3) === immutable.HashSet(1L, 2L, 3L))
      assert(mutable.HashSet(1L, 2L, 3L) === immutable.HashSet(1, 2, 3))
      assert(immutable.HashSet(1, 2, 3) === mutable.HashSet(1L, 2L, 3L))
      assert(immutable.HashSet(1L, 2L, 3L) === mutable.HashSet(1, 2, 3))
      assert(mutable.HashSet(new Apple, new Apple) === immutable.HashSet(new Fruit("apple"), new Fruit("apple")))
      assert(immutable.HashSet(new Fruit("apple"), new Fruit("apple")) === mutable.HashSet(new Apple, new Apple))
      // assert(mutable.HashSet(new Apple, new Apple) === immutable.HashSet(new Orange, new Orange)) // does not compile last time I checked
      // assert(immutable.HashSet(new Apple, new Apple) === mutable.HashSet(new Orange, new Orange)) // does not compile last time I checked
      // assert(immutable.HashSet(new Orange, new Orange) === mutable.HashSet(new Apple, new Apple)) // does not compile last time I checked
      // assert(mutable.HashSet(new Orange, new Orange) === immutable.HashSet(new Apple, new Apple)) // does not compile last time I checked
    }

    def `should allow any Map to be compared with any other Map, so long as the element types of the two Maps adhere to the equality constraint in force for those types` {
      assert(mutable.HashMap('a' -> 1, 'b' -> 2, 'c' -> 3) === immutable.HashMap('a' -> 1, 'b' -> 2, 'c' -> 3))
      assert(mutable.HashMap('a' -> 1, 'b' -> 2, 'c' -> 3) === immutable.HashMap('a' -> 1L, 'b' -> 2L, 'c' -> 3L)) // does not compile last time I checked
      assert(mutable.HashMap('a' -> 1L, 'b' -> 2L, 'c' -> 3L) === immutable.HashMap('a' -> 1, 'b' -> 2, 'c' -> 3)) // does not compile last time I checked
      assert(immutable.HashMap('a' -> 1, 'b' -> 2, 'c' -> 3) === mutable.HashMap('a' -> 1L, 'b' -> 2L, 'c' -> 3L)) // does not compile last time I checked
      assert(immutable.HashMap('a' -> 1L, 'b' -> 2L, 'c' -> 3L) === mutable.HashMap('a' -> 1, 'b' -> 2, 'c' -> 3)) // does not compile last time I checked
      assert(mutable.HashMap('a' -> new Apple, 'b' -> new Apple) === immutable.HashMap('a' -> new Fruit("apple"), 'b' -> new Fruit("apple")))
      assert(immutable.HashMap('a' -> new Fruit("apple"), 'b' -> new Fruit("apple")) === mutable.HashMap('a' -> new Apple, 'b' -> new Apple))
      // assert(mutable.HashMap('a' -> new Apple, 'b' -> new Apple) === immutable.HashMap('a' -> new Orange, 'b' -> new Orange)) // does not compile last time I checked
      // assert(immutable.HashMap('a' -> new Apple, 'b' -> new Apple) === mutable.HashMap('a' -> new Orange, 'b' -> new Orange)) // does not compile last time I checked
      // assert(immutable.HashMap('a' -> new Orange, 'b' -> new Orange) === mutable.HashMap('a' -> new Apple, 'b' -> new Apple)) // does not compile last time I checked
      // assert(mutable.HashMap('a' -> new Orange, 'b' -> new Orange) === immutable.HashMap('a' -> new Apple, 'b' -> new Apple)) // does not compile last time I checked
    }
  }
}

