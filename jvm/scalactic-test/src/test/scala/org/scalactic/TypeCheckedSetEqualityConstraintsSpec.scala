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
package org.scalactic

import org.scalatest._
import scala.collection.{mutable,immutable}

class TypeCheckedSetEqualityConstraintsSpec extends funspec.AnyFunSpec with NonImplicitAssertions with TypeCheckedTripleEquals with SetEqualityConstraints {

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

  describe("the SetEqualityConstraints trait") {

    it("should allow any Set to be compared with any other Set, so long as the element types of the two Sets adhere to the equality constraint in force for those types") {
      assert(mutable.HashSet(1, 2, 3) === immutable.HashSet(1, 2, 3))
      // assert(mutable.HashSet(1, 2, 3) === immutable.HashSet(1L, 2L, 3L)) // does not compile last time I checked
      // assert(mutable.HashSet(1L, 2L, 3L) === immutable.HashSet(1, 2, 3)) // does not compile last time I checked
      // assert(immutable.HashSet(1, 2, 3) === mutable.HashSet(1L, 2L, 3L)) // does not compile last time I checked
      // assert(immutable.HashSet(1L, 2L, 3L) === mutable.HashSet(1, 2, 3)) // does not compile last time I checked
      assert(mutable.HashSet(new Apple, new Apple) === immutable.HashSet(new Fruit("apple"), new Fruit("apple")))
      assert(immutable.HashSet(new Fruit("apple"), new Fruit("apple")) === mutable.HashSet(new Apple, new Apple))
      // assert(mutable.HashSet(new Apple, new Apple) === immutable.HashSet(new Orange, new Orange)) // does not compile last time I checked
      // assert(immutable.HashSet(new Apple, new Apple) === mutable.HashSet(new Orange, new Orange)) // does not compile last time I checked
      // assert(immutable.HashSet(new Orange, new Orange) === mutable.HashSet(new Apple, new Apple)) // does not compile last time I checked
      // assert(mutable.HashSet(new Orange, new Orange) === immutable.HashSet(new Apple, new Apple)) // does not compile last time I checked
    }
  }
}

