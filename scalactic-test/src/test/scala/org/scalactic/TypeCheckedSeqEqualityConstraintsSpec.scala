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

class TypeCheckedSeqEqualityConstraintsSpec extends FunSpec with NonImplicitAssertions with TypeCheckedTripleEquals with SeqEqualityConstraints {

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

  describe("the SeqEqualityConstraints trait") {

    it("should allow any Seq to be compared with any other Seq, so long as the element types of the two Seq's are in a subtype/supertype relationship") {
      assert(Vector(1, 2, 3) === List(1, 2, 3))
      // assert(Vector(1, 2, 3) === List(1L, 2L, 3L)) // does not compile last time I checked
      assert(Vector(new Apple, new Apple) === List(new Fruit("apple"), new Fruit("apple")))
      assert(List(new Fruit("apple"), new Fruit("apple")) === Vector(new Apple, new Apple))
      // assert(Vector(new Apple, new Apple) === List(new Orange, new Orange)) // does not compile last time I checked
      // assert(List(new Orange, new Orange) === Vector(new Apple, new Apple)) // does not compile last time I checked
    }
  }
}

