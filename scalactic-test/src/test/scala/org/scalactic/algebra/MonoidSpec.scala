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
package org.scalactic.algebra

import org.scalactic.UnitSpec

class MonoidSpec extends UnitSpec {
   
  val listMonoid = new Monoid[List[_]] {
    def z: List[_] = Nil
    def op(xs: List[_], ys: List[_]): List[_] = xs ++ ys
  }
  
  def listMonoidTyped[A] = new Monoid[List[A]] {
    def z: List[A] = Nil
    def op(xs: List[A], ys: List[A]): List[A] = xs ++ ys
  }
  
  val additionMonoid = new Monoid[Int] { 
    def z: Int = 0
    def op(a1: Int, a2: Int): Int = a1 + a2
  }
  
  "The list monoid of any type " should " have a binaray associative op" in {
    val as = List(1,2,3)
    val bs = List("five","six")
    val cs = List(10)
    val op: (List[_], List[_]) => List[Any] = listMonoid.op _
    op( as, op(bs, cs) ) shouldEqual op( op(as, bs), cs )
  }
  
  "The list monoid " should " have a binaray associative op that supports infix notation" in {
    import Monoid.adapters
    implicit val monoid = listMonoidTyped[Int]
    val as = List(1,2,3)
    val bs = List(5,6)
    val cs = List(10)
    ((as op bs) op cs) shouldEqual (as op (bs op cs))
  }
  
  "The list monoid " should " have a zero element that is a left identity and right identity" in {
    import Monoid.adapters
    implicit val monoid = listMonoidTyped[Int]
    val as = List(1,2,3)
    (as op monoid.z) shouldEqual as
    (monoid.z op as) shouldEqual as
  }
  
  "The list monoid " should " be foldable over a collections of types " in {
    val lists = List(List(1,2,3), List(4,5,6), List(7,8))
    val foldResult: List[Any] = lists.fold(listMonoid.z)(listMonoid.op)
    foldResult shouldEqual List(1,2,3,4,5,6,7,8)
  }
  
  "The addition monoid " should " have a binary associative op " in {
    import Monoid.adapters
    implicit val monoid = additionMonoid
    val x = 80 
    val y = 86
    val z = 70
    ((x op y) op z) shouldEqual (x op (y op z))
  } 
  
  "The addition monoid " should " have a zero element, i.e. an identity element." in {
    import Monoid.adapters
    implicit val monoid = additionMonoid
    val x = 8086
    (x op monoid.z) shouldEqual x 
  }
  
  "The addition monoid's op and zero element " should " enable summing over a List of ints " in {
    val m = additionMonoid
    val listInts = List(1,4,5,10)
    val foldResult = listInts.fold(m.z)(m.op)
    foldResult shouldEqual 20
  }

  "Monoid" should "provide an parameterless apply method in its companion to summon an implicit" in {
    implicit val monoid = listMonoidTyped[Int]
    monoid should be theSameInstanceAs implicitly[Monoid[List[Int]]]
    monoid should be theSameInstanceAs Monoid[List[Int]]
  }
}

