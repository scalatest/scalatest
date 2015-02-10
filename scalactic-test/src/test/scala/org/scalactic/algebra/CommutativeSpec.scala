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
import org.scalactic.Every

class CommutativeSpec extends UnitSpec {
  
  // Set Commutative
  class SetCommutative[A] extends Commutative[Set[A]] {
    def op(a: Set[A], b: Set[A]): Set[A] = a ++ b
  }
  
  // Int Multiplication Commutative
  class IntMultiCommutative extends Commutative[Int] {
    def op(a: Int, b: Int): Int = a * b
  }
  
  "Sets addition " should  " be commutative " in {
    implicit val commut = new SetCommutative[Int]
    import Commutative.adapters
    val as = Set(1,2,3)
    val bs = Set(2,3,4,5)
    // (adapters[Set[_]](as) op bs) shouldEqual (adapters[Set[_]](bs) op as)
    (as op bs) shouldEqual (bs op as)
   }
   
  "An Int Commutative " should  "have a binary commutative op" in {
    implicit val commut = new IntMultiCommutative
    import Commutative.adapters
    val a = 64
    val b = 256
    (a op b) shouldEqual (b op a)
   }
    
  "A BadSubstractionCommutative" should  "fail to be commutative" in {
    // Bad case Substraction Associative, should fail.
    class BadSubstractionCommutative extends Commutative[Int] {
      def op(a: Int, b: Int): Int = a - b
    }
    implicit val commut = new BadSubstractionCommutative
    import Commutative.adapters
    val a = 64
    val b = 256
    (a op b) should not be (b op a)
  }

  "Commutative " should "offer an op method directly" in {
    val a = 64
    val b = 256
    val commut = new IntMultiCommutative()
    import commut.op
    op(a, b) shouldEqual op(b, a)
  }

  it should "provide a parameterless apply method in its companion to summon an implicit" in {
    implicit val commut = new IntMultiCommutative
    commut should be theSameInstanceAs implicitly[Commutative[Int]]
    commut should be theSameInstanceAs Commutative[Int]
  }
}
