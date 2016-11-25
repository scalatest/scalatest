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

class AssociativeSpec extends UnitSpec {
  
  // Every Associative
  class EveryAssociative[A] extends Associative[Every[A]] {
    def combine(a: Every[A], b: Every[A]): Every[A] = a ++ b
  }

  // Int Multiplication Associatvie
  class IntMultiAssociative extends Associative[Int] {
    def combine(a: Int, b: Int): Int = a * b
  }

  "An Every Associative (Semigroup)" should "have a binary associative combine" in {
    implicit val assoc = new EveryAssociative[Int]
    import Associative.adapters
    val a = Every(1,2)
    val b = Every(5,6,7)
    val c = Every(9,9)
    ((a combine b) combine c) shouldEqual (a combine (b combine c))
  }
  
  "An Int Associative (Semigroup)" should  "have a binary associative combine" in {
    implicit val assoc = new IntMultiAssociative
    import Associative.adapters
    val intAssoc = new IntMultiAssociative()
    // val a = new Associative.Adapter(1)(intAssoc)
    val a = 1
    val b = 64
    val c = 256
    ((a combine b) combine c) shouldEqual (a combine (b combine c))
   }
    
  "A BadSubstractionAssociative" should  "fail to be associative" in {
    // Bad case Substraction Associative, should fail.
    class BadSubstractionAssociative extends Associative[Int] {
      def combine(a: Int, b: Int): Int = a - b
    }
    implicit val assoc = new BadSubstractionAssociative 
    import Associative.adapters
    val badSubAssoc = new BadSubstractionAssociative()
    val a = 1
    val b = 64
    val c = 256
    ((a combine b) combine c) should not be (a combine (b combine c))
  }

  "Associative" should "offer an combine method directly" in {
    val a = Every(1,2)
    val b = Every(5,6,7)
    val c = Every(9,9)
    val assoc = new EveryAssociative[Int]
    import assoc.combine
    combine(combine(a, b), c) shouldEqual combine(a, combine(b, c))
  }

  it should "provide an parameterless apply method in its companion to summon an implicit" in {
    implicit val assoc = new IntMultiAssociative
    assoc should be theSameInstanceAs implicitly[Associative[Int]]
    assoc should be theSameInstanceAs Associative[Int]
  }
}
