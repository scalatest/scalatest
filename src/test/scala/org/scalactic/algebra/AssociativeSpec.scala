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

  // Every non-empty vector    
  case class EveryAssociative[A](underlying: Every[A]) extends Associative[Every[A]] {
    override 
    def op(other: Associative[Every[A]]): EveryAssociative[A] = EveryAssociative(underlying ++ other.underlying)
  }
  
  case class IntMultiplicationAssociative(underlying: Int) extends Associative[Int] {
    override 
    def op(other: Associative[Int]): IntMultiplicationAssociative = IntMultiplicationAssociative(underlying * other.underlying )
  }
  
  case class BadSubstractionAssociative(underlying: Int) extends Associative[Int] {
    override 
    def op(other: Associative[Int]): BadSubstractionAssociative = BadSubstractionAssociative(underlying - other.underlying )
  }
    
  "An Every Associative (Semigroup) " should " have a binaray associative op" in {
    val a = EveryAssociative(Every(1,2))
    val b = EveryAssociative(Every(5,6,7))
    val c = EveryAssociative(Every(9,9))
    ((a op b) op c) shouldEqual (a op (b op c))
  }
  
  "An Int Associative (Semigroup) " should  "have a binaray associative op" in {
    val a = IntMultiplicationAssociative(1)
    val b = IntMultiplicationAssociative(64)
    val c = IntMultiplicationAssociative(256)
    ((a op b) op c) shouldEqual (a op (b op c))
  }
  
  "A BadSubstractionAssociative " should  " fail to be associative" in {
      val a = BadSubstractionAssociative(1)
      val b = BadSubstractionAssociative(64)
      val c = BadSubstractionAssociative(256)
      ((a op b) op c) should not be (a op (b op c))
  }
  
}