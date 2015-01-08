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

class SemigroupSpec extends UnitSpec {

  // Every non-empty vector    
  trait EverySemigroup[A] extends Semigroup[Every[A]] {
    def op(a1: Every[A], a2: Every[A]): Every[A] = a1 ++ a2
  }
  
  "A SemigroupProxy " should " have a binaray associative op" in {
    val semi = new EverySemigroup[Int] { }
    val a = Every(1,2)
    val b = Every(5,6,7)
    val c = Every(9,9)
    val op = semi.op _
    op( op(a, b), c ) shouldEqual op( a, op(b, c) )
  }
  
}

