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

import scala.language.higherKinds
import org.scalactic.UnitSpec

class DistributiveSpec extends UnitSpec {
  
  // removing a list of elements is distributive over concatination
  class ListRemoveDistributive[A] extends Distributive[List[A]] {
    def op(a: List[A], b: List[A]): List[A] = a ++ b
    def dop(as: List[A], bs: List[A]): List[A] = bs.filter(b => !as.contains(b)) 
  }
  
  // Int multiplication over addition is distributive
  class IntMultiOverAdditionDistributive extends Distributive[Int] {
    def op(a: Int, b: Int): Int = a + b
    def dop(a: Int, b: Int): Int = a * b
  }
  
  "Removing a list of elemenents and concatination " should  
  "be distributive where remove is the distributive dop and concat is op which is distributed over " in {
    implicit val dist = new ListRemoveDistributive[Int]
    import Distributive.adapters
    val as = List(1,2,3)
    val bs = List(2,3,4,5)
    val cs = List()
    
    val op = dist.op _
    val dop = dist.dop _
    val fn: (Int => Int) = x => x + 1
    
    (as dop (bs op cs)) shouldEqual ((as dop bs) op (as dop cs))
   }
   
  "Int multiplication over addition distributive " should  
  "have a binary operation 'op' and another binary operation (dop) that distributes over 'op'" in {
    implicit val dist = new IntMultiOverAdditionDistributive
    import Distributive.adapters
    val a = 64
    val b = 256
    val c = 32
    (a dop (b op c)) shouldEqual ((a dop b) op (a dop c))
   }
   
}
