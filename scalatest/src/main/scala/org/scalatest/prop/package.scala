/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalatest

import org.scalactic.anyvals._
import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag

package object prop {

  // Called by the general function1 generator.
  def valueOf[B](a: Any, f: Int => Int)(implicit genOfB: Generator[B]): B = {
   val seed = (f(a.hashCode)).toLong
   val rnd = Randomizer(seed)
   val (size, nextRnd) = rnd.chooseInt(1, 20)
   val (result, _, _) = genOfB.next(size, Nil, nextRnd)
   result
  }

  // Called by the general function2 generator.
  def valueOf[C](a: Any, b: Any, f: Int => Int)(implicit genOfC: Generator[C]): C = {
   def combinedHashCode(a: Any, b: Any): Int =
     37 * (
       37 + a.hashCode
     ) + b.hashCode
   val seed = (f(combinedHashCode(a, b))).toLong
   val rnd = Randomizer(seed)
   val (size, nextRnd) = rnd.chooseInt(1, 20)
   val (result, _, _) = genOfC.next(size, Nil, nextRnd)
   result
  }
}


