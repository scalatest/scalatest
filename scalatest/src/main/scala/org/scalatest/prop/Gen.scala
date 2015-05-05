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
package org.scalatest.prop

// (size: Int, randomNumGen: Rnd) => (value, new randomNumGen)
class Gen[T] private (val genFun: (Int, Rnd) => (T, Rnd)) {
  def next(): T = genFun(100, new Rnd(100))._1
  def map[U](f: T => U): Gen[U] =
    new Gen[U]( (size: Int, rnd: Rnd) => {
      val (value, nextRnd) = genFun(size, rnd)
      (f(value), nextRnd)
    }
  )
  def flatMap[U](f: T => Gen[U]): Gen[U] = 
    new Gen[U]((size: Int, rnd: Rnd) => {
      val (value, nextRnd) = genFun(size, rnd)
      f(value).genFun(size, nextRnd)
    }
  )
}

object Gen {
  val intGen: Gen[Int] = new Gen((_, rnd) => rnd.nextInt)
  val doubleGen: Gen[Double] = new Gen((_, rnd) => rnd.nextDouble)
}
