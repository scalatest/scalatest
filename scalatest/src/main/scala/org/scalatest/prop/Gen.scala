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

import scala.util.Random

abstract class Gen[T](seed: Int) {
  def next(): T
  def map[U](f: T => U): Gen[U]
  def flatMap[U](f: T => Gen[U]): Gen[U]
}

object Gen {
  private abstract class AbstractGen[T](seed: Int) extends Gen[T](seed) { thisAbstractGen =>
    protected val rnd = new Rnd(seed) // Only usable in this file, so can be sure this doesn't mutate
    def map[U](f: T => U): Gen[U] =
      new AbstractGen[U](seed) {
        def next() = f(thisAbstractGen.next())
      }
    def flatMap[U](f: T => Gen[U]): Gen[U] = 
      new AbstractGen[U](seed) {
        def next() = f(thisAbstractGen.next()).next()
      }
  }
  private class IntGen(seed: Int) extends AbstractGen[Int](seed: Int) {
    def next(): Int = rnd.nextInt()
  }
  def intGen(seed: Int): Gen[Int] = new IntGen(seed)

  private class DoubleGen(seed: Int) extends AbstractGen[Double](seed: Int) {
    def next(): Double = rnd.nextDouble()
  }
  def doubleGen(seed: Int): Gen[Int] = new IntGen(seed)
}
