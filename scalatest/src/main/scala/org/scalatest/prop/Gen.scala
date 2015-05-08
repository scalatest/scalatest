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

/*
I need to know how many or at least what percentage of edges I should
produce. When I map and flatmap I want it to combine edges and regulars
both. What if instead of T, we filled two buckets, and instead of just
a size, we also pass in how many of each we want?
Or, how about an Rng for edge conditions, and I just hmm. Yes that might
work. If I have 7 edges, then I want a random number from 0 to 6 and
I pick from the edges. Yes, so my edges could be a thing on Gen, which
also gets mapped and flatMapped? Do I have a number of them? Or do I not
worry about that. Probably just don't worry about it.
Edges[T] {
  can have map and flatmap on it
}
*/
// (size: Int, randomNumGen: Rnd) => (value, new randomNumGen)
// def yeOldeNext(size: Int = 10, rnd: Rnd = Rnd.default()): (T, Rnd, Gen[T]) = next(size, rnd)
trait Gen[T] { thisGenOfT =>
  def next(size: Int = 100, rnd: Rnd = Rnd.default): (T, Rnd, Gen[T])
  def map[U](f: T => U): Gen[U] =
    new Gen[U] {
      def next(size: Int, rnd: Rnd): (U, Rnd, Gen[U]) = {
        val (nextT, nextRnd, nextGenOfT) = thisGenOfT.next(size, rnd)
        (f(nextT), nextRnd, nextGenOfT.map(f))
      }
    }
  def flatMap[U](f: T => Gen[U]): Gen[U] = 
    new Gen[U] { thisInnerGen =>
      def next(size: Int, rnd: Rnd): (U, Rnd, Gen[U]) = {
        val (nextT, nextRnd, nextGenOfT) = thisGenOfT.next(size, rnd)
        val (a, b, c) = f(nextT).next(size, nextRnd)
        (a, b, thisInnerGen)
      }
    }
}

object Gen {
  private final class IntGen(val edges: List[Int]) extends Gen[Int] { thisIntGen =>
    def next(size: Int, rnd: Rnd): (Int, Rnd, Gen[Int]) = {
      val (nextValue, nextRnd) = rnd.nextInt
      edges match {
        case head :: tail => (head, nextRnd, new IntGen(tail))
        case Nil => (nextValue, nextRnd, thisIntGen)
      }
    }
    def mappedNext(size: Int, rnd: Rnd): (Int, Rnd, Gen[Int]) = {
      val (nextValue, nextRnd) = rnd.nextInt
      (nextValue, nextRnd, new IntGen(Nil))
    }
    override def map[U](f: Int => U): Gen[U] =
      new Gen[U] {
        def next(size: Int, rnd: Rnd): (U, Rnd, Gen[U]) = {
          val (nextT, nextRnd, nextGenOfT) = thisIntGen.mappedNext(size, rnd)
          (f(nextT), nextRnd, nextGenOfT.map(f))
        }
      }
    override def flatMap[U](f: Int => Gen[U]): Gen[U] = 
      new Gen[U] { thisInnerGen =>
        def next(size: Int, rnd: Rnd): (U, Rnd, Gen[U]) = {
          val (nextT, nextRnd, nextGenOfT) = thisIntGen.mappedNext(size, rnd)
          val (a, b, c) = f(nextT).next(size, nextRnd)
          (a, b, thisInnerGen)
        }
      }
  }
  implicit val intGen: Gen[Int] =
    new IntGen(List(Int.MinValue, -1, 0, 1, Int.MaxValue))

  private final class DoubleGen(edges: List[Double]) extends Gen[Double] { thisDoubleGen =>
    def next(size: Int, rnd: Rnd): (Double, Rnd, Gen[Double]) = {
      val (nextValue, nextRnd) = rnd.nextDouble
      edges match {
        case head :: tail => (head, nextRnd, new DoubleGen(tail))
        case Nil => (nextValue, nextRnd, thisDoubleGen)
      }
    }
    def mappedNext(size: Int, rnd: Rnd): (Double, Rnd, Gen[Double]) = {
      val (nextValue, nextRnd) = rnd.nextDouble
      (nextValue, nextRnd, new DoubleGen(Nil))
    }
    override def map[U](f: Double => U): Gen[U] =
      new Gen[U] {
        def next(size: Int, rnd: Rnd): (U, Rnd, Gen[U]) = {
          val (nextT, nextRnd, nextGenOfT) = thisDoubleGen.mappedNext(size, rnd)
          (f(nextT), nextRnd, nextGenOfT.map(f))
        }
      }
    override def flatMap[U](f: Double => Gen[U]): Gen[U] = 
      new Gen[U] { thisInnerGen =>
        def next(size: Int, rnd: Rnd): (U, Rnd, Gen[U]) = {
          val (nextT, nextRnd, nextGenOfT) = thisDoubleGen.mappedNext(size, rnd)
          val (a, b, c) = f(nextT).next(size, nextRnd)
          (a, b, thisInnerGen)
        }
      }
  }
  implicit val doubleGen: Gen[Double] = new DoubleGen(List(0.0))
}

