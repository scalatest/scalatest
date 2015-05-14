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

import scala.annotation.tailrec
import scala.util.{Try, Failure, Success}
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.exceptions.TestFailedException

// For now, hard coding a size of 10. Later will need to do the size based on config
trait GenDrivenPropertyChecks extends Configuration with Whenever {
  def forAll[A](fun: (A) => Unit)
      (implicit 
        config: PropertyCheckConfig,
        genA: org.scalatest.prop.Generator[A]
      ): Unit = {
    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRnd: Rnd, initialSizes: List[Int]): Unit = {
      val (size, nextInitialSizes, nextRnd2) =
        initialSizes match {
          case head :: tail => (head, tail, nextRnd)
          case Nil =>
            val (sz, r2) = nextRnd.chooseInt(config.minSize, config.maxSize)
            (sz, Nil, r2)
        }
      val (v, r) = genA.next(size, nextRnd2)
      val result: Try[Unit] = Try { fun(v) }
      result match {
        case Success(()) =>
          val nextSucceededCount = succeededCount + 1
          if (nextSucceededCount < config.minSuccessful)
            loop(nextSucceededCount, discardedCount, r, nextInitialSizes)
        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < config.maxDiscarded)
            loop(succeededCount, nextDiscardedCount, r, nextInitialSizes)
          else throw new TestFailedException("too many discarded evaluations", 0)
        case Failure(ex) => throw ex
      }
    }
    // Make a List of 10 sizes between minSize and maxSize and sort them. Will
    // do those sizes first, and after that, use a random size between minSize and maxSize
    // The reason 10 is chosen is it will be the default minSuccessful, and o
    // When a different minSuccessful is used, we'll just generate random ones so
    // the max preallocated list will always have size 10.
    @tailrec
    def sizesLoop(sizes: List[Int], count: Int, rnd: Rnd): List[Int] = {
      sizes match {
        case Nil => sizesLoop(List(config.minSize), 1, rnd)
        case szs if count < 10 =>
          val (nextSize, nextRnd) = rnd.chooseInt(config.minSize, config.maxSize)
          sizesLoop(nextSize :: sizes, count + 1,  nextRnd)
        case _ => sizes.sorted
      }
    }
    val initialSizes = sizesLoop(Nil, 0, Rnd.default)
    loop(0, 0, Rnd.default, initialSizes)
  }
  def forAll[A, B](fun: (A, B) => Unit)
      (implicit 
        config: PropertyCheckConfig,
        genA: org.scalatest.prop.Generator[A],
        genB: org.scalatest.prop.Generator[B]
      ): Unit = {
    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRnd: Rnd): Unit = {
      val (a, ar) = genA.next(10, nextRnd)
      val (b, br) = genB.next(10, ar)
      val result: Try[Unit] = Try { fun(a, b) }
      result match {
        case Success(()) =>
          val nextSucceededCount = succeededCount + 1
          if (nextSucceededCount < config.minSuccessful)
            loop(nextSucceededCount, discardedCount, br)
        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < config.maxDiscarded)
            loop(succeededCount, nextDiscardedCount, br)
          else throw new TestFailedException("too many discarded evaluations", 0)
        case Failure(ex) => throw ex
      }
    }
    loop(0, 0, Rnd.default)
  }
  def forAll[A, B, C](fun: (A, B, C) => Unit)
      (implicit 
        config: PropertyCheckConfig,
        genA: org.scalatest.prop.Generator[A],
        genB: org.scalatest.prop.Generator[B],
        genC: org.scalatest.prop.Generator[C]
      ): Unit = {
    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRnd: Rnd): Unit = {
      val (a, ar) = genA.next(10, nextRnd)
      val (b, br) = genB.next(10, ar)
      val (c, cr) = genC.next(10, br)
      val result: Try[Unit] = Try { fun(a, b, c) }
      result match {
        case Success(()) =>
          val nextSucceededCount = succeededCount + 1
          if (nextSucceededCount < config.minSuccessful)
            loop(nextSucceededCount, discardedCount, cr)
        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < config.maxDiscarded)
            loop(succeededCount, nextDiscardedCount, cr)
          else throw new TestFailedException("too many discarded evaluations", 0)
        case Failure(ex) => throw ex
      }
    }
    loop(0, 0, Rnd.default)
  }
}

object GenDrivenPropertyChecks extends GenDrivenPropertyChecks

