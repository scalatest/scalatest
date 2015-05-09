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
        genA: org.scalatest.prop.Gen[A]
      ): Unit = {
    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRnd: Rnd): Unit = {
      val (v, r) = genA.next(10, nextRnd)
      val result: Try[Unit] = Try { fun(v) }
      result match {
        case Success(()) =>
          val nextSucceededCount = succeededCount + 1
          if (nextSucceededCount < config.minSuccessful)
            loop(nextSucceededCount, discardedCount, r)
        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < config.maxDiscarded)
            loop(succeededCount, nextDiscardedCount, r)
          else throw new TestFailedException("too many discarded evaluations", 0)
        case Failure(ex) => throw ex
      }
    }
    loop(0, 0, Rnd.default)
  }
  def forAll[A, B](fun: (A, B) => Unit)
      (implicit 
        config: PropertyCheckConfig,
        genA: org.scalatest.prop.Gen[A],
        genB: org.scalatest.prop.Gen[B]
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
        genA: org.scalatest.prop.Gen[A],
        genB: org.scalatest.prop.Gen[B],
        genC: org.scalatest.prop.Gen[C]
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

