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
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException
import org.scalatest.exceptions.StackDepth
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString

// For now, hard coding a size of 10. Later will need to do the size based on config
trait GenDrivenPropertyChecks extends Configuration with Whenever {
  import GenDrivenPropertyChecks.stackDepthFileName
  import GenDrivenPropertyChecks.stackDepthMethodName
  import GenDrivenPropertyChecks.prettyArgs
  def forAll[A](fun: (A) => Unit)
      (implicit 
        config: PropertyCheckConfig,
        genA: org.scalatest.prop.Generator[A]
      ): Unit = {
    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRandomizer: Randomizer, initialSizes: List[Int]): Unit = {
      val (size, nextInitialSizes, nextRandomizer2) =
        initialSizes match {
          case head :: tail => (head, tail, nextRandomizer)
          case Nil =>
            val (sz, r2) = nextRandomizer.chooseInt(config.minSize, config.maxSize)
            (sz, Nil, r2)
        }
      val (v, r) = genA.next(size, nextRandomizer2)
      val result: Try[Unit] = Try { fun(v) }
      val argsPassed = List(v)
      val scalaCheckLabels = Set.empty[String]
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
        case Failure(ex) => 
          throw new GeneratorDrivenPropertyCheckFailedException(
            sde => FailureMessages.propertyException(UnquotedString(sde.getClass.getSimpleName)) + "\n" +
              ( sde.failedCodeFileNameAndLineNumberString match { case Some(s) => " (" + s + ")"; case None => "" }) + "\n" + 
              "  " + FailureMessages.propertyFailed(succeededCount) + "\n" +
              (
                sde match {
                  case sd: StackDepth if sd.failedCodeFileNameAndLineNumberString.isDefined =>
                    "  " + FailureMessages.thrownExceptionsLocation(UnquotedString(sd.failedCodeFileNameAndLineNumberString.get)) + "\n"
                  case _ => ""
                }
              ) +
              "  " + FailureMessages.occurredOnValues + "\n" +
              prettyArgs(argsPassed) + "\n" +
              "  )" +
              "", // getLabelDisplay(scalaCheckLabels),
            Some(ex),
            getStackDepthFun(stackDepthFileName, stackDepthMethodName),
            None,
            FailureMessages.propertyFailed(succeededCount),
            argsPassed,
            None,
            scalaCheckLabels.toList
          )
      }
    }
    // Make a List of 10 sizes between minSize and maxSize and sort them. Will
    // do those sizes first, and after that, use a random size between minSize and maxSize
    // The reason 10 is chosen is it will be the default minSuccessful, and o
    // When a different minSuccessful is used, we'll just generate random ones so
    // the max preallocated list will always have size 10.
    @tailrec
    def sizesLoop(sizes: List[Int], count: Int, rnd: Randomizer): List[Int] = {
      sizes match {
        case Nil => sizesLoop(List(config.minSize), 1, rnd)
        case szs if count < 10 =>
          val (nextSize, nextRandomizer) = rnd.chooseInt(config.minSize, config.maxSize)
          sizesLoop(nextSize :: sizes, count + 1,  nextRandomizer)
        case _ => sizes.sorted
      }
    }
    val initialSizes = sizesLoop(Nil, 0, Randomizer.default)
    loop(0, 0, Randomizer.default, initialSizes)
  }
  def forAll[A, B](fun: (A, B) => Unit)
      (implicit 
        config: PropertyCheckConfig,
        genA: org.scalatest.prop.Generator[A],
        genB: org.scalatest.prop.Generator[B]
      ): Unit = {
    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRandomizer: Randomizer): Unit = {
      val (a, ar) = genA.next(10, nextRandomizer)
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
    loop(0, 0, Randomizer.default)
  }
  def forAll[A, B, C](fun: (A, B, C) => Unit)
      (implicit 
        config: PropertyCheckConfig,
        genA: org.scalatest.prop.Generator[A],
        genB: org.scalatest.prop.Generator[B],
        genC: org.scalatest.prop.Generator[C]
      ): Unit = {
    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRandomizer: Randomizer): Unit = {
      val (a, ar) = genA.next(10, nextRandomizer)
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
    loop(0, 0, Randomizer.default)
  }
}

object GenDrivenPropertyChecks extends GenDrivenPropertyChecks {
  private val stackDepthFileName = "GenDrivenPropertyChecks.scala"
  private val stackDepthMethodName = "apply"
  import FailureMessages.decorateToStringValue
  private def prettyArgs(args: List[Any]) = {
    val strs = for((a, i) <- args.zipWithIndex) yield (
      "    " +
      ("arg" + i) +
      " = " + decorateToStringValue(a) + (if (i < args.length - 1) "," else "") // +
      // (if (a.shrinks > 0) " // " + a.shrinks + (if (a.shrinks == 1) " shrink" else " shrinks") else "")
    )
    strs.mkString("\n")
  }
}

