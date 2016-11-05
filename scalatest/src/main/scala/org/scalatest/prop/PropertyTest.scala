/*
 * Copyright 2001-2013 Artima, Inc.
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

import org.scalatest.prop.Configuration.Parameter
import org.scalatest.{FailureMessages, UnquotedString, _}
import org.scalatest.exceptions.DiscardedEvaluationException

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

trait PropertyTest {

  type RESULT

  def check: PropertyTest.Result

  def succeed(v: RESULT): Boolean

  def checkForAll[A](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A])(fun: (A) => RESULT): PropertyTest.Result = {
    val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
    val maxSize = config.minSize + config.sizeRange

    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRandomizer: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, nextRandomizer2) =
        initialSizes match {
          case head :: tail => (head, tail, nextRandomizer)
          case Nil =>
            val (sz, r2) = nextRandomizer.chooseInt(config.minSize, maxSize)
            (sz, Nil, r2)
        }
      val (a, ar) = genA.next(size, nextRandomizer)

      val result: Try[RESULT] = Try { fun(a) }
      val argsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a))
      result match {
        case Success(r) =>
          if (succeed(r)) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, ar, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else
            new PropertyTest.CheckFailure(succeededCount, None, names, argsPassed)

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, ar, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

    @tailrec
    def sizesLoop(sizes: List[Int], count: Int, rnd: Randomizer): List[Int] = {
      sizes match {
        case Nil => sizesLoop(List(config.minSize), 1, rnd)
        case szs if count < 10 =>
          val (nextSize, nextRandomizer) = rnd.chooseInt(config.minSize, maxSize)
          sizesLoop(nextSize :: sizes, count + 1,  nextRandomizer)
        case _ => sizes.sorted
      }
    }
    val initialSizes = sizesLoop(Nil, 0, Randomizer.default)
    loop(0, 0, Randomizer.default, initialSizes)
  }

  def checkForAll[A, B](names: List[String], config: Parameter,
                                 genA: org.scalatest.prop.Generator[A],
                                 genB: org.scalatest.prop.Generator[B])
                                (fun: (A, B) => RESULT): PropertyTest.Result = {
    val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
    val maxSize = config.minSize + config.sizeRange

    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRandomizer: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, nextRandomizer2) =
        initialSizes match {
          case head :: tail => (head, tail, nextRandomizer)
          case Nil =>
            val (sz, r2) = nextRandomizer.chooseInt(config.minSize, maxSize)
            (sz, Nil, r2)
        }
      val (a, ar) = genA.next(size, nextRandomizer)
      val (b, br) = genB.next(size, ar)
      val result: Try[RESULT] = Try { fun(a, b) }
      val argsPassed =
        List(
          if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
          if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b)
        )
      result match {
        case Success(r) =>
          if (succeed(r)) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, br, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else
            new PropertyTest.CheckFailure(succeededCount, None, names, argsPassed)

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, br, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

    @tailrec
    def sizesLoop(sizes: List[Int], count: Int, rnd: Randomizer): List[Int] = {
      sizes match {
        case Nil => sizesLoop(List(config.minSize), 1, rnd)
        case szs if count < 10 =>
          val (nextSize, nextRandomizer) = rnd.chooseInt(config.minSize, maxSize)
          sizesLoop(nextSize :: sizes, count + 1,  nextRandomizer)
        case _ => sizes.sorted
      }
    }
    val initialSizes = sizesLoop(Nil, 0, Randomizer.default)
    loop(0, 0, Randomizer.default, initialSizes)
  }

  def checkForAll[A, B, C](names: List[String], config: Parameter,
                                    genA: org.scalatest.prop.Generator[A],
                                    genB: org.scalatest.prop.Generator[B],
                                    genC: org.scalatest.prop.Generator[C])
                                   (fun: (A, B, C) => RESULT): PropertyTest.Result = {
    val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
    val maxSize = config.minSize + config.sizeRange

    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRandomizer: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, nextRandomizer2) =
        initialSizes match {
          case head :: tail => (head, tail, nextRandomizer)
          case Nil =>
            val (sz, r2) = nextRandomizer.chooseInt(config.minSize, maxSize)
            (sz, Nil, r2)
        }
      val (a, ar) = genA.next(size, nextRandomizer)
      val (b, br) = genB.next(size, ar)
      val (c, cr) = genC.next(size, br)
      val result: Try[RESULT] = Try { fun(a, b, c) }
      val argsPassed =
        List(
          if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
          if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b),
          if (names.isDefinedAt(2)) PropertyArgument(Some(names(2)), c) else PropertyArgument(None, c)
        )
      result match {
        case Success(r) =>
          if (succeed(r)) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, cr, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else
            new PropertyTest.CheckFailure(succeededCount, None, names, argsPassed)

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, cr, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

    @tailrec
    def sizesLoop(sizes: List[Int], count: Int, rnd: Randomizer): List[Int] = {
      sizes match {
        case Nil => sizesLoop(List(config.minSize), 1, rnd)
        case szs if count < 10 =>
          val (nextSize, nextRandomizer) = rnd.chooseInt(config.minSize, maxSize)
          sizesLoop(nextSize :: sizes, count + 1,  nextRandomizer)
        case _ => sizes.sorted
      }
    }
    val initialSizes = sizesLoop(Nil, 0, Randomizer.default)
    loop(0, 0, Randomizer.default, initialSizes)
  }

  def checkForAll[A, B, C, D](names: List[String], config: Parameter,
                                       genA: org.scalatest.prop.Generator[A],
                                       genB: org.scalatest.prop.Generator[B],
                                       genC: org.scalatest.prop.Generator[C],
                                       genD: org.scalatest.prop.Generator[D])
                                      (fun: (A, B, C, D) => RESULT): PropertyTest.Result = {
    val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
    val maxSize = config.minSize + config.sizeRange

    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRandomizer: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, nextRandomizer2) =
        initialSizes match {
          case head :: tail => (head, tail, nextRandomizer)
          case Nil =>
            val (sz, r2) = nextRandomizer.chooseInt(config.minSize, maxSize)
            (sz, Nil, r2)
        }
      val (a, ar) = genA.next(size, nextRandomizer)
      val (b, br) = genB.next(size, ar)
      val (c, cr) = genC.next(size, br)
      val (d, dr) = genD.next(size, cr)
      val result: Try[RESULT] = Try { fun(a, b, c, d) }
      val argsPassed =
        List(
          if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
          if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b),
          if (names.isDefinedAt(2)) PropertyArgument(Some(names(2)), c) else PropertyArgument(None, c),
          if (names.isDefinedAt(3)) PropertyArgument(Some(names(3)), d) else PropertyArgument(None, d)
        )
      result match {
        case Success(r) =>
          if (succeed(r)) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, dr, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else
            new PropertyTest.CheckFailure(succeededCount, None, names, argsPassed)

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, dr, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

    @tailrec
    def sizesLoop(sizes: List[Int], count: Int, rnd: Randomizer): List[Int] = {
      sizes match {
        case Nil => sizesLoop(List(config.minSize), 1, rnd)
        case szs if count < 10 =>
          val (nextSize, nextRandomizer) = rnd.chooseInt(config.minSize, maxSize)
          sizesLoop(nextSize :: sizes, count + 1,  nextRandomizer)
        case _ => sizes.sorted
      }
    }
    val initialSizes = sizesLoop(Nil, 0, Randomizer.default)
    loop(0, 0, Randomizer.default, initialSizes)
  }

  def checkForAll[A, B, C, D, E](names: List[String], config: Parameter,
                                          genA: org.scalatest.prop.Generator[A],
                                          genB: org.scalatest.prop.Generator[B],
                                          genC: org.scalatest.prop.Generator[C],
                                          genD: org.scalatest.prop.Generator[D],
                                          genE: org.scalatest.prop.Generator[E])
                                         (fun: (A, B, C, D, E) => RESULT): PropertyTest.Result = {
    val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
    val maxSize = config.minSize + config.sizeRange

    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRandomizer: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, nextRandomizer2) =
        initialSizes match {
          case head :: tail => (head, tail, nextRandomizer)
          case Nil =>
            val (sz, r2) = nextRandomizer.chooseInt(config.minSize, maxSize)
            (sz, Nil, r2)
        }
      val (a, ar) = genA.next(size, nextRandomizer)
      val (b, br) = genB.next(size, ar)
      val (c, cr) = genC.next(size, br)
      val (d, dr) = genD.next(size, cr)
      val (e, er) = genE.next(size, dr)
      val result: Try[RESULT] = Try { fun(a, b, c, d, e) }
      val argsPassed =
        List(
          if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
          if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b),
          if (names.isDefinedAt(2)) PropertyArgument(Some(names(2)), c) else PropertyArgument(None, c),
          if (names.isDefinedAt(3)) PropertyArgument(Some(names(3)), d) else PropertyArgument(None, d),
          if (names.isDefinedAt(4)) PropertyArgument(Some(names(4)), e) else PropertyArgument(None, e)
        )
      result match {
        case Success(r) =>
          if (succeed(r)) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, er, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else
            new PropertyTest.CheckFailure(succeededCount, None, names, argsPassed)

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, er, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

    @tailrec
    def sizesLoop(sizes: List[Int], count: Int, rnd: Randomizer): List[Int] = {
      sizes match {
        case Nil => sizesLoop(List(config.minSize), 1, rnd)
        case szs if count < 10 =>
          val (nextSize, nextRandomizer) = rnd.chooseInt(config.minSize, maxSize)
          sizesLoop(nextSize :: sizes, count + 1,  nextRandomizer)
        case _ => sizes.sorted
      }
    }
    val initialSizes = sizesLoop(Nil, 0, Randomizer.default)
    loop(0, 0, Randomizer.default, initialSizes)
  }

  def checkForAll[A, B, C, D, E, F](names: List[String], config: Parameter,
                                             genA: org.scalatest.prop.Generator[A],
                                             genB: org.scalatest.prop.Generator[B],
                                             genC: org.scalatest.prop.Generator[C],
                                             genD: org.scalatest.prop.Generator[D],
                                             genE: org.scalatest.prop.Generator[E],
                                             genF: org.scalatest.prop.Generator[F])
                                            (fun: (A, B, C, D, E, F) => RESULT): PropertyTest.Result = {
    val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
    val maxSize = config.minSize + config.sizeRange

    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, nextRandomizer: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, nextRandomizer2) =
        initialSizes match {
          case head :: tail => (head, tail, nextRandomizer)
          case Nil =>
            val (sz, r2) = nextRandomizer.chooseInt(config.minSize, maxSize)
            (sz, Nil, r2)
        }
      val (a, ar) = genA.next(size, nextRandomizer)
      val (b, br) = genB.next(size, ar)
      val (c, cr) = genC.next(size, br)
      val (d, dr) = genD.next(size, cr)
      val (e, er) = genE.next(size, dr)
      val (f, fr) = genF.next(size, er)
      val result: Try[RESULT] = Try { fun(a, b, c, d, e, f) }
      val argsPassed =
        List(
          if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
          if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b),
          if (names.isDefinedAt(2)) PropertyArgument(Some(names(2)), c) else PropertyArgument(None, c),
          if (names.isDefinedAt(3)) PropertyArgument(Some(names(3)), d) else PropertyArgument(None, d),
          if (names.isDefinedAt(4)) PropertyArgument(Some(names(4)), e) else PropertyArgument(None, e),
          if (names.isDefinedAt(5)) PropertyArgument(Some(names(5)), f) else PropertyArgument(None, f)
        )
      result match {
        case Success(r) =>
          if (succeed(r)) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, fr, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else {
            new PropertyTest.CheckFailure(succeededCount, None, names, argsPassed)
          }

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, fr, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

    @tailrec
    def sizesLoop(sizes: List[Int], count: Int, rnd: Randomizer): List[Int] = {
      sizes match {
        case Nil => sizesLoop(List(config.minSize), 1, rnd)
        case szs if count < 10 =>
          val (nextSize, nextRandomizer) = rnd.chooseInt(config.minSize, maxSize)
          sizesLoop(nextSize :: sizes, count + 1,  nextRandomizer)
        case _ => sizes.sorted
      }
    }
    val initialSizes = sizesLoop(Nil, 0, Randomizer.default)
    loop(0, 0, Randomizer.default, initialSizes)
  }
}

object PropertyTest {

  sealed trait Result

  case class CheckSuccess(args: List[PropertyArgument]) extends Result

  case class CheckExhausted(succeeded: Long, discarded: Long, names: List[String], argsPassed: List[PropertyArgument]) extends Result

  case class CheckFailure(succeeded: Long, ex: Option[Throwable], names: List[String], argsPassed: List[PropertyArgument]) extends Result

  def forAll1[A, ASSERTION](names: List[String], config: Parameter)(fun: (A) => ASSERTION)
                                   (implicit
                                    genA: org.scalatest.prop.Generator[A],
                                    resultChecker: PropertyTestResultHandler[ASSERTION]
                                   ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT): Boolean = resultChecker.succeed(v)
      def check: Result = checkForAll(names, config, genA)(fun)
    }

  def forAll2[A, B, ASSERTION](names: List[String], config: Parameter)(fun: (A, B) => ASSERTION)
                                   (implicit
                                     genA: org.scalatest.prop.Generator[A],
                                     genB: org.scalatest.prop.Generator[B],
                                     resultChecker: PropertyTestResultHandler[ASSERTION]
                                   ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT): Boolean = resultChecker.succeed(v)
      def check: Result = checkForAll(names, config, genA, genB)(fun)
    }

  def forAll3[A, B, C, ASSERTION](names: List[String], config: Parameter)(fun: (A, B, C) => ASSERTION)
                                   (implicit
                                     genA: org.scalatest.prop.Generator[A],
                                     genB: org.scalatest.prop.Generator[B],
                                     genC: org.scalatest.prop.Generator[C],
                                     resultChecker: PropertyTestResultHandler[ASSERTION]
                                   ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT): Boolean = resultChecker.succeed(v)
      def check: Result = checkForAll(names, config, genA, genB, genC)(fun)
    }

  def forAll4[A, B, C, D, ASSERTION](names: List[String], config: Parameter)(fun: (A, B, C, D) => ASSERTION)
                                      (implicit
                                        genA: org.scalatest.prop.Generator[A],
                                        genB: org.scalatest.prop.Generator[B],
                                        genC: org.scalatest.prop.Generator[C],
                                        genD: org.scalatest.prop.Generator[D],
                                        resultChecker: PropertyTestResultHandler[ASSERTION]
                                      ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT): Boolean = resultChecker.succeed(v)
      def check: Result = checkForAll(names, config, genA, genB, genC, genD)(fun)
    }

  def forAll5[A, B, C, D, E, ASSERTION](names: List[String], config: Parameter)(fun: (A, B, C, D, E) => ASSERTION)
                                         (implicit
                                           genA: org.scalatest.prop.Generator[A],
                                           genB: org.scalatest.prop.Generator[B],
                                           genC: org.scalatest.prop.Generator[C],
                                           genD: org.scalatest.prop.Generator[D],
                                           genE: org.scalatest.prop.Generator[E],
                                           resultChecker: PropertyTestResultHandler[ASSERTION]
                                         ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT): Boolean = resultChecker.succeed(v)
      def check: Result = checkForAll(names, config, genA, genB, genC, genD, genE)(fun)
    }

  def forAll6[A, B, C, D, E, F, ASSERTION](names: List[String], config: Parameter)(fun: (A, B, C, D, E, F) => ASSERTION)
                                                  (implicit
                                                   genA: org.scalatest.prop.Generator[A],
                                                   genB: org.scalatest.prop.Generator[B],
                                                   genC: org.scalatest.prop.Generator[C],
                                                   genD: org.scalatest.prop.Generator[D],
                                                   genE: org.scalatest.prop.Generator[E],
                                                   genF: org.scalatest.prop.Generator[F],
                                                   resultChecker: PropertyTestResultHandler[ASSERTION]
                                                  ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT): Boolean = resultChecker.succeed(v)
      def check: Result = checkForAll(names, config, genA, genB, genC, genD, genE, genF)(fun)
    }

}