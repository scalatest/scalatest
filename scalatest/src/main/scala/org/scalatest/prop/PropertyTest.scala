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
import org.scalatest.enablers.PropCheckerAsserting

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

trait PropertyTest {

  type RESULT

  def check: PropertyTest.Result

  def succeed(v: RESULT): (Boolean, Option[Throwable])

  def checkForAll[A](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A])(fun: (A) => RESULT): PropertyTest.Result = {
    val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
    val minSize = config.minSize
    val maxSize = minSize + config.sizeRange
    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, edges: List[A], rnd: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, nextRnd) =
        initialSizes match {
          case head :: tail => (head, tail, rnd)
          case Nil =>
            val (sz, nextRnd) = rnd.chooseInt(minSize, maxSize)
            (sz, Nil, nextRnd)
        }
      val (a, nextEdges, nextNextRnd) = genA.next(size, edges, nextRnd)

      val result: Try[RESULT] = Try { fun(a) }
      val argsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a))
      result match {
        case Success(r) =>
          val (success, cause) = succeed(r)
          if (success) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, nextEdges, nextNextRnd, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else
            new PropertyTest.CheckFailure(succeededCount, cause, names, argsPassed)

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, nextEdges, nextNextRnd, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

/*
    @tailrec
    def sizesLoop(sizes: List[Int], count: Int, rndm: Randomizer): (List[Int], Randomizer) = {
      sizes match {
        case Nil => sizesLoop(List(config.minSize), 1, rndm)
        case szs if count < 10 =>
          val (nextSize, nextRndm) = rndm.chooseInt(config.minSize, maxSize)
          sizesLoop(nextSize :: sizes, count + 1, nextRndm)
        case _ => (sizes.sorted, rndm)
      }
    }
*/
/*
Here I could grab config.minSuccessful and maybe divide it by 5, so that at most 20% are edges? If it is 10, then we get 2 edges.
If 100, we get at most 20 edges. I could say
We'd need to grab the random. Which could be random.default unless it is specified on the command line. I'd say it is the
one and only? Yes, I think so. Each forAll uses the same key each run. And we just print it out. I can put it in the summary.
Maybe they need to ask for it?
val edgesA: List[A] = genA.edges(maxEdges, rnd)
I'd then just feed the edges through along with the randomizer.
*/
    val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
    val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
    val (initEdges, afterEdgesRnd) = genA.initEdges(config.minSuccessful / 5, afterSizesRnd)
    loop(0, 0, initEdges, afterEdgesRnd, initialSizes) // We may need to be able to pass in a oh, pass in a key? Or grab it from the outside via cmd ln parm?
  }

  def checkForAll[A, B](names: List[String], config: Parameter,
                                 genA: org.scalatest.prop.Generator[A],
                                 genB: org.scalatest.prop.Generator[B])
                                (fun: (A, B) => RESULT): PropertyTest.Result = {
    val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
    val minSize = config.minSize
    val maxSize = minSize + config.sizeRange

    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], rnd: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, rnd1) =
        initialSizes match {
          case head :: tail => (head, tail, rnd)
          case Nil =>
            val (sz, nextRnd) = rnd.chooseInt(minSize, maxSize)
            (sz, Nil, nextRnd)
        }
      val (a, nextAEdges, rnd2) = genA.next(size, aEdges, rnd1)
      val (b, nextBEdges, rnd3) = genB.next(size, bEdges, rnd2)
      val result: Try[RESULT] = Try { fun(a, b) }
      val argsPassed =
        List(
          if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
          if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b)
        )
      result match {
        case Success(r) =>
          val (success, cause) = succeed(r)
          if (success) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, rnd3, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else
            new PropertyTest.CheckFailure(succeededCount, cause, names, argsPassed)

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, rnd3, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

/*
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
*/

    val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
    val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
    val maxEdges = config.minSuccessful / 5
    val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
    val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
    loop(0, 0, initAEdges, initBEdges, afterBEdgesRnd, initialSizes)
  }

  def checkForAll[A, B, C](names: List[String], config: Parameter,
                                    genA: org.scalatest.prop.Generator[A],
                                    genB: org.scalatest.prop.Generator[B],
                                    genC: org.scalatest.prop.Generator[C])
                                   (fun: (A, B, C) => RESULT): PropertyTest.Result = {
    val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
    val minSize = config.minSize
    val maxSize = minSize + config.sizeRange

    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], rnd: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, rnd1) =
        initialSizes match {
          case head :: tail => (head, tail, rnd)
          case Nil =>
            val (sz, nextRnd) = rnd.chooseInt(minSize, maxSize)
            (sz, Nil, nextRnd)
        }
      val (a, nextAEdges, rnd2) = genA.next(size, aEdges, rnd1)
      val (b, nextBEdges, rnd3) = genB.next(size, bEdges, rnd2)
      val (c, nextCEdges, rnd4) = genC.next(size, cEdges, rnd3)
      val result: Try[RESULT] = Try { fun(a, b, c) }
      val argsPassed =
        List(
          if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
          if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b),
          if (names.isDefinedAt(2)) PropertyArgument(Some(names(2)), c) else PropertyArgument(None, c)
        )
      result match {
        case Success(r) =>
          val (success, cause) = succeed(r)
          if (success) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, rnd4, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else
            new PropertyTest.CheckFailure(succeededCount, cause, names, argsPassed)

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, rnd4, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

/*
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
*/

    val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
    val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
    val maxEdges = config.minSuccessful / 5
    val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
    val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
    val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
    loop(0, 0, initAEdges, initBEdges, initCEdges, afterCEdgesRnd, initialSizes)
  }

  def checkForAll[A, B, C, D](names: List[String], config: Parameter,
                                       genA: org.scalatest.prop.Generator[A],
                                       genB: org.scalatest.prop.Generator[B],
                                       genC: org.scalatest.prop.Generator[C],
                                       genD: org.scalatest.prop.Generator[D])
                                      (fun: (A, B, C, D) => RESULT): PropertyTest.Result = {
    val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
    val minSize = config.minSize
    val maxSize = minSize + config.sizeRange

    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], rnd: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, rnd1) =
        initialSizes match {
          case head :: tail => (head, tail, rnd)
          case Nil =>
            val (sz, nextRnd) = rnd.chooseInt(minSize, maxSize)
            (sz, Nil, nextRnd)
        }
      val (a, nextAEdges, rnd2) = genA.next(size, aEdges, rnd1)
      val (b, nextBEdges, rnd3) = genB.next(size, bEdges, rnd2)
      val (c, nextCEdges, rnd4) = genC.next(size, cEdges, rnd3)
      val (d, nextDEdges, rnd5) = genD.next(size, dEdges, rnd4)
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
          val (success, cause) = succeed(r)
          if (success) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, rnd5, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else
            new PropertyTest.CheckFailure(succeededCount, cause, names, argsPassed)

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, rnd5, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

/*
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
*/

    val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
    val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
    val maxEdges = config.minSuccessful / 5
    val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
    val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
    val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
    val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
    loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, afterDEdgesRnd, initialSizes)
  }

  def checkForAll[A, B, C, D, E](names: List[String], config: Parameter,
                                          genA: org.scalatest.prop.Generator[A],
                                          genB: org.scalatest.prop.Generator[B],
                                          genC: org.scalatest.prop.Generator[C],
                                          genD: org.scalatest.prop.Generator[D],
                                          genE: org.scalatest.prop.Generator[E])
                                         (fun: (A, B, C, D, E) => RESULT): PropertyTest.Result = {
    val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
    val minSize = config.minSize
    val maxSize = minSize + config.sizeRange

    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], rnd: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, rnd1) =
        initialSizes match {
          case head :: tail => (head, tail, rnd)
          case Nil =>
            val (sz, nextRnd) = rnd.chooseInt(minSize, maxSize)
            (sz, Nil, nextRnd)
        }
      val (a, nextAEdges, rnd2) = genA.next(size, aEdges, rnd1)
      val (b, nextBEdges, rnd3) = genB.next(size, bEdges, rnd2)
      val (c, nextCEdges, rnd4) = genC.next(size, cEdges, rnd3)
      val (d, nextDEdges, rnd5) = genD.next(size, dEdges, rnd4)
      val (e, nextEEdges, rnd6) = genE.next(size, eEdges, rnd5)
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
          val (success, cause) = succeed(r)
          if (success) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, rnd6, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else
            new PropertyTest.CheckFailure(succeededCount, cause, names, argsPassed)

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, rnd6, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

/*
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
*/

    val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
    val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
    val maxEdges = config.minSuccessful / 5
    val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
    val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
    val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
    val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
    val (initEEdges, afterEEdgesRnd) = genE.initEdges(maxEdges, afterDEdgesRnd)
    loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, initEEdges, afterEEdgesRnd, initialSizes)
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
    val minSize = config.minSize
    val maxSize = minSize + config.sizeRange

    @tailrec
    def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], fEdges: List[F], rnd: Randomizer, initialSizes: List[Int]): PropertyTest.Result = {
      val (size, nextInitialSizes, rnd1) =
        initialSizes match {
          case head :: tail => (head, tail, rnd)
          case Nil =>
            val (sz, nextRnd) = rnd.chooseInt(minSize, maxSize)
            (sz, Nil, nextRnd)
        }
      val (a, nextAEdges, rnd2) = genA.next(size, aEdges, rnd1)
      val (b, nextBEdges, rnd3) = genB.next(size, bEdges, rnd2)
      val (c, nextCEdges, rnd4) = genC.next(size, cEdges, rnd3)
      val (d, nextDEdges, rnd5) = genD.next(size, dEdges, rnd4)
      val (e, nextEEdges, rnd6) = genE.next(size, eEdges, rnd5)
      val (f, nextFEdges, rnd7) = genF.next(size, fEdges, rnd6)
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
          val (success, cause) = succeed(r)
          if (success) {
            val nextSucceededCount = succeededCount + 1
            if (nextSucceededCount < config.minSuccessful)
              loop(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, rnd7, nextInitialSizes)
            else
              PropertyTest.CheckSuccess(argsPassed)
          }
          else {
            new PropertyTest.CheckFailure(succeededCount, cause, names, argsPassed)
          }

        case Failure(ex: DiscardedEvaluationException) =>
          val nextDiscardedCount = discardedCount + 1
          if (nextDiscardedCount < maxDiscarded)
            loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, rnd7, nextInitialSizes)
          else
            new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
        case Failure(ex) =>
          new PropertyTest.CheckFailure(succeededCount, Some(ex), names, argsPassed)
      }
    }

/*
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
*/

    val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
    val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
    val maxEdges = config.minSuccessful / 5
    val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
    val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
    val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
    val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
    val (initEEdges, afterEEdgesRnd) = genE.initEdges(maxEdges, afterDEdgesRnd)
    val (initFEdges, afterFEdgesRnd) = genF.initEdges(maxEdges, afterEEdgesRnd)
    loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, initEEdges, initFEdges, afterFEdgesRnd, initialSizes)
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
                                    asserting: PropCheckerAsserting[ASSERTION]
                                   ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT) = asserting.succeed(v)
      def check: Result = checkForAll(names, config, genA)(fun)
    }

  def forAll2[A, B, ASSERTION](names: List[String], config: Parameter)(fun: (A, B) => ASSERTION)
                                   (implicit
                                     genA: org.scalatest.prop.Generator[A],
                                     genB: org.scalatest.prop.Generator[B],
                                     asserting: PropCheckerAsserting[ASSERTION]
                                   ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT) = asserting.succeed(v)
      def check: Result = checkForAll(names, config, genA, genB)(fun)
    }

  def forAll3[A, B, C, ASSERTION](names: List[String], config: Parameter)(fun: (A, B, C) => ASSERTION)
                                   (implicit
                                     genA: org.scalatest.prop.Generator[A],
                                     genB: org.scalatest.prop.Generator[B],
                                     genC: org.scalatest.prop.Generator[C],
                                     asserting: PropCheckerAsserting[ASSERTION]
                                   ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT) = asserting.succeed(v)
      def check: Result = checkForAll(names, config, genA, genB, genC)(fun)
    }

  def forAll4[A, B, C, D, ASSERTION](names: List[String], config: Parameter)(fun: (A, B, C, D) => ASSERTION)
                                      (implicit
                                        genA: org.scalatest.prop.Generator[A],
                                        genB: org.scalatest.prop.Generator[B],
                                        genC: org.scalatest.prop.Generator[C],
                                        genD: org.scalatest.prop.Generator[D],
                                        asserting: PropCheckerAsserting[ASSERTION]
                                      ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT) = asserting.succeed(v)
      def check: Result = checkForAll(names, config, genA, genB, genC, genD)(fun)
    }

  def forAll5[A, B, C, D, E, ASSERTION](names: List[String], config: Parameter)(fun: (A, B, C, D, E) => ASSERTION)
                                         (implicit
                                           genA: org.scalatest.prop.Generator[A],
                                           genB: org.scalatest.prop.Generator[B],
                                           genC: org.scalatest.prop.Generator[C],
                                           genD: org.scalatest.prop.Generator[D],
                                           genE: org.scalatest.prop.Generator[E],
                                           asserting: PropCheckerAsserting[ASSERTION]
                                         ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT) = asserting.succeed(v)
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
                                                   asserting: PropCheckerAsserting[ASSERTION]
                                                  ): PropertyTest =
    new PropertyTest {
      type RESULT = ASSERTION
      def succeed(v: RESULT) = asserting.succeed(v)
      def check: Result = checkForAll(names, config, genA, genB, genC, genD, genE, genF)(fun)
    }

    def calcSizes(minSize: Int, maxSize: Int, initRndm: Randomizer): (List[Int], Randomizer) = {
      @tailrec
      def sizesLoop(sizes: List[Int], count: Int, rndm: Randomizer): (List[Int], Randomizer) = {
        sizes match {
          case Nil => sizesLoop(List(minSize), 1, rndm)
          case szs if count < 10 =>
            val (nextSize, nextRndm) = rndm.chooseInt(minSize, maxSize)
            sizesLoop(nextSize :: sizes, count + 1, nextRndm)
          case _ => (sizes.sorted, rndm)
      }
    }
    sizesLoop(Nil, 0, initRndm)
  }
}
