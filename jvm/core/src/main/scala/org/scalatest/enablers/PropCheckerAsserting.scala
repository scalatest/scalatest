/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalatest.enablers

import org.scalactic.{Prettifier, source}
import org.scalatest.exceptions.{StackDepth, StackDepthException, GeneratorDrivenPropertyCheckFailedException}
import org.scalatest.prop.{Configuration, PropertyArgument, PropertyCheckResult}
import org.scalatest.{FailureMessages, Resources, UnquotedString, Fact, Expectation, Assertion, Succeeded}
import FailureMessages.decorateToStringValue
import org.scalactic.anyvals.PosZInt
import org.scalatest.prop.Randomizer
import scala.annotation.tailrec
import org.scalatest.prop.Configuration.Parameter
import org.scalatest.prop.{SizeParam}
import scala.util.{Try, Success, Failure}
import org.scalatest.exceptions.DiscardedEvaluationException
import scala.concurrent.Future
import scala.compat.Platform.EOL

trait PropCheckerAsserting[T] {

  /**
    * The result type of the <code>check</code> method.
    */
  type Result

  type S

  def discard(result: S): Boolean

  def succeed(result: S): (Boolean, Option[Throwable])

  //private[scalatest] def indicateSuccess(message: => String): Result

  //private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Result

  /**
    * Perform the property check using the given function, generator and <code>Configuration.Parameters</code>.
    *
    * @param fun the function to be used to check
    * @param genA the generator of type <code>A</code>
    * @param prms the <code>Configuration.Parameters</code> to be used to check
    * @param prettifier the <code>Prettifier</code> to be used to prettify error message
    * @param pos the <code>Position</code> of the caller site
    * @param names the list of names
    * @param argNames the list of argument names
    * @return the <code>Result</code> of the property check.
    */
  def check1[A](fun: (A) => T,
               genA: org.scalatest.prop.Generator[A],
               prms: Configuration.Parameter,
               prettifier: Prettifier,
               pos: source.Position,
               names: List[String],
               argNames: Option[List[String]] = None): Result

  def check2[A, B](fun: (A, B) => T,
                   genA: org.scalatest.prop.Generator[A],
                   genB: org.scalatest.prop.Generator[B],
                   prms: Configuration.Parameter,
                   prettifier: Prettifier,
                   pos: source.Position,
                   names: List[String],
                   argNames: Option[List[String]] = None): Result

  def check3[A, B, C](fun: (A, B, C) => T,
                      genA: org.scalatest.prop.Generator[A],
                      genB: org.scalatest.prop.Generator[B],
                      genC: org.scalatest.prop.Generator[C],
                      prms: Configuration.Parameter,
                      prettifier: Prettifier,
                      pos: source.Position,
                      names: List[String],
                      argNames: Option[List[String]] = None): Result

  def check4[A, B, C, D](fun: (A, B, C, D) => T,
                         genA: org.scalatest.prop.Generator[A],
                         genB: org.scalatest.prop.Generator[B],
                         genC: org.scalatest.prop.Generator[C],
                         genD: org.scalatest.prop.Generator[D],
                         prms: Configuration.Parameter,
                         prettifier: Prettifier,
                         pos: source.Position,
                         names: List[String],
                         argNames: Option[List[String]] = None): Result

  def check5[A, B, C, D, E](fun: (A, B, C, D, E) => T,
                            genA: org.scalatest.prop.Generator[A],
                            genB: org.scalatest.prop.Generator[B],
                            genC: org.scalatest.prop.Generator[C],
                            genD: org.scalatest.prop.Generator[D],
                            genE: org.scalatest.prop.Generator[E],
                            prms: Configuration.Parameter,
                            prettifier: Prettifier,
                            pos: source.Position,
                            names: List[String],
                            argNames: Option[List[String]] = None): Result

  def check6[A, B, C, D, E, F](fun: (A, B, C, D, E, F) => T,
                               genA: org.scalatest.prop.Generator[A],
                               genB: org.scalatest.prop.Generator[B],
                               genC: org.scalatest.prop.Generator[C],
                               genD: org.scalatest.prop.Generator[D],
                               genE: org.scalatest.prop.Generator[E],
                               genF: org.scalatest.prop.Generator[F],
                               prms: Configuration.Parameter,
                               prettifier: Prettifier,
                               pos: source.Position,
                               names: List[String],
                               argNames: Option[List[String]] = None): Result

}

/**
  * Class holding lowest priority <code>CheckerAsserting</code> implicit, which enables [[org.scalatest.prop.GeneratorDrivenPropertyChecks GeneratorDrivenPropertyChecks]] expressions that have result type <code>Unit</code>.
  */
abstract class UnitPropCheckerAsserting {

  import PropCheckerAsserting._

  abstract class PropCheckerAssertingImpl[T] extends PropCheckerAsserting[T] {

    type S = T

    private def checkForAll[A](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A])(fun: (A) => T): PropertyCheckResult = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, edges: List[A], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): PropertyCheckResult = {
        val (size, nextInitialSizes, nextRnd) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextEdges, nextNextRnd) = genA.next(SizeParam(PosZInt(0), maxSize, size), edges, nextRnd) // TODO: Move PosZInt farther out

        val result: Try[T] = Try { fun(a) }
        val argsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a))
        result match {
          case Success(r) =>
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                loop(succeededCount, nextDiscardedCount, nextEdges, nextNextRnd, nextInitialSizes, initSeed)
              else
                new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  loop(nextSucceededCount, discardedCount, nextEdges, nextNextRnd, nextInitialSizes, initSeed)
                else
                  PropertyCheckResult.Success(argsPassed, initSeed)
              }
              else
                new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)
            }

          case Failure(ex: DiscardedEvaluationException) =>
            val nextDiscardedCount = discardedCount + 1
            if (nextDiscardedCount < maxDiscarded)
              loop(succeededCount, nextDiscardedCount, nextEdges, nextNextRnd, nextInitialSizes, initSeed)
            else
              new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
          case Failure(ex) =>
            @tailrec
            def shrinkLoop(shrinksRemaining: List[A]): PropertyCheckResult = {
              shrinksRemaining match {
                case Nil => new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)
                case shrinkHead :: shrinkTail =>
                  val result: Try[T] = Try { fun(shrinkHead) }
                  result match {
                    case Success(_) => shrinkLoop(shrinkTail)
                    case Failure(shrunkEx) =>
                      val shrunkArgsPassed =
                        List(
                          if (names.isDefinedAt(0))
                            PropertyArgument(Some(names(0)), shrinkHead)
                          else
                            PropertyArgument(None, shrinkHead)
                        )
                      new PropertyCheckResult.Failure(succeededCount, Some(shrunkEx), names, shrunkArgsPassed, initSeed)
                  }
              }
            }
            val (it, _) = genA.shrink(a, rnd)
            shrinkLoop(it.take(100).toList)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      // ensuringValid will always succeed because /ing a PosInt by a positive number will always yield a positive or zero
      val (initEdges, afterEdgesRnd) = genA.initEdges(PosZInt.ensuringValid(config.minSuccessful / 5), afterSizesRnd)
      loop(0, 0, initEdges, afterEdgesRnd, initialSizes, initSeed) // We may need to be able to pass in a oh, pass in a key? Or grab it from the outside via cmd ln parm?
    }

    private def checkForAll[A, B](names: List[String], config: Parameter,
                          genA: org.scalatest.prop.Generator[A],
                          genB: org.scalatest.prop.Generator[B])
                         (fun: (A, B) => T): PropertyCheckResult = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): PropertyCheckResult = {
        val (size, nextInitialSizes, rnd1) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, rnd1) // TODO: See if PosZInt can be moved farther out
        val (b, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val result: Try[T] = Try { fun(a, b) }
        val argsPassed =
          List(
            if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
            if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b)
          )
        result match {
          case Success(r) =>
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, rnd3, nextInitialSizes, initSeed)
              else
                new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  loop(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, rnd3, nextInitialSizes, initSeed)
                else
                  PropertyCheckResult.Success(argsPassed, initSeed)
              }
              else
                new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)
            }

          case Failure(ex: DiscardedEvaluationException) =>
            val nextDiscardedCount = discardedCount + 1
            if (nextDiscardedCount < maxDiscarded)
              loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, rnd3, nextInitialSizes, initSeed)
            else
              new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
          case Failure(ex) =>
            new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, afterBEdgesRnd, initialSizes, initSeed)
    }

    private def checkForAll[A, B, C](names: List[String], config: Parameter,
                             genA: org.scalatest.prop.Generator[A],
                             genB: org.scalatest.prop.Generator[B],
                             genC: org.scalatest.prop.Generator[C])
                            (fun: (A, B, C) => T): PropertyCheckResult = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): PropertyCheckResult = {
        val (size, nextInitialSizes, rnd1) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, rnd1)
        val (b, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (c, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val result: Try[T] = Try { fun(a, b, c) }
        val argsPassed =
          List(
            if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
            if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b),
            if (names.isDefinedAt(2)) PropertyArgument(Some(names(2)), c) else PropertyArgument(None, c)
          )
        result match {
          case Success(r) =>
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, rnd4, nextInitialSizes, initSeed)
              else
                new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  loop(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, rnd4, nextInitialSizes, initSeed)
                else
                  PropertyCheckResult.Success(argsPassed, initSeed)
              }
              else
                new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)
            }

          case Failure(ex: DiscardedEvaluationException) =>
            val nextDiscardedCount = discardedCount + 1
            if (nextDiscardedCount < maxDiscarded)
              loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, rnd4, nextInitialSizes, initSeed)
            else
              new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
          case Failure(ex) =>
            new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, initCEdges, afterCEdgesRnd, initialSizes, initSeed)
    }

    private def checkForAll[A, B, C, D](names: List[String], config: Parameter,
                                genA: org.scalatest.prop.Generator[A],
                                genB: org.scalatest.prop.Generator[B],
                                genC: org.scalatest.prop.Generator[C],
                                genD: org.scalatest.prop.Generator[D])
                               (fun: (A, B, C, D) => T): PropertyCheckResult = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): PropertyCheckResult = {
        val (size, nextInitialSizes, rnd1) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, rnd1)
        val (b, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (c, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (d, nextDEdges, rnd5) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)
        val result: Try[T] = Try { fun(a, b, c, d) }
        val argsPassed =
          List(
            if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
            if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b),
            if (names.isDefinedAt(2)) PropertyArgument(Some(names(2)), c) else PropertyArgument(None, c),
            if (names.isDefinedAt(3)) PropertyArgument(Some(names(3)), d) else PropertyArgument(None, d)
          )
        result match {
          case Success(r) =>
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, rnd5, nextInitialSizes, initSeed)
              else
                new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  loop(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, rnd5, nextInitialSizes, initSeed)
                else
                  PropertyCheckResult.Success(argsPassed, initSeed)
              }
              else
                new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)
            }

          case Failure(ex: DiscardedEvaluationException) =>
            val nextDiscardedCount = discardedCount + 1
            if (nextDiscardedCount < maxDiscarded)
              loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, rnd5, nextInitialSizes, initSeed)
            else
              new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
          case Failure(ex) =>
            new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, afterDEdgesRnd, initialSizes, initSeed)
    }

    private def checkForAll[A, B, C, D, E](names: List[String], config: Parameter,
                                   genA: org.scalatest.prop.Generator[A],
                                   genB: org.scalatest.prop.Generator[B],
                                   genC: org.scalatest.prop.Generator[C],
                                   genD: org.scalatest.prop.Generator[D],
                                   genE: org.scalatest.prop.Generator[E])
                                  (fun: (A, B, C, D, E) => T): PropertyCheckResult = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): PropertyCheckResult = {
        val (size, nextInitialSizes, rnd1) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, rnd1)
        val (b, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (c, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (d, nextDEdges, rnd5) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)
        val (e, nextEEdges, rnd6) = genE.next(SizeParam(PosZInt(0), maxSize, size), eEdges, rnd5)
        val result: Try[T] = Try { fun(a, b, c, d, e) }
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
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, rnd6, nextInitialSizes, initSeed)
              else
                new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  loop(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, rnd6, nextInitialSizes, initSeed)
                else
                  PropertyCheckResult.Success(argsPassed, initSeed)
              }
              else
                new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)
            }

          case Failure(ex: DiscardedEvaluationException) =>
            val nextDiscardedCount = discardedCount + 1
            if (nextDiscardedCount < maxDiscarded)
              loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, rnd6, nextInitialSizes, initSeed)
            else
              new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
          case Failure(ex) =>
            new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
      val (initEEdges, afterEEdgesRnd) = genE.initEdges(maxEdges, afterDEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, initEEdges, afterEEdgesRnd, initialSizes, initSeed)
    }

    private def checkForAll[A, B, C, D, E, F](names: List[String], config: Parameter,
                                      genA: org.scalatest.prop.Generator[A],
                                      genB: org.scalatest.prop.Generator[B],
                                      genC: org.scalatest.prop.Generator[C],
                                      genD: org.scalatest.prop.Generator[D],
                                      genE: org.scalatest.prop.Generator[E],
                                      genF: org.scalatest.prop.Generator[F])
                                     (fun: (A, B, C, D, E, F) => T): PropertyCheckResult = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], fEdges: List[F], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): PropertyCheckResult = {
        val (size, nextInitialSizes, rnd1) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, rnd1)
        val (b, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (c, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (d, nextDEdges, rnd5) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)
        val (e, nextEEdges, rnd6) = genE.next(SizeParam(PosZInt(0), maxSize, size), eEdges, rnd5)
        val (f, nextFEdges, rnd7) = genF.next(SizeParam(PosZInt(0), maxSize, size), fEdges, rnd6)
        val result: Try[T] = Try { fun(a, b, c, d, e, f) }
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
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, rnd7, nextInitialSizes, initSeed)
              else
                new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  loop(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, rnd7, nextInitialSizes, initSeed)
                else
                  PropertyCheckResult.Success(argsPassed, initSeed)
              }
              else {
                new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)
              }
            }

          case Failure(ex: DiscardedEvaluationException) =>
            val nextDiscardedCount = discardedCount + 1
            if (nextDiscardedCount < maxDiscarded)
              loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, rnd7, nextInitialSizes, initSeed)
            else
              new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)
          case Failure(ex) =>
            new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
      val (initEEdges, afterEEdgesRnd) = genE.initEdges(maxEdges, afterDEdgesRnd)
      val (initFEdges, afterFEdgesRnd) = genF.initEdges(maxEdges, afterEEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, initEEdges, initFEdges, afterFEdgesRnd, initialSizes, initSeed)
    }

    private def checkResult(result: PropertyCheckResult, prettifier: Prettifier, pos: source.Position, argNames: Option[List[String]] = None): Result = {
      val (args, labels) = argsAndLabels(result)
      result match {
        case PropertyCheckResult.Exhausted(succeeded, discarded, names, argsPassed, initSeed) =>
          val failureMsg =
            if (succeeded == 1)
              FailureMessages.propCheckExhaustedAfterOne(prettifier, discarded) + EOL + FailureMessages.initSeed(prettifier, initSeed)
            else
              FailureMessages.propCheckExhausted(prettifier, succeeded, discarded) + EOL + FailureMessages.initSeed(prettifier, initSeed)

          indicateFailure(
            sde => failureMsg,
            failureMsg,
            args,
            labels,
            None,
            pos
          )

        case failure @ PropertyCheckResult.Failure(succeeded, ex, names, argsPassed, initSeed) =>
          indicateFailure(
            sde => failureStr(failure, sde, prettifier, argNames, labels),
            FailureMessages.propertyFailed(prettifier, succeeded),
            argsPassed,
            labels,
            ex,
            pos
          )

        case _ => indicateSuccess(FailureMessages.propertyCheckSucceeded)
      }
    }

    def check1[A](fun: (A) => T,
                  genA: org.scalatest.prop.Generator[A],
                  prms: Configuration.Parameter,
                  prettifier: Prettifier,
                  pos: source.Position,
                  names: List[String],
                  argNames: Option[List[String]] = None): Result = {
      val result = checkForAll(names, prms, genA)(fun)
      checkResult(result, prettifier, pos, argNames)
    }

    def check2[A, B](fun: (A, B) => T,
                     genA: org.scalatest.prop.Generator[A],
                     genB: org.scalatest.prop.Generator[B],
                     prms: Configuration.Parameter,
                     prettifier: Prettifier,
                     pos: source.Position,
                     names: List[String],
                     argNames: Option[List[String]] = None): Result = {
      val result = checkForAll(names, prms, genA, genB)(fun)
      checkResult(result, prettifier, pos, argNames)
    }

    def check3[A, B, C](fun: (A, B, C) => T,
                        genA: org.scalatest.prop.Generator[A],
                        genB: org.scalatest.prop.Generator[B],
                        genC: org.scalatest.prop.Generator[C],
                        prms: Configuration.Parameter,
                        prettifier: Prettifier,
                        pos: source.Position,
                        names: List[String],
                        argNames: Option[List[String]] = None): Result = {
      val result = checkForAll(names, prms, genA, genB, genC)(fun)
      checkResult(result, prettifier, pos, argNames)
    }

    def check4[A, B, C, D](fun: (A, B, C, D) => T,
                           genA: org.scalatest.prop.Generator[A],
                           genB: org.scalatest.prop.Generator[B],
                           genC: org.scalatest.prop.Generator[C],
                           genD: org.scalatest.prop.Generator[D],
                           prms: Configuration.Parameter,
                           prettifier: Prettifier,
                           pos: source.Position,
                           names: List[String],
                           argNames: Option[List[String]] = None): Result = {
      val result = checkForAll(names, prms, genA, genB, genC, genD)(fun)
      checkResult(result, prettifier, pos, argNames)
    }

    def check5[A, B, C, D, E](fun: (A, B, C, D, E) => T,
                              genA: org.scalatest.prop.Generator[A],
                              genB: org.scalatest.prop.Generator[B],
                              genC: org.scalatest.prop.Generator[C],
                              genD: org.scalatest.prop.Generator[D],
                              genE: org.scalatest.prop.Generator[E],
                              prms: Configuration.Parameter,
                              prettifier: Prettifier,
                              pos: source.Position,
                              names: List[String],
                              argNames: Option[List[String]] = None): Result = {
      val result = checkForAll(names, prms, genA, genB, genC, genD, genE)(fun)
      checkResult(result, prettifier, pos, argNames)
    }

    def check6[A, B, C, D, E, F](fun: (A, B, C, D, E, F) => T,
                                 genA: org.scalatest.prop.Generator[A],
                                 genB: org.scalatest.prop.Generator[B],
                                 genC: org.scalatest.prop.Generator[C],
                                 genD: org.scalatest.prop.Generator[D],
                                 genE: org.scalatest.prop.Generator[E],
                                 genF: org.scalatest.prop.Generator[F],
                                 prms: Configuration.Parameter,
                                 prettifier: Prettifier,
                                 pos: source.Position,
                                 names: List[String],
                                 argNames: Option[List[String]] = None): Result = {
      val result = checkForAll(names, prms, genA, genB, genC, genD, genE, genF)(fun)
      checkResult(result, prettifier, pos, argNames)
    }

    private[scalatest] def indicateSuccess(message: => String): Result

    private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Result

  }

}

trait FuturePropCheckerAsserting {

  import PropCheckerAsserting._

  abstract class FuturePropCheckerAssertingImpl[T] extends PropCheckerAsserting[Future[T]] {

    implicit val executionContext: scala.concurrent.ExecutionContext

    type Result = Future[Assertion]
    type S = T

    private def checkForAll[A](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A])(fun: (A) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, edges: List[A], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult], failedA: Option[A])

      def shrunkenFuture(future: Future[PropertyCheckResult], a: A, rnd: Randomizer): Future[PropertyCheckResult] =
        future.flatMap {
          case pcr @ PropertyCheckResult.Failure(succeededCount, optEx, _, argsPassed, initSeed) => 
            def shrinkLoop(shrinksRemaining: List[A]): Future[PropertyCheckResult] = {
              shrinksRemaining match {
                case Nil => Future.successful(pcr) // Can I reuse future here out of curiosity? That one is also completed.
                case shrinkHead :: shrinkTail =>
                  val result: Future[T] = fun(shrinkHead)
                  // Once we drop support for Scala 2.11, we can use transformWith here
                  result.flatMap(_ => shrinkLoop(shrinkTail)).recoverWith {
                    case shrunkEx: Throwable =>
                      val shrunkArgsPassed =
                        List(
                          if (names.isDefinedAt(0))
                            PropertyArgument(Some(names(0)), shrinkHead)
                          else
                            PropertyArgument(None, shrinkHead)
                        )
                      Future.successful(new PropertyCheckResult.Failure(succeededCount, Some(shrunkEx), names, shrunkArgsPassed, initSeed))
                  }
              }
            }
            val (it, _) = genA.shrink(a, rnd)
            shrinkLoop(it.take(100).toList)
          case pcr => Future.successful(pcr)
        }

      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      def loop(succeededCount: Int, discardedCount: Int, edges: List[A], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): Future[AccumulatedResult] = {
        val (size, nextInitialSizes, nextRnd) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextEdges, nextNextRnd) = genA.next(SizeParam(PosZInt(0), maxSize, size), edges, nextRnd) // TODO: Move PosZInt farther out

        val argsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a))
        try {
          val future = fun(a)
          future.map { r =>
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, edges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  AccumulatedResult(nextSucceededCount, discardedCount, nextEdges, nextNextRnd, nextInitialSizes, None, None)
                else
                  AccumulatedResult(succeededCount, discardedCount, edges, rnd, initialSizes, Some(PropertyCheckResult.Success(argsPassed, initSeed)), None)

              }
              else
                AccumulatedResult(succeededCount, discardedCount, edges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)), Some(a))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, edges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, edges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)), Some(a))
          } flatMap { result =>
            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.edges, result.rnd, result.initialSizes, initSeed)
          }
        }
        catch {
          case ex: DiscardedEvaluationException =>
            val nextDiscardedCount = discardedCount + 1
            val result =
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, edges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.edges, result.rnd, result.initialSizes, initSeed)

          case ex: Throwable =>
            val result = AccumulatedResult(succeededCount, discardedCount, edges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)), Some(a))
            Future.successful(result)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      // ensuringValid will always succeed because /ing a PosInt by a positive number will always yield a positive or zero
      val (initEdges, afterEdgesRnd) = genA.initEdges(PosZInt.ensuringValid(config.minSuccessful / 5), afterSizesRnd)

      loop(0, 0, initEdges, afterEdgesRnd, initialSizes, initSeed).flatMap { accResult =>
        accResult match {
          case AccumulatedResult(_, _, _, rnd, _, Some(candidate), Some(a)) =>
            shrunkenFuture(Future.successful(candidate), a, rnd)
          case _ =>
            Future.successful(accResult.result.get)
        }
      }
    }

    private def checkForAll[A, B](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A], genB: org.scalatest.prop.Generator[B])(fun: (A, B) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult])

      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): Future[AccumulatedResult] = {
        val (size, nextInitialSizes, nextRnd) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, nextRnd)
        val (b, nextBEdges, nextNextRnd) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)

        val argsPassed =
          List(
            if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
            if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b)
          )
        try {
          val future = fun(a, b)
          future.map { r =>
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  AccumulatedResult(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextNextRnd, nextInitialSizes, None)
                else
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(PropertyCheckResult.Success(argsPassed, initSeed)))

              }
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)))
          } flatMap { result =>
            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.rnd, result.initialSizes, initSeed)
          }
        }
        catch {
          case ex: DiscardedEvaluationException =>
            val nextDiscardedCount = discardedCount + 1
            val result =
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.rnd, result.initialSizes, initSeed)

          case ex: Throwable =>
            val result = AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)))
            Future.successful(result)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)

      loop(0, 0, initAEdges, initBEdges, afterBEdgesRnd, initialSizes, initSeed).map { accResult =>
        accResult.result.get
      }
    }

    private def checkForAll[A, B, C](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A], genB: org.scalatest.prop.Generator[B],
                                     genC: org.scalatest.prop.Generator[C])(fun: (A, B, C) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult])

      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): Future[AccumulatedResult] = {
        val (size, nextInitialSizes, nextRnd) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, nextRnd)
        val (b, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (c, nextCEdges, nextNextRnd) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)

        val argsPassed =
          List(
            if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
            if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b),
            if (names.isDefinedAt(2)) PropertyArgument(Some(names(2)), c) else PropertyArgument(None, c)
          )
        try {
          val future = fun(a, b, c)
          future.map { r =>
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  AccumulatedResult(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextNextRnd, nextInitialSizes, None)
                else
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(PropertyCheckResult.Success(argsPassed, initSeed)))

              }
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)))
          } flatMap { result =>
            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.rnd, result.initialSizes, initSeed)
          }
        }
        catch {
          case ex: DiscardedEvaluationException =>
            val nextDiscardedCount = discardedCount + 1
            val result =
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.rnd, result.initialSizes, initSeed)

          case ex: Throwable =>
            val result = AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)))
            Future.successful(result)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)

      loop(0, 0, initAEdges, initBEdges, initCEdges, afterCEdgesRnd, initialSizes, initSeed).map { accResult =>
        accResult.result.get
      }
    }

    private def checkForAll[A, B, C, D](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A], genB: org.scalatest.prop.Generator[B],
                                     genC: org.scalatest.prop.Generator[C], genD: org.scalatest.prop.Generator[D])(fun: (A, B, C, D) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult])

      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): Future[AccumulatedResult] = {
        val (size, nextInitialSizes, nextRnd) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, nextRnd)
        val (b, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (c, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (d, nextDEdges, nextNextRnd) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)

        val argsPassed =
          List(
            if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
            if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b),
            if (names.isDefinedAt(2)) PropertyArgument(Some(names(2)), c) else PropertyArgument(None, c),
            if (names.isDefinedAt(3)) PropertyArgument(Some(names(3)), d) else PropertyArgument(None, d)
          )
        try {
          val future = fun(a, b, c, d)
          future.map { r =>
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  AccumulatedResult(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextNextRnd, nextInitialSizes, None)
                else
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(PropertyCheckResult.Success(argsPassed, initSeed)))

              }
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)))
          } flatMap { result =>
            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.rnd, result.initialSizes, initSeed)
          }
        }
        catch {
          case ex: DiscardedEvaluationException =>
            val nextDiscardedCount = discardedCount + 1
            val result =
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.rnd, result.initialSizes, initSeed)

          case ex: Throwable =>
            val result = AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)))
            Future.successful(result)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)

      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, afterDEdgesRnd, initialSizes, initSeed).map { accResult =>
        accResult.result.get
      }
    }

    private def checkForAll[A, B, C, D, E](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A], genB: org.scalatest.prop.Generator[B],
                                        genC: org.scalatest.prop.Generator[C], genD: org.scalatest.prop.Generator[D], genE: org.scalatest.prop.Generator[E])(fun: (A, B, C, D, E) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult])

      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): Future[AccumulatedResult] = {
        val (size, nextInitialSizes, nextRnd) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, nextRnd)
        val (b, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (c, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (d, nextDEdges, rnd5) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)
        val (e, nextEEdges, nextNextRnd) = genE.next(SizeParam(PosZInt(0), maxSize, size), eEdges, rnd5)

        val argsPassed =
          List(
            if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
            if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b),
            if (names.isDefinedAt(2)) PropertyArgument(Some(names(2)), c) else PropertyArgument(None, c),
            if (names.isDefinedAt(3)) PropertyArgument(Some(names(3)), d) else PropertyArgument(None, d),
            if (names.isDefinedAt(4)) PropertyArgument(Some(names(4)), e) else PropertyArgument(None, e)
          )
        try {
          val future = fun(a, b, c, d, e)
          future.map { r =>
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  AccumulatedResult(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextNextRnd, nextInitialSizes, None)
                else
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(PropertyCheckResult.Success(argsPassed, initSeed)))

              }
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)))
          } flatMap { result =>
            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.eEdges, result.rnd, result.initialSizes, initSeed)
          }
        }
        catch {
          case ex: DiscardedEvaluationException =>
            val nextDiscardedCount = discardedCount + 1
            val result =
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.eEdges, result.rnd, result.initialSizes, initSeed)

          case ex: Throwable =>
            val result = AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)))
            Future.successful(result)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
      val (initEEdges, afterEEdgesRnd) = genE.initEdges(maxEdges, afterDEdgesRnd)

      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, initEEdges, afterEEdgesRnd, initialSizes, initSeed).map { accResult =>
        accResult.result.get
      }
    }

    private def checkForAll[A, B, C, D, E, F](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A], genB: org.scalatest.prop.Generator[B],
                                           genC: org.scalatest.prop.Generator[C], genD: org.scalatest.prop.Generator[D], genE: org.scalatest.prop.Generator[E],
                                           genF: org.scalatest.prop.Generator[F])(fun: (A, B, C, D, E, F) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], fEdges: List[F], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult])

      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], fEdges: List[F], rnd: Randomizer, initialSizes: List[PosZInt], initSeed: Long): Future[AccumulatedResult] = {
        val (size, nextInitialSizes, nextRnd) =
          initialSizes match {
            case head :: tail => (head, tail, rnd)
            case Nil =>
              val (sz, nextRnd) = rnd.choosePosZInt(minSize, maxSize)
              (sz, Nil, nextRnd)
          }
        val (a, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, nextRnd)
        val (b, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (c, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (d, nextDEdges, rnd5) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)
        val (e, nextEEdges, rnd6) = genE.next(SizeParam(PosZInt(0), maxSize, size), eEdges, rnd5)
        val (f, nextFEdges, nextNextRnd) = genF.next(SizeParam(PosZInt(0), maxSize, size), fEdges, rnd6)

        val argsPassed =
          List(
            if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), a) else PropertyArgument(None, a),
            if (names.isDefinedAt(1)) PropertyArgument(Some(names(1)), b) else PropertyArgument(None, b),
            if (names.isDefinedAt(2)) PropertyArgument(Some(names(2)), c) else PropertyArgument(None, c),
            if (names.isDefinedAt(3)) PropertyArgument(Some(names(3)), d) else PropertyArgument(None, d),
            if (names.isDefinedAt(4)) PropertyArgument(Some(names(4)), e) else PropertyArgument(None, e),
            if (names.isDefinedAt(5)) PropertyArgument(Some(names(5)), f) else PropertyArgument(None, f)
          )
        try {
          val future = fun(a, b, c, d, e, f)
          future.map { r =>
            if (discard(r)) {
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  AccumulatedResult(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, nextNextRnd, nextInitialSizes, None)
                else
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(PropertyCheckResult.Success(argsPassed, initSeed)))

              }
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)))
          } flatMap { result =>
            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.eEdges, result.fEdges, result.rnd, result.initialSizes, initSeed)
          }
        }
        catch {
          case ex: DiscardedEvaluationException =>
            val nextDiscardedCount = discardedCount + 1
            val result =
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, nextNextRnd, nextInitialSizes, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)))

            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.eEdges, result.fEdges, result.rnd, result.initialSizes, initSeed)

          case ex: Throwable =>
            val result = AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)))
            Future.successful(result)
        }
      }

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
      val (initEEdges, afterEEdgesRnd) = genE.initEdges(maxEdges, afterDEdgesRnd)
      val (initFEdges, afterFEdgesRnd) = genF.initEdges(maxEdges, afterEEdgesRnd)

      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, initEEdges, initFEdges, afterFEdgesRnd, initialSizes, initSeed).map { accResult =>
        accResult.result.get
      }
    }

    private def checkResult(result: PropertyCheckResult, prettifier: Prettifier, pos: source.Position, argNames: Option[List[String]] = None): Assertion = {
      val (args, labels) = argsAndLabels(result)
      result match {
        case PropertyCheckResult.Exhausted(succeeded, discarded, names, argsPassed, initSeed) =>
          val failureMsg =
            if (succeeded == 1)
              FailureMessages.propCheckExhaustedAfterOne(prettifier, discarded) + EOL + FailureMessages.initSeed(prettifier, initSeed)
            else
              FailureMessages.propCheckExhausted(prettifier, succeeded, discarded) + EOL + FailureMessages.initSeed(prettifier, initSeed)

          indicateFutureFailure(
            sde => failureMsg,
            failureMsg,
            args,
            labels,
            None,
            pos
          )

        case failure @ PropertyCheckResult.Failure(succeeded, ex, names, argsPassed, initSeed) =>
          indicateFutureFailure(
            sde => failureStr(failure, sde, prettifier, argNames, labels),
            FailureMessages.propertyFailed(prettifier, succeeded),
            argsPassed,
            labels,
            ex,
            pos
          )

        case _ => indicateFutureSuccess(FailureMessages.propertyCheckSucceeded)
      }
    }

    def check1[A](fun: (A) => Future[T],
                  genA: org.scalatest.prop.Generator[A],
                  prms: Configuration.Parameter,
                  prettifier: Prettifier,
                  pos: source.Position,
                  names: List[String],
                  argNames: Option[List[String]] = None): Result = {
      val future = checkForAll(names, prms, genA)(fun)
      future.map { result =>
        checkResult(result, prettifier, pos, argNames)
      }
    }

    def check2[A, B](fun: (A, B) => Future[T],
                     genA: org.scalatest.prop.Generator[A],
                     genB: org.scalatest.prop.Generator[B],
                     prms: Configuration.Parameter,
                     prettifier: Prettifier,
                     pos: source.Position,
                     names: List[String],
                     argNames: Option[List[String]] = None): Result = {
      val future = checkForAll(names, prms, genA, genB)(fun)
      future.map { result =>
        checkResult(result, prettifier, pos, argNames)
      }
    }

    def check3[A, B, C](fun: (A, B, C) => Future[T],
                        genA: org.scalatest.prop.Generator[A],
                        genB: org.scalatest.prop.Generator[B],
                        genC: org.scalatest.prop.Generator[C],
                        prms: Configuration.Parameter,
                        prettifier: Prettifier,
                        pos: source.Position,
                        names: List[String],
                        argNames: Option[List[String]] = None): Result = {
      val future = checkForAll(names, prms, genA, genB, genC)(fun)
      future.map { result =>
        checkResult(result, prettifier, pos, argNames)
      }
    }

    def check4[A, B, C, D](fun: (A, B, C, D) => Future[T],
                           genA: org.scalatest.prop.Generator[A],
                           genB: org.scalatest.prop.Generator[B],
                           genC: org.scalatest.prop.Generator[C],
                           genD: org.scalatest.prop.Generator[D],
                           prms: Configuration.Parameter,
                           prettifier: Prettifier,
                           pos: source.Position,
                           names: List[String],
                           argNames: Option[List[String]] = None): Result = {
      val future = checkForAll(names, prms, genA, genB, genC, genD)(fun)
      future.map { result =>
        checkResult(result, prettifier, pos, argNames)
      }
    }

    def check5[A, B, C, D, E](fun: (A, B, C, D, E) => Future[T],
                              genA: org.scalatest.prop.Generator[A],
                              genB: org.scalatest.prop.Generator[B],
                              genC: org.scalatest.prop.Generator[C],
                              genD: org.scalatest.prop.Generator[D],
                              genE: org.scalatest.prop.Generator[E],
                              prms: Configuration.Parameter,
                              prettifier: Prettifier,
                              pos: source.Position,
                              names: List[String],
                              argNames: Option[List[String]] = None): Result = {
      val future = checkForAll(names, prms, genA, genB, genC, genD, genE)(fun)
      future.map { result =>
        checkResult(result, prettifier, pos, argNames)
      }
    }

    def check6[A, B, C, D, E, F](fun: (A, B, C, D, E, F) => Future[T],
                                 genA: org.scalatest.prop.Generator[A],
                                 genB: org.scalatest.prop.Generator[B],
                                 genC: org.scalatest.prop.Generator[C],
                                 genD: org.scalatest.prop.Generator[D],
                                 genE: org.scalatest.prop.Generator[E],
                                 genF: org.scalatest.prop.Generator[F],
                                 prms: Configuration.Parameter,
                                 prettifier: Prettifier,
                                 pos: source.Position,
                                 names: List[String],
                                 argNames: Option[List[String]] = None): Result = {
      val future = checkForAll(names, prms, genA, genB, genC, genD, genE, genF)(fun)
      future.map { result =>
        checkResult(result, prettifier, pos, argNames)
      }
    }

    private[scalatest] def indicateFutureSuccess(message: => String): Assertion

    private[scalatest] def indicateFutureFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Assertion

  }

}

abstract class ExpectationPropCheckerAsserting extends UnitPropCheckerAsserting {

  implicit def assertingNatureOfExpectation(implicit prettifier: Prettifier): PropCheckerAsserting[Expectation] { type Result = Expectation } = {
    new PropCheckerAssertingImpl[Expectation] {
      type Result = Expectation
      def discard(result: Expectation): Boolean = result.isVacuousYes
      def succeed(result: Expectation): (Boolean, Option[Throwable]) = (result.isYes, result.cause)
      private[scalatest] def indicateSuccess(message: => String): Expectation = Fact.Yes(message, prettifier)
      private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Expectation = {
        val gdpcfe =
          new GeneratorDrivenPropertyCheckFailedException(
            messageFun,
            optionalCause,
            pos,
            None,
            undecoratedMessage,
            scalaCheckArgs,
            None,
            scalaCheckLabels.toList
          )
        val message: String = gdpcfe.getMessage
        Fact.No(message, prettifier)
      }
    }
  }
}

object PropCheckerAsserting extends ExpectationPropCheckerAsserting with FuturePropCheckerAsserting {

  implicit def assertingNatureOfAssertion: PropCheckerAsserting[Assertion] { type Result = Assertion } = {
    new PropCheckerAssertingImpl[Assertion] {
      type Result = Assertion
      def discard(result: Assertion): Boolean = false
      def succeed(result: Assertion): (Boolean, Option[Throwable]) = (true, None)
      private[scalatest] def indicateSuccess(message: => String): Assertion = Succeeded
      private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Assertion = {
        throw new GeneratorDrivenPropertyCheckFailedException(
          messageFun,
          optionalCause,
          pos,
          None,
          undecoratedMessage,
          scalaCheckArgs,
          None,
          scalaCheckLabels.toList
        )
      }
    }
  }

  implicit def assertingNatureOfFutureAssertion(implicit exeCtx: scala.concurrent.ExecutionContext): PropCheckerAsserting[Future[Assertion]] { type Result = Future[Assertion] } = {
    new FuturePropCheckerAssertingImpl[Assertion] {
      implicit val executionContext = exeCtx
      def discard(result: Assertion): Boolean = false
      def succeed(result: Assertion): (Boolean, Option[Throwable]) = (true, None)
      private[scalatest] def indicateFutureSuccess(message: => String): Assertion = Succeeded
      private[scalatest] def indicateFutureFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Assertion = {
        throw new GeneratorDrivenPropertyCheckFailedException(
          messageFun,
          optionalCause,
          pos,
          None,
          undecoratedMessage,
          scalaCheckArgs,
          None,
          scalaCheckLabels.toList
        )
      }
    }
  }

  private[enablers] def argsAndLabels(result: PropertyCheckResult): (List[PropertyArgument], List[String]) = {

    val (args: List[PropertyArgument], labels: List[String]) =
      result match {
        case PropertyCheckResult.Success(args, _) => (args.toList, List())
        case PropertyCheckResult.Failure(_, _, names, args, _) => (args.toList, List())
        case _ => (List(), List())
      }

    (args, labels)
  }

  private[enablers] def decorateArgToStringValue(arg: PropertyArgument, prettifier: Prettifier): String =
    decorateToStringValue(prettifier, arg.value)

  private[enablers] def prettyArgs(args: List[PropertyArgument], prettifier: Prettifier) = {
    val strs = for((a, i) <- args.zipWithIndex) yield {

      val argString =
        a.label match {
          case None => ""
          case Some(label) => s"$label = "
        }

      "    " + argString +
        decorateArgToStringValue(a, prettifier) + (if (i < args.length - 1) "," else "") /*+
        (if (a.shrinks > 0) " // " + a.shrinks + (if (a.shrinks == 1) " shrink" else " shrinks") else "")*/
    }
    strs.mkString("\n")
  }

  private[enablers] def getArgsWithSpecifiedNames(argNames: Option[List[String]], checkArgs: List[PropertyArgument]) = {
    if (argNames.isDefined) {
      // length of scalaCheckArgs should equal length of argNames
      val zipped = argNames.get zip checkArgs
      zipped map { case (argName, arg) => arg.copy(label = Some(argName)) }
    }
    else
      checkArgs
  }

  private[enablers] def getLabelDisplay(labels: Set[String]): String =
    if (labels.size > 0)
      "\n  " + (if (labels.size == 1) Resources.propCheckLabel else Resources.propCheckLabels) + "\n" + labels.map("    " + _).mkString("\n")
    else
      ""

  /**
    * This computes the string to display when a property check fails. It's showing quite a bit, so there's a lot
    * to it.
    *
    * @param failure the actual property check failure, which contains lots of stuff we need to show
    * @param outerEx the outer exception, generally pointing to the forAll itself
    * @param prettifier the Prettifier that we will use to improve the error displays
    * @param argNames the names on the property check arguments, if any
    * @param labels
    * @return the detailed error message to show to the user
    */
  private[enablers] def failureStr(failure: PropertyCheckResult.Failure, outerEx: StackDepthException, prettifier: Prettifier, argNames: Option[List[String]], labels: List[String]): String = {
    // ex is the *inner* Exception, where we actually threw. If defined, this is typically the line
    // that the user really cares about:
    val PropertyCheckResult.Failure(succeeded, ex, names, argsPassed, initSeed) = failure

    FailureMessages.propertyException(prettifier, UnquotedString(outerEx.getClass.getSimpleName)) +
      ( outerEx.failedCodeFileNameAndLineNumberString match { case Some(s) => " (" + s + ")"; case None => "" }) + EOL +
      "  " + FailureMessages.propertyFailed(prettifier, succeeded) + EOL + (
        ex match {
          case Some(ex: Throwable) if ex.getMessage != null =>
            "  " + FailureMessages.thrownExceptionsMessage(prettifier, UnquotedString(ex.getMessage)) + EOL
          case _ => ""
        }
      ) + (
        ex match {
          case Some(sd: StackDepth) if sd.failedCodeFileNameAndLineNumberString.isDefined =>
            "  " + FailureMessages.thrownExceptionsLocation(prettifier, UnquotedString(sd.failedCodeFileNameAndLineNumberString.get)) + EOL
          case _ => ""
        }
      ) +
      "  " + FailureMessages.occurredOnValues + EOL +
      prettyArgs(getArgsWithSpecifiedNames(argNames, argsPassed), prettifier) + EOL +
      "  )" +
      getLabelDisplay(labels.toSet) + EOL +
      "  " + FailureMessages.initSeed(prettifier, initSeed)
  }

  def calcSizes(minSize: PosZInt, maxSize: PosZInt, initRndm: Randomizer): (List[PosZInt], Randomizer) = {
    @tailrec
    def sizesLoop(sizes: List[PosZInt], count: Int, rndm: Randomizer): (List[PosZInt], Randomizer) = {
      sizes match {
        case Nil => sizesLoop(List(minSize), 1, rndm)
        case szs if count < 10 =>
          val (nextSize, nextRndm) = rndm.choosePosZInt(minSize, maxSize)
          sizesLoop(nextSize :: sizes, count + 1, nextRndm)
        case _ => (sizes.sorted, rndm)
      }
    }
    sizesLoop(Nil, 0, initRndm)
  }
}
