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
import org.scalatest.prop.{Configuration, PropertyArgument, PropertyTest}
import org.scalatest.{FailureMessages, Resources, UnquotedString, Fact, Expectation, Assertion, Succeeded}
import FailureMessages.decorateToStringValue

trait PropCheckerAsserting[T] {

  /**
    * The result type of the <code>check</code> method.
    */
  type Result

  def discard(result: T): Boolean

  def succeed(result: T): (Boolean, Option[Throwable])

  private[scalatest] def indicateSuccess(message: => String): Result

  private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Result

  /**
    * Perform the property check using the given <code>Prop</code> and <code>Test.Parameters</code>.
    *
    * @param p the <code>Prop</code> to be used to check
    * @param prms the <code>Test.Parameters</code> to be used to check
    * @param prettifier the <code>Prettifier</code> to be used to prettify error message
    * @param pos the <code>Position</code> of the caller site
    * @param argNames the list of argument names
    * @return the <code>Result</code> of the property check.
    */
  def check(p: PropertyTest, prms: Configuration.Parameter, prettifier: Prettifier, pos: source.Position, argNames: Option[List[String]] = None): Result

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

    import org.scalatest.prop.Configuration.Parameter
    import org.scalactic.anyvals.PosZInt
    import scala.annotation.tailrec
    import org.scalatest.prop.{Randomizer, SizeParam}
    import scala.util.{Try, Success, Failure}
    import org.scalatest.exceptions.DiscardedEvaluationException

    private def checkForAll[A](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A])(fun: (A) => T): PropertyTest.Result = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)
      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, edges: List[A], rnd: Randomizer, initialSizes: List[PosZInt]): PropertyTest.Result = {
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
                loop(succeededCount, nextDiscardedCount, nextEdges, nextNextRnd, nextInitialSizes)
              else
                new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
            }
            else {
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
            }

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

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
      // ensuringValid will always succeed because /ing a PosInt by a positive number will always yield a positive or zero
      val (initEdges, afterEdgesRnd) = genA.initEdges(PosZInt.ensuringValid(config.minSuccessful / 5), afterSizesRnd)
      loop(0, 0, initEdges, afterEdgesRnd, initialSizes) // We may need to be able to pass in a oh, pass in a key? Or grab it from the outside via cmd ln parm?
    }

    private def checkForAll[A, B](names: List[String], config: Parameter,
                          genA: org.scalatest.prop.Generator[A],
                          genB: org.scalatest.prop.Generator[B])
                         (fun: (A, B) => T): PropertyTest.Result = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], rnd: Randomizer, initialSizes: List[PosZInt]): PropertyTest.Result = {
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
                loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, rnd3, nextInitialSizes)
              else
                new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
            }
            else {
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
            }

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

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, afterBEdgesRnd, initialSizes)
    }

    private def checkForAll[A, B, C](names: List[String], config: Parameter,
                             genA: org.scalatest.prop.Generator[A],
                             genB: org.scalatest.prop.Generator[B],
                             genC: org.scalatest.prop.Generator[C])
                            (fun: (A, B, C) => T): PropertyTest.Result = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], rnd: Randomizer, initialSizes: List[PosZInt]): PropertyTest.Result = {
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
                loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, rnd4, nextInitialSizes)
              else
                new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
            }
            else {
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
            }

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

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, initCEdges, afterCEdgesRnd, initialSizes)
    }

    private def checkForAll[A, B, C, D](names: List[String], config: Parameter,
                                genA: org.scalatest.prop.Generator[A],
                                genB: org.scalatest.prop.Generator[B],
                                genC: org.scalatest.prop.Generator[C],
                                genD: org.scalatest.prop.Generator[D])
                               (fun: (A, B, C, D) => T): PropertyTest.Result = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], rnd: Randomizer, initialSizes: List[PosZInt]): PropertyTest.Result = {
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
                loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, rnd5, nextInitialSizes)
              else
                new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
            }
            else {
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
            }

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

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, afterDEdgesRnd, initialSizes)
    }

    private def checkForAll[A, B, C, D, E](names: List[String], config: Parameter,
                                   genA: org.scalatest.prop.Generator[A],
                                   genB: org.scalatest.prop.Generator[B],
                                   genC: org.scalatest.prop.Generator[C],
                                   genD: org.scalatest.prop.Generator[D],
                                   genE: org.scalatest.prop.Generator[E])
                                  (fun: (A, B, C, D, E) => T): PropertyTest.Result = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], rnd: Randomizer, initialSizes: List[PosZInt]): PropertyTest.Result = {
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
                loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, rnd6, nextInitialSizes)
              else
                new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
            }
            else {
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
            }

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

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
      val (initEEdges, afterEEdgesRnd) = genE.initEdges(maxEdges, afterDEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, initEEdges, afterEEdgesRnd, initialSizes)
    }

    private def checkForAll[A, B, C, D, E, F](names: List[String], config: Parameter,
                                      genA: org.scalatest.prop.Generator[A],
                                      genB: org.scalatest.prop.Generator[B],
                                      genC: org.scalatest.prop.Generator[C],
                                      genD: org.scalatest.prop.Generator[D],
                                      genE: org.scalatest.prop.Generator[E],
                                      genF: org.scalatest.prop.Generator[F])
                                     (fun: (A, B, C, D, E, F) => T): PropertyTest.Result = {
      val maxDiscarded = Configuration.calculateMaxDiscarded(config.maxDiscardedFactor, config.minSuccessful)
      val minSize = config.minSize
      val maxSize = PosZInt.ensuringValid(minSize + config.sizeRange)

      @tailrec
      def loop(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], fEdges: List[F], rnd: Randomizer, initialSizes: List[PosZInt]): PropertyTest.Result = {
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
                loop(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, rnd7, nextInitialSizes)
              else
                new PropertyTest.CheckExhausted(succeededCount, nextDiscardedCount, names, argsPassed)
            }
            else {
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

      val initRnd = Randomizer.default // Eventually we'll grab this from a global that can be set by a cmd line param.
      val (initialSizes, afterSizesRnd) = PropertyTest.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
      val (initEEdges, afterEEdgesRnd) = genE.initEdges(maxEdges, afterDEdgesRnd)
      val (initFEdges, afterFEdgesRnd) = genF.initEdges(maxEdges, afterEEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, initEEdges, initFEdges, afterFEdgesRnd, initialSizes)
    }

    private def checkResult(result: PropertyTest.Result, prettifier: Prettifier, pos: source.Position, argNames: Option[List[String]] = None): Result = {
      val (args, labels) = argsAndLabels(result)
      result match {
        case PropertyTest.CheckExhausted(succeeded, discarded, names, argsPassed) =>
          val failureMsg =
            if (succeeded == 1)
              FailureMessages.propCheckExhaustedAfterOne(prettifier, discarded)
            else
              FailureMessages.propCheckExhausted(prettifier, succeeded, discarded)

          indicateFailure(
            sde => failureMsg,
            failureMsg,
            args,
            labels,
            None,
            pos
          )

        case PropertyTest.CheckFailure(succeeded, ex, names, argsPassed) =>
          indicateFailure(
            sde => FailureMessages.propertyException(prettifier, UnquotedString(sde.getClass.getSimpleName)) + "\n" +
              ( sde.failedCodeFileNameAndLineNumberString match { case Some(s) => " (" + s + ")"; case None => "" }) + "\n" +
              "  " + FailureMessages.propertyFailed(prettifier, succeeded) + "\n" +
              (
                sde match {
                  case sd: StackDepth if sd.failedCodeFileNameAndLineNumberString.isDefined =>
                    "  " + FailureMessages.thrownExceptionsLocation(prettifier, UnquotedString(sd.failedCodeFileNameAndLineNumberString.get)) + "\n"
                  case _ => ""
                }
                ) +
              "  " + FailureMessages.occurredOnValues + "\n" +
              prettyArgs(getArgsWithSpecifiedNames(argNames, argsPassed), prettifier) + "\n" +
              "  )" +
              getLabelDisplay(labels.toSet),
            FailureMessages.propertyFailed(prettifier, succeeded),
            argsPassed,
            labels,
            None,
            pos
          )

        case _ => indicateSuccess(FailureMessages.propertyCheckSucceeded)
      }
    }

    /**
      * Check the given <code>Prop</code> and <code>Test.Parameters</code> by calling [[http://www.scalacheck.org ScalaCheck]]'s <code>Test.check</code>.
      * If the check succeeds, call <code>indicateSuccess</code>, else call <code>indicateFailure</code>.
      *
      *
      * @param p the <code>Prop</code> to be used to check
      * @param prms the <code>Test.Parameters</code> to be used to check
      * @param prettifier the <code>Prettifier</code> to be used to prettify error message
      * @param pos the <code>Position</code> of the caller site
      * @param argNames the list of argument names
      * @return the <code>Result</code> of the property check.
      */
    def check(p: PropertyTest, prms: Configuration.Parameter, prettifier: Prettifier, pos: source.Position, argNames: Option[List[String]] = None): Result = {
      val result = p.check
      checkResult(result, prettifier, pos, argNames)
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

abstract class ExpectationPropCheckerAsserting extends UnitPropCheckerAsserting {

  implicit def assertingNatureOfExpectation(implicit prettifier: Prettifier): PropCheckerAsserting[Expectation] { type Result = Expectation } = {
    new PropCheckerAssertingImpl[Expectation] {
      type Result = Expectation
      def discard(result: Expectation): Boolean = result.isVacuousYes
      def succeed(result: Expectation): (Boolean, Option[Throwable]) = (result.isYes, result.cause)
      private[scalatest] def indicateSuccess(message: => String): Expectation = Fact.Yes(message)(prettifier)
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
        Fact.No(message)(prettifier)
      }
    }
  }
}

object PropCheckerAsserting extends ExpectationPropCheckerAsserting {

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

  private[enablers] def argsAndLabels(result: PropertyTest.Result): (List[PropertyArgument], List[String]) = {

    val (args: List[PropertyArgument], labels: List[String]) =
      result match {
        case PropertyTest.CheckSuccess(args) => (args.toList, List())
        case PropertyTest.CheckFailure(_, _, names, args) => (args.toList, List())
        case _ => (List(), List())
      }

    (args, labels)
  }

  private[enablers] def decorateArgToStringValue(arg: PropertyArgument, prettifier: Prettifier): String =
    decorateToStringValue(prettifier, arg.value)

  private[enablers] def prettyArgs(args: List[PropertyArgument], prettifier: Prettifier) = {
    val strs = for((a, i) <- args.zipWithIndex) yield (
      "    " +
        (if (a.label == "") "arg" + i else a.label) +
        " = " + decorateArgToStringValue(a, prettifier) + (if (i < args.length - 1) "," else "") /*+
        (if (a.shrinks > 0) " // " + a.shrinks + (if (a.shrinks == 1) " shrink" else " shrinks") else "")*/
      )
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

}