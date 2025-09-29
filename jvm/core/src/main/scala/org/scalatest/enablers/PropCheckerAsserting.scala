/*
 * Copyright 2001-2025 Artima, Inc.
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
import System.{lineSeparator => EOL}
import org.scalatest.prop.RoseTree
import org.scalactic.ColCompatHelper._

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
    * @param fun the function to be used to perform the check
    * @param genA the generator of type <code>A</code>
    * @param prms the <code>Configuration.Parameters</code> to be used to perform the check
    * @param prettifier the <code>Prettifier</code> to be used to prettify error message
    * @param pos the <code>Position</code> of the caller's site
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

abstract class ExpectationPropCheckerAsserting {

  import PropCheckerAsserting.PropCheckerAssertingImpl

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

object PropCheckerAsserting extends ExpectationPropCheckerAsserting {

  abstract class PropCheckerAssertingImpl[T] extends PropCheckerAsserting[T] {

    type S = T

    /**
      * Checks a property for all combinations of generated values of type A.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param fun the function to apply to the generated values
      * @return the result of the property check
      */
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
        val (roseTreeOfA, nextEdges, nextNextRnd) = genA.next(SizeParam(PosZInt(0), maxSize, size), edges, nextRnd) // TODO: Move PosZInt farther out
        val a = roseTreeOfA.value
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
            // Let's shrink the failing value
            val (bestA, err) =
              roseTreeOfA.shrinkSearch(
                value => {
                  val result: Try[T] = Try { fun(value) }
                  result match {
                    case Success(_) => None
                    case Failure(shrunkEx) => Some(shrunkEx)
                  }
                }
              ).getOrElse((roseTreeOfA.value, ex))
            val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestA) else PropertyArgument(None, bestA))
            val theRes = new PropertyCheckResult.Failure(succeededCount, Some(err), names, shrunkArgsPassed, initSeed)
            theRes
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      // ensuringValid will always succeed because /ing a PosInt by a positive number will always yield a positive or zero
      val (initEdges, afterEdgesRnd) = genA.initEdges(PosZInt.ensuringValid(config.minSuccessful / 5), afterSizesRnd)
      loop(0, 0, initEdges, afterEdgesRnd, initialSizes, initSeed) // We may need to be able to pass in a oh, pass in a key? Or grab it from the outside via cmd ln parm?
    }

    /**
      * Checks a property for all combinations of generated values of types A and B.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param fun the function to apply to the generated values
      * @return the result of the property check
      */
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
        val (roseTreeOfA, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, rnd1) // TODO: See if PosZInt can be moved farther out
        val (roseTreeOfB, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val a = roseTreeOfA.value
        val b = roseTreeOfB.value
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
            // Let's shrink the failing value
            val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
            val (bestAB, err) =
              roseTreeOfAB.shrinkSearch {
                case (a, b) => 
                  Try { fun(a, b) } match {
                    case Success(_) => None
                    case Failure(shrunkEx) => Some(shrunkEx)
                  }
              }.getOrElse((roseTreeOfA.value, ex))
            val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestAB) else PropertyArgument(None, bestAB))
            val theRes = new PropertyCheckResult.Failure(succeededCount, Some(err), names, shrunkArgsPassed, initSeed)
            theRes
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, afterBEdgesRnd, initialSizes, initSeed)
    }

    /**
      * Checks a property for all combinations of generated values of types A, B, and C.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of the third generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param fun the function to apply to the generated values
      * @return the result of the property check
      */
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
        val (roseTreeOfA, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, rnd1)
        val (roseTreeOfB, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (roseTreeOfC, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val a = roseTreeOfA.value
        val b = roseTreeOfB.value
        val c = roseTreeOfC.value
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
            val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
            val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
            val (bestABC, err) =
              roseTreeOfABC.shrinkSearch {
                case (a, b, c) => 
                  Try { fun(a, b, c) } match {
                    case Success(_) => None
                    case Failure(shrunkEx) => Some(shrunkEx)
                  }
              }.getOrElse((roseTreeOfA.value, ex))
            val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABC) else PropertyArgument(None, bestABC))
            val theRes = new PropertyCheckResult.Failure(succeededCount, Some(err), names, shrunkArgsPassed, initSeed)
            theRes
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, initCEdges, afterCEdgesRnd, initialSizes, initSeed)
    }

    /**
      * Checks a property for all combinations of generated values of types A, B, C, and D.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of the third generated value
      * @tparam D the type of the fourth generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param genD the generator for values of type D
      * @param fun the function to apply to the generated values
      * @return the result of the property check
      */
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
        val (roseTreeOfA, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, rnd1)
        val (roseTreeOfB, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (roseTreeOfC, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (roseTreeOfD, nextDEdges, rnd5) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)
        val a = roseTreeOfA.value
        val b = roseTreeOfB.value
        val c = roseTreeOfC.value
        val d = roseTreeOfD.value
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
            val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
            val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
            val roseTreeOfABCD = RoseTree.map2(roseTreeOfABC, roseTreeOfD) { case ((a, b, c), d) => (a, b, c, d) }
            val (bestABCD, err) =
              roseTreeOfABCD.shrinkSearch { 
                case (a, b, c, d) => 
                  val result: Try[T] = Try { fun(a, b, c, d) }
                  result match {
                    case Success(_) => None
                    case Failure(shrunkEx) => Some(shrunkEx)
                  }
              }.getOrElse((roseTreeOfA.value, ex))
            val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABCD) else PropertyArgument(None, bestABCD))
            val theRes = new PropertyCheckResult.Failure(succeededCount, Some(err), names, shrunkArgsPassed, initSeed)
            theRes
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, afterDEdgesRnd, initialSizes, initSeed)
    }

    /**
      * Checks a property for all combinations of generated values of types A, B, C, D, and E.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of the third generated value
      * @tparam D the type of the fourth generated value
      * @tparam E the type of the fifth generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param genD the generator for values of type D
      * @param genE the generator for values of type E
      * @param fun the function to apply to the generated values
      * @return the result of the property check
      */
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
        val (roseTreeOfA, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, rnd1)
        val (roseTreeOfB, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (roseTreeOfC, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (roseTreeOfD, nextDEdges, rnd5) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)
        val (roseTreeOfE, nextEEdges, rnd6) = genE.next(SizeParam(PosZInt(0), maxSize, size), eEdges, rnd5)
        val a = roseTreeOfA.value
        val b = roseTreeOfB.value
        val c = roseTreeOfC.value
        val d = roseTreeOfD.value
        val e = roseTreeOfE.value
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
            val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
            val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
            val roseTreeOfABCD = RoseTree.map2(roseTreeOfABC, roseTreeOfD) { case ((a, b, c), d) => (a, b, c, d) }
            val roseTreeOfABCDE = RoseTree.map2(roseTreeOfABCD, roseTreeOfE) { case ((a, b, c, d), e) => (a, b, c, d, e)}
            val (bestABCDE, err) =
              roseTreeOfABCDE.shrinkSearch { case (a, b, c, d, e) => 
                Try { fun(a, b, c, d, e) } match {
                  case Success(_) => None
                  case Failure(shrunkEx) => Some(shrunkEx)
                }
              }.getOrElse((roseTreeOfA.value, ex))
            val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABCDE) else PropertyArgument(None, bestABCDE))
            val theRes = new PropertyCheckResult.Failure(succeededCount, Some(err), names, shrunkArgsPassed, initSeed)
            theRes
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
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

    /**
      * Checks a property for all combinations of generated values of types A, B, C, D, E, and F.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of the third generated value
      * @tparam D the type of the fourth generated value
      * @tparam E the type of the fifth generated value
      * @tparam F the type of the sixth generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param genD the generator for values of type D
      * @param genE the generator for values of type E
      * @param genF the generator for values of type F
      * @param fun the function to apply to the generated values
      * @return the result of the property check
      */
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
        val (roseTreeOfA, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, rnd1)
        val (roseTreeOfB, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (roseTreeOfC, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (roseTreeOfD, nextDEdges, rnd5) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)
        val (roseTreeOfE, nextEEdges, rnd6) = genE.next(SizeParam(PosZInt(0), maxSize, size), eEdges, rnd5)
        val (roseTreeOfF, nextFEdges, rnd7) = genF.next(SizeParam(PosZInt(0), maxSize, size), fEdges, rnd6)
        val a = roseTreeOfA.value
        val b = roseTreeOfB.value
        val c = roseTreeOfC.value
        val d = roseTreeOfD.value
        val e = roseTreeOfE.value
        val f = roseTreeOfF.value
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
            val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
            val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
            val roseTreeOfABCD = RoseTree.map2(roseTreeOfABC, roseTreeOfD) { case ((a, b, c), d) => (a, b, c, d) }
            val roseTreeOfABCDE = RoseTree.map2(roseTreeOfABCD, roseTreeOfE) { case ((a, b, c, d), e) => (a, b, c, d, e)}
            val roseTreeOfABCDEF = RoseTree.map2(roseTreeOfABCDE, roseTreeOfF) { case ((a, b, c, d, e), f) => (a, b, c, d, e, f)}
            val (bestABCDEF, err) =
              roseTreeOfABCDEF.shrinkSearch { case (a, b, c, d, e, f) => 
                Try { fun(a, b, c, d, e, f) } match {
                  case Success(_) => None
                  case Failure(shrunkEx) => Some(shrunkEx)
                }
              }.getOrElse((roseTreeOfA.value, ex))
            val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABCDEF) else PropertyArgument(None, bestABCDEF))
            val theRes = new PropertyCheckResult.Failure(succeededCount, Some(err), names, shrunkArgsPassed, initSeed)
            theRes
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
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

  abstract class FuturePropCheckerAssertingImpl[T] extends PropCheckerAsserting[Future[T]] {

    implicit val executionContext: scala.concurrent.ExecutionContext

    type Result = Future[Assertion]
    type S = T

    /**
      * Checks a property for all combinations of generated values of type A.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param fun the function to apply to the generated values
      * @return a Future containing the result of the property check
      */
    private def checkForAll[A](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A])(fun: (A) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, edges: List[A], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult], failedA: Option[A])

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
        val (roseTreeOfA, nextEdges, nextNextRnd) = genA.next(SizeParam(PosZInt(0), maxSize, size), edges, nextRnd) // TODO: Move PosZInt farther out
        val a = roseTreeOfA.value

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
                AccumulatedResult(succeededCount, discardedCount, edges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)), Some(roseTreeOfA.value))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, edges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, edges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)), Some(roseTreeOfA.value))
          } flatMap { result =>
            result.result match {
              case Some(f: PropertyCheckResult.Failure) => 
                roseTreeOfA.shrinkSearchForFuture(
                  value => {
                    val result: Future[T] = fun(value)
                    result.map { r =>
                      None
                    }.recoverWith {
                      case shrunkEx: Throwable =>
                      Future.successful(Some(shrunkEx))
                    }
                  }
                ).map { shrinkOpt =>
                  val (bestA, errOpt) = 
                    shrinkOpt match {
                      case Some((shrunkOfA, errOpt1)) => (shrunkOfA, Some(errOpt1))
                      case None => (roseTreeOfA.value, f.ex)
                    }
                  val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestA) else PropertyArgument(None, bestA))  
                  val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
                  AccumulatedResult(succeededCount, discardedCount, edges, nextNextRnd, initialSizes, Some(theRes), Some(bestA))
                }

              case Some(_) => Future.successful(result)
              case None => loop(result.succeededCount, result.discardedCount, result.edges, result.rnd, result.initialSizes, initSeed)
            }
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
            roseTreeOfA.shrinkSearchForFuture(
              value => {
                val result: Future[T] = fun(value)
                result.map { r =>
                  None
                }.recoverWith {
                  case shrunkEx: Throwable =>
                  Future.successful(Some(shrunkEx))
                }
              }
            ).map { shrinkOpt =>
              val (bestA, errOpt) = 
                shrinkOpt match {
                  case Some((shrunkOfA, errOpt1)) => (shrunkOfA, Some(errOpt1))
                  case None => (roseTreeOfA.value, Some(ex))
                }
              val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestA) else PropertyArgument(None, bestA))  
              val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
              AccumulatedResult(succeededCount, discardedCount, edges, nextNextRnd, initialSizes, Some(theRes), Some(bestA))
            }
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      // ensuringValid will always succeed because /ing a PosInt by a positive number will always yield a positive or zero
      val (initEdges, afterEdgesRnd) = genA.initEdges(PosZInt.ensuringValid(config.minSuccessful / 5), afterSizesRnd)

      loop(0, 0, initEdges, afterEdgesRnd, initialSizes, initSeed).map(_.result.getOrElse(PropertyCheckResult.Success(List.empty, initSeed)))
    }

    /**
      * Checks a property for all combinations of generated values of types A and B.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param fun the function to apply to the generated values
      * @return a Future containing the result of the property check
      */
    private def checkForAll[A, B](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A], genB: org.scalatest.prop.Generator[B])(fun: (A, B) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult], failedAB: Option[(A, B)])

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
        val (roseTreeOfA, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, nextRnd)
        val (roseTreeOfB, nextBEdges, nextNextRnd) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val a = roseTreeOfA.value
        val b = roseTreeOfB.value

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
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  AccumulatedResult(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextNextRnd, nextInitialSizes, None, None)
                else
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(PropertyCheckResult.Success(argsPassed, initSeed)), None)

              }
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)), Some((a, b)))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)), Some((a, b)))
          } flatMap { result =>

            result.result match {
              case Some(f: PropertyCheckResult.Failure) => 
                val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
                roseTreeOfAB.shrinkSearchForFuture { case (a, b) =>
                  val result: Future[T] = fun(a, b)
                  result.map { r =>
                    None
                  }.recoverWith {
                    case shrunkEx: Throwable =>
                    Future.successful(Some(shrunkEx))
                  }
                }.map { shrinkOpt =>
                  val (bestAB, errOpt) = 
                    shrinkOpt match {
                      case Some((shrunkOfAB, errOpt1)) => (shrunkOfAB, Some(errOpt1))
                      case None => (roseTreeOfAB.value, f.ex)
                    }
                  val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestAB) else PropertyArgument(None, bestAB))  
                  val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, result.rnd, initialSizes, Some(theRes), Some(bestAB))
                }
                
              case Some(_) => Future.successful(result)
              case None => loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.rnd, result.initialSizes, initSeed)
            }
          }
        }
        catch {
          case ex: DiscardedEvaluationException =>
            val nextDiscardedCount = discardedCount + 1
            val result =
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.rnd, result.initialSizes, initSeed)

          case ex: Throwable =>
            val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
            roseTreeOfAB.shrinkSearchForFuture { case (a, b) =>
              val result: Future[_] = fun(a, b)
              result.map { r =>
                None
              }.recoverWith {
                case shrunkEx: Throwable =>
                Future.successful(Some(shrunkEx))
              }
            }.map { shrinkOpt =>
              val (bestAB, errOpt) = 
                shrinkOpt match {
                  case Some((shrunkOfAB, errOpt1)) => (shrunkOfAB, Some(errOpt1))
                  case None => (roseTreeOfAB.value, Some(ex))
                }
              val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestAB) else PropertyArgument(None, bestAB))  
              val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, nextNextRnd, initialSizes, Some(theRes), Some(bestAB))
            }
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)

      loop(0, 0, initAEdges, initBEdges, afterBEdgesRnd, initialSizes, initSeed).map(_.result.getOrElse(PropertyCheckResult.Success(List.empty, initSeed)))
    }

    /**
      * Checks a property for all combinations of generated values of types A, B, and C.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of the third generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param fun the function to apply to the generated values
      * @return a Future containing the result of the property check
      */
    private def checkForAll[A, B, C](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A], genB: org.scalatest.prop.Generator[B],
                                     genC: org.scalatest.prop.Generator[C])(fun: (A, B, C) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult], failedABC: Option[(A, B, C)])

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
        val (roseTreeOfA, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, nextRnd)
        val (roseTreeOfB, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (roseTreeOfC, nextCEdges, nextNextRnd) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val a = roseTreeOfA.value
        val b = roseTreeOfB.value
        val c = roseTreeOfC.value
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
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  AccumulatedResult(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextNextRnd, nextInitialSizes, None, None)
                else
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(PropertyCheckResult.Success(argsPassed, initSeed)), None)

              }
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)), Some(a, b, c))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)), Some(a, b, c))
          } flatMap { result =>
            result.result match {
              case Some(f: PropertyCheckResult.Failure) => 
                val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
                val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
                roseTreeOfABC.shrinkSearchForFuture { case (a, b, c) =>
                  val result: Future[T] = fun(a, b, c)
                  result.map { r =>
                    None
                  }.recoverWith {
                    case shrunkEx: Throwable =>
                    Future.successful(Some(shrunkEx))
                  }
                }.map { shrinkOpt =>
                  val (bestABC, errOpt) = 
                    shrinkOpt match {
                      case Some((shrunkOfABC, errOpt1)) => (shrunkOfABC, Some(errOpt1))
                      case None => (roseTreeOfABC.value, f.ex)
                    }
                  val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABC) else PropertyArgument(None, bestABC))  
                  val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, result.rnd, initialSizes, Some(theRes), Some(bestABC))
                }
                
              case Some(_) => Future.successful(result)
              case None => loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.rnd, result.initialSizes, initSeed)
            }
          }
        }
        catch {
          case ex: DiscardedEvaluationException =>
            val nextDiscardedCount = discardedCount + 1
            val result =
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.rnd, result.initialSizes, initSeed)

          case ex: Throwable =>
            val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
            val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
            roseTreeOfABC.shrinkSearchForFuture { case (a, b, c) =>
              val result: Future[_] = fun(a, b, c)
              result.map { r =>
                None
              }.recoverWith {
                case shrunkEx: Throwable =>
                Future.successful(Some(shrunkEx))
              }
            }.map { shrinkOpt =>
              val (bestABC, errOpt) = 
                shrinkOpt match {
                  case Some((shrunkOfABC, errOpt1)) => (shrunkOfABC, Some(errOpt1))
                  case None => (roseTreeOfABC.value, Some(ex))
                }
              val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABC) else PropertyArgument(None, bestABC))  
              val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, nextNextRnd, initialSizes, Some(theRes), Some(bestABC))
            }
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)

      loop(0, 0, initAEdges, initBEdges, initCEdges, afterCEdgesRnd, initialSizes, initSeed).map(_.result.getOrElse(PropertyCheckResult.Success(List.empty, initSeed)))
    }

    /**
      * Checks a property for all combinations of generated values of types A, B, C, and D.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of the third generated value
      * @tparam D the type of the fourth generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param genD the generator for values of type D
      * @param fun the function to apply to the generated values
      * @return a Future containing the result of the property check
      */
    private def checkForAll[A, B, C, D](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A], genB: org.scalatest.prop.Generator[B],
                                     genC: org.scalatest.prop.Generator[C], genD: org.scalatest.prop.Generator[D])(fun: (A, B, C, D) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult], failedABCD: Option[(A, B, C, D)])

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
        val (roseTreeOfA, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, nextRnd)
        val (roseTreeOfB, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (roseTreeOfC, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (roseTreeOfD, nextDEdges, nextNextRnd) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)
        val a = roseTreeOfA.value
        val b = roseTreeOfB.value
        val c = roseTreeOfC.value
        val d = roseTreeOfD.value
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
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  AccumulatedResult(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextNextRnd, nextInitialSizes, None, None)
                else
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(PropertyCheckResult.Success(argsPassed, initSeed)), None)

              }
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)), Some(a, b, c, d))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)), Some(a, b, c, d))
          } flatMap { result =>
            result.result match {
              case Some(f: PropertyCheckResult.Failure) => 
                val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
                val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
                val roseTreeOfABCD = RoseTree.map2(roseTreeOfABC, roseTreeOfD) { case ((a, b, c), d) => (a, b, c, d) }
                roseTreeOfABCD.shrinkSearchForFuture { case (a, b, c, d) =>
                  val result: Future[_] = fun(a, b, c, d)
                  result.map { r =>
                    None
                  }.recoverWith {
                    case shrunkEx: Throwable =>
                    Future.successful(Some(shrunkEx))
                  }
                }.map { shrinkOpt =>
                  val (bestABCD, errOpt) = 
                    shrinkOpt match {
                      case Some((shrunkOfABCD, errOpt1)) => (shrunkOfABCD, Some(errOpt1))
                      case None => (roseTreeOfABCD.value, f.ex)
                    }
                  val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABCD) else PropertyArgument(None, bestABCD))  
                  val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, result.rnd, initialSizes, Some(theRes), Some(bestABCD))
                }
                
              case Some(_) => Future.successful(result)
              case None => loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.rnd, result.initialSizes, initSeed)
            }
          }
        }
        catch {
          case ex: DiscardedEvaluationException =>
            val nextDiscardedCount = discardedCount + 1
            val result =
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.rnd, result.initialSizes, initSeed)

          case ex: Throwable =>
            val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
            val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
            val roseTreeOfABCD = RoseTree.map2(roseTreeOfABC, roseTreeOfD) { case ((a, b, c), d) => (a, b, c, d) }
            roseTreeOfABCD.shrinkSearchForFuture { case (a, b, c, d) =>
              val result: Future[_] = fun(a, b, c, d)
              result.map { r =>
                None
              }.recoverWith {
                case shrunkEx: Throwable =>
                Future.successful(Some(shrunkEx))
              }
            }.map { shrinkOpt =>
              val (bestABCD, errOpt) = 
                shrinkOpt match {
                  case Some((shrunkOfABCD, errOpt1)) => (shrunkOfABCD, Some(errOpt1))
                  case None => (roseTreeOfABCD.value, Some(ex))
                }
              val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABCD) else PropertyArgument(None, bestABCD))  
              val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, nextNextRnd, initialSizes, Some(theRes), Some(bestABCD))
            }
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)

      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, afterDEdgesRnd, initialSizes, initSeed).map(_.result.getOrElse(PropertyCheckResult.Success(List.empty, initSeed)))
    }

    /**
      * Checks a property for all combinations of generated values of types A, B, C, D, and E.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of the third generated value
      * @tparam D the type of the fourth generated value
      * @tparam E the type of the fifth generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param genD the generator for values of type D
      * @param genE the generator for values of type E
      * @param fun the function to apply to the generated values
      * @return a Future containing the result of the property check
      */
    private def checkForAll[A, B, C, D, E](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A], genB: org.scalatest.prop.Generator[B],
                                        genC: org.scalatest.prop.Generator[C], genD: org.scalatest.prop.Generator[D], genE: org.scalatest.prop.Generator[E])(fun: (A, B, C, D, E) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult], failedABCDE: Option[(A, B, C, D, E)])

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
        val (roseTreeOfA, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, nextRnd)
        val (roseTreeOfB, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (roseTreeOfC, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (roseTreeOfD, nextDEdges, rnd5) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)
        val (roseTreeOfE, nextEEdges, nextNextRnd) = genE.next(SizeParam(PosZInt(0), maxSize, size), eEdges, rnd5)
        val a = roseTreeOfA.value
        val b = roseTreeOfB.value
        val c = roseTreeOfC.value
        val d = roseTreeOfD.value
        val e = roseTreeOfE.value
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
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  AccumulatedResult(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextNextRnd, nextInitialSizes, None, None)
                else
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(PropertyCheckResult.Success(argsPassed, initSeed)), None)

              }
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)), Some((a, b, c, d, e)))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)), Some((a, b, c, d, e)))
          } flatMap { result =>
            result.result match {
              case Some(f: PropertyCheckResult.Failure) => 
                val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
                val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
                val roseTreeOfABCD = RoseTree.map2(roseTreeOfABC, roseTreeOfD) { case ((a, b, c), d) => (a, b, c, d) }
                val roseTreeOfABCDE = RoseTree.map2(roseTreeOfABCD, roseTreeOfE) { case ((a, b, c, d), e) => (a, b, c, d, e)}
                roseTreeOfABCDE.shrinkSearchForFuture { case (a, b, c, d, e) =>
                  val result: Future[_] = fun(a, b, c, d, e)
                  result.map { r =>
                    None
                  }.recoverWith {
                    case shrunkEx: Throwable =>
                    Future.successful(Some(shrunkEx))
                  }
                }.map { shrinkOpt =>
                  val (bestABCDE, errOpt) = 
                    shrinkOpt match {
                      case Some((shrunkOfABCDE, errOpt1)) => (shrunkOfABCDE, Some(errOpt1))
                      case None => (roseTreeOfABCDE.value, f.ex)
                    }
                  val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABCDE) else PropertyArgument(None, bestABCDE))  
                  val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, result.rnd, initialSizes, Some(theRes), Some(bestABCDE))
                }
                
              case Some(_) => Future.successful(result)
              case None => loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.eEdges, result.rnd, result.initialSizes, initSeed)
            }
          }
        }
        catch {
          case ex: DiscardedEvaluationException =>
            val nextDiscardedCount = discardedCount + 1
            val result =
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.eEdges, result.rnd, result.initialSizes, initSeed)

          case ex: Throwable =>
            val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
            val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
            val roseTreeOfABCD = RoseTree.map2(roseTreeOfABC, roseTreeOfD) { case ((a, b, c), d) => (a, b, c, d) }
            val roseTreeOfABCDE = RoseTree.map2(roseTreeOfABCD, roseTreeOfE) { case ((a, b, c, d), e) => (a, b, c, d, e)}
            roseTreeOfABCDE.shrinkSearchForFuture { case (a, b, c, d, e) =>
              val result: Future[_] = fun(a, b, c, d, e)
              result.map { r =>
                None
              }.recoverWith {
                case shrunkEx: Throwable =>
                Future.successful(Some(shrunkEx))
              }
            }.map { shrinkOpt =>
              val (bestABCDE, errOpt) = 
                shrinkOpt match {
                  case Some((shrunkOfABCDE, errOpt1)) => (shrunkOfABCDE, Some(errOpt1))
                  case None => (roseTreeOfABCDE.value, Some(ex))
                }
              val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABCDE) else PropertyArgument(None, bestABCDE))  
              val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, nextNextRnd, initialSizes, Some(theRes), Some(bestABCDE))
            }
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
      val (initEEdges, afterEEdgesRnd) = genE.initEdges(maxEdges, afterDEdgesRnd)

      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, initEEdges, afterEEdgesRnd, initialSizes, initSeed).map(_.result.getOrElse(PropertyCheckResult.Success(List.empty, initSeed)))
    }

    /**
      * Checks a property for all combinations of generated values of types A, B, C, D, E, and F.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to each combination of generated values. It tracks the number of successful evaluations, 
      * discarded evaluations, and maintains relevant state. It will stop when the specified number 
      * of successful evaluations has been reached or the maximum number of discarded evaluations 
      * has been exceeded.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of the third generated value
      * @tparam D the type of the fourth generated value
      * @tparam E the type of the fifth generated value
      * @tparam F the type of the sixth generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param genD the generator for values of type D
      * @param genE the generator for values of type E
      * @param genF the generator for values of type F
      * @param fun the function to apply to the generated values
      * @return a Future containing the result of the property check
      */
    private def checkForAll[A, B, C, D, E, F](names: List[String], config: Parameter, genA: org.scalatest.prop.Generator[A], genB: org.scalatest.prop.Generator[B],
                                           genC: org.scalatest.prop.Generator[C], genD: org.scalatest.prop.Generator[D], genE: org.scalatest.prop.Generator[E],
                                           genF: org.scalatest.prop.Generator[F])(fun: (A, B, C, D, E, F) => Future[T]): Future[PropertyCheckResult] = {

      case class AccumulatedResult(succeededCount: Int, discardedCount: Int, aEdges: List[A], bEdges: List[B], cEdges: List[C], dEdges: List[D], eEdges: List[E], fEdges: List[F], rnd: Randomizer, initialSizes: List[PosZInt], result: Option[PropertyCheckResult], failedABCDEF: Option[(A, B, C, D, E, F)])

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
        val (roseTreeOfA, nextAEdges, rnd2) = genA.next(SizeParam(PosZInt(0), maxSize, size), aEdges, nextRnd)
        val (roseTreeOfB, nextBEdges, rnd3) = genB.next(SizeParam(PosZInt(0), maxSize, size), bEdges, rnd2)
        val (roseTreeOfC, nextCEdges, rnd4) = genC.next(SizeParam(PosZInt(0), maxSize, size), cEdges, rnd3)
        val (roseTreeOfD, nextDEdges, rnd5) = genD.next(SizeParam(PosZInt(0), maxSize, size), dEdges, rnd4)
        val (roseTreeOfE, nextEEdges, rnd6) = genE.next(SizeParam(PosZInt(0), maxSize, size), eEdges, rnd5)
        val (roseTreeOfF, nextFEdges, nextNextRnd) = genF.next(SizeParam(PosZInt(0), maxSize, size), fEdges, rnd6)
        val a = roseTreeOfA.value
        val b = roseTreeOfB.value
        val c = roseTreeOfC.value
        val d = roseTreeOfD.value
        val e = roseTreeOfE.value
        val f = roseTreeOfF.value
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
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            }
            else {
              val (success, cause) = succeed(r)
              if (success) {
                val nextSucceededCount = succeededCount + 1
                if (nextSucceededCount < config.minSuccessful)
                  AccumulatedResult(nextSucceededCount, discardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, nextNextRnd, nextInitialSizes, None, None)
                else
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(PropertyCheckResult.Success(argsPassed, initSeed)), None)

              }
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, cause, names, argsPassed, initSeed)), Some((a, b, c, d, e, f)))

            }
          } recover {
            case ex: DiscardedEvaluationException =>
              val nextDiscardedCount = discardedCount + 1
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            case ex: Throwable =>
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(new PropertyCheckResult.Failure(succeededCount, Some(ex), names, argsPassed, initSeed)), Some((a, b, c, d, e, f)))
          } flatMap { result =>
            result.result match {
              case Some(f: PropertyCheckResult.Failure) => 
                val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
                val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
                val roseTreeOfABCD = RoseTree.map2(roseTreeOfABC, roseTreeOfD) { case ((a, b, c), d) => (a, b, c, d) }
                val roseTreeOfABCDE = RoseTree.map2(roseTreeOfABCD, roseTreeOfE) { case ((a, b, c, d), e) => (a, b, c, d, e)}
                val roseTreeOfABCDEF = RoseTree.map2(roseTreeOfABCDE, roseTreeOfF) { case ((a, b, c, d, e), f) => (a, b, c, d, e, f)}
                roseTreeOfABCDEF.shrinkSearchForFuture { case (a, b, c, d, e, f) =>
                  val result: Future[_] = fun(a, b, c, d, e, f)
                  result.map { r =>
                    None
                  }.recoverWith {
                    case shrunkEx: Throwable =>
                    Future.successful(Some(shrunkEx))
                  }
                }.map { shrinkOpt =>
                  val (bestABCDEF, errOpt) = 
                    shrinkOpt match {
                      case Some((shrunkOfABCDEF, errOpt1)) => (shrunkOfABCDEF, Some(errOpt1))
                      case None => (roseTreeOfABCDEF.value, f.ex)
                    }
                  val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABCDEF) else PropertyArgument(None, bestABCDEF))  
                  val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
                  AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, result.rnd, initialSizes, Some(theRes), Some(bestABCDEF))
                }
                
              case Some(_) => Future.successful(result)
              case None => loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.eEdges, result.fEdges, result.rnd, result.initialSizes, initSeed)
            }
          }
        }
        catch {
          case ex: DiscardedEvaluationException =>
            val nextDiscardedCount = discardedCount + 1
            val result =
              if (nextDiscardedCount < maxDiscarded)
                AccumulatedResult(succeededCount, nextDiscardedCount, nextAEdges, nextBEdges, nextCEdges, nextDEdges, nextEEdges, nextFEdges, nextNextRnd, nextInitialSizes, None, None)
              else
                AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, rnd, initialSizes, Some(new PropertyCheckResult.Exhausted(succeededCount, nextDiscardedCount, names, argsPassed, initSeed)), None)

            if (result.result.isDefined)
              Future.successful(result)
            else
              loop(result.succeededCount, result.discardedCount, result.aEdges, result.bEdges, result.cEdges, result.dEdges, result.eEdges, result.fEdges, result.rnd, result.initialSizes, initSeed)

          case ex: Throwable =>
            val roseTreeOfAB = RoseTree.map2(roseTreeOfA, roseTreeOfB) { case (a, b) => (a, b) }
            val roseTreeOfABC = RoseTree.map2(roseTreeOfAB, roseTreeOfC) { case ((a, b), c) => (a, b, c) }
            val roseTreeOfABCD = RoseTree.map2(roseTreeOfABC, roseTreeOfD) { case ((a, b, c), d) => (a, b, c, d) }
            val roseTreeOfABCDE = RoseTree.map2(roseTreeOfABCD, roseTreeOfE) { case ((a, b, c, d), e) => (a, b, c, d, e)}
            val roseTreeOfABCDEF = RoseTree.map2(roseTreeOfABCDE, roseTreeOfF) { case ((a, b, c, d, e), f) => (a, b, c, d, e, f)}
            roseTreeOfABCDEF.shrinkSearchForFuture { case (a, b, c, d, e, f) =>
              val result: Future[_] = fun(a, b, c, d, e, f)
              result.map { r =>
                None
              }.recoverWith {
                case shrunkEx: Throwable =>
                Future.successful(Some(shrunkEx))
              }
            }.map { shrinkOpt =>
              val (bestABCDEF, errOpt) = 
                shrinkOpt match {
                  case Some((shrunkOfABCDEF, errOpt1)) => (shrunkOfABCDEF, Some(errOpt1))
                  case None => (roseTreeOfABCDEF.value, Some(ex))
                }
              val shrunkArgsPassed = List(if (names.isDefinedAt(0)) PropertyArgument(Some(names(0)), bestABCDEF) else PropertyArgument(None, bestABCDEF))  
              val theRes = new PropertyCheckResult.Failure(succeededCount, errOpt, names, shrunkArgsPassed, initSeed)
              AccumulatedResult(succeededCount, discardedCount, aEdges, bEdges, cEdges, dEdges, eEdges, fEdges, nextNextRnd, initialSizes, Some(theRes), Some(bestABCDEF))
            }
        }
      }

      val initRnd = Randomizer.default // This can be set by a cmd line param.
      val initSeed = initRnd.seed
      val (initialSizes, afterSizesRnd) = PropCheckerAsserting.calcSizes(minSize, maxSize, initRnd)
      val maxEdges = PosZInt.ensuringValid(config.minSuccessful / 5) // Because PosInt / positive Int is always going to be positive
      val (initAEdges, afterAEdgesRnd) = genA.initEdges(maxEdges, afterSizesRnd)
      val (initBEdges, afterBEdgesRnd) = genB.initEdges(maxEdges, afterAEdgesRnd)
      val (initCEdges, afterCEdgesRnd) = genC.initEdges(maxEdges, afterBEdgesRnd)
      val (initDEdges, afterDEdgesRnd) = genD.initEdges(maxEdges, afterCEdgesRnd)
      val (initEEdges, afterEEdgesRnd) = genE.initEdges(maxEdges, afterDEdgesRnd)
      val (initFEdges, afterFEdgesRnd) = genF.initEdges(maxEdges, afterEEdgesRnd)

      loop(0, 0, initAEdges, initBEdges, initCEdges, initDEdges, initEEdges, initFEdges, afterFEdgesRnd, initialSizes, initSeed).map(_.result.getOrElse(PropertyCheckResult.Success(List.empty, initSeed)))
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

    /**
      * Checks a property using a single generated value of type A.
      *
      * This method generates a value using the provided generator and applies the given function `fun`
      * to the generated value. It tracks the number of successful evaluations and any relevant state.
      * It will stop when the specified number of successful evaluations has been reached.
      *
      * @tparam A the type of the generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param fun the function to apply to the generated value
      * @return the result of the property check
      */
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

    /**
      * Checks a property using two generated values of types A and B.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to the generated values. It tracks the number of successful evaluations and any relevant state.
      * It will stop when the specified number of successful evaluations has been reached.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param fun the function to apply to the generated values
      * @return the result of the property check
      */
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

    /**
      * Checks a property using three generated values of types A, B, and C.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to the generated values. It tracks the number of successful evaluations and any relevant state.
      * It will stop when the specified number of successful evaluations has been reached.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of third generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param fun the function to apply to the generated values
      * @return the result of the property check
      */
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

    /**
      * Checks a property using four generated values of types A, B, C, and D.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to the generated values. It tracks the number of successful evaluations and any relevant state.
      * It will stop when the specified number of successful evaluations has been reached.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of the third generated value
      * @tparam D the type of the fourth generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param genD the generator for values of type D
      * @param fun the function to apply to the generated values
      * @return the result of the property check
      */
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

    /**
      * Checks a property using four generated values of types A, B, C, D, and E.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to the generated values. It tracks the number of successful evaluations and any relevant state.
      * It will stop when the specified number of successful evaluations has been reached.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of the third generated value
      * @tparam D the type of the fourth generated value
      * @tparam E the type of the fifth generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param genD the generator for values of type D
      * @param genE the generator for values of type E
      * @param fun the function to apply to the generated values
      * @return the result of the property check
      */
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

    /**
      * Checks a property using four generated values of types A, B, C, D, E, and F.
      *
      * This method generates values using the provided generators and applies the given function `fun`
      * to the generated values. It tracks the number of successful evaluations and any relevant state.
      * It will stop when the specified number of successful evaluations has been reached.
      *
      * @tparam A the type of the first generated value
      * @tparam B the type of the second generated value
      * @tparam C the type of the third generated value
      * @tparam D the type of the fourth generated value
      * @tparam E the type of the fifth generated value
      * @tparam F the type of the sixth generated value
      * @param names a list of names for the generated arguments, used for reporting results
      * @param config the configuration parameters for property checking
      * @param genA the generator for values of type A
      * @param genB the generator for values of type B
      * @param genC the generator for values of type C
      * @param genD the generator for values of type D
      * @param genE the generator for values of type E
      * @param genF the generator for values of type F
      * @param fun the function to apply to the generated values
      * @return the result of the property check
      */
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
