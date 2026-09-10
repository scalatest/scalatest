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

import org.scalatest.{Assertion, Expectation}
import org.scalatest.exceptions.DiscardedEvaluationException
import scala.concurrent.Future

/**
  * Supertrait for <code>WheneverAsserting</code> typeclasses, which are used to implement and determine the result
  * type of [[org.scalatest.prop.Whenever Whenever]]'s <code>whenever</code> method.
  *
  * <p>
  * Currently, an [[org.scalatest.prop.Whenever Whenever]] expression will have result type <code>Assertion</code>, if the function passed has result type <code>Assertion</code>,
  * else it will have result type <code>Unit</code>.
  * </p>
  *
  * <p>
  * The trait is parameterized with two type parameters:
  * </p>
  *
  * <ul>
  * <li><code>T</code> - The type of the function passed to <code>whenever</code>.</li>
  * <li><code>Result</code> - The result type of the <code>whenever</code> method.
  *     This is the final result that users receive after evaluating the function.</li>
  * </ul>
  *
  * @tparam T the type of the function passed to <code>whenever</code>
  * @tparam Result the result type of the <code>whenever</code> method
  */
trait WheneverAsserting[T, Result] {
  /**
    * Implementation method for [[org.scalatest.prop.Whenever Whenever]]'s <code>whenever</code> syntax.
    *
    * @param condition the boolean condition that determines whether <code>whenever</code> will evaluate the
    *    <code>fun</code> function (<code>condition</code> is true) or throws <code>DiscardedEvaluationException</code> (<code>condition</code> is false)
    * @param fun the function to evaluate if the specified <code>condition</code> is true
    * @return the result of evaluating the function
    */
  def whenever(condition: Boolean)(fun: => T): Result
}

/**
  * Class holding lowest priority <code>WheneverAsserting</code> implicit, which enables [[org.scalatest.prop.Whenever Whenever]] expressions that have result type <code>Unit</code>.
  */
abstract class UnitWheneverAsserting {

  /**
    * Provides support of [[org.scalatest.enablers.WheneverAsserting WheneverAsserting]] for Unit.  Return <code>Unit</code> when the check succeeds,
    * but throw [[org.scalatest.exceptions.DiscardedEvaluationException DiscardedEvaluationException]]
    * when check fails.
    */
  // SKIP-DOTTY-START
  implicit def assertingNatureOfT[T]: WheneverAsserting[T, Unit] = {
  // SKIP-DOTTY-END
  //DOTTY-ONLY def assertingNatureOfT[T]: WheneverAsserting[T, Unit] = {
    new WheneverAsserting[T, Unit] {
      def whenever(condition: Boolean)(fun: => T): Unit =
        if (!condition)
          throw new DiscardedEvaluationException
        else
          fun
    }
  }
  //DOTTY-ONLY given [T]: WheneverAsserting[T, Unit] = assertingNatureOfT
}

/**
  * Abstract class that in the future will hold an intermediate priority <code>WheneverAsserting</code> implicit, which will enable inspector expressions
  * that have result type <code>Expectation</code>, a more composable form of assertion that returns a result instead of throwing an exception when it fails.
  */
abstract class ExpectationWheneverAsserting extends UnitWheneverAsserting {
  // SKIP-DOTTY-START
  implicit def assertingNatureOfExpectation: WheneverAsserting[Expectation, Expectation] = {
  // SKIP-DOTTY-END
  //DOTTY-ONLY def assertingNatureOfExpectation: WheneverAsserting[Expectation, Expectation] = {
    new WheneverAsserting[Expectation, Expectation] {
      def whenever(condition: Boolean)(fun: => Expectation): Expectation =
        if (!condition)
          throw new DiscardedEvaluationException
        else
         fun
    }
  }
  //DOTTY-ONLY given WheneverAsserting[Expectation, Expectation] = assertingNatureOfExpectation
  // SKIP-DOTTY-START
  implicit def assertingNatureOfFutureAssertion: WheneverAsserting[Future[Assertion], Future[Assertion]] = {
  // SKIP-DOTTY-END
  //DOTTY-ONLY def assertingNatureOfFutureAssertion: WheneverAsserting[Future[Assertion], Future[Assertion]] = {
    new WheneverAsserting[Future[Assertion], Future[Assertion]] {
      def whenever(condition: Boolean)(fun: => Future[Assertion]): Future[Assertion] =
        if (!condition)
          throw new DiscardedEvaluationException
        else
          fun
    }
  }
  //DOTTY-ONLY given WheneverAsserting[Future[Assertion], Future[Assertion]] = assertingNatureOfFutureAssertion
}

/**
  * Companion object to <code>WheneverAsserting</code> that provides two implicit providers, a higher priority one for passed functions that have result
  * type <code>Assertion</code>, which also yields result type <code>Assertion</code>, and one for any other type, which yields result type <code>Unit</code>.
  */
object WheneverAsserting extends ExpectationWheneverAsserting {
  // SKIP-DOTTY-START
  implicit def assertingNatureOfAssertion: WheneverAsserting[Assertion, Assertion] = {
  // SKIP-DOTTY-END
  //DOTTY-ONLY def assertingNatureOfAssertion: WheneverAsserting[Assertion, Assertion] = {
    new WheneverAsserting[Assertion, Assertion] {
      def whenever(condition: Boolean)(fun: => Assertion): Assertion =
        if (!condition)
          throw new DiscardedEvaluationException
        else
          fun
    }
  }
  //DOTTY-ONLY given WheneverAsserting[Assertion, Assertion] = assertingNatureOfAssertion
}