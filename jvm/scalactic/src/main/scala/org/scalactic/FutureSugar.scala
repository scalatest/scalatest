/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalactic

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import exceptions.ValidationFailedException

/**
 * Trait providing an implicit class that adds a <code>validating</code> method to
 * <code>Future</code>, which takes one or more validation functions and returns either the
 * same <code>Future</code> if either the <code>Future</code> had already failed or its value
 * passes all the functions, or [[org.scalactic.exceptions.ValidationFailedException `ValidationFailedException`]] containing an error message
 * describing the first validation that failed.
 *
 * <p>
 * Here's an example validation method, which passes if the given <code>Int</code> is evenly
 * divisible by 10 (<em>i.e.</em>, the result will be [[org.scalactic.Pass <code>Pass</code>]]). If the value does not pass
 * this test, the result is a [[org.scalactic.Fail <code>Fail</code>]] containing a helpful error message string.
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import FutureSugar._
 * import org.scalactic.FutureSugar._
 *
 * scala&gt; import scala.concurrent.Future
 * import scala.concurrent.Future
 *
 * scala&gt; import scala.concurrent.ExecutionContext.Implicits.global
 * import scala.concurrent.ExecutionContext.Implicits.global
 *
 * scala&gt; def isRound(i: Int): Validation[ErrorMessage] =
 *      |   if (i % 10 == 0) Pass else Fail(i + " was not a round number")
 * isRound: (i: Int)org.scalactic.Validation[org.scalactic.ErrorMessage]
 * </pre>
 *
 * <p>
 * Validation will be attempted on a successful <code>Try</code>. If the validation succeeds, the
 * resulting <code>Future</code> will be the same successful <code>Future</code> with the same value. (A
 * "validation" only transforms the <code>Future</code> if the validation fails, otherwise it is the
 * same <code>Future</code>. The only difference is its value has now been proven <em>valid</em>.)
 * In the following example, a successful <code>Future[Int]</code> with the value 100
 * passes the validation (which checks whether 100 is evenly divisible by 10), therefore
 * the result of the <code>validating</code> call is the same successful <code>Future</code>
 * with the same value.
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val fut100 = Future(100)
 * fut100: scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPromise@67f9c9c6
 *
 * scala&gt; fut100.value
 * res0: Option[scala.util.Try[Int]] = Some(Success(100))
 *
 * scala&gt; val round100 = fut100.validating(isRound)
 * round100: scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPromise@1ac2f0d1
 *
 * scala&gt; round100.value
 * res1: Option[scala.util.Try[Int]] = Some(Success(100))
 * </pre>
 *
 * <p>
 * If validation fails, the successful <code>Future</code> will be transformed into a failed one, with
 * a <code>ValidationFailedException</code> that contains the error message
 * returned by the validation function. In the following example, 42 fails the validation because it
 * is not evenly divisible by 10:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val fut42 = Future(42)
 * fut42: scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPromise@19c6e4d1
 *
 * scala&gt; fut42.value
 * res2: Option[scala.util.Try[Int]] = Some(Success(42))
 *
 * scala&gt; val round42 = fut42.validating(isRound)
 * round42: scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPromise@b5175d
 *
 * scala&gt; round42.value
 * res3: Option[scala.util.Try[Int]] = Some(Failure(org.scalactic.exceptions.ValidationFailedException: 42 was not a round number))
 * </pre>
 *
 * <p>
 * If <code>validating</code> is called on a failed <code>Future</code>, it just returns the same failed <code>Future</code>:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val futEx = Future[Int] { throw new Exception("oops!") }
 * futEx: scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPromise@3ba0299c
 *
 * scala&gt; futEx.value
 * res4: Option[scala.util.Try[Int]] = Some(Failure(java.lang.Exception: oops!))
 *
 * scala&gt; val roundEx = futEx.validating(isRound)
 * roundEx: scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPromise@22bf1acf
 *
 * scala&gt; roundEx.value
 * res5: Option[scala.util.Try[Int]] = Some(Failure(java.lang.Exception: oops!))
 * </pre>
 *
 * <p>
 * The <code>validating</code> method accepts one or more validation functions. If you 
 * pass more than one, they will be tried in order up until the first failure, whose
 * error message will appear in the <code>ValidationFailedException</code>. In other words,
 * <code>validating</code> will short circuit at the first error and return that. It
 * will not accumulate errors. For example, the following validation will short circuit
 * after the <code>isDivBy3</code> function fails:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; def isDivBy3(i: Int): Validation[ErrorMessage] =
 *      |   if (i % 3 == 0) Pass else Fail(i + " was not divisible by 3")
 * isDivBy3: (i: Int)org.scalactic.Validation[org.scalactic.ErrorMessage]
 *
 * scala&gt; def isAnswerToLifeTheUniverseAndEverything(i: Int): Validation[ErrorMessage] =
 *      |   if (i == 42) Pass else Fail(i + " did not equal 42")
 * isAnswerToLifeTheUniverseAndEverything: (i: Int)org.scalactic.Validation[org.scalactic.ErrorMessage]
 *
 * scala&gt; val futShort = fut100.validating(isRound, isDivBy3, isAnswerToLifeTheUniverseAndEverything)
 * futShort: scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPromise@30bb943e
 *
 * scala&gt; futShort.value
 * res11: Option[scala.util.Try[Int]] = Some(Failure(org.scalactic.exceptions.ValidationFailedException: 100 was not divisible by 3))
 * </pre>
 */
trait FutureSugar {

  /**
   * Implicit class that adds a <code>validation</code> method to
   * <code>Future</code>, which takes one or more functions that validate
   * the <code>Future</code>'s value.
   *
   * <p>
   * See the main documentation for trait [[org.scalactic.FutureSugar `FutureSugar`]] for more detail and examples.
   * </p>
   *
   * @param theFuture the <code>Future</code> to which to add a <code>validating</code> method.
   */
  implicit class Futureizer[T](theFuture: Future[T]) {

    /**
     * Validates a <code>Future</code> using the passed validation functions.
     *
     * <p>
     * See the main documentation for trait [[org.scalactic.FutureSugar `FutureSugar`]] for more detail and examples.
     * </p>
     *
     * @param first the first validation function to apply
     * @param rest the subsequent validation functions to apply, if any
     * @return a "validated" <code>Future</code>, either a <code>Future</code> with the same value, or
     *   if validation failed, a failed <code>Future</code> containing a <code>ValidationFailedException</code>.
     */
    def validating(first: T => Validation[ErrorMessage], rest: (T => Validation[ErrorMessage])*)(implicit executor: ExecutionContext): Future[T] = {
      theFuture.flatMap { (o: T) =>
        TrySugar.passOrFirstFail(o, first :: rest.toList) match {
          case Pass => theFuture
          case Fail(errorMessage) => Future.failed(ValidationFailedException(errorMessage))
        }
      }
    }
  }
}

/**
 * Companion object for <code>FutureSugar</code> enabling its members to be
 * imported as an alternative to mixing them in.
 */
object FutureSugar extends FutureSugar
