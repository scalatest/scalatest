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
package org.scalactic

import scala.util.Try
import scala.util.Failure
import annotation.tailrec
import exceptions.ValidationFailedException

/**
 * Trait providing an implicit class that adds a <code>toOr</code> method to
 * <code>Try</code>, which converts <code>Success</code> to <code>Good</code>,
 * and <code>Failure</code> to <code>Bad</code>, as well as a <code>validating</code> method,
 * which takes one or more validation functions and returns either the
 * same <code>Try</code> if either the <code>Try</code> had already failed or its value
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
 * scala&gt; import TrySugar._
 * import TrySugar._
 *
 * scala&gt; import scala.util.Try
 * import scala.util.Try
 *
 * scala&gt; def isRound(i: Int): Validation[ErrorMessage] =
 *      |   if (i % 10 == 0) Pass else Fail(i + " was not a round number")
 * isRound: (i: Int)org.scalactic.Validation[org.scalactic.ErrorMessage]
 * </pre>
 *
 * <p>
 * Validation will be attempted on a successful <code>Try</code>. If the validation succeeds, the
 * resulting <code>Try</code> will be the same successful <code>Try</code> with the same value. (A
 * "validation" only transforms the <code>Try</code> if the validation fails, otherwise it is the
 * same <code>Try</code>. The only difference is its value has now been proven <em>valid</em>.)
 * In the following example, a successful <code>Try[Int]</code> with the value 100
 * passes the validation (which checks whether 100 is evenly divisible by 10), therefore
 * the result of the <code>validating</code> call is the same successful <code>Try</code>
 * with the same value.
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val try100 = Try(100)
 * try100: scala.util.Try[Int] = Success(100)
 *
 * scala&gt; val round100 = try100.validating(isRound)
 * round100: scala.util.Try[Int] = Success(100)
 * </pre>
 *
 * <p>
 * If validation fails, the successful <code>Try</code> will be transformed into a failed one, with
 * a <code>ValidationFailedException</code> that contains the error message
 * returned by the validation function. In the following example, 42 fails the validation because it
 * is not evenly divisible by 10:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val try42 = Try(42)
 * try42: scala.util.Try[Int] = Success(42)
 *
 * scala&gt; val round42 = try42.validating(isRound)
 * round42: scala.util.Try[Int] = Failure(org.scalactic.exceptions.ValidationFailedException: 42 was not a round number)
 * </pre>
 *
 * <p>
 * If <code>validating</code> is called on a failed <code>Try</code>, it just returns the same failed <code>Try</code>:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val tryEx = Try[Int] { throw new Exception("oops!") }
 * tryEx: scala.util.Try[Int] = Failure(java.lang.Exception: oops!)
 *
 * scala&gt; val roundEx = tryEx.validating(isRound)
 * roundEx: scala.util.Try[Int] = Failure(java.lang.Exception: oops!)
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
 * scala&gt; try100.validating(isRound, isDivBy3, isAnswerToLifeTheUniverseAndEverything)
 * res0: scala.util.Try[Int] = Failure(org.scalactic.exceptions.ValidationFailedException: 100 was not divisible by 3)
 * </pre>
 *
 * <p>
 * Here are some examples of the <code>toOr</code> method:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; try100.toOr
 * res1: org.scalactic.Or[Int,Throwable] = Good(100)
 *
 * scala&gt; tryEx.toOr
 * res2: org.scalactic.Or[Int,Throwable] = Bad(java.lang.Exception: oops!)
 * </pre>
 */
trait TrySugar {

  /**
   * Implicit class that adds a <code>toOr</code> method to
   * <code>Try</code>, which converts <code>Success</code> to <code>Good</code>,
   * and <code>Failure</code> to <code>Bad</code>, as well as a
   * <code>validation</code> method, which takes one or more functions that validate
   * the <code>Future</code>'s value.
   *
   * <p>
   * See the main documentation for trait [[org.scalactic.TrySugar `TrySugar`]] for more detail and examples.
   * </p>
   *
   * @param theTry the <code>Try</code> to which to add <code>toOr</code> and <code>validating</code> methods.
   */
  implicit class Tryizer[T](theTry: Try[T]) {

    /**
     * Converts a <code>Try</code> to an <code>Or</code>, with
     * <code>Success</code> becoming <code>Good</code> and
     * <code>Failure</code> becoming <code>Bad</code>.
     */
    def toOr: T Or Throwable = Or.from(theTry)

    /**
     * Validates a <code>Try</code> using the passed validation functions.
     *
     * <p>
     * See the main documentation for trait [[org.scalactic.TrySugar `TrySugar`]] for more detail and examples.
     * </p>
     *
     * @param first the first validation function to apply
     * @param rest the subsequent validation functions to apply, if any
     * @return a "validated" <code>Try</code>, either a <code>Try</code> with the same value, or
     *   if validation failed, a failed <code>Try</code> containing a <code>ValidationFailedException</code>.
     */
    def validating(hd: T => Validation[ErrorMessage], tl: (T => Validation[ErrorMessage])*): Try[T] = {
      theTry.flatMap { (o: T) =>
        TrySugar.passOrFirstFail(o, hd :: tl.toList) match {
          case Pass => theTry
          case Fail(errorMessage) => Failure(ValidationFailedException(errorMessage))
        }
      }
    }
  }
} 

/**
 * Companion object for <code>TrySugar</code> enabling its members to be
 * imported as an alternative to mixing them in.
 */
object TrySugar extends TrySugar {
  @tailrec
  private[scalactic] def passOrFirstFail[T, E](o: T, fs: List[T => Validation[E]]): Validation[E] = {
    fs match {
      case Nil => Pass
      case head :: tail => 
        head(o) match {
          case Pass => passOrFirstFail(o, tail)
          case firstFail => firstFail
        }
    }
  }
}
/*
import org.scalactic._

import TrySugar._

import scala.util.Try

def isRound(i: Int): Validation[ErrorMessage] =
  if (i % 10 == 0) Pass else Fail(i + " was not a round number")

val try100 = Try(100)

val round100 = try100.validating(isRound)

val try42 = Try(42)

val round42 = try42.validating(isRound)

val tryEx = Try[Int] { throw new Exception("oops!") }

val roundEx = tryEx.validating(isRound)

def isDivBy3(i: Int): Validation[ErrorMessage] =
  if (i % 3 == 0) Pass else Fail(i + " was not divisible by 3")

def isAnswerToLifeTheUniverseAndEverything(i: Int): Validation[ErrorMessage] =
  if (i == 42) Pass else Fail(i + "did not equal 42")

try100.validating(isRound, isDivBy3, isAnswerToLifeTheUniverseAndEverything)

*/

