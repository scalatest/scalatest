/*
 * Copyright 2001-2026 Artima, Inc.
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
package org.scalactic.opaquetypes

import org.scalactic.Resources
import scala.quoted.*
import scala.util.{ Try, Success, Failure }
import java.util.regex.Pattern
import org.scalactic.{ Validation, Pass, Fail }
import org.scalactic.{ Or, Good, Bad }

/** Factory object for the [[Regexes.RegexString]] opaque type, which restricts
  * `String` values to those that are well-formed regular expressions.
  */
object Regexes {

  /** Opaque type representing a `String` that is a valid regular expression.
    *
    * Instances of this type are guaranteed to be compilable by
    * `java.util.regex.Pattern`. Use the compile-time
    * [[Regexes.RegexString.apply]] method to construct instances from String
    * literals, or the runtime factory methods for values known only at runtime.
    */
  private[scalactic] opaque type RegexString = String

  /** Companion object for [[Regexes.RegexString]] with construction and
    * validation helpers.
    *
    * Provides a compile-time-checked factory for String literals, runtime
    * validation helpers, and extension methods for common regular expression
    * operations.
    */
  private[scalactic] object RegexString {

    /** Compile-time factory for creating a [[Regexes.RegexString]] from a String literal.
      *
      * Rejects string literals that are not well-formed regular expressions at
      * compile time, reporting the underlying pattern syntax error when one is
      * available.
      */
    inline def apply(inline s: String): RegexString = ${ RegexString.applyImpl('s) }

    private def applyImpl(s: Expr[String])(using Quotes): Expr[RegexString] = {
      import quotes.reflect.*
      s.value match {
        case Some(v) =>
          if (Regexes.isValidPattern(v))
            s.asExprOf[RegexString]
          else
            report.errorAndAbort(
              "RegexString.apply can only be invoked on String literals that " +
              "represent valid regular expressions." + Regexes.invalidPatternMessage(v)
            )
        case None =>
          report.errorAndAbort(
            "RegexString.apply can only be invoked on String literals that " +
            "represent valid regular expressions. Please use RegexString.from " +
            "instead."
          )
      }
    }

    /** Construct a [[Regexes.RegexString]] from a runtime String if it is a valid regex.
      *
      * @param s the String to validate
      * @return Some(RegexString) if s is a well-formed regular expression, else None
      */
    def from(s: String): Option[RegexString] =
      if (Regexes.isValidPattern(s)) Some(s.asInstanceOf[RegexString]) else None

    /** Validate and return the given String as [[Regexes.RegexString]].
      *
      * @throws AssertionError if s is not a well-formed regular expression
      */
    def ensuringValid(s: String): RegexString =
      if (Regexes.isValidPattern(s))
        s.asInstanceOf[RegexString]
      else
        throw new AssertionError(Resources.invalidRegexString)

    /** Runtime factory that returns Success for valid input, Failure otherwise.
      *
      * @param value the String to validate
      * @return Success(RegexString) if value is a well-formed regular expression,
      *   else Failure(AssertionError)
      */
    def tryingValid(value: String): Try[RegexString] =
      if (Regexes.isValidPattern(value))
        Success(value.asInstanceOf[RegexString])
      else
        Failure(new AssertionError(Resources.invalidRegexString))

    /** Predicate indicating whether the given String is valid for [[Regexes.RegexString]].
      *
      * @param value the String to validate
      * @return true if value is a well-formed regular expression, else false
      */
    def isValid(value: String): Boolean = Regexes.isValidPattern(value)

    /** Validate a value and return Pass, else Fail(f(value)).
      *
      * @param value the String to validate
      * @param f function to produce an error value when validation fails
      * @return Pass if value is a valid RegexString, else Fail(f(value))
      */
    def passOrElse[E](value: String)(f: String => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** Validate a value and return Good(RegexString), else Bad(f(value)).
      *
      * @param value the String to validate
      * @param f function to produce an error value when validation fails
      * @return Good(RegexString) if value is valid, else Bad(f(value))
      */
    def goodOrElse[B](value: String)(f: String => B): RegexString Or B =
      if (isValid(value)) Good(value.asInstanceOf[RegexString]) else Bad(f(value))

    /** Validate a value and return Right(RegexString), else Left(f(value)).
      *
      * @param value the String to validate
      * @param f function to produce an error value when validation fails
      * @return Right(RegexString) if value is valid, else Left(f(value))
      */
    def rightOrElse[L](value: String)(f: String => L): Either[L, RegexString] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** Return a validated value or the provided default if invalid.
      *
      * @param value the String to validate
      * @param default the RegexString to return if value is not valid
      * @return value as RegexString if valid, else default
      */
    def fromOrElse(value: String, default: => RegexString): RegexString =
      if (isValid(value)) value.asInstanceOf[RegexString] else default

    /** Convert [[Regexes.RegexString]] to [[String]] for interoperability. */
    given Conversion[RegexString, String] with {
      def apply(x: RegexString): String = x
    }

    /** Ordering instance based on underlying String ordering. */
    given Ordering[RegexString] with {
      def compare(x: RegexString, y: RegexString): Int = x.compareTo(y)
    }
  }

  extension (x: RegexString) {

    /** Return the underlying String value. */
    def value: String = x

    /** Length of the underlying String. */
    def length: Int = x.length

    /** True if the underlying String is empty. */
    def isEmpty: Boolean = x.isEmpty

    /** True if the underlying String entirely matches the given regular expression. */
    def matches(regex: String): Boolean = x.matches(regex)

    /** True if the underlying String contains a match for the given regular
      * expression anywhere within it.
      */
    def find(regex: String): Boolean =
      Pattern.compile(regex).matcher(x).find()

    /** Replace all matches of the given regular expression with the replacement. */
    def replaceAll(regex: String, replacement: String): String = x.replaceAll(regex, replacement)

    /** Replace the first match of the given regular expression with the replacement. */
    def replaceFirst(regex: String, replacement: String): String = x.replaceFirst(regex, replacement)

    /** Split the underlying String around matches of the given regular expression. */
    def split(regex: String): Array[String] = x.split(regex)

    /** Split the underlying String around matches of the given regular expression,
      * limiting the number of results.
      */
    def split(regex: String, limit: Int): Array[String] = x.split(regex, limit)

    /** Apply the given function to the underlying String and return the result as a
      * [[Regexes.RegexString]] if it is still a well-formed regular expression.
      *
      * @throws AssertionError if the result of applying f to the underlying String is
      *   not a well-formed regular expression
      */
    def ensuringValid(f: String => String): RegexString = {
      val candidateResult: String = f(x)
      if (Regexes.isValidPattern(candidateResult)) candidateResult.asInstanceOf[RegexString]
      else throw new AssertionError(Resources.invalidRegexString)
    }
  }

  /** Determine whether the given String is a well-formed regular expression. */
  private def isValidPattern(s: String): Boolean =
    try {
      Pattern.compile(s)
      true
    }
    catch {
      case e: Exception =>
        false
    }

  /** Produce the compiler-facing message describing why the given String is not a
    * well-formed regular expression, for use in compile-time errors.
    */
  private def invalidPatternMessage(s: String): String =
    try {
      Pattern.compile(s)
      ""
    }
    catch {
      case e: Exception =>
        "\n" + e.getMessage
    }
}