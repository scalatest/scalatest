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
package org.scalatest.expectations

import org.scalactic.{Resources => _, _}
import org.scalatest.{Assertion, CompileMacro, Expectation, Fact, Suite, Resources}
import Fact._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.compiletime.testing.{typeChecks, typeCheckErrors}

/**
 * A representation of an expectation for an assertion.
 * Expectations provide a way to express assertions that return a [[Fact]] object instead of throwing exceptions
 * when the assertion fails.
 */
trait Expectations {

  /**
   * Asserts that `actual` is equal to `expected` using default equality.
   *
   * @param expected the expected value
   * @param actual the actual value
   * @param prettifier the prettifier used to pretty-print the values
   * @return a [[Fact]] representing the result of the assertion
   */
  def expectResult(expected: Any)(actual: Any)(implicit prettifier: Prettifier): Fact = {
    if (!DefaultEquality.areEqualComparingArraysStructurally(actual, expected)) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val rawFactMessage = Resources.rawExpectedButGot
      val rawSimplifiedFactMessage = Resources.rawDidNotEqual
      val rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedButGot
      val rawMidSentenceSimplifiedFactMessage = Resources.rawDidNotEqual
      No(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act)
      )(prettifier)
    }
    else {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val rawFactMessage = Resources.rawExpectedAndGot
      val rawSimplifiedFactMessage = Resources.rawEqualed
      val rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedAndGot
      val rawMidSentenceSimplifiedFactMessage = Resources.rawEqualed
      Yes(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act)
      )(prettifier)
    }
  }

  /**
   * Asserts that a block of code throws an exception of type `T`.
   *
   * @param f the block of code to be executed
   * @param classTag the class tag representing the exception type `T`
   * @param prettifier the prettifier used to pretty-print the values
   * @return an [[Expectation]] representing the result of the assertion
   */
  def expectThrows[T <: AnyRef](f: => Any)(implicit classTag: ClassTag[T], prettifier: Prettifier): Expectation = {
    val clazz = classTag.runtimeClass
    try {
      f
      No(
        rawFactMessage = Resources.rawExceptionExpected,
        rawSimplifiedFactMessage = Resources.rawFactNoExceptionWasThrown,
        rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedExceptionWasThrown,
        rawMidSentenceSimplifiedFactMessage = Resources.rawMidSentenceFactNoExceptionWasThrown,
        factMessageArgs = Vector(clazz.getName),
        simplifiedFactMessageArgs = Vector.empty,
        midSentenceFactMessageArgs = Vector(clazz.getName),
        midSentenceSimplifiedFactMessageArgs = Vector.empty
      )(prettifier)
    }
    catch {
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass))
          No(
            rawFactMessage = Resources.rawWrongException,
            rawSimplifiedFactMessage = Resources.rawFactExceptionWasThrown,
            rawMidSentenceFactMessage = Resources.rawMidSentenceWrongException,
            rawMidSentenceSimplifiedFactMessage = Resources.rawMidSentenceFactExceptionWasThrown,
            factMessageArgs = Vector(clazz.getName, u.getClass.getName),
            simplifiedFactMessageArgs = Vector(u.getClass.getName),
            midSentenceFactMessageArgs = Vector(clazz.getName, u.getClass.getName),
            midSentenceSimplifiedFactMessageArgs = Vector(u.getClass.getName),
            cause = Some(u)
          )(prettifier)
        else
          Yes(
            rawFactMessage = Resources.rawExceptionExpected,
            rawSimplifiedFactMessage = Resources.rawFactExceptionWasThrown,
            rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedExceptionWasThrown,
            rawMidSentenceSimplifiedFactMessage = Resources.rawMidSentenceFactExceptionWasThrown,
            factMessageArgs = Vector(clazz.getName),
            simplifiedFactMessageArgs = Vector(clazz.getName),
            midSentenceFactMessageArgs = Vector(clazz.getName),
            midSentenceSimplifiedFactMessageArgs = Vector(clazz.getName),
            cause = Some(u)
          )(prettifier)
      }
    }
  }

  /**
   * Asserts that a boolean expression is `true`.
   *
   * @param expression the boolean expression to be evaluated
   * @param prettifier the prettifier used to pretty-print the values
   * @return a [[Fact]] representing the result of the assertion
   */
  inline def expect(expression: Boolean)(implicit prettifier: Prettifier): Fact =
    ${ ExpectationsMacro.expect('{expression})('{prettifier}) }

  /**
   * Expects that a given code snippet does not compile.
   *
   * @param code the code snippet to be compiled
   * @param prettifier the prettifier used to pretty-print the values
   * @return a [[Fact]] representing the result of the assertion
   */
  transparent inline def expectDoesNotCompile(inline code: String)(implicit prettifier: Prettifier): Fact =
    ${ CompileMacro.expectDoesNotCompileImpl('code, '{typeChecks(code)}, 'prettifier) }

  /**
   * Expects that a given code snippet compiles successfully.
   *
   * @param code the code snippet to be compiled
   * @param prettifier the prettifier used to pretty-print the values
   * @return a [[Fact]] representing the result of the assertion
   */
  transparent inline def expectCompiles(inline code: String)(implicit prettifier: Prettifier): Fact =
    ${ CompileMacro.expectCompilesImpl('code, '{typeCheckErrors(code)}, 'prettifier) }

  /**
   * Expects that a given code snippet results in a type error during compilation.
   *
   * @param code the code snippet to be compiled
   * @param prettifier the prettifier used to pretty-print the values
   * @return a [[Fact]] representing the result of the assertion
   */
  transparent inline def expectTypeError(inline code: String)(implicit prettifier: Prettifier): Fact =
    ${ CompileMacro.expectTypeErrorImpl('code, '{typeCheckErrors(code)}, 'prettifier) }

  import scala.language.implicitConversions

  /**
   * Implicitly converts a boolean expression to a [[Fact]] for assertion purposes, which makes (x &gt; 0) implies expect(x &gt; -1) syntax works
   *
   * @param expression the boolean expression to be evaluated
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  implicit inline def booleanToFact(expression: Boolean)(implicit prettifier: Prettifier): Fact =
    ${ ExpectationsMacro.expect('expression)('prettifier) }

  /**
   * Implicitly converts an [[Expectation]] to an [[Assertion]].
   *
   * @param exp the expectation to be converted
   * @return an [[Assertion]] representing the result of the expectation
   */
  implicit def convertExpectationToAssertion(exp: Expectation): Assertion = exp.toAssertion
}

/**
 * The companion object for the `Expectation` trait.
 */
object Expectations extends Expectations {

  /**
   * A helper used by macro-generated code.
   */
  class ExpectationsHelper {

    /**
     * A helper method for macro-generated assertions.
     *
     * @param bool the [[Bool]] object representing the assertion result
     * @param clue the clue to be used in case of failure
     * @param prettifier the prettifier used to pretty-print the values
     * @return a [[Fact]] representing the result of the assertion
     */
    def macroExpect(bool: Bool, clue: Any, prettifier: Prettifier): Fact = {
      //requireNonNull(clue)
      if (!bool.value)
        No(
          bool.rawFailureMessage,
          bool.rawFailureMessage,
          bool.rawFailureMessage,
          bool.rawFailureMessage,
          bool.failureMessageArgs,
          bool.failureMessageArgs,
          bool.failureMessageArgs,
          bool.failureMessageArgs
        )(prettifier)
      else
        Yes(
          bool.rawNegatedFailureMessage,
          bool.rawNegatedFailureMessage,
          bool.rawNegatedFailureMessage,
          bool.rawNegatedFailureMessage,
          bool.negatedFailureMessageArgs,
          bool.negatedFailureMessageArgs,
          bool.negatedFailureMessageArgs,
          bool.negatedFailureMessageArgs
        )(prettifier)
    }

  }

  val expectationsHelper = new ExpectationsHelper
}
