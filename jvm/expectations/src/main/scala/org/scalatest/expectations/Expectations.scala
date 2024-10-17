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
package org.scalatest
package expectations

import org.scalactic.{Resources => _, _}
import Fact._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.reflect.ClassTag

import org.scalactic.Requirements.requireNonNull

/**
 * A representation of an expectation for an assertion.
 * Expectations provide a way to express assertions that return a [[Fact]] object instead of throwing exceptions
 * when the assertion fails.
 */
trait Expectations {
  
  /**
   * Expects that `actual` is equal to `expected` using default equality.
   *
   * @param expected the expected value
   * @param actual the actual value
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expectResult(expected: Any)(actual: Any)(implicit prettifier: Prettifier, pos: source.Position): Fact = {
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
        Vector(exp, act), 
        prettifier
      )
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
        Vector(exp, act), 
        prettifier
      )
    }
  }

  /**
   * Expects that `actual` is equal to `expected` using default equality.
   *
   * @param expected the expected value
   * @param clue an object whose <code>toString</code> method returns a message to be appended to the result [[Fact]]'s message
   * @param actual the actual value
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expectResult(expected: Any, clue: Any)(actual: Any)(implicit prettifier: Prettifier, pos: source.Position): Fact = {
    requireNonNull(clue)
    if (!DefaultEquality.areEqualComparingArraysStructurally(actual, expected)) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val rawFactMessage = Resources.rawExpectedButGot
      val rawSimplifiedFactMessage = Resources.rawDidNotEqual
      val rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedButGot
      val rawMidSentenceSimplifiedFactMessage = Resources.rawDidNotEqual
      No(
        AppendedClues.appendClue(rawFactMessage, clue.toString()),
        AppendedClues.appendClue(rawSimplifiedFactMessage, clue.toString()),
        AppendedClues.appendClue(rawMidSentenceFactMessage, clue.toString()),
        AppendedClues.appendClue(rawMidSentenceSimplifiedFactMessage, clue.toString()),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act), 
        prettifier
      )
    }
    else {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val rawFactMessage = Resources.rawExpectedAndGot
      val rawSimplifiedFactMessage = Resources.rawEqualed
      val rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedAndGot
      val rawMidSentenceSimplifiedFactMessage = Resources.rawEqualed
      Yes(
        AppendedClues.appendClue(rawFactMessage, clue.toString()),
        AppendedClues.appendClue(rawSimplifiedFactMessage, clue.toString()),
        AppendedClues.appendClue(rawMidSentenceFactMessage, clue.toString()),
        AppendedClues.appendClue(rawMidSentenceSimplifiedFactMessage, clue.toString()),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act), 
        prettifier
      )
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
        midSentenceSimplifiedFactMessageArgs = Vector.empty, 
        prettifier
      )
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
            cause = Some(u), 
            prettifier
          )
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
            cause = Some(u), 
            prettifier
          )
      }
    }
  }

  import language.experimental.macros

  /**
   * Expects that a boolean expression is `true`.
   *
   * @param expression the boolean expression to be evaluated
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expect(expression: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro ExpectationsMacro.expect

  /**
   * Expects that a boolean expression is `true`, message included in the returned [[Fact]] will be appended with <code>clue</code>'s <code>toString</code>.
   *
   * @param expression the boolean expression to be evaluated
   * @param clue An object whose <code>toString</code> method returns a message to be appended to the result [[Fact]]'s message
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expect(expression: Boolean, clue: Any)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro ExpectationsMacro.expectWithClue

  /**
   * Expects that a given code snippet does not compile.
   *
   * @param code the code snippet to be compiled
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expectDoesNotCompile(code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro CompileMacro.expectDoesNotCompileImpl

  /**
   * Expects that a given code snippet compiles successfully.
   *
   * @param code the code snippet to be compiled
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expectCompiles(code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro CompileMacro.expectCompilesImpl

  /**
   * Expects that a given code snippet results in a type error during compilation.
   *
   * @param code the code snippet to be compiled
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expectTypeError(code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro CompileMacro.expectTypeErrorImpl

  import scala.language.implicitConversions

  /**
   * Implicitly converts a boolean expression to a [[Fact]] for assertion purposes, which makes (x &gt; 0) implies expect(x &gt; -1) syntax works
   *
   * @param expression the boolean expression to be evaluated
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */  
  implicit def booleanToFact(expression: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro ExpectationsMacro.expect
  
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

    import org.scalactic.Requirements.requireNonNull

    /**
     * A helper method for macro-generated assertions.
     *
     * @param bool the [[Bool]] object representing the assertion result
     * @param clue the clue to be used in case of failure
     * @param prettifier the prettifier used to pretty-print the values
     * @param pos the source position
     * @return a [[Fact]] representing the result of the assertion
     */
    def macroExpect(bool: Bool, clue: Any, prettifier: Prettifier, pos: source.Position): Fact = {
      requireNonNull(clue)
      val result = 
        if (!bool.value)
          No(
            bool.rawFailureMessage,
            bool.rawFailureMessage,
            bool.rawFailureMessage,
            bool.rawFailureMessage,
            bool.failureMessageArgs,
            bool.failureMessageArgs,
            bool.failureMessageArgs,
            bool.failureMessageArgs, 
            prettifier
          )
        else
          Yes(
            bool.rawNegatedFailureMessage,
            bool.rawNegatedFailureMessage,
            bool.rawNegatedFailureMessage,
            bool.rawNegatedFailureMessage,
            bool.negatedFailureMessageArgs,
            bool.negatedFailureMessageArgs,
            bool.negatedFailureMessageArgs,
            bool.negatedFailureMessageArgs, 
            prettifier
          )
      if (clue == "") result else result.modifyMessage(_.map(ori => AppendedClues.appendClue(ori, clue.toString)))    
    }

  }

  val expectationsHelper = new ExpectationsHelper
}
