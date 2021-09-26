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
import scala.compiletime.testing.typeChecks

private[scalatest] trait Expectations {

  // TODO: Need to make this and assertResult use custom equality I think.
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

  inline def expect(expression: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Fact =
    ${ ExpectationsMacro.expect('{expression})('{prettifier}, '{pos}) }

  inline def expectDoesNotCompile(inline code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact =
    ${ CompileMacro.expectDoesNotCompileImpl('code, '{typeChecks(code)}, 'prettifier, 'pos) }

  inline def expectCompiles(inline code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact =
    ${ CompileMacro.expectCompilesImpl('code, '{typeChecks(code)}, 'prettifier, 'pos) }

  inline def expectTypeError(inline code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact =
    ${ CompileMacro.expectTypeErrorImpl('code, '{typeChecks(code)}, 'prettifier, 'pos) }

  import scala.language.implicitConversions

  /**
    * Implicit conversion that makes (x &gt; 0) implies expect(x &gt; -1) syntax works
    */
  implicit inline def booleanToFact(expression: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Fact =
    ${ ExpectationsMacro.expect('expression)('prettifier, 'pos) }

  implicit def convertExpectationToAssertion(exp: Expectation): Assertion = exp.toAssertion
}

object Expectations extends Expectations {

  class ExpectationsHelper {

    def macroExpect(bool: Bool, clue: Any, prettifier: Prettifier, pos: source.Position): Fact = {
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
