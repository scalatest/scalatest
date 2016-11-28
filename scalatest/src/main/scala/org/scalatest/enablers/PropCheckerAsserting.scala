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

}

/**
  * Class holding lowest priority <code>CheckerAsserting</code> implicit, which enables [[org.scalatest.prop.GeneratorDrivenPropertyChecks GeneratorDrivenPropertyChecks]] expressions that have result type <code>Unit</code>.
  */
abstract class UnitPropCheckerAsserting {

  import PropCheckerAsserting._

  abstract class PropCheckerAssertingImpl[T] extends PropCheckerAsserting[T] {

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

    private[scalatest] def indicateSuccess(message: => String): Result

    private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Result

  }

}

abstract class ExpectationPropCheckerAsserting extends UnitPropCheckerAsserting {

  implicit def assertingNatureOfExpectation(implicit prettifier: Prettifier): PropCheckerAsserting[Expectation] { type Result = Expectation } = {
    new PropCheckerAssertingImpl[Expectation] {
      type Result = Expectation
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