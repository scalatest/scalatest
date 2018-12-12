/*
 * Copyright 2001-2013 Artima, Inc.
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

import org.scalactic._
import NameUtil.getSimpleNameOfAnObjectsClass
import org.scalacheck.Prop
import org.scalacheck.Prop.Arg
import org.scalacheck.Test
import org.scalacheck.util.Pretty
import org.scalatest.Assertion
import org.scalatest.Expectation
import org.scalatest.Fact
import org.scalatest.FailureMessages
import org.scalatest.Resources
import org.scalatest.Succeeded
import org.scalatest.UnquotedString
import org.scalatest.exceptions.StackDepthException
import FailureMessages.decorateToStringValue
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException
import org.scalatest.exceptions.StackDepth

/**
 * Supertrait for <code>CheckerAsserting</code> typeclasses, which are used to implement and determine the result
 * type of [[org.scalatest.prop.GeneratorDrivenPropertyChecks GeneratorDrivenPropertyChecks]]'s <code>apply</code> and <code>forAll</code> method.
 *
 * <p>
 * Currently, an [[org.scalatest.prop.GeneratorDrivenPropertyChecks GeneratorDrivenPropertyChecks]] expression will have result type <code>Assertion</code>, if the function passed has result type <code>Assertion</code>,
 * else it will have result type <code>Unit</code>.
 * </p>
 */
trait CheckerAsserting[T] {
  /**
   * The result type of the <code>check</code> method.
   */
  type Result

  def succeed(result: T): (Boolean, Option[Throwable])

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
  def check(p: Prop, prms: Test.Parameters, prettifier: Prettifier, pos: source.Position, argNames: Option[List[String]] = None): Result
}

/**
  * Class holding lowest priority <code>CheckerAsserting</code> implicit, which enables [[org.scalatest.prop.GeneratorDrivenPropertyChecks GeneratorDrivenPropertyChecks]] expressions that have result type <code>Unit</code>.
  */
abstract class UnitCheckerAsserting {

  /**
   * Abstract subclass of <code>CheckerAsserting</code> that provides the bulk of the implementations of <code>CheckerAsserting</code>
   * <code>check</code> method.
   */
  /* protected[scalatest]*/ abstract class CheckerAssertingImpl[T] extends CheckerAsserting[T] {

    import CheckerAsserting._

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
    def check(p: Prop, prms: Test.Parameters, prettifier: Prettifier, pos: source.Position, argNames: Option[List[String]] = None): Result = {

      val result = Test.check(prms, p)
      if (!result.passed) {

        val (args, labels) = argsAndLabels(result)

        (result.status: @unchecked) match {

          case Test.Exhausted =>

            val failureMsg =
              if (result.succeeded == 1)
                FailureMessages.propCheckExhaustedAfterOne(prettifier, result.discarded)
              else
                FailureMessages.propCheckExhausted(prettifier, result.succeeded, result.discarded)

            indicateFailure(
              sde => failureMsg,
              failureMsg,
              args,
              labels,
              None,
              pos
            )

          case Test.Failed(scalaCheckArgs, scalaCheckLabels) =>

            val stackDepth = 1
            
            indicateFailure(
              sde => FailureMessages.propertyException(prettifier, UnquotedString(sde.getClass.getSimpleName)) + "\n" +
              ( sde.failedCodeFileNameAndLineNumberString match { case Some(s) => " (" + s + ")"; case None => "" }) + "\n" +
              "  " + FailureMessages.propertyFailed(prettifier, result.succeeded) + "\n" +
              (
                sde match {
                  case sd: StackDepth if sd.failedCodeFileNameAndLineNumberString.isDefined =>
                    "  " + FailureMessages.thrownExceptionsLocation(prettifier, UnquotedString(sd.failedCodeFileNameAndLineNumberString.get)) + "\n"
                  case _ => ""
                }
                ) +
              "  " + FailureMessages.occurredOnValues + "\n" +
              prettyArgs(getArgsWithSpecifiedNames(argNames, scalaCheckArgs), prettifier) + "\n" +
              "  )" +
              getLabelDisplay(scalaCheckLabels),
              FailureMessages.propertyFailed(prettifier, result.succeeded),
              scalaCheckArgs,
              scalaCheckLabels.toList,
              None,
              pos
            )

          case Test.PropException(scalaCheckArgs, e, scalaCheckLabels) =>

            indicateFailure(
              sde => FailureMessages.propertyException(prettifier, UnquotedString(e.getClass.getSimpleName)) + "\n" +
              "  " + FailureMessages.thrownExceptionsMessage(prettifier, if (e.getMessage == null) "None" else UnquotedString(e.getMessage)) + "\n" +
              (
                e match {
                  case sd: StackDepth if sd.failedCodeFileNameAndLineNumberString.isDefined =>
                    "  " + FailureMessages.thrownExceptionsLocation(prettifier, UnquotedString(sd.failedCodeFileNameAndLineNumberString.get)) + "\n"
                  case _ => ""
                }
              ) +
              "  " + FailureMessages.occurredOnValues + "\n" +
              prettyArgs(getArgsWithSpecifiedNames(argNames, scalaCheckArgs), prettifier) + "\n" +
              "  )" +
              getLabelDisplay(scalaCheckLabels),
              FailureMessages.propertyException(prettifier, UnquotedString(e.getClass.getName)),
              scalaCheckArgs,
              scalaCheckLabels.toList,
              Some(e),
              pos
            )
        }
      } else indicateSuccess(FailureMessages.propertyCheckSucceeded)
    }

    private[scalatest] def indicateSuccess(message: => String): Result

    private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Result
  }

  /**
   * Provides support of [[org.scalatest.enablers.CheckerAsserting CheckerAsserting]] for Unit.  Do nothing when the check succeeds,
   * but throw [[org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException GeneratorDrivenPropertyCheckFailedException]]
   * when check fails.
   */
  implicit def assertingNatureOfT[T]: CheckerAsserting[T] { type Result = Unit } =
    new CheckerAssertingImpl[T] {
      type Result = Unit
      def succeed(result: T) = (true, None)
      private[scalatest] def indicateSuccess(message: => String): Unit = ()
      private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Unit = {
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

/**
 * Abstract class that in the future will hold an intermediate priority <code>CheckerAsserting</code> implicit, which will enable inspector expressions
 * that have result type <code>Expectation</code>, a more composable form of assertion that returns a result instead of throwing an exception when it fails.
 */
abstract class ExpectationCheckerAsserting extends UnitCheckerAsserting {

  implicit def assertingNatureOfExpectation(implicit prettifier: Prettifier): CheckerAsserting[Expectation] { type Result = Expectation } = {
    new CheckerAssertingImpl[Expectation] {
      type Result = Expectation
      def succeed(result: Expectation) = (result.isYes, result.cause)
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

/**
 * Companion object to <code>CheckerAsserting</code> that provides two implicit providers, a higher priority one for passed functions that have result
 * type <code>Assertion</code>, which also yields result type <code>Assertion</code>, and one for any other type, which yields result type <code>Unit</code>.
 */
object CheckerAsserting extends ExpectationCheckerAsserting {

  /**
    * Provides support of [[org.scalatest.enablers.CheckerAsserting CheckerAsserting]] for Assertion.  Returns [[org.scalatest.Succeeded Succeeded]] when the check succeeds,
    * but throw [[org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException GeneratorDrivenPropertyCheckFailedException]]
    * when check fails.
    */
  implicit def assertingNatureOfAssertion: CheckerAsserting[Assertion] { type Result = Assertion } = {
    new CheckerAssertingImpl[Assertion] {
      type Result = Assertion
      def succeed(result: Assertion) = (true, None)
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

  private[enablers] def getArgsWithSpecifiedNames(argNames: Option[List[String]], scalaCheckArgs: List[Arg[Any]]) = {
    if (argNames.isDefined) {
      // length of scalaCheckArgs should equal length of argNames
      val zipped = argNames.get zip scalaCheckArgs
      zipped map { case (argName, arg) => arg.copy(label = argName) }
    }
    else
      scalaCheckArgs
  }

  private[enablers] def getLabelDisplay(labels: Set[String]): String =
    if (labels.size > 0)
      "\n  " + (if (labels.size == 1) Resources.propCheckLabel else Resources.propCheckLabels) + "\n" + labels.map("    " + _).mkString("\n")
    else
      ""

  private[enablers] def argsAndLabels(result: Test.Result): (List[Any], List[String]) = {

    val (scalaCheckArgs, scalaCheckLabels) =
      result.status match {
        case Test.Proved(args) => (args.toList, List())
        case Test.Failed(args, labels) => (args.toList, labels.toList)
        case Test.PropException(args, _, labels) => (args.toList, labels.toList)
        case _ => (List(), List())
      }

    val args: List[Any] = for (scalaCheckArg <- scalaCheckArgs.toList) yield scalaCheckArg.arg

    // scalaCheckLabels is a Set[String], I think
    val labels: List[String] = for (scalaCheckLabel <- scalaCheckLabels.iterator.toList) yield scalaCheckLabel

    (args, labels)
  }

  // TODO: Internationalize these, and make them consistent with FailureMessages stuff (only strings get quotes around them, etc.)
  private[enablers] def prettyTestStats(result: Test.Result, prettifier: Prettifier) = result.status match {

    case Test.Proved(args) =>
      "OK, proved property:                   \n" + prettyArgs(args, prettifier)

    case Test.Passed =>
      "OK, passed " + result.succeeded + " tests."

    case Test.Failed(args, labels) =>
      "Falsified after " + result.succeeded + " passed tests:\n" + prettyLabels(labels) + prettyArgs(args, prettifier)

    case Test.Exhausted =>
      "Gave up after only " + result.succeeded + " passed tests. " +
        result.discarded + " tests were discarded."

    case Test.PropException(args, e, labels) =>
      FailureMessages.propertyException(prettifier, UnquotedString(e.getClass.getSimpleName)) + "\n" + prettyLabels(labels) + prettyArgs(args, prettifier)
  }

  private[enablers] def prettyLabels(labels: Set[String]) = {
    if (labels.isEmpty) ""
    else if (labels.size == 1) "Label of failing property: " + labels.iterator.next + "\n"
    else "Labels of failing property: " + labels.mkString("\n") + "\n"
  }

  //
  // If scalacheck arg contains a type that
  // decorateToStringValue processes, then let
  // decorateToStringValue handle it.  Otherwise use its
  // prettyArg method to generate the display string.
  //
  // Passes 0 as verbosity value to prettyArg function.
  //
  private[enablers] def decorateArgToStringValue(arg: Arg[_], prettifier: Prettifier): String =
    arg.arg match {
      case null         => decorateToStringValue(prettifier, arg.arg)
      case _: Unit      => decorateToStringValue(prettifier, arg.arg)
      case _: String    => decorateToStringValue(prettifier, arg.arg)
      case _: Char      => decorateToStringValue(prettifier, arg.arg)
      case _: Array[_]  => decorateToStringValue(prettifier, arg.arg)
      case _            => arg.prettyArg(new Pretty.Params(0))
    }

  private[enablers] def prettyArgs(args: List[Arg[_]], prettifier: Prettifier) = {
    val strs = for((a, i) <- args.zipWithIndex) yield (
      "    " +
        (if (a.label == "") "arg" + i else a.label) +
        " = " + decorateArgToStringValue(a, prettifier) + (if (i < args.length - 1) "," else "") +
        (if (a.shrinks > 0) " // " + a.shrinks + (if (a.shrinks == 1) " shrink" else " shrinks") else "")
      )
    strs.mkString("\n")
  }
}

