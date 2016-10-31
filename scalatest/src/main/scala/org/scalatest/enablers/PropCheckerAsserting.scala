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
import org.scalatest.exceptions.{StackDepth, StackDepthException}
import org.scalatest.prop.{Configuration, PropertyArgument, PropertyFun}
import org.scalatest.{FailureMessages, Resources, UnquotedString}
import FailureMessages.decorateToStringValue

trait PropCheckerAsserting[T] {

  /**
    * The result type of the <code>check</code> method.
    */
  type Result

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
  def check(p: PropertyFun, prms: Configuration.Parameter, prettifier: Prettifier, pos: source.Position, argNames: Option[List[String]] = None): Result

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
    def check(p: PropertyFun, prms: Configuration.Parameter, prettifier: Prettifier, pos: source.Position, argNames: Option[List[String]] = None): Result = {
      val result = p.check(argNames.getOrElse(List.empty), prms)
      val (args, labels) = argsAndLabels(result)
      result match {
        case PropertyFun.CheckExhausted(succeeded, discarded) =>
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

        case PropertyFun.CheckFailure(succeeded, ex, names, argsPassed) =>
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
      /*if (!result.passed) {

        val (args, labels) = argsAndLabels(result)

        (result.status: @unchecked) match {

          case PropertyFun.CheckExhausted =>

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
      } else indicateSuccess(FailureMessages.propertyCheckSucceeded)*/
    }

    private[scalatest] def indicateSuccess(message: => String): Result

    private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Result

  }

}

object PropCheckerAsserting {

  private[enablers] def argsAndLabels(result: PropertyFun.Result): (List[Any], List[String]) = {

    val (args: List[Any], labels: List[String]) =
      result match {
        case PropertyFun.CheckSuccess(args) => (args.toList, List())
        case PropertyFun.CheckFailure(_, _, args, labels) => (args.toList, labels.toList)
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