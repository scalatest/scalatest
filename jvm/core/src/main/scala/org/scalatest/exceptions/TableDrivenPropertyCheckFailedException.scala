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
package exceptions

import org.scalactic.source

/**
 * Exception that indicates a table-driven property check failed.
 *
 * <p>
 * For an introduction to using tables, see the documentation for trait
 * <a href="../prop/TableDrivenPropertyChecks.html">TableDrivenPropertyChecks</a>.
 * </p>
 *
 * @param messageFun a function that returns a detail message, not optional) for this <code>TableDrivenPropertyCheckFailedException</code>.
 * @param cause an optional cause, the <code>Throwable</code> that caused this <code>TableDrivenPropertyCheckFailedException</code> to be thrown.
 * @param posOrStackDepthFun either a source position or a function that returns the depth in the stack trace of this exception at which the line of test code that failed resides.
 * @param payload an optional payload, which ScalaTest will include in a resulting <code>TestFailed</code> event
 * @param undecoratedMessage just a short message that has no redundancy with args, labels, etc. The regular "message" has everything in it
 * @param args the argument values
 * @param namesOfArgs a list of string names for the arguments
 * @param row the index of the table row that failed the property check, causing this exception to be thrown
 *
 * @throws NullArgumentException if any parameter is <code>null</code> or <code>Some(null)</code>.
 *
 * @author Bill Venners
 */
class TableDrivenPropertyCheckFailedException(
  messageFun: StackDepthException => String,
  cause: Option[Throwable],
  posOrStackDepthFun: Either[source.Position, StackDepthException => Int],
  payload: Option[Any],
  undecoratedMessage: String,
  args: List[Any],
  namesOfArgs: List[String],
  val row: Int
) extends PropertyCheckFailedException(
  messageFun, cause, posOrStackDepthFun, payload, undecoratedMessage, args, Some(namesOfArgs)
) {

  /**
    * Constructs a <code>TableDrivenPropertyCheckFailedException</code> with given error message function, optional cause, source position,
    * optional payload, undecorated message, argument values and names, and index of the failing row.
    *
    * @param messageFun a function that returns a detail message, not optional) for this <code>TableDrivenPropertyCheckFailedException</code>.
    * @param cause an optional cause, the <code>Throwable</code> that caused this <code>TableDrivenPropertyCheckFailedException</code> to be thrown.
    * @param pos a source position
    * @param payload an optional payload, which ScalaTest will include in a resulting <code>TestFailed</code> event
    * @param undecoratedMessage just a short message that has no redundancy with args, labels, etc. The regular "message" has everything in it
    * @param args the argument values
    * @param namesOfArgs a list of string names for the arguments
    * @param row the index of the table row that failed the property check, causing this exception to be thrown
    *
    */
  def this(
    messageFun: StackDepthException => String,
    cause: Option[Throwable],
    pos: source.Position,
    payload: Option[Any],
    undecoratedMessage: String,
    args: List[Any],
    namesOfArgs: List[String],
    row: Int
  ) = this(messageFun, cause, Left(pos), payload, undecoratedMessage, args, namesOfArgs, row)

  /**
    * Constructs a <code>TableDrivenPropertyCheckFailedException</code> with given error message function, optional cause, source position,
    * optional payload, undecorated message, argument values and names, and index of the failing row.
    *
    * @param messageFun a function that returns a detail message, not optional) for this <code>TableDrivenPropertyCheckFailedException</code>.
    * @param cause an optional cause, the <code>Throwable</code> that caused this <code>TableDrivenPropertyCheckFailedException</code> to be thrown.
    * @param failedCodeStackDepthFun a function that returns the depth in the stack trace of this exception at which the line of test code that failed resides.
    * @param payload an optional payload, which ScalaTest will include in a resulting <code>TestFailed</code> event
    * @param undecoratedMessage just a short message that has no redundancy with args, labels, etc. The regular "message" has everything in it
    * @param args the argument values
    * @param namesOfArgs a list of string names for the arguments
    * @param row the index of the table row that failed the property check, causing this exception to be thrown
    *
    */
  def this(
    messageFun: StackDepthException => String,
    cause: Option[Throwable],
    failedCodeStackDepthFun: StackDepthException => Int,
    payload: Option[Any],
    undecoratedMessage: String,
    args: List[Any],
    namesOfArgs: List[String],
    row: Int
  ) = this(messageFun, cause, Right(failedCodeStackDepthFun), payload, undecoratedMessage, args, namesOfArgs, row)

  /**
   * Returns an instance of this exception's class, identical to this exception,
   * except with the detail message option string replaced with the result of passing
   * the current detail message to the passed function, <code>fun</code>.
   *
   * @param fun A function that, given the current optional detail message, will produce
   * the modified optional detail message for the result instance of <code>TestFailedDueToTimeoutException</code>.
   */
  override def modifyMessage(fun: Option[String] => Option[String]): TableDrivenPropertyCheckFailedException = {
    val mod =
      new TableDrivenPropertyCheckFailedException(
        (sde: StackDepthException) => fun(message).getOrElse(messageFun(this)),
        cause,
        posOrStackDepthFun,
        payload,
        undecoratedMessage,
        args,
        namesOfArgs,
        row
      )
    mod.setStackTrace(getStackTrace)
    mod
  }

  /**
   * Returns an instance of this exception's class, identical to this exception,
   * except with the payload option replaced with the result of passing
   * the current payload option to the passed function, <code>fun</code>.
   *
   * @param fun A function that, given the current optional payload, will produce
   * the modified optional payload for the result instance of <code>TableDrivenPropertyCheckFailedException</code>.
   */
  override def modifyPayload(fun: Option[Any] => Option[Any]): TableDrivenPropertyCheckFailedException = {
    val currentPayload = payload
    val mod =
      new TableDrivenPropertyCheckFailedException(
        messageFun,
        cause,
        posOrStackDepthFun,
        fun(currentPayload),
        undecoratedMessage,
        args,
        namesOfArgs,
        row
      )
    mod.setStackTrace(getStackTrace)
    mod
  }
}
