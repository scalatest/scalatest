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
package org.scalatest
package exceptions

import org.scalactic.Requirements._
import org.scalactic.exceptions.NullArgumentException
import org.scalactic.source

/**
 * Exception that indicates a property check failed.
 *
 * @param messageFun a function that returns a detail message (not optional) for this <code>PropertyCheckFailedException</code>.
 * @param cause an optional cause, the <code>Throwable</code> that caused this <code>PropertyCheckFailedException</code> to be thrown.
 * @param failedCodeStackDepthFun a function that returns the depth in the stack trace of this exception at which the line of test code that failed resides.
 * @param payload an optional payload, which ScalaTest will include in a resulting <code>TestFailed</code> event
 * @param undecoratedMessage just a short message that has no redundancy with args, labels, etc. The regular "message" has everything in it.
 * @param args the argument values that caused the property check to fail.
 * @param optionalArgNames an optional list of string names for the arguments.
 *
 * @throws NullArgumentException if any parameter is <code>null</code> or <code>Some(null)</code>.
 *
 * @author Bill Venners
 */
abstract class PropertyCheckFailedException(
  messageFun: StackDepthException => String,
  cause: Option[Throwable],
  pos: Option[source.Position],
  failedCodeStackDepthFun: StackDepthException => Int,
  payload: Option[Any],
  val undecoratedMessage: String,
  val args: List[Any],
  optionalArgNames: Option[List[String]]
) extends TestFailedException(sde => Some(messageFun(sde)), cause, pos, failedCodeStackDepthFun, payload) {

  requireNonNull(
    messageFun, cause, failedCodeStackDepthFun, undecoratedMessage, args,
    optionalArgNames)

  cause match {
    case Some(null) => throw new NullArgumentException("cause was a Some(null)")
    case _ =>
  }

  optionalArgNames match {
    case Some(null) => throw new NullArgumentException("optionalArgNames was a Some(null)")
    case _ =>
  }

  /**
   * A list of names for the arguments that caused the property check to fail.
   *
   * <p>
   * If the <code>optionalArgNames</code> class parameter is defined, this method returns
   * the <code>List[String]</code> contained in the <code>Some</code>. Otherwise, it returns
   * a list that gives <code>"arg0"</code> for the zeroeth argument, <code>"arg1"</code> for the
   * first argument, <code>"arg2"</code> for the second argument, and so on.
   * </p>
   */
  def argNames: List[String] =
    optionalArgNames match {
      case Some(argNames) => argNames
      case None => (for (idx <- 0 until args.length) yield { "arg" + idx }).toList
    }
}

