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
package org.scalatest.prop

import org.scalatest.prop.Configuration.Parameter
import org.scalatest.{FailureMessages, UnquotedString, _}
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.enablers.PropCheckerAsserting

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import org.scalactic.anyvals.PosZInt

// TODO: What is this used for?
trait PropertyTest {

  type RESULT

  def check: PropertyTest.Result

  def discard(v: RESULT): Boolean

  def succeed(v: RESULT): (Boolean, Option[Throwable])

}

object PropertyTest {

  sealed trait Result

  case class CheckSuccess(args: List[PropertyArgument]) extends Result

  case class CheckExhausted(succeeded: Long, discarded: Long, names: List[String], argsPassed: List[PropertyArgument]) extends Result

  case class CheckFailure(succeeded: Long, ex: Option[Throwable], names: List[String], argsPassed: List[PropertyArgument]) extends Result
}
