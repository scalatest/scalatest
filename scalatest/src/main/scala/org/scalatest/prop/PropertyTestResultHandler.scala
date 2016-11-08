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

import org.scalactic.{source, Prettifier}

import org.scalatest.{Fact, Assertion}
import org.scalatest.exceptions.{StackDepthException, GeneratorDrivenPropertyCheckFailedException}

private[scalatest] trait PropertyTestResultHandler[T] {

  def succeed(v: T): Boolean

  def indicateSuccess(): T

  def indicateFailure(messageFun: StackDepthException => String,
                      cause: Option[Throwable],
                      posOrStackDepthFun: Either[source.Position, StackDepthException => Int],
                      prettifier: Prettifier,
                      payload: Option[Any],
                      undecoratedMessage: String,
                      args: List[Any],
                      namesOfArgs: Option[List[String]]): T

}

private[scalatest] object PropertyTestResultHandler {

  implicit def checkerForAny[ASSERTION <: Assertion]: PropertyTestResultHandler[ASSERTION] =
    new PropertyTestResultHandler[ASSERTION] {
      def succeed(v: ASSERTION): Boolean = true

      def indicateSuccess(): ASSERTION = org.scalatest.Succeeded.asInstanceOf[ASSERTION]

      def indicateFailure(messageFun: StackDepthException => String,
                          cause: Option[Throwable],
                          posOrStackDepthFun: Either[source.Position, StackDepthException => Int],
                          prettifier: Prettifier,
                          payload: Option[Any],
                          undecoratedMessage: String,
                          args: List[Any],
                          namesOfArgs: Option[List[String]]): ASSERTION =
        throw new GeneratorDrivenPropertyCheckFailedException(
          messageFun,
          cause,
          posOrStackDepthFun,
          payload,
          undecoratedMessage,
          args,
          namesOfArgs,
          List.empty[String]
        )

    }

  implicit def checkerForBoolean[FACT <: Fact]: PropertyTestResultHandler[FACT] =
    new PropertyTestResultHandler[FACT] {
      def succeed(v: FACT): Boolean = {
        v.isYes
      }

      def indicateSuccess(): FACT = Fact.Yes("Property test passed").asInstanceOf[FACT]

      def indicateFailure(messageFun: StackDepthException => String,
                          cause: Option[Throwable],
                          posOrStackDepthFun: Either[source.Position, StackDepthException => Int],
                          prettifier: Prettifier,
                          payload: Option[Any],
                          undecoratedMessage: String,
                          args: List[Any],
                          namesOfArgs: Option[List[String]]): FACT = {
        val gdpcfe = new GeneratorDrivenPropertyCheckFailedException(
          messageFun,
          cause,
          posOrStackDepthFun,
          payload,
          undecoratedMessage,
          args,
          namesOfArgs,
          List.empty[String]
        )
        (Fact.No(gdpcfe.getMessage)(prettifier)).asInstanceOf[FACT]
      }
    }

  val default = checkerForAny

}