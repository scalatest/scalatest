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
/*package org.scalatest.enablers

import org.scalatest.Assertion
import org.scalatest.Succeeded
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TableDrivenPropertyCheckFailedException

trait TableAsserting[T] {
  type Result
  val Singleton: Result
}

abstract class LowPriorityTableAsserting {

  abstract class TableAssertingImpl[T] extends TableAsserting[T] {


    def indicateSuccess(message: => String): Result

    def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, args: List[Any], namesOfArgs: List[String], optionalCause: Option[Throwable], payload: Option[Any], stackDepthFun: StackDepthException => Int, idx: Int): Result
  }

  implicit def assertingNatureOfT[T]: TableAsserting[T] { type Result = Unit } = {
    new TableAssertingImpl[T] {
      type Result = Unit
      val Singleton: Result = ()
      def indicateSuccess(message: => String): Unit = ()
      def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, args: List[Any], namesOfArgs: List[String], optionalCause: Option[Throwable], payload: Option[Any], stackDepthFun: StackDepthException => Int, idx: Int): Unit =
        throw new TableDrivenPropertyCheckFailedException(
          messageFun,
          optionalCause,
          stackDepthFun,
          payload,
          undecoratedMessage,
          args,
          namesOfArgs,
          idx
        )

    }
  }
}

abstract class MediumPriorityTableAsserting extends LowPriorityTableAsserting {
  implicit def assertingNatureOfAssertion: TableAsserting[Assertion] { type Result = Assertion } = {
    new TableAssertingImpl[Assertion] {
      type Result = Assertion
      val Singleton = Succeeded
      def indicateSuccess(message: => String): Assertion = Succeeded
      def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, args: List[Any], namesOfArgs: List[String], optionalCause: Option[Throwable], payload: Option[Any], stackDepthFun: StackDepthException => Int, idx: Int): Assertion =
        throw new TableDrivenPropertyCheckFailedException(
          messageFun,
          optionalCause,
          stackDepthFun,
          payload,
          undecoratedMessage,
          args,
          namesOfArgs,
          idx
        )
    }
  }

/*
  implicit def assertingNatureOfExpectation: TableAsserting[Expectation] { type Result = Expectation } = {
    new TableAsserting[Expectation] {
      type Result = Expectation
    }
  }
*/
}

object TableAsserting extends MediumPriorityTableAsserting {

  /*implicit def assertingNatureOfNothing: TableAsserting[Nothing] { type Result = Nothing } = {
    new TableAsserting[Nothing] {
      type Result = Nothing
      val Singleton = throw new NoSuchElementException
    }
  }*/
}*/
