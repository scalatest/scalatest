/*
 * Copyright 2001-2017 Artima, Inc.
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

import org.scalactic.source
import org.scalatest.exceptions.{StackDepthException, TestFailedException, TestPendingException}
import org.scalatest.{Assertion, PendingStatement, Resources}

import scala.collection.generic.FilterMonadic
import scala.concurrent.{ExecutionContext, Future}

trait Fixable[T] {
  type Result
  def pendingUntilFixed(f: => T, pos: source.Position): Result
}

trait LowerPriorityFixable {

  implicit def fixableFor[T]: Fixable[T] { type Result = Unit with PendingStatement } = new Fixable[T] {
    type Result = Unit with PendingStatement
    def pendingUntilFixed(f: => T, pos: source.Position): Unit with PendingStatement = {
      val isPending =
        try {
          f
          false
        }
        catch {
          case _: Exception => true
          case _: AssertionError => true
        }
      if (isPending)
        throw new TestPendingException
      else
        throw new TestFailedException((sde: StackDepthException) => Some(Resources.pendingUntilFixed), None, pos)
    }
  }
}

object Fixable extends LowerPriorityFixable {

  implicit def fixableForAssertion: Fixable[Assertion] { type Result = Assertion with PendingStatement } = new Fixable[Assertion] {
    type Result = Assertion with PendingStatement
    def pendingUntilFixed(f: => Assertion, pos: source.Position): Assertion with PendingStatement = {
      val isPending =
        try {
          f
          false
        }
        catch {
          case _: Exception => true
          case _: AssertionError => true
        }
      if (isPending)
        throw new TestPendingException
      else
        throw new TestFailedException((sde: StackDepthException) => Some(Resources.pendingUntilFixed), None, pos)
    }
  }

  implicit def fixableForFutureAssertion(implicit ec: ExecutionContext): Fixable[Future[Assertion]] { type Result = Future[Assertion with PendingStatement] } = new Fixable[Future[Assertion]] {
    type Result = Future[Assertion with PendingStatement] 
    def pendingUntilFixed(f: => Future[Assertion], pos: source.Position): Future[Assertion with PendingStatement] = {
      val future: Future[Assertion] = f
      future.transform(
        // Given the Future succeeded, the Future should now fail to indicate the pendingUntilFixed should be removed.
        _ => throw new TestFailedException((sde: StackDepthException) => Some(Resources.pendingUntilFixed), None, pos),
        ex => 
          ex match {
            case _: Exception => new TestPendingException
            case _: AssertionError => new TestPendingException
            case _ => new TestFailedException((sde: StackDepthException) => Some(Resources.pendingUntilFixed), None, pos)
          }
      )
    }
  }
}
