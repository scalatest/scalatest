/*
 * Copyright 2001-2014 Artima, Inc.
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

import scala.concurrent.Future
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun

trait AsyncSuite extends Suite { thisAsyncSuite =>

  protected[scalatest] def parallelAsyncTestExecution: Boolean = thisAsyncSuite.isInstanceOf[org.scalatest.ParallelTestExecution] ||
      thisAsyncSuite.isInstanceOf[org.scalatest.RandomTestOrder]

  final override def withFixture(test: NoArgTest): Outcome = {
    throw new exceptions.NotAllowedException(FailureMessages.withFixtureNotAllowedInAsyncFixtures, getStackDepthFun("AsyncSuite.scala", "withFixture"))
  }

  /**
   * A test function taking no arguments and returning an <code>Future[Outcome]</code>.
   *
   * <p>
   * For more detail and examples, see the relevant section in the
   * <a href="FlatSpec.html#withFixtureNoArgTest">documentation for trait <code>fixture.FlatSpec</code></a>.
   * </p>
   */
  trait NoArgAsyncTest extends (() => Future[Outcome]) with TestData {
    /**
     * Runs the body of the test, returning an <code>Future[Outcome]</code>.
     */
    def apply(): Future[Outcome]
  }

  def withAsyncFixture(test: NoArgAsyncTest): Future[Outcome] = {
    test()
  }
}
