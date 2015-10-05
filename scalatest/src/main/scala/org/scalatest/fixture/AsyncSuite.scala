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
package org.scalatest.fixture

import org.scalatest._
import org.scalatest.exceptions.StackDepthExceptionHelper._

import scala.concurrent.Future

trait AsyncSuite extends Suite with AsyncTestRegistration {

  final override def withFixture(test: NoArgTest): Outcome = {
    throw new exceptions.NotAllowedException(FailureMessages.withFixtureNotAllowedInAsyncFixtures, getStackDepthFun("AsyncFixtures.scala", "withFixture"))
  }

  final override def withFixture(test: OneArgTest): Outcome = {
    throw new exceptions.NotAllowedException(FailureMessages.withFixtureNotAllowedInAsyncFixtures, getStackDepthFun("AsyncFixtures.scala", "withFixture"))
  }

  /**
   * A test function taking no arguments and returning an <code>Future[Outcome]</code>.
   *
   * <p>
   * For more detail and examples, see the relevant section in the
   * <a href="FlatSpec.html#withFixtureNoArgTest">documentation for trait <code>fixture.FlatSpec</code></a>.
   * </p>
   */
  trait OneArgAsyncTest extends ((FixtureParam) => Future[Outcome]) with TestData {
    /**
     * Runs the body of the test, returning an <code>Future[Outcome]</code>.
     */
    def apply(fixture: FixtureParam): Future[Outcome]
  }

  def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome]

  import scala.language.implicitConversions

  implicit def convertToFuture(o: Any): Future[Any] = Future.successful(o)
}
