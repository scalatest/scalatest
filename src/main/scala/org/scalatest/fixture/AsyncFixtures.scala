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

import org.scalatest.OutcomeOf._
import org.scalatest._
import exceptions.StackDepthExceptionHelper._
import scala.concurrent.{ExecutionContext, Future}

trait AsyncFixtures extends SuiteMixin { this: Suite with TestRegistration =>

  final override def withFixture(test: NoArgTest): Outcome = {
    throw new NotAllowedException(FailureMessages("withFixtureNotAllowedInAsyncFixtures"), getStackDepthFun("AsyncFixtures.scala", "withFixture"))
  }

  final override def withFixture(test: OneArgTest): Outcome = {
    throw new NotAllowedException(FailureMessages("withFixtureNotAllowedInAsyncFixtures"), getStackDepthFun("AsyncFixtures.scala", "withFixture"))
  }

  implicit def defaultExecutionContext: ExecutionContext = ExecutionContext.Implicits.global

  trait NoArgAsyncTest extends (() => Future[Outcome]) with TestData {
    /**
     * Runs the body of the test, returning an <code>Future[Outcome]</code>.
     */
    def apply(): Future[Outcome]
  }

  private[fixture] class FixturelessAsyncTestFunAndConfigMap(override val name: String, test: () => Future[Any], override val configMap: ConfigMap)
    extends NoArgAsyncTest {

    def apply(): Future[Outcome] = {
      test().map(any => Succeeded).recover {
        case ex: exceptions.TestCanceledException => Canceled(ex)
        case _: exceptions.TestPendingException => Pending
        case tfe: exceptions.TestFailedException => Failed(tfe)
        case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
      }
    }
    private val testData = testDataFor(name, configMap)
    val scopes = testData.scopes
    val text = testData.text
    val tags = testData.tags
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

  private[fixture] class AsyncTestFunAndConfigMap(val name: String, test: FixtureParam => Future[Any], val configMap: ConfigMap)
    extends OneArgAsyncTest {

    def apply(fixture: FixtureParam): Future[Outcome] = {
      test(fixture).map(any => Succeeded).recover {
        case ex: exceptions.TestCanceledException => Canceled(ex)
        case _: exceptions.TestPendingException => Pending
        case tfe: exceptions.TestFailedException => Failed(tfe)
        case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
      }
    }
    private val testData = testDataFor(name, configMap)
    val scopes = testData.scopes
    val text = testData.text
    val tags = testData.tags
  }

  def withAsyncFixture(test: NoArgAsyncTest): Future[Outcome] = {
    test()
  }

  def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome]
}