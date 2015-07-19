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

import scala.concurrent.{ExecutionContext, Future}
import org.scalatest._

//SCALATESTJS-ONLY @scala.scalajs.js.annotation.JSExportDescendentClasses(ignoreInvalidDescendants = true)
trait AsyncFreeSpecLike extends FreeSpecRegistration with AsyncClassicTests with OneInstancePerTest { thisSuite =>

  implicit def executionContext: ExecutionContext

  override private[scalatest] def transformToOutcome(testFun: FixtureParam => Registration): FixtureParam => AsyncOutcome =
    (fixture: FixtureParam) => {
      val futureUnit = testFun(fixture)
      FutureOutcome(
        futureUnit.map(u => Succeeded).recover {
          case ex: exceptions.TestCanceledException => Canceled(ex)
          case _: exceptions.TestPendingException => Pending
          case tfe: exceptions.TestFailedException => Failed(tfe)
          case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
        }
      )
    }

  private final val engine: FixtureEngine[FixtureParam] = getEngine

  import engine._

  protected override def runTest(testName: String, args: Args): Status = {

    if (args.runTestInNewInstance) {
      // In initial instance, so create a new test-specific instance for this test and invoke run on it.
      val oneInstance = newInstance
      oneInstance.run(Some(testName), args)
    }
    else {
      // Therefore, in test-specific instance, so run the test.
      def invokeWithAsyncFixture(theTest: TestLeaf): AsyncOutcome = {
        val theConfigMap = args.configMap
        val testData = testDataFor(testName, theConfigMap)
        FutureOutcome(
          withAsyncFixture(
            new OneArgAsyncTest {
              val name = testData.name

              def apply(fixture: FixtureParam): Future[Outcome] =
                theTest.testFun(fixture).toFutureOutcome

              val configMap = testData.configMap
              val scopes = testData.scopes
              val text = testData.text
              val tags = testData.tags
            }
          )
        )
      }

      runTestImpl(thisSuite, testName, args, true, invokeWithAsyncFixture)
    }
  }
}
