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
package org.scalatest.examples.asyncfunsuite.oneargasynctest

import org.scalatest._
import java.io._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class ExampleSuite extends fixture.AsyncFunSuite {

  implicit val executionContext = ExecutionContext.Implicits.global

  case class FixtureParam(file: File, writer: FileWriter)

  def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] = {

    // create the fixture
    val file = File.createTempFile("hello", "world")
    val writer = new FileWriter(file)

    withCleanup {
      writer.write("ScalaTest is ") // set up the fixture
      val theFixture = FixtureParam(file, writer)
      // "loan" the fixture to the test
      withAsyncFixture(test.toNoArgAsyncTest(theFixture))
    } {
      writer.close() // clean up the fixture
    }
  }

  test("Testing should be easy") { f =>
    val futureFile =
      Future {
        f.writer.write("easy!")
        f.writer.flush()
        f.file
      }
    futureFile map { file => assert(file.length === 18) }
  }

  test("Testing should be fun") { f =>
    val futureFile =
      Future {
        f.writer.write("fun!")
        f.writer.flush()
        f.file
      }
    futureFile map { file => assert(file.length === 17) }
  }
}
