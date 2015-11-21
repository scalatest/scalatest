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
package org.scalatest.examples.asyncfunsuite.noargasynctest

import java.io.File
import org.scalatest._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class ExampleSuite extends AsyncFunSuite {

  implicit val executionContext = ExecutionContext.Implicits.global

  override def withAsyncFixture(test: NoArgAsyncTest) = {

    val futureOutcome = super.withAsyncFixture(test)

    futureOutcome onSuccess {
      case _: Failed =>
        val currDir = new File(".")
        val fileNames = currDir.list()
        println("Dir snapshot: " + fileNames.mkString(", "))
    }

    futureOutcome
  }

  def addSoon(addends: Int*): Future[Int] = Future { addends.sum }

  test("This test should succeed") {
    addSoon(1, 1) map { sum => assert(sum === 2) }
  }

  test("This test should fail") {
    addSoon(1, 1) map { sum => assert(sum === 3) }
  }
}

