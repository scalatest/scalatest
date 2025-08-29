/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalatest.examples.asyncfunsuite.ignoreall

import org.scalatest.AsyncFunSuite
import scala.concurrent.Future
import org.scalatest.Ignore

@Ignore
class AddSuite extends AsyncFunSuite {

  def addSoon(addends: Int*): Future[Int] = Future { addends.sum }

  test("addSoon will eventually compute a sum of passed Ints") {
    val futureSum: Future[Int] = addSoon(1, 2)
    // You can map assertions onto a Future, then return
    // the resulting Future[Assertion] to ScalaTest:
    futureSum map { sum => assert(sum == 3) }
  } 

  def addNow(addends: Int*): Int = addends.sum

  test("addNow will immediately compute a sum of passed Ints") {
    val sum: Int = addNow(1, 2)
    // You can also write synchronous tests. The body
    // must have result type Assertion:
    assert(sum == 3)
  }
}

