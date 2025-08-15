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
/*package org.scalatest.examples.asyncpropspec

import org.scalatest.AsyncPropSpec
import scala.concurrent.Future
import org.scalatest._
import prop._

class AddSpec extends AsyncPropSpec with TableDrivenPropertyChecks {

  val examples =
    Table(
      ("a", "b", "sum"), 
      (1, 2, 3),
      (2, 3, 5),
      (3, 4, 7)
    )

  def asyncForAll(table: TableFor3[Int, Int, Int])(fun: ((Int, Int, Int)) => Future[Assertion]): Future[Assertion] = {
    Future.sequence(
      table.map { case (a, b, expectedSum) =>
        fun((a, b, expectedSum))
      }
    ).map { assertions =>
      succeed
    }
  }

  def addSoon(addends: Int*): Future[Int] = Future { addends.sum }

  property("addSoon will eventually compute a sum of passed Ints") {
    asyncForAll(examples) { case (a, b, expectedSum) =>
      val futureSum: Future[Int] = addSoon(a, b)
      futureSum map { sum => assert(sum == expectedSum) }
    }
  } 

  def addNow(addends: Int*): Int = addends.sum

  property("addNow will immediately compute a sum of passed Ints") {
    forAll(examples) { case (a, b, expectedSum) =>
      val sum: Int = addNow(a, b)
      // You can also write synchronous tests. The body
      // must have result type Assertion:
      assert(sum == expectedSum)
    }
  }
}*/

