/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.examples.asyncfunspec.tagging

import org.scalatest.Tag

object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.AsyncFunSpec
import org.scalatest.tagobjects.Slow
import scala.concurrent.Future

class AddSpec extends AsyncFunSpec {

  def addSoon(addends: Int*): Future[Int] = Future { addends.sum }

  describe("addSoon") {
    it("will eventually compute a sum of passed Ints", Slow) {
      val futureSum: Future[Int] = addSoon(1, 2)
      // You can map assertions onto a Future, then return
      // the resulting Future[Assertion] to ScalaTest:
      futureSum map { sum => assert(sum == 3) }
    }
  }

  def addNow(addends: Int*): Int = addends.sum

  describe("addNow") {
    it("will immediately compute a sum of passed Ints",
      Slow, DbTest) {

      val sum: Int = addNow(1, 2)
      // You can also write synchronous tests. The body
      // must have result type Assertion:
      assert(sum == 3)
    }
  }
}

