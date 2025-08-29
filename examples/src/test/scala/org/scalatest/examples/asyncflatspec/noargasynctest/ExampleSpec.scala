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
package org.scalatest.examples.asyncflatspec.noargasynctest

import java.io.File
import org.scalatest._
import scala.concurrent.Future

class ExampleSpec extends AsyncFlatSpec {

  override def withFixture(test: NoArgAsyncTest) = {

    super.withFixture(test) onFailedThen { _ =>
      val currDir = new File(".")
      val fileNames = currDir.list()
      info("Dir snapshot: " + fileNames.mkString(", "))
    }
  }

  def addSoon(addends: Int*): Future[Int] = Future { addends.sum }
    
  "This test" should "succeed" in {
    addSoon(1, 1) map { sum => assert(sum == 2) }
  }

  it should "fail" in {
    addSoon(1, 1) map { sum => assert(sum == 3) }
  }
}

