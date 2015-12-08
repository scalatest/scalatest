/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalatest.examples.asyncfunspec.getfixture

import org.scalatest.AsyncFunSpec
import scala.concurrent.Future

class ExampleSpec extends AsyncFunSpec {

  def fixture: Future[String] = Future { "ScalaTest is " }

  describe("Testing") {
    it("should be easy") {
      val future = fixture
      val result = future map { s => s + "easy!" }
      result map { s =>
        assert(s == "ScalaTest is easy!")
      }
    }

    it("should be fun") {
      val future = fixture
      val result = future map { s => s + "fun!" }
      result map { s =>
        assert(s == "ScalaTest is fun!")
      }
    }
  }
}

