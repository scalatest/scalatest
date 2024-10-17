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
package org.scalatest.examples.asyncfeaturespec.getfixture

import org.scalatest.AsyncFeatureSpec
import scala.concurrent.Future

class ExampleSpec extends AsyncFeatureSpec {

  def fixture: Future[String] = Future { "ScalaTest is designed to " }

  feature("Simplicity") {
    scenario("User needs to read test code written by others") {
      val future = fixture
      val result = future map { s => s + "encourage clear code!" }
      result map { s =>
        assert(s == "ScalaTest is designed to encourage clear code!")
      }
    }

    scenario("User needs to understand what the tests are doing") {
      val future = fixture
      val result = future map { s => s + "be easy to reason about!" }
      result map { s =>
        assert(s == "ScalaTest is designed to be easy to reason about!")
      }
    }
  }
}

