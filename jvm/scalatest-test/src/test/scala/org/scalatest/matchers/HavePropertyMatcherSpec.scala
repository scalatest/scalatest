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
package org.scalatest.matchers

import org.scalatest._
import java.io.File
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class HavePropertyMatcherSpec extends AnyFunSpec {

  describe("HavePropertyMatcher ") {

    describe("instance created by HavePropertyMatcher apply method") {

      val havePropertyMatcher = HavePropertyMatcher[String, Int] { str =>
        HavePropertyMatchResult(true, "name", 8, 9)
      }

      it("should have pretty toString") {
        havePropertyMatcher.toString should be ("HavePropertyMatcher[java.lang.String, int](java.lang.String => HavePropertyMatchResult[int])")
      }

    }

  }

}
