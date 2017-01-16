/*
* Copyright 2001-2014 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalactic

import org.scalatest._

class PresentSpec extends FunSpec {

  import Matchers._

  describe("A Present") {

    val present = Present("Happy Birthday!!")

    it("should offer a get method that returns the underlying value") {
      present.get shouldEqual "Happy Birthday!!"
    }

    it("should offer a map method") {
      present.map(v => "!!" + v).get shouldEqual "!!Happy Birthday!!"
    }

    it("should offer a flatMap method") {
      present.flatMap(v => Present("**" + v)).get shouldEqual "**Happy Birthday!!"
    }

    it("should equal when the underlying values are equal") {
      val a = Present("Thank you")
      val b = Present("Thank you")
      a shouldEqual b
    }

    it("should work correctly with 'for' using yield") {
      for (value <- Present("TesTing")) yield {
        value shouldEqual "TesTing"
      }
    }

    it("should work correctly with 'for' without yield") {
      for (value <- Present("TesTing")) {
        value shouldEqual "TesTing"
      }
    }

    it("should have a pretty toString") {
      Present("hello").toString shouldBe "Present(hello)"
      Present(33).toString shouldBe "Present(33)"
    }
  }
}
