/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalactic.source

import org.scalatest._
//SCALATESTJS-ONLY import scala.scalajs.js.annotation.JSExport

class ObjectMetaSpec extends funspec.AnyFunSpec with matchers.should.Matchers {

  case class Person(name: String, private val age: Int) {
    //SCALATESTJS-ONLY @JSExport
    val otherField = "test other field"
    private val privateField = "test private field"
  }

  describe("ObjectMeta") {

    it("should extract case class attribute names correctly") {
      ObjectMeta(Person("test", 33)).fieldNames should contain theSameElementsAs Set("name", "age", "otherField")
    }

    it("should extract dynamically field value correctly") {
      val meta = ObjectMeta(Person("test", 33))
      meta.value("name") shouldBe "test"
      meta.value("age") shouldBe 33
      meta.value("otherField") shouldBe "test other field"
    }

    it("should throw IllegalArgumentException when invalid attribute name is used to retrieve value") {
      val meta = ObjectMeta(Person("test", 33))
      val e = intercept[IllegalArgumentException] {
        meta.value("invalid")
      }
      e.getMessage shouldBe "'invalid' is not attribute for this instance."
    }

    it("should throw IllegalArgumentException when private attribute name is used to retrieve value") {
      val meta = ObjectMeta(Person("test", 33))
      val e = intercept[IllegalArgumentException] {
        meta.value("invalid")
      }
      e.getMessage shouldBe "'invalid' is not attribute for this instance."
    }
  }
}