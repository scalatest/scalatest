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

class CaseClassMetaSpec extends FunSpec with Matchers {

  case class Person(name: String, age: Int) {
    val otherField = "test"
  }

  describe("CaseClassMeta") {
    it("should throw IllegalArgumentException when instance passed in is not a case class") {
      val e = intercept[IllegalArgumentException] {
        CaseClassMeta("test")
      }
      e.getMessage shouldBe "java.lang.String is not a case class."
    }

    it("should extract case class attribute names correctly") {
      CaseClassMeta(Person("test", 33)).caseAccessorNames should contain theSameElementsAs Set("name", "age")
    }

    it("should extract dynamically field value correctly") {
      val meta = CaseClassMeta(Person("test", 33))
      meta.value("name") shouldBe "test"
      meta.value("age") shouldBe 33
    }

    it("should throw IllegalArgumentException when non case class accessor name is used to retrieve value") {
      val meta = CaseClassMeta(Person("test", 33))
      val e = intercept[IllegalArgumentException] {
        meta.value("otherField")
      }
      e.getMessage shouldBe "'otherField' is not a case accessor for this instance of case class."
    }
  }
}