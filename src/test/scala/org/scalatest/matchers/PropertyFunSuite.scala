/*
 * Copyright 2001-2008 Artima, Inc.
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

class PropertyFunSuite extends FunSuite with ShouldMatchers {

  test("object has no appropriately named field, method, or get method (0, 0, 0)") {
    class DontGotNuthin
    val dgn = new DontGotNuthin
    val result = Helper.accessProperty(dgn, 'fred, false)
    result should be (None)
  }

  test("object has only an appropriately named get method (0, 0, 1)") {
    class HasGetMethod {
      def getCow: Int = 1
    }
    val obj = new HasGetMethod
    val result = Helper.accessProperty(obj, 'cow, false)
    result should be (Some(1))
  }

  test("object has only an appropriately named Boolean is method (0, 0, 1)") {
    class HasIsMethod {
      def isCow: Boolean = true
    }
    val obj = new HasIsMethod
    val result = Helper.accessProperty(obj, 'cow, true)
    result should be (Some(true))
  }

  test("object has only an appropriately named method (0, 1, 0)") {
    class HasMethod {
      def cow: Int = 1
    }
    val obj = new HasMethod
    val result = Helper.accessProperty(obj, 'cow, false)
    result should be (Some(1))
  }

  test("object has only an appropriately named Scala field, which results in a Java method (0, 1, 0)") {
    class HasScalaField {
      val cow: Int = 1
    }
    val obj = new HasScalaField
    val result = Helper.accessProperty(obj, 'cow, false)
    result should be (Some(1))
  }

  test("object has only an appropriately named method and getMethod (0, 1, 1)") {
    class HasMethod {
      def cow: Int = 1
      def getCow: Int = 2
    }
    val obj = new HasMethod
    val result = Helper.accessProperty(obj, 'cow, false)
    result should be (Some(1))
  }

  test("object has only an appropriately named field (1, 0, 0)") {
    val obj = new HasField // A Java class, because can't get a field in a Scala class
    val result = Helper.accessProperty(obj, 'cow, false)
    result should be (Some(1))
  }

  test("object has an appropriately named field and getMethod (1, 0, 1)") {
    val obj = new HasFieldAndGetMethod // A Java class, because can't get a field in a Scala class
    val result = Helper.accessProperty(obj, 'cow, false)
    result should be (Some(1))
  }

  test("object has an appropriately named field and method (1, 1, 0)") {
    val obj = new HasFieldAndMethod // A Java class, because can't get a field in a Scala class
    val result = Helper.accessProperty(obj, 'cow, false)
    result should be (Some(1))
  }

  test("object has an appropriately named field and method and getMethod (1, 1, 1)") {
    val obj = new HasFieldMethodAndGetMethod // A Java class, because can't get a field in a Scala class
    val result = Helper.accessProperty(obj, 'cow, false)
    result should be (Some(1))
  }

  test("works on set.empty") {
    val result1 = Helper.accessProperty(Set(), 'empty, true)
    result1 should be (Some(true))
    val result2 = Helper.accessProperty(Set(1, 2, 3), 'empty, true)
    result2 should be (Some(false))
  }
}
