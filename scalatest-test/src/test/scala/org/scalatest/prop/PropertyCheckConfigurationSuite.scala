/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalatest
package prop

import org.scalatest.prop.Configuration._

class PropertyCheckConfigurationSuite extends FunSuite with Matchers {

  test("minSuccessful will not compile if less than 1") {
    "PropertyCheckConfiguration(minSuccessful = 0)"  shouldNot compile
    "PropertyCheckConfiguration(minSuccessful = -1)" shouldNot compile
    "PropertyCheckConfiguration(minSuccessful = -2)" shouldNot compile
  }

  test("minSuccussful value is passed value, if valid") {
      PropertyCheckConfiguration(minSuccessful = 1).minSuccessful.value should be (1)
      PropertyCheckConfiguration(minSuccessful = 2).minSuccessful.value should be (2)
      PropertyCheckConfiguration(minSuccessful = 5678).minSuccessful.value should be (5678)
  }

  test("maxDiscarded should not compile if less than 0") {
    "PropertyCheckConfiguration(maxDiscardedFactor = -1)" shouldNot compile
    "PropertyCheckConfiguration(maxDiscardedFactor = -2)" shouldNot compile
  }

  test("maxDiscardedFactor value is passed value, if valid") {
      PropertyCheckConfiguration(maxDiscardedFactor = 0.0).maxDiscardedFactor.value should be (0.0)
      PropertyCheckConfiguration(maxDiscardedFactor = 1.1).maxDiscardedFactor.value should be (1.1)
      PropertyCheckConfiguration(maxDiscardedFactor = 2.2).maxDiscardedFactor.value should be (2.2)
      PropertyCheckConfiguration(maxDiscardedFactor = 5678.3).maxDiscardedFactor.value should be (5678.3)
  }

  test("minSize should not compile if less than 0") {
    "PropertyCheckConfiguration(minSize = -1)" shouldNot compile
    "PropertyCheckConfiguration(minSize = -2)" shouldNot compile
  }

  test("minSize value is passed value, if valid") {
      PropertyCheckConfiguration(minSize = 0).minSize.value should be (0)
      PropertyCheckConfiguration(minSize = 1).minSize.value should be (1)
      PropertyCheckConfiguration(minSize = 2).minSize.value should be (2)
      PropertyCheckConfiguration(minSize = 99).minSize.value should be (99)
      PropertyCheckConfiguration(minSize = 100).minSize.value should be (100)
  }

  test("maxSize should not compile if less than 0") {
    "PropertyCheckConfiguration(sizeRange = -1)" shouldNot compile
    "PropertyCheckConfiguration(sizeRange = -2)" shouldNot compile
  }

  test("sizeRange value is passed value, if valid") {
      PropertyCheckConfiguration(sizeRange = 0).sizeRange.value should be (0)
      PropertyCheckConfiguration(sizeRange = 1).sizeRange.value should be (1)
      PropertyCheckConfiguration(sizeRange = 2).sizeRange.value should be (2)
      PropertyCheckConfiguration(sizeRange = 5678).sizeRange.value should be (5678)
  }

  test("workers should not compile if less than 1") {
    "PropertyCheckConfiguration(workers = 0)" shouldNot compile
    "PropertyCheckConfiguration(workers = -1)" shouldNot compile
    "PropertyCheckConfiguration(workers = -2)" shouldNot compile
  }

  test("workers value is passed value, if valid") {
      PropertyCheckConfiguration(workers = 1).workers.value should be (1)
      PropertyCheckConfiguration(workers = 2).workers.value should be (2)
      PropertyCheckConfiguration(workers = 5678).workers.value should be (5678)
  }
}
