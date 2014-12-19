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

import org.scalactic.anyvals._

class PropertyCheckConfigurationHelperSuite extends FunSuite with Matchers {

  import org.scalatest.prop.Configuration._

  val DefaultMinSuccessful: PosInt = 9
  val PassedMinSuccessful: PosInt = 3

  val DefaultMinSize: PozInt = 99
  val PassedMinSize: PozInt = 33

  val DefaultWorkers: PosInt = 99
  val PassedWorkers: PosInt = 33

  val DefaultSizeRange: PozInt = 0
  val PassedSizeRange: PozInt = 10

  val DefaultMaxDiscardedFactor: PozDouble = 1.0
  val PassedMaxDiscardedFactor: PozDouble = 0.5

  val defaultConfig =
    PropertyCheckConfiguration(
      minSuccessful = DefaultMinSuccessful,
      maxDiscardedFactor = DefaultMaxDiscardedFactor,
      minSize = DefaultMinSize,
      sizeRange = DefaultSizeRange,
      workers = DefaultWorkers
    )

  // minSuccessful
  test("getParams returns passed minSuccessful config param") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.minSuccessfulTests should equal (PassedMinSuccessful.value)
  }

  test("getParams throws IAE if passed multiple minSuccessful config params") {
    intercept[IllegalArgumentException] {
      getParams(Seq(MinSuccessful(33), MinSuccessful(34)), defaultConfig)
    }
  }

  test("getParams returns default minSuccessful config param if none passed") {
    val params = getParams(Seq(Workers(DefaultWorkers)), defaultConfig)
    params.minSuccessfulTests should equal (DefaultMinSuccessful.value)
  }

  def maxDiscardRatio(maxDiscardedTests: Int, minSuccessfulTests: Int): Float =
    (maxDiscardedTests: Float)/(minSuccessfulTests: Float)

  // maxDiscarded
  test("getParams returns passed maxDiscarded config param") {
    val params = getParams(Seq(MaxDiscardedFactor(PassedMaxDiscardedFactor)), defaultConfig)
    params.maxDiscardRatio should equal (PassedMaxDiscardedFactor.value)
  }

  test("getParams throws IAE if passed multiple maxDiscarded config params") {
    intercept[IllegalArgumentException] {
      getParams(Seq(MaxDiscardedFactor(33.0), MaxDiscardedFactor(34.0)), defaultConfig)
    }
  }

  test("getParams returns default maxDiscarded config param if none passed") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.maxDiscardRatio should equal (DefaultMaxDiscardedFactor.value)
  }

  // minSize
  test("getParams returns passed minSize config param") {
    val params = getParams(Seq(MinSize(PassedMinSize)), defaultConfig)
    params.minSize should equal (PassedMinSize.value)
  }

  test("getParams throws IAE if passed multiple minSize config params") {
    intercept[IllegalArgumentException] {
      getParams(Seq(MinSize(33), MinSize(34)), defaultConfig)
    }
  }

  test("getParams returns default minSize config param if none passed") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.minSize should equal (DefaultMinSize.value)
  }

  // sizeRange
  test("getParams returns passed sizeRange config param") {
    val params = getParams(Seq(SizeRange(PassedSizeRange)), defaultConfig)
    params.maxSize should equal (DefaultMinSize + PassedSizeRange)
  }

  test("getParams returns passed minSize and sizeRange config param") {
    val params = getParams(Seq(MinSize(PassedMinSize), SizeRange(PassedSizeRange)), defaultConfig)
    params.maxSize should equal (PassedMinSize + PassedSizeRange)
  }

  test("getParams throws IAE if passed multiple maxSize config params") {
    intercept[IllegalArgumentException] {
      getParams(Seq(MaxSize(33), MaxSize(34)), defaultConfig)
    }
    intercept[IllegalArgumentException] {
      getParams(Seq(MaxSize(33), SizeRange(34)), defaultConfig)
    }
    intercept[IllegalArgumentException] {
      getParams(Seq(SizeRange(33), SizeRange(34)), defaultConfig)
    }
  }

  test("getParams returns default sizeRange config if none passed") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.maxSize should equal (DefaultMinSize + DefaultSizeRange)
  }

  // workers
  test("getParams returns passed workers config param") {
    val params = getParams(Seq(Workers(PassedWorkers)), defaultConfig)
    params.workers should equal (PassedWorkers.value)
  }

  test("getParams throws IAE if passed multiple workers config params") {
    intercept[IllegalArgumentException] {
      getParams(Seq(Workers(33), Workers(34)), defaultConfig)
    }
  }

  test("getParams returns default workers config param if none passed") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.workers should equal (DefaultWorkers.value)
  }

  test("getParams returns all default if no config params passed") {
    val params = getParams(Seq(), defaultConfig)
    params.minSuccessfulTests should equal (DefaultMinSuccessful.value)
    params.maxDiscardRatio should equal (DefaultMaxDiscardedFactor.value)
    params.minSize should equal (DefaultMinSize.value)
    params.maxSize should equal (DefaultMinSize.value + DefaultSizeRange.value)
    params.workers should equal (DefaultWorkers.value)
  }

  test("getParams returns all passed if all config params passed") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful), MaxDiscardedFactor(PassedMaxDiscardedFactor), MinSize(PassedMinSize),
      SizeRange(PassedSizeRange), Workers(PassedWorkers)), defaultConfig)
    params.minSuccessfulTests should equal (PassedMinSuccessful.value)
    params.maxDiscardRatio should equal (PassedMaxDiscardedFactor.value)
    params.minSize should equal (PassedMinSize.value)
    params.maxSize should equal (PassedMinSize.value + PassedSizeRange.value)
    params.workers should equal (PassedWorkers.value)
  }
}
