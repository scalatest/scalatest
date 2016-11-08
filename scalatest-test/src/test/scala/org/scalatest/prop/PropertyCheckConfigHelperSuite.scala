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

@deprecated("Remove when removing PropertyCheckConfig")
class PropertyCheckConfigHelperSuite extends FunSuite with Matchers {

  import Configuration._

  val DefaultMinSuccessful: PosInt = 9
  val PassedMinSuccessful: PosInt = 3

  val DefaultMaxDiscarded = 99
  val PassedMaxDiscarded = 33

  val DefaultMinSize: PosZInt = 99
  val PassedMinSize: PosZInt = 33

  val DefaultMaxSize = 99
  val PassedMaxSize = 33

  val DefaultWorkers: PosInt = 99
  val PassedWorkers: PosInt = 33

  val defaultConfig =
    PropertyCheckConfig(
      minSuccessful = DefaultMinSuccessful,
      maxDiscarded = DefaultMaxDiscarded,
      minSize = DefaultMinSize,
      maxSize = DefaultMaxSize,
      workers = DefaultWorkers
    )

  // minSuccessful
  test("getScalaCheckParams returns passed minSuccessful config param") {
    val params = getScalaCheckParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.minSuccessfulTests should equal (PassedMinSuccessful.value)
  }

  test("getScalaCheckParams throws IAE if passed multiple minSuccessful config params") {
    intercept[IllegalArgumentException] {
      getScalaCheckParams(Seq(MinSuccessful(33), MinSuccessful(34)), defaultConfig)
    }
  }

  test("getScalaCheckParams returns default minSuccessful config param if none passed") {
    val params = getScalaCheckParams(Seq(MaxDiscarded(PassedMaxDiscarded)), defaultConfig)
    params.minSuccessfulTests should equal (DefaultMinSuccessful.value)
  }

  def maxDiscardRatio(maxDiscardedTests: Int, minSuccessfulTests: Int): Float =
    (maxDiscardedTests: Float)/(minSuccessfulTests: Float)

  // maxDiscarded
  test("getScalaCheckParams returns passed maxDiscarded config param") {
    val params = getScalaCheckParams(Seq(MaxDiscarded(PassedMaxDiscarded)), defaultConfig)
    params.maxDiscardRatio should equal (maxDiscardRatio(PassedMaxDiscarded + 1, params.minSuccessfulTests))
  }

  test("getScalaCheckParams throws IAE if passed multiple maxDiscarded config params") {
    intercept[IllegalArgumentException] {
      getScalaCheckParams(Seq(MaxDiscarded(33), MaxDiscarded(34)), defaultConfig)
    }
  }

  test("getScalaCheckParams returns default maxDiscarded config param if none passed") {
    val params = getScalaCheckParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.maxDiscardRatio should equal (maxDiscardRatio(DefaultMaxDiscarded + 1, params.minSuccessfulTests))
  }

  // minSize
  test("getScalaCheckParams returns passed minSize config param") {
    val params = getScalaCheckParams(Seq(MinSize(PassedMinSize)), defaultConfig)
    params.minSize should equal (PassedMinSize.value)
  }

  test("getScalaCheckParams throws IAE if passed multiple minSize config params") {
    intercept[IllegalArgumentException] {
      getScalaCheckParams(Seq(MinSize(33), MinSize(34)), defaultConfig)
    }
  }

  test("getScalaCheckParams returns default minSize config param if none passed") {
    val params = getScalaCheckParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.minSize should equal (DefaultMinSize.value)
  }

  // maxSize
  test("getScalaCheckParams returns passed maxSize config param") {
    val params = getScalaCheckParams(Seq(MaxSize(PassedMaxSize)), defaultConfig)
    params.maxSize should equal (PassedMaxSize)
  }

  test("getScalaCheckParams throws IAE if passed multiple maxSize config params") {
    intercept[IllegalArgumentException] {
      getScalaCheckParams(Seq(MaxSize(33), MaxSize(34)), defaultConfig)
    }
  }

  test("getScalaCheckParams returns default maxSize config param if none passed") {
    val params = getScalaCheckParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.maxSize should equal (DefaultMaxSize)
  }

  test("getScalaCheckParams returns default maxSize config param if none passed and MinSuccessful changed") {
    val params = getScalaCheckParams(Seq(MinSize(PassedMinSize)), defaultConfig)
    println("Params.maxsize: " + params.maxSize)
    params.maxSize should equal (DefaultMaxSize)
    params.minSize should equal (PassedMinSize.value)
  }

  // workers
  test("getScalaCheckParams returns passed workers config param") {
    val params = getScalaCheckParams(Seq(Workers(PassedWorkers)), defaultConfig)
    params.workers should equal (PassedWorkers.value)
  }

  test("getScalaCheckParams throws IAE if passed multiple workers config params") {
    intercept[IllegalArgumentException] {
      getScalaCheckParams(Seq(Workers(33), Workers(34)), defaultConfig)
    }
  }

  test("getScalaCheckParams returns default workers config param if none passed") {
    val params = getScalaCheckParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.workers should equal (DefaultWorkers.value)
  }

  test("getScalaCheckParams returns all default if no config params passed") {
    val params = getScalaCheckParams(Seq(), defaultConfig)
    params.minSuccessfulTests should equal (DefaultMinSuccessful.value)
    params.maxDiscardRatio should equal (maxDiscardRatio(DefaultMaxDiscarded + 1, params.minSuccessfulTests))
    params.minSize should equal (DefaultMinSize.value)
    params.maxSize should equal (DefaultMaxSize)
    params.workers should equal (DefaultWorkers.value)
  }

  test("getScalaCheckParams returns all passed if all config params passed") {
    val params = getScalaCheckParams(Seq(MinSuccessful(PassedMinSuccessful), MaxDiscarded(PassedMaxDiscarded), MinSize(PassedMinSize), MaxSize(PassedMaxSize), Workers(PassedWorkers)), defaultConfig)
    params.minSuccessfulTests should equal (PassedMinSuccessful.value)
    params.maxDiscardRatio should equal (maxDiscardRatio(PassedMaxDiscarded + 1, params.minSuccessfulTests))
    params.minSize should equal (PassedMinSize.value)
    params.maxSize should equal (PassedMaxSize)
    params.workers should equal (PassedWorkers.value)
  }
}
