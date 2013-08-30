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

import matchers.ShouldMatchers

class HelperSuite extends FunSuite with ShouldMatchers {

  import Configuration._

  val DefaultMinSuccessful = 9
  val PassedMinSuccessful = 3

  val DefaultMaxDiscarded = 99
  val PassedMaxDiscarded = 33

  val DefaultMinSize = 99
  val PassedMinSize = 33

  val DefaultMaxSize = 99
  val PassedMaxSize = 33

  val DefaultWorkers = 99
  val PassedWorkers = 33

  val defaultConfig =
    PropertyCheckConfig(
      minSuccessful = DefaultMinSuccessful,
      maxDiscarded = DefaultMaxDiscarded,
      minSize = DefaultMinSize,
      maxSize = DefaultMaxSize,
      workers = DefaultWorkers
    )

  // minSuccessful
  test("getParams returns passed minSuccessful config param") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.minSuccessfulTests should equal (PassedMinSuccessful)
  }

  test("getParams throws IAE if passed multiple minSuccessful config params") {
    intercept[IllegalArgumentException] {
      getParams(Seq(MinSuccessful(33), MinSuccessful(34)), defaultConfig)
    }
  }

  test("getParams returns default minSuccessful config param if none passed") {
    val params = getParams(Seq(MaxDiscarded(PassedMaxDiscarded)), defaultConfig)
    params.minSuccessfulTests should equal (DefaultMinSuccessful)
  }

  // maxDiscarded
  test("getParams returns passed maxDiscarded config param") {
    val params = getParams(Seq(MaxDiscarded(PassedMaxDiscarded)), defaultConfig)
    params.maxDiscardedTests should equal (PassedMaxDiscarded + 1)
  }

  test("getParams throws IAE if passed multiple maxDiscarded config params") {
    intercept[IllegalArgumentException] {
      getParams(Seq(MaxDiscarded(33), MaxDiscarded(34)), defaultConfig)
    }
  }

  test("getParams returns default maxDiscarded config param if none passed") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.maxDiscardedTests should equal (DefaultMaxDiscarded + 1)
  }

  // minSize
  test("getParams returns passed minSize config param") {
    val params = getParams(Seq(MinSize(PassedMinSize)), defaultConfig)
    params.minSize should equal (PassedMinSize)
  }

  test("getParams throws IAE if passed multiple minSize config params") {
    intercept[IllegalArgumentException] {
      getParams(Seq(MinSize(33), MinSize(34)), defaultConfig)
    }
  }

  test("getParams returns default minSize config param if none passed") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.minSize should equal (DefaultMinSize)
  }

  // maxSize
  test("getParams returns passed maxSize config param") {
    val params = getParams(Seq(MaxSize(PassedMaxSize)), defaultConfig)
    params.maxSize should equal (PassedMaxSize)
  }

  test("getParams throws IAE if passed multiple maxSize config params") {
    intercept[IllegalArgumentException] {
      getParams(Seq(MaxSize(33), MaxSize(34)), defaultConfig)
    }
  }

  test("getParams returns default maxSize config param if none passed") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.maxSize should equal (DefaultMaxSize)
  }

  // workers
  test("getParams returns passed workers config param") {
    val params = getParams(Seq(Workers(PassedWorkers)), defaultConfig)
    params.workers should equal (PassedWorkers)
  }

  test("getParams throws IAE if passed multiple workers config params") {
    intercept[IllegalArgumentException] {
      getParams(Seq(Workers(33), Workers(34)), defaultConfig)
    }
  }

  test("getParams returns default workers config param if none passed") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful)), defaultConfig)
    params.workers should equal (DefaultWorkers)
  }

  test("getParams returns all default if no config params passed") {
    val params = getParams(Seq(), defaultConfig)
    params.minSuccessfulTests should equal (DefaultMinSuccessful)
    params.maxDiscardedTests should equal (DefaultMaxDiscarded + 1)
    params.minSize should equal (DefaultMinSize)
    params.maxSize should equal (DefaultMaxSize)
    params.workers should equal (DefaultWorkers)
  }

  test("getParams returns all passed if all config params passed") {
    val params = getParams(Seq(MinSuccessful(PassedMinSuccessful), MaxDiscarded(PassedMaxDiscarded), MinSize(PassedMinSize), MaxSize(PassedMaxSize), Workers(PassedWorkers)), defaultConfig)
    params.minSuccessfulTests should equal (PassedMinSuccessful)
    params.maxDiscardedTests should equal (PassedMaxDiscarded + 1)
    params.minSize should equal (PassedMinSize)
    params.maxSize should equal (PassedMaxSize)
    params.workers should equal (PassedWorkers)
  }
}
