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
package org.scalatest.tools

import org.scalatest._

class PrintReporterSuite extends FunSuite {

  import PrintReporter.makeDurationString

  test("makeDurationString when duration < 1000") {
    assert(makeDurationString(23) === "23 milliseconds")
  }

  test("makeDurationString when duration == 1") {
    assert(makeDurationString(1) === "1 millisecond")
  }

  test("makeDurationString when duration == 1000") {
    assert(makeDurationString(1000) === "1 second")
  }

  test("makeDurationString when duration == 1001") {
    assert(makeDurationString(1001) === "1 second, 1 millisecond")
  }

  test("makeDurationString when duration == 1049") {
    assert(makeDurationString(1049) === "1 second, 49 milliseconds")
  }

  test("makeDurationString when duration == 2000") {
    assert(makeDurationString(2000) === "2 seconds")
  }

  test("makeDurationString when duration == 10000") {
    assert(makeDurationString(10000) === "10 seconds")
  }

  test("makeDurationString when duration == 3049") {
    assert(makeDurationString(3049) === "3 seconds, 49 milliseconds")
  }

  test("makeDurationString when duration == 60000") {
    assert(makeDurationString(60000) === "1 minute")
  }

  test("makeDurationString when duration == 60001") {
    assert(makeDurationString(60000) === "1 minute")
  }

  test("makeDurationString when duration == 60999") {
    assert(makeDurationString(60000) === "1 minute")
  }

  test("makeDurationString when duration == 61000") {
    assert(makeDurationString(61000) === "1 minute, 1 second")
  }

  test("makeDurationString when duration == 61999") {
    assert(makeDurationString(61000) === "1 minute, 1 second")
  }

  test("makeDurationString when duration == 62000") {
    assert(makeDurationString(62000) === "1 minute, 2 seconds")
  }

  test("makeDurationString when duration == 65388") {
    assert(makeDurationString(65388) === "1 minute, 5 seconds")
  }

  test("makeDurationString when duration == 120000") {
    assert(makeDurationString(120000) === "2 minutes")
  }

  test("makeDurationString when duration == 120999") {
    assert(makeDurationString(120999) === "2 minutes")
  }

  test("makeDurationString when duration == 121000") {
    assert(makeDurationString(121000) === "2 minutes, 1 second")
  }

  test("makeDurationString when duration == 241999") {
    assert(makeDurationString(241999) === "4 minutes, 1 second")
  }

  test("makeDurationString when duration == 122000") {
    assert(makeDurationString(122000) === "2 minutes, 2 seconds")
  }

  test("makeDurationString when duration == 299999") {
    assert(makeDurationString(299999) === "4 minutes, 59 seconds")
  }

  test("makeDurationString when duration == 3600000") {
    assert(makeDurationString(3600000) === "1 hour")
  }

  test("makeDurationString when duration == 3600999") {
    assert(makeDurationString(3600999) === "1 hour")
  }

  test("makeDurationString when duration == 3601000") {
    assert(makeDurationString(3601000) === "1 hour, 1 second")
  }

  test("makeDurationString when duration == 3601999") {
    assert(makeDurationString(3601999) === "1 hour, 1 second")
  }

  test("makeDurationString when duration == 3602000") {
    assert(makeDurationString(3602000) === "1 hour, 2 seconds")
  }

  test("makeDurationString when duration == 3659999") {
    assert(makeDurationString(3659999) === "1 hour, 59 seconds")
  }

  test("makeDurationString when duration == 3660000") {
    assert(makeDurationString(3660000) === "1 hour, 1 minute")
  }

  test("makeDurationString when duration == 3660999") {
    assert(makeDurationString(3660999) === "1 hour, 1 minute")
  }

  test("makeDurationString when duration == 3661000") {
    assert(makeDurationString(3661000) === "1 hour, 1 minute, 1 second")
  }

  test("makeDurationString when duration == 3661999") {
    assert(makeDurationString(3661999) === "1 hour, 1 minute, 1 second")
  }

  test("makeDurationString when duration == 3662000") {
    assert(makeDurationString(3662000) === "1 hour, 1 minute, 2 seconds")
  }

  test("makeDurationString when duration == 3719999") {
    assert(makeDurationString(3719999) === "1 hour, 1 minute, 59 seconds")
  }

  test("makeDurationString when duration == 3720000") {
    assert(makeDurationString(3720000) === "1 hour, 2 minutes")
  }

  test("makeDurationString when duration == 7140999") {
    assert(makeDurationString(7140999) === "1 hour, 59 minutes")
  }

  test("makeDurationString when duration == 3721000") {
    assert(makeDurationString(3721000) === "1 hour, 2 minutes, 1 second")
  }

  test("makeDurationString when duration == 7141999") {
    assert(makeDurationString(7141999) === "1 hour, 59 minutes, 1 second")
  }

  test("makeDurationString when duration == 3722500") {
    assert(makeDurationString(3722500) === "1 hour, 2 minutes, 2 seconds")
  }

  test("makeDurationString when duration == 7199999") {
    assert(makeDurationString(7199999) === "1 hour, 59 minutes, 59 seconds")
  }

  test("makeDurationString when duration == 7200000") {
    assert(makeDurationString(7200000) === "2 hours")
  }

  test("makeDurationString when duration == 360000000") {
    assert(makeDurationString(360000000) === "100 hours")
  }

  test("makeDurationString when duration == 7201000") {
    assert(makeDurationString(7201000) === "2 hours, 1 second")
  }

  test("makeDurationString when duration == 7201999") {
    assert(makeDurationString(7201999) === "2 hours, 1 second")
  }

  test("makeDurationString when duration == 7202000") {
    assert(makeDurationString(7202000) === "2 hours, 2 seconds")
  }

  test("makeDurationString when duration == 7259999") {
    assert(makeDurationString(7259999) === "2 hours, 59 seconds")
  }

  test("makeDurationString when duration == 7260000") {
    assert(makeDurationString(7260000) === "2 hours, 1 minute")
  }

  test("makeDurationString when duration == 7260999") {
    assert(makeDurationString(7260999) === "2 hours, 1 minute")
  }

  test("makeDurationString when duration == 7261000") {
    assert(makeDurationString(7261000) === "2 hours, 1 minute, 1 second")
  }

  test("makeDurationString when duration == 7261999") {
    assert(makeDurationString(7261999) === "2 hours, 1 minute, 1 second")
  }

  test("makeDurationString when duration == 7262000") {
    assert(makeDurationString(7262000) === "2 hours, 1 minute, 2 seconds")
  }

  test("makeDurationString when duration == 7319999") {
    assert(makeDurationString(7319999) === "2 hours, 1 minute, 59 seconds")
  }

  test("makeDurationString when duration == 7320000") {
    assert(makeDurationString(7320000) === "2 hours, 2 minutes")
  }

  test("makeDurationString when duration == 10740999") {
    assert(makeDurationString(10740999) === "2 hours, 59 minutes")
  }

  test("makeDurationString when duration == 7321000") {
    assert(makeDurationString(7321000) === "2 hours, 2 minutes, 1 second")
  }

  test("makeDurationString when duration == 10741999") {
    assert(makeDurationString(10741999) === "2 hours, 59 minutes, 1 second")
  }

  test("makeDurationString when duration == 7322500") {
    assert(makeDurationString(7322500) === "2 hours, 2 minutes, 2 seconds")
  }

  test("makeDurationString when duration == 10799999") {
    assert(makeDurationString(10799999) === "2 hours, 59 minutes, 59 seconds")
  }
}
