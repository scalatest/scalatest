/*
 * Copyright 2001-2012 Artima, Inc.
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
package org.scalatest.time

import org.scalatest.{SeveredStackTraces, FunSpec}
import org.scalatest.matchers.ShouldMatchers

class SpanSugarSpec extends FunSpec with SpanMatchers with ShouldMatchers with SeveredStackTraces {
  
  import SpanSugar._
 
  describe("The SpanSugar trait") {
    
    it("should provide implicit conversions for Int time spans") {
      assert((1 nanosecond) === Span(1, Nanosecond))
      assert((2 nanoseconds) === Span(2, Nanoseconds))
      assert((1 microsecond) === Span(1, Microsecond))
      assert((2 microseconds) === Span(2, Microseconds))
      assert((1 millisecond) === Span(1, Millisecond))
      assert((2 milliseconds) === Span(2, Milliseconds))
      assert((2 millis) === Span(2, Millis))
      assert((2 seconds) === Span(2, Seconds))
      assert((1 second) === Span(1, Second))
      assert((2 seconds) === Span(2, Seconds))
      assert((1 minute) === Span(1, Minute))
      assert((2 minutes) === Span(2, Minutes))
      assert((1 hour) === Span(1, Hour))
      assert((2 hours) === Span(2, Hours))
      assert((1 day) === Span(1, Day))
      assert((2 days) === Span(2, Days))
    }
    
    it("should provide implicit conversions for Long time spans") {
      assert((1L nanosecond) === Span(1, Nanosecond))
      assert((2L nanoseconds) === Span(2, Nanoseconds))
      assert((1L microsecond) === Span(1, Microsecond))
      assert((2L microseconds) === Span(2, Microseconds))
      assert((1L millisecond) === Span(1, Millisecond))
      assert((2L milliseconds) === Span(2, Milliseconds))
      assert((2L millis) === Span(2, Millis))
      assert((2L seconds) === Span(2, Seconds))
      assert((1L second) === Span(1, Second))
      assert((2L seconds) === Span(2, Seconds))
      assert((1L minute) === Span(1, Minute))
      assert((2L minutes) === Span(2, Minutes))
      assert((1L hour) === Span(1, Hour))
      assert((2L hours) === Span(2, Hours))
      assert((1L day) === Span(1, Day))
      assert((2L days) === Span(2, Days))
    }

    it("should provide an implicit conversion from GrainOfTime to Long") {
      def getALong(aSpan: Span) = aSpan.totalNanos
      assert(getALong(1 nanosecond) === 1L)
      assert(getALong(2 nanoseconds) === 2L)
      assert(getALong(1 microsecond) === 1L * 1000)
      assert(getALong(2 microseconds) === 2L * 1000)
      assert(getALong(1 millisecond) === 1L * 1000 * 1000)
      assert(getALong(2 milliseconds) === 2L * 1000 * 1000)
      assert(getALong(2 millis) === 2L * 1000 * 1000)
      assert(getALong(2 seconds) === 2L * 1000 * 1000 * 1000)
      assert(getALong(1 second) === 1000L * 1000 * 1000)
      assert(getALong(2 seconds) === 2L * 1000 * 1000 * 1000)
      assert(getALong(1 minute) === 1000L * 60 * 1000 * 1000)
      assert(getALong(2 minutes) === 2L * 1000 * 60 * 1000 * 1000)
      assert(getALong(1 hour) === 1000L * 60 * 60 * 1000 * 1000)
      assert(getALong(2 hours) === 2L * 1000 * 60 * 60 * 1000 * 1000)
      assert(getALong(1 day) === 1000L * 60 * 60 * 24 * 1000 * 1000)
      assert(getALong(2 days) === 2L * 1000 * 60 * 60 * 24 * 1000 * 1000)
      assert(getALong(1L millisecond) === 1L * 1000 * 1000)
      assert(getALong(2L milliseconds) === 2L * 1000 * 1000)
      assert(getALong(2L millis) === 2L * 1000 * 1000)
      assert(getALong(2L seconds) === 2L * 1000 * 1000 * 1000)
      assert(getALong(1L second) === 1000L * 1000 * 1000)
      assert(getALong(2L seconds) === 2L * 1000 * 1000 * 1000)
      assert(getALong(1L minute) === 1000L * 60 * 1000 * 1000)
      assert(getALong(2L minutes) === 2L * 1000 * 60 * 1000 * 1000)
      assert(getALong(1L hour) === 1000L * 60 * 60 * 1000 * 1000)
      assert(getALong(2L hours) === 2L * 1000 * 60 * 60 * 1000 * 1000)
      assert(getALong(1L day) === 1000L * 60 * 60 * 24 * 1000 * 1000)
      assert(getALong(2L days) === 2L * 1000 * 60 * 60 * 24 * 1000 * 1000)
    }
  }
  describe("A Span creating via SpanSugar") {

    it("should produce IAE if a negative length is passed") {
      for (f <- Seq((i: Long) => i nanosecond, (i: Long) => i nanoseconds, (i: Long) => i microsecond, (i: Long) => i microseconds,
        (i: Long) => i millisecond, (i: Long) => i milliseconds, (i: Long) => i millis, (i: Long) => i second, (i: Long) => i seconds,
        (i: Long) => i minute, (i: Long) => i minutes, (i: Long) => i hour, (i: Long) => i hours, (i: Long) => i day, (i: Long) => i days)) {
        for (i <- Seq(-1L, -2L, -3L, Long.MinValue)) {
          withClue("i was: " + i) {
            intercept[IllegalArgumentException] {
              f(i)
            }
          }
        }
      }
      for (f <- Seq((d: Double) => d nanosecond, (d: Double) => d nanoseconds, (d: Double) => d microsecond, (d: Double) => d microseconds,
        (d: Double) => d millisecond, (d: Double) => d milliseconds, (d: Double) => d millis, (d: Double) => d second, (d: Double) => d seconds,
        (d: Double) => d minute, (d: Double) => d minutes, (d: Double) => d hour, (d: Double) => d hours, (d: Double) => d day, (d: Double) => d days)) {
        for (d <- Seq(-1.0, -2.0, -3.0, -1.5, -9.98, Double.MinValue)) {
          withClue("d was: " + d) {
            intercept[IllegalArgumentException] {
              f(d)
            }
          }
        }
      }
    }

    it("should produce IAE if anything other than 1 is passed for singular units forms") {
      for (f <- Seq((i: Long) => i nanosecond, (i: Long) => i microsecond, (i: Long) => i millisecond, (i: Long) => i second,
        (i: Long) => i minute, (i: Long) => i hour, (i: Long) => i day)) {
        for (i <- Seq(0L, 2L, 3L, Long.MaxValue)) {
          withClue("i was: " + i) {
            intercept[IllegalArgumentException] {
              f(i)
            }
          }
        }
      }
      for (f <- Seq((d: Double) => d nanosecond, (d: Double) => d microsecond, (d: Double) => d millisecond,
        (d: Double) => d second, (d: Double) => d minute,(d: Double) => d hour,(d: Double) => d day)) {
        for (d <- Seq(0.0, 0.1, 1.1, 2.0, 9.98, Double.MaxValue)) {
          withClue("d was: " + d) {
            intercept[IllegalArgumentException] {
              f(d)
            }
          }
        }
      }
    }

    it("should construct with valid nanoseconds passed") {

      (0 nanoseconds) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1 nanosecond) should have (totalNanos(1), millisPart(0), nanosPart(1))
      (1 nanoseconds) should have (totalNanos(1), millisPart(0), nanosPart(1))
      (2 nanoseconds) should have (totalNanos(2), millisPart(0), nanosPart(2))
      (Long.MaxValue nanoseconds) should have (
        totalNanos(1000L * 1000 * 9223372036854L + 775807),
        millisPart(9223372036854L),
        nanosPart(775807)
      )

      (0.0 nanoseconds) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1.0 nanosecond) should have (totalNanos(1), millisPart(0), nanosPart(1))
      (1.0 nanoseconds) should have (totalNanos(1), millisPart(0), nanosPart(1))
      (2.0 nanoseconds) should have (totalNanos(2), millisPart(0), nanosPart(2))
      (0.1 nanoseconds) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1.1 nanoseconds) should have (totalNanos(1), millisPart(0), nanosPart(1))
      (1.2 nanoseconds) should have (totalNanos(1), millisPart(0), nanosPart(1))
      (1.499 nanoseconds) should have (totalNanos(1), millisPart(0), nanosPart(1))
      (1.5 nanoseconds) should have (totalNanos(1), millisPart(0), nanosPart(1))
      (1.9 nanoseconds) should have (totalNanos(1), millisPart(0), nanosPart(1))
      (2.2 nanoseconds) should have (totalNanos(2), millisPart(0), nanosPart(2))
      (Long.MaxValue.toDouble nanoseconds) should have (
        totalNanos(1000L * 1000 * 9223372036854L + 775807),
        millisPart(9223372036854L),
        nanosPart(775807)
      )
    }

    it("should throw IAE if a Double nanos value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue.toDouble
      for (d <- Seq(biggest + 10000, biggest + 20000, biggest + 30000, Double.MaxValue)) {
        withClue("d was: " + d) {
          val caught =
            intercept[IllegalArgumentException] {
              (d nanoseconds)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should construct with valid microseconds passed") {

      (0 microseconds) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1 microsecond) should have (totalNanos(1000), millisPart(0), nanosPart(1000))
      (1 microseconds) should have (totalNanos(1000), millisPart(0), nanosPart(1000))
      (2 microseconds) should have (totalNanos(2000), millisPart(0), nanosPart(2000))
      (1000 microseconds) should have (totalNanos(1000 * 1000), millisPart(1), nanosPart(0))
      (1001 microseconds) should have (totalNanos(1001L * 1000), millisPart(1), nanosPart(1000))
      (1002 microseconds) should have (totalNanos(1002L * 1000), millisPart(1), nanosPart(2000))
      (2000 microseconds) should have (totalNanos(2000 * 1000), millisPart(2), nanosPart(0))
      (2001 microseconds) should have (totalNanos(2001 * 1000), millisPart(2), nanosPart(1000))
      (2002 microseconds) should have (totalNanos(2002 * 1000), millisPart(2), nanosPart(2000))
      (Long.MaxValue / 1000 microseconds) should have (
        totalNanos(1000L * 1000 * 9223372036854L + 775000),
        millisPart(9223372036854L),
        nanosPart(775000)
      )

      (0.0 microseconds) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1.0 microsecond) should have (totalNanos(1000), millisPart(0), nanosPart(1000))
      (1.0 microseconds) should have (totalNanos(1000), millisPart(0), nanosPart(1000))
      (2.0 microseconds) should have (totalNanos(2000), millisPart(0), nanosPart(2000))
      (1000.0 microseconds) should have (totalNanos(1000L * 1000), millisPart(1), nanosPart(0))
      (1001.0 microseconds) should have (totalNanos(1001L * 1000), millisPart(1), nanosPart(1000))
      (1002.0 microseconds) should have (totalNanos(1002L * 1000), millisPart(1), nanosPart(2000))
      (2000.0 microseconds) should have (totalNanos(2000 * 1000), millisPart(2), nanosPart(0))
      (2001.0 microseconds) should have (totalNanos(2001 * 1000), millisPart(2), nanosPart(1000))
      (2002.0 microseconds) should have (totalNanos(2002 * 1000), millisPart(2), nanosPart(2000))
      (0.1 microseconds) should have (totalNanos(100), millisPart(0), nanosPart(100))
      (1.1 microseconds) should have (totalNanos(1100), millisPart(0), nanosPart(1100))
      (1.2 microseconds) should have (totalNanos(1200), millisPart(0), nanosPart(1200))
      (1.499 microseconds) should have (totalNanos(1499), millisPart(0), nanosPart(1499))
      (1.5 microseconds) should have (totalNanos(1500), millisPart(0), nanosPart(1500))
      (1.9 microseconds) should have (totalNanos(1900), millisPart(0), nanosPart(1900))
      (2.2 microseconds) should have (totalNanos(2200), millisPart(0), nanosPart(2200))
      ((Long.MaxValue / 1000).toDouble microseconds) should have (
        totalNanos(1000L * 1000 * 9223372036854L + 775807),
        millisPart(9223372036854L),
        nanosPart(775807)
      )
    }

    it("should throw IAE if a microseconds value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue / 1000
      for (i <- Seq(biggest + 1, biggest + 2, biggest + 3, Long.MaxValue)) {
        withClue("i was: " + i) {
          val caught =
            intercept[IllegalArgumentException] {
              (i microseconds)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should throw IAE if a Double microseconds value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue.toDouble / 1000
      for (d <- Seq(biggest + 10, biggest + 20, biggest + 30, Double.MaxValue)) {
        withClue("d was: " + d) {
          val caught =
            intercept[IllegalArgumentException] {
              (d microseconds)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should construct with valid milliseconds passed") {

      (0 milliseconds) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1 millisecond) should have (totalNanos(1000L * 1000), millisPart(1), nanosPart(0))
      (1 milliseconds) should have (totalNanos(1000L * 1000), millisPart(1), nanosPart(0))
      (2 milliseconds) should have (totalNanos(2 * 1000 * 1000), millisPart(2), nanosPart(0))
      (1000 milliseconds) should have (totalNanos(1000L * 1000 * 1000), millisPart(1000), nanosPart(0))
      (1001 milliseconds) should have (totalNanos(1001 * 1000 * 1000), millisPart(1001), nanosPart(0))
      (1002 milliseconds) should have (totalNanos(1002L * 1000 * 1000), millisPart(1002), nanosPart(0))
      (2000 milliseconds) should have (totalNanos(2000L * 1000 * 1000), millisPart(2000), nanosPart(0))
      (2001 milliseconds) should have (totalNanos(2001L * 1000 * 1000), millisPart(2001), nanosPart(0))
      (2002 milliseconds) should have (totalNanos(2002L * 1000 * 1000), millisPart(2002), nanosPart(0))
      (Long.MaxValue / 1000 / 1000 milliseconds) should have (
        totalNanos(1000L * 1000 * 9223372036854L),
        millisPart(9223372036854L),
        nanosPart(0)
      )

      (0.0 milliseconds) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1.0 millisecond) should have (totalNanos(1000L * 1000), millisPart(1), nanosPart(0))
      (1.0 milliseconds) should have (totalNanos(1000L * 1000), millisPart(1), nanosPart(0))
      (2.0 milliseconds) should have (totalNanos(2 * 1000 * 1000), millisPart(2), nanosPart(0))
      (1000.0 milliseconds) should have (totalNanos(1000L * 1000 * 1000), millisPart(1000), nanosPart(0))
      (1001.0 milliseconds) should have (totalNanos(1001 * 1000 * 1000), millisPart(1001), nanosPart(0))
      (1002.0 milliseconds) should have (totalNanos(1002L * 1000 * 1000), millisPart(1002), nanosPart(0))
      (2000.0 milliseconds) should have (totalNanos(2000L * 1000 * 1000), millisPart(2000), nanosPart(0))
      (2001.0 milliseconds) should have (totalNanos(2001L * 1000 * 1000), millisPart(2001), nanosPart(0))
      (2002.0 milliseconds) should have (totalNanos(2002L * 1000 * 1000), millisPart(2002), nanosPart(0))
      (0.1 milliseconds) should have (totalNanos(100L * 1000), millisPart(0), nanosPart(100000))
      (1.1 milliseconds) should have (totalNanos(1100L * 1000), millisPart(1), nanosPart(100000))
      (1.2 milliseconds) should have (totalNanos(1200L * 1000), millisPart(1), nanosPart(200000))
      (1.499 milliseconds) should have (totalNanos(1499L * 1000), millisPart(1), nanosPart(499000))
      (1.5 milliseconds) should have (totalNanos(1500L * 1000), millisPart(1), nanosPart(500000))
      (1.9 milliseconds) should have (totalNanos(1900L * 1000), millisPart(1), nanosPart(900000))
      (2.2 milliseconds) should have (totalNanos(2200 * 1000), millisPart(2), nanosPart(200000))
      (Long.MaxValue.toDouble / 1000 / 1000 milliseconds) should have (
        totalNanos(1000L * 1000 * 9223372036854L + 775807),
        millisPart(9223372036854L),
        nanosPart(775807)
      )
    }

    it("should throw IAE if a milliseconds value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue / 1000 / 1000
      for (i <- Seq(biggest + 1, biggest + 2, biggest + 3, Long.MaxValue)) {
        withClue("i was: " + i) {
          val caught =
            intercept[IllegalArgumentException] {
              (i milliseconds)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should throw IAE if a Double milliseconds value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue.toDouble / 1000 / 1000
      for (d <- Seq(biggest + 1, biggest + 2, biggest + 3, Double.MaxValue)) {
        withClue("d was: " + d) {
          val caught =
            intercept[IllegalArgumentException] {
              (d milliseconds)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should construct with valid milliseconds passed when used with the shorthand millis") {

      (0 millis) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1 millis) should have (totalNanos(1000L * 1000), millisPart(1), nanosPart(0))
      (1 millis) should have (totalNanos(1000L * 1000), millisPart(1), nanosPart(0))
      (2 millis) should have (totalNanos(2 * 1000 * 1000), millisPart(2), nanosPart(0))
      (1000 millis) should have (totalNanos(1000L * 1000 * 1000), millisPart(1000), nanosPart(0))
      (1001 millis) should have (totalNanos(1001 * 1000 * 1000), millisPart(1001), nanosPart(0))
      (1002 millis) should have (totalNanos(1002L * 1000 * 1000), millisPart(1002), nanosPart(0))
      (2000 millis) should have (totalNanos(2000L * 1000 * 1000), millisPart(2000), nanosPart(0))
      (2001 millis) should have (totalNanos(2001L * 1000 * 1000), millisPart(2001), nanosPart(0))
      (2002 millis) should have (totalNanos(2002L * 1000 * 1000), millisPart(2002), nanosPart(0))
      (Long.MaxValue / 1000 / 1000 millis) should have (
        totalNanos(1000L * 1000 * 9223372036854L),
        millisPart(9223372036854L),
        nanosPart(0)
      )

      (0.0 millis) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1.0 millisecond) should have (totalNanos(1000L * 1000), millisPart(1), nanosPart(0))
      (1.0 millis) should have (totalNanos(1000L * 1000), millisPart(1), nanosPart(0))
      (2.0 millis) should have (totalNanos(2 * 1000 * 1000), millisPart(2), nanosPart(0))
      (1000.0 millis) should have (totalNanos(1000L * 1000 * 1000), millisPart(1000), nanosPart(0))
      (1001.0 millis) should have (totalNanos(1001 * 1000 * 1000), millisPart(1001), nanosPart(0))
      (1002.0 millis) should have (totalNanos(1002L * 1000 * 1000), millisPart(1002), nanosPart(0))
      (2000.0 millis) should have (totalNanos(2000L * 1000 * 1000), millisPart(2000), nanosPart(0))
      (2001.0 millis) should have (totalNanos(2001L * 1000 * 1000), millisPart(2001), nanosPart(0))
      (2002.0 millis) should have (totalNanos(2002L * 1000 * 1000), millisPart(2002), nanosPart(0))
      (0.1 millis) should have (totalNanos(100L * 1000), millisPart(0), nanosPart(100000))
      (1.1 millis) should have (totalNanos(1100L * 1000), millisPart(1), nanosPart(100000))
      (1.2 millis) should have (totalNanos(1200L * 1000), millisPart(1), nanosPart(200000))
      (1.499 millis) should have (totalNanos(1499L * 1000), millisPart(1), nanosPart(499000))
      (1.5 millis) should have (totalNanos(1500L * 1000), millisPart(1), nanosPart(500000))
      (1.9 millis) should have (totalNanos(1900L * 1000), millisPart(1), nanosPart(900000))
      (2.2 millis) should have (totalNanos(2200 * 1000), millisPart(2), nanosPart(200000))
      (Long.MaxValue.toDouble / 1000 / 1000 millis) should have (
        totalNanos(1000L * 1000 * 9223372036854L + 775807),
        millisPart(9223372036854L),
        nanosPart(775807)
      )
    }

    it("should throw IAE if a milliseconds value larger than the largest expressible amount is passed when used with the shorthand millis.") {
      val biggest = Long.MaxValue / 1000 / 1000
      for (i <- Seq(biggest + 1, biggest + 2, biggest + 3, Long.MaxValue)) {
        withClue("i was: " + i) {
          val caught =
            intercept[IllegalArgumentException] {
              (i millis)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should throw IAE if a Double milliseconds value larger than the largest expressible amount is passed when used with the shorthand millis.") {
      val biggest = Long.MaxValue.toDouble / 1000 / 1000
      for (d <- Seq(biggest + 1, biggest + 2, biggest + 3, Double.MaxValue)) {
        withClue("d was: " + d) {
          val caught =
            intercept[IllegalArgumentException] {
              (d millis)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should construct with valid seconds passed") {

      (0 seconds) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1 second) should have (totalNanos(1000L * 1000 * 1000), millisPart(1000), nanosPart(0))
      (1 seconds) should have (totalNanos(1000L * 1000 * 1000), millisPart(1000), nanosPart(0))
      (2 seconds) should have (totalNanos(2000L * 1000 * 1000), millisPart(2000), nanosPart(0))
      (1000 seconds) should have (totalNanos(1000L * 1000 * 1000000), millisPart(1000 * 1000), nanosPart(0))
      (1001 seconds) should have (totalNanos(1000L * 1000 * 1001000), millisPart(1001 * 1000), nanosPart(0))
      (1002 seconds) should have (totalNanos(1000L * 1000 * 1002000), millisPart(1002 * 1000), nanosPart(0))
      (2000 seconds) should have (totalNanos(1000L * 1000 * 2000000), millisPart(2000 * 1000), nanosPart(0))
      (2001 seconds) should have (totalNanos(1000L * 1000 * 2001000), millisPart(2001 * 1000), nanosPart(0))
      (2002 seconds) should have (totalNanos(1000L * 1000 * 2002000), millisPart(2002 * 1000), nanosPart(0))
      (Long.MaxValue / 1000 / 1000 / 1000 seconds) should have (
        totalNanos(1000L * 1000 * 9223372036000L),
        millisPart(9223372036000L),
        nanosPart(0)
      )

      (0.0 seconds) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1.0 second) should have (totalNanos(1000L * 1000 * 1000), millisPart(1000), nanosPart(0))
      (1.0 seconds) should have (totalNanos(1000L * 1000 * 1000), millisPart(1000), nanosPart(0))
      (2.0 seconds) should have (totalNanos(2000L * 1000 * 1000), millisPart(2000), nanosPart(0))
      (1000.0 seconds) should have (totalNanos(1000L * 1000 * 1000000), millisPart(1000 * 1000), nanosPart(0))
      (1001.0 seconds) should have (totalNanos(1000L * 1000 * 1001000), millisPart(1001 * 1000), nanosPart(0))
      (1002.0 seconds) should have (totalNanos(1000L * 1000 * 1002000), millisPart(1002 * 1000), nanosPart(0))
      (2000.0 seconds) should have (totalNanos(1000L * 1000 * 2000000), millisPart(2000 * 1000), nanosPart(0))
      (2001.0 seconds) should have (totalNanos(1000L * 1000 * 2001000), millisPart(2001 * 1000), nanosPart(0))
      (2002.0 seconds) should have (totalNanos(1000L * 1000 * 2002000), millisPart(2002 * 1000), nanosPart(0))
      (0.1 seconds) should have (totalNanos(1000L * 1000 * 100), millisPart(100), nanosPart(0))
      (1.1 seconds) should have (totalNanos(1000L * 1000 * 1100), millisPart(1100), nanosPart(0))
      (1.2 seconds) should have (totalNanos(1000L * 1000 * 1200), millisPart(1200), nanosPart(0))
      (1.499 seconds) should have (totalNanos(1000L * 1000 * 1499), millisPart(1499), nanosPart(0))
      (1.5 seconds) should have (totalNanos(1000L * 1000 * 1500), millisPart(1500), nanosPart(0))
      (1.9 seconds) should have (totalNanos(1000L * 1000 * 1900), millisPart(1900), nanosPart(0))
      (2.2 seconds) should have (totalNanos(1000L * 1000 * 2200), millisPart(2200), nanosPart(0))
      (0.001 seconds) should have (totalNanos(1000L * 1000), millisPart(1), nanosPart(0))
      (88.0001 seconds) should have (totalNanos(1000L * 1000 * 88000 + 100000), millisPart(88 * 1000), nanosPart(100000))
      (88.000001 seconds) should have (totalNanos(1000L * 1000 * 88000 + 1000), millisPart(88 * 1000), nanosPart(1000))
      (88.000000001 seconds) should have (totalNanos(1000L * 1000 * 88000 + 1), millisPart(88 * 1000), nanosPart(1))
      (Long.MaxValue.toDouble / 1000 / 1000 / 1000 seconds) should have (
        totalNanos(1000L * 1000 * 9223372036854L + 775807),
        millisPart(9223372036854L),
        nanosPart(775807)
      )
    }

    it("should throw IAE if a seconds value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue / 1000 / 1000 / 1000
      for (i <- Seq(biggest + 1, biggest + 2, biggest + 3, Long.MaxValue)) {
        withClue("i was: " + i) {
          val caught =
            intercept[IllegalArgumentException] {
              (i seconds)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should throw IAE if a Double seconds value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue.toDouble / 1000 / 1000 / 1000
      for (d <- Seq(biggest + 1, biggest + 2, biggest + 3, Double.MaxValue)) {
        withClue("d was: " + d) {
          val caught =
            intercept[IllegalArgumentException] {
              (d seconds)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should construct with valid minutes passed") {

      (0 minutes) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1 minute) should have (totalNanos(1000L * 1000 * 1000 * 60), millisPart(60 * 1000), nanosPart(0))
      (1 minutes) should have (totalNanos(1000L * 1000 * 1000 * 60), millisPart(60 * 1000), nanosPart(0))
      (2 minutes) should have (totalNanos(1000L * 1000 * 2 * 1000 * 60), millisPart(2 * 60 * 1000), nanosPart(0))
      (1000 minutes) should have (totalNanos(1000L * 1000 * 1000 * 1000 * 60), millisPart(1000 * 60 * 1000), nanosPart(0))
      (1001 minutes) should have (totalNanos(1000L * 1000 * 1001 * 1000 * 60), millisPart(1001 * 60 * 1000), nanosPart(0))
      (1002 minutes) should have (totalNanos(1000L * 1000 * 1002 * 1000 * 60), millisPart(1002 * 60 * 1000), nanosPart(0))
      (2000 minutes) should have (totalNanos(1000L * 1000 * 2000 * 1000 * 60), millisPart(2000 * 60 * 1000), nanosPart(0))
      (2001 minutes) should have (totalNanos(1000L * 1000 * 2001 * 1000 * 60), millisPart(2001 * 60 * 1000), nanosPart(0))
      (2002 minutes) should have (totalNanos(1000L * 1000 * 2002 * 1000 * 60), millisPart(2002 * 60 * 1000), nanosPart(0))
      (Long.MaxValue / 1000 / 1000 / 1000 / 60 minutes) should have (
        totalNanos(1000L * 1000 * 9223372020000L),
        millisPart(9223372020000L),
        nanosPart(0)
      )

      (0.0 minutes) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1.0 minute) should have (totalNanos(1000L * 1000 * 1000 * 60), millisPart(60 * 1000), nanosPart(0))
      (1.0 minutes) should have (totalNanos(1000L * 1000 * 1000 * 60), millisPart(60 * 1000), nanosPart(0))
      (2.0 minutes) should have (totalNanos(1000L * 1000 * 2 * 1000 * 60), millisPart(2 * 60 * 1000), nanosPart(0))
      (1000.0 minutes) should have (totalNanos(1000L * 1000 * 1000 * 1000 * 60), millisPart(1000 * 60 * 1000), nanosPart(0))
      (1001.0 minutes) should have (totalNanos(1000L * 1000 * 1001 * 1000 * 60), millisPart(1001 * 60 * 1000), nanosPart(0))
      (1002.0 minutes) should have (totalNanos(1000L * 1000 * 1002 * 1000 * 60), millisPart(1002 * 60 * 1000), nanosPart(0))
      (2000.0 minutes) should have (totalNanos(1000L * 1000 * 2000 * 1000 * 60), millisPart(2000 * 60 * 1000), nanosPart(0))
      (2001.0 minutes) should have (totalNanos(1000L * 1000 * 2001 * 1000 * 60), millisPart(2001 * 60 * 1000), nanosPart(0))
      (2002.0 minutes) should have (totalNanos(1000L * 1000 * 2002 * 1000 * 60), millisPart(2002 * 60 * 1000), nanosPart(0))
      (0.1 minutes) should have (totalNanos(1000L * 1000 * 100 * 60), millisPart(100 * 60), nanosPart(0))
      (1.1 minutes) should have (totalNanos(1000L * 1000 * 1100 * 60), millisPart(1100 * 60), nanosPart(0))
      (1.2 minutes) should have (totalNanos(1000L * 1000 * 1200 * 60), millisPart(1200 * 60), nanosPart(0))
      (1.499 minutes) should have (totalNanos(1000L * 1000 * 1499 * 60), millisPart(1499 * 60), nanosPart(0))
      (1.5 minutes) should have (totalNanos(1000L * 1000 * 1500 * 60), millisPart(1500 * 60), nanosPart(0))
      (1.9 minutes) should have (totalNanos(1000L * 1000 * 1900 * 60), millisPart(1900 * 60), nanosPart(0))
      (2.2 minutes) should have (totalNanos(1000L * 1000 * 2200 * 60), millisPart(2200 * 60), nanosPart(0))
      (0.001 minutes) should have (totalNanos(1000L * 1000 * 60), millisPart(60), nanosPart(0))
      (88.0001 minutes) should have (totalNanos(1000L * 1000 * 5280006), millisPart(88 * 1000 * 60 + 6), nanosPart(0))
      (88.000001 minutes) should have (totalNanos(1000L * 1000 * 5280000 + 60000), millisPart(88 * 1000 * 60), nanosPart(60000))
      (88.000000001 minutes) should have (totalNanos(1000L * 1000 * 5280000 + 60), millisPart(88 * 1000 * 60), nanosPart(60))
      (Long.MaxValue.toDouble / 1000 / 1000 / 1000 / 60 minutes) should have (
        totalNanos(1000L * 1000 * 9223372036854L + 773760),
        millisPart(9223372036854L),
        nanosPart(773760)
      )
    }

    it("should throw IAE if a minutes value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue / 1000 / 1000 / 1000 / 60
      for (i <- Seq(biggest + 1, biggest + 2, biggest + 3, Long.MaxValue)) {
        withClue("i was: " + i) {
          val caught =
            intercept[IllegalArgumentException] {
              (i minutes)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should throw IAE if a Double minutes value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue.toDouble / 1000 / 1000 / 1000 / 60
      for (d <- Seq(biggest + 1, biggest + 2, biggest + 3, Double.MaxValue)) {
        withClue("d was: " + d) {
          val caught =
            intercept[IllegalArgumentException] {
              (d minutes)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should construct with valid hours passed") {

      (0 hours) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1 hour) should have (totalNanos(1000L * 1000 * 1000 * 60 * 60), millisPart(60 * 60 * 1000), nanosPart(0))
      (1 hours) should have (totalNanos(1000L * 1000 * 1000 * 60 * 60), millisPart(60 * 60 * 1000), nanosPart(0))
      (2 hours) should have (totalNanos(1000L * 1000 * 2 * 1000 * 60 * 60), millisPart(2 * 60 * 60 * 1000), nanosPart(0))
      (1000 hours) should have (totalNanos(1000L * 1000 * 1000L * 1000 * 60 * 60), millisPart(1000L * 60 * 60 * 1000), nanosPart(0))
      (1001 hours) should have (totalNanos(1000L * 1000 * 1001L * 1000 * 60 * 60), millisPart(1001L * 60 * 60 * 1000), nanosPart(0))
      (1002 hours) should have (totalNanos(1000L * 1000 * 1002L * 1000 * 60 * 60), millisPart(1002L * 60 * 60 * 1000), nanosPart(0))
      (2000 hours) should have (totalNanos(1000L * 1000 * 2000L * 1000 * 60 * 60), millisPart(2000L * 60 * 60 * 1000), nanosPart(0))
      (2001 hours) should have (totalNanos(1000L * 1000 * 2001L * 1000 * 60 * 60), millisPart(2001L * 60 * 60 * 1000), nanosPart(0))
      (2002 hours) should have (totalNanos(1000L * 1000 * 2002L * 1000 * 60 * 60), millisPart(2002L * 60 * 60 * 1000), nanosPart(0))
      (Long.MaxValue / 1000 / 1000 / 1000 / 60 / 60 hours) should have (
        totalNanos(1000L * 1000 * 9223369200000L),
        millisPart(9223369200000L),
        nanosPart(0)
      )

      (0.0 hours) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1.0 hour) should have (totalNanos(1000L * 1000 * 1000 * 60 * 60), millisPart(60 * 60 * 1000), nanosPart(0))
      (1.0 hours) should have (totalNanos(1000L * 1000 * 1000 * 60 * 60), millisPart(60 * 60 * 1000), nanosPart(0))
      (2.0 hours) should have (totalNanos(1000L * 1000 * 2 * 1000 * 60 * 60), millisPart(2 * 60 * 60 * 1000), nanosPart(0))
      (1000.0 hours) should have (totalNanos(1000L * 1000 * 1000L * 1000 * 60 * 60), millisPart(1000L * 60 * 60 * 1000), nanosPart(0))
      (1001.0 hours) should have (totalNanos(1000L * 1000 * 1001L * 1000 * 60 * 60), millisPart(1001L * 60 * 60 * 1000), nanosPart(0))
      (1002.0 hours) should have (totalNanos(1000L * 1000 * 1002L * 1000 * 60 * 60), millisPart(1002L * 60 * 60 * 1000), nanosPart(0))
      (2000.0 hours) should have (totalNanos(1000L * 1000 * 2000L * 1000 * 60 * 60), millisPart(2000L * 60 * 60 * 1000), nanosPart(0))
      (2001.0 hours) should have (totalNanos(1000L * 1000 * 2001L * 1000 * 60 * 60), millisPart(2001L * 60 * 60 * 1000), nanosPart(0))
      (2002.0 hours) should have (totalNanos(1000L * 1000 * 2002L * 1000 * 60 * 60), millisPart(2002L * 60 * 60 * 1000), nanosPart(0))
      (0.1 hours) should have (totalNanos(1000L * 1000 * 100 * 60 * 60), millisPart(100 * 60 * 60), nanosPart(0))
      (1.1 hours) should have (totalNanos(1000L * 1000 * 1100 * 60 * 60), millisPart(1100 * 60 * 60), nanosPart(0))
      (1.2 hours) should have (totalNanos(1000L * 1000 * 1200 * 60 * 60), millisPart(1200 * 60 * 60), nanosPart(0))
      (1.499 hours) should have (totalNanos(1000L * 1000 * 1499 * 60 * 60), millisPart(1499 * 60 * 60), nanosPart(0))
      (1.5 hours) should have (totalNanos(1000L * 1000 * 1500 * 60 * 60), millisPart(1500 * 60 * 60), nanosPart(0))
      (1.9 hours) should have (totalNanos(1000L * 1000 * 1900 * 60 * 60), millisPart(1900 * 60 * 60), nanosPart(0))
      (2.2 hours) should have (totalNanos(1000L * 1000 * 2200 * 60 * 60), millisPart(2200 * 60 * 60), nanosPart(0))
      (0.001 hours) should have (totalNanos(1000L * 1000 * 60 * 60), millisPart(60 * 60), nanosPart(0))
      (88.0001 hours) should have (totalNanos(1000L * 1000 * 5280006 * 60), millisPart(88 * 1000 * 60 * 60 + 6 * 60), nanosPart(0))
      (88.000001 hours) should have (totalNanos(1000L * 1000 * 316800003 + 600000), millisPart(88 * 1000 * 60 * 60 + 3), nanosPart(600000))
      (88.000000001 hours) should have (totalNanos(1000L * 1000 * 5280000 * 60 + 3600), millisPart(88 * 1000 * 60 * 60), nanosPart(3600))
      (Long.MaxValue.toDouble / 1000 / 1000 / 1000 / 60 / 60 hours) should have (
        totalNanos(1000L * 1000 * 9223372036854L + 775807),
        millisPart(9223372036854L),
        nanosPart(775807)
      )
    }

    it("should throw IAE if an hours value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue / 1000 / 1000 / 1000 / 60 / 60
      for (i <- Seq(biggest + 1, biggest + 2, biggest + 3, Long.MaxValue)) {
        withClue("i was: " + i) {
          val caught =
            intercept[IllegalArgumentException] {
              (i hours)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should throw IAE if a Double hours value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue.toDouble / 1000 / 1000 / 1000 / 60 / 60
      for (d <- Seq(biggest + 1, biggest + 2, biggest + 3, Double.MaxValue)) {
        withClue("d was: " + d) {
          val caught =
            intercept[IllegalArgumentException] {
              (d hours)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should construct with valid days passed") {

      (0 days) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1 day) should have (totalNanos(1000L * 1000 * 1000 * 60 * 60 * 24), millisPart(60 * 60 * 24 * 1000), nanosPart(0))
      (1 days) should have (totalNanos(1000L * 1000 * 1000 * 60 * 60 * 24), millisPart(60 * 60 * 24 * 1000), nanosPart(0))
      (2 days) should have (totalNanos(1000L * 1000 * 2 * 1000 * 60 * 60 * 24), millisPart(2 * 60 * 60 * 24 * 1000), nanosPart(0))
      (1000 days) should have (totalNanos(1000L * 1000 * 1000L * 1000 * 60 * 60 * 24), millisPart(1000L * 60 * 60 * 24 * 1000), nanosPart(0))
      (1001 days) should have (totalNanos(1000L * 1000 * 1001L * 1000 * 60 * 60 * 24), millisPart(1001L * 60 * 60 * 24 * 1000), nanosPart(0))
      (1002 days) should have (totalNanos(1000L * 1000 * 1002L * 1000 * 60 * 60 * 24), millisPart(1002L * 60 * 60 * 24 * 1000), nanosPart(0))
      (2000 days) should have (totalNanos(1000L * 1000 * 2000L * 1000 * 60 * 60 * 24), millisPart(2000L * 60 * 60 * 24 * 1000), nanosPart(0))
      (2001 days) should have (totalNanos(1000L * 1000 * 2001L * 1000 * 60 * 60 * 24), millisPart(2001L * 60 * 60 * 24 * 1000), nanosPart(0))
      (2002 days) should have (totalNanos(1000L * 1000 * 2002L * 1000 * 60 * 60 * 24), millisPart(2002L * 60 * 60 * 24 * 1000), nanosPart(0))
      (Long.MaxValue / 1000 / 1000 / 1000 / 60 / 60 / 24 days) should have (
        totalNanos(1000L * 1000 * 9223286400000L),
        millisPart(9223286400000L),
        nanosPart(0)
      )

      (0.0 days) should have (totalNanos(0), millisPart(0), nanosPart(0))
      (1.0 day) should have (totalNanos(1000L * 1000 * 1000 * 60 * 60 * 24), millisPart(60 * 60 * 24 * 1000), nanosPart(0))
      (1.0 days) should have (totalNanos(1000L * 1000 * 1000 * 60 * 60 * 24), millisPart(60 * 60 * 24 * 1000), nanosPart(0))
      (2.0 days) should have (totalNanos(1000L * 1000 * 2 * 1000 * 60 * 60 * 24), millisPart(2 * 60 * 60 * 24 * 1000), nanosPart(0))
      (1000.0 days) should have (totalNanos(1000L * 1000 * 1000L * 1000 * 60 * 60 * 24), millisPart(1000L * 60 * 60 * 24 * 1000), nanosPart(0))
      (1001.0 days) should have (totalNanos(1000L * 1000 * 1001L * 1000 * 60 * 60 * 24), millisPart(1001L * 60 * 60 * 24 * 1000), nanosPart(0))
      (1002.0 days) should have (totalNanos(1000L * 1000 * 1002L * 1000 * 60 * 60 * 24), millisPart(1002L * 60 * 60 * 24 * 1000), nanosPart(0))
      (2000.0 days) should have (totalNanos(1000L * 1000 * 2000L * 1000 * 60 * 60 * 24), millisPart(2000L * 60 * 60 * 24 * 1000), nanosPart(0))
      (2001.0 days) should have (totalNanos(1000L * 1000 * 2001L * 1000 * 60 * 60 * 24), millisPart(2001L * 60 * 60 * 24 * 1000), nanosPart(0))
      (2002.0 days) should have (totalNanos(1000L * 1000 * 2002L * 1000 * 60 * 60 * 24), millisPart(2002L * 60 * 60 * 24 * 1000), nanosPart(0))
      (0.1 days) should have (totalNanos(1000L * 1000 * 100 * 60 * 60 * 24), millisPart(100 * 60 * 60 * 24), nanosPart(0))
      (1.1 days) should have (totalNanos(1000L * 1000 * 1100 * 60 * 60 * 24), millisPart(1100 * 60 * 60 * 24), nanosPart(0))
      (1.2 days) should have (totalNanos(1000L * 1000 * 1200 * 60 * 60 * 24), millisPart(1200 * 60 * 60 * 24), nanosPart(0))
      (1.499 days) should have (totalNanos(1000L * 1000 * 1499 * 60 * 60 * 24), millisPart(1499 * 60 * 60 * 24), nanosPart(0))
      (1.5 days) should have (totalNanos(1000L * 1000 * 1500 * 60 * 60 * 24), millisPart(1500 * 60 * 60 * 24), nanosPart(0))
      (1.9 days) should have (totalNanos(1000L * 1000 * 1900 * 60 * 60 * 24), millisPart(1900 * 60 * 60 * 24), nanosPart(0))
      (2.2 days) should have (totalNanos(1000L * 1000 * 2200 * 60 * 60 * 24), millisPart(2200 * 60 * 60 * 24), nanosPart(0))
      (0.001 days) should have (totalNanos(1000L * 1000 * 60 * 60 * 24), millisPart(60 * 60 * 24), nanosPart(0))
      (88.0001 days) should have (totalNanos(1000L * 1000 * 5280006L * 60 * 24), millisPart(88L * 1000 * 60 * 60 * 24 + 6 * 60 * 24), nanosPart(0))
      (88.000001 days) should have (totalNanos(1000L * 1000 * 7603200086L + 400000), millisPart(88L * 1000 * 60 * 60 * 24 + 86), nanosPart(400000))
      (88.000000001 days) should have (totalNanos(1000L * 1000 * 5280000L * 60 * 24 + 86400), millisPart(88L * 1000 * 60 * 60 * 24), nanosPart(86400))
      (Long.MaxValue.toDouble / 1000 / 1000 / 1000 / 60 / 60 / 24 days) should have (
        totalNanos(1000L * 1000 * 9223372036854L + 775807),
        millisPart(9223372036854L),
        nanosPart(775807)
      )
    }

    it("should throw IAE if a days value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue / 1000 / 1000 / 1000 / 60 / 60 / 24
      for (i <- Seq(biggest + 1, biggest + 2, biggest + 3, Long.MaxValue)) {
        withClue("i was: " + i) {
          val caught =
            intercept[IllegalArgumentException] {
              (i days)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should throw IAE if a Double days value larger than the largest expressible amount is passed.") {
      val biggest = Long.MaxValue.toDouble / 1000 / 1000 / 1000 / 60 / 60 / 24
      for (d <- Seq(biggest + 1, biggest + 2, biggest + 3, Double.MaxValue)) {
        withClue("d was: " + d) {
          val caught =
            intercept[IllegalArgumentException] {
              (d days)
            }
          caught.getMessage should include ("Passed length")
        }
      }
    }

    it("should give pretty, localized toStrings reflecting what went in") {
      (1 nanosecond).prettyString should be ("1 nanosecond")
      (1 nanoseconds).prettyString should be ("1 nanosecond")
      (2 nanoseconds).prettyString should be ("2 nanoseconds")
      (1.0 nanosecond).prettyString should be ("1.0 nanosecond")
      (1.0 nanoseconds).prettyString should be ("1.0 nanosecond")
      (1.1 nanoseconds).prettyString should be ("1.1 nanoseconds")
      (2.0 nanoseconds).prettyString should be ("2.0 nanoseconds")

      (1 microsecond).prettyString should be ("1 microsecond")
      (1 microseconds).prettyString should be ("1 microsecond")
      (2 microseconds).prettyString should be ("2 microseconds")
      (1.0 microsecond).prettyString should be ("1.0 microsecond")
      (1.0 microseconds).prettyString should be ("1.0 microsecond")
      (1.1 microseconds).prettyString should be ("1.1 microseconds")
      (2.0 microseconds).prettyString should be ("2.0 microseconds")

      (1 millisecond).prettyString should be ("1 millisecond")
      (1 milliseconds).prettyString should be ("1 millisecond")
      (2 milliseconds).prettyString should be ("2 milliseconds")
      (1.0 millisecond).prettyString should be ("1.0 millisecond")
      (1.0 milliseconds).prettyString should be ("1.0 millisecond")
      (1.1 milliseconds).prettyString should be ("1.1 milliseconds")
      (2.0 milliseconds).prettyString should be ("2.0 milliseconds")

      (1 millis).prettyString should be ("1 millisecond")
      (2 millis).prettyString should be ("2 milliseconds")
      (1.0 millis).prettyString should be ("1.0 millisecond")
      (1.1 millis).prettyString should be ("1.1 milliseconds")
      (2.0 millis).prettyString should be ("2.0 milliseconds")

      (1 second).prettyString should be ("1 second")
      (1 seconds).prettyString should be ("1 second")
      (2 seconds).prettyString should be ("2 seconds")
      (1.0 second).prettyString should be ("1.0 second")
      (1.0 seconds).prettyString should be ("1.0 second")
      (1.1 seconds).prettyString should be ("1.1 seconds")
      (2.0 seconds).prettyString should be ("2.0 seconds")

      (1 minute).prettyString should be ("1 minute")
      (1 minutes).prettyString should be ("1 minute")
      (2 minutes).prettyString should be ("2 minutes")
      (1.0 minute).prettyString should be ("1.0 minute")
      (1.0 minutes).prettyString should be ("1.0 minute")
      (1.1 minutes).prettyString should be ("1.1 minutes")
      (2.0 minutes).prettyString should be ("2.0 minutes")

      (1 hour).prettyString should be ("1 hour")
      (1 hours).prettyString should be ("1 hour")
      (2 hours).prettyString should be ("2 hours")
      (1.0 hour).prettyString should be ("1.0 hour")
      (1.0 hours).prettyString should be ("1.0 hour")
      (1.1 hours).prettyString should be ("1.1 hours")
      (2.0 hours).prettyString should be ("2.0 hours")

      (1 day).prettyString should be ("1 day")
      (1 days).prettyString should be ("1 day")
      (2 days).prettyString should be ("2 days")
      (1.0 day).prettyString should be ("1.0 day")
      (1.0 days).prettyString should be ("1.0 day")
      (1.1 days).prettyString should be ("1.1 days")
      (2.0 days).prettyString should be ("2.0 days")
    }

    it("should have a pretty toString") {
      (1 nanosecond).toString should be ("Span(1, Nanosecond)")
      (1 nanoseconds).toString should be ("Span(1, Nanoseconds)")
      (2 nanoseconds).toString should be ("Span(2, Nanoseconds)")
      (1.0 nanosecond).toString should be ("Span(1.0, Nanosecond)")
      (1.0 nanoseconds).toString should be ("Span(1.0, Nanoseconds)")
      (1.1 nanoseconds).toString should be ("Span(1.1, Nanoseconds)")
      (2.0 nanoseconds).toString should be ("Span(2.0, Nanoseconds)")

      (1 microsecond).toString should be ("Span(1, Microsecond)")
      (1 microseconds).toString should be ("Span(1, Microseconds)")
      (2 microseconds).toString should be ("Span(2, Microseconds)")
      (1.0 microsecond).toString should be ("Span(1.0, Microsecond)")
      (1.0 microseconds).toString should be ("Span(1.0, Microseconds)")
      (1.1 microseconds).toString should be ("Span(1.1, Microseconds)")
      (2.0 microseconds).toString should be ("Span(2.0, Microseconds)")

      (1 millisecond).toString should be ("Span(1, Millisecond)")
      (1 milliseconds).toString should be ("Span(1, Milliseconds)")
      (2 milliseconds).toString should be ("Span(2, Milliseconds)")
      (1.0 millisecond).toString should be ("Span(1.0, Millisecond)")
      (1.0 milliseconds).toString should be ("Span(1.0, Milliseconds)")
      (1.1 milliseconds).toString should be ("Span(1.1, Milliseconds)")
      (2.0 milliseconds).toString should be ("Span(2.0, Milliseconds)")

      (1 millis).toString should be ("Span(1, Millis)")
      (2 millis).toString should be ("Span(2, Millis)")
      (1.0 millis).toString should be ("Span(1.0, Millis)")
      (1.1 millis).toString should be ("Span(1.1, Millis)")
      (2.0 millis).toString should be ("Span(2.0, Millis)")

      (1 second).toString should be ("Span(1, Second)")
      (1 seconds).toString should be ("Span(1, Seconds)")
      (2 seconds).toString should be ("Span(2, Seconds)")
      (1.0 second).toString should be ("Span(1.0, Second)")
      (1.0 seconds).toString should be ("Span(1.0, Seconds)")
      (1.1 seconds).toString should be ("Span(1.1, Seconds)")
      (2.0 seconds).toString should be ("Span(2.0, Seconds)")

      (1 minute).toString should be ("Span(1, Minute)")
      (1 minutes).toString should be ("Span(1, Minutes)")
      (2 minutes).toString should be ("Span(2, Minutes)")
      (1.0 minute).toString should be ("Span(1.0, Minute)")
      (1.0 minutes).toString should be ("Span(1.0, Minutes)")
      (1.1 minutes).toString should be ("Span(1.1, Minutes)")
      (2.0 minutes).toString should be ("Span(2.0, Minutes)")

      (1 hour).toString should be ("Span(1, Hour)")
      (1 hours).toString should be ("Span(1, Hours)")
      (2 hours).toString should be ("Span(2, Hours)")
      (1.0 hour).toString should be ("Span(1.0, Hour)")
      (1.0 hours).toString should be ("Span(1.0, Hours)")
      (1.1 hours).toString should be ("Span(1.1, Hours)")
      (2.0 hours).toString should be ("Span(2.0, Hours)")

      (1 day).toString should be ("Span(1, Day)")
      (1 days).toString should be ("Span(1, Days)")
      (2 days).toString should be ("Span(2, Days)")
      (1.0 day).toString should be ("Span(1.0, Day)")
      (1.0 days).toString should be ("Span(1.0, Days)")
      (1.1 days).toString should be ("Span(1.1, Days)")
      (2.0 days).toString should be ("Span(2.0, Days)")
    }
  }
}
