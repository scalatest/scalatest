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
package org.scalatest.time

import org.scalatest._
import scala.concurrent.duration._

class DurationSpec extends Spec with Matchers {

  def span(passed: Span): Span = passed
  def duration(passed: Duration): Duration = passed

  object `A Span` {
    def `can be specified with a finite scala.concurrent.Duration via an implicit conversion` {
      span(100 millis) shouldEqual Span(100, Millis)
      span(100 nanos) shouldEqual Span(100, Nanoseconds)
    }
    def `can be specified with an infinite scala.concurrent.Duration via an implicit conversion` {
      span(Duration.Inf) shouldEqual Span.Max
      span(Duration.MinusInf) shouldEqual Span.Zero
    }
    def `can be specified with an undefined scala.concurrent.Duration via an implicit conversion` {
      span(Duration.Undefined) shouldEqual Span.Max
    }
  }
  object `A Duration` {
    def `can be specified with a Span via an implicit conversion` {
      duration(Span(100, Millis)) shouldEqual (100 millis)
      duration(Span(100, Nanoseconds)) shouldEqual (100 nanos)
    }
  }
}

