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
package org.scalatest.concurrent

import org.scalatest.UnitSpec

import PatienceConfiguration._

class PatienceConfigurationSpec extends UnitSpec {
  object `An Interval` {
    def `should throw NPE if null is passed` = {
      a [NullPointerException] should be thrownBy { new Interval(null) }
    }
  }
  object `A Timeout` {
    def `should throw NPE if null is passed` = {
      a [NullPointerException] should be thrownBy { new Timeout(null) }
    }
  }
}
