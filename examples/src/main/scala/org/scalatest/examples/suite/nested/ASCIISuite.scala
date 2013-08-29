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
package org.scalatest.examples.suite.nested

import org.scalatest._

class ASuite extends Suite {
  def `test: A should have ASCII value 41 hex` {
    assert('A' === 0x41)
  }
  def `test: a should have ASCII value 61 hex` {
    assert('a' === 0x61)
  }
}
class BSuite extends Suite {
  def `test: B should have ASCII value 42 hex` {
    assert('B' === 0x42)
  }
  def `test: b should have ASCII value 62 hex` {
    assert('b' === 0x62)
  }
}
class CSuite extends Suite {
  def `test: C should have ASCII value 43 hex` {
    assert('C' === 0x43)
  }
  def `test: c should have ASCII value 63 hex` {
    assert('c' === 0x63)
  }
}

class ASCIISuite extends Suites(
  new ASuite,
  new BSuite,
  new CSuite
)
