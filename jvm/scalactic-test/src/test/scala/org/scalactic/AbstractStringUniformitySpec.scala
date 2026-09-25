/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalactic

class AbstractStringUniformitySpec extends UnitSpec {

  val capitalized: Uniformity[String] =
    new AbstractStringUniformity {
      def normalized(s: String): String =
        if (s.isEmpty) "" else s.charAt(0).toUpper + s.substring(1)
    }

  "AbstractStringUniformity" should "return true from normalizedCanHandle for Strings" in {
    assert(capitalized.normalizedCanHandle("hello"))
    assert(capitalized.normalizedCanHandle(""))
  }

  it should "return false from normalizedCanHandle for non-Strings" in {
    assert(!capitalized.normalizedCanHandle(3))
    assert(!capitalized.normalizedCanHandle(null))
    assert(!capitalized.normalizedCanHandle('c'))
  }

  it should "normalize Strings via normalizedOrSame" in {
    assert(capitalized.normalizedOrSame("hello") === "Hello")
    assert(capitalized.normalizedOrSame("") === "")
  }

  it should "return the same object from normalizedOrSame for non-Strings" in {
    val three = 3
    assert(capitalized.normalizedOrSame(three) === three)
    assert(capitalized.normalizedOrSame(null) === null)
    assert(capitalized.normalizedOrSame('c') === 'c')
  }
}