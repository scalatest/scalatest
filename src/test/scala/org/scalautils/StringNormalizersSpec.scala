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
package org.scalautils

import org.scalatest._

class StringNormalizationsSpec extends Spec with StringNormalizations {

  object `StringNormalizations ` {
    def `should provide a way to normalize a string by making it lower case` { 
      assert(lowerCased.normalized("Hello") === "hello")
      assert(lowerCased.normalized("hello") === "hello")
      assert(lowerCased.normalized("") === "")
      assert(lowerCased.normalized("HELLO") === "hello")
    }
    def `should provide a way to normalize a string by trimming it` { 
      assert(trimmed.normalized("hello") === "hello")
      assert(trimmed.normalized(" hello") === "hello")
      assert(trimmed.normalized("hello ") === "hello")
      assert(trimmed.normalized("\nhello") === "hello")
      assert(trimmed.normalized("hello\n") === "hello")
      assert(trimmed.normalized("\n") === "")
      assert(trimmed.normalized("  ") === "")
      assert(trimmed.normalized("") === "")
    }
  }
}

