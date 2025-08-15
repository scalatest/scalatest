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
package org.scalatest.examples.freespec.tagging

import org.scalatest.Tag

object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FreeSpec
import org.scalatest.tagobjects.Slow

class SetSpec extends FreeSpec {

  "A Set" - {
    "when empty" - {
      "should have size 0" taggedAs(Slow) in {
        assert(Set.empty.size === 0)
      }
      
      "should produce NoSuchElementException when head is invoked" taggedAs(Slow, DbTest) in {
        assertThrows[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}
