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
package org.scalatest.examples.propspec.note

import org.scalatest._
import prop._
import collection.mutable

class SetSpec extends PropSpec with TableDrivenPropertyChecks {

  val examples =
    Table(
      "set",
      mutable.BitSet.empty,
      mutable.HashSet.empty[Int],
      mutable.LinkedHashSet.empty[Int]
    )

  property("an element can be added to an empty mutable Set") {

    info("info is recorded")
    markup("markup is *also* recorded")
    note("notes are sent immediately")
    alert("alerts are also sent immediately")

    forAll(examples) { set =>

      assert(set.isEmpty)
      set += 99
      assert(set.size === 1)
      assert(set.contains(99))
    }
  }
}
