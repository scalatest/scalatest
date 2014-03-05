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

class SnapshotsSpec extends Spec {

  object `Snapshot ` {

    def `should have a nice toString` {
      val snapshot = Snapshot("aName", "ScalaTest")
      assert(snapshot.toString == "aName = \"ScalaTest\"")
    }
  }

  object `Snapshots should ` {

    def `snap variable names and values and put them in IndexedSeq of Snapshot` {

      val a = 3
      val b = 4
      val snapshots = Snapshots.snap(a, b, a + b)
      assert(snapshots.size == 3)
      assert(snapshots(0) == Snapshot("a", 3))
      assert(snapshots(1) == Snapshot("b", 4))
      assert(snapshots(2) == Snapshot("a.+(b)", 7))
      assert(snapshots.toString == "a = 3, b = 4, a.+(b) = 7")
    }

    def `snap variable names and values and user can transform them using map` {
      val a = 3
      val b = 4
      val snapshots = Snapshots.snap(a, b)
      val result = snapshots.map(v => v.name + " was equal, not surprisingly, to " + v.value).mkString("\n")
      assert(result == "a was equal, not surprisingly, to 3\n" +
                       "b was equal, not surprisingly, to 4")
    }

    def `allow people to call .lines to get multiple line output` {
      val a = 3
      val b = 4
      val snapshots = Snapshots.snap(a, b, a + b)
      assert(snapshots.lines == "a = 3\nb = 4\na.+(b) = 7")
    }
  }

}

