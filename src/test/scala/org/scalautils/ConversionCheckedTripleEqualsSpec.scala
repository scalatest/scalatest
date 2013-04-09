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
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce

// TODO: Write a test with both A => B and B => A in scope and make sure it
// still compiles. Should because should pick the higher priority one in each case.
class ConversionCheckedTripleEqualsSpec extends Spec with ConversionCheckedTripleEquals {

  object `the ConversionCheckedTripleEquals trait` {

    case class Box[T](value: T)

    def `should automatically unbox an object on the left` {

      implicit def unbox[T](box: Box[T]): T = box.value

      assert(Box(1) === 1)
      assert(Box("s") === "s")
    }

    def `should automatically unbox an object on the right` {

      implicit def unbox[T](box: Box[T]): T = box.value

      assert(1 === Box(1))
      assert("s" === Box("s"))
    }

    def `should automatically box an object on the left` {

      implicit def box[T](obj: T): Box[T] = Box(obj)

      assert(1 === Box(1))
      assert("s" === Box("s"))
    }
    def `should automatically box an object on the right` {

      implicit def box[T](obj: T): Box[T] = Box(obj)

      assert(Box(1) === 1)
      assert(Box("s") === "s")
    }
  }
}

