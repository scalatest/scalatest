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
package org.scalatest

import collection.immutable.TreeSet
import reflect.NameTransformer.encode

class EncodedOrderingSpec extends WordSpec {
  "EncodedOrdering" should {
    "sort unencoded strings the same as the default string ordering" in {
      val default = TreeSet("testHi", "testHo", "testPlus", "testMinus")
      val encoded = TreeSet("testHi", "testHo", "testPlus", "testMinus")(EncodedOrdering)
      assert(default.iterator.toList === encoded.iterator.toList)
    }
    "sort encoded strings in unencoded order" in {
      val set = TreeSet(encode("test: ho"), encode("test: hi"), encode("test: +"), encode("test: -"))(EncodedOrdering)
      val expected = List(encode("test: +"), encode("test: -"), encode("test: hi"), encode("test: ho"))
      assert(set.iterator.toList === expected)
    }
  }
}
