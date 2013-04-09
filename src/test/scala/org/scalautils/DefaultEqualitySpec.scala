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

class DefaultEqualitySpec extends Spec with NonImplicitAssertions {

  object `the AnyEquality type class` {

    def `should call .equals on the left hand object (and not on the right hand object)` {

      class MyObject extends Object {
        var equalsWasCalled = false
        override def equals(o: Any): Boolean = {
          equalsWasCalled = true
          super.equals(o)
        }
      }

      val a = new MyObject
      val b = new MyObject
      assert(!a.equalsWasCalled)
      assert(!b.equalsWasCalled)
      (new DefaultEquality[MyObject]).areEqual(a, b)
      assert(a.equalsWasCalled)
      assert(!b.equalsWasCalled)
    }

    def `should call .deep first if left side, right side, or both are Arrays` {
      val a = Array(1, 2, 3)
      val b = Array(1, 2, 3)
      val v = Vector(1, 2, 3)
      assert((new DefaultEquality[Array[Int]]).areEqual(a, v))
      assert((new DefaultEquality[Vector[Int]]).areEqual(v, a))
      assert((new DefaultEquality[Array[Int]]).areEqual(a, b))
    }
  }
}

