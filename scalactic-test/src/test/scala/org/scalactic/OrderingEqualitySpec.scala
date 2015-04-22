/*
 * Copyright 2001-2014 Artima, Inc.
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

import org.scalatest._
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce

class OrderingEqualitySpec extends FunSpec with MustMatchers {
  describe("A Normalization") {
    it("can be converted to a OrderingEquality via toOrderingEquality") {
      /*
        scala> val ord = implicitly[Ordering[String]]
        ord: Ordering[String] = scala.math.Ordering$String$@f634763

        scala> ord.compare("happy", "happy")
        res2: Int = 0

        scala> ord.compare("happy", "dappy")
        res3: Int = 4

        scala> ord.compare("dappy", "happy")
        res4: Int = -4
      */
      val ordEq = StringNormalizations.lowerCased.toOrderingEquality
      ordEq.compare("HI", "hi") mustEqual 0
      ordEq.compare("happy", "happy") mustEqual 0
      ordEq.compare("hapPy", "hAPpy") mustEqual 0
      ordEq.compare("happy", "dappy") must be > 0
      ordEq.compare("dappy", "happy") must be < 0
    }
  }
}

