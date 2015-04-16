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

class NumericEqualityConstraintsSpec extends FunSpec with TypeCheckedTripleEquals with NumericEqualityConstraints {

  describe("The NumericEqualityConstraints trait") {
    it("should allow equality comparisons between types that co-operate in Scala") {

      val aChar: Char = 'c'
      val aByte: Byte = 99
      val aShort: Short = 99
      val anInt: Int = 99
      val aLong: Long = 99L
      val aFloat: Float = 99.0F
      val aDouble: Double = 99.0
      // TODO: To re-enable this once scala.js support big numbers
      // SKIP-SCALATESTJS-START
      val aBigInt: BigInt = BigInt(99)
      val aBigDecimal: BigDecimal = BigDecimal(99.0)
      // SKIP-SCALATESTJS-END
      // val aJavaBigInteger: java.math.BigInteger = new java.math.BigInteger("99")
      // val aJavaBigDecimal: java.math.BigDecimal = new java.math.BigDecimal(99.0)

      assert(aChar === aByte)
      assert(aChar === aShort)
      assert(aChar === anInt)
      assert(aChar === aLong)
      assert(aChar === aFloat)
      assert(aChar === aDouble)
      // TODO: To re-enable this once scala.js support big numbers
      // SKIP-SCALATESTJS-START
      assert(aChar === aBigInt)
      assert(aChar === aBigDecimal)
      // assert(aChar == aJavaBigInteger)
      // assert(aChar == aJavaBigDecimal)

      assert(aByte === aShort)
      assert(aByte === anInt)
      assert(aByte === aLong)
      assert(aByte === aFloat)
      assert(aByte === aDouble)
      // TODO: To re-enable this once scala.js support big numbers
      // SKIP-SCALATESTJS-START
      assert(aByte === aBigInt)
      assert(aByte === aBigDecimal)
      // SKIP-SCALATESTJS-END
      // assert(aByte == aJavaBigInteger)
      // assert(aByte == aJavaBigDecimal)

      assert(aShort === anInt)
      assert(aShort === aLong)
      assert(aShort === aFloat)
      assert(aShort === aDouble)
      // TODO: To re-enable this once scala.js support big numbers
      // SKIP-SCALATESTJS-START
      assert(aShort === aBigInt)
      assert(aShort === aBigDecimal)
      // SKIP-SCALATESTJS-END
      // assert(aShort == aJavaBigInteger)
      // assert(aShort == aJavaBigDecimal)

      assert(anInt === aLong)
      assert(anInt === aFloat)
      assert(anInt === aDouble)
      // TODO: To re-enable this once scala.js support big numbers
      // SKIP-SCALATESTJS-START
      assert(anInt === aBigInt)
      assert(anInt === aBigDecimal)
      // SKIP-SCALATESTJS-END
      // assert(anInt == aJavaBigInteger)
      // assert(anInt == aJavaBigDecimal)

      assert(aLong === aFloat)
      assert(aLong === aDouble)
      // TODO: To re-enable this once scala.js support big numbers
      // SKIP-SCALATESTJS-START
      assert(aLong === aBigInt)
      assert(aLong === aBigDecimal)
      // SKIP-SCALATESTJS-END
      // assert(aLong == aJavaBigInteger)
      // assert(aLong == aJavaBigDecimal)

      assert(aFloat === aDouble)
      // TODO: To re-enable this once scala.js support big numbers
      // SKIP-SCALATESTJS-START
      assert(aFloat === aBigInt)
      assert(aFloat === aBigDecimal)
      // SKIP-SCALATESTJS-END
      // assert(aFloat == aJavaBigInteger)
      // assert(aFloat == aJavaBigDecimal)

      // TODO: To re-enable this once scala.js support big numbers
      // SKIP-SCALATESTJS-START
      assert(aDouble === aBigInt)
      assert(aDouble === aBigDecimal)
      // SKIP-SCALATESTJS-END
      // assert(aDouble == aJavaBigInteger)
      // assert(aDouble == aJavaBigDecimal)

      // TODO: To re-enable this once scala.js support big numbers
      // SKIP-SCALATESTJS-START
      assert(aBigInt === aBigDecimal)
      // SKIP-SCALATESTJS-END
      // assert(aBigInt == aJavaBigInteger)
      // assert(aBigInt == aJavaBigDecimal)
      // assert(aBigDecimal == aJavaBigInteger)
      // assert(aBigDecimal == aJavaBigDecimal)
      // assert(aJavaBigInteger == aJavaBigDecimal)
    }
  }
}

