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

class StrictEnabledEqualityCooperatingAnyValsSpec extends FunSpec with StrictEnabledEquality {

  describe("The StrictEnabledEquality trait") {
    it("should disallow equality comparisons between types that co-operate in Scala") {

      val aChar: Char = 'c'
      val aByte: Byte = 99
      val aShort: Short = 99
      val anInt: Int = 99
      val aLong: Long = 99L
      val aFloat: Float = 99.0F
      val aDouble: Double = 99.0
      // SKIP-SCALATESTJS-START
      val aBigInt: BigInt = BigInt(99)
      val aBigDecimal: BigDecimal = BigDecimal(99.0)
      // SKIP-SCALATESTJS-END
      val aJavaCharacter: java.lang.Character = new java.lang.Character('c')
      val aJavaByte: java.lang.Byte = new java.lang.Byte(99.toByte)
      val aJavaShort: java.lang.Short = new java.lang.Short(99.toShort)
      val aJavaInteger: java.lang.Integer = new java.lang.Integer(99)
      val aJavaLong: java.lang.Long = new java.lang.Long(99L)
      val aJavaFloat: java.lang.Float = new java.lang.Float(99.0F)
      val aJavaDouble: java.lang.Double = new java.lang.Double(99.0)

      val aBoolean: Boolean = true
      val aJavaBoolean: java.lang.Boolean = new java.lang.Boolean(true)

      assert(aChar === aChar)
      assertTypeError("aChar === aByte")
      assertTypeError("aChar === aShort")
      assertTypeError("aChar === anInt")
      assertTypeError("aChar === aLong")
      assertTypeError("aChar === aFloat")
      assertTypeError("aChar === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aChar === aBigInt")
      assertTypeError("aChar === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aChar === aJavaCharacter")
      assertTypeError("aChar === aJavaByte")
      assertTypeError("aChar === aJavaShort")
      assertTypeError("aChar === aJavaInteger")
      assertTypeError("aChar === aJavaLong")
      assertTypeError("aChar === aJavaFloat")
      assertTypeError("aChar === aJavaDouble")

      assertTypeError("aByte === aChar")
      assert(aByte === aByte)
      assertTypeError("aByte === aShort")
      assertTypeError("aByte === anInt")
      assertTypeError("aByte === aLong")
      assertTypeError("aByte === aFloat")
      assertTypeError("aByte === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aByte === aBigInt")
      assertTypeError("aByte === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aByte === aJavaCharacter")
      assertTypeError("aByte === aJavaByte")
      assertTypeError("aByte === aJavaShort")
      assertTypeError("aByte === aJavaInteger")
      assertTypeError("aByte === aJavaLong")
      assertTypeError("aByte === aJavaFloat")
      assertTypeError("aByte === aJavaDouble")

      assertTypeError("aShort === aChar")
      assertTypeError("aShort === aByte")
      assert(aShort === aShort)
      assertTypeError("aShort === anInt")
      assertTypeError("aShort === aLong")
      assertTypeError("aShort === aFloat")
      assertTypeError("aShort === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aShort === aBigInt")
      assertTypeError("aShort === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aShort === aJavaCharacter")
      assertTypeError("aShort === aJavaByte")
      assertTypeError("aShort === aJavaShort")
      assertTypeError("aShort === aJavaInteger")
      assertTypeError("aShort === aJavaLong")
      assertTypeError("aShort === aJavaFloat")
      assertTypeError("aShort === aJavaDouble")

      assertTypeError("anInt === aChar")
      assertTypeError("anInt === aByte")
      assertTypeError("anInt === aShort")
      assert(anInt === anInt)
      assertTypeError("anInt === aLong")
      assertTypeError("anInt === aFloat")
      assertTypeError("anInt === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("anInt === aBigInt")
      assertTypeError("anInt === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("anInt === aJavaCharacter")
      assertTypeError("anInt === aJavaByte")
      assertTypeError("anInt === aJavaShort")
      assertTypeError("anInt === aJavaInteger")
      assertTypeError("anInt === aJavaLong")
      assertTypeError("anInt === aJavaFloat")
      assertTypeError("anInt === aJavaDouble")

      assertTypeError("aLong === aChar")
      assertTypeError("aLong === aByte")
      assertTypeError("aLong === aShort")
      assertTypeError("aLong === anInt")
      assert(aLong === aLong)
      assertTypeError("aLong === aFloat")
      assertTypeError("aLong === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aLong === aBigInt")
      assertTypeError("aLong === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aLong === aJavaCharacter")
      assertTypeError("aLong === aJavaByte")
      assertTypeError("aLong === aJavaShort")
      assertTypeError("aLong === aJavaInteger")
      assertTypeError("aLong === aJavaLong")
      assertTypeError("aLong === aJavaFloat")
      assertTypeError("aLong === aJavaDouble")

      assertTypeError("aFloat === aChar")
      assertTypeError("aFloat === aByte")
      assertTypeError("aFloat === aShort")
      assertTypeError("aFloat === anInt")
      assertTypeError("aFloat === aLong")
      assert(aFloat === aFloat)
      assertTypeError("aFloat === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aFloat === aBigInt")
      assertTypeError("aFloat === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aFloat === aJavaCharacter")
      assertTypeError("aFloat === aJavaByte")
      assertTypeError("aFloat === aJavaShort")
      assertTypeError("aFloat === aJavaInteger")
      assertTypeError("aFloat === aJavaLong")
      assertTypeError("aFloat === aJavaFloat")
      assertTypeError("aFloat === aJavaDouble")

      assertTypeError("aDouble === aChar")
      assertTypeError("aDouble === aByte")
      assertTypeError("aDouble === aShort")
      assertTypeError("aDouble === anInt")
      assertTypeError("aDouble === aLong")
      assertTypeError("aDouble === aFloat")
      assert(aDouble === aDouble)
      // SKIP-SCALATESTJS-START
      assertTypeError("aDouble === aBigInt")
      assertTypeError("aDouble === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aDouble === aJavaCharacter")
      assertTypeError("aDouble === aJavaByte")
      assertTypeError("aDouble === aJavaShort")
      assertTypeError("aDouble === aJavaInteger")
      assertTypeError("aDouble === aJavaLong")
      assertTypeError("aDouble === aJavaFloat")
      assertTypeError("aDouble === aJavaDouble")

      assertTypeError("aBigInt === aChar")
      assertTypeError("aBigInt === aByte")
      assertTypeError("aBigInt === aShort")
      assertTypeError("aBigInt === anInt")
      assertTypeError("aBigInt === aLong")
      assertTypeError("aBigInt === aFloat")
      assertTypeError("aBigInt === aDouble")
      // SKIP-SCALATESTJS-START
      assert(aBigInt === aBigInt)
      assertTypeError("aBigInt === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aBigInt === aJavaCharacter")
      assertTypeError("aBigInt === aJavaByte")
      assertTypeError("aBigInt === aJavaShort")
      assertTypeError("aBigInt === aJavaInteger")
      assertTypeError("aBigInt === aJavaLong")
      assertTypeError("aBigInt === aJavaFloat")
      assertTypeError("aBigInt === aJavaDouble")

      assertTypeError("aBigDecimal === aChar")
      assertTypeError("aBigDecimal === aByte")
      assertTypeError("aBigDecimal === aShort")
      assertTypeError("aBigDecimal === anInt")
      assertTypeError("aBigDecimal === aLong")
      assertTypeError("aBigDecimal === aFloat")
      assertTypeError("aBigDecimal === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aBigDecimal === aBigInt")
      assert(aBigDecimal === aBigDecimal)
      // SKIP-SCALATESTJS-END
      assertTypeError("aBigDecimal === aJavaCharacter")
      assertTypeError("aBigDecimal === aJavaByte")
      assertTypeError("aBigDecimal === aJavaShort")
      assertTypeError("aBigDecimal === aJavaInteger")
      assertTypeError("aBigDecimal === aJavaLong")
      assertTypeError("aBigDecimal === aJavaFloat")
      assertTypeError("aBigDecimal === aJavaDouble")

      assertTypeError("aJavaCharacter === aChar")
      assertTypeError("aJavaCharacter === aByte")
      assertTypeError("aJavaCharacter === aShort")
      assertTypeError("aJavaCharacter === anInt")
      assertTypeError("aJavaCharacter === aLong")
      assertTypeError("aJavaCharacter === aFloat")
      assertTypeError("aJavaCharacter === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aJavaCharacter === aBigInt")
      assertTypeError("aJavaCharacter === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assert(aJavaCharacter === aJavaCharacter)
      assertTypeError("aJavaCharacter === aJavaByte")
      assertTypeError("aJavaCharacter === aJavaShort")
      assertTypeError("aJavaCharacter === aJavaInteger")
      assertTypeError("aJavaCharacter === aJavaLong")
      assertTypeError("aJavaCharacter === aJavaFloat")
      assertTypeError("aJavaCharacter === aJavaDouble")

      assertTypeError("aJavaByte === aChar")
      assertTypeError("aJavaByte === aByte")
      assertTypeError("aJavaByte === aShort")
      assertTypeError("aJavaByte === anInt")
      assertTypeError("aJavaByte === aLong")
      assertTypeError("aJavaByte === aFloat")
      assertTypeError("aJavaByte === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aJavaByte === aBigInt")
      assertTypeError("aJavaByte === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aJavaByte === aJavaCharacter")
      assert(aJavaByte === aJavaByte)
      assertTypeError("aJavaByte === aJavaShort")
      assertTypeError("aJavaByte === aJavaInteger")
      assertTypeError("aJavaByte === aJavaLong")
      assertTypeError("aJavaByte === aJavaFloat")
      assertTypeError("aJavaByte === aJavaDouble")

      assertTypeError("aJavaShort === aChar")
      assertTypeError("aJavaShort === aByte")
      assertTypeError("aJavaShort === aShort")
      assertTypeError("aJavaShort === anInt")
      assertTypeError("aJavaShort === aLong")
      assertTypeError("aJavaShort === aFloat")
      assertTypeError("aJavaShort === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aJavaShort === aBigInt")
      assertTypeError("aJavaShort === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aJavaShort === aJavaCharacter")
      assertTypeError("aJavaShort === aJavaByte")
      assert(aJavaShort === aJavaShort)
      assertTypeError("aJavaShort === aJavaInteger")
      assertTypeError("aJavaShort === aJavaLong")
      assertTypeError("aJavaShort === aJavaFloat")
      assertTypeError("aJavaShort === aJavaDouble")

      assertTypeError("aJavaInteger === aChar")
      assertTypeError("aJavaInteger === aByte")
      assertTypeError("aJavaInteger === aShort")
      assertTypeError("aJavaInteger === anInt")
      assertTypeError("aJavaInteger === aLong")
      assertTypeError("aJavaInteger === aFloat")
      assertTypeError("aJavaInteger === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aJavaInteger === aBigInt")
      assertTypeError("aJavaInteger === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aJavaInteger === aJavaCharacter")
      assertTypeError("aJavaInteger === aJavaByte")
      assertTypeError("aJavaInteger === aJavaShort")
      assert(aJavaInteger === aJavaInteger)
      assertTypeError("aJavaInteger === aJavaLong")
      assertTypeError("aJavaInteger === aJavaFloat")
      assertTypeError("aJavaInteger === aJavaDouble")

      assertTypeError("aJavaLong === aChar")
      assertTypeError("aJavaLong === aByte")
      assertTypeError("aJavaLong === aShort")
      assertTypeError("aJavaLong === anInt")
      assertTypeError("aJavaLong === aLong")
      assertTypeError("aJavaLong === aFloat")
      assertTypeError("aJavaLong === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aJavaLong === aBigInt")
      assertTypeError("aJavaLong === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aJavaLong === aJavaCharacter")
      assertTypeError("aJavaLong === aJavaByte")
      assertTypeError("aJavaLong === aJavaShort")
      assertTypeError("aJavaLong === aJavaInteger")
      assert(aJavaLong === aJavaLong)
      assertTypeError("aJavaLong === aJavaFloat")
      assertTypeError("aJavaLong === aJavaDouble")

      assertTypeError("aJavaFloat === aChar")
      assertTypeError("aJavaFloat === aByte")
      assertTypeError("aJavaFloat === aShort")
      assertTypeError("aJavaFloat === anInt")
      assertTypeError("aJavaFloat === aLong")
      assertTypeError("aJavaFloat === aFloat")
      assertTypeError("aJavaFloat === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aJavaFloat === aBigInt")
      assertTypeError("aJavaFloat === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aJavaFloat === aJavaCharacter")
      assertTypeError("aJavaFloat === aJavaByte")
      assertTypeError("aJavaFloat === aJavaShort")
      assertTypeError("aJavaFloat === aJavaInteger")
      assertTypeError("aJavaFloat === aJavaLong")
      assert(aJavaFloat === aJavaFloat)
      assertTypeError("aJavaFloat === aJavaDouble")

      assertTypeError("aJavaDouble === aChar")
      assertTypeError("aJavaDouble === aByte")
      assertTypeError("aJavaDouble === aShort")
      assertTypeError("aJavaDouble === anInt")
      assertTypeError("aJavaDouble === aLong")
      assertTypeError("aJavaDouble === aFloat")
      assertTypeError("aJavaDouble === aDouble")
      // SKIP-SCALATESTJS-START
      assertTypeError("aJavaDouble === aBigInt")
      assertTypeError("aJavaDouble === aBigDecimal")
      // SKIP-SCALATESTJS-END
      assertTypeError("aJavaDouble === aJavaCharacter")
      assertTypeError("aJavaDouble === aJavaByte")
      assertTypeError("aJavaDouble === aJavaShort")
      assertTypeError("aJavaDouble === aJavaInteger")
      assertTypeError("aJavaDouble === aJavaLong")
      assertTypeError("aJavaDouble === aJavaFloat")
      assert(aJavaDouble === aJavaDouble)

      assert(aBoolean === aBoolean)
      assertTypeError("aBoolean === aJavaBoolean")

      assert(aJavaBoolean === aJavaBoolean)
      assertTypeError("aJavaBoolean === aBoolean")
    }
  }
}

