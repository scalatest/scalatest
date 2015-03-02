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
package org.scalactic.anyvals

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import org.scalactic.Equality
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks._
import scala.collection.mutable.WrappedArray
import OptionValues._

import scala.util.{Failure, Success, Try}

//import org.scalactic.StrictCheckedEquality

class PosZIntSpec extends Spec with Matchers/* with StrictCheckedEquality*/ {

  val posZIntGen: Gen[PosZInt] =
    for {i <- choose(0, Int.MaxValue)} yield PosZInt.from(i).get

  implicit val arbPosZInt: Arbitrary[PosZInt] = Arbitrary(posZIntGen)

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case _: Success[_] => a == b
      case Failure(ex) => b match {
        case _: Success[_] => false
        case Failure(otherEx) => ex.getClass == otherEx.getClass && ex.getMessage == otherEx.getMessage
        case _ => false
      }
    }
  }

  object `A PosZInt` {
    object `should offer a from factory method that` {
      def `returns Some[PosZInt] if the passed Int is greater than or equal to 0`
      {
        PosZInt.from(0).value.value shouldBe 0
        PosZInt.from(50).value.value shouldBe 50
        PosZInt.from(100).value.value shouldBe 100
      }
      def `returns None if the passed Int is NOT greater than or equal to 0` {
        PosZInt.from(-1) shouldBe None
        PosZInt.from(-99) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PosZInt.from(42).value.toString shouldBe "PosZInt(42)"
    }
    def `should return the same type from its unary_+ method` {
      +PosZInt(3) shouldEqual PosZInt(3)
    } 
    def `should be automatically widened to compatible AnyVal targets` {
      (PosZInt(3): Int) shouldEqual 3
      (PosZInt(3): Long) shouldEqual 3L
      (PosZInt(3): Float) shouldEqual 3.0F
      (PosZInt(3): Double) shouldEqual 3.0

      "(PosZInt(3): PosInt)" shouldNot typeCheck
      "(PosZInt(3): PosLong)" shouldNot typeCheck
      "(PosZInt(3): PosFloat)" shouldNot typeCheck
      "(PosZInt(3): PosDouble)" shouldNot typeCheck

      (PosZInt(3): PosZInt) shouldEqual PosZInt(3)
      (PosZInt(3): PosZLong) shouldEqual PosZLong(3L)
      (PosZInt(3): PosZFloat) shouldEqual PosZFloat(3.0F)
      (PosZInt(3): PosZDouble) shouldEqual PosZDouble(3.0)
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = PosZInt(3) + 3
        opInt shouldEqual 6

        val opLong = PosZInt(3) + 3L
        opLong shouldEqual 6L

        val opFloat = PosZInt(3) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = PosZInt(3) + 3.0
        opDouble shouldEqual 6.0

        // When adding a PosZ*
        val opPosInt = PosZInt(3) + PosInt(3)
        opPosInt shouldEqual 6

        val opPosLong = PosZInt(3) + PosLong(3L)
        opPosLong shouldEqual 6L

        val opPosFloat = PosZInt(3) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = PosZInt(3) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = PosZInt(3) + PosZInt(3)
        opPosZ shouldEqual 6

        val opPosZLong = PosZInt(3) + PosZLong(3L)
        opPosZLong shouldEqual 6L

        val opPosZFloat = PosZInt(3) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0F

        val opPosZDouble = PosZInt(3) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0
      }
    }

    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "PosZInt(8)" should compile
        PosZInt(8).value shouldEqual 8
      }

      def `should compile when 0 is passed in`: Unit = {
        "PosZInt(0)" should compile
        PosZInt(0).value shouldEqual 0
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PosZInt(-8)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = -8
        "PosZInt(x)" shouldNot compile
      }
    }
    object `when specified as a plain-old Int` {

      def takesPosZInt(pos: PosZInt): Int = pos.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPosZInt(8)" should compile
        takesPosZInt(8) shouldEqual 8
      }

      def `should compile when 0 is passed in`: Unit = {
        "takesPosZInt(0)" should compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPosZInt(-8)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = -8
        "takesPosZInt(x)" shouldNot compile
      }
    }

    def `should offer a unary ~ method that is consistent with Int` {
      forAll { (pzint: PosZInt) =>
        (~pzint) shouldEqual (~(pzint.toInt))
      }
    }

    def `should offer a unary + method that is consistent with Int` {
      forAll { (pzint: PosZInt) =>
        (+pzint).toInt shouldEqual (+(pzint.toInt))
      }
    }

    def `should offer a unary - method that is consistent with Int` {
      forAll { (pzint: PosZInt) =>
        (-pzint) shouldEqual (-(pzint.toInt))
      }
    }

    def `should offer << methods that are consistent with Int` {
      forAll { (pzint: PosZInt, shift: Int) =>
        pzint << shift shouldEqual pzint.toInt << shift
      }
      forAll { (pzint: PosZInt, shift: Long) =>
        pzint << shift shouldEqual pzint.toInt << shift
      }
    }

    def `should offer >>> methods that are consistent with Int` {
      forAll { (pzint: PosZInt, shift: Int) =>
        pzint >>> shift shouldEqual pzint.toInt >>> shift
      }
      forAll { (pzint: PosZInt, shift: Long) =>
        pzint >>> shift shouldEqual pzint.toInt >>> shift
      }
    }

    def `should offer >> methods that are consistent with Int` {
      forAll { (pzint: PosZInt, shift: Int) =>
        pzint >> shift shouldEqual pzint.toInt >> shift
      }
      forAll { (pzint: PosZInt, shift: Long) =>
        pzint >> shift shouldEqual pzint.toInt >> shift
      }
    }

    def `should offer '<' comparison that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint < byte) shouldEqual (pzint.toInt < byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint < short) shouldEqual (pzint.toInt < short)
      }
      forAll { (pzint: PosZInt, char: Char) =>
        (pzint < char) shouldEqual (pzint.toInt < char)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint < int) shouldEqual (pzint.toInt < int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint < long) shouldEqual (pzint.toInt < long)
      }
      forAll { (pzint: PosZInt, float: Float) =>
        (pzint < float) shouldEqual (pzint.toInt < float)
      }
      forAll { (pzint: PosZInt, double: Double) =>
        (pzint < double) shouldEqual (pzint.toInt < double)
      }
    }

    def `should offer '<=' comparison that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint <= byte) shouldEqual (pzint.toInt <= byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint <= short) shouldEqual (pzint.toInt <= short)
      }
      forAll { (pzint: PosZInt, char: Char) =>
        (pzint <= char) shouldEqual (pzint.toInt <= char)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint <= int) shouldEqual (pzint.toInt <= int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint <= long) shouldEqual (pzint.toInt <= long)
      }
      forAll { (pzint: PosZInt, float: Float) =>
        (pzint <= float) shouldEqual (pzint.toInt <= float)
      }
      forAll { (pzint: PosZInt, double: Double) =>
        (pzint <= double) shouldEqual (pzint.toInt <= double)
      }
    }

    def `should offer '>' comparison that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint > byte) shouldEqual (pzint.toInt > byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint > short) shouldEqual (pzint.toInt > short)
      }
      forAll { (pzint: PosZInt, char: Char) =>
        (pzint > char) shouldEqual (pzint.toInt > char)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint > int) shouldEqual (pzint.toInt > int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint > long) shouldEqual (pzint.toInt > long)
      }
      forAll { (pzint: PosZInt, float: Float) =>
        (pzint > float) shouldEqual (pzint.toInt > float)
      }
      forAll { (pzint: PosZInt, double: Double) =>
        (pzint > double) shouldEqual (pzint.toInt > double)
      }
    }

    def `should offer '>=' comparison that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint >= byte) shouldEqual (pzint.toInt >= byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint >= short) shouldEqual (pzint.toInt >= short)
      }
      forAll { (pzint: PosZInt, char: Char) =>
        (pzint >= char) shouldEqual (pzint.toInt >= char)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint >= int) shouldEqual (pzint.toInt >= int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint >= long) shouldEqual (pzint.toInt >= long)
      }
      forAll { (pzint: PosZInt, float: Float) =>
        (pzint >= float) shouldEqual (pzint.toInt >= float)
      }
      forAll { (pzint: PosZInt, double: Double) =>
        (pzint >= double) shouldEqual (pzint.toInt >= double)
      }
    }

    def `should offer a '|' method that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint | byte) shouldEqual (pzint.toInt | byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint | short) shouldEqual (pzint.toInt | short)
      }
      forAll { (pzint: PosZInt, char: Char) =>
        (pzint | char) shouldEqual (pzint.toInt | char)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint | int) shouldEqual (pzint.toInt | int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint | long) shouldEqual (pzint.toInt | long)
      }
    }

    def `should offer an '&' method that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint & byte) shouldEqual (pzint.toInt & byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint & short) shouldEqual (pzint.toInt & short)
      }
      forAll { (pzint: PosZInt, char: Char) =>
        (pzint & char) shouldEqual (pzint.toInt & char)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint & int) shouldEqual (pzint.toInt & int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint & long) shouldEqual (pzint.toInt & long)
      }
    }

    def `should offer an '^' method that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint ^ byte) shouldEqual (pzint.toInt ^ byte)
      }
      forAll { (pzint: PosZInt, char: Char) =>
        (pzint ^ char) shouldEqual (pzint.toInt ^ char)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint ^ short) shouldEqual (pzint.toInt ^ short)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint ^ int) shouldEqual (pzint.toInt ^ int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint ^ long) shouldEqual (pzint.toInt ^ long)
      }
    }

    def `should offer a '+' method that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint + byte) shouldEqual (pzint.toInt + byte)
      }
      forAll { (pzint: PosZInt, char: Char) =>
        (pzint + char) shouldEqual (pzint.toInt + char)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint + short) shouldEqual (pzint.toInt + short)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint + int) shouldEqual (pzint.toInt + int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint + long) shouldEqual (pzint.toInt + long)
      }
      forAll { (pzint: PosZInt, float: Float) =>
        (pzint + float) shouldEqual (pzint.toInt + float)
      }
      forAll { (pzint: PosZInt, double: Double) =>
        (pzint + double) shouldEqual (pzint.toInt + double)
      }
    }

    def `should offer a '-' method that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint - byte) shouldEqual (pzint.toInt - byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint - short) shouldEqual (pzint.toInt - short)
      }
      forAll { (pzint: PosZInt, byte: Char) =>
        (pzint - byte) shouldEqual (pzint.toInt - byte)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint - int) shouldEqual (pzint.toInt - int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint - long) shouldEqual (pzint.toInt - long)
      }
      forAll { (pzint: PosZInt, float: Float) =>
        (pzint - float) shouldEqual (pzint.toInt - float)
      }
      forAll { (pzint: PosZInt, double: Double) =>
        (pzint - double) shouldEqual (pzint.toInt - double)
      }
    }

    def `should offer a '*' method that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint * byte) shouldEqual (pzint.toInt * byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint * short) shouldEqual (pzint.toInt * short)
      }
      forAll { (pzint: PosZInt, byte: Char) =>
        (pzint * byte) shouldEqual (pzint.toInt * byte)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint * int) shouldEqual (pzint.toInt * int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint * long) shouldEqual (pzint.toInt * long)
      }
      forAll { (pzint: PosZInt, float: Float) =>
        (pzint * float) shouldEqual (pzint.toInt * float)
      }
      forAll { (pzint: PosZInt, double: Double) =>
        (pzint * double) shouldEqual (pzint.toInt * double)
      }
    }

    def `should offer a '/' method that is consistent with Int`: Unit = {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (pzint: PosZInt, byte: Byte) =>
        Try(pzint / byte) shouldEqual Try(pzint.toInt / byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        Try(pzint / short) shouldEqual Try(pzint.toInt / short)
      }
      forAll { (pzint: PosZInt, byte: Char) =>
        Try(pzint / byte) shouldEqual Try(pzint.toInt / byte)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        Try(pzint / int) shouldEqual Try(pzint.toInt / int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        Try(pzint / long) shouldEqual Try(pzint.toInt / long)
      }
      forAll { (pzint: PosZInt, float: Float) =>
        Try(pzint / float) shouldEqual Try(pzint.toInt / float)
      }
      forAll { (pzint: PosZInt, double: Double) =>
        Try(pzint / double) shouldEqual Try(pzint.toInt / double)
      }
    }

    def `should offer a '%' method that is consistent with Int`: Unit = {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (pzint: PosZInt, byte: Byte) =>
        Try(pzint % byte) shouldEqual Try(pzint.toInt % byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        Try(pzint % short) shouldEqual Try(pzint.toInt % short)
      }
      forAll { (pzint: PosZInt, byte: Char) =>
        Try(pzint % byte) shouldEqual Try(pzint.toInt % byte)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        Try(pzint % int) shouldEqual Try(pzint.toInt % int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        Try(pzint % long) shouldEqual Try(pzint.toInt % long)
      }
      forAll { (pzint: PosZInt, float: Float) =>
        Try(pzint % float) shouldEqual Try(pzint.toInt % float)
      }
      forAll { (pzint: PosZInt, double: Double) =>
        Try(pzint % double) shouldEqual Try(pzint.toInt % double)
      }
    }

    def `should offer a 'toBinaryString' method that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt) =>
        pzint.toBinaryString shouldEqual pzint.toInt.toBinaryString
      }
    }

    def `should offer a 'toHexString' method that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt) =>
        pzint.toHexString shouldEqual pzint.toInt.toHexString
      }
    }

    def `should offer a 'toOctalString' method that is consistent with Int`: Unit = {
      forAll { (pzint: PosZInt) =>
        pzint.toOctalString shouldEqual pzint.toInt.toOctalString
      }
    }

    def `should offer 'to' and 'until' methods that are consistent with Int`: Unit = {
      forAll { (pzint: PosZInt, end: Int, step: Int) =>
        Try(pzint.to(end)) shouldEqual Try(pzint.toInt.to(end))
        Try(pzint.to(end, step)) shouldEqual Try(pzint.toInt.to(end, step))
        Try(pzint.until(end)) shouldEqual Try(pzint.toInt.until(end))
        Try(pzint.until(end, step)) shouldEqual Try(pzint.toInt.until(end, step))
      }
    }

    def `should offer widening methods for basic types that are consistent with Int`: Unit = {
      forAll { (pzint: PosZInt) =>
        def widen(value: Int): Int = value
        widen(pzint) shouldEqual widen(pzint.toInt)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: Long): Long = value
        widen(pzint) shouldEqual widen(pzint.toInt)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: Float): Float = value
        widen(pzint) shouldEqual widen(pzint.toInt)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: Double): Double = value
        widen(pzint) shouldEqual widen(pzint.toInt)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: PosZLong): PosZLong = value
        widen(pzint) shouldEqual widen(PosZLong.from(pzint.toInt).get)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: PosZFloat): PosZFloat = value
        widen(pzint) shouldEqual widen(PosZFloat.from(pzint.toInt).get)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: PosZDouble): PosZDouble = value
        widen(pzint) shouldEqual widen(PosZDouble.from(pzint.toInt).get)
      }
    }
  }
}

