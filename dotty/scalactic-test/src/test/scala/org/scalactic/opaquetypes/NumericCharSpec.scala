package org.scalactic.opaquetypes

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalactic._
import scala.util.{Try, Success, Failure}
import scala.util.Try

import org.scalactic.opaquetypes.Numerics.NumericChar

trait NumericCharSpecSupport {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>  // This is because in scala.js x/0 results to NaN not ArithmetricException like in jvm, and we need to make sure Success(NaN) == Success(NaN) is true to pass the test.
        b match {
          case Success(bDouble: Double) if bDouble.isNaN => true
          case _ => false
        }
        // I needed this because with GenDrivenPropertyChecks, got:
        // [info] - should offer a '%' method that is consistent with Int *** FAILED ***
        // [info]   Success(NaN) did not equal Success(NaN) (PosIntExperiment.scala:498)
      case Success(float: Float) if float.isNaN =>
        b match {
          case Success(bFloat: Float) if bFloat.isNaN => true
          case _ => false
        }
      case _: Success[_] => a == b
      case Failure(ex) => b match {
        case _: Success[_] => false
        case Failure(otherEx) => ex.getClass == otherEx.getClass && ex.getMessage == otherEx.getMessage
        case _ => false
      }
    }
  }

}

class NumericCharSpec extends funspec.AnyFunSpec with matchers.should.Matchers with OptionValues with GeneratorDrivenPropertyChecks with NumericCharSpecSupport {

  def areEqualForgivingNaNs(x: AnyVal, y: AnyVal): Assertion = {
    def areEqualDoublesForgivingNaNs(xDouble: Double, yDouble: Double): Assertion = {
      if (xDouble.isNaN && yDouble.isNaN)
        succeed
      else
        xDouble shouldEqual yDouble
    }
    (x: AnyVal, y: AnyVal) match {
      case (a: Float, b: Float) =>
        areEqualDoublesForgivingNaNs(a, b)
      case (a: Float, b: Double) =>
        areEqualDoublesForgivingNaNs(a, b)
      case (a: Double, b: Float) =>
        areEqualDoublesForgivingNaNs(a, b)
      case (a: Double, b: Double) =>
        areEqualDoublesForgivingNaNs(a, b)
      case _ =>
        x shouldEqual y
    }
  }

  val typeName = "NumericChar"

  describe("NumericChar") {

    it("should be automatically widened to compatible AnyVal targets") {

      (NumericChar('3'): FiniteFloat) shouldEqual FiniteFloat(51.0f)
      (NumericChar('3'): FiniteDouble) shouldEqual FiniteDouble(51.0)
      (NumericChar('3'): PosInt) shouldEqual PosInt(51)
      (NumericChar('3'): PosLong) shouldEqual PosLong(51L)
      (NumericChar('3'): PosFloat) shouldEqual PosFloat(51.0f)
      (NumericChar('3'): PosDouble) shouldEqual PosDouble(51.0)
      (NumericChar('3'): PosZInt) shouldEqual PosZInt(51)
      (NumericChar('3'): PosZLong) shouldEqual PosZLong(51L)
      // PosZFloat and PosZFiniteFloat conversions via explicit construction and subtype widening
      val pfz: PosZFloat = PosFloat.ensuringValid(NumericChar('3').toFloat)
      pfz shouldEqual PosZFloat(51.0f)
      val pdz: PosZDouble = PosDouble.ensuringValid(NumericChar('3').toDouble)
      pdz shouldEqual PosZDouble(51.0)
      (NumericChar('3'): PosFiniteFloat) shouldEqual PosFiniteFloat(51.0f)
      (NumericChar('3'): PosFiniteDouble) shouldEqual PosFiniteDouble(51.0)
      // PosZFiniteFloat and PosZFiniteDouble can be obtained via .from method
      PosZFiniteFloat.from(NumericChar('3').toFloat).get shouldEqual PosZFiniteFloat(51.0f)
      PosZFiniteDouble.from(NumericChar('3').toDouble).get shouldEqual PosZFiniteDouble(51.0)
      "(NumericChar(3): Int)" shouldNot compile
      "(NumericChar(3): Long)" shouldNot compile
      "(NumericChar(3): Float)" shouldNot compile
      "(NumericChar(3): Double)" shouldNot compile
      "(NumericChar(3): NonZeroFiniteFloat)" shouldNot compile
      "(NumericChar(3): NonZeroFiniteDouble)" shouldNot compile
      "(NumericChar(3): NonZeroInt)" shouldNot compile
      "(NumericChar(3): NonZeroLong)" shouldNot compile
      "(NumericChar(3): NonZeroFloat)" shouldNot compile
      "(NumericChar(3): NonZeroDouble)" shouldNot compile
      "(NumericChar(3): NegFiniteFloat)" shouldNot compile
      "(NumericChar(3): NegFiniteDouble)" shouldNot compile
      "(NumericChar(3): NegZFiniteFloat)" shouldNot compile
      "(NumericChar(3): NegZFiniteDouble)" shouldNot compile
    }

    it("when a compatible AnyVal is passed to a + method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NumericChar('3') + 3 shouldEqual 54
      NumericChar('3') + 3L shouldEqual 54L
      NumericChar('3') + 3.0f shouldEqual 54.0f
      NumericChar('3') + 3.0 shouldEqual 54.0
      NumericChar('3') + PosInt(3) shouldEqual 54
      NumericChar('3') + PosLong(3L) shouldEqual 54L
      NumericChar('3') + PosFloat(3.0f) shouldEqual 54.0f
      NumericChar('3') + PosDouble(3.0) shouldEqual 54.0
      NumericChar('3') + PosZInt(3) shouldEqual 54
      NumericChar('3') + PosZLong(3L) shouldEqual 54L
      NumericChar('3') + PosZFloat(3.0f) shouldEqual 54.0f
      NumericChar('3') + PosZDouble(3.0) shouldEqual 54.0
      NumericChar('3') + NonZeroFiniteFloat(3.0f) shouldEqual 54.0f
      NumericChar('3') + NonZeroFiniteDouble(3.0) shouldEqual 54.0
      NumericChar('3') + NonZeroInt(3) shouldEqual 54
      NumericChar('3') + NonZeroLong(3L) shouldEqual 54L
      NumericChar('3') + NonZeroFloat(3.0f) shouldEqual 54.0f
      NumericChar('3') + NonZeroDouble(3.0) shouldEqual 54.0
      NumericChar('3') + PosFiniteFloat(3.0f) shouldEqual 54.0f
      NumericChar('3') + PosFiniteDouble(3.0) shouldEqual 54.0
      NumericChar('3') + PosZFiniteFloat(3.0f) shouldEqual 54.0f
      NumericChar('3') + PosZFiniteDouble(3.0) shouldEqual 54.0
      NumericChar('3') + NegFiniteFloat(-3.0f) shouldEqual 48.0f
      NumericChar('3') + NegFiniteDouble(-3.0) shouldEqual 48.0
      NumericChar('3') + NegZFiniteFloat(-3.0f) shouldEqual 48.0f
      NumericChar('3') + NegZFiniteDouble(-3.0) shouldEqual 48.0
      NumericChar('3') + FiniteFloat(3.0f) shouldEqual 54.0f
      NumericChar('3') + FiniteDouble(3.0) shouldEqual 54.0
    }

    it("when a compatible AnyVal is passed to a - method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NumericChar('3') - 2 shouldEqual 49
      NumericChar('3') - 2L shouldEqual 49L
      NumericChar('3') - 2.0f shouldEqual 49.0f
      NumericChar('3') - 2.0 shouldEqual 49.0
      NumericChar('3') - PosInt(2) shouldEqual 49
      NumericChar('3') - PosLong(2L) shouldEqual 49L
      NumericChar('3') - PosFloat(2.0f) shouldEqual 49.0f
      NumericChar('3') - PosDouble(2.0) shouldEqual 49.0
      NumericChar('3') - PosZInt(2) shouldEqual 49
      NumericChar('3') - PosZLong(2L) shouldEqual 49L
      NumericChar('3') - PosZFloat(2.0f) shouldEqual 49.0f
      NumericChar('3') - PosZDouble(2.0) shouldEqual 49.0
      NumericChar('3') - NonZeroFiniteFloat(2.0f) shouldEqual 49.0f
      NumericChar('3') - NonZeroFiniteDouble(2.0) shouldEqual 49.0
      NumericChar('3') - NonZeroInt(2) shouldEqual 49
      NumericChar('3') - NonZeroLong(2L) shouldEqual 49L
      NumericChar('3') - NonZeroFloat(2.0f) shouldEqual 49.0f
      NumericChar('3') - NonZeroDouble(2.0) shouldEqual 49.0
      NumericChar('3') - PosFiniteFloat(2.0f) shouldEqual 49.0f
      NumericChar('3') - PosFiniteDouble(2.0) shouldEqual 49.0
      NumericChar('3') - PosZFiniteFloat(2.0f) shouldEqual 49.0f
      NumericChar('3') - PosZFiniteDouble(2.0) shouldEqual 49.0
      NumericChar('3') - NegFiniteFloat(-2.0f) shouldEqual 53.0f
      NumericChar('3') - NegFiniteDouble(-2.0) shouldEqual 53.0
      NumericChar('3') - NegZFiniteFloat(-2.0f) shouldEqual 53.0f
      NumericChar('3') - NegZFiniteDouble(-2.0) shouldEqual 53.0
      NumericChar('3') - FiniteFloat(2.0f) shouldEqual 49.0f
      NumericChar('3') - FiniteDouble(2.0) shouldEqual 49.0
    }

    it("when a compatible AnyVal is passed to a * method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NumericChar('3') * 2 shouldEqual 102
      NumericChar('3') * 2L shouldEqual 102L
      NumericChar('3') * 2.0f shouldEqual 102.0f
      NumericChar('3') * 2.0 shouldEqual 102.0
      NumericChar('3') * PosInt(2) shouldEqual 102
      NumericChar('3') * PosLong(2L) shouldEqual 102L
      NumericChar('3') * PosFloat(2.0f) shouldEqual 102.0f
      NumericChar('3') * PosDouble(2.0) shouldEqual 102.0
      NumericChar('3') * PosZInt(2) shouldEqual 102
      NumericChar('3') * PosZLong(2L) shouldEqual 102L
      NumericChar('3') * PosZFloat(2.0f) shouldEqual 102.0f
      NumericChar('3') * PosZDouble(2.0) shouldEqual 102.0
      NumericChar('3') * NonZeroFiniteFloat(2.0f) shouldEqual 102.0f
      NumericChar('3') * NonZeroFiniteDouble(2.0) shouldEqual 102.0
      NumericChar('3') * NonZeroInt(2) shouldEqual 102
      NumericChar('3') * NonZeroLong(2L) shouldEqual 102L
      NumericChar('3') * NonZeroFloat(2.0f) shouldEqual 102.0f
      NumericChar('3') * NonZeroDouble(2.0) shouldEqual 102.0
      NumericChar('3') * PosFiniteFloat(2.0f) shouldEqual 102.0f
      NumericChar('3') * PosFiniteDouble(2.0) shouldEqual 102.0
      NumericChar('3') * PosZFiniteFloat(2.0f) shouldEqual 102.0f
      NumericChar('3') * PosZFiniteDouble(2.0) shouldEqual 102.0
      NumericChar('3') * NegFiniteFloat(-2.0f) shouldEqual -102.0f
      NumericChar('3') * NegFiniteDouble(-2.0) shouldEqual -102.0
      NumericChar('3') * NegZFiniteFloat(-2.0f) shouldEqual -102.0f
      NumericChar('3') * NegZFiniteDouble(-2.0) shouldEqual -102.0
      NumericChar('3') * FiniteFloat(2.0f) shouldEqual 102.0f
      NumericChar('3') * FiniteDouble(2.0) shouldEqual 102.0
    }

    it("when a compatible AnyVal is passed to a / method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NumericChar('3') / 3 shouldEqual 17
      NumericChar('3') / 3L shouldEqual 17L
      NumericChar('3') / 3.0f shouldEqual 17.0f
      NumericChar('3') / 3.0 shouldEqual 17.0
      NumericChar('3') / PosInt(3) shouldEqual 17
      NumericChar('3') / PosLong(3L) shouldEqual 17L
      NumericChar('3') / PosFloat(3.0f) shouldEqual 17.0f
      NumericChar('3') / PosDouble(3.0) shouldEqual 17.0
      NumericChar('3') / PosZInt(3) shouldEqual 17
      NumericChar('3') / PosZLong(3L) shouldEqual 17L
      NumericChar('3') / PosZFloat(3.0f) shouldEqual 17.0f
      NumericChar('3') / PosZDouble(3.0) shouldEqual 17.0
      NumericChar('3') / NonZeroFiniteFloat(3.0f) shouldEqual 17.0f
      NumericChar('3') / NonZeroFiniteDouble(3.0) shouldEqual 17.0
      NumericChar('3') / NonZeroInt(3) shouldEqual 17
      NumericChar('3') / NonZeroLong(3L) shouldEqual 17L
      NumericChar('3') / NonZeroFloat(3.0f) shouldEqual 17.0f
      NumericChar('3') / NonZeroDouble(3.0) shouldEqual 17.0
      NumericChar('3') / PosFiniteFloat(3.0f) shouldEqual 17.0f
      NumericChar('3') / PosFiniteDouble(3.0) shouldEqual 17.0
      NumericChar('3') / PosZFiniteFloat(3.0f) shouldEqual 17.0f
      NumericChar('3') / PosZFiniteDouble(3.0) shouldEqual 17.0
      NumericChar('3') / NegFiniteFloat(-3.0f) shouldEqual -17.0f
      NumericChar('3') / NegFiniteDouble(-3.0) shouldEqual -17.0
      NumericChar('3') / NegZFiniteFloat(-3.0f) shouldEqual -17.0f
      NumericChar('3') / NegZFiniteDouble(-3.0) shouldEqual -17.0
      NumericChar('3') / FiniteFloat(3.0f) shouldEqual 17.0f
      NumericChar('3') / FiniteDouble(3.0) shouldEqual 17.0
    }

    it("when a compatible AnyVal is passed to a % method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NumericChar('3') % 3 shouldEqual 0
      NumericChar('3') % 3L shouldEqual 0L
      NumericChar('3') % 3.0f shouldEqual 0.0f
      NumericChar('3') % 3.0 shouldEqual 0.0
      NumericChar('3') % PosInt(3) shouldEqual 0
      NumericChar('3') % PosLong(3L) shouldEqual 0L
      NumericChar('3') % PosFloat(3.0f) shouldEqual 0.0f
      NumericChar('3') % PosDouble(3.0) shouldEqual 0.0
      NumericChar('3') % PosZInt(3) shouldEqual 0
      NumericChar('3') % PosZLong(3L) shouldEqual 0L
      NumericChar('3') % PosZFloat(3.0f) shouldEqual 0.0f
      NumericChar('3') % PosZDouble(3.0) shouldEqual 0.0
      NumericChar('3') % NonZeroFiniteFloat(3.0f) shouldEqual 0.0f
      NumericChar('3') % NonZeroFiniteDouble(3.0) shouldEqual 0.0
      NumericChar('3') % NonZeroInt(3) shouldEqual 0
      NumericChar('3') % NonZeroLong(3L) shouldEqual 0L
      NumericChar('3') % NonZeroFloat(3.0f) shouldEqual 0.0f
      NumericChar('3') % NonZeroDouble(3.0) shouldEqual 0.0
      NumericChar('3') % PosFiniteFloat(3.0f) shouldEqual 0.0f
      NumericChar('3') % PosFiniteDouble(3.0) shouldEqual 0.0
      NumericChar('3') % PosZFiniteFloat(3.0f) shouldEqual 0.0f
      NumericChar('3') % PosZFiniteDouble(3.0) shouldEqual 0.0
      NumericChar('3') % NegFiniteFloat(-3.0f) shouldEqual 0.0f
      NumericChar('3') % NegFiniteDouble(-3.0) shouldEqual 0.0
      NumericChar('3') % NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      NumericChar('3') % NegZFiniteDouble(-3.0) shouldEqual 0.0
      NumericChar('3') % FiniteFloat(3.0f) shouldEqual 0.0f
      NumericChar('3') % FiniteDouble(3.0) shouldEqual 0.0
    }

    it("should have a pretty toString") {
      NumericChar.from('0').value.toString shouldBe "0"
      NumericChar.from('9').value.toString shouldBe "9"
    }

    it("should return the same type from its unary_+ method") {
      +NumericChar('3') shouldEqual NumericChar('3')
    }

    it("should offer a unary + method that is consistent with Char") {
      forAll { (p: NumericChar) =>
        (+p).toChar shouldEqual (+(p.toChar))
      }
    }

    it("should offer a unary - method that is consistent with Char") {
      forAll { (p: NumericChar) =>
        if (typeName.endsWith("Char"))
          (-p).toChar should not equal (-(p.toChar))
        else
          (-p).toChar shouldEqual (-(p.toChar))
      }
    }

    it("should offer '<' comparison that is consistent with Char") {
      forAll { (p: NumericChar, byte: Byte) =>
        (p < byte) shouldEqual (p.toChar < byte)
      }
      forAll { (p: NumericChar, short: Short) =>
        (p < short) shouldEqual (p.toChar < short)
      }
      forAll { (p: NumericChar, char: Char) =>
        (p < char) shouldEqual (p.toChar < char)
      }
      forAll { (p: NumericChar, int: Int) =>
        (p < int) shouldEqual (p.toChar < int)
      }
      forAll { (p: NumericChar, long: Long) =>
        (p < long) shouldEqual (p.toChar < long)
      }
      forAll { (p: NumericChar, float: Float) =>
        (p < float) shouldEqual (p.toChar < float)
      }
      forAll { (p: NumericChar, double: Double) =>
        (p < double) shouldEqual (p.toChar < double)
      }
    }

    it("should offer '<=' comparison that is consistent with Char") {
      forAll { (p: NumericChar, byte: Byte) =>
        (p <= byte) shouldEqual (p.toChar <= byte)
      }
      forAll { (p: NumericChar, short: Short) =>
        (p <= short) shouldEqual (p.toChar <= short)
      }
      forAll { (p: NumericChar, char: Char) =>
        (p <= char) shouldEqual (p.toChar <= char)
      }
      forAll { (p: NumericChar, int: Int) =>
        (p <= int) shouldEqual (p.toChar <= int)
      }
      forAll { (p: NumericChar, long: Long) =>
        (p <= long) shouldEqual (p.toChar <= long)
      }
      forAll { (p: NumericChar, float: Float) =>
        (p <= float) shouldEqual (p.toChar <= float)
      }
      forAll { (p: NumericChar, double: Double) =>
        (p <= double) shouldEqual (p.toChar <= double)
      }
    }

    it("should offer '>' comparison that is consistent with Char") {
      forAll { (p: NumericChar, byte: Byte) =>
        (p > byte) shouldEqual (p.toChar > byte)
      }
      forAll { (p: NumericChar, short: Short) =>
        (p > short) shouldEqual (p.toChar > short)
      }
      forAll { (p: NumericChar, char: Char) =>
        (p > char) shouldEqual (p.toChar > char)
      }
      forAll { (p: NumericChar, int: Int) =>
        (p > int) shouldEqual (p.toChar > int)
      }
      forAll { (p: NumericChar, long: Long) =>
        (p > long) shouldEqual (p.toChar > long)
      }
      forAll { (p: NumericChar, float: Float) =>
        (p > float) shouldEqual (p.toChar > float)
      }
      forAll { (p: NumericChar, double: Double) =>
       (p > double) shouldEqual (p.toChar > double)
      }
    }

    it("should offer '>=' comparison that is consistent with Char") {
      forAll { (p: NumericChar, byte: Byte) =>
        (p >= byte) shouldEqual (p.toChar >= byte)
      }
      forAll { (p: NumericChar, short: Short) =>
        (p >= short) shouldEqual (p.toChar >= short)
      }
      forAll { (p: NumericChar, char: Char) =>
        (p >= char) shouldEqual (p.toChar >= char)
      }
      forAll { (p: NumericChar, int: Int) =>
        (p >= int) shouldEqual (p.toChar >= int)
      }
      forAll { (p: NumericChar, long: Long) =>
        (p >= long) shouldEqual (p.toChar >= long)
      }
      forAll { (p: NumericChar, float: Float) =>
        (p >= float) shouldEqual (p.toChar >= float)
      }
      forAll { (p: NumericChar, double: Double) =>
        (p >= double) shouldEqual (p.toChar >= double)
      }
    }

    it("should offer a '+' method that is consistent with Char") {
      forAll { (p: NumericChar, byte: Byte) =>
        (p + byte) shouldEqual (p.toChar + byte)
      }
      forAll { (p: NumericChar, char: Char) =>
        (p + char) shouldEqual (p.toChar + char)
      }
      forAll { (p: NumericChar, short: Short) =>
        (p + short) shouldEqual (p.toChar + short)
      }
      forAll { (p: NumericChar, int: Int) =>
        (p + int) shouldEqual (p.toChar + int)
      }
      forAll { (p: NumericChar, long: Long) =>
        (p + long) shouldEqual (p.toChar + long)
      }
      forAll { (p: NumericChar, float: Float) =>
        (p + float) shouldEqual (p.toChar + float)
      }
      forAll { (p: NumericChar, double: Double) =>
        (p + double) shouldEqual (p.toChar + double)
      }
    }

    it("should offer a '-' method that is consistent with Char") {
      forAll { (p: NumericChar, byte: Byte) =>
        (p - byte) shouldEqual (p.toChar - byte)
      }
      forAll { (p: NumericChar, short: Short) =>
        (p - short) shouldEqual (p.toChar - short)
      }
      forAll { (p: NumericChar, byte: Char) =>
        (p - byte) shouldEqual (p.toChar - byte)
      }
      forAll { (p: NumericChar, int: Int) =>
        (p - int) shouldEqual (p.toChar - int)
      }
      forAll { (p: NumericChar, long: Long) =>
        (p - long) shouldEqual (p.toChar - long)
      }
      forAll { (p: NumericChar, float: Float) =>
        val x = p - float
        val y = p.toChar - float
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NumericChar, double: Double) =>
        val x = p - double
        val y = p.toChar - double
        areEqualForgivingNaNs(x, y)
      }
    }

    it("should offer a '*' method that is consistent with Char") {
      forAll { (p: NumericChar, byte: Byte) =>
        val x = p * byte
        val y = p.toChar * byte
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NumericChar, short: Short) =>
        val x = p * short
        val y = p.toChar * short
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NumericChar, char: Char) =>
        val x = p * char
        val y = p.toChar * char
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NumericChar, int: Int) =>
        val x = p * int
        val y = p.toChar * int
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NumericChar, long: Long) =>
        val x = p * long
        val y = p.toChar * long
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NumericChar, float: Float) =>
        val x = p * float
        val y = p.toChar * float
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NumericChar, double: Double) =>
        val x = p * double
        val y = p.toChar * double
        areEqualForgivingNaNs(x, y)
      }
    }

    it("should offer a '/' method that is consistent with Char") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (p: NumericChar, byte: Byte) =>
        Try(p / byte) shouldEqual Try(p.toChar / byte)
      }
      forAll { (p: NumericChar, short: Short) =>
        Try(p / short) shouldEqual Try(p.toChar / short)
      }
      forAll { (p: NumericChar, char: Char) =>
        Try(p / char) shouldEqual Try(p.toChar / char)
      }
      forAll { (p: NumericChar, int: Int) =>
        Try(p / int) shouldEqual Try(p.toChar / int)
      }
      forAll { (p: NumericChar, long: Long) =>
        Try(p / long) shouldEqual Try(p.toChar / long)
      }
      forAll { (p: NumericChar, float: Float) =>
        Try(p / float) shouldEqual Try(p.toChar / float)
      }
      forAll { (p: NumericChar, double: Double) =>
        Try(p / double) shouldEqual Try(p.toChar / double)
      }
    }

    it("should offer a '%' method that is consistent with Char") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (p: NumericChar, byte: Byte) =>
        Try(p % byte) shouldEqual Try(p.toChar % byte)
      }
      forAll { (p: NumericChar, short: Short) =>
        Try(p % short) shouldEqual Try(p.toChar % short)
      }
      forAll { (p: NumericChar, char: Char) =>
        Try(p % char) shouldEqual Try(p.toChar % char)
      }
      forAll { (p: NumericChar, int: Int) =>
        Try(p % int) shouldEqual Try(p.toChar % int)
      }
      forAll { (p: NumericChar, long: Long) =>
        Try(p % long) shouldEqual Try(p.toChar % long)
      }
      forAll { (p: NumericChar, float: Float) =>
        Try(p % float) shouldEqual Try(p.toChar % float)
      }
      forAll { (p: NumericChar, double: Double) =>
        Try(p % double) shouldEqual Try(p.toChar % double)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Char") {
      forAll { (p1: NumericChar, p2: NumericChar) =>
        p1.max(p2).toChar shouldEqual p1.toChar.max(p2.toChar)
        p1.min(p2).toChar shouldEqual p1.toChar.min(p2.toChar)
      }
    }

    it("should offer widening methods for basic types that are consistent with Char") {
      forAll { (p: NumericChar) =>
        def widen(value: FiniteFloat): FiniteFloat = value
        widen(p) shouldEqual widen(FiniteFloat.from(p.toChar).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: FiniteDouble): FiniteDouble = value
        widen(p) shouldEqual widen(FiniteDouble.from(p.toChar).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosInt): PosInt = value
        widen(p) shouldEqual widen(PosInt.from(p.toChar).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosLong): PosLong = value
        widen(p) shouldEqual widen(PosLong.from(p.toChar).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosFloat): PosFloat = value
        widen(p) shouldEqual widen(PosFloat.from(p.toChar).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosDouble): PosDouble = value
        widen(p) shouldEqual widen(PosDouble.from(p.toChar).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosZInt): PosZInt = value
        widen(p) shouldEqual widen(PosZInt.from(p.toChar).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosZLong): PosZLong = value
        widen(p) shouldEqual widen(PosZLong.from(p.toChar).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosZFloat): PosZFloat = value
        widen(PosZFloat.from(p.toFloat).get) shouldEqual widen(PosZFloat.from(p.value).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosZDouble): PosZDouble = value
        widen(PosZDouble.from(p.toDouble).get) shouldEqual widen(PosZDouble.from(p.value).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosFiniteFloat): PosFiniteFloat = value
        widen(p) shouldEqual widen(PosFiniteFloat.from(p.toChar).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosFiniteDouble): PosFiniteDouble = value
        widen(p) shouldEqual widen(PosFiniteDouble.from(p.toChar).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosZFiniteFloat): PosZFiniteFloat = value
        // Use explicit construction since implicit conversions cause ambiguity
        widen(PosZFiniteFloat.from(p.value).get) shouldEqual widen(PosZFiniteFloat.from(p.toChar).get)
      }
             
      forAll { (p: NumericChar) =>
        def widen(value: PosZFiniteDouble): PosZFiniteDouble = value
        // Use explicit construction since implicit conversions cause ambiguity
        widen(PosZFiniteDouble.from(p.value).get) shouldEqual widen(PosZFiniteDouble.from(p.toChar).get)
      }
             
    }

  }

}
