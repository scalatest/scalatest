package org.scalactic.opaquetypes

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.util.{Try, Success, Failure}
import org.scalactic.Equality

class NonZeroLongSpec extends funspec.AnyFunSpec with matchers.should.Matchers with OptionValues with GeneratorDrivenPropertyChecks {

  // Custom equality for Try to handle NaN and Failure comparison
  implicit def tryEq[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>
        b match {
          case Success(bDouble: Double) if bDouble.isNaN => true
          case _ => false
        }
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

  val typeName = "NonZeroLong"

  describe("NonZeroLong") {

    it("should be automatically widened to compatible AnyVal targets") {
      (NonZeroLong(3): Long) shouldEqual 3L
      (NonZeroLong(3).toFloat: Float) shouldEqual 3.0f
      (NonZeroLong(3).toDouble: Double) shouldEqual 3.0
      (NonZeroLong(3L): NonZeroFloat) shouldEqual NonZeroFloat(3.0f)
      (NonZeroLong(3L): NonZeroDouble) shouldEqual NonZeroDouble(3.0)
      "(NonZeroLong(3): Int)" shouldNot compile
      "(NonZeroLong(3): PosInt)" shouldNot compile
      "(NonZeroLong(3): PosLong)" shouldNot compile
      "(NonZeroLong(3): PosFloat)" shouldNot compile
      "(NonZeroLong(3): PosDouble)" shouldNot compile
      "(NonZeroLong(3): PosZInt)" shouldNot compile
      "(NonZeroLong(3): PosZLong)" shouldNot compile
      "(NonZeroLong(3): PosZFloat)" shouldNot compile
      "(NonZeroLong(3): PosZDouble)" shouldNot compile
      "(NonZeroLong(3): NonZeroFiniteFloat)" shouldNot compile
      "(NonZeroLong(3): NonZeroFiniteDouble)" shouldNot compile
      "(NonZeroLong(3): NonZeroInt)" shouldNot compile
      "(NonZeroLong(3): PosFiniteFloat)" shouldNot compile
      "(NonZeroLong(3): PosFiniteDouble)" shouldNot compile
      "(NonZeroLong(3): PosZFiniteFloat)" shouldNot compile
      "(NonZeroLong(3): PosZFiniteDouble)" shouldNot compile
      "(NonZeroLong(3): NegFiniteFloat)" shouldNot compile
      "(NonZeroLong(3): NegFiniteDouble)" shouldNot compile
      "(NonZeroLong(3): NegZFiniteFloat)" shouldNot compile
      "(NonZeroLong(3): NegZFiniteDouble)" shouldNot compile
      "(NonZeroLong(3): FiniteFloat)" shouldNot compile
      "(NonZeroLong(3): FiniteDouble)" shouldNot compile
    }

    it("when a compatible AnyVal is passed to a + method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroLong(3L) + 3 shouldEqual 6
      NonZeroLong(3L) + 3L shouldEqual 6L
      NonZeroLong(3L) + 3.0f shouldEqual 6.0f
      NonZeroLong(3L) + 3.0 shouldEqual 6.0
      NonZeroLong(3L) + PosInt(3) shouldEqual 6
      NonZeroLong(3L) + PosLong(3L) shouldEqual 6L
      NonZeroLong(3L) + PosFloat(3.0f) shouldEqual 6.0f
      NonZeroLong(3L) + PosDouble(3.0) shouldEqual 6.0
      NonZeroLong(3L) + PosZInt(3) shouldEqual 6
      NonZeroLong(3L) + PosZLong(3L) shouldEqual 6L
      NonZeroLong(3L) + PosZFloat(3.0f) shouldEqual 6.0f
      NonZeroLong(3L) + PosZDouble(3.0) shouldEqual 6.0
      NonZeroLong(3L) + NonZeroFiniteFloat.ensuringValid(3.0f) shouldEqual 6.0f
      NonZeroLong(3L) + NonZeroFiniteDouble.ensuringValid(3.0) shouldEqual 6.0
      NonZeroLong(3L) + NonZeroInt(3) shouldEqual 6
      NonZeroLong(3L) + NonZeroLong(3L) shouldEqual 6L
      NonZeroLong(3L) + NonZeroFloat(3.0f) shouldEqual 6.0f
      NonZeroLong(3L) + NonZeroDouble(3.0) shouldEqual 6.0
      NonZeroLong(3L) + PosFiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroLong(3L) + PosFiniteDouble(3.0) shouldEqual 6.0
      NonZeroLong(3L) + PosZFiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroLong(3L) + PosZFiniteDouble(3.0) shouldEqual 6.0
      NonZeroLong(3L) + NegFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroLong(3L) + NegFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroLong(3L) + NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroLong(3L) + NegZFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroLong(3L) + FiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroLong(3L) + FiniteDouble(3.0) shouldEqual 6.0
    }

    it("when a compatible AnyVal is passed to a - method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroLong(3L) - 2 shouldEqual 1
      NonZeroLong(3L) - 2L shouldEqual 1L
      NonZeroLong(3L) - 2.0f shouldEqual 1.0f
      NonZeroLong(3L) - 2.0 shouldEqual 1.0
      NonZeroLong(3L) - PosInt(2) shouldEqual 1
      NonZeroLong(3L) - PosLong(2L) shouldEqual 1L
      NonZeroLong(3L) - PosFloat(2.0f) shouldEqual 1.0f
      NonZeroLong(3L) - PosDouble(2.0) shouldEqual 1.0
      NonZeroLong(3L) - PosZInt(2) shouldEqual 1
      NonZeroLong(3L) - PosZLong(2L) shouldEqual 1L
      NonZeroLong(3L) - PosZFloat(2.0f) shouldEqual 1.0f
      NonZeroLong(3L) - PosZDouble(2.0) shouldEqual 1.0
      NonZeroLong(3L) - NonZeroFiniteFloat.ensuringValid(2.0f) shouldEqual 1.0f
      NonZeroLong(3L) - NonZeroFiniteDouble.ensuringValid(2.0) shouldEqual 1.0
      NonZeroLong(3L) - NonZeroInt(2) shouldEqual 1
      NonZeroLong(3L) - NonZeroLong(2L) shouldEqual 1L
      NonZeroLong(3L) - NonZeroFloat(2.0f) shouldEqual 1.0f
      NonZeroLong(3L) - NonZeroDouble(2.0) shouldEqual 1.0
      NonZeroLong(3L) - PosFiniteFloat(2.0f) shouldEqual 1.0f
      NonZeroLong(3L) - PosFiniteDouble(2.0) shouldEqual 1.0
      NonZeroLong(3L) - PosZFiniteFloat(2.0f) shouldEqual 1.0f
      NonZeroLong(3L) - PosZFiniteDouble(2.0) shouldEqual 1.0
      NonZeroLong(3L) - NegFiniteFloat(-2.0f) shouldEqual 5.0f
      NonZeroLong(3L) - NegFiniteDouble(-2.0) shouldEqual 5.0
      NonZeroLong(3L) - NegZFiniteFloat(-2.0f) shouldEqual 5.0f
      NonZeroLong(3L) - NegZFiniteDouble(-2.0) shouldEqual 5.0
      NonZeroLong(3L) - FiniteFloat(2.0f) shouldEqual 1.0f
      NonZeroLong(3L) - FiniteDouble(2.0) shouldEqual 1.0
    }

    it("when a compatible AnyVal is passed to a * method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroLong(3L) * 2 shouldEqual 6
      NonZeroLong(3L) * 2L shouldEqual 6L
      NonZeroLong(3L) * 2.0f shouldEqual 6.0f
      NonZeroLong(3L) * 2.0 shouldEqual 6.0
      NonZeroLong(3L) * PosInt(2) shouldEqual 6
      NonZeroLong(3L) * PosLong(2L) shouldEqual 6L
      NonZeroLong(3L) * PosFloat(2.0f) shouldEqual 6.0f
      NonZeroLong(3L) * PosDouble(2.0) shouldEqual 6.0
      NonZeroLong(3L) * PosZInt(2) shouldEqual 6
      NonZeroLong(3L) * PosZLong(2L) shouldEqual 6L
      NonZeroLong(3L) * PosZFloat(2.0f) shouldEqual 6.0f
      NonZeroLong(3L) * PosZDouble(2.0) shouldEqual 6.0
      NonZeroLong(3L) * NonZeroFiniteFloat.ensuringValid(2.0f) shouldEqual 6.0f
      NonZeroLong(3L) * NonZeroFiniteDouble.ensuringValid(2.0) shouldEqual 6.0
      NonZeroLong(3L) * NonZeroInt(2) shouldEqual 6
      NonZeroLong(3L) * NonZeroLong(2L) shouldEqual 6L
      NonZeroLong(3L) * NonZeroFloat(2.0f) shouldEqual 6.0f
      NonZeroLong(3L) * NonZeroDouble(2.0) shouldEqual 6.0
      NonZeroLong(3L) * PosFiniteFloat(2.0f) shouldEqual 6.0f
      NonZeroLong(3L) * PosFiniteDouble(2.0) shouldEqual 6.0
      NonZeroLong(3L) * PosZFiniteFloat(2.0f) shouldEqual 6.0f
      NonZeroLong(3L) * PosZFiniteDouble(2.0) shouldEqual 6.0
      NonZeroLong(3L) * NegFiniteFloat(-2.0f) shouldEqual -6.0f
      NonZeroLong(3L) * NegFiniteDouble(-2.0) shouldEqual -6.0
      NonZeroLong(3L) * NegZFiniteFloat(-2.0f) shouldEqual -6.0f
      NonZeroLong(3L) * NegZFiniteDouble(-2.0) shouldEqual -6.0
      NonZeroLong(3L) * FiniteFloat(2.0f) shouldEqual 6.0f
      NonZeroLong(3L) * FiniteDouble(2.0) shouldEqual 6.0
    }

    it("when a compatible AnyVal is passed to a / method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroLong(3L) / 3 shouldEqual 1
      NonZeroLong(3L) / 3L shouldEqual 1L
      NonZeroLong(3L) / 3.0f shouldEqual 1.0f
      NonZeroLong(3L) / 3.0 shouldEqual 1.0
      NonZeroLong(3L) / PosInt(3) shouldEqual 1
      NonZeroLong(3L) / PosLong(3L) shouldEqual 1L
      NonZeroLong(3L) / PosFloat(3.0f) shouldEqual 1.0f
      NonZeroLong(3L) / PosDouble(3.0) shouldEqual 1.0
      NonZeroLong(3L) / PosZInt(3) shouldEqual 1
      NonZeroLong(3L) / PosZLong(3L) shouldEqual 1L
      NonZeroLong(3L) / PosZFloat(3.0f) shouldEqual 1.0f
      NonZeroLong(3L) / PosZDouble(3.0) shouldEqual 1.0
      NonZeroLong(3L) / NonZeroFiniteFloat.ensuringValid(3.0f) shouldEqual 1.0f
      NonZeroLong(3L) / NonZeroFiniteDouble.ensuringValid(3.0) shouldEqual 1.0
      NonZeroLong(3L) / NonZeroInt(3) shouldEqual 1
      NonZeroLong(3L) / NonZeroLong(3L) shouldEqual 1L
      NonZeroLong(3L) / NonZeroFloat(3.0f) shouldEqual 1.0f
      NonZeroLong(3L) / NonZeroDouble(3.0) shouldEqual 1.0
      NonZeroLong(3L) / PosFiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroLong(3L) / PosFiniteDouble(3.0) shouldEqual 1.0
      NonZeroLong(3L) / PosZFiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroLong(3L) / PosZFiniteDouble(3.0) shouldEqual 1.0
      NonZeroLong(3L) / NegFiniteFloat(-3.0f) shouldEqual -1.0f
      NonZeroLong(3L) / NegFiniteDouble(-3.0) shouldEqual -1.0
      NonZeroLong(3L) / NegZFiniteFloat(-3.0f) shouldEqual -1.0f
      NonZeroLong(3L) / NegZFiniteDouble(-3.0) shouldEqual -1.0
      NonZeroLong(3L) / FiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroLong(3L) / FiniteDouble(3.0) shouldEqual 1.0
    }

    it("when a compatible AnyVal is passed to a % method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroLong(3L) % 3 shouldEqual 0
      NonZeroLong(3L) % 3L shouldEqual 0L
      NonZeroLong(3L) % 3.0f shouldEqual 0.0f
      NonZeroLong(3L) % 3.0 shouldEqual 0.0
      NonZeroLong(3L) % PosInt(3) shouldEqual 0
      NonZeroLong(3L) % PosLong(3L) shouldEqual 0L
      NonZeroLong(3L) % PosFloat(3.0f) shouldEqual 0.0f
      NonZeroLong(3L) % PosDouble(3.0) shouldEqual 0.0
      NonZeroLong(3L) % PosZInt(3) shouldEqual 0
      NonZeroLong(3L) % PosZLong(3L) shouldEqual 0L
      NonZeroLong(3L) % PosZFloat(3.0f) shouldEqual 0.0f
      NonZeroLong(3L) % PosZDouble(3.0) shouldEqual 0.0
      NonZeroLong(3L) % NonZeroFiniteFloat.ensuringValid(3.0f) shouldEqual 0.0f
      NonZeroLong(3L) % NonZeroFiniteDouble.ensuringValid(3.0) shouldEqual 0.0
      NonZeroLong(3L) % NonZeroInt(3) shouldEqual 0
      NonZeroLong(3L) % NonZeroLong(3L) shouldEqual 0L
      NonZeroLong(3L) % NonZeroFloat(3.0f) shouldEqual 0.0f
      NonZeroLong(3L) % NonZeroDouble(3.0) shouldEqual 0.0
      NonZeroLong(3L) % PosFiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroLong(3L) % PosFiniteDouble(3.0) shouldEqual 0.0
      NonZeroLong(3L) % PosZFiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroLong(3L) % PosZFiniteDouble(3.0) shouldEqual 0.0
      NonZeroLong(3L) % NegFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroLong(3L) % NegFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroLong(3L) % NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroLong(3L) % NegZFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroLong(3L) % FiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroLong(3L) % FiniteDouble(3.0) shouldEqual 0.0
    }

    it("should return the same type from its unary_+ method") {
      +NonZeroLong(3L) shouldEqual NonZeroLong(3L)
    }

    it("should offer a unary + method that is consistent with Long") {
      forAll { (p: NonZeroLong) =>
        (+p).toLong shouldEqual (+(p.toLong))
      }
    }

    it("should offer a unary - method that is consistent with Long") {
      forAll { (p: NonZeroLong) =>
        if (typeName.endsWith("Char"))
          (-p).toLong should not equal (-(p.toLong))
        else
          (-p).toLong shouldEqual (-(p.toLong))
      }
    }

    it("should offer '<' comparison that is consistent with Long") {
      forAll { (p: NonZeroLong, byte: Byte) =>
        (p < byte) shouldEqual (p.toLong < byte)
      }
      forAll { (p: NonZeroLong, short: Short) =>
        (p < short) shouldEqual (p.toLong < short)
      }
      forAll { (p: NonZeroLong, char: Char) =>
        (p < char) shouldEqual (p.toLong < char)
      }
      forAll { (p: NonZeroLong, int: Int) =>
        (p < int) shouldEqual (p.toLong < int)
      }
      forAll { (p: NonZeroLong, long: Long) =>
        (p < long) shouldEqual (p.toLong < long)
      }
      forAll { (p: NonZeroLong, float: Float) =>
        (p < float) shouldEqual (p.toLong < float)
      }
      forAll { (p: NonZeroLong, double: Double) =>
        (p < double) shouldEqual (p.toLong < double)
      }
    }

    it("should offer '<=' comparison that is consistent with Long") {
      forAll { (p: NonZeroLong, byte: Byte) =>
        (p <= byte) shouldEqual (p.toLong <= byte)
      }
      forAll { (p: NonZeroLong, short: Short) =>
        (p <= short) shouldEqual (p.toLong <= short)
      }
      forAll { (p: NonZeroLong, char: Char) =>
        (p <= char) shouldEqual (p.toLong <= char)
      }
      forAll { (p: NonZeroLong, int: Int) =>
        (p <= int) shouldEqual (p.toLong <= int)
      }
      forAll { (p: NonZeroLong, long: Long) =>
        (p <= long) shouldEqual (p.toLong <= long)
      }
      forAll { (p: NonZeroLong, float: Float) =>
        (p <= float) shouldEqual (p.toLong <= float)
      }
      forAll { (p: NonZeroLong, double: Double) =>
        (p <= double) shouldEqual (p.toLong <= double)
      }
    }

    it("should offer '>' comparison that is consistent with Long") {
      forAll { (p: NonZeroLong, byte: Byte) =>
        (p > byte) shouldEqual (p.toLong > byte)
      }
      forAll { (p: NonZeroLong, short: Short) =>
        (p > short) shouldEqual (p.toLong > short)
      }
      forAll { (p: NonZeroLong, char: Char) =>
        (p > char) shouldEqual (p.toLong > char)
      }
      forAll { (p: NonZeroLong, int: Int) =>
        (p > int) shouldEqual (p.toLong > int)
      }
      forAll { (p: NonZeroLong, long: Long) =>
        (p > long) shouldEqual (p.toLong > long)
      }
      forAll { (p: NonZeroLong, float: Float) =>
        (p > float) shouldEqual (p.toLong > float)
      }
      forAll { (p: NonZeroLong, double: Double) =>
       (p > double) shouldEqual (p.toLong > double)
      }
    }

    it("should offer '>=' comparison that is consistent with Long") {
      forAll { (p: NonZeroLong, byte: Byte) =>
        (p >= byte) shouldEqual (p.toLong >= byte)
      }
      forAll { (p: NonZeroLong, short: Short) =>
        (p >= short) shouldEqual (p.toLong >= short)
      }
      forAll { (p: NonZeroLong, char: Char) =>
        (p >= char) shouldEqual (p.toLong >= char)
      }
      forAll { (p: NonZeroLong, int: Int) =>
        (p >= int) shouldEqual (p.toLong >= int)
      }
      forAll { (p: NonZeroLong, long: Long) =>
        (p >= long) shouldEqual (p.toLong >= long)
      }
      forAll { (p: NonZeroLong, float: Float) =>
        (p >= float) shouldEqual (p.toLong >= float)
      }
      forAll { (p: NonZeroLong, double: Double) =>
        (p >= double) shouldEqual (p.toLong >= double)
      }
    }

    it("should offer a '+' method that is consistent with Long") {
      forAll { (p: NonZeroLong, byte: Byte) =>
        (p + byte) shouldEqual (p.toLong + byte)
      }
      forAll { (p: NonZeroLong, char: Char) =>
        (p + char) shouldEqual (p.toLong + char)
      }
      forAll { (p: NonZeroLong, short: Short) =>
        (p + short) shouldEqual (p.toLong + short)
      }
      forAll { (p: NonZeroLong, int: Int) =>
        (p + int) shouldEqual (p.toLong + int)
      }
      forAll { (p: NonZeroLong, long: Long) =>
        (p + long) shouldEqual (p.toLong + long)
      }
      forAll { (p: NonZeroLong, float: Float) =>
        (p + float) shouldEqual (p.toLong + float)
      }
      forAll { (p: NonZeroLong, double: Double) =>
        (p + double) shouldEqual (p.toLong + double)
      }
    }

    it("should offer a '-' method that is consistent with Long") {
      forAll { (p: NonZeroLong, byte: Byte) =>
        (p - byte) shouldEqual (p.toLong - byte)
      }
      forAll { (p: NonZeroLong, short: Short) =>
        (p - short) shouldEqual (p.toLong - short)
      }
      forAll { (p: NonZeroLong, byte: Char) =>
        (p - byte) shouldEqual (p.toLong - byte)
      }
      forAll { (p: NonZeroLong, int: Int) =>
        (p - int) shouldEqual (p.toLong - int)
      }
      forAll { (p: NonZeroLong, long: Long) =>
        (p - long) shouldEqual (p.toLong - long)
      }
      forAll { (p: NonZeroLong, float: Float) =>
        val x = p - float
        val y = p.toLong - float
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroLong, double: Double) =>
        val x = p - double
        val y = p.toLong - double
        areEqualForgivingNaNs(x, y)
      }
    }

    it("should offer a '*' method that is consistent with Long") {
      forAll { (p: NonZeroLong, byte: Byte) =>
        val x = p * byte
        val y = p.toLong * byte
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroLong, short: Short) =>
        val x = p * short
        val y = p.toLong * short
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroLong, char: Char) =>
        val x = p * char
        val y = p.toLong * char
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroLong, int: Int) =>
        val x = p * int
        val y = p.toLong * int
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroLong, long: Long) =>
        val x = p * long
        val y = p.toLong * long
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroLong, float: Float) =>
        val x = p * float
        val y = p.toLong * float
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroLong, double: Double) =>
        val x = p * double
        val y = p.toLong * double
        areEqualForgivingNaNs(x, y)
      }
    }

    it("should offer a '/' method that is consistent with Long") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (p: NonZeroLong, byte: Byte) =>
        Try(p / byte) shouldEqual Try(p.toLong / byte)
      }
      forAll { (p: NonZeroLong, short: Short) =>
        Try(p / short) shouldEqual Try(p.toLong / short)
      }
      forAll { (p: NonZeroLong, char: Char) =>
        Try(p / char) shouldEqual Try(p.toLong / char)
      }
      forAll { (p: NonZeroLong, int: Int) =>
        Try(p / int) shouldEqual Try(p.toLong / int)
      }
      forAll { (p: NonZeroLong, long: Long) =>
        Try(p / long) shouldEqual Try(p.toLong / long)
      }
      forAll { (p: NonZeroLong, float: Float) =>
        Try(p / float) shouldEqual Try(p.toLong / float)
      }
      forAll { (p: NonZeroLong, double: Double) =>
        Try(p / double) shouldEqual Try(p.toLong / double)
      }
    }

    it("should offer a '%' method that is consistent with Long") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (p: NonZeroLong, byte: Byte) =>
        Try(p % byte) shouldEqual Try(p.toLong % byte)
      }
      forAll { (p: NonZeroLong, short: Short) =>
        Try(p % short) shouldEqual Try(p.toLong % short)
      }
      forAll { (p: NonZeroLong, char: Char) =>
        Try(p % char) shouldEqual Try(p.toLong % char)
      }
      forAll { (p: NonZeroLong, int: Int) =>
        Try(p % int) shouldEqual Try(p.toLong % int)
      }
      forAll { (p: NonZeroLong, long: Long) =>
        Try(p % long) shouldEqual Try(p.toLong % long)
      }
      forAll { (p: NonZeroLong, float: Float) =>
        Try(p % float) shouldEqual Try(p.toLong % float)
      }
      forAll { (p: NonZeroLong, double: Double) =>
        Try(p % double) shouldEqual Try(p.toLong % double)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Long") {
      forAll { (p1: NonZeroLong, p2: NonZeroLong) =>
        p1.max(p2).toLong shouldEqual p1.toLong.max(p2.toLong)
        p1.min(p2).toLong shouldEqual p1.toLong.min(p2.toLong)
      }
    }

    it("should offer widening methods for basic types that are consistent with Long") {
      forAll { (p: NonZeroLong) =>
        def widen(value: Long): Long = value
        widen(p) shouldEqual widen(p.toLong)
      }
             
      forAll { (p: NonZeroLong) =>
        def widen(value: Float): Float = value
        widen(p.toFloat) shouldEqual widen(p.toLong)
      }
             
      forAll { (p: NonZeroLong) =>
        def widen(value: Double): Double = value
        widen(p.toDouble) shouldEqual widen(p.toLong)
      }
             forAll { (p: NonZeroLong) =>
        def widen(value: NonZeroFloat): NonZeroFloat = value
        widen(p) shouldEqual widen(NonZeroFloat.from(p.toLong).get)
      }
             
      forAll { (p: NonZeroLong) =>
        def widen(value: NonZeroDouble): NonZeroDouble = value
        widen(p) shouldEqual widen(NonZeroDouble.from(p.toLong).get)
      }
             
    }

  }

}
