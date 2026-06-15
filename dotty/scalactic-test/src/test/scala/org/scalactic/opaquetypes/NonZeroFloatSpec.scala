package org.scalactic.opaquetypes

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.util.Try
import scala.util.{Try, Success, Failure}
import org.scalactic.Equality

import org.scalactic.opaquetypes.NonZeroFloats.NonZeroFloat

trait NonZeroFloatSpecSupport {
  implicit def tryEqualityForFloat[T]: Equality[Try[T]] = new Equality[Try[T]] {
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
}

class NonZeroFloatSpec extends funspec.AnyFunSpec with matchers.should.Matchers with OptionValues with GeneratorDrivenPropertyChecks with NonZeroFloatSpecSupport {

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

  val typeName = "NonZeroFloat"

  describe("NonZeroFloat") {

    it("should be automatically widened to compatible AnyVal targets") {
      (NonZeroFloat(3): Float) shouldEqual 3.0f
      (NonZeroFloat(3).toDouble: Double) shouldEqual 3.0
      (NonZeroFloat(3.0f): NonZeroDouble) shouldEqual NonZeroDouble(3.0)
      "(NonZeroFloat(3): Int)" shouldNot compile
      "(NonZeroFloat(3): Long)" shouldNot compile
      "(NonZeroFloat(3): PosInt)" shouldNot compile
      "(NonZeroFloat(3): PosLong)" shouldNot compile
      "(NonZeroFloat(3): PosFloat)" shouldNot compile
      "(NonZeroFloat(3): PosDouble)" shouldNot compile
      "(NonZeroFloat(3): PosZInt)" shouldNot compile
      "(NonZeroFloat(3): PosZLong)" shouldNot compile
      "(NonZeroFloat(3): PosZFloat)" shouldNot compile
      "(NonZeroFloat(3): PosZDouble)" shouldNot compile
      "(NonZeroFloat(3): NonZeroFiniteFloat)" shouldNot compile
      "(NonZeroFloat(3): NonZeroFiniteDouble)" shouldNot compile
      "(NonZeroFloat(3): NonZeroInt)" shouldNot compile
      "(NonZeroFloat(3): NonZeroLong)" shouldNot compile
      "(NonZeroFloat(3): PosFiniteFloat)" shouldNot compile
      "(NonZeroFloat(3): PosFiniteDouble)" shouldNot compile
      "(NonZeroFloat(3): PosZFiniteFloat)" shouldNot compile
      "(NonZeroFloat(3): PosZFiniteDouble)" shouldNot compile
      "(NonZeroFloat(3): NegFiniteFloat)" shouldNot compile
      "(NonZeroFloat(3): NegFiniteDouble)" shouldNot compile
      "(NonZeroFloat(3): NegZFiniteFloat)" shouldNot compile
      "(NonZeroFloat(3): NegZFiniteDouble)" shouldNot compile
      "(NonZeroFloat(3): FiniteFloat)" shouldNot compile
      "(NonZeroFloat(3): FiniteDouble)" shouldNot compile
    }

    it("when a compatible AnyVal is passed to a + method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroFloat(3.0f) + 3 shouldEqual 6.0f
      NonZeroFloat(3.0f) + 3L shouldEqual 6.0f
      NonZeroFloat(3.0f) + 3.0f shouldEqual 6.0f
      NonZeroFloat(3.0f) + 3.0 shouldEqual 6.0
      NonZeroFloat(3.0f) + PosInt(3) shouldEqual 6.0f
      NonZeroFloat(3.0f) + PosLong(3L) shouldEqual 6.0f
      NonZeroFloat(3.0f) + PosFloat(3.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) + PosDouble(3.0) shouldEqual 6.0
      NonZeroFloat(3.0f) + PosZInt(3) shouldEqual 6.0f
      NonZeroFloat(3.0f) + PosZLong(3L) shouldEqual 6.0f
      NonZeroFloat(3.0f) + PosZFloat(3.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) + PosZDouble(3.0) shouldEqual 6.0
      NonZeroFloat(3.0f) + NonZeroFiniteFloat.ensuringValid(3.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) + NonZeroFiniteDouble.ensuringValid(3.0) shouldEqual 6.0
      NonZeroFloat(3.0f) + NonZeroInt(3) shouldEqual 6.0f
      NonZeroFloat(3.0f) + NonZeroLong(3L) shouldEqual 6.0f
      NonZeroFloat(3.0f) + NonZeroFloat(3.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) + NonZeroDouble(3.0) shouldEqual 6.0
      NonZeroFloat(3.0f) + PosFiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) + PosFiniteDouble(3.0) shouldEqual 6.0
      NonZeroFloat(3.0f) + PosZFiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) + PosZFiniteDouble(3.0) shouldEqual 6.0
      NonZeroFloat(3.0f) + NegFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloat(3.0f) + NegFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroFloat(3.0f) + NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloat(3.0f) + NegZFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroFloat(3.0f) + FiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) + FiniteDouble(3.0) shouldEqual 6.0
    }

    it("when a compatible AnyVal is passed to a - method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroFloat(3.0f) - 2 shouldEqual 1.0f
      NonZeroFloat(3.0f) - 2L shouldEqual 1.0f
      NonZeroFloat(3.0f) - 2.0f shouldEqual 1.0f
      NonZeroFloat(3.0f) - 2.0 shouldEqual 1.0
      NonZeroFloat(3.0f) - PosInt(2) shouldEqual 1.0f
      NonZeroFloat(3.0f) - PosLong(2L) shouldEqual 1.0f
      NonZeroFloat(3.0f) - PosFloat(2.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) - PosDouble(2.0) shouldEqual 1.0
      NonZeroFloat(3.0f) - PosZInt(2) shouldEqual 1.0f
      NonZeroFloat(3.0f) - PosZLong(2L) shouldEqual 1.0f
      NonZeroFloat(3.0f) - PosZFloat(2.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) - PosZDouble(2.0) shouldEqual 1.0
      NonZeroFloat(3.0f) - NonZeroFiniteFloat.ensuringValid(2.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) - NonZeroFiniteDouble.ensuringValid(2.0) shouldEqual 1.0
      NonZeroFloat(3.0f) - NonZeroInt(2) shouldEqual 1.0f
      NonZeroFloat(3.0f) - NonZeroLong(2L) shouldEqual 1.0f
      NonZeroFloat(3.0f) - NonZeroFloat(2.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) - NonZeroDouble(2.0) shouldEqual 1.0
      NonZeroFloat(3.0f) - PosFiniteFloat(2.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) - PosFiniteDouble(2.0) shouldEqual 1.0
      NonZeroFloat(3.0f) - PosZFiniteFloat(2.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) - PosZFiniteDouble(2.0) shouldEqual 1.0
      NonZeroFloat(3.0f) - NegFiniteFloat(-2.0f) shouldEqual 5.0f
      NonZeroFloat(3.0f) - NegFiniteDouble(-2.0) shouldEqual 5.0
      NonZeroFloat(3.0f) - NegZFiniteFloat(-2.0f) shouldEqual 5.0f
      NonZeroFloat(3.0f) - NegZFiniteDouble(-2.0) shouldEqual 5.0
      NonZeroFloat(3.0f) - FiniteFloat(2.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) - FiniteDouble(2.0) shouldEqual 1.0
    }

    it("when a compatible AnyVal is passed to a * method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroFloat(3.0f) * 2 shouldEqual 6.0f
      NonZeroFloat(3.0f) * 2L shouldEqual 6.0f
      NonZeroFloat(3.0f) * 2.0f shouldEqual 6.0f
      NonZeroFloat(3.0f) * 2.0 shouldEqual 6.0
      NonZeroFloat(3.0f) * PosInt(2) shouldEqual 6.0f
      NonZeroFloat(3.0f) * PosLong(2L) shouldEqual 6.0f
      NonZeroFloat(3.0f) * PosFloat(2.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) * PosDouble(2.0) shouldEqual 6.0
      NonZeroFloat(3.0f) * PosZInt(2) shouldEqual 6.0f
      NonZeroFloat(3.0f) * PosZLong(2L) shouldEqual 6.0f
      NonZeroFloat(3.0f) * PosZFloat(2.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) * PosZDouble(2.0) shouldEqual 6.0
      NonZeroFloat(3.0f) * NonZeroFiniteFloat.ensuringValid(2.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) * NonZeroFiniteDouble.ensuringValid(2.0) shouldEqual 6.0
      NonZeroFloat(3.0f) * NonZeroInt(2) shouldEqual 6.0f
      NonZeroFloat(3.0f) * NonZeroLong(2L) shouldEqual 6.0f
      NonZeroFloat(3.0f) * NonZeroFloat(2.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) * NonZeroDouble(2.0) shouldEqual 6.0
      NonZeroFloat(3.0f) * PosFiniteFloat(2.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) * PosFiniteDouble(2.0) shouldEqual 6.0
      NonZeroFloat(3.0f) * PosZFiniteFloat(2.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) * PosZFiniteDouble(2.0) shouldEqual 6.0
      NonZeroFloat(3.0f) * NegFiniteFloat(-2.0f) shouldEqual -6.0f
      NonZeroFloat(3.0f) * NegFiniteDouble(-2.0) shouldEqual -6.0
      NonZeroFloat(3.0f) * NegZFiniteFloat(-2.0f) shouldEqual -6.0f
      NonZeroFloat(3.0f) * NegZFiniteDouble(-2.0) shouldEqual -6.0
      NonZeroFloat(3.0f) * FiniteFloat(2.0f) shouldEqual 6.0f
      NonZeroFloat(3.0f) * FiniteDouble(2.0) shouldEqual 6.0
    }

    it("when a compatible AnyVal is passed to a / method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroFloat(3.0f) / 3 shouldEqual 1.0f
      NonZeroFloat(3.0f) / 3L shouldEqual 1.0f
      NonZeroFloat(3.0f) / 3.0f shouldEqual 1.0f
      NonZeroFloat(3.0f) / 3.0 shouldEqual 1.0
      NonZeroFloat(3.0f) / PosInt(3) shouldEqual 1.0f
      NonZeroFloat(3.0f) / PosLong(3L) shouldEqual 1.0f
      NonZeroFloat(3.0f) / PosFloat(3.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) / PosDouble(3.0) shouldEqual 1.0
      NonZeroFloat(3.0f) / PosZInt(3) shouldEqual 1.0f
      NonZeroFloat(3.0f) / PosZLong(3L) shouldEqual 1.0f
      NonZeroFloat(3.0f) / PosZFloat(3.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) / PosZDouble(3.0) shouldEqual 1.0
      NonZeroFloat(3.0f) / NonZeroFiniteFloat.ensuringValid(3.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) / NonZeroFiniteDouble.ensuringValid(3.0) shouldEqual 1.0
      NonZeroFloat(3.0f) / NonZeroInt(3) shouldEqual 1.0f
      NonZeroFloat(3.0f) / NonZeroLong(3L) shouldEqual 1.0f
      NonZeroFloat(3.0f) / NonZeroFloat(3.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) / NonZeroDouble(3.0) shouldEqual 1.0
      NonZeroFloat(3.0f) / PosFiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) / PosFiniteDouble(3.0) shouldEqual 1.0
      NonZeroFloat(3.0f) / PosZFiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) / PosZFiniteDouble(3.0) shouldEqual 1.0
      NonZeroFloat(3.0f) / NegFiniteFloat(-3.0f) shouldEqual -1.0f
      NonZeroFloat(3.0f) / NegFiniteDouble(-3.0) shouldEqual -1.0
      NonZeroFloat(3.0f) / NegZFiniteFloat(-3.0f) shouldEqual -1.0f
      NonZeroFloat(3.0f) / NegZFiniteDouble(-3.0) shouldEqual -1.0
      NonZeroFloat(3.0f) / FiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroFloat(3.0f) / FiniteDouble(3.0) shouldEqual 1.0
    }

    it("when a compatible AnyVal is passed to a % method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroFloat(3.0f) % 3 shouldEqual 0.0f
      NonZeroFloat(3.0f) % 3L shouldEqual 0.0f
      NonZeroFloat(3.0f) % 3.0f shouldEqual 0.0f
      NonZeroFloat(3.0f) % 3.0 shouldEqual 0.0
      NonZeroFloat(3.0f) % PosInt(3) shouldEqual 0.0f
      NonZeroFloat(3.0f) % PosLong(3L) shouldEqual 0.0f
      NonZeroFloat(3.0f) % PosFloat(3.0f) shouldEqual 0.0f
      NonZeroFloat(3.0f) % PosDouble(3.0) shouldEqual 0.0
      NonZeroFloat(3.0f) % PosZInt(3) shouldEqual 0.0f
      NonZeroFloat(3.0f) % PosZLong(3L) shouldEqual 0.0f
      NonZeroFloat(3.0f) % PosZFloat(3.0f) shouldEqual 0.0f
      NonZeroFloat(3.0f) % PosZDouble(3.0) shouldEqual 0.0
      NonZeroFloat(3.0f) % NonZeroFiniteFloat.ensuringValid(3.0f) shouldEqual 0.0f
      NonZeroFloat(3.0f) % NonZeroFiniteDouble.ensuringValid(3.0) shouldEqual 0.0
      NonZeroFloat(3.0f) % NonZeroInt(3) shouldEqual 0.0f
      NonZeroFloat(3.0f) % NonZeroLong(3L) shouldEqual 0.0f
      NonZeroFloat(3.0f) % NonZeroFloat(3.0f) shouldEqual 0.0f
      NonZeroFloat(3.0f) % NonZeroDouble(3.0) shouldEqual 0.0
      NonZeroFloat(3.0f) % PosFiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroFloat(3.0f) % PosFiniteDouble(3.0) shouldEqual 0.0
      NonZeroFloat(3.0f) % PosZFiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroFloat(3.0f) % PosZFiniteDouble(3.0) shouldEqual 0.0
      NonZeroFloat(3.0f) % NegFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloat(3.0f) % NegFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroFloat(3.0f) % NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloat(3.0f) % NegZFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroFloat(3.0f) % FiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroFloat(3.0f) % FiniteDouble(3.0) shouldEqual 0.0
    }

    it("should have toString same as underlying Float") {
      NonZeroFloat.from(3.0f).value.toString shouldBe 3.0f.toString
    }

    it("should return the same type from its unary_+ method") {
      +NonZeroFloat(3.0f) shouldEqual NonZeroFloat(3.0f)
    }

    it("should offer a unary + method that is consistent with Float") {
      forAll { (p: NonZeroFloat) =>
        (+p).toFloat shouldEqual (+(p.toFloat))
      }
    }

    it("should offer a unary - method that is consistent with Float") {
      forAll { (p: NonZeroFloat) =>
        if (typeName.endsWith("Char"))
          (-p).toFloat should not equal (-(p.toFloat))
        else
          (-p).toFloat shouldEqual (-(p.toFloat))
      }
    }

    it("should offer '<' comparison that is consistent with Float") {
      forAll { (p: NonZeroFloat, byte: Byte) =>
        (p < byte) shouldEqual (p.toFloat < byte)
      }
      forAll { (p: NonZeroFloat, short: Short) =>
        (p < short) shouldEqual (p.toFloat < short)
      }
      forAll { (p: NonZeroFloat, char: Char) =>
        (p < char) shouldEqual (p.toFloat < char)
      }
      forAll { (p: NonZeroFloat, int: Int) =>
        (p < int) shouldEqual (p.toFloat < int)
      }
      forAll { (p: NonZeroFloat, long: Long) =>
        (p < long) shouldEqual (p.toFloat < long)
      }
      forAll { (p: NonZeroFloat, float: Float) =>
        (p < float) shouldEqual (p.toFloat < float)
      }
      forAll { (p: NonZeroFloat, double: Double) =>
        (p < double) shouldEqual (p.toFloat < double)
      }
    }

    it("should offer '<=' comparison that is consistent with Float") {
      forAll { (p: NonZeroFloat, byte: Byte) =>
        (p <= byte) shouldEqual (p.toFloat <= byte)
      }
      forAll { (p: NonZeroFloat, short: Short) =>
        (p <= short) shouldEqual (p.toFloat <= short)
      }
      forAll { (p: NonZeroFloat, char: Char) =>
        (p <= char) shouldEqual (p.toFloat <= char)
      }
      forAll { (p: NonZeroFloat, int: Int) =>
        (p <= int) shouldEqual (p.toFloat <= int)
      }
      forAll { (p: NonZeroFloat, long: Long) =>
        (p <= long) shouldEqual (p.toFloat <= long)
      }
      forAll { (p: NonZeroFloat, float: Float) =>
        (p <= float) shouldEqual (p.toFloat <= float)
      }
      forAll { (p: NonZeroFloat, double: Double) =>
        (p <= double) shouldEqual (p.toFloat <= double)
      }
    }

    it("should offer '>' comparison that is consistent with Float") {
      forAll { (p: NonZeroFloat, byte: Byte) =>
        (p > byte) shouldEqual (p.toFloat > byte)
      }
      forAll { (p: NonZeroFloat, short: Short) =>
        (p > short) shouldEqual (p.toFloat > short)
      }
      forAll { (p: NonZeroFloat, char: Char) =>
        (p > char) shouldEqual (p.toFloat > char)
      }
      forAll { (p: NonZeroFloat, int: Int) =>
        (p > int) shouldEqual (p.toFloat > int)
      }
      forAll { (p: NonZeroFloat, long: Long) =>
        (p > long) shouldEqual (p.toFloat > long)
      }
      forAll { (p: NonZeroFloat, float: Float) =>
        (p > float) shouldEqual (p.toFloat > float)
      }
      forAll { (p: NonZeroFloat, double: Double) =>
       (p > double) shouldEqual (p.toFloat > double)
      }
    }

    it("should offer '>=' comparison that is consistent with Float") {
      forAll { (p: NonZeroFloat, byte: Byte) =>
        (p >= byte) shouldEqual (p.toFloat >= byte)
      }
      forAll { (p: NonZeroFloat, short: Short) =>
        (p >= short) shouldEqual (p.toFloat >= short)
      }
      forAll { (p: NonZeroFloat, char: Char) =>
        (p >= char) shouldEqual (p.toFloat >= char)
      }
      forAll { (p: NonZeroFloat, int: Int) =>
        (p >= int) shouldEqual (p.toFloat >= int)
      }
      forAll { (p: NonZeroFloat, long: Long) =>
        (p >= long) shouldEqual (p.toFloat >= long)
      }
      forAll { (p: NonZeroFloat, float: Float) =>
        (p >= float) shouldEqual (p.toFloat >= float)
      }
      forAll { (p: NonZeroFloat, double: Double) =>
        (p >= double) shouldEqual (p.toFloat >= double)
      }
    }

    it("should offer a '+' method that is consistent with Float") {
      forAll { (p: NonZeroFloat, byte: Byte) =>
        (p + byte) shouldEqual (p.toFloat + byte)
      }
      forAll { (p: NonZeroFloat, char: Char) =>
        (p + char) shouldEqual (p.toFloat + char)
      }
      forAll { (p: NonZeroFloat, short: Short) =>
        (p + short) shouldEqual (p.toFloat + short)
      }
      forAll { (p: NonZeroFloat, int: Int) =>
        (p + int) shouldEqual (p.toFloat + int)
      }
      forAll { (p: NonZeroFloat, long: Long) =>
        (p + long) shouldEqual (p.toFloat + long)
      }
      forAll { (p: NonZeroFloat, float: Float) =>
        (p + float) shouldEqual (p.toFloat + float)
      }
      forAll { (p: NonZeroFloat, double: Double) =>
        (p + double) shouldEqual (p.toFloat + double)
      }
    }

    it("should offer a '-' method that is consistent with Float") {
      forAll { (p: NonZeroFloat, byte: Byte) =>
        (p - byte) shouldEqual (p.toFloat - byte)
      }
      forAll { (p: NonZeroFloat, short: Short) =>
        (p - short) shouldEqual (p.toFloat - short)
      }
      forAll { (p: NonZeroFloat, byte: Char) =>
        (p - byte) shouldEqual (p.toFloat - byte)
      }
      forAll { (p: NonZeroFloat, int: Int) =>
        (p - int) shouldEqual (p.toFloat - int)
      }
      forAll { (p: NonZeroFloat, long: Long) =>
        (p - long) shouldEqual (p.toFloat - long)
      }
      forAll { (p: NonZeroFloat, float: Float) =>
        val x = p - float
        val y = p.toFloat - float
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroFloat, double: Double) =>
        val x = p - double
        val y = p.toFloat - double
        areEqualForgivingNaNs(x, y)
      }
    }

    it("should offer a '*' method that is consistent with Float") {
      forAll { (p: NonZeroFloat, byte: Byte) =>
        val x = p * byte
        val y = p.toFloat * byte
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroFloat, short: Short) =>
        val x = p * short
        val y = p.toFloat * short
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroFloat, char: Char) =>
        val x = p * char
        val y = p.toFloat * char
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroFloat, int: Int) =>
        val x = p * int
        val y = p.toFloat * int
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroFloat, long: Long) =>
        val x = p * long
        val y = p.toFloat * long
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroFloat, float: Float) =>
        val x = p * float
        val y = p.toFloat * float
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroFloat, double: Double) =>
        val x = p * double
        val y = p.toFloat * double
        areEqualForgivingNaNs(x, y)
      }
    }

    it("should offer a '/' method that is consistent with Float") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (p: NonZeroFloat, byte: Byte) =>
        Try(p / byte) shouldEqual Try(p.toFloat / byte)
      }
      forAll { (p: NonZeroFloat, short: Short) =>
        Try(p / short) shouldEqual Try(p.toFloat / short)
      }
      forAll { (p: NonZeroFloat, char: Char) =>
        Try(p / char) shouldEqual Try(p.toFloat / char)
      }
      forAll { (p: NonZeroFloat, int: Int) =>
        Try(p / int) shouldEqual Try(p.toFloat / int)
      }
      forAll { (p: NonZeroFloat, long: Long) =>
        Try(p / long) shouldEqual Try(p.toFloat / long)
      }
      forAll { (p: NonZeroFloat, float: Float) =>
        Try(p / float) shouldEqual Try(p.toFloat / float)
      }
      forAll { (p: NonZeroFloat, double: Double) =>
        Try(p / double) shouldEqual Try(p.toFloat / double)
      }
    }

    it("should offer a '%' method that is consistent with Float") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (p: NonZeroFloat, byte: Byte) =>
        Try(p % byte) shouldEqual Try(p.toFloat % byte)
      }
      forAll { (p: NonZeroFloat, short: Short) =>
        Try(p % short) shouldEqual Try(p.toFloat % short)
      }
      forAll { (p: NonZeroFloat, char: Char) =>
        Try(p % char) shouldEqual Try(p.toFloat % char)
      }
      forAll { (p: NonZeroFloat, int: Int) =>
        Try(p % int) shouldEqual Try(p.toFloat % int)
      }
      forAll { (p: NonZeroFloat, long: Long) =>
        Try(p % long) shouldEqual Try(p.toFloat % long)
      }
      forAll { (p: NonZeroFloat, float: Float) =>
        Try(p % float) shouldEqual Try(p.toFloat % float)
      }
      forAll { (p: NonZeroFloat, double: Double) =>
        Try(p % double) shouldEqual Try(p.toFloat % double)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (p1: NonZeroFloat, p2: NonZeroFloat) =>
        p1.max(p2).toFloat shouldEqual p1.toFloat.max(p2.toFloat)
        p1.min(p2).toFloat shouldEqual p1.toFloat.min(p2.toFloat)
      }
    }

    it("should offer widening methods for basic types that are consistent with Float") {
      forAll { (p: NonZeroFloat) =>
        def widen(value: Float): Float = value
        widen(p.toFloat) shouldEqual widen(p.toFloat)
      }
             
      forAll { (p: NonZeroFloat) =>
        def widen(value: Double): Double = value
        widen(p.toDouble) shouldEqual widen(p.toFloat)
      }
             forAll { (p: NonZeroFloat) =>
        def widen(value: NonZeroDouble): NonZeroDouble = value
        widen(NonZeroDouble.from(p.toFloat).get) shouldEqual widen(NonZeroDouble.from(p.toFloat).get)
      }
             
    }

  }

}
