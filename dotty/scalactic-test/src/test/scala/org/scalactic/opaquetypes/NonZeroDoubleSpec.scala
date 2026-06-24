package org.scalactic.opaquetypes

import org.scalactic.Equality
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.util.Try

import org.scalactic.opaquetypes.NonZeroDoubles.NonZeroDouble
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}
import scala.util.{Failure, Success}

trait NonZeroDoubleSpecSupport {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>  // This is because in scala.js x/0 results to NaN not ArithmetricException like in jvm, and we need to make sure Success(NaN) == Success(NaN) is true to pass the test.
        b match {
          case Success(bDouble: Double) if bDouble.isNaN => true
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

class NonZeroDoubleSpec extends funspec.AnyFunSpec with matchers.should.Matchers with OptionValues with GeneratorDrivenPropertyChecks with NonZeroDoubleSpecSupport {

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

  val typeName = "NonZeroDouble"

  describe("NonZeroDouble") {

    it("should be automatically widened to compatible AnyVal targets") {
      (NonZeroDouble(3): Double) shouldEqual 3.0

      "(NonZeroDouble(3): Int)" shouldNot compile
      "(NonZeroDouble(3): Long)" shouldNot compile
      "(NonZeroDouble(3): Float)" shouldNot compile
      "(NonZeroDouble(3): PosInt)" shouldNot compile
      "(NonZeroDouble(3): PosLong)" shouldNot compile
      "(NonZeroDouble(3): PosFloat)" shouldNot compile
      "(NonZeroDouble(3): PosDouble)" shouldNot compile
      "(NonZeroDouble(3): PosZInt)" shouldNot compile
      "(NonZeroDouble(3): PosZLong)" shouldNot compile
      "(NonZeroDouble(3): PosZFloat)" shouldNot compile
      "(NonZeroDouble(3): PosZDouble)" shouldNot compile
      "(NonZeroDouble(3): NonZeroFiniteFloat)" shouldNot compile
      "(NonZeroDouble(3): NonZeroFiniteDouble)" shouldNot compile
      "(NonZeroDouble(3): NonZeroInt)" shouldNot compile
      "(NonZeroDouble(3): NonZeroLong)" shouldNot compile
      "(NonZeroDouble(3): NonZeroFloat)" shouldNot compile
      "(NonZeroDouble(3): PosFiniteFloat)" shouldNot compile
      "(NonZeroDouble(3): PosFiniteDouble)" shouldNot compile
      "(NonZeroDouble(3): PosZFiniteFloat)" shouldNot compile
      "(NonZeroDouble(3): PosZFiniteDouble)" shouldNot compile
      "(NonZeroDouble(3): NegFiniteFloat)" shouldNot compile
      "(NonZeroDouble(3): NegFiniteDouble)" shouldNot compile
      "(NonZeroDouble(3): NegZFiniteFloat)" shouldNot compile
      "(NonZeroDouble(3): NegZFiniteDouble)" shouldNot compile
      "(NonZeroDouble(3): FiniteFloat)" shouldNot compile
      "(NonZeroDouble(3): FiniteDouble)" shouldNot compile
    }

    it("when a compatible AnyVal is passed to a + method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroDouble(3.0) + 3 shouldEqual 6
      NonZeroDouble(3.0) + 3L shouldEqual 6L
      NonZeroDouble(3.0) + 3.0f shouldEqual 6.0f
      NonZeroDouble(3.0) + 3.0 shouldEqual 6.0
      NonZeroDouble(3.0) + PosInt(3) shouldEqual 6
      NonZeroDouble(3.0) + PosLong(3L) shouldEqual 6L
      NonZeroDouble(3.0) + PosFloat(3.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) + PosDouble(3.0) shouldEqual 6.0
      NonZeroDouble(3.0) + PosZInt(3) shouldEqual 6
      NonZeroDouble(3.0) + PosZLong(3L) shouldEqual 6L
      NonZeroDouble(3.0) + PosZFloat(3.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) + PosZDouble(3.0) shouldEqual 6.0
      NonZeroDouble(3.0) + NonZeroFiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) + NonZeroFiniteDouble(3.0) shouldEqual 6.0
      NonZeroDouble(3.0) + NonZeroInt(3) shouldEqual 6
      NonZeroDouble(3.0) + NonZeroLong(3L) shouldEqual 6L
      NonZeroDouble(3.0) + NonZeroFloat(3.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) + NonZeroDouble(3.0) shouldEqual 6.0
      NonZeroDouble(3.0) + PosFiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) + PosFiniteDouble(3.0) shouldEqual 6.0
      NonZeroDouble(3.0) + PosZFiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) + PosZFiniteDouble(3.0) shouldEqual 6.0
      NonZeroDouble(3.0) + NegFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroDouble(3.0) + NegFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroDouble(3.0) + NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroDouble(3.0) + NegZFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroDouble(3.0) + FiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) + FiniteDouble(3.0) shouldEqual 6.0
    }

    it("when a compatible AnyVal is passed to a - method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroDouble(3.0) - 2 shouldEqual 1
      NonZeroDouble(3.0) - 2L shouldEqual 1L
      NonZeroDouble(3.0) - 2.0f shouldEqual 1.0f
      NonZeroDouble(3.0) - 2.0 shouldEqual 1.0
      NonZeroDouble(3.0) - PosInt(2) shouldEqual 1
      NonZeroDouble(3.0) - PosLong(2L) shouldEqual 1L
      NonZeroDouble(3.0) - PosFloat(2.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) - PosDouble(2.0) shouldEqual 1.0
      NonZeroDouble(3.0) - PosZInt(2) shouldEqual 1
      NonZeroDouble(3.0) - PosZLong(2L) shouldEqual 1L
      NonZeroDouble(3.0) - PosZFloat(2.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) - PosZDouble(2.0) shouldEqual 1.0
      NonZeroDouble(3.0) - NonZeroFiniteFloat(2.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) - NonZeroFiniteDouble(2.0) shouldEqual 1.0
      NonZeroDouble(3.0) - NonZeroInt(2) shouldEqual 1
      NonZeroDouble(3.0) - NonZeroLong(2L) shouldEqual 1L
      NonZeroDouble(3.0) - NonZeroFloat(2.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) - NonZeroDouble(2.0) shouldEqual 1.0
      NonZeroDouble(3.0) - PosFiniteFloat(2.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) - PosFiniteDouble(2.0) shouldEqual 1.0
      NonZeroDouble(3.0) - PosZFiniteFloat(2.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) - PosZFiniteDouble(2.0) shouldEqual 1.0
      NonZeroDouble(3.0) - NegFiniteFloat(-2.0f) shouldEqual 5.0f
      NonZeroDouble(3.0) - NegFiniteDouble(-2.0) shouldEqual 5.0
      NonZeroDouble(3.0) - NegZFiniteFloat(-2.0f) shouldEqual 5.0f
      NonZeroDouble(3.0) - NegZFiniteDouble(-2.0) shouldEqual 5.0
      NonZeroDouble(3.0) - FiniteFloat(2.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) - FiniteDouble(2.0) shouldEqual 1.0
    }

    it("when a compatible AnyVal is passed to a * method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroDouble(3.0) * 2 shouldEqual 6
      NonZeroDouble(3.0) * 2L shouldEqual 6L
      NonZeroDouble(3.0) * 2.0f shouldEqual 6.0f
      NonZeroDouble(3.0) * 2.0 shouldEqual 6.0
      NonZeroDouble(3.0) * PosInt(2) shouldEqual 6
      NonZeroDouble(3.0) * PosLong(2L) shouldEqual 6L
      NonZeroDouble(3.0) * PosFloat(2.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) * PosDouble(2.0) shouldEqual 6.0
      NonZeroDouble(3.0) * PosZInt(2) shouldEqual 6
      NonZeroDouble(3.0) * PosZLong(2L) shouldEqual 6L
      NonZeroDouble(3.0) * PosZFloat(2.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) * PosZDouble(2.0) shouldEqual 6.0
      NonZeroDouble(3.0) * NonZeroFiniteFloat(2.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) * NonZeroFiniteDouble(2.0) shouldEqual 6.0
      NonZeroDouble(3.0) * NonZeroInt(2) shouldEqual 6
      NonZeroDouble(3.0) * NonZeroLong(2L) shouldEqual 6L
      NonZeroDouble(3.0) * NonZeroFloat(2.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) * NonZeroDouble(2.0) shouldEqual 6.0
      NonZeroDouble(3.0) * PosFiniteFloat(2.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) * PosFiniteDouble(2.0) shouldEqual 6.0
      NonZeroDouble(3.0) * PosZFiniteFloat(2.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) * PosZFiniteDouble(2.0) shouldEqual 6.0
      NonZeroDouble(3.0) * NegFiniteFloat(-2.0f) shouldEqual -6.0f
      NonZeroDouble(3.0) * NegFiniteDouble(-2.0) shouldEqual -6.0
      NonZeroDouble(3.0) * NegZFiniteFloat(-2.0f) shouldEqual -6.0f
      NonZeroDouble(3.0) * NegZFiniteDouble(-2.0) shouldEqual -6.0
      NonZeroDouble(3.0) * FiniteFloat(2.0f) shouldEqual 6.0f
      NonZeroDouble(3.0) * FiniteDouble(2.0) shouldEqual 6.0
    }

    it("when a compatible AnyVal is passed to a / method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroDouble(3.0) / 3 shouldEqual 1
      NonZeroDouble(3.0) / 3L shouldEqual 1L
      NonZeroDouble(3.0) / 3.0f shouldEqual 1.0f
      NonZeroDouble(3.0) / 3.0 shouldEqual 1.0
      NonZeroDouble(3.0) / PosInt(3) shouldEqual 1
      NonZeroDouble(3.0) / PosLong(3L) shouldEqual 1L
      NonZeroDouble(3.0) / PosFloat(3.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) / PosDouble(3.0) shouldEqual 1.0
      NonZeroDouble(3.0) / PosZInt(3) shouldEqual 1
      NonZeroDouble(3.0) / PosZLong(3L) shouldEqual 1L
      NonZeroDouble(3.0) / PosZFloat(3.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) / PosZDouble(3.0) shouldEqual 1.0
      NonZeroDouble(3.0) / NonZeroFiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) / NonZeroFiniteDouble(3.0) shouldEqual 1.0
      NonZeroDouble(3.0) / NonZeroInt(3) shouldEqual 1
      NonZeroDouble(3.0) / NonZeroLong(3L) shouldEqual 1L
      NonZeroDouble(3.0) / NonZeroFloat(3.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) / NonZeroDouble(3.0) shouldEqual 1.0
      NonZeroDouble(3.0) / PosFiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) / PosFiniteDouble(3.0) shouldEqual 1.0
      NonZeroDouble(3.0) / PosZFiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) / PosZFiniteDouble(3.0) shouldEqual 1.0
      NonZeroDouble(3.0) / NegFiniteFloat(-3.0f) shouldEqual -1.0f
      NonZeroDouble(3.0) / NegFiniteDouble(-3.0) shouldEqual -1.0
      NonZeroDouble(3.0) / NegZFiniteFloat(-3.0f) shouldEqual -1.0f
      NonZeroDouble(3.0) / NegZFiniteDouble(-3.0) shouldEqual -1.0
      NonZeroDouble(3.0) / FiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroDouble(3.0) / FiniteDouble(3.0) shouldEqual 1.0
    }

    it("when a compatible AnyVal is passed to a % method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroDouble(3.0) % 3 shouldEqual 0
      NonZeroDouble(3.0) % 3L shouldEqual 0L
      NonZeroDouble(3.0) % 3.0f shouldEqual 0.0f
      NonZeroDouble(3.0) % 3.0 shouldEqual 0.0
      NonZeroDouble(3.0) % PosInt(3) shouldEqual 0
      NonZeroDouble(3.0) % PosLong(3L) shouldEqual 0L
      NonZeroDouble(3.0) % PosFloat(3.0f) shouldEqual 0.0f
      NonZeroDouble(3.0) % PosDouble(3.0) shouldEqual 0.0
      NonZeroDouble(3.0) % PosZInt(3) shouldEqual 0
      NonZeroDouble(3.0) % PosZLong(3L) shouldEqual 0L
      NonZeroDouble(3.0) % PosZFloat(3.0f) shouldEqual 0.0f
      NonZeroDouble(3.0) % PosZDouble(3.0) shouldEqual 0.0
      NonZeroDouble(3.0) % NonZeroFiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroDouble(3.0) % NonZeroFiniteDouble(3.0) shouldEqual 0.0
      NonZeroDouble(3.0) % NonZeroInt(3) shouldEqual 0
      NonZeroDouble(3.0) % NonZeroLong(3L) shouldEqual 0L
      NonZeroDouble(3.0) % NonZeroFloat(3.0f) shouldEqual 0.0f
      NonZeroDouble(3.0) % NonZeroDouble(3.0) shouldEqual 0.0
      NonZeroDouble(3.0) % PosFiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroDouble(3.0) % PosFiniteDouble(3.0) shouldEqual 0.0
      NonZeroDouble(3.0) % PosZFiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroDouble(3.0) % PosZFiniteDouble(3.0) shouldEqual 0.0
      NonZeroDouble(3.0) % NegFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroDouble(3.0) % NegFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroDouble(3.0) % NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroDouble(3.0) % NegZFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroDouble(3.0) % FiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroDouble(3.0) % FiniteDouble(3.0) shouldEqual 0.0
    }

    it("should have a toString consistent with Double") {
      NonZeroDouble.from(3.0).value.toString shouldBe 3.0.toString
    }

    it("should return the same type from its unary_+ method") {
      +NonZeroDouble(3.0) shouldEqual NonZeroDouble(3.0)
    }

    it("should offer a unary + method that is consistent with Double") {
      forAll { (p: NonZeroDouble) =>
        (+p).toDouble shouldEqual (+(p.toDouble))
      }
    }

    it("should offer a unary - method that is consistent with Double") {
      forAll { (p: NonZeroDouble) =>
        if (typeName.endsWith("Char"))
          (-p).toDouble should not equal (-(p.toDouble))
        else
          (-p).toDouble shouldEqual (-(p.toDouble))
      }
    }

    it("should offer '<' comparison that is consistent with Double") {
      forAll { (p: NonZeroDouble, byte: Byte) =>
        (p < byte) shouldEqual (p.toDouble < byte)
      }
      forAll { (p: NonZeroDouble, short: Short) =>
        (p < short) shouldEqual (p.toDouble < short)
      }
      forAll { (p: NonZeroDouble, char: Char) =>
        (p < char) shouldEqual (p.toDouble < char)
      }
      forAll { (p: NonZeroDouble, int: Int) =>
        (p < int) shouldEqual (p.toDouble < int)
      }
      forAll { (p: NonZeroDouble, long: Long) =>
        (p < long) shouldEqual (p.toDouble < long)
      }
      forAll { (p: NonZeroDouble, float: Float) =>
        (p < float) shouldEqual (p.toDouble < float)
      }
      forAll { (p: NonZeroDouble, double: Double) =>
        (p < double) shouldEqual (p.toDouble < double)
      }
    }

    it("should offer '<=' comparison that is consistent with Double") {
      forAll { (p: NonZeroDouble, byte: Byte) =>
        (p <= byte) shouldEqual (p.toDouble <= byte)
      }
      forAll { (p: NonZeroDouble, short: Short) =>
        (p <= short) shouldEqual (p.toDouble <= short)
      }
      forAll { (p: NonZeroDouble, char: Char) =>
        (p <= char) shouldEqual (p.toDouble <= char)
      }
      forAll { (p: NonZeroDouble, int: Int) =>
        (p <= int) shouldEqual (p.toDouble <= int)
      }
      forAll { (p: NonZeroDouble, long: Long) =>
        (p <= long) shouldEqual (p.toDouble <= long)
      }
      forAll { (p: NonZeroDouble, float: Float) =>
        (p <= float) shouldEqual (p.toDouble <= float)
      }
      forAll { (p: NonZeroDouble, double: Double) =>
        (p <= double) shouldEqual (p.toDouble <= double)
      }
    }

    it("should offer '>' comparison that is consistent with Double") {
      forAll { (p: NonZeroDouble, byte: Byte) =>
        (p > byte) shouldEqual (p.toDouble > byte)
      }
      forAll { (p: NonZeroDouble, short: Short) =>
        (p > short) shouldEqual (p.toDouble > short)
      }
      forAll { (p: NonZeroDouble, char: Char) =>
        (p > char) shouldEqual (p.toDouble > char)
      }
      forAll { (p: NonZeroDouble, int: Int) =>
        (p > int) shouldEqual (p.toDouble > int)
      }
      forAll { (p: NonZeroDouble, long: Long) =>
        (p > long) shouldEqual (p.toDouble > long)
      }
      forAll { (p: NonZeroDouble, float: Float) =>
        (p > float) shouldEqual (p.toDouble > float)
      }
      forAll { (p: NonZeroDouble, double: Double) =>
       (p > double) shouldEqual (p.toDouble > double)
      }
    }

    it("should offer '>=' comparison that is consistent with Double") {
      forAll { (p: NonZeroDouble, byte: Byte) =>
        (p >= byte) shouldEqual (p.toDouble >= byte)
      }
      forAll { (p: NonZeroDouble, short: Short) =>
        (p >= short) shouldEqual (p.toDouble >= short)
      }
      forAll { (p: NonZeroDouble, char: Char) =>
        (p >= char) shouldEqual (p.toDouble >= char)
      }
      forAll { (p: NonZeroDouble, int: Int) =>
        (p >= int) shouldEqual (p.toDouble >= int)
      }
      forAll { (p: NonZeroDouble, long: Long) =>
        (p >= long) shouldEqual (p.toDouble >= long)
      }
      forAll { (p: NonZeroDouble, float: Float) =>
        (p >= float) shouldEqual (p.toDouble >= float)
      }
      forAll { (p: NonZeroDouble, double: Double) =>
        (p >= double) shouldEqual (p.toDouble >= double)
      }
    }

    it("should handle NaN results from + with Infinity consistently (regression test)") {
      // When p is Infinity and float/double is NaN, both p + NaN and p.toDouble + NaN produce NaN.
      // Since NaN != NaN in IEEE 754, plain shouldEqual fails. Use areEqualForgivingNaNs.
      val p: NonZeroDouble = NonZeroDouble.PositiveInfinity
      val nanF: Float = Float.NaN
      val nanD: Double = Double.NaN
      areEqualForgivingNaNs(p + nanF, p.toDouble + nanF)
      areEqualForgivingNaNs(p + nanD, p.toDouble + nanD)
    }

    it("should offer a '+' method that is consistent with Double") {
      forAll { (p: NonZeroDouble, byte: Byte) =>
        (p + byte) shouldEqual (p.toDouble + byte)
      }
      forAll { (p: NonZeroDouble, char: Char) =>
        (p + char) shouldEqual (p.toDouble + char)
      }
      forAll { (p: NonZeroDouble, short: Short) =>
        (p + short) shouldEqual (p.toDouble + short)
      }
      forAll { (p: NonZeroDouble, int: Int) =>
        (p + int) shouldEqual (p.toDouble + int)
      }
      forAll { (p: NonZeroDouble, long: Long) =>
        (p + long) shouldEqual (p.toDouble + long)
      }
      forAll { (p: NonZeroDouble, float: Float) =>
        val x = p + float
        val y = p.toDouble + float
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroDouble, double: Double) =>
        val x = p + double
        val y = p.toDouble + double
        areEqualForgivingNaNs(x, y)
      }
    }

    it("should offer a '-' method that is consistent with Double") {
      forAll { (p: NonZeroDouble, byte: Byte) =>
        (p - byte) shouldEqual (p.toDouble - byte)
      }
      forAll { (p: NonZeroDouble, short: Short) =>
        (p - short) shouldEqual (p.toDouble - short)
      }
      forAll { (p: NonZeroDouble, byte: Char) =>
        (p - byte) shouldEqual (p.toDouble - byte)
      }
      forAll { (p: NonZeroDouble, int: Int) =>
        (p - int) shouldEqual (p.toDouble - int)
      }
      forAll { (p: NonZeroDouble, long: Long) =>
        (p - long) shouldEqual (p.toDouble - long)
      }
      forAll { (p: NonZeroDouble, float: Float) =>
        val x = p - float
        val y = p.toDouble - float
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroDouble, double: Double) =>
        val x = p - double
        val y = p.toDouble - double
        areEqualForgivingNaNs(x, y)
      }
    }

    it("should offer a '*' method that is consistent with Double") {
      forAll { (p: NonZeroDouble, byte: Byte) =>
        val x = p * byte
        val y = p.toDouble * byte
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroDouble, short: Short) =>
        val x = p * short
        val y = p.toDouble * short
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroDouble, char: Char) =>
        val x = p * char
        val y = p.toDouble * char
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroDouble, int: Int) =>
        val x = p * int
        val y = p.toDouble * int
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroDouble, long: Long) =>
        val x = p * long
        val y = p.toDouble * long
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroDouble, float: Float) =>
        val x = p * float
        val y = p.toDouble * float
        areEqualForgivingNaNs(x, y)
      }
      forAll { (p: NonZeroDouble, double: Double) =>
        val x = p * double
        val y = p.toDouble * double
        areEqualForgivingNaNs(x, y)
      }
    }

    it("should offer a '/' method that is consistent with Double") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (p: NonZeroDouble, byte: Byte) =>
        Try(p / byte) shouldEqual Try(p.toDouble / byte)
      }
      forAll { (p: NonZeroDouble, short: Short) =>
        Try(p / short) shouldEqual Try(p.toDouble / short)
      }
      forAll { (p: NonZeroDouble, char: Char) =>
        Try(p / char) shouldEqual Try(p.toDouble / char)
      }
      forAll { (p: NonZeroDouble, int: Int) =>
        Try(p / int) shouldEqual Try(p.toDouble / int)
      }
      forAll { (p: NonZeroDouble, long: Long) =>
        Try(p / long) shouldEqual Try(p.toDouble / long)
      }
      forAll { (p: NonZeroDouble, float: Float) =>
        Try(p / float) shouldEqual Try(p.toDouble / float)
      }
      forAll { (p: NonZeroDouble, double: Double) =>
        Try(p / double) shouldEqual Try(p.toDouble / double)
      }
    }

    it("should offer a '%' method that is consistent with Double") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (p: NonZeroDouble, byte: Byte) =>
        Try(p % byte) shouldEqual Try(p.toDouble % byte)
      }
      forAll { (p: NonZeroDouble, short: Short) =>
        Try(p % short) shouldEqual Try(p.toDouble % short)
      }
      forAll { (p: NonZeroDouble, char: Char) =>
        Try(p % char) shouldEqual Try(p.toDouble % char)
      }
      forAll { (p: NonZeroDouble, int: Int) =>
        Try(p % int) shouldEqual Try(p.toDouble % int)
      }
      forAll { (p: NonZeroDouble, long: Long) =>
        Try(p % long) shouldEqual Try(p.toDouble % long)
      }
      forAll { (p: NonZeroDouble, float: Float) =>
        Try(p % float) shouldEqual Try(p.toDouble % float)
      }
      forAll { (p: NonZeroDouble, double: Double) =>
        Try(p % double) shouldEqual Try(p.toDouble % double)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      forAll { (p1: NonZeroDouble, p2: NonZeroDouble) =>
        p1.max(p2).toDouble shouldEqual p1.toDouble.max(p2.toDouble)
        p1.min(p2).toDouble shouldEqual p1.toDouble.min(p2.toDouble)
      }
    }

    it("should offer widening methods for basic types that are consistent with Double") {
      forAll { (p: NonZeroDouble) =>
        def widen(value: Double): Double = value
        widen(p) shouldEqual widen(p.toDouble)
      }
             
    }

  }

}
