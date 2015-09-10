package org.scalactic

import org.scalatest.FunSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks._

/**
 * Generators of ordered pairs with various properties.
 */
class OrderedGenSpec extends FunSpec {

  describe("OrderedGen.orderedLongs") {
    it("should generate only Longs where the first value is less than the second value") {
      forAll(OrderedGen.orderedLongs) { (pair: (Long, Long)) =>
        assert(pair._1 < pair._2)
      }
    }
  }
  describe("OrderedGen.orderedLongsEq") {
    it("should generate only Longs where the first value is less than or equal to the second value") {
      forAll(OrderedGen.orderedLongsEq) { (pair: (Long, Long)) =>
        assert(pair._1 <= pair._2)
      }
    }
  }

  describe("OrderedGen.orderedInts") {
    it("should generate only Ints where the first value is less than the second value") {
      forAll(OrderedGen.orderedInts) { (pair: (Int, Int)) =>
        assert(pair._1 < pair._2)
      }
    }
  }
  describe("OrderedGen.orderedIntsEq") {
    it("should generate only Ints where the first value is less than or equal to the second value") {
      forAll(OrderedGen.orderedIntsEq) { (pair: (Int, Int)) =>
        assert(pair._1 <= pair._2)
      }
    }
  }

  describe("OrderedGen.orderedChars") {
    it("should generate only Chars where the first value is less than the second value") {
      forAll(OrderedGen.orderedChars) { (pair: (Char, Char)) =>
        assert(pair._1 < pair._2)
      }
    }
  }
  describe("OrderedGen.orderedCharsEq") {
    it("should generate only Chars where the first value is less than or equal to the second value") {
      forAll(OrderedGen.orderedCharsEq) { (pair: (Char, Char)) =>
        assert(pair._1 <= pair._2)
      }
    }
  }

  describe("OrderedGen.orderedBytes") {
    it("should generate only Bytes where the first value is less than the second value") {
      forAll(OrderedGen.orderedBytes) { (pair: (Byte, Byte)) =>
        assert(pair._1 < pair._2)
      }
    }
  }
  describe("OrderedGen.orderedBytesEq") {
    it("should generate only Bytes where the first value is less than or equal to the second value") {
      forAll(OrderedGen.orderedBytesEq) { (pair: (Byte, Byte)) =>
        assert(pair._1 <= pair._2)
      }
    }
  }

  describe("OrderedGen.orderedDoubles") {
    it("should generate only Doubles where the first value is less than the second value") {
      forAll(OrderedGen.orderedDoubles) { (pair: (Double, Double)) =>
        assert(pair._1 < pair._2)
      }
    }
  }
  describe("OrderedGen.orderedDoublesEq") {
    it("should generate only Doubles where the first value is less than or equal to the second value") {
      forAll(OrderedGen.orderedDoublesEq) { (pair: (Double, Double)) =>
        assert(pair._1 <= pair._2)
      }
    }
  }

  describe("OrderedGen.orderedFloats") {
    it("should generate only Floats where the first value is less than the second value") {
      forAll(OrderedGen.orderedFloats) { (pair: (Float, Float)) =>
        assert(pair._1 < pair._2)
      }
    }
  }
  describe("OrderedGen.orderedFloatsEq") {
    it("should generate only Floats where the first value is less than or equal to the second value") {
      forAll(OrderedGen.orderedFloatsEq) { (pair: (Float, Float)) =>
        assert(pair._1 <= pair._2)
      }
    }
  }
}
