package org.scalactic.algebra

import org.scalacheck._

import org.scalactic.{Equality, CheckedEquality}
import org.scalatest._
import org.scalatest.Matchers._

import org.scalatest.prop.GeneratorDrivenPropertyChecks._

import scala.language.higherKinds

/**
 *
 */

class FunctorLaws[Context[_]](implicit functor: Functor[Context],
  arbCa: Arbitrary[Context[Int]],
  shrCa: Shrink[Context[Int]],
  arbAb: Arbitrary[Int => String],
  shrAb: Shrink[Int => String],
  arbBc: Arbitrary[String => Double],
  shrBc: Shrink[String => Double],
  aqCa: Equality[Context[Int]]) extends Laws("functor") {

  def id(): Fact = {
    val lawName = "identity"
    forAll { (ca: Context[Int]) =>
      //if (true) {
      if (functor(ca).map(identity[Int]) !== ca) {
        return Laws.no(lawsName, lawName)
      }
    }
    Laws.yes(lawsName, lawName)
  }

  def composition(): Fact = {
    val lawName = "composition"
    // (ca map f map g) should be equal to (ca map (g(f))
    forAll { (ca: Context[Int], ab: Int => String, bc: String => Double) =>
      if ((functor(functor(ca) map ab) map bc) !== (functor(ca) map (bc compose ab)))
        return Laws.no(lawsName, lawName)
    }
    Laws.yes(lawsName, lawName)
  }

  override def test = id() && composition()

}
