package org.scalatest.laws

import org.scalacheck._

import org.scalactic.Equality
import org.scalactic.CheckedEquality._
import org.scalatest._

import org.scalatest.prop.GeneratorDrivenPropertyChecks._

import org.scalactic.algebra._

import Functor.adapters

import scala.language.higherKinds

class FunctorLaws[Context[_]](implicit functor: Functor[Context],
  arbCa: Arbitrary[Context[Int]],
  shrCa: Shrink[Context[Int]]) extends Laws {

  override val lawsName = "functor"

  def id(): Fact = {
    val lawName = "identity"
    forAll { (ca: Context[Int]) =>
      if (ca.map(identity[Int]) !== ca) {
        return Laws.no(lawsName, lawName)
      }
    }
    Laws.yes(lawsName, lawName)
  }

  def composition(): Fact = {
    val lawName = "composition"
    // (ca map f map g) should be equal to (ca map (g(f))
    forAll { (ca: Context[Int], ab: Int => Int, bc: Int => Int) =>
      if (functor.map(functor.map(ca)(ab))(bc) !== ca.map(bc compose ab))
        return Laws.no(lawsName, lawName)
    }
    Laws.yes(lawsName, lawName)
  }
  override def test = id() && composition()
}
