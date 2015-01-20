package org.scalactic.algebra

import org.scalacheck._

import org.scalactic.{Equality, CheckedEquality}
import org.scalatest._

import org.scalatest.prop.GeneratorDrivenPropertyChecks._

import scala.language.higherKinds

class FunctorLaws[Context[_]](implicit functor: Functor[Context, Int],
  arbCa: Arbitrary[Context[Int]],
  shrCa: Shrink[Context[Int]]) extends Laws {

  override val lawsName = "functor"

  def id(): Fact = {
    val lawName = "identity"
    forAll { (ca: Context[Int]) =>
      if (functor.map(ca)(identity[Int]) != ca) {
        return Laws.no(lawsName, lawName)
      }
    }
    Laws.yes(lawsName, lawName)
  }

  def composition(): Fact = {
    val lawName = "composition"
    // (ca map f map g) should be equal to (ca map (g(f))
    forAll { (ca: Context[Int], ab: Int => Int, bc: Int => Int) =>
      if (functor.map(functor.map(ca)(ab))(bc) != functor.map(ca)(bc compose ab))
        return Laws.no(lawsName, lawName)
    }
    Laws.yes(lawsName, lawName)
  }
  override def test = id() && composition()
}
