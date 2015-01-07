package org.scalactic.algebra

import org.scalacheck._
import org.scalactic.CheckedEquality._
import org.scalactic.Every
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks._

import scala.language.higherKinds

class FunctorLaws {}
/**
 * Implementation of Laws for Funcgtors
 */
/**
class FunctorLaws[Context[_], A, B, C](implicit functor: Functor[Context],
  arbCa: Arbitrary[Context[A]],
  shrCa: Shrink[Context[A]],
  arbAb: Arbitrary[A => B],
  shrAb: Shrink[A => B],
  arbBc: Arbitrary[B => C],
  shrBc: Shrink[B => C]) extends Laws[Context] {

  val name = "Functor Laws"

  val laws = Every  (
    new Law {
      override val name = "functorIdentityLaw"
      override val test() = {
        forAll { (ca: Context[A]) =>
          val fca = functor(ca)
          if ( (fca map identity[A]) !== ca )
            return no
        }
        yes
      }
    }
  )

    if ((functor(ca) map identity[A]) == ca)
      Yes("Obeys ")
}
    **/

/*
    val checkFunctorIdentity: Law = () =>

    // conforms if (ca map f map g) is the same as (ca map (g(f)), via the proxy
    val checkFunctorComposition: Law = () =>
      forAll { (ca: Context[A], ab: A => B, bc: B => C) =>
        (fun(fun(ca) map ab) map bc) shouldEqual (fun(ca) map (bc compose ab))
      }

 */
