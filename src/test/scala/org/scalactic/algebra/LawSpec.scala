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
package org.scalactic.algebra

import org.scalactic.{Bad, Good, Or}
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks._
import org.scalacheck._

import scala.language.higherKinds

class LawSpec extends FlatSpec with Matchers {

  /* Local def of Functor */
  trait FunctorProxy[Context[_], A] {
    def map[B](f: A => B): Context[B]
  }

  trait Functor[Context[_]] {
    def apply[A](ct: Context[A]): FunctorProxy[Context, A]
  }

  trait ApplicativeProxy[Context[_], A] extends FunctorProxy[Context, A] {
    def ap[B](cab: Context[A => B]): Context[B]
  }

  trait Applicative[Context[_]] extends Functor[Context] {
    def apply[A](ca: Context[A]): ApplicativeProxy[Context, A]

    // a.k.a. pure, point
    def insert[A](a: A): Context[A]
  }

  trait MonadProxy[Context[_], A] extends ApplicativeProxy[Context, A] {
    // a.k.a. bind
    def flatMap[B](acb: A => Context[B]): Context[B]
  }

  trait Monad[Context[_]] extends Applicative[Context] {
    def apply[A](ca: Context[A]): MonadProxy[Context, A]
  }

  trait Laws[Context[_]] {
    type Law = () => Unit

    def laws: Seq[Law]

    def assert(): Unit = laws.foreach(law => law())
  }

  class FunctorLaws[Context[_], A, B, C](implicit fun: Functor[Context],
    arbCa: Arbitrary[Context[A]],
    shrCa: Shrink[Context[A]],
    arbAb: Arbitrary[A => B],
    shrAb: Shrink[A => B],
    arbBc: Arbitrary[B => C],
    shrBc: Shrink[B => C]) extends Laws[Context] {

    // mapping over an identity function (e.g. a => a) should cause no change
    val checkFunctorIdentity: Law = () =>
      forAll { (ca: Context[A]) =>
        (fun(ca) map identity[A]) shouldEqual ca
      }

    // conforms if (ca map f map g) is the same as (ca map (g(f)), via the proxy
    val checkFunctorComposition: Law = () =>
      forAll { (ca: Context[A], ab: A => B, bc: B => C) =>
        (fun(fun(ca) map ab) map bc) shouldEqual (fun(ca) map (bc compose ab))
      }

    override def laws = Seq(checkFunctorIdentity, checkFunctorComposition)
  }

  class ApplicativeLaws[Context[_], A, B, C](implicit applic: Applicative[Context],
    arbA: Arbitrary[A],
    shrA: Shrink[A],
    arbCa: Arbitrary[Context[A]],
    shrCa: Shrink[Context[A]],
    arbCab: Arbitrary[Context[A => B]],
    shrCab: Shrink[Context[A => B]],
    arbCbc: Arbitrary[Context[B => C]],
    shrCbc: Shrink[Context[B => C]],
    arbAb: Arbitrary[A => B],
    shrAb: Shrink[A => B],
    arbBc: Arbitrary[B => C],
    shrBc: Shrink[B => C]) extends FunctorLaws[Context, A, B, C] {

    val checkApplicativeComposition: Law = () =>
      forAll { (ca: Context[A], cab: Context[A => B], cbc: Context[B => C]) =>
        // (ca ap cab ap cbc) should be the same as (ca ap (cab ap (cbc map (bc compose ab))))
        (applic(applic(ca) ap cab) ap cbc) shouldEqual
          (applic(ca) ap
            (applic(cab) ap
              (applic(cbc) map ((bc: B => C) => (ab: A => B) => bc compose ab))))
      }

    // ca ap (a => a) should be the same as ca
    val checkApplicativeIdentity: Law = () =>
      forAll { (ca: Context[A]) => (applic(ca) ap applic.insert((a: A) => a)) shouldEqual ca}


    // (insert(a) ap insert(ab)) should be the same as insert(ab(a))
    val checkApplicativeHomomorphism: Law = () =>
      forAll { (a: A, ab: A => B) =>
        (applic(applic.insert(a)) ap applic.insert(ab)) shouldEqual applic.insert(ab(a))
      }

    val checkApplicativeInterchange: Law = () =>

      forAll { (a: A, cab: Context[A => B]) =>
        (applic(applic.insert(a)) ap cab) shouldEqual (applic(cab) ap applic.insert((ab: A => B) => ab(a)))
      }

    val checkApplicativeMapConsistentWithAp: Law = () =>
      forAll { (ca: Context[A], ab: A => B) =>
        (applic(ca) map ab) shouldEqual (applic(ca) ap applic.insert(ab))
      }

    override def laws = super.laws ++ Seq(
      checkApplicativeComposition,
      checkApplicativeHomomorphism,
      checkApplicativeInterchange,
      checkApplicativeMapConsistentWithAp)
  }

  class MonadLaws[Context[_], A, B, C]()(implicit monad: Monad[Context],
    arbA: Arbitrary[A],
    shrA: Shrink[A],
    arbCa: Arbitrary[Context[A]],
    shrCa: Shrink[Context[A]],
    arbCab: Arbitrary[Context[A => B]],
    shrCab: Shrink[Context[A => B]],
    arbCbc: Arbitrary[Context[B => C]],
    shrCbc: Shrink[Context[B => C]],
    arbAcb: Arbitrary[A => Context[B]],
    shrAcb: Shrink[A => Context[B]],
    arbBcc: Arbitrary[B => Context[C]],
    shrBcc: Shrink[B => Context[C]],
    arbAb: Arbitrary[A => B],
    shrAb: Shrink[A => B],
    arbBc: Arbitrary[B => C],
    shrBc: Shrink[B => C]) extends ApplicativeLaws[Context, A, B, C] {

    val checkMonadicAssociativity: Law = () =>
      forAll { (ca: Context[A], acb: A => Context[B], bcc: B => Context[C]) =>
        (monad(monad(ca) flatMap acb) flatMap bcc) shouldEqual (monad(ca) flatMap ((a: A) => monad(acb(a)) flatMap bcc))
      }

    val checkMonadicFlatMapConsistentWithAp: Law = () =>
      forAll { (ca: Context[A], cab: Context[A => B]) =>
        (monad(ca) ap cab) shouldEqual (monad(cab) flatMap (ab => monad(ca) map ab))
      }

    val checkMonadicRightIdentity: Law = () =>
      forAll { (ca: Context[A]) => (monad(ca) flatMap (t => monad.insert(t))) shouldEqual ca}

    val checkMonadicLeftIdentity: Law = () =>
      forAll { (a: A, acb: A => Context[B]) =>
        (monad(monad.insert(a)) flatMap acb) shouldEqual acb(a)
      }

    override def laws = super.laws ++ Seq(
      checkMonadicAssociativity,
      checkMonadicFlatMapConsistentWithAp,
      checkMonadicLeftIdentity,
      checkMonadicRightIdentity)
  }

  /**
   * This trait is used to curry the type parameters of Or, which takes two type parameters,
   * into a type (the trait) which takes one parameter, and another (the type member) which
   * takes the other.  The resulting type (OrWithBad[B]#AndGood) takes a single (Good) type
   * parameter.
   */
  trait OrWithBad[B] {
    type AndGood[G] = G Or B
  }

  /**
   * This trait is used to curry the type parameters of Or, which takes two type parameters,
   * into a type (the trait) which takes one parameter, and another (the type member) which
   * takes the other.  The resulting type (OrWithGood[G]#AndBad) takes a single (Bad) type
   * parameter.
   */
  trait OrWithGood[G] {
    type AndBad[B] = G Or B
  }

  // generator used for verifying the Good nature of Or
  def orArbGood[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] = Arbitrary(for (g <- Arbitrary.arbitrary[G]) yield Good(g))

  // generator used for verifying the Bad nature of Or
  def orArbBad[G, B](implicit arbG: Arbitrary[B]): Arbitrary[G Or B] = Arbitrary(for (b <- Arbitrary.arbitrary[B]) yield Bad(b))


  "Option" should "obey the functor laws via its map method" in {
    class OptionFunctorProxy[T](opt: Option[T]) extends FunctorProxy[Option, T] {
      override def map[U](f: (T) => U): Option[U] = opt.map(f)
    }

    implicit object OptionFunctor extends Functor[Option] {
      def apply[T](opt: Option[T]) = new OptionFunctorProxy[T](opt)
    }

    new FunctorLaws[Option, Int, String, Double]().assert()
  }

  "Option" should "obey the monad laws" in {
    class OptionMonadProxy[A](opt: Option[A]) extends MonadProxy[Option, A] {
      override def map[B](ab: A => B): Option[B] = opt.map(ab)

      override def flatMap[B](aob: A => Option[B]): Option[B] = opt.flatMap(aob)

      override def ap[B](oab: Option[A => B]): Option[B] = opt.zip(oab).map(tup => tup._2(tup._1)).headOption
    }

    implicit object OptionMonad extends Monad[Option] {
      override def apply[A](opt: Option[A]) = new OptionMonadProxy[A](opt)

      override def insert[A](a: A): Option[A] = Option(a)
    }

    new MonadLaws[Option, Int, String, Double]().assert()
  }

  "List" should "obey the functor laws via its map method" in {
    /* functor def for List */
    class ListFunctorProxy[T](list: List[T]) extends FunctorProxy[List, T] {
      override def map[U](f: (T) => U): List[U] = list.map(f)
    }

    implicit object ListFunctor extends Functor[List] {
      def apply[T](list: List[T]) = new ListFunctorProxy[T](list)
    }

    new FunctorLaws[List, Int, String, Double]().assert()
  }

  "Or" should "obey the functor laws (for its 'good' type) via its map method" in {

    class GoodOrFunctorProxy[Good, Bad](ctx: Good Or Bad) extends FunctorProxy[OrWithBad[Bad]#AndGood, Good] {
      def map[C](gc: Good => C): C Or Bad = ctx.map(gc)
    }

    implicit def goodOrFunctor[B]: Functor[OrWithBad[B]#AndGood] = new Functor[OrWithBad[B]#AndGood] {
      def apply[G](ctx: G Or B) = new GoodOrFunctorProxy[G, B](ctx)
    }

    implicit def orArb[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] = orArbGood

    new FunctorLaws[OrWithBad[Int]#AndGood, Int, String, Double]().assert()
  }

  "Or" should "obey the functor laws (for its 'bad' type) via its badMap method" in {

    class BadOrFunctorProxy[Good, Bad](ctx: Good Or Bad) extends FunctorProxy[OrWithGood[Good]#AndBad, Bad] {
      def map[C](bc: Bad => C): Good Or C = ctx.badMap(bc)
    }

    implicit def badOrFunctor[G]: Functor[OrWithGood[G]#AndBad] = new Functor[OrWithGood[G]#AndBad] {
      def apply[B](ctx: G Or B) = new BadOrFunctorProxy[G, B](ctx)
    }

    implicit def orArb[G, B](implicit arbB: Arbitrary[B]): Arbitrary[G Or B] = orArbBad

    new FunctorLaws[OrWithGood[Int]#AndBad, Int, String, Double]().assert()
  }

  "Or" should "obey the monad laws (for its 'good' type)" in {
    class OrMonadProxy[Good, Bad](or: Good Or Bad) extends MonadProxy[OrWithBad[Bad]#AndGood, Good] {
      override def map[C](ab: Good => C): C Or Bad = or.map(ab)

      override def flatMap[C](goc: Good => C Or Bad): C Or Bad = or.flatMap(goc)

      override def ap[C](other: OrWithBad[Bad]#AndGood[Good => C]): C Or Bad =
        for { g <- or; gc <- other } yield gc(g)
    }

    implicit def orMonad[B]: Monad[OrWithBad[B]#AndGood] = new Monad[OrWithBad[B]#AndGood] {
      override def apply[G](or: G Or B) = new OrMonadProxy[G, B](or)

      override def insert[G](g: G): G Or B = Good(g)
    }

    implicit def orArb[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] = orArbGood

    new MonadLaws[OrWithBad[Int]#AndGood, Int, String, Double]().assert()
  }
}
