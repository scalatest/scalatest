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

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks._
import org.scalacheck._

import scala.language.higherKinds

class LawSpec extends FlatSpec with Matchers {

  /* Local def of Functor */
  trait FunctorProxy[Context[_], T] {
    def map[U](f: T => U): Context[U]
  }

  trait Functor[Context[_]] {
    def apply[T](ct: Context[T]): FunctorProxy[Context, T]
  }

  trait ApplicativeProxy[Context[_], T] extends FunctorProxy[Context, T] {
    // 'ap' in scalaz, renamed to make things more readable (zap = zip + apply)
    def zap[U](ctu: => Context[T => U]): Context[U]
  }

  trait Applicative[Context[_]] extends Functor[Context] {
    def apply[T](ct: Context[T]): ApplicativeProxy[Context, T]

    // a.k.a. pure, point
    def insert[T](t: T): Context[T]
  }

  trait MonadProxy[Context[_], T] extends ApplicativeProxy[Context, T] {
    // a.k.a. bind
    def flatMap[U](f: T => Context[U]): Context[U]
  }

  trait Monad[Context[_]] extends Applicative[Context] {
    def apply[T](ct: Context[T]): MonadProxy[Context, T]
  }

  trait FunctorIdentityLaw {
    def obeysIdentityLaw[Context[_], T](ct: Context[T])(implicit fun: Functor[Context]): Boolean =
      fun(ct).map(identity[T]) === ct
  }

  trait FunctorCompositionLaw {
    // conforms if (ctx map f map g) is the same as (ctx map (g(f)), via the proxy
    def obeysFunctorCompositionLaw[Context[_], T, U, V](ct: Context[T], f: T => U, g: U => V)(implicit fun: Functor[Context]): Boolean = {
      fun(fun(ct).map(f)).map(g) === fun(ct).map(g compose f)
    }
  }

  trait FunctorLaws extends FunctorIdentityLaw with FunctorCompositionLaw

  trait ApplicativeCompositionLaw {
    def obeysApplicativeComposition[Context[_], T, U, V](ct: Context[T], ctu: Context[T => U], cuv: Context[U => V])
                                                        (implicit applic: Applicative[Context]): Boolean = {
      (applic( applic(ct) zap ctu ) zap cuv) ===
        applic(ct).zap( applic(ctu).zap( applic(cuv).map((uv: U => V) => (tu: T => U) => uv compose tu) ) )
    }
  }

  trait ApplicativeIdentityLaw {
    def obeysApplicativeIdentity[Context[_], T](ct: Context[T])(implicit applic: Applicative[Context]): Boolean = {
      (applic(ct) zap applic.insert((t: T) => t)) === ct
    }
  }

  trait ApplicativeHomomorphismLaw {
    def obeysApplicativeHomomorphism[Context[_], T, U](f: T => U, t: T)(implicit applic: Applicative[Context]): Boolean = {
      (applic( applic.insert(t) ) zap applic.insert(f)) === applic.insert(f(t))
    }
  }

  trait ApplicativeInterchangeLaw {
    def obeysApplicativeInterchange[Context[_], T, U](cf: Context[T => U], t: T)(implicit applic: Applicative[Context]): Boolean = {
      (applic(applic.insert(t)) zap cf) === (applic(cf) zap applic.insert((f: T => U) => f(t)))
    }
  }

  trait ApplicativeMapConsistentWithApLaw {
    def obeysApplicativeMapConsistentWithZap[Context[_], T, U](ct: Context[T], f: T => U)(implicit applic: Applicative[Context]): Boolean = {
      (applic(ct) map f) === (applic(ct) zap applic.insert(f))
    }
  }

  trait ApplicativeLaws extends FunctorLaws
    with ApplicativeCompositionLaw
    with ApplicativeIdentityLaw
    with ApplicativeHomomorphismLaw
    with ApplicativeInterchangeLaw
    with ApplicativeMapConsistentWithApLaw

  trait MonadicAssociativityLaw {
    def obeysMonadicAssociativity[Context[_], T, U, V](ct: Context[T], tcu: T => Context[U], ucv: U => Context[V])(implicit monad: Monad[Context]): Boolean = {

      (monad(monad(ct) flatMap tcu) flatMap ucv) === (monad(ct) flatMap ((t: T) => monad(tcu(t)) flatMap ucv))
    }
  }

  trait MonadicFlatMapConsistentWithZapLaw {
    def obeysFlatMapConsistentWithZap[Context[_], T, U](ct: Context[T], ctu: Context[T => U])(implicit monad: Monad[Context]): Boolean = {
      (monad(ct) zap ctu) === (monad(ctu) flatMap (f => monad(ct) map f))
    }
  }

  trait MonadicRightIdentityLaw {
    def obeysRightIdentity[Context[_], T](ct: Context[T])(implicit monad: Monad[Context]): Boolean = {
      (monad(ct) flatMap (t => monad.insert(t))) === ct

    }
  }

  trait MonadicLeftIdentityLaw {
    def obeysLeftIdentity[Context[_], T, U](t: T, tcu: T => Context[U])(implicit monad: Monad[Context]): Boolean = {
      (monad(monad.insert(t)) flatMap tcu) === tcu(t)
    }
  }

  trait MonadLaws extends ApplicativeLaws
    with MonadicAssociativityLaw
    with MonadicFlatMapConsistentWithZapLaw
    with MonadicLeftIdentityLaw
    with MonadicRightIdentityLaw

  trait FunctorAssertions[Context[_]] extends FunctorIdentityLaw with FunctorCompositionLaw {

    def assert[T, U, V]()(implicit fun: Functor[Context],
                          arbContextT:  Arbitrary[Context[T]],
                          shrContextT:  Shrink[Context[T]],
                          arbFunT:      Arbitrary[T => U],
                          shrFunT:      Shrink[T => U],
                          arbFunU:      Arbitrary[U => V],
                          shrFunU:      Shrink[U => V]
      ): Unit = {
      checkIdentity[T]()
      checkComposition[T, U, V]()
    }

    def checkIdentity[T]()(implicit fun: Functor[Context],
                           arbContextT:  Arbitrary[Context[T]],
                           shrContextT:  Shrink[Context[T]]
      ): Unit = {
      forAll { (ctx: Context[T]) => obeysIdentityLaw(ctx) shouldBe true }
    }

    def checkComposition[T, U, V]()(implicit
                                    adapter:      Functor[Context],
                                    arbContextT:  Arbitrary[Context[T]],
                                    shrContextT:  Shrink[Context[T]],
                                    arbFunT:      Arbitrary[T => U],
                                    shrFunT:      Shrink[T => U],
                                    arbFunU:      Arbitrary[U => V],
                                    shrFunU:      Shrink[U => V]
      ): Unit = {
      // TODO: how to loop through types? (code generation/macros?)
      forAll { (ctx: Context[T], f: T => U, g: U => V) => obeysFunctorCompositionLaw(ctx, f, g) shouldBe true }
    }
  }

  /* functor def for Option */
  class OptionFunctorProxy[T](opt: Option[T]) extends FunctorProxy[Option, T] {
    override def map[U](f: (T) => U): Option[U] = opt.map(f)
  }

  implicit object OptionFunctor extends Functor[Option] {
    def apply[T](opt: Option[T]) = new OptionFunctorProxy[T](opt)
  }

  /* functor def for List */
  class ListFunctorProxy[T](list: List[T]) extends FunctorProxy[List, T] {
    override def map[U](f: (T) => U): List[U] = list.map(f)
  }

  implicit object ListFunctor extends Functor[List] {
    def apply[T](list: List[T]) = new ListFunctorProxy[T](list)
  }

  "Option" should "obey the functor laws via its map method (generically)" in {

    object OptionFunctorAssertions extends FunctorAssertions[Option]
    OptionFunctorAssertions.assert[String, Int, Double]()

    // or

    new FunctorAssertions[Option](){}.assert[String, Int, Double]()
  }

  "List" should "obey the functor laws via its map method (generically)" in {
    new FunctorAssertions[List](){}.assert[Double, Byte, String]()
  }
}
