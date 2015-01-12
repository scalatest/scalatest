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

import org.scalacheck.Arbitrary
import org.scalactic.{Bad, Good, Or, UnitSpec}

class FunctorSpec extends UnitSpec {

  class OptionFunctorProxy[T](underlying: Option[T]) extends FunctorAdapter[Option, T] {
    def map[U](f: T => U): Option[U]  = underlying.map(f)
  }

  "Option" should "obey the functor laws via its map method" in {
    class OptionFunctorAdapter[T](opt: Option[T]) extends FunctorAdapter[Option, T] {
      override def map[U](f: (T) => U): Option[U] = opt.map(f)
    }

    implicit object OptionFunctor extends Functor[Option] {
      def apply[T](opt: Option[T]) = new OptionFunctorProxy[T](opt)
    }

    new FunctorLaws[Option]().assert()
  }

  "List" should "obey the functor laws via its map method" in {
    class ListFunctorAdapter[T](list: List[T]) extends FunctorAdapter[List, T] {
      override def map[U](f: (T) => U): List[U] = list.map(f)
    }

    implicit object ListFunctor extends Functor[List] {
      def apply[T](list: List[T]) = new ListFunctorAdapter[T](list)
    }

    new FunctorLaws[List]().assert()
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


  "Or" should "obey the functor laws (for its 'good' type) via its map method" in {

    class GoodOrFunctorProxy[Good, Bad](ctx: Good Or Bad) extends FunctorAdapter[OrWithBad[Bad]#AndGood, Good] {
      def map[C](gc: Good => C): C Or Bad = ctx.map(gc)
    }

    implicit def goodOrFunctor[B]: Functor[OrWithBad[B]#AndGood] = new Functor[OrWithBad[B]#AndGood] {
      def apply[G](ctx: G Or B) = new GoodOrFunctorProxy[G, B](ctx)
    }

    implicit def orArb[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] = orArbGood

    new FunctorLaws[OrWithBad[Int]#AndGood]().assert()
  }

  "Or" should "obey the functor laws (for its 'bad' type) via its badMap method" in {

    class BadOrFunctorProxy[Good, Bad](ctx: Good Or Bad) extends FunctorAdapter[OrWithGood[Good]#AndBad, Bad] {
      def map[C](bc: Bad => C): Good Or C = ctx.badMap(bc)
    }

    implicit def badOrFunctor[G]: Functor[OrWithGood[G]#AndBad] = new Functor[OrWithGood[G]#AndBad] {
      def apply[B](ctx: G Or B) = new BadOrFunctorProxy[G, B](ctx)
    }

    implicit def orArb[G, B](implicit arbB: Arbitrary[B]): Arbitrary[G Or B] = orArbBad

    new FunctorLaws[OrWithGood[Int]#AndBad]().assert()
  }

}

