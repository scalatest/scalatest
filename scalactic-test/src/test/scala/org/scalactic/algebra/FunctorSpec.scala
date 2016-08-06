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
import org.scalactic._
import org.scalatest.laws._

import scala.language.implicitConversions

class FunctorSpec extends UnitSpec {

  class ListFunctor extends Functor[List] {
    override def map[A, B](ca: List[A])(f: (A) => B): List[B] = ca.map(f)
  }

  class OptionFunctor extends Functor[Option] {
    override def map[A, B](ca: Option[A])(f: (A) => B): Option[B] = ca.map(f)
  }

  "Option" should "obey the functor laws via its map method" in {
    implicit val optionIntFunctor = new OptionFunctor
    implicit def arbOptionSome[G](implicit arbG: Arbitrary[G]): Arbitrary[Option[G]] =
      Arbitrary(for (g <- Arbitrary.arbitrary[G]) yield Some(g))

    new FunctorLaws[Option]().assert()
  }

  "List" should "obey the functor laws via its map method" in {
    implicit val listIntFunctor = new ListFunctor

    new FunctorLaws[List]().assert()
  }

  "Or" should "obey the functor laws (for its 'good' type) via its map method" in {

    class OrFunctor[BAD] extends Functor[Or.B[BAD]#G] {
      override def map[G, H](ca: G Or BAD)(f: G => H): H Or BAD = ca.map(f)
    }

    implicit val orFunctor = new OrFunctor[Int]
    implicit def orArbGood[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] =
      Arbitrary(for (g <- Arbitrary.arbitrary[G]) yield Good(g))

    new FunctorLaws[Or.B[Int]#G]().assert()
  }

  "Or" should "obey the functor laws (for its 'bad' type) via its badMap method" in {

    class BadOrFunctor[GOOD] extends Functor[Or.G[GOOD]#B] {
      override def map[B, C](ca: GOOD Or B)(f: B => C): GOOD Or C = ca.badMap(f)
    }

    implicit val badOrFunctor = new BadOrFunctor[Int]
    implicit def orArbBad[G, B](implicit arbG: Arbitrary[B]): Arbitrary[G Or B] =
      Arbitrary(for (b <- Arbitrary.arbitrary[B]) yield Bad(b))

    new FunctorLaws[Or.G[Int]#B]().assert()
  }
}

