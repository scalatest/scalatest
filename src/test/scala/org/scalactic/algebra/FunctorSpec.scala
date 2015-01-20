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

import scala.language.implicitConversions

class FunctorSpec extends UnitSpec {

  class ListFunctor[A] extends Functor[List, A] {
    override def map[B](ca: List[A])(f: (A) => B): List[B] = ca.map(f)
  }

  class OptionFunctor[A] extends Functor[Option, A] {
    override def map[B](ca: Option[A])(f: (A) => B): Option[B] = ca.map(f)
  }

  "Option" should "obey the functor laws via its map method" in {
    implicit val optionIntFunctor = new OptionFunctor[Int]
    implicit def orArbGood[G, B](implicit arbG: Arbitrary[G]): Arbitrary[Option[G]] = Arbitrary(for (g <- Arbitrary.arbitrary[G]) yield Some(g))


    new FunctorLaws[Option]().assert()
  }

  "List" should "obey the functor laws via its map method" in {
    implicit val listIntFunctor = new ListFunctor[Int]

    new FunctorLaws[List]().assert()
  }

  "Or" should "obey the functor laws (for its 'good' type) via its map method" in {

    // generator used for verifying the Good nature of Or

    class GoodOrFunctor[Good, Bad] extends Functor[OrWithBad[Bad]#AndGood, Good] {
      override def map[B](ca: OrWithBad[Bad]#AndGood[Good])(f: (Good) => B): OrWithBad[Bad]#AndGood[B] = ca.map(f)
    }

    implicit val badOrFunctor = new GoodOrFunctor[Int, Int]
    implicit def orArbGood[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] = Arbitrary(for (g <- Arbitrary.arbitrary[G]) yield Good(g))

    new FunctorLaws[OrWithBad[Int]#AndGood]().assert()
  }

  "Or" should "obey the functor laws (for its 'bad' type) via its badMap method" in {

    // generator used for verifying the Bad nature of Or

    class BadOrFunctor[Good, Bad]extends Functor[OrWithGood[Good]#AndBad, Bad] {
      override def map[B](ca: OrWithGood[Good]#AndBad[Bad])(f: (Bad) => B): OrWithGood[Good]#AndBad[B] = ca.badMap(f)
    }

    implicit val badOrFunctor = new BadOrFunctor[Int, Int]
    implicit def orArbBad[G, B](implicit arbG: Arbitrary[B]): Arbitrary[G Or B] = Arbitrary(for (b <- Arbitrary.arbitrary[B]) yield Bad(b))

    new FunctorLaws[OrWithGood[Int]#AndBad]().assert()
  }
}

