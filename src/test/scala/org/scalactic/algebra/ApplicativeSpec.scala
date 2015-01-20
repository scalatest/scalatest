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

import Applicative.Adapters._

import scala.language.implicitConversions

class ApplicativeSpec extends UnitSpec {


  "Option" should "obey the applicative laws" in {
    class OptionApplicativeAdapter[A](optA: Option[A])(ap: Applicative[Option]) extends Applicative.Adapter[Option, A](optA)(ap) {
      override def applying[B](optAB: Option[A => B]): Option[B] = optAB.flatMap(ab => optA.map(ab))
    }

    class OptionApplicativeAdapter2[A, B](optA: Option[A], optB: Option[B])(implicit ap: Applicative[Option])
      extends Applicative.Adapter2[Option, A, B](optA, optB)(ap)

    class OptionApplicativeAdapter3[A, B, C](optA: Option[A], optB: Option[B], optC: Option[C])(implicit ap: Applicative[Option])
      extends Applicative.Adapter3[Option, A, B, C](optA, optB, optC)(ap)

    implicit object OptionApplicative extends Applicative[Option] {
      // a.k.a. pure, point
      override def insert[A](a: A): Option[A] = Option(a)

      override def apply[A](opt: Option[A]) = new OptionApplicativeAdapter[A](opt)(this)
      override def apply[A, B](optA: Option[A], optB: Option[B]) = new OptionApplicativeAdapter2(optA, optB)(this)
      override def apply[A, B, C](optA: Option[A], optB: Option[B], optC: Option[C]) = new OptionApplicativeAdapter3(optA, optB, optC)(this)
    }

    new ApplicativeLaws[Option]().assert()
  }

  "List" should "obey the applicative laws" in {
    // implementation for List

    class ListApplicativeAdapter[A](listA: List[A])(implicit ap: Applicative[List]) extends Applicative.Adapter[List, A](listA)(ap) {
      override def applying[B](listAB: List[A => B]): List[B] = listAB.flatMap(ab => listA.map(ab))
    }
    class ListApplicativeAdapter2[A, B](listA: List[A], listB: List[B])(implicit ap: Applicative[List]) extends Applicative.Adapter2[List, A, B](listA, listB)(ap)
    class ListApplicativeAdapter3[A, B, C](listA: List[A], listB: List[B], listC: List[C])(implicit ap: Applicative[List]) extends Applicative.Adapter3[List, A, B, C](listA, listB, listC)(ap)

    implicit object ListApplicative extends Applicative[List] {
      override def insert[A](a: A): List[A] = List(a)
      override def apply[A](listA: List[A]) = new ListApplicativeAdapter[A](listA)(this)
      override def apply[A, B](listA: List[A], listB: List[B]) = new ListApplicativeAdapter2[A, B](listA, listB)(this)
      override def apply[A, B, C](listA: List[A], listB: List[B], listC: List[C]) = new ListApplicativeAdapter3[A, B, C](listA, listB, listC)(this)

    }
    new ApplicativeLaws[List]().assert()
  }

  "The good nature of Or" should "obey the applicative laws" in {
    // implementation for the good Or
    class OrApplicativeAdapter[Good, Bad](orA: Good Or Bad)(implicit ap: Applicative[OrWithBad[Bad]#AndGood])
      extends Applicative.Adapter[OrWithBad[Bad]#AndGood, Good](orA) {
      override def applying[C](orAB: Or[Good => C, Bad]): C Or Bad = orAB.flatMap(ab => orA.map(ab))
    }

    class OrApplicativeAdapter2[G1, G2, Bad](orA: G1 Or Bad, orB: G2 Or Bad)(implicit ap: Applicative[OrWithBad[Bad]#AndGood])
      extends Applicative.Adapter2[OrWithBad[Bad]#AndGood, G1, G2](orA, orB)

    class OrApplicativeAdapter3[G1, G2, G3, Bad](orA: G1 Or Bad, orB: G2 Or Bad, orC: G3 Or Bad)(implicit ap: Applicative[OrWithBad[Bad]#AndGood])
      extends Applicative.Adapter3[OrWithBad[Bad]#AndGood, G1, G2, G3](orA, orB, orC)

    implicit object OrApplicative extends Applicative[OrWithBad[Int]#AndGood] {
      override def apply[Good](or: Good Or Int): Applicative.Adapter[OrWithBad[Int]#AndGood, Good] = new OrApplicativeAdapter[Good, Int](or)
      override def apply[G1, G2](or1: G1 Or Int, or2: G2 Or Int): Applicative.Adapter2[OrWithBad[Int]#AndGood, G1, G2] =
        new OrApplicativeAdapter2[G1, G2, Int](or1, or2)
      override def apply[G1, G2, G3](or1: G1 Or Int, or2: G2 Or Int, or3: G3 Or Int): Applicative.Adapter3[OrWithBad[Int]#AndGood, G1, G2, G3] =
        new OrApplicativeAdapter3[G1, G2, G3, Int](or1, or2, or3)

      // a.k.a. pure, point
      override def insert[A](a: A): A Or Int = Good(a)


    }

    // generator used for verifying the Good nature of Or
    implicit def orArbGood[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] = Arbitrary(for (g <- Arbitrary.arbitrary[G]) yield Good(g))

    new ApplicativeLaws[OrWithBad[Int]#AndGood].assert()
  }

}
