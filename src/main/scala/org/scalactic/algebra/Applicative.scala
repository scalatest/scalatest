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
import scala.language.higherKinds

trait Applicative[Context[_]] {
  def apply[A](ca: Context[A]): ApplicativeAdapter[Context, A]
  def apply[A, B](ca: Context[A], cb: Context[B]): ApplicativeAdapter2[Context, A, B]
  def apply[A, B, C](ca: Context[A], cb: Context[B], cc: Context[C]): ApplicativeAdapter3[Context, A, B, C]

  // a.k.a. pure, point
  def insert[T](t: T): Context[T]

  def lift[A, B](f: A => B): Context[A] => Context[B] =
    (ca: Context[A]) => apply(ca).map(f)

  def lift2[A, B, C](f: (A, B) => C): (Context[A], Context[B]) => Context[C] =
    (ca: Context[A], cb: Context[B]) => apply(ca, cb).map2(f)

  def lift3[A, B, C, D](f: (A, B, C) => D): (Context[A], Context[B], Context[C]) => Context[D] =
    (ca: Context[A], cb: Context[B], cc: Context[C]) => apply(ca, cb, cc).map3(f)

}

object ApplicativeAdapter {
  object conversions {
    implicit def adapt[Context[_], A](ca: Context[A])(implicit ev: Applicative[Context]): ApplicativeAdapter[Context, A] =
      ev.apply(ca)
    implicit def adapt2[Context[_], A, B](tup: (Context[A], Context[B]))(implicit ev: Applicative[Context]): ApplicativeAdapter2[Context, A, B] =
      ev.apply(tup._1, tup._2)
    implicit def adapt3[Context[_], A, B, C](tup: (Context[A], Context[B], Context[C]))(implicit ev: Applicative[Context]): ApplicativeAdapter3[Context, A, B, C] =
      ev.apply(tup._1, tup._2, tup._3)
  }
}

abstract class ApplicativeAdapter[Context[_], A](applicative: Applicative[Context], ca: Context[A])
  extends FunctorAdapter[Context, A] {

  def applying[B](cab: Context[A => B]): Context[B]

  def map[B](f: A => B): Context[B] = applying(applicative.insert(f))
}

class ApplicativeAdapter2[Context[_], A, B](applicative: Applicative[Context], ca: Context[A], cb: Context[B]) {

  def applying2[C](cf: Context[(A, B) => C]): Context[C] = {
    val ap1 = applicative(ca).applying(applicative(cf).map(_.curried))
    val ap2 = applicative(cb).applying(ap1)
    ap2
  }

  def map2[C](f: (A, B) => C): Context[C] = applying2(applicative.insert(f))

  def tupled2: ApplicativeAdapter[Context, (A, B)] = applicative.apply(map2((_,_)))

  def mapAll[C](f: (A, B) => C): Context[C] = map2(f)
}

class ApplicativeAdapter3[Context[_], A, B, C](applicative: Applicative[Context], ca: Context[A], cb: Context[B], cc: Context[C]) {
  def applying3[D](cf: Context[(A, B, C) => D]): Context[D] = {
    val ap1 = applicative(ca).applying(applicative(cf).map(_.curried))
    val ap2 = applicative(cb).applying(ap1)
    val ap3 = applicative(cc).applying(ap2)
    ap3
  }

  def map3[D](f: (A, B, C) => D): Context[D] = applying3(applicative.insert(f))

  def tupled3: ApplicativeAdapter[Context, (A, B, C)] = applicative(mapAll((_,_,_)))

  def mapAll[D](f: (A, B, C) => D): Context[D] = map3(f)
}

// implementation for Option

object Applicatives {

  class OptionApplicativeAdapter[A](ap: Applicative[Option], optA: Option[A]) extends ApplicativeAdapter[Option, A](ap, optA) {
    override def applying[B](optAB: Option[A => B]): Option[B] = optAB.flatMap(ab => optA.map(ab))
  }

  class OptionApplicativeAdapter2[A, B](ap: Applicative[Option],
    optA: Option[A],
    optB: Option[B]) extends ApplicativeAdapter2[Option, A, B](ap, optA, optB)

  class OptionApplicativeAdapter3[A, B, C](ap: Applicative[Option],
    optA: Option[A],
    optB: Option[B],
    optC: Option[C]) extends ApplicativeAdapter3[Option, A, B, C](ap, optA, optB, optC)

  implicit object OptionApplicative extends Applicative[Option] {
    def apply[A](opt: Option[A]) = new OptionApplicativeAdapter[A](this, opt)
    def apply[A, B](optA: Option[A], optB: Option[B]) = new OptionApplicativeAdapter2(this, optA, optB)
    def apply[A, B, C](optA: Option[A], optB: Option[B], optC: Option[C]) = new OptionApplicativeAdapter3(this, optA, optB, optC)

    // a.k.a. pure, point
    override def insert[A](a: A): Option[A] = Option(a)
  }

  // implementation for List

  class ListApplicativeAdapter[A](ap: Applicative[List], listA: List[A]) extends ApplicativeAdapter[List, A](ap, listA) {
    override def applying[B](listAB: List[A => B]): List[B] = listAB.flatMap(ab => listA.map(ab))
  }

  class ListApplicativeAdapter2[A, B](ap: Applicative[List], listA: List[A], listB: List[B]) extends ApplicativeAdapter2[List, A, B](ap, listA, listB)
  class ListApplicativeAdapter3[A, B, C](ap: Applicative[List], listA: List[A], listB: List[B], listC: List[C]) extends ApplicativeAdapter3[List, A, B, C](ap, listA, listB, listC)

  implicit object ListApplicative extends Applicative[List] {
    def apply[A](listA: List[A]) = new ListApplicativeAdapter[A](this, listA)

    def apply[A, B](listA: List[A], listB: List[B]) = new ListApplicativeAdapter2[A, B](this, listA, listB)

    def apply[A, B, C](listA: List[A],
      listB: List[B],
      listC: List[C]) = new ListApplicativeAdapter3[A, B, C](this, listA, listB, listC)

    override def insert[A](a: A): List[A] = List(a)
  }

  // implementation for the good Or
  class OrApplicativeAdapter[Good, Bad](ap: Applicative[OrWithBad[Bad]#AndGood], orA: Good Or Bad) extends ApplicativeAdapter[OrWithBad[Bad]#AndGood, Good](ap, orA) {
    override def applying[C](orAB: Or[Good => C, Bad]): C Or Bad = orAB.flatMap(ab => orA.map(ab))
  }

  class OrApplicativeAdapter2[G1, G2, Bad](ap: Applicative[OrWithBad[Bad]#AndGood], orA: G1 Or Bad, orB: G2 Or Bad)
    extends ApplicativeAdapter2[OrWithBad[Bad]#AndGood, G1, G2](ap, orA, orB)

  class OrApplicativeAdapter3[G1, G2, G3, Bad](ap: Applicative[OrWithBad[Bad]#AndGood], orA: G1 Or Bad, orB: G2 Or Bad, orC: G3 Or Bad)
    extends ApplicativeAdapter3[OrWithBad[Bad]#AndGood, G1, G2, G3](ap, orA, orB, orC)

  implicit object OrApplicative extends Applicative[OrWithBad[Int]#AndGood] {
    override def apply[Good](ca: Good Or Int): ApplicativeAdapter[OrWithBad[Int]#AndGood, Good] =
      new OrApplicativeAdapter[Good, Int](this, ca)


    override def apply[G1, G2](ca: G1 Or Int, cb: G2 Or Int): ApplicativeAdapter2[OrWithBad[Int]#AndGood, G1, G2] =
      new OrApplicativeAdapter2[G1, G2, Int](this, ca, cb)

    override def apply[G1, G2, G3](ca: G1 Or Int, cb: G2 Or Int, cc: G3 Or Int): ApplicativeAdapter3[OrWithBad[Int]#AndGood, G1, G2, G3] = ???
      //new OrApplicativeAdapter3[G1, G2, G3, Int](this, ca, cb, cc)

    // a.k.a. pure, point
    override def insert[A](a: A): A Or Int = Good(a)


  }


}
