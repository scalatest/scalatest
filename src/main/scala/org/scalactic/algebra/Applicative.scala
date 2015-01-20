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

import scala.language.implicitConversions
import scala.language.higherKinds

trait Applicative[Context[_]] {
  def apply[A](ca: Context[A]): Applicative.Adapter[Context, A]
  def apply[A, B](ca: Context[A], cb: Context[B]): Applicative.Adapter2[Context, A, B]
  def apply[A, B, C](ca: Context[A], cb: Context[B], cc: Context[C]): Applicative.Adapter3[Context, A, B, C]

  // a.k.a. pure, point
  def insert[A](a: A): Context[A]

  def lift[A, B](f: A => B): Context[A] => Context[B] =
    (ca: Context[A]) => apply(ca).map(f)

  def lift2[A, B, C](f: (A, B) => C): (Context[A], Context[B]) => Context[C] =
    (ca: Context[A], cb: Context[B]) => apply(ca, cb).map2(f)

  def lift3[A, B, C, D](f: (A, B, C) => D): (Context[A], Context[B], Context[C]) => Context[D] =
    (ca: Context[A], cb: Context[B], cc: Context[C]) => apply(ca, cb, cc).map3(f)

}

object Applicative {

  def apply[Context[_], A](implicit ap: Applicative[Context]): Applicative[Context] = ap

  abstract class Adapter[Context[_], A](ca: Context[A])(implicit val applicative: Applicative[Context]) {
    def applying[B](cab: Context[A => B]): Context[B]
    def map[B](f: A => B): Context[B] = applying(applicative.insert(f))
  }

  class Adapter2[Context[_], A, B](ca: Context[A], cb: Context[B])(implicit val applicative: Applicative[Context]) {
    def applying2[C](cf: Context[(A, B) => C]): Context[C] = {
      val ap1 = applicative(ca).applying(applicative(cf).map(_.curried))
      val ap2 = applicative(cb).applying(ap1)
      ap2
    }
    def map2[C](f: (A, B) => C): Context[C] = applying2(applicative.insert(f))
    def tupled2: Adapter[Context, (A, B)] = applicative.apply(map2((_,_)))
    def mapAll[C](f: (A, B) => C): Context[C] = map2(f)
  }

  class Adapter3[Context[_], A, B, C](ca: Context[A], cb: Context[B], cc: Context[C])(implicit val applicative: Applicative[Context]) {
    def applying3[D](cf: Context[(A, B, C) => D]): Context[D] = {
      val ap1 = applicative(ca).applying(applicative(cf).map(_.curried))
      val ap2 = applicative(cb).applying(ap1)
      val ap3 = applicative(cc).applying(ap2)
      ap3
    }
    def map3[D](f: (A, B, C) => D): Context[D] = applying3(applicative.insert(f))
    def tupled3: Adapter[Context, (A, B, C)] = applicative(mapAll((_,_,_)))
    def mapAll[D](f: (A, B, C) => D): Context[D] = map3(f)
  }

  object Adapters {
    implicit def adapt[Context[_], A](ca: Context[A])(implicit applicative: Applicative[Context]): Adapter[Context, A] =
      applicative.apply(ca)

    implicit def adapt2[Context[_], A, B](tup: (Context[A], Context[B]))(implicit applicative: Applicative[Context]): Adapter2[Context, A, B] =
      applicative.apply(tup._1, tup._2)

    implicit def adapt3[Context[_], A, B, C](tup: (Context[A], Context[B], Context[C]))(implicit applicative: Applicative[Context]): Adapter3[Context, A, B, C] =
      applicative.apply(tup._1, tup._2, tup._3)

  }
}
