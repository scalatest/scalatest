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

import scala.language.higherKinds

trait Applicative[Context[_]] {
  def apply[A](ca: Context[A]): ApplicativeAdapter[Context, A]
  def apply[A, B](ca: Context[A], cb: Context[B]): ApplicativeAdapter2[Context, A, B]
  def apply[A, B, C](ca: Context[A], cb: Context[B], cc: Context[C]): ApplicativeAdapter3[Context, A, B, C]

  def lift[A, B](f: A => B): Context[A] => Context[B] =
    (ca: Context[A]) => apply(ca).map(f)

  def lift[A, B, C](f: (A, B) => C): (Context[A], Context[B]) => Context[C] =
    (ca: Context[A], cb: Context[B]) => apply(ca, cb).mapAll(f)

  def lift[A, B, C, D](f: (A, B, C) => D): (Context[A], Context[B], Context[C]) => Context[D] =
    (ca: Context[A], cb: Context[B], cc: Context[C]) => apply(ca, cb, cc).mapAll(f)

  // a.k.a. pure, point
  def insert[T](t: T): Context[T]
}

abstract class ApplicativeAdapter[Context[_], A](applicative: Applicative[Context], ca: Context[A]) {
  def applying[B](cab: Context[A => B]): Context[B]

  def map[B](f: A => B): Context[B] = applying(applicative.insert(f))
}

object ApplicativeAdapter {
  implicit def adapt[Context[_], A](ca: Context[A])(implicit ev: Applicative[Context]): ApplicativeAdapter[Context, A] = ev.apply(ca)
}

class ApplicativeAdapter2[Context[_], A, B](applicative: Applicative[Context], ca: Context[A], cb: Context[B]) {

  def applying[C](cf: Context[(A, B) => C]): Context[C] = {
    val ap1 = applicative(ca).applying(applicative(cf).map(_.curried))
    val ap2 = applicative(cb).applying(ap1)
    ap2
  }

  def mapAll[C](f: (A, B) => C): Context[C] = applying(applicative.insert(f))

  def tupled: ApplicativeAdapter[Context, (A, B)] = applicative.apply(mapAll((_,_)))
}

class ApplicativeAdapter3[Context[_], A, B, C](applicative: Applicative[Context], ca: Context[A], cb: Context[B], cc: Context[C]) {
  def applying[D](cf: Context[(A, B, C) => D]): Context[D] = {
    val ap1 = applicative(ca).applying(applicative(cf).map(_.curried))
    val ap2 = applicative(cb).applying(ap1)
    val ap3 = applicative(cc).applying(ap2)
    ap3
  }

  def mapAll[D](f: (A, B, C) => D): Context[D] = applying(applicative.insert(f))

  def tupled: ApplicativeAdapter[Context, (A, B, C)] = applicative(mapAll((_,_,_)))
}
