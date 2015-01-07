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

abstract class ApplicativeAdapter[Context[_], A](ap: Applicative[Context], ca: Context[A]) {
  def inMap[B](cab: Context[A => B]): Context[B]

  def map[B](f: A => B): Context[B] = inMap(ap.insert(f))
}

class ApplicativeAdapter2[Context[_], A, B](ap: Applicative[Context], ca: Context[A], cb: Context[B]) {

  def inMapAll[C](cf: Context[(A, B) => C]): Context[C] = {
    val ap1 = ap.apply(ca).inMap(ap.apply(cf).map(_.curried))
    val ap2 = ap.apply(cb).inMap(ap1)
    ap2
  }

  def mapAll[C](f: (A, B) => C): Context[C] = {
    val ap1 = ap.apply(ca).inMap(ap.insert(f.curried))  // gives us Context[B => C] with A partially applied
    val ap2 = ap.apply(cb).inMap(ap1)
    ap2
  }

  def tupled: ApplicativeAdapter[Context, (A, B)] = ap.apply(mapAll((_,_)))
}

class ApplicativeAdapter3[Context[_], A, B, C](ap: Applicative[Context], ca: Context[A], cb: Context[B], cc: Context[C]) {
  def inMapAll[D](cf: Context[(A, B, C) => D]): Context[D] = {
    val ap1 = ap.apply(ca).inMap(ap.apply(cf).map(_.curried))
    val ap2 = ap.apply(cb).inMap(ap1)
    val ap3 = ap.apply(cc).inMap(ap2)
    ap3
  }

  def mapAll[D](f: (A, B, C) => D): Context[D] = {
    val ap1 = ap.apply(ca).inMap(ap.insert(f.curried))  // gives us Context[B => C] with A partially applied
    val ap2 = ap.apply(cb).inMap(ap1)
    val ap3 = ap.apply(cc).inMap(ap2)
    ap3
  }

  def tupled: ApplicativeAdapter[Context, (A, B, C)] = ap.apply(mapAll((_,_,_)))
}
