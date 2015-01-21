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

trait Applicative[Context[_]] extends Functor[Context] {

  def applying[A, B](ca: Context[A])(cab: Context[A => B]): Context[B]

  def applying2[A, B, C](ca: Context[A], cb: Context[B])(cf: Context[(A, B) => C]): Context[C] = {
    val ap1 = applying(ca)(map(cf)(_.curried))
    val ap2 = applying(cb)(ap1)
    ap2
  }

  def applying3[A, B, C, D](ca: Context[A], cb: Context[B], cc: Context[C])(cf: Context[(A, B, C) => D]): Context[D] = {
    val ap1 = applying(ca)(map(cf)(_.curried))
    val ap2 = applying(cb)(ap1)
    val ap3 = applying(cc)(ap2)
    ap3
  }

  // a.k.a. pure, point
  def insert[A](a: A): Context[A]

  def map[A, B](ca: Context[A])(f: A => B): Context[B] = applying(ca)(insert(f))

  def map2[A, B, C](ca: Context[A], cb: Context[B])(f: (A, B) => C): Context[C] =
    applying2(ca, cb)(insert(f))

  def map3[A, B, C, D](ca: Context[A], cb: Context[B], cc: Context[C])(f: (A, B, C) => D): Context[D] =
    applying3(ca, cb, cc)(insert(f))

  def lift[A, B](f: A => B): Context[A] => Context[B] =
    (ca: Context[A]) => map(ca)(f)

  def lift2[A, B, C](f: (A, B) => C): (Context[A], Context[B]) => Context[C] =
    (ca: Context[A], cb: Context[B]) => map2(ca, cb)(f)

  def lift3[A, B, C, D](f: (A, B, C) => D): (Context[A], Context[B], Context[C]) => Context[D] =
    (ca: Context[A], cb: Context[B], cc: Context[C]) => map3(ca, cb, cc)(f)

}

object Applicative {

  def apply[Context[_]](implicit ev: Applicative[Context]): Applicative[Context] = ev

  class Adapter[Context[_], A](val underlying: Context[A])(implicit val applicative: Applicative[Context]) {
    def applying[B](cab: Context[A => B]): Context[B] = applicative.applying(underlying)(cab)
    def map[B](f: A => B): Context[B] = applicative.map(underlying)(f)
  }

  implicit def adapters[Context[_], A](ca: Context[A])(implicit ev: Applicative[Context]): Adapter[Context, A] =
    new Applicative.Adapter(ca)(ev)
}
