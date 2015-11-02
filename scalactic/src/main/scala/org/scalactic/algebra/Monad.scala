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
import scala.language.implicitConversions

/**
 * Typeclass trait for algebraic structure containing <em>insertion</em> and <em>flat-mapping</em> methods that obey
 * laws of <em>identity</em> and <em>associativity</em>.
 *
 * <p>
 * A <code>Monad</code> instance wraps an object that in some way behaves as a <code>Monad</code>.
 * </p>
 */
trait Monad[Context[_]] extends Applicative[Context] {
  /**
   * Applies the given function to the value contained in this context, returning the result 
   * of the function, which is a value wrapped in another context.
   */
  def flatMap[A, B](ca: Context[A])(f: A => Context[B]): Context[B]

  // TODO: Flesh out the scaladoc, explaining the implementation here.
  /**
   * Applies the given function in context to the given value in context, returning the result in
   * the context.
   */
  def applying[A, B](ca: Context[A])(cab: Context[A => B]): Context[B] = flatMap(ca)(a => map(cab)(ab => ab(a)))

  /**
   * Flattens a nested context into a single context.
   */
  def flatten[A](cca: Context[Context[A]]): Context[A] = flatMap(cca)(a => a)
}

/**
 * Companion object for <code>Monad</code> typeclass.
 */
object Monad {

  /**
   * Monad adapter class for any Context[A].
   */
  class Adapter[Context[_], A](val underlying: Context[A])(implicit val monad: Monad[Context]) {
    def map[B](f: A => B) = monad.map(underlying)(f)
    def flatMap[B](f: A => Context[B]) = monad.flatMap(underlying)(f)
    def flatten[B](implicit ev: A <:< Context[B]) = monad.flatMap(underlying)(ev)
  }

  /**
   * Implicit conversion from a Context[A] to a Monad.Adapter.
   */
  implicit def adapters[Context[_], A](ca: Context[A])(implicit ev: Monad[Context]): Adapter[Context, A] =
    new Adapter(ca)(ev)

  /**
   * Summons an implicitly available <code>Monad[Context]</code>.
   *
   * @param ev Evidence (implicit typeclass) that Context is an Applicative.
   * @tparam Context The type of the <code>Monad[Context]</code> to summon.
   * @return The <code>Monad[Context]</code> instance.
   */
  def apply[Context[_]](implicit ev: Monad[Context]): Monad[Context] = ev

  private class ListMonad extends Monad[List] {
    override def flatMap[A, B](ca: List[A])(f: (A) => List[B]): List[B] = ca.flatMap(f)
    override def insert[A](a: A): List[A] = List(a)
  }

  implicit val listMonad: Monad[List] = new ListMonad
}
