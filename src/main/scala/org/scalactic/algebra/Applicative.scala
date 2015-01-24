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

/**
 * Typeclass trait representing an algebraic structure that is a <code>Functor[Context]</code> (<em>i.e.</em>, it
 * declares a <code>map</code> method that obeys the functor laws</a>) augmented by <code>insert</code>
 * and <code>applying</code> methods that obey laws of <em>homomorphism</em> and <em>interchange</em>.
 *
 * <p>
 * The <em>homomorhphism law</em> states that given a value, <code>a</code>, of type <code>A</code> and
 * a function, <code>f</code>, of type <code>A =&gt; B</code> (and implicit <code>Applicative.adapters</code> imported): 
 * </p>
 *
 * <pre>
 * val ca = applicative.insert(a)
 * val cf = applicative.insert(f)
 * ca.applying(cf) === applicative.insert(f(a))
 * </pre>
 *
 * <p>
 * The <em>interchange law</em> states that given a value, <code>a</code>, of type <code>A</code> and
 * a function, <code>cf</code>, of type <code>Context[A =&gt; B]</code> (and implicit <code>Applicative.adapters</code> imported): 
 * </p>
 *
 * <pre>
 * val ca = applicative.insert(a)
 * val cg = applicative.insert((f: A =&gt; B) =&gt; f(a))
 * ca.applying(cf) === cf.applying(cg)
 * </pre>
 */
trait Applicative[Context[_]] extends Functor[Context] {

  /**
   * Inserts a value into a context.
   */
  def insert[A](a: A): Context[A]

  /**
   * Applies the given function in context to the given value in context, returning the result in
   * the context.
   */
  def applying[A, B](ca: Context[A])(cab: Context[A => B]): Context[B]

  /**
   * Applies the given function in context to the given values in context, returning the result in
   * the context.
   */
  def applying2[A, B, C](ca: Context[A], cb: Context[B])(cf: Context[(A, B) => C]): Context[C] = {
    val ap1 = applying(ca)(map(cf)(_.curried))
    val ap2 = applying(cb)(ap1)
    ap2
  }

  /**
   * Applies the given function in context to the given values in context, returning the result in
   * the context.
   */
  def applying3[A, B, C, D](ca: Context[A], cb: Context[B], cc: Context[C])(cf: Context[(A, B, C) => D]): Context[D] = {
    val ap1 = applying(ca)(map(cf)(_.curried))
    val ap2 = applying(cb)(ap1)
    val ap3 = applying(cc)(ap2)
    ap3
  }

  /**
   * Applies the given function to the given value in context, returning the result in
   * the context.
   */
  def map[A, B](ca: Context[A])(f: A => B): Context[B] = applying(ca)(insert(f))

  /**
   * Applies the given function to the given values in context, returning the result in
   * the context.
   */
  def map2[A, B, C](ca: Context[A], cb: Context[B])(f: (A, B) => C): Context[C] =
    applying2(ca, cb)(insert(f))

  /**
   * Applies the given function to the given values in context, returning the result in
   * the context.
   */
  def map3[A, B, C, D](ca: Context[A], cb: Context[B], cc: Context[C])(f: (A, B, C) => D): Context[D] =
    applying3(ca, cb, cc)(insert(f))

  /**
   * Transforms the given function into another function where the parameter and result types are lifted into a context.
   */
  def lift[A, B](f: A => B): Context[A] => Context[B] =
    (ca: Context[A]) => map(ca)(f)

  /**
   * Transforms the given function into another function where each parameter type and the result type are lifted into a context.
   */
  def lift2[A, B, C](f: (A, B) => C): (Context[A], Context[B]) => Context[C] =
    (ca: Context[A], cb: Context[B]) => map2(ca, cb)(f)

  /**
   * Transforms the given function into another function where each parameter type and the result type are lifted into a context.
   */
  def lift3[A, B, C, D](f: (A, B, C) => D): (Context[A], Context[B], Context[C]) => Context[D] =
    (ca: Context[A], cb: Context[B], cc: Context[C]) => map3(ca, cb, cc)(f)
}

/**
 * Companion object for trait <a href="Applicative.html"><code>Applicative</code></a>.
 */
object Applicative {

  /**
   * Adapter class for <a href="Applicative.html"><code>Applicative</code></a>
   * that wraps a value of type <code>Context[A]</code> given an
   * implicit <code>Applicative[Context]</code>.
   *
   * @param underlying The value of type <code>Context[A]</code> to wrap.
   * @param applicative The captured <code>Applicative[Context]</code> whose behavior
   *   is used to implement this class's methods.
   */
  class Adapter[Context[_], A](val underlying: Context[A])(implicit val applicative: Applicative[Context]) {

    /**
     * An applying operation that obeys the homomorphism and interchange laws.
     *
     * See the main documentation for trait <a href="Applicative.html"><code>Applicative</code></a> for more detail.
     */ 
    def applying[B](cab: Context[A => B]): Context[B] = applicative.applying(underlying)(cab)

    /**
     * A mapping operation that obeys the identity and composition laws.
     *
     * See the main documentation for trait <a href="Functor.html"><code>Functor</code></a> for more detail.
     */ 
    def map[B](f: A => B): Context[B] = applicative.map(underlying)(f)
  }

  /**
   * Implicitly wraps an object in a <code>Applicative.Adapter[Context, A]</code>
   * so long as an implicit <code>Applicative[Context]</code> is available.
   */
  implicit def adapters[Context[_], A](ca: Context[A])(implicit ev: Applicative[Context]): Adapter[Context, A] =
    new Adapter(ca)(ev)

  /**
   * Summons an implicitly available Applicative[Context].
   *
   * <p>
   * This method allows you to write expressions like <code>Applicative[List]</code> instead of
   * <code>implicitly[Applicative[List]]</code>.
   * </p>
   */
  def apply[Context[_]](implicit ev: Applicative[Context]): Applicative[Context] = ev
}
