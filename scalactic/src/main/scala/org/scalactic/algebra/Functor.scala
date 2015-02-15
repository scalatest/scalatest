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
 * Typeclass trait representing an algebraic structure defined by a <code>map</code> method that
 * obeys laws of <em>identity</em> and <em>composition</em>.
 *
 * <p>
 * The <em>identity law</em> states that given a value <code>ca</code> of type <code>Context[A]</code> and
 * the identity function, <code>(a: A) => a</code> (and implicit <code>Functor.adapters</code> imported): 
 * </p>
 *
 * <pre>
 * ca.map((a: A) =&gt; a) === ca
 * </pre>
 *
 * <p>
 * The <em>composition law</em> states that given a value <code>ca</code> of type <code>Context[A]</code> and
 * two functions, <code>f</code> of type <code>A =&gt; B</code> and <code>g</code> of type <code>B =&gt; C</code>
 * (and implicit <code>Functor.adapters</code> imported): 
 * </p>
 *
 * <pre>
 * ca.map(f).map(g) === ca.map(g compose f)
 * </pre>
 */
trait Functor[Context[_]] {

  /**
   * Applies the given function to the given value in context, returning the result in
   * the context.
   *
   * <p>
   * See the main documentation for this trait for more detail.
   * </p>
   */
  def map[A, B](ca: Context[A])(f: A => B): Context[B]
}

/**
 * Companion object for trait <a href="Functor.html"><code>Functor</code></a>.
 */
object Functor {

  /**
   * Adapter class for <a href="Functor.html"><code>Functor</code></a>
   * that wraps a value of type <code>Context[A]</code> given an
   * implicit <code>Functor[Context]</code>.
   *
   * @param underlying The value of type <code>Context[A]</code> to wrap.
   * @param functor The captured <code>Functor[Context]</code> whose behavior
   *   is used to implement this class's methods.
   */
   class Adapter[Context[_], A](val underlying: Context[A])(implicit val functor: Functor[Context]) {

    /**
     * A mapping operation that obeys the identity and composition laws.
     *
     * See the main documentation for trait <a href="Functor.html"><code>Functor</code></a> for more detail.
     */ 
    def map[B](f: A => B): Context[B] = functor.map(underlying)(f)
  }

  /**
   * Implicitly wraps an object in a <code>Functor.Adapter[Context, A]</code>
   * so long as an implicit <code>Functor[Context]</code> is available.
   */
  implicit def adapters[Context[_], A](ca: Context[A])(implicit ev: Functor[Context]): Functor.Adapter[Context, A] =
    new Adapter(ca)(ev)

  /**
   * Summons an implicitly available Functor[Context].
   *
   * <p>
   * This method allows you to write expressions like <code>Functor[List]</code> instead of
   * <code>implicitly[Functor[List]]</code>.
   * </p>
   */
  def apply[Context[_]](implicit ev: Functor[Context]): Functor[Context] = ev
}
