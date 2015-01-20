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
 * Typeclass trait representing a binary operation that obeys the associative law and an identity element that
 * obeys the left and right identity laws.
 *
 * <p>
 * The <em>associative law</em> states that given values <code>a</code>, <code>b</code>, and <code>c</code>
 * of type <code>A</code> (and implicit <code>Monoid.adapters</code> imported): 
 * </p>
 *
 * <pre>
 * ((a op b) op c) === (a op (b op c))
 * </pre>
 *
 * <p>
 * The <em>left identity law</em> states that given the identity value, <code>z</code>, and any other value, <code>a</code>,
 * of type <code>A</code> (and implicit <code>Monoid.adapters</code> imported): 
 * </p>
 *
 * <pre>
 * (z op a) === a
 * </pre>
 *
 * <p>
 * An similarly, the <em>right identity law</em> states that given the same values and implicit:
 * </p>
 *
 * <pre>
 * (a op z) === a
 * </pre>
 *
 */
trait Monoid[A] extends Associative[A] {
    
    /**
     * The identity element.
     *
     * Passing the identity element, <code>z</code>, to <code>op</code> along with any other value, <code>a</code>, of type <code>A</code>
     * will result in the same value, <code>a</code>. See the main documentation for this trait for more detail.
     */
    def z: A
}

/**
 * Companion object for <code>Monoid</code> that contains
 * an <code>Adapter</code> that wraps a value of type <code>A</code> given an
 * implicit <code>Monoid[A]</code> as well as an
 * an implicit conversion method from <code>A</code> to <code>Monoid.Adapter[A]</code>
 */
object Monoid {

  /**
   * Adapter class for <a href="Monoid.html"><code>Monoid</code></a>.
   *
   * @param underlying The value of type <code>A</code> to wrap.
   * @param monoid The captured <code>Monoid[A]</code> whose behavior
   *   is used to implement this class's methods.
   */
  class Adapter[A](val underlying: A)(implicit val monoid: Monoid[A]) {

    /**
     * A binary operation that obeys the associative law.
     *
     * See the main documentation for trait <a href="Monoid.html"><code>Monoid</code></a> for more detail.
     */ 
    def op(a2: A): A = monoid.op(underlying, a2)
  }

  /**
   * Implicitly wraps an object in a <code>Monoid.Adapter[A]</code>
   * so long as an implicit <code>Monoid[A]</code> is available.
   */
  implicit def adapters[A](a: A)(implicit ev: Monoid[A]): Monoid.Adapter[A] = new Adapter(a)(ev)
}

