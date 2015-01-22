/*
 * Copyright 2001-2015 Artima, Inc.
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
 * Typeclass trait representing a binary operation that obeys the associative law.
 *
 * <p>
 * The associative law states that given values <code>a</code>, <code>b</code>, and <code>c</code>
 * of type <code>A</code> (and implicit <code>Associative.adapters</code> imported): 
 * </p>
 *
 * <pre>
 * ((a op b) op c) === (a op (b op c))
 * </pre>
 *
 * <p>
 * Note: In mathematics, the algebraic structure consisting of a set along with an associative binary operation
 * is known as a <em>semigroup</em>.
 * </p>
 */
trait Associative[A] {

  /**
   * A binary operation that obeys the associative law.
   *
   * See the main documentation for this trait for more detail.
   */ 
  def op(a1: A, a2: A): A
}

/**
 * Companion object for <code>Associative</code>.
 * an implicit conversion method from <code>A</code> to <code>Associative.Adapter[A]</code>
 */
object Associative {

  /**
   * Adapter class for <a href="Associative.html"><code>Associative</code></a>
   * that wraps a value of type <code>A</code> given an
   * implicit <code>Associative[A]</code>.
   *
   * @param underlying The value of type <code>A</code> to wrap.
   * @param associative The captured <code>Associative[A]</code> whose behavior
   *   is used to implement this class's methods.
   */
  class Adapter[A](val underlying: A)(implicit val associative: Associative[A]) {

    /**
     * A binary operation that obeys the associative law.
     *
     * See the main documentation for trait <a href="Associative.html"><code>Associative</code></a> for more detail.
     */ 
    def op(a2: A): A = associative.op(underlying, a2)
  }

  /**
   * Implicitly wraps an object in an <code>Associative.Adapter[A]</code>
   * so long as an implicit <code>Associative[A]</code> is available.
   */
  implicit def adapters[A](a: A)(implicit ev: Associative[A]): Associative.Adapter[A] = new Adapter(a)(ev)

  /**
   * Summons an implicitly available <code>Associative[A]</code>.
   *
   * <p>
   * This method allows you to write expressions like <code>Associative[String]</code> instead of
   * <code>implicitly[Associative[String]]</code>.
   * </p>
   */
  def apply[A](implicit ev: Associative[A]): Associative[A] = ev
}

