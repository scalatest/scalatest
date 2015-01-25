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
 * Typeclass trait representing a binary operation that obeys the commutative law.
 *
 * <p>
 * The commutative law states that changing the order of the operands to a binary operation
 * does not change the result, i.e. given values <code>a</code>, <code>b</code>
 * </p>
 *
 * <pre>
 * (a op b) === (b op a)
 * </pre>
 *
 */
trait Commutative[A] {

  /**
   * A binary operation that obeys the associative law.
   *
   * See the main documentation for this trait for more detail.
   */ 
  def op(a1: A, a2: A): A
}

/**
 * Companion object for <code>Commutative</code>.
 * an implicit conversion method from <code>A</code> to <code>Commutative.Adapter[A]</code>
 */
object Commutative {

  /**
   * Adapter class for <a href="Commutative.html"><code>Commutative</code></a>
   * that wraps a value of type <code>A</code> given an
   * implicit <code>Commutative[A]</code>.
   *
   * @param underlying The value of type <code>A</code> to wrap.
   * @param commutative The captured <code>Commutative[A]</code> whose behavior
   *   is used to implement this class's methods.
   */
  class Adapter[A](val underlying: A)(implicit val commutative: Commutative[A]) {

    /**
     * A binary operation that obeys the commutative law.
     *
     * See the main documentation for trait <a href="Commutative.html"><code>Commutative</code></a> for more detail.
     */ 
    def op(a2: A): A = commutative.op(underlying, a2)
  }

  /**
   * Implicitly wraps an object in an <code>Commutative.Adapter[A]</code>
   * so long as an implicit <code>Commutative[A]</code> is available.
   */
  implicit def adapters[A](a: A)(implicit ev: Commutative[A]): Commutative.Adapter[A] = new Adapter(a)(ev)

  /**
   * Summons an implicitly available <code>Commutative[A]</code>.
   *
   * <p>
   * This method allows you to write expressions like <code>Commutative[String]</code> instead of
   * <code>implicitly[Commutative[String]]</code>.
   * </p>
   */
  def apply[A](implicit ev: Commutative[A]): Commutative[A] = ev
}

