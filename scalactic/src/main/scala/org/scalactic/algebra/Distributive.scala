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
 * Typeclass trait representing two binary operations that obeys the distributive law:
 * one, <code>dop</code> that does the distributing, and the other <code>op</code> binary op that dop is applied to.
 *
 * <p>
 * The distributive law states that given values <code>a</code>, <code>b</code>, <code>c</code>
 * </p>
 *
 * <pre>
 * a dop (b op c) === (a dop b) op (a dop c)
 * </pre>
 *
 */
trait Distributive[A] {
  
 /**
  * A binary  operation to be distributed over.
  */
  def op(a1: A, a2: A): A
  
  /**
   * A binary operation that does the distributing, "distributing op."
   * 
   * See the main documentation for this trait for more detail.
   */ 
  def dop(a1: A, a2: A): A
}

/**
 * Companion object for <code>Distributive</code>.
 * an implicit conversion method from <code>A</code> to <code>Distributive.Adapter[A]</code>
 */
object Distributive {

  /**
   * Adapter class for <a href="Distributive.html"><code>Distributive</code></a>
   * that wraps a value of type <code>A</code> given an
   * implicit <code>Distributive[A]</code>.
   *
   * @param underlying The value of type <code>A</code> to wrap.
   * @param distributive The captured <code>Distributive[A]</code> whose behavior
   *   is used to implement this class's methods.
   */
  class Adapter[A](val underlying: A)(implicit val distributive: Distributive[A]) {

    /**
     * A binary operation to be distributed over: used with dop.
     *
     * See the main documentation for trait <a href="Distributive.html"><code>Distributive</code></a> for more detail.
     */ 
    def op(a2: A): A = distributive.op(underlying, a2)
    
    /**
     * A binary operation that does the distributing.
     *
     * See the main documentation for trait <a href="Distributive.html"><code>Distributive</code></a> for more detail.
     */
    def dop(a2: A): A = distributive.dop(underlying, a2)
  }

  /**
   * Implicitly wraps an object in an <code>Distributive.Adapter[A]</code>
   * so long as an implicit <code>Distributive[A]</code> is available.
   */
  implicit def adapters[A](a: A)(implicit ev: Distributive[A]): Distributive.Adapter[A] = new Adapter(a)(ev)

  /**
   * Summons an implicitly available <code>Distributive[A]</code>.
   *
   * <p>
   * This method allows you to write expressions like <code>Distributive[Int]</code> instead of
   * <code>implicitly[Distributive[Ints]]</code>.
   * </p>
   */
  def apply[A](implicit ev: Distributive[A]): Distributive[A] = ev
}

