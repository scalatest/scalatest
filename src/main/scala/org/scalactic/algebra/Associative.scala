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

/**
 * Trait representing a binary operation that obeys the associative law.
 *
 * <pre>
 * ((a op b) op c) == (a op (b op c))
 * </pre>
 *
 * <p>
 * Note: <code>Associative</code> models a <em>semigroup</em> in category theory terminology.
 * </p>
 */
trait Associative[A] {

  /**
   * A binary operation that obeys the associative law.
   */ 
  def op(a1: A, a2: A): A

  /**
   * Wraps an object in an <code>AssociativeAdapter</code>.
   */
  def apply(a: A): AssociativeAdapter[A] = new AssociativeAdapter(this, a)
}

/**
 * Adapter class for <code>Associative</code>.
 */
class AssociativeAdapter[A](associative: Associative[A], a1: A) {

  /**
   * A binary operation that obeys the associative law.
   */ 
  def op(a2: A): A = associative.op(a1, a2)
}

/**
 * Companion object for <code>AssociativeAdapter</code> that offers
 * an implicit conversion that wraps an object in an <code>AssociativeAdapter</code>
 * so long as an implicit <code>Associative</code> is available.
 */
object AssociativeAdapter {
  /**
   * Implicitly wraps an object in an <code>AssociativeAdapter</code>
   * so long as an implicit <code>Associative</code> is available.
   */
  implicit def adapt[A](a: A)(implicit ev: Associative[A]): AssociativeAdapter[A] = ev(a)
}







