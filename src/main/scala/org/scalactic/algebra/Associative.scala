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
}

/**
 * Companion object for <code>Associative</code> that offers
 * an implicit conversion that wraps an object in an <code>Associative.Adapter</code>
 * so long as an implicit <code>Associative</code> is available.
 */
object Associative {

  /**
   * Adapter class for <code>Associative</code>.
   */
  class Adapter[A](val underlying: A)(implicit val associative: Associative[A]) {

    /**
     * A binary operation that obeys the associative law.
     */ 
    def op(a2: A): A = associative.op(underlying, a2)
  }

  /*
   * Implicitly wraps an object in an <code>Associative.Adapter[A]</code>
   * so long as an implicit <code>Associative[A]</code> is available.
   */
  implicit def adapters[A](a: A)(implicit ev: Associative[A]): Associative.Adapter[A] = new Adapter(a)(ev)
}

