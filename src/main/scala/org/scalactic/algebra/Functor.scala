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
 * Algebraic structure containing a <em>map</em> method that obeys laws of <em>identity</em> and <em>composition</em>.
 */
trait Functor[Context[_]] {

  /**
   * Produces a <code>FunctorAdapter</code> wrapping the given context instance.
   */
  def apply[A](ct: Context[A]): FunctorAdapter[Context, A]
}

/**
 * Adapter for algebraic structure containing a <em>mapping</em> method that obeys laws of <em>identity</em> and <em>composition</em>.
 *
 * <p>
 * A <code>FunctorAdapter</code> instance wraps an object that in some way behaves as a <code>Functor</code>.
 * </p>
 */
trait FunctorAdapter[Context[_], A] {

  /**
   * Applies the given function to the value contained in this context, returning the result 
   * of the function lifted into the same context.
   *
   * <p>
   * Given the functions:
   * </p>
   *
   * <ul>
   * <li><code>id</code>: <code>A => A</code> // Identity function, <code>(o: T) => o</code></li>
   * <li><code>g</code>: <code>A => B</code></li>
   * <li><code>f</code>: <code>B => C</code></li>
   * </ul>
   *
   * <p>
   * Implementations of this trait obey the following laws:
   * </p>
   *
   * <ul>
   * <li>identity: <code>functor.map(a => a)</code> <code>===</code> <code>functor</code></li>
   * <li>composite: <code>functor.map(g).map(f)</code> <code>===</code> <code>functor.map(f compose g)</code></li>
   * </ul>
   */
  def map[B](f: A => B): Context[B]
}
