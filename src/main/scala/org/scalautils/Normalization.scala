/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalautils

/**
 * Defines a custom way to normalize instances of a type.
 *
 * <p>
 * This trait exists to enforce that <code>Normalization</code> instances can be
 * passed to <code>whenBothAre</code> and <code>whenAllAre</code>, but not <code>decidedBy</code>.
 * </p>
 * 
 * @tparam A the type whose normalization is being defined
 */
trait Normalization[A] { thisNormalization =>

  /**
   * Returns a normalized form of the passed object.
   *
   * <p>
   * If the passed object is already in normal form, this method may return the same instance passed.
   * </p>
   *
   * @param a the object to normalize
   * @return the normalized form of the passed object
   */
  def normalized(a: A): A

  /**
   * Returns a new <code>Normalization</code> that composes this and the passed <code>Normalization</code>.
   *
   * <p>
   * The <code>normalized</code> method of the <code>Normalization</code>'s returned by this method returns a normalized form of the passed
   * object obtained by passing it first to this <code>Normalization</code>'s <code>normalized</code> method,
   * then passing that result to the other (<em>i.e.</em>, passed to <code>and</code> as <code>other</code>) <code>Normalization</code>'s <code>normalized</code> method.
   * Essentially, the body of the composed <code>normalized</code> method is:
   * </p>
   *
   * <pre class="stHighlight">
   * normalizationPassedToAnd.normalized(normalizationOnWhichAndWasInvoked.normalized(a))
   * </pre>
   *
   * @param other a <code>Normalization</code> to 'and' with this one
   * @return a <code>Normalization</code> representing the composition of this and the passed <code>Normalization</code>
   */
  final def and(other: Normalization[A]): Normalization[A] =
    new Normalization[A] {
      def normalized(a: A): A = other.normalized(thisNormalization.normalized(a))
    }
}

