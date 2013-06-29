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
   * Normalizes the passed object.
   *
   * @param a the object to normalize
   * @return the normalized form of the passed object
   */
  def normalized(a: A): A

  final def and(other: Normalization[A]): Normalization[A] =
    new Normalization[A] {
      // Note in Scaladoc what order, and recommend people don't do side effects anyway.
      // By order, I mean left's normalized gets called first then right's normalized gets called on that result, for "left and right"
      def normalized(a: A): A = other.normalized(thisNormalization.normalized(a))
    }
}

