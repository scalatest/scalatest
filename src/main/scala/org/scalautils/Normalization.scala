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

// TODO: Add an example of Array[String] isInstanceOfA here and in NormalizedEquality
  /**
   * Indicates whether the passed object is an instance of type <code>A</code>.
   *
   * <p>
   * This method is invoked by the <code>areEqual</code> method of subclass <code>NormalizedEquality</code> to determine whether or not
   * <code>b</code> can be cast to </code>A</code> so that it can be safely passed to <code>normalized</code>.
   * To implement this method, simply call <code>b.isInstanceOf[A]</code> for the actual <code>A</code> type.
   * For example, if you are defining a <code>NormalizedEquality[String]</code>, your <code>isInstanceOf</code>
   * method should look like:
   * </p>
   *
   * <pre class="stHighlight">
   * def isInstanceOfA(b: Any) = b.isInstanceOf[String]
   * </pre>
   *
   * <p>
   * If you are defining a <code>NormalizedEquality[xml.Node]</code> your <code>isInstanceOf</code> method
   * should look like:
   * </p>
   *
   * <pre class="stHighlight">
   * def isInstanceOfA(b: Any) = b.isInstanceOf[xml.Node]
   * </pre>
   *
   * @param b the object to inspect to determine whether it is an instance of <code>A<code>
   * @return true if the passed object is an instance of <code>A</code>
   */
  def isInstanceOfA(b: Any): Boolean

  /**
   * Normalizes the passed object.
   *
   * @param a the object to normalize
   * @return the normalized form of the passed object
   */
  def normalized(a: A): A

  final def and(other: Normalization[A]): Normalization[A] =
    new Normalization[A] {
      // Note in Scaladoc that we just use this one. (They should be the same).
      def isInstanceOfA(b: Any): Boolean = thisNormalization.isInstanceOfA(b)
      // Note in Scaladoc what order, and recommend people don't do side effects anyway.
      def normalized(a: A): A = other.normalized(thisNormalization.normalized(a))
    }
}

