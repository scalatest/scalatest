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
 * An <code>Equality</code> implementation that determines the equality of two objects by normalizing 
 * both objects, if possible, and then comparing the results using default equality (as defined by
 * the <code>areEqual</code> method of <a href="DefaultEquality.html"><code>DefaultEquality</code></a>).
 * </p>
 * <pre class="stHighlight">
 * import org.scalautils._
 *
 * class StringEquality extends NormalizedEquality[String] {
 *   def isInstanceOfA(b: Any) = b.isInstanceOf[String]
 *   def normalized(s: String): String = s.trim.toLowerCase
 * }
 * </pre>
 *
 */
abstract class NormalizingEquality[A](equality: Equality[A] = new DefaultEquality[A]) extends Equality[A] { thisNormEq =>

  def usesDefaultEquality: Boolean = equality.isInstanceOf[DefaultEquality[_]]

  final def areEqual(a: A, b: Any): Boolean = {
    val nb = if (isInstanceOfA(b)) normalized(b.asInstanceOf[A]) else b
    equality.areEqual(normalized(a), nb)
  }

  def isInstanceOfA(b: Any): Boolean

  def normalized(a: A): A

  final def and(other: Normalization[A]): NormalizingEquality[A] =
    new ComposedNormalizingEquality[A](equality, this.toNormalization and other)

  final def toNormalization: Normalization[A] =
    new Normalization[A] {
      // Note in Scaladoc that we just use this one. (They should be the same).
      def isInstanceOfA(b: Any): Boolean = thisNormEq.isInstanceOfA(b)
      // Note in Scaladoc what order, and recommend people don't do side effects anyway.
      def normalized(a: A): A = thisNormEq.normalized(a)
    }
}

