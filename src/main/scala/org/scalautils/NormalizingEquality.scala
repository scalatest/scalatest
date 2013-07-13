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
 * one or both objects, then comparing the results using an "after normalization" equality referenced from
 * the <code>afterNormalizationEquality</code>  member. By default, the <code>afterNormalizationEquality</code> is 
 * an instance of <a href="DefaultEquality.html"><code>DefaultEquality</code></a>.
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
trait NormalizingEquality[A] extends Equality[A] { thisNormEq =>

  val afterNormalizationEquality: Equality[A] = new DefaultEquality[A]

  final def areEqual(a: A, b: Any): Boolean = {
    afterNormalizationEquality.areEqual(normalized(a), normalizedOrSame(b))
  }

  def normalized(a: A): A

  def canNormalize(b: Any): Boolean

  def normalizedOrSame(b: Any): Any

  final def and(other: Uniformity[A]): NormalizingEquality[A] =
    new ComposedNormalizingEquality[A](afterNormalizationEquality, this.toUniformity and other)

  // TODO: If someone passes a Normalization only to and, should I give them back a NormalizingEquivalence?
  // If so, the same thing should be one if Equality.afterBeing gets just a Normalization.

  final def toUniformity: Uniformity[A] =
    new Uniformity[A] {
      def normalized(a: A): A = thisNormEq.normalized(a)
      def canNormalize(b: Any): Boolean = thisNormEq.canNormalize(b)
      def normalizedOrSame(b: Any): Any = thisNormEq.normalizedOrSame(b)
    }
}

