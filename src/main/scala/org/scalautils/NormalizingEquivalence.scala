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
trait NormalizingEquivalence[A] extends Equivalence[A] { thisNormEq =>

  val afterNormalizationEquivalence: Equivalence[A] = new DefaultEquality[A]

  final def areEquivalent(a: A, b: A): Boolean = {
    afterNormalizationEquivalence.areEquivalent(normalized(a), normalized(b))
  }

  def normalized(a: A): A

  final def and(other: Normalization[A]): NormalizingEquivalence[A] =
    new ComposedNormalizingEquivalence[A](afterNormalizationEquivalence, this.toNormalization and other)

  final def toNormalization: Normalization[A] =
    new Normalization[A] {
      def normalized(a: A): A = thisNormEq.normalized(a)
    }
}

