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
 * both objects, if possible, and then comparing the results using an equality (as defined by
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
private[scalautils] final class ComposedNormalizingEquivalence[A](
  override val afterNormalizationEquivalence: Equivalence[A],
  normalization: Normalization[A]
) extends NormalizingEquivalence[A] {

  /**
   * Normalizes the passed object.
   *
   * @param o the object to normalize
   * @return the normalized form of the passed object
   */
  def normalized(a: A): A = normalization.normalized(a)
}

