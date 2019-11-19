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
package org.scalactic

/**
 * An <code>Equivalence[A]</code> implementation that determines the equality of two objects by normalizing 
 * one or both objects, then comparing the results using an &ldquo;after normalization&rdquo; <code>Equivalence</code> referenced from
 * the <code>afterNormalizationEquivalence</code>  member. By default, the <code>afterNormalizationEquivalence</code> is 
 * an instance of <a href="Equivalence$.html"><code>Equivalence.default[A]</code></a>.
 * </p>
 *
 * <p>
 * <code>NormalizingEquivalence</code> is returned by the <code>Explicitly</code> DSL's &ldquo;<code>after</code> <code>being</code>&rdquo;
 * syntax, using for the <code>afterNormalizationEquivalence</code> the implicit <code>Equivalence</code> in scope for the type
 * of <code>Normalization</code> passed to <code>being</code>. Here's an example:
 * </p>
 *
 * <pre class="stREPL"> 
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import Explicitly._
 * import Explicitly._
 *
 * scala&gt; val lowerCased: Normalization[String] = StringNormalizations.lowerCased
 * lowerCased: org.scalactic.Normalization[String] = lowerCased
 *
 * scala&gt; after being lowerCased
 * res0: org.scalactic.NormalizingEquivalence[String] = ComposedNormalizingEquivalence(Equality.default,lowerCased)
 * </pre>
 */
trait NormalizingEquivalence[A] extends Equivalence[A] { thisNormEq =>

  /**
   * The <code>Equivalence</code> with which to determine equality after normalizing the left-hand and right-hand values.
   *
   * <p>
   * In this trait's implementation, this <code>val</code> is initialized with the result of invoking <code>Equivalence.default[A]</code>.
   * Thus default <code>Equivalence</code> is the default <code>afterNormalizationEquivalence</code>. This may be changed by overriding
   * <code>afterNormalizationEquivalence</code> in subclasses.
   * </p>
   */
  val afterNormalizationEquivalence: Equivalence[A] = Equivalence.default[A]

  /**
   * Determines the equality of two objects by normalizing the left-hand value, <code>a</code>, and the right-hand
   * value, <code>b</code>, then passing them to <code>areEquivalent</code> method of <code>afterNormalizationEquivalence</code>.
   *
   * <p>
   * Both the left-hand value, <code>a</code>, and right-hand value, <code>b</code>, are normalized by passing them to the <code>normalized</code> method of this
   * <code>NormalizingEquivalence</code>.
   * </p>
   */
  final def areEquivalent(a: A, b: A): Boolean = {
    afterNormalizationEquivalence.areEquivalent(normalized(a), normalized(b))
  }

  /**
   * Returns a normalized form of the passed object.
   *
   * <p>
   * If the passed object is already in normal form, this method may return the same instance passed.
   * </p>
   *
   * @tparam A the type of the object to normalize
   * @param a the object to normalize
   * @return the normalized form of the passed object
   */
  def normalized(a: A): A

  /**
   * Returns a new <code>NormalizingEquivalence</code> that combines this and the passed <code>Normalization</code>.
   *
   * <p>
   * The <code>normalized</code> method of the <code>NormalizingEquivalence</code>'s returned by this method returns a result 
   * obtained by forwarding the passed value first to this <code>NormalizingEquivalence</code>'s implementation of the method,
   * then passing that result to the passed <code>Normalization</code>'s implementation of the method.
   * Essentially, the body of the composed <code>normalized</code> method is:
   * </p>
   *
   * <pre class="stHighlight">
   * uniformityPassedToAnd.normalized(uniformityOnWhichAndWasInvoked.normalized(a))
   * </pre>
   *
   * @param other a <code>Normalization</code> to 'and' with this one
   * @return a <code>NormalizingEquivalence</code> representing the composition of this and the passed <code>Normalization</code>
   */
  final def and(other: Normalization[A]): NormalizingEquivalence[A] =
    new ComposedNormalizingEquivalence[A](afterNormalizationEquivalence, this.toNormalization and other)

  /**
   * Converts this <code>NormalizingEquivalence</code> to a <code>Normalization</code>.
   *
   * @return a <code>Normalization</code> whose <code>normalized</code> method 
   *     is implemented by forwarding to the <code>normalized</code> method of this <code>NormalizingEquivalence</code>.
   */
  final def toNormalization: Normalization[A] =
    new Normalization[A] {
      def normalized(a: A): A = thisNormEq.normalized(a)
    }
}

