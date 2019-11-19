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
 * An <code>Equality[A]</code> implementation that determines the equality of two objects by normalizing 
 * one or both objects, then comparing the results using an &ldquo;after normalization&rdquo; equality referenced from
 * the <code>afterNormalizationEquality</code>  member. By default, the <code>afterNormalizationEquality</code> is 
 * an instance of <a href="Equality$.html"><code>Equality.default[A]</code></a>.
 * </p>
 *
 * <p>
 * <code>NormalizingEquality</code> is returned by the <code>Explicitly</code> DSL's &ldquo;<code>after</code> <code>being</code>&rdquo;
 * syntax, using for the <code>afterNormalizationEquality</code> the implicit <code>Equality</code> in scope for the type
 * of <code>Uniformity</code> passed to <code>being</code>. Here's an example:
 * </p>
 *
 * <pre class="stREPL"> 
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import Explicitly._
 * import Explicitly._
 *
 * scala&gt; import StringNormalizations._
 * import StringNormalizations._
 *
 * scala&gt; after being lowerCased
 * res0: org.scalactic.NormalizingEquality[String] = ComposedNormalizingEquality(Equality.default,lowerCased)
 * </pre>
 */
trait NormalizingEquality[A] extends Equality[A] { thisNormEq =>

  /**
   * The <code>Equality</code> with which to determine equality after normalizing the left-hand and, if appropriate,
   * the right-hand values.
   *
   * <p>
   * In this trait's implementation, this <code>val</code> is initialized with the result of invoking <code>Equality.default[A]</code>.
   * Thus default <code>Equality</code> is the default <code>afterNormalizationEquality</code>. This may be changed by overriding
   * <code>afterNormalizationEquality</code> in subclasses.
   * </p>
   */
  val afterNormalizationEquality: Equality[A] = Equality.default[A]

  /**
   * Determines the equality of two objects by normalizing the left-hand value, <code>a</code>, and, if appropriate, the right-hand
   * value, <code>b</code>, then passing them to <code>areEqual</code> method of <code>afterNormalizationEquality</code>.
   *
   * <p>
   * The left-hand value, <code>a</code>, is normalized by passing it to the <code>normalized</code> method of this
   * <code>NormalizingEquality</code>. The right-hand value, <code>b</code>, is normalized, if appropriate, by passing it
   * to the <code>normalizedOrSame</code> method of this <code>NormalizingEquality</code>.
   * </p>
   */
  final def areEqual(a: A, b: Any): Boolean = {
    afterNormalizationEquality.areEqual(normalized(a), normalizedOrSame(b))
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
   * Indicates whether this <code>NormalizingEquality</code>'s <code>normalized</code> method can &ldquo;handle&rdquo; the passed object, if cast to the
   * appropriate type <code>A</code>.
   *
   * <p>
   * If this method returns true for a particular passed object, it means that if the object is passed
   * to <code>normalizedOrSame</code>, that method will return the result of passing it to <code>normalized</code>.
   * It does not mean that the object will necessarily be <em>modified</em> when passed to <code>normalizedOrSame</code> or <code>normalized</code>.
   * For more information and examples, see the documentation for <code>normalizedCanHandle</code> in trait <a href="Uniformity.html"><code>Uniformity</code></a>,
   * which has the same contract.
   * </p>
   */
  def normalizedCanHandle(b: Any): Boolean

  /**
   * Returns either the result of passing this object to <code>normalized</code>, if appropriate, or the same object.
   *
   * @param b the object to normalize, if appropriate
   * @return a normalized form of the passed object, if this <code>Uniformity</code> was able to normalize it, else the same object passed
   */
  def normalizedOrSame(b: Any): Any

  /**
   * Returns a new <code>NormalizingEquality</code> that combines this and the passed <code>Uniformity</code>.
   *
   * <p>
   * The <code>normalized</code> and <code>normalizedOrSame</code> methods
   * of the <code>NormalizingEquality</code>'s returned by this method return a result 
   * obtained by forwarding the passed value first to this <code>NormalizingEquality</code>'s implementation of the method,
   * then passing that result to the passed <code>Uniformity</code>'s implementation of the method, respectively.
   * Essentially, the body of the composed <code>normalized</code> method is:
   * </p>
   *
   * <pre class="stHighlight">
   * uniformityPassedToAnd.normalized(uniformityOnWhichAndWasInvoked.normalized(a))
   * </pre>
   *
   * <p>
   * And the body of the composed <code>normalizedOrSame</code> method is:
   * </p>
   *
   * <pre class="stHighlight">
   * uniformityPassedToAnd.normalizedOrSame(uniformityOnWhichAndWasInvoked.normalizedOrSame(a))
   * </pre>
   *
   * <p>
   * The <code>normalizeCanHandle</code> method of the <code>NormalizingEquality</code> returned by this method returns a result 
   * obtained by anding the result of forwarding the passed value to this <code>NormalizingEquality</code>'s implementation of the method
   * with the result of forwarding it to the passed <code>Uniformity</code>'s implementation.
   * Essentially, the body of the composed <code>normalizeCanHandle</code> method is:
   * </p>
   *
   * <pre class="stHighlight">
   * normEqOnWhichAndWasInvoked.normalizeCanHandle(a) &amp;&amp; uniformityPassedToAnd.normalizeCanHandle(a)
   * </pre>
   *
   * @param other a <code>Uniformity</code> to 'and' with this one
   * @return a <code>NormalizingEquality</code> representing the composition of this and the passed <code>Uniformity</code>
   */
  final def and(other: Uniformity[A]): NormalizingEquality[A] =
    new ComposedNormalizingEquality[A](afterNormalizationEquality, this.toUniformity and other)

  // TODO: If someone passes a Normalization only to and, should I give them back a NormalizingEquivalence?
  // If so, the same thing should be one if Equality.afterBeing gets just a Normalization.

  /**
   * Converts this <code>NormalizingEquality</code> to a <code>Uniformity</code>.
   *
   * @return a <code>Uniformity</code> whose <code>normalized</code>, <code>normalizedCanHandle</code>, and <code>normalizedOrSame</code> methods 
   *     are implemented by the corresponding methods of this <code>NormalizingEquality</code>.
   */
  final def toUniformity: Uniformity[A] =
    new Uniformity[A] {
      def normalized(a: A): A = thisNormEq.normalized(a)
      def normalizedCanHandle(b: Any): Boolean = thisNormEq.normalizedCanHandle(b)
      def normalizedOrSame(b: Any): Any = thisNormEq.normalizedOrSame(b)
    }
}

