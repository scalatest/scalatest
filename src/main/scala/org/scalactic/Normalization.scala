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
 * Defines a custom way to normalize instances of a type.
 *
 * <p>
 * For example, to normalize <code>Double</code>s by truncating off any decimal part,
 * you might write:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalactic._
 *
 * val truncated = 
 *   new Normalization[Double] {
 *    def normalized(d: Double) = d.floor
 *  }
 * </pre>
 *
 * <p>
 * Given this definition you could use it with the <a href="Explicitly.html"><code>Explicitly</code></a> DSL like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest._
 * import Matchers._
 * import TypeCheckedTripleEquals._
 * 
 * (2.1 should === (2.0)) (after being truncated)
 * </pre>
 *
 * <p>
 * Note that to use a <code>Normalization</code> with the <code>Explicitly</code> DSL, you'll need to be using either
 * <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> or
 * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>. If you're just using plain-old
 * <a href="TripleEquals.html"><code>TripleEquals</code></a>, you'll need a <a href="Uniformity.html"><code>Uniformity</code></a>, a <code>Normalization</code> subclass.
 * </p>
 *
 * <p>
 * If you make the <code>truncated</code> <code>val</code> implicit and import or mix in the members of <a href="NormMethods.html"><code>NormMethods</code></a>,
 * you can access the behavior by invoking <code>.norm</code> on <code>Double</code>s.
 * </p>
 *
 * <pre class="stHighlight">
 * implicit val doubleNormalization = truncated
 * import NormMethods._
 *
 * val d = 2.1
 * d.norm // returns 2.0
 * </pre>
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
   * @tparam A the type of the object to normalize
   * @param a the object to normalize
   * @return the normalized form of the passed object
   */
  def normalized(a: A): A

  /**
   * Returns a new <code>Normalization</code> that composes this and the passed <code>Normalization</code>.
   *
   * <p>
   * The <code>normalized</code> method of the <code>Normalization</code> returned by this method returns a normalized form of the passed
   * object obtained by forwarding the passed value first to this <code>Normalization</code>'s <code>normalized</code> method,
   * then passing that result to the other <code>Normalization</code>'s <code>normalized</code> method.
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

  /**
   * Converts this <code>Normalization</code> to a <code>NormalizingEquivalence[A]</code> whose <code>normalized</code>
   * method delegates to this <code>Normalization[A]</code> and whose <code>afterNormalizationEquivalence</code> field returns the
   * implicitly passed <code>Equivalence[A]</code>.
   *
   * @param equivalence the <code>Equivalence</code> that the returned <code>NormalizingEquivalence</code>
   *     will delegate to determine equality after normalizing both left and right (if appropriate) sides.
   */
  final def toEquivalence(implicit equivalence: Equivalence[A]): NormalizingEquivalence[A] =
    new NormalizingEquivalence[A] {
      override val afterNormalizationEquivalence = equivalence
      def normalized(a: A): A = thisNormalization.normalized(a)
    }
}

