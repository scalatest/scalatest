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
 * Provides ScalaTest's &ldquo;explicitly DSL,&rdquo; which facilitates the
 * explicit specification of an <code>Equality[T]</code> or a <code>Uniformity[T]</code> where
 * <code>Equality[T]</code> is taken implicitly.
 *
 * <p>
 * The Explicitly DSL can be used with the <code>===</code> and <code>!==</code> operators of ScalaUtils
 * as well as the <code>should</code> <code>equal</code>, <code>be</code>, <code>contain</code>, and
 * <code>===</code> syntax of ScalaTest matchers. 
 * </p>
 * 
 * <p>
 * If you want to customize equality for a type in general, you would likely want to place an
 * implicit <code>Equality[T]</code> for that type in scope (or in <code>T</code>'s companion object). That implicit
 * equality definition will then be picked
 * up and used when that type is compared for equality with the <code>equal</code>, <code>be</code>, and
 * <code>contain</code> matchers in ScalaTest tests and with
 * <code>===</code> in both tests and production code.
 * If you just want to use a custom equality for a single comparison, however, you may prefer to pass it explicitly. For
 * example, if you have an implicit
 * <code>Equality[String]</code> in scope, you can force a comparison to use the default equality with this syntax:
 * </p>
 *
 * <pre class="stHighlight">
 * // In production code:
 * if ((result === "hello")(decided by defaultEquality)) true else false
 *
 * // In tests:
 * result should equal ("hello") (decided by defaultEquality)
 * </pre>
 *
 * <p>
 * The explicitly DSL also provides support for specifying a one-off equality that is based on a normalization. For
 * example, ScalaUtils offers a <a href="StringNormalizations.html"><code>StringNormalizations</code></a> trait that
 * provides methods such as <code>trimmed</code> and <code>lowerCased</code> that return
 * <code>Normalization[String]</code> instances that normalize by trimming and lower-casing, respectively. If you bring
 * those into scope by mixing in or importing the members of <code>StringNormalizations</code>, you could use the
 * explicitly DSL like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // In production code:
 * if ((result === "hello")(after being lowerCased)) true else false
 *
 * // In tests:
 * result should equal ("hello") (after being lowerCased and trimmed)
 * </pre>
 *
 * @author Bill Venners
 */
trait Explicitly {

  /**
   * This class is part of the ScalaUtils &ldquo;explicitly DSL&rdquo;. Please
   * see the documentation for <a href="Explicitly.html"><code>Explicitly</code></a> for an overview of
   * the explicitly DSL.
   *
   * @author Bill Venners
   */
  class DecidedWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should equal ("hello") (decided by defaultEquality)
     *                                        ^
     * </pre>
     */
    def by[A](equality: Equality[A]): DecidedByEquality[A] = new DecidedByEquality[A](equality)
  }

  /**
   * This field enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * result should equal ("hello") (decided by defaultEquality)
   *                                ^
   * </pre>
   */
  val decided = new DecidedWord

  /**
   * This class is part of the ScalaUtils &ldquo;explicitly DSL&rdquo;. Please
   * see the documentation for <a href="Explicitly.html"><code>Explicitly</code></a> for an overview of
   * the explicitly DSL.
   *
   * @author Bill Venners
   */
  class DeterminedWord {

    /**
     * This method enables the following syntax, given an <a href="Equivalence.html"><code>Equivalence[String]</code></a>
     * named <code>myStringEquivalence</code>:
     *
     * <pre class="stHighlight">
     * result should equal ("hello") (determined by myStringEquivalence)
     *                                           ^
     * </pre>
     *
     */
    def by[A](equivalence: Equivalence[A]): DeterminedByEquivalence[A] = new DeterminedByEquivalence[A](equivalence)
  }

  /**
   * This field enables syntax such as the following, given an
   * <a href="Equivalence.html"><code>Equivalence[String]</code></a> named <code>myStringEquivalence</code>:
   *
   * <pre class="stHighlight">
   * result should equal ("hello") (determined by myStringEquivalence)
   *                                ^
   * </pre>
   */
  val determined = new DeterminedWord

  /**
   * This class is part of the ScalaUtils &ldquo;explicitly DSL&rdquo;. Please
   * see the documentation for <a href="Explicitly.html"><code>Explicitly</code></a> for an overview of
   * the explicitly DSL.
   *
   * <p>
   * Instances of this class are returned via the <code>decided</code> <code>by</code> <code>&lt;an Equality&gt;</code>
   * syntax, and enables <code>afterBeing</code> to be invoked on it. Here's an example, given an
   * <code>Equality[String]</code> named <code>myStringEquality</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * result should equal ("hello") (decided by myStringEquality afterBeing lowerCased)
   * </pre>
   *
   * @author Bill Venners
   */
  class DecidedByEquality[A](equality: Equality[A]) extends Equality[A] {

    /**
     * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal by
     * delegating to the <code>areEqual</code> method of the <code>Equality[T]</code> passed to
     * this class's constructor.
     *
     * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
     * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
     * @return true if the passed objects are "equal," as defined by this <code>Equality</code> instance
     */
    def areEqual(a: A, b: Any): Boolean = equality.areEqual(a, b)

    /**
     * This method enables syntax such as the following, given an
     * <a href="Equality.html"><code>Equality[String]</code></a> named <code>myStringEquality</code>:
     *
     * <pre class="stHighlight">
     * result should equal ("hello") (decided by myStringEquality afterBeing lowerCased)
     *                                                            ^
     * </pre>
     *
     * @param uniformity A <code>Uniformity</code> with which to normalize objects of type <code>A</code>
     * before comparing them for equality using the <code>Equality[A]</code> passed to this object's
     * constructor.
     */
    def afterBeing(uniformity: Uniformity[A]): NormalizingEquality[A] =
      new ComposedNormalizingEquality[A](equality, uniformity)
  } 

  /**
   * This class is part of the ScalaUtils &ldquo;explicitly DSL&rdquo;. Please
   * see the documentation for <a href="Explicitly.html"><code>Explicitly</code></a> for an overview of
   * the explicitly DSL.
   *
   * <p>
   * Instances of this class are returned via the <code>decided</code> <code>by</code> <code>&lt;an Equivalence&gt;</code>
   * syntax, and enables <code>afterBeing</code> to be invoked on it. Here's an example, given an
   * <code> Equivalence[String]</code> named <code>myStringEquivalence</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * result should equal ("hello") (determined by myStringEquivalence afterBeing lowerCased)
   * </pre>
   *
   * @author Bill Venners
   */
  class DeterminedByEquivalence[T](equivalence: Equivalence[T]) extends Equivalence[T] {
  
    /**
     * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal by 
     * delegating to the <code>areEquivalent</code> method of the <code>Equivalence[T]</code> passed to
     * this class's constructor.
     *
     * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
     * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
     * @return true if the passed objects are "equal," as defined by this <code>Equality</code> instance
     */
    def areEquivalent(a: T, b: T): Boolean = equivalence.areEquivalent(a, b)
  
    /**
     * This method enables syntax such as the following, given an
     * <a href="Equivalence.html"><code>Equivalence[String]</code></a> named <code>myStringEquivalence</code>:
     *
     * <pre class="stHighlight">
     * result should equal ("hello") (determined by myStringEquivalence afterBeing lowerCased)
     *                                                                  ^
     * </pre>
     *
     * @param normalization A <code>Normalization</code> with which to normalize objects of type <code>T</code>
     * before comparing them for equality using the <code>Equivalence[T]</code> passed to this object's
     * constructor.
     */
    def afterBeing(normalization: Normalization[T]): NormalizingEquivalence[T] =
      new ComposedNormalizingEquivalence[T](equivalence, normalization)
  } 

  /**
   * This class is part of the ScalaUtils &ldquo;explicitly DSL&rdquo;. Please
   * see the documentation for <a href="Explicitly.html"><code>Explicitly</code></a> for an overview of
   * the explicitly DSL.
   *
   * @author Bill Venners
   */
  class TheAfterWord {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should equal ("hello") (after being lowerCased)
     *                                ^
     * </pre>
     *
     * @param uniformity a <code>Uniformity</code> with which to normalize an object of type <code>N</code>
     *          before comparing it for equality with another <code>N</code> using the implicitly
     *          passed <code>Equality[N]</code>.
     */
    def being[N](uniformity: Uniformity[N])(implicit equality: Equality[N]): NormalizingEquality[N] =
      new ComposedNormalizingEquality[N](equality, uniformity)

    /**
     * This method enables syntax such as the following, given a <code>Normalization[String]</code> named
     * <code>capitalized</code> (and assuming the members of
     * <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> or
     * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a> are in scope:
     *
     * <pre class="stHighlight">
     * result should === ("hello") (after being capitalized)
     *                                    ^
     * </pre>
     *
     * @param normalization a <code>Normalization</code> with which to normalize an object of type <code>N</code>
     *          before comparing it for equality with another <code>N</code> using the implicitly
     *          passed <code>Equivalence[N]</code>.
     */
    def being[N](normalization: Normalization[N])(implicit equivalence: Equivalence[N]): NormalizingEquivalence[N] =
      new ComposedNormalizingEquivalence[N](equivalence, normalization)
  }

  /**
   * This field enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * result should equal ("hello") (after being lowerCased)
   *                                ^
   * </pre>
   */
  val after = new TheAfterWord
}

/**
 * Companion object for <code>Explicitly</code>, which enables the ScalaUtils explicitly DSL to 
 * be imported rather than mixed in, like this:
 *
 * <pre class="stHighlight">
 * import org.scalautils._
 * import Explicitly._
 *
 * // Use the explicitly DSL...
 * </pre>
 */
object Explicitly extends Explicitly

