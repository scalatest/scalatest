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
 * Defines a custom way to determine equality for a type.
 *
 * <p>
 * For example, here's how you could define equality between <code>Double</code>s such that
 * a tolerance of &plusmn; 0.01 is allowed:
 * </p>
 *
 * <pre>
 * class TolerantDoubleEquality extends Equality[Double] {
 *
 *   private val Tol = 0.01
 *
 *   def areEqual(a: Double, b: Any): Boolean = {
 *     b match {
 *       case bDouble: Double =&gt; (a <= bDouble + Tol) && (a >= bDouble - Tol)
 *       case _ =&gt; false
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If an implicit instance of <code>TolerantDoubleEquality</code> is in scope, it will be
 * used by ScalaTest's <code>===</code> operators and its <code>should equal</code> and <code>should ===</code> matcher
 * syntax. Here's an example:
 * </p>
 *
 * <pre>
 * $ scala -cp target/jar_contents/
 * Welcome to Scala version 2.10.0 (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_33).
 * Type in expressions to have them evaluated.
 * Type :help for more information.

 * scala&gt; import org.scalautils._
 * import org.scalautils._

 * scala&gt; class TolerantDoubleEquality extends Equality[Double] {
 *      | 
 *      |   private val Tol = 0.01
 *      | 
 *      |   def areEqual(a: Double, b: Any): Boolean = {
 *      |     b match {
 *      |       case bDouble: Double =&gt; (a &gt;= bDouble + Tol) && (a &gt;= bDouble - Tol)
 *      |       case _ =&gt; false
 *      |     }
 *      |   }
 *      | }
 * defined class TolerantDoubleEquality
 *
 * scala&gt; import TripleEquals._
 * import TripleEquals._
 *
 * scala&gt; 2.0 === 2.001
 * res0: Boolean = false
 *
 * scala&gt; implicit val tolerantDoubleEquality = new TolerantDoubleEquality
 * tolerantDoubleEquality: TolerantDoubleEquality = TolerantDoubleEquality@70c13c17
 *
 * scala&gt; 2.0 === 2.001
 * res1: Boolean = true
 * </pre>
 *
 * <p>
 * <em>Note: The <code>Equality</code> type class was inspired in part by the <code>Equal</code> type class of the 
 * <a href="http://code.google.com/p/scalaz/" target="_blank"><code>scalaz</code></a> project.</em>
 * </p>
 *
 * @tparam A the type whose equality is being customized
 */
class DecidedByEquality[A](equality: Equality[A]) extends Equality[A] {

  /**
   * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal.
   *
   * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   * @return true if the passed objects are "equal," as defined by this <code>Equality</code> instance
   */
  def areEqual(a: A, b: Any): Boolean = equality.areEqual(a, b)

  def afterBeing(normalization: Normalization[A]): NormalizingEquality[A] =
    new ComposedNormalizingEquality[A](equality, normalization)
} 

