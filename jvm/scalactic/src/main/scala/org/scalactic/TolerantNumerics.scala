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

/*
 * Defines a custom way to determine equality for a type.
 *
 * <p>
 * For example, here's how you could define equality between <code>Double</code>s such that
 * a tolerance of &plusmn; 0.01 is allowed:
 * </p>
 *
 * <pre class="stHighlight">
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
 * <pre class="stREPL">
 * $ scala -cp target/jar_contents/
 * Welcome to Scala version 2.10.0 (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_33).
 * Type in expressions to have them evaluated.
 * Type :help for more information.

 * scala&gt; import org.scalactic._
 * import org.scalactic._

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
 * <a href="https://github.com/scalaz/scalaz/" target="_blank"><code>scalaz</code></a> project.</em>
 * </p>
 *
 * @tparam A the type whose equality is being customized
 */
/**
 * Provides <code>Equality</code> and <code>Equivalence</code> instances for <code>Numeric</code> types that 
 * compare for equality with a given tolerance.
 *
 * <p>Here's an example:</p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import TripleEquals._
 * import TripleEquals._
 *
 * scala&gt; 2.001 === 2.0
 * res0: Boolean = false
 *
 * scala&gt; implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)
 * doubleEquality: org.scalactic.Equality[Double] = org.scalactic.TolerantNumerics$$anon$1@16c2bd13
 *
 * scala&gt; 2.001 === 2.0
 * res1: Boolean = true
 * </pre>
 */
trait TolerantNumerics {

// TODO: Pretty toStrings on the results
  /**
   * Provides an <code>Equality</code> instance for <code>Double</code>s that 
   * compares for equality with the passed tolerance.
   *
   * @param tolerance the tolerance with which the returned <code>Equality</code> will compare <code>Double</code>s.
   * @return an <code>Equality</code> that compares <code>Double</code>s using the passed tolerance.
   */
  def tolerantDoubleEquality(tolerance: Double): Equality[Double] = {
    if (tolerance <= 0.0)
      throw new IllegalArgumentException(tolerance.toString + " passed to tolerantDoubleEquality was zero or negative. Must be a positive non-zero number.")
    new Equality[Double] {
      def areEqual(a: Double, b: Any): Boolean = {
        b match {
          case bDouble: Double => (a <= bDouble + tolerance) && (a >= bDouble - tolerance)
          case _ => false
        }
      }
      override def toString: String = s"TolerantDoubleEquality($tolerance)"
    } 
  } 

  /**
   * Provides an <code>Equality</code> instance for <code>Float</code>s that 
   * compares for equality with the passed tolerance.
   *
   * @param tolerance the tolerance with which the returned <code>Equality</code> will compare <code>Float</code>s.
   * @return an <code>Equality</code> that compares <code>Float</code>s using the passed tolerance.
   */
  def tolerantFloatEquality(tolerance: Float): Equality[Float] = {
    if (tolerance <= 0.0f)
      throw new IllegalArgumentException(tolerance.toString + " passed to tolerantFloatEquality was zero or negative. Must be a positive non-zero number.")
    new Equality[Float] {
      def areEqual(a: Float, b: Any): Boolean = {
        b match {
          case bFloat: Float => (a <= bFloat + tolerance) && (a >= bFloat - tolerance)
          case _ => false
        }
      }
      override def toString: String = s"TolerantFloatEquality($tolerance)"
    } 
  } 

  /**
   * Provides an <code>Equality</code> instance for <code>Long</code>s that 
   * compares for equality with the passed tolerance.
   *
   * @param tolerance the tolerance with which the returned <code>Equality</code> will compare <code>Long</code>s.
   * @return an <code>Equality</code> that compares <code>Long</code>s using the passed tolerance.
   */
  def tolerantLongEquality(tolerance: Long): Equality[Long] = {
    if (tolerance <= 0L)
      throw new IllegalArgumentException(tolerance.toString + " passed to tolerantLongEquality was zero or negative. Must be a positive non-zero number.")
    new Equality[Long] {
      def areEqual(a: Long, b: Any): Boolean = {
        b match {
          case bLong: Long => (a <= bLong + tolerance) && (a >= bLong - tolerance)
          case _ => false
        }
      }
      override def toString: String = s"TolerantLongEquality($tolerance)"
    } 
  } 

  /**
   * Provides an <code>Equality</code> instance for <code>Int</code>s that 
   * compares for equality with the passed tolerance.
   *
   * @param tolerance the tolerance with which the returned <code>Equality</code> will compare <code>Int</code>s.
   * @return an <code>Equality</code> that compares <code>Int</code>s using the passed tolerance.
   */
  def tolerantIntEquality(tolerance: Int): Equality[Int] = {
    if (tolerance <= 0)
      throw new IllegalArgumentException(tolerance.toString + " passed to tolerantIntEquality was zero or negative. Must be a positive non-zero number.")
    new Equality[Int] {
      def areEqual(a: Int, b: Any): Boolean = {
        b match {
          case bInt: Int => (a <= bInt + tolerance) && (a >= bInt - tolerance)
          case _ => false
        }
      }
      override def toString: String = s"TolerantIntEquality($tolerance)"
    } 
  } 

  /**
   * Provides an <code>Equality</code> instance for <code>Short</code>s that 
   * compares for equality with the passed tolerance.
   *
   * @param tolerance the tolerance with which the returned <code>Equality</code> will compare <code>Short</code>s.
   * @return an <code>Equality</code> that compares <code>Short</code>s using the passed tolerance.
   */
  def tolerantShortEquality(tolerance: Short): Equality[Short] = {
    if (tolerance <= 0)
      throw new IllegalArgumentException(tolerance.toString + " passed to tolerantShortEquality was zero or negative. Must be a positive non-zero number.")
    new Equality[Short] {
      def areEqual(a: Short, b: Any): Boolean = {
        b match {
          case bShort: Short => (a <= bShort + tolerance) && (a >= bShort - tolerance)
          case _ => false
        }
      }
      override def toString: String = s"TolerantShortEquality($tolerance)"
    } 
  } 

  /**
   * Provides an <code>Equality</code> instance for <code>Byte</code>s that 
   * compares for equality with the passed tolerance.
   *
   * @param tolerance the tolerance with which the returned <code>Equality</code> will compare <code>Byte</code>s.
   * @return an <code>Equality</code> that compares <code>Byte</code>s using the passed tolerance.
   */
  def tolerantByteEquality(tolerance: Byte): Equality[Byte] = {
    if (tolerance <= 0)
      throw new IllegalArgumentException(tolerance.toString + " passed to tolerantByteEquality was zero or negative. Must be a positive non-zero number.")
    new Equality[Byte] {
      def areEqual(a: Byte, b: Any): Boolean = {
        b match {
          case bByte: Byte => (a <= bByte + tolerance) && (a >= bByte - tolerance)
          case _ => false
        }
      }
      override def toString: String = s"TolerantByteEquality($tolerance)"
    } 
  } 

  /**
   * Provides an <code>Equivalence[N]</code> instance for any type for which a <code>Numeric[N]</code> is available that 
   * compares <code>N</code>s for equality with the passed tolerance.
   *
   * @param tolerance the tolerance with which the returned <code>Equality</code> will compare <code>Numeric</code>s.
   * @return an <code>Equivalence</code> that compares <code>Numeric</code>s using the passed tolerance.
   */
  def tolerantEquivalence[N : Numeric](tolerance: N): Equivalence[N] = {
    val numeric = implicitly[Numeric[N]]
    if (numeric.lt(tolerance, numeric.zero))
      throw new IllegalArgumentException(tolerance.toString + " passed to tolerantEquivalence was zero or negative. Must be a positive non-zero number.")
    new Equivalence[N] {
      def areEquivalent(a: N, b: N): Boolean = {
        val bPlusTolerance = numeric.plus(b, tolerance)
        val bMinusTolerance = numeric.minus(b, tolerance)
        (numeric.lteq(a, bPlusTolerance)) && (numeric.gteq(a, bMinusTolerance))
      }
      override def toString: String = s"TolerantEquivalence($tolerance)"
    } 
  } 
} 

/**
 * Companion object for <code>TolerantNumerics</code> that enables its members to be imported as an alternative to
 * mixing them in.
 */
object TolerantNumerics extends TolerantNumerics

