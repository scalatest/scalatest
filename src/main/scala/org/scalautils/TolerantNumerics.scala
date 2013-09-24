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
trait TolerantNumerics {

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
    } 
  } 
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
    } 
  } 
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
    } 
  } 
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
    } 
  } 
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
    } 
  } 
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
    } 
  } 
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
    } 
  } 
} 

/**
 * Companion object for <code>TolerantNumerics</code> that enables its members to be imported as an alternative to
 * mixing them in.
 */
object TolerantNumerics extends TolerantNumerics

