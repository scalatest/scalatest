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

import EqualityPolicy.Spread

/**
 * Trait containing an implicit conversion that adds a <code>+-</code> method to <code>Numeric</code> types, which enables
 * spreads to be expressed in terms of a <em>pivot</em> and <em>tolerance</em>.
 *
 * <p>
 * For example, the <code>TripleEquals</code> trait (and its type-checking siblings <code>TypeCheckedTripleEquals</code> and
 * <code>ConversionCheckedTripleEquals</code>) enable you to write:
 * </p>
 *
 * <pre class="stHighlight">
 * a === (1.0 +- 0.1)
 * </pre>
 *
 * @author Bill Venners
 */
trait Tolerance {

  /**
   * Wrapper class with a <code>+-</code> method that, given a <code>Numeric</code> argument, returns a <code>Spread</code>.
   * 
   * @param tolerance the tolerance with which to create (and return) a <code>Spread</code>
   *
   * @author Bill Venners
   */
  final class PlusOrMinusWrapper[T: Numeric](pivot: T) {

    /**
     * Creates and returns a <code>Spread<code> from the <code>pivot</code> passed to the constructor and
     * the <code>tolerance</code> passed to this method.
     *
     * @param tolerance the tolerance with which to create (and return) the <code>Spread</code>
     */
    def +-(tolerance: T): Spread[T] = {
      val numeric = implicitly[Numeric[T]]
      if (numeric.lteq(tolerance, numeric.zero))
        throw new IllegalArgumentException(tolerance.toString + " passed to +- was zero or negative. Must be a positive non-zero number.")
        // throw newTestFailedException(Resources("negativeOrZeroRange", tolerance.toString))
      Spread(pivot, tolerance)
    }
  }

  import scala.language.implicitConversions

  /**
   * Implicitly converts an object of a <code>Numeric</code> type to a <code>PlusOrMinusWrapper</code>,
   * to enable a <code>+-</code> method to be invoked on that object.
   */
  implicit def convertNumericToPlusOrMinusWrapper[T : Numeric](pivot: T): PlusOrMinusWrapper[T] = new PlusOrMinusWrapper(pivot)
}

/**
 * Companion object to trait <code>Tolerance</code> that facilitates the importing of <code>Tolerance</code> members as 
 * an alternative to mixing it in. One use case is to import <code>Tolerance</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $ scala -classpath scalactic.jar
 * Welcome to Scala version 2.10.0
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import Tolerance._
 * import Tolerance._
 *
 * scala&gt; 1.0 +- 0.1
 * res0: org.scalactic.EqualityPolicy.Spread[Double] = Spread(1.0,0.1)
 * </pre>
 */
object Tolerance extends Tolerance

