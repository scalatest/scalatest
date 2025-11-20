/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalactic.opaquetypes

import org.scalactic.Resources
import scala.compiletime.{ constValueOpt, error }
import scala.util.{Try, Success, Failure}

/** Opaque type representing positive (greater than zero) Int values.
  *
  * Instances of this type are guaranteed to be > 0. Use the factory and
  * validation methods in the companion object to create or validate values.
  */
opaque type PosInt = Int

/** Companion object for the [[PosInt]] opaque type.
  *
  * Provides factory and validation methods, given conversions, extension
  * methods, and useful constants (e.g. MaxValue, MinValue). Prefer the
  * inline [[apply]] overload for compile-time checked construction from
  * integer literals; use [[ensuringValid]], [[from]], or other helpers for
  * runtime values.
  */
object PosInt {

  /** Compile-time factory for creating a [[PosInt]] from an integer literal.
    *
    * This inline method inspects the provided integer literal at compile time
    * and rejects non-positive literals. Use it as: `PosInt(5)`. For non-literal
    * values, use [[ensuringValid]] or [[from]].
    *
    * @tparam I the singleton Int literal type
    * @param i the Int literal
    * @return a [[PosInt]] representing the given positive literal
    * @throws a compile-time error if the literal is negative, zero or not a literal
    */
  inline def apply[I <: Int & Singleton](inline i: I): PosInt =
    inline constValueOpt[I] match {
      case Some(v: Int) =>
        inline if v <= 0 then
          error("PosInt cannot be instantiated with a negative or zero integer literal")
        else
          v.asInstanceOf[PosInt]
      case None =>
        error("PosInt.apply requires an integer literal")
    }

  /** Create a [[PosInt]] if the given Int is valid.
    *
    * @param i the Int to inspect
    * @return Some([[PosInt]]) if the given Int is greater than 0, else None
    */
  def from(i: Int): Option[PosInt] =
    if (i > 0) Some(i) else None

  /** Create a [[PosInt]], throwing an AssertionError if the given Int is invalid.
    *
    * @param i the Int to inspect
    * @return the [[PosInt]] if the given Int is greater than 0
    * @throws AssertionError if the given Int is less than or equal to 0
    */
  def ensuringValid(i: Int): PosInt =
    if (i <= 0)
      throw new AssertionError(Resources.invalidPosInt)
    else i

  /**
   * A factory/validation method that produces a <code>PosInt</code>, wrapped
   * in a <code>Success</code>, given a valid <code>Int</code> value, or if the
   * given <code>Int</code> is invalid, an <code>AssertionError</code>, wrapped
   * in a <code>Failure</code>.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a PosInt <code>Int</code>, it will return a <code>PosInt</code>
   * representing that value, wrapped in a <code>Success</code>.
   * Otherwise, if the passed <code>Int</code> value is not PosInt, this
   * method will return an <code>AssertionError</code>, wrapped in a <code>Failure</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas this method inspects
   * <code>Int</code> values at run time.
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if a positive integer, return
   *     wrapped in a <code>Success(PosInt)</code>.
   * @return the specified <code>Int</code> value wrapped
   *     in a <code>Success(PosInt)</code>, if it is a positive integer, else a <code>Failure(AssertionError)</code>.
   */
   def tryingValid(value: Int): Try[PosInt] =
     if (value > 0)
       Success(value)
     else
       Failure(new AssertionError(Resources.invalidPosInt))  

}