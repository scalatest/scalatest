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

object PosDoubles {

  opaque type PosZDouble = Double

  object PosZDouble {

    /** Compile-time factory for creating a [[PosZDouble]] from a float literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects negative literals. Use it as: `PosZDouble(5.0)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton Float literal type
      * @param f the Float literal
      * @return a [[PosZDouble]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[D <: Double & Singleton](inline d: D): PosZDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v < 0.0 then
            error("PosZDouble cannot be instantiated with a negative double literal")
          else
            v.asInstanceOf[PosZDouble]
        case None =>
          error("PosZDouble.apply requires a integer, long or double literal")
      }

    /** 
      * Return true when the provided Double is a valid [[PosZDouble]] value (>= 0). 
      *
      * @param value the Double to validate
      * @return true if the specified Double is a non-negative double, else false
      */
    def isValid(value: Double): Boolean = value >= 0.0  

    /**
      * A factory/validation method that produces a <code>PosZDouble</code>, wrapped
      * in a <code>Success</code>, given a valid <code>Double</code> value, or if the
      * given <code>Double</code> is invalid, an <code>AssertionError</code>, wrapped
      * in a <code>Failure</code>.
      *
      * <p>
      * This method will inspect the passed <code>Double</code> value and if
      * it is a PosZDouble <code>Double</code>, it will return a <code>PosZDouble</code>
      * representing that value, wrapped in a <code>Success</code>.
      * Otherwise, if the passed <code>Double</code> value is not PosZDouble, this
      * method will return an <code>AssertionError</code>, wrapped in a <code>Failure</code>.
      * </p>
      *
      * <p>
      * This factory method differs from the <code>apply</code> factory method
      * in that <code>apply</code> is implemented via a macro that inspects
      * <code>Float</code> literals at compile time, whereas this method inspects
      * <code>Float</code> values at run time.
      * </p>
      *
      * @param value the <code>Double</code> to inspect, and if a non-negative double, return
      *     wrapped in a <code>Success(PosZDouble)</code>.
      * @return the specified <code>Double</code> value wrapped
      *     in a <code>Success(PosZDouble)</code>, if it is a non-negative double, else a <code>Failure(AssertionError)</code>.
      */
    def tryingValid(value: Double): Try[PosZDouble] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosZDouble))
    
    def from(d: Double): Option[PosZDouble] =
      if (d >= 0.0) Some(d) else None

    def ensuringValid(d: Double): PosZDouble = 
      if (d < 0.0) 
        throw new AssertionError(Resources.invalidPosZDouble)
      else d

  }

  opaque type PosDouble = Double

  object PosDouble {
    
    def from(d: Double): Option[PosDouble] =
      if (d > 0.0) Some(d) else None

    def ensuringValid(d: Double): PosDouble = 
      if (d <= 0.0) 
        throw new AssertionError(Resources.invalidPosDouble)
      else d

  } 

}

