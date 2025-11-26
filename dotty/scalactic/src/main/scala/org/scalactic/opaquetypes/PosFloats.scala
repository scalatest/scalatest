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

object PosFloats {

  opaque type PosZFloat = Float

  object PosZFloat {

    /** Compile-time factory for creating a [[PosZFloat]] from a float literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects negative literals. Use it as: `PosZFloat(5.0f)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam F the singleton Float literal type
      * @param i the Float literal
      * @return a [[PosZFloat]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[F <: Float & Singleton](inline f: F): PosZFloat =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v < 0 then
            error("PosZFloat cannot be instantiated with a negative float literal")
          else
            v.asInstanceOf[PosZFloat]
        case None =>
          error("PosZFloat.apply requires a float literal")
      }
    
    /** Construct a [[PosZFloat]] from a runtime Float if it is non-negative.
      *
      * @param f runtime Float to validate
      * @return Some(PosZFloat) if f >= 0, otherwise None
      */
    def from(f: Float): Option[PosZFloat] =
      if (f >= 0.0f) Some(f) else None

    /** Ensure the runtime Float is non-negative and return it as a [[PosZFloat]].
      *
      * @param f runtime Float to check
      * @return the given float as a [[PosZFloat]] if valid
      * @throws AssertionError if the given Float is negative
      */
    def ensuringValid(f: Float): PosZFloat = 
      if (f < 0.0f) 
        throw new AssertionError(Resources.invalidPosZFloat)
      else f

    extension (p: PosZFloat) {
      /** Return the underlying Float value. */
      def value: Float = p
    }  

  }

  opaque type PosFloat = Float

  object PosFloat {
    
    def from(f: Float): Option[PosFloat] =
      if (f >= 0.0f) Some(f) else None

    def ensuringValid(f: Float): PosFloat = 
      if (f < 0.0f) 
        throw new AssertionError(Resources.invalidPosFloat)
      else f

  } 

}
