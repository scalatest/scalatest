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

