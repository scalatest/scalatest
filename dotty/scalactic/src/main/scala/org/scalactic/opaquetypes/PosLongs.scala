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

object PosLongs {

  opaque type PosZLong = Long

  object PosZLong {
    
    /** Compile-time factory for creating a [[PosZLong]] from an integer literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects negative literals. Use it as: `PosZLong(5)`. For non-literal
      * values, use [[ensuringValid]] or [[from]].
      *
      * @tparam I the singleton Int literal type
      * @param l the Long literal
      * @return a [[PosZLong]] representing the given non-negative literal
      * @throws a compile-time error if the literal is negative or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): PosZLong =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v < 0L then
            error("PosZLong cannot be instantiated with a negative integer literal")
          else
            v.asInstanceOf[PosZLong]
        case None =>
          error("PosZLong.apply requires an long literal")
      }

    def from(l: Long): Option[PosZLong] =
      if (l >= 0L) Some(l) else None

    def ensuringValid(l: Long): PosZLong = 
      if (l < 0L) 
        throw new AssertionError(Resources.invalidPosZLong)
      else l  

  }

}
