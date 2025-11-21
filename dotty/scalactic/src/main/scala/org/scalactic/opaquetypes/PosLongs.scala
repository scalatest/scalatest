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

    /**
      * A factory/validation method that produces a <code>PosZLong</code>, wrapped
      * in a <code>Success</code>, given a valid <code>Long</code> value, or if the
      * given <code>Long</code> is invalid, an <code>AssertionError</code>, wrapped
      * in a <code>Failure</code>.
      *
      * <p>
      * This method will inspect the passed <code>Long</code> value and if
      * it is a PosZLong <code>Long</code>, it will return a <code>PosZLong</code>
      * representing that value, wrapped in a <code>Success</code>.
      * Otherwise, if the passed <code>Long</code> value is not PosZLong, this
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
      * @param value the <code>Int</code> to inspect, and if a non-negative integer, return
      *     wrapped in a <code>Success(PosZLong)</code>.
      * @return the specified <code>Long</code> value wrapped
      *     in a <code>Success(PosZLong)</code>, if it is a non-negative integer, else a <code>Failure(AssertionError)</code>.
      */
    def tryingValid(value: Long): Try[PosZLong] =
      if (value >= 0L)
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidPosZLong))  

  }

}
