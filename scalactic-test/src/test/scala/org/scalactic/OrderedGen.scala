/*
 * Copyright 2001-2015 Artima, Inc.
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

import org.scalacheck.{Arbitrary, Gen}
import Gen._

/**
 * An assortment of generators that can efficiently produce ordered pairs of various
 * numeric types, including or excluding equality.
 *
 * Generators with a name not ending in 'Eq' will produce ordered pairs (a, b) such that a < b,
 * while those with an 'Eq' suffix will produce pairs (a, b) such that a <= b.
 */
object OrderedGen {
  
  def orderedPair[T](allowEqual: Boolean)(implicit arbTuple: Arbitrary[(T, T)], order: Ordering[T]): Gen[(T, T)] = {
    val arb = if (allowEqual)
      arbTuple.arbitrary
    else
      arbTuple.arbitrary.filter { pair => !order.equiv(pair._1, pair._2) }
    arb.map { case (a, b) => if ( order.compare(a, b) < 0 ) (a, b) else (b, a) }
  }

  val orderedLongs          = orderedPair[Long](allowEqual = false)
  val orderedOrEqualLongs   = orderedPair[Long](allowEqual = true)

  val orderedInts           = orderedPair[Int](allowEqual = false)
  val orderedOrEqualInts    = orderedPair[Int](allowEqual = true)

  val orderedChars          = orderedPair[Char](allowEqual = false)
  val orderedOrEqualChars   = orderedPair[Char](allowEqual = true)

  val orderedBytes          = orderedPair[Byte](allowEqual = false)
  val orderedOrEqualBytes   = orderedPair[Byte](allowEqual = true)

  val orderedDoubles        = orderedPair[Double](allowEqual = false)
  val orderedOrEqualDoubles = orderedPair[Double](allowEqual = true)

  val orderedFloats         = orderedPair[Float](allowEqual = false)
  val orderedOrEqualFloats  = orderedPair[Float](allowEqual = true)
}
