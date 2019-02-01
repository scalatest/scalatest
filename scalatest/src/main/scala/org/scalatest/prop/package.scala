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
package org.scalatest

import org.scalactic.anyvals._

// Documentation outline:
//
// - Discuss the general concept of property-based testing.
// - (Bill) Mention the history (QuickCheck, ScalaCheck) and the rationales for having a dependency-free version here.
// - Describe the concept of a Property, and how it relates to a body of code.
// - Describe what a Generator is, and how it is used to produce sample data.
// - Discuss Randomizer, and how it allows you to create reproducible pseudo-random data.
// - Discuss Configuration, and the switches it lets you throw.
// - Provide several realistic examples along the way, showing how to use all of this. Ideally,
//   these should come from the test suites.
//
// Note that this documentation in the package should be *outline*, not fully detailed, in the interest
// of DRY. It should have heavy pointers to the classes and functions, for further details.
package object prop {
  /**
    * Deterministically generate a value for the given Generator.
    *
    * This function takes a set of anywhere from 1-22 parameters, plus a "multiplier". It combines these to
    * generate a pseudo-random (but deterministic) seed, feeds that into the Generator, and returns the
    * result. Since the results are deterministic, calling this repeatedly with the same parameters will produce
    * the same output.
    *
    * This is mainly helpful when generating random Functions -- since the inputs for a test run are
    * complex, you need more than a simple random seed to reproduce the same results. In order to make
    * this more useful, the `toString` of a instance of a Function [[Generator]] shows how to invoke
    * `valueOf()` to reproduce the same result.
    *
    * @param first The first parameter to use for calculating the seed.
    * @param others Any additional parameters to use for calculating the seed.
    * @param multiplier A number to combine with the other parameters, to calculate the seed.
    * @param genOfA A Generator. (Usually a Function Generator.)
    * @tparam A The type of the Generator.
    * @return An instance of A, computed by feeding the calculated seed into the Generator.
    */
  def valueOf[A](first: Any, others: Any*)(multiplier: Int)(implicit genOfA: Generator[A]): A = {
    val combinedHashCode: Int =
      others.foldLeft(first.hashCode) { (acc, next) =>
        (37 * (acc + 37)) + next.hashCode
      }
    val seed = combinedHashCode.toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfA.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }
}
