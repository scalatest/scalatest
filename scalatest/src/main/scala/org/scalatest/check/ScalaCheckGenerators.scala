/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalatest.check

import org.scalatest.prop.{Generator, Randomizer, SizeParam}

trait ScalaCheckGenerators {

  import org.scalacheck.{Arbitrary, Gen, Shrink}
  import org.scalacheck.rng.Seed

  implicit def scalaCheckArbitaryGenerator[T](implicit arb: Arbitrary[T], shrk: Shrink[T]): Generator[T] =
    new Generator[T] {
      def next(szp: SizeParam, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            arb.arbitrary.apply(Gen.Parameters.default.withSize(szp.size), Seed(rnd.seed)) match {
              case Some(nextT) => (nextT, Nil, rnd.nextRandomizer)
              case None => throw new IllegalStateException("Unable to generate value using ScalaCheck Arbitary.")
            }
        }
      }
      override def shrink(value: T, rnd: Randomizer): (Iterator[T], Randomizer) = {
        (shrk.shrink(value).take(10000).reverse.toIterator, rnd)
      }
    }
}

object ScalaCheckGenerators extends ScalaCheckGenerators 
