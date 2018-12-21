/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalatest.laws

import org.scalactic._
import org.scalactic.algebra._
import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks._
import org.scalatest.prop.Generator

import Applicative.adapters

import scala.language.higherKinds

class ApplicativeLaws[Context[_], A, B, C] protected (
  implicit ap: Applicative[Context],
  genA: Generator[A],
  genCtxOfA: Generator[Context[A]],
  genAToB: Generator[A => B],
  genBToC: Generator[B => C],
  genCtxOfAToB: Generator[Context[A => B]],
  genCtxOfBToC: Generator[Context[B => C]]
) extends FunctorLaws[Context, A, B, C] {

  override val lawsName = "applicative"

  override def laws =

    Vector(
      law("composition") {
        forAll { (ctxOfA: Context[A], ctxOfAToB: Context[A => B], ctxOfBToC: Context[B => C]) =>
          ctxOfA.applying(ctxOfAToB).applying(ctxOfBToC) shouldEqual
            ctxOfA.applying(ctxOfAToB.applying(ctxOfBToC.map((bToC: B => C) => (aToB: A => B) => bToC.compose(aToB))))
        }
      },

      // ctxOfA ap (a => a) should be the same as ctxOfA
      law("identity") {
        forAll { (ctxOfA: Context[A]) =>
          ctxOfA.applying(ap.insert((a: A) => a)) shouldEqual ctxOfA
        }
      },

      // (insert(a) ap insert(aToB)) should be the same as insert(aToB(a))
      law("homomorphism") {
        forAll { (a: A, aToB: A => B) =>
          ap.insert(a).applying(ap.insert(aToB)) shouldEqual ap.insert(aToB(a))
        }
      },

      law("interchange") {
        forAll { (a: A, ctxOfAToB: Context[A => B]) =>
          ap.insert(a).applying(ctxOfAToB) shouldEqual ctxOfAToB.applying(ap.insert((aToB: A => B) => aToB(a)))
        }
      }
    ) ++ super.laws
}
// Can add a usingTypes[A, B, C] to ApplicativeLows instance, so you can say
// it should obey the ApplicativeLaws[List].usingTypes[Int, Short, Long]

object ApplicativeLaws {
  // types A, B, and C = Int; in parametricity we trust
  def apply[Context[_]](
    implicit ap: Applicative[Context],
    genInt: Generator[Int],
    genCtxOfInt: Generator[Context[Int]],
    genIntToInt: Generator[Int => Int],
    genCtxOfIntToInt: Generator[Context[Int => Int]]
  ): ApplicativeLaws[Context, Int, Int, Int] = new ApplicativeLaws[Context, Int, Int, Int]

  // Can offer a usingTypes[List, Float, Double, String], etc., kind of factory method
  // List should obey the FunctorLaws.usingTypes[List, String, Long, List[Int]]
}

