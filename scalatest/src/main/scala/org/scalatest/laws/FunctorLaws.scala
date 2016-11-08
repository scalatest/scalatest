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

import org.scalacheck._
import org.scalactic._
import org.scalactic.algebra._
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks._
import org.scalatest.prop.Generator
import Matchers._

import Functor.adapters

import scala.language.higherKinds

class FunctorLaws[Context[_]] private (
  implicit functor: Functor[Context],
  /*arbCa: Arbitrary[Context[Int]],
  shrCa: Shrink[Context[Int]]*/
  genCa: Generator[Context[Int]],
  genAa: Generator[Int => Int]
) extends Laws {

  val lawsName = "functor"

  override def laws =
    Vector(
      law("identity") {
        forAll { (ca: Context[Int]) =>
          (ca map identity[Int]) shouldEqual ca
        }
      },

      law("composition") {
        forAll { (ca: Context[Int], f: Int => Int, g: Int => Int) =>
          ((ca map f) map g) shouldEqual (ca map (g compose f))
        }
      }
    )
}

object FunctorLaws {
  def apply[Context[_]](
    implicit functor: Functor[Context],
    /*arbCa: Arbitrary[Context[Int]],
    shrCa: Shrink[Context[Int]]*/
    genCa: Generator[Context[Int]],
    genAa: Generator[Int => Int]
  ): FunctorLaws[Context] = new FunctorLaws[Context]
}
