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

class FunctorLaws[Context[_], A, B, C] private (
  implicit functor: Functor[Context],
  genCa: Generator[Context[A]],
  genAb: Generator[A => B],
  genBc: Generator[B => C]
) extends Laws {

  val lawsName = "functor"

  override def laws =
    Vector(
      law("identity") {
        forAll { (ca: Context[A]) =>
          (ca map identity[A]) shouldEqual ca
        }
      },

      law("composition") {
        forAll { (ca: Context[A], ab: A => B, bc: B => C) =>
          ((ca map ab) map bc) shouldEqual (ca map (bc compose ab))
        }
      }
    )
}

object FunctorLaws {
  // type A = Int
  // type B = Short
  // type C = Byte
  def apply[Context[_]](
    implicit functor: Functor[Context],
    genCa: Generator[Context[Int]],
    genAb: Generator[Int => Short],
    genBc: Generator[Short => Byte]
  ): FunctorLaws[Context, Int, Short, Byte] = new FunctorLaws[Context, Int, Short, Byte]
}
