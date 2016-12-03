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

import org.scalacheck.{Arbitrary, Shrink}
import org.scalactic._
import org.scalactic.algebra._
import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks._
import org.scalatest.prop.Generator

import Applicative.adapters

import scala.language.higherKinds

class ApplicativeLaws[Context[_], A, B, C] private (
  implicit ap: Applicative[Context],
  genA: Generator[A],
  genCa: Generator[Context[A]],
  genAb: Generator[A => B],
  genBc: Generator[B => C],
  genCab: Generator[Context[A => B]],
  genCbc: Generator[Context[B => C]]
) extends Laws {

  val lawsName = "applicative"

  override def laws =
    Vector(
      law("composition") {
        forAll { (ca: Context[A], cab: Context[A => B], cbc: Context[B => C]) =>
          ((ca applying cab) applying cbc) shouldEqual
            (ca applying (cab applying (cbc map ( (g: B => C) => (f: A => B) => g compose f))))
        }
      },

      // ca ap (a => a) should be the same as ca
      law("identity") {
        forAll { (ca: Context[A]) =>
          (ca applying ap.insert((a: A) => a)) shouldEqual ca
        }
      },

      // (insert(a) ap insert(ab)) should be the same as insert(ab(a))
      law("homomorphism") {
        forAll { (a: A, ab: A => B) =>
          (ap.insert(a) applying ap.insert(ab)) shouldEqual ap.insert(ab(a))
        }
      },

      law("interchange") {
        forAll { (a: A, cab: Context[A => B]) =>
          (ap.insert(a) applying cab) shouldEqual (cab applying ap.insert((f: A => B) => f(a)))
        }
      }
    )
}

object ApplicativeLaws {
  // type A = Int
  // type B = Short
  // type C = Byte
  def apply[Context[_]](
    implicit ap: Applicative[Context],
    genA: Generator[Int],
    genCa: Generator[Context[Int]],
    genAb: Generator[Int => Short],
    genBc: Generator[Short => Byte],
    genCab: Generator[Context[Int => Short]],
    genCbc: Generator[Context[Short => Byte]]
  ): ApplicativeLaws[Context, Int, Short, Byte] = new ApplicativeLaws[Context, Int, Short, Byte]

  // Can offer a withTypes[List, Float, Double, String], etc., kind of factory method
}

