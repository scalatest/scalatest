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

class FunctorLaws[Context[_], A, B, C] protected (
  implicit functor: Functor[Context],
  genCtxOfA: Generator[Context[A]],
  genAToB: Generator[A => B],
  genBToC: Generator[B => C]
) extends Laws {

  val lawsName = "functor"

  override def laws =
    Vector(
      law("identity") {
        forAll { (ctxOfA: Context[A]) =>
          ctxOfA.map(identity[A]) shouldEqual ctxOfA
        }
      },

      law("composition") {
        forAll { (ctxOfA: Context[A], aToB: A => B, bToC: B => C) =>
          ctxOfA.map(aToB).map(bToC) shouldEqual ctxOfA.map(bToC.compose(aToB))
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
    genCtxOfA: Generator[Context[Int]],
    genAToB: Generator[Int => Short],
    genBToC: Generator[Short => Byte]
  ): FunctorLaws[Context, Int, Short, Byte] = new FunctorLaws[Context, Int, Short, Byte]

  def using[Context[_], A, B, C](
    implicit functor: Functor[Context],
    genCtxOfA: Generator[Context[A]],
    genAToB: Generator[A => B],
    genBToC: Generator[B => C]
  ): FunctorLaws[Context, A, B, C] = new FunctorLaws[Context, A, B, C]
}
