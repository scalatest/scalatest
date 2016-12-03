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
import org.scalactic.Every
import org.scalactic.algebra._
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks._
import org.scalatest.prop.Generator
import org.scalactic.source

import Monad.adapters

import scala.language.higherKinds

/**
 * Represents the laws that should hold true for an algebraic structure (a Monad) which
 * contains a "flatMap" operation and obeys the laws of associativity, right identity,
 * and left identity.
 */
class MonadLaws[Context[_], A, B, C] protected (
  implicit monad: Monad[Context],
  genA: Generator[A],
  genCtxOfA: Generator[Context[A]],
  genAcb: Generator[A => Context[B]],
  genBToCc: Generator[B => Context[C]],
  genAToB: Generator[A => B],
  genBToC: Generator[B => C],
  genCtxOfAToB: Generator[Context[A => B]],
  genCtxOfBToC: Generator[Context[B => C]]
) extends ApplicativeLaws[Context, A, B, C] {

  override val lawsName = "monad"

  override def laws =
    Vector(
      law("associativity") {
        forAll { (ctxOfA: Context[A], aToCtxOfB: A => Context[B], bToCtxOfC: B => Context[C]) =>
          ((ctxOfA flatMap aToCtxOfB) flatMap bToCtxOfC) shouldEqual (ctxOfA flatMap (a => aToCtxOfB(a) flatMap bToCtxOfC))
        }
      },

      law("left identity") {
        forAll { (ctxOfA: Context[A]) =>
          ctxOfA.flatMap(a => monad.insert(a)) shouldEqual ctxOfA
        }
      },

      law("right identity") {
        forAll { (a: A, aToCtxOfB: A => Context[B]) =>
          (monad.insert(a) flatMap aToCtxOfB) shouldEqual aToCtxOfB(a)
        }
      }
    ) ++ super.laws
}

object MonadLaws {
  def apply[Context[_]](
    implicit monad: Monad[Context],
    genA: Generator[Int],
    genCtxOfA: Generator[Context[Int]],
    genAcb: Generator[Int => Context[Short]],
    genBToCc: Generator[Short => Context[Byte]],
    genAToB: Generator[Int => Short],
    genBToC: Generator[Short => Byte],
    genCtxOfAToB: Generator[Context[Int => Short]],
    genCtxOfBToC: Generator[Context[Short => Byte]]
  ): MonadLaws[Context, Int, Short, Byte] = new MonadLaws[Context, Int, Short, Byte]
}

