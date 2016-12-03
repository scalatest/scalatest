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
class MonadLaws[Context[_], A, B, C] private (
  implicit monad: Monad[Context],
  genA: Generator[A],
  genCa: Generator[Context[A]],
  genAcb: Generator[A => Context[B]],
  genBcc: Generator[B => Context[C]]
) extends Laws {

  val lawsName = "monad"

  override def laws =
    Vector(
      law("associativity") {
        forAll { (ca: Context[A], acb: A => Context[B], bcc: B => Context[C]) =>
          ((ca flatMap acb) flatMap bcc) shouldEqual (ca flatMap (a => acb(a) flatMap bcc))
        }
      },

      law("left identity") {
        forAll { (ca: Context[A]) =>
          ca.flatMap(a => monad.insert(a)) shouldEqual ca
        }
      },

      law("right identity") {
        forAll { (a: A, acb: A => Context[B]) =>
          (monad.insert(a) flatMap acb) shouldEqual acb(a)
        }
      }
    )
}

object MonadLaws {
  def apply[Context[_]](
    implicit monad: Monad[Context],
    genA: Generator[Long],
    genCa: Generator[Context[Long]],
    genCab: Generator[Long => Context[Int]],
    genCbc: Generator[Int => Context[Short]]
  ): MonadLaws[Context, Long, Int, Short] = new MonadLaws[Context, Long, Int, Short]
}

