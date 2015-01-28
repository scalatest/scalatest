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

import Monad.adapters._

import scala.language.higherKinds

/**
 * Represents the laws that should hold true for an algebraic structure (a Monad) which
 * contains a "flatMap" operation and obeys the laws of associativity, right identity,
 * and left identity.
 */
class MonadLaws[Context[_]](implicit monad: Monad[Context],
  arbCa: Arbitrary[Context[Int]],
  shrCa: Shrink[Context[Int]],
  arbCab: Arbitrary[Int => Context[String]],
  shrCab: Shrink[Int => Context[String]],
  arbCbc: Arbitrary[String => Context[Double]],
  shrCbc: Shrink[String => Context[Double]]) extends Laws("monad") {

  override val laws = Every (

    law("associativity") { () =>
      forAll { (ca: Context[Int], f: Int => Context[String], g: String => Context[Double]) =>
        ((ca flatMap f) flatMap g) shouldEqual (ca flatMap (a => f(a) flatMap g))
      }
    },

    law("left identity") { () =>
      forAll { (ca: Context[Int]) =>
        ca.flatMap(a => monad.insert(a)) shouldEqual ca
      }
    },

    law("right identity") { () =>
      forAll { (a: Int, f: Int => Context[String]) =>
        (monad.insert(a) flatMap f) shouldEqual f(a)
      }
    }
  )
}
