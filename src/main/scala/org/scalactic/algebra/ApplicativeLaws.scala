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
package org.scalactic.algebra

import org.scalacheck.{Arbitrary, Shrink}
import org.scalactic.Equality
import org.scalactic.CheckedEquality._
import org.scalatest.Fact
import org.scalatest.prop.GeneratorDrivenPropertyChecks._

import ApplicativeAdapter.conversions._

import scala.language.higherKinds

class ApplicativeLaws[Context[_]](implicit ap: Applicative[Context],
  arbCa: Arbitrary[Context[Int]],
  shrCa: Shrink[Context[Int]],
  arbAb: Arbitrary[Int => String],
  shrAb: Shrink[Int => String],
  arbBc: Arbitrary[String => Double],
  shrBc: Shrink[String => Double],
  arbCab: Arbitrary[Context[Int => String]],
  shrCab: Shrink[Context[Int => String]],
  arbCbc: Arbitrary[Context[String => Double]],
  shrCbc: Shrink[Context[String => Double]],
  eqCa: Equality[Context[Int]]) extends Laws {

  override val lawsName = "applicative"

  def composition(): Fact = {
    val lawName = "composition"
    forAll { (ca: Context[Int], cab: Context[Int => String], cbc: Context[String => Double]) =>
      // (ca ap cab ap cbc) should be the same as (ca ap (cab ap (cbc map (bc compose ab))))
      if (((ca applying cab) applying cbc) !==
          (ca applying
            (cab applying
              (cbc map ((bc: String => Double) => (ab: Int => String) => bc compose ab)))))
        return Laws.no(lawsName, lawName)
    }
    Laws.yes(lawsName, lawName)
  }

  // ca ap (a => a) should be the same as ca
  def id(): Fact = {
    val lawName = "identity"
    forAll { (ca: Context[Int]) =>
      if ((ca applying ap.insert((a: Int) => a)) !== ca)
        return Laws.no(lawsName, lawName)
    }
    Laws.yes(lawsName, lawName)
  }

  // (insert(a) ap insert(ab)) should be the same as insert(ab(a))
  def homomorphism(): Fact = {
    val lawName = "homomorphism"
    forAll { (a: Int, ab: Int => String) =>
      if ((ap.insert(a) applying ap.insert(ab)) !== ap.insert(ab(a)))
        return Laws.no(lawsName, lawName)
    }
    Laws.yes(lawsName, lawName)
  }

  def interchange(): Fact = {
    val lawName = "interchange"
    forAll { (a: Int, cab: Context[Int => String]) =>
      if ((ap.insert(a) applying cab) !== (cab applying ap.insert((ab: Int => String) => ab(a))))
        return Laws.no(lawsName, lawName)
    }
    Laws.yes(lawsName, lawName)
  }

  override def test() = id() && composition() && homomorphism() && interchange()
}
