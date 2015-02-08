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

import Applicative.adapters

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
  shrCbc: Shrink[Context[String => Double]]) extends Laws("applicative") {

/*
  "applicative" laws {
    "composition" law {
      forAll { (ca: Context[Int], cf: Context[Int => String], cg: Context[String => Double]) =>
        ((ca applying cf) applying cg) shouldEqual
          (ca applying (cf applying (cg map ( (g: String => Double) => (f: Int => String) => g compose f))))
      }
    }
    "identity" law {
    }
  }
  }
*/

  override val laws = Every(
    law("composition") { () =>
      forAll { (ca: Context[Int], cf: Context[Int => String], cg: Context[String => Double]) =>
        ((ca applying cf) applying cg) shouldEqual
          (ca applying (cf applying (cg map ( (g: String => Double) => (f: Int => String) => g compose f))))
      }
    },

    // ca ap (a => a) should be the same as ca
    law("identity") { () =>
      forAll { (ca: Context[Int]) =>
        (ca applying ap.insert((a: Int) => a)) shouldEqual ca
      }
    },

    // (insert(a) ap insert(ab)) should be the same as insert(ab(a))
    law("homomorphism") { () =>
      forAll { (a: Int, f: Int => String) =>
        (ap.insert(a) applying ap.insert(f)) shouldEqual ap.insert(f(a))
      }
    },

    law("interchange") { () =>
      forAll { (a: Int, cf: Context[Int => String]) =>
        (ap.insert(a) applying cf) shouldEqual (cf applying ap.insert((f: Int => String) => f(a)))
      }
    }
  )
}
