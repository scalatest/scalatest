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

import Applicatives._
import ApplicativeAdapter.conversions._
import org.scalacheck.Arbitrary

import org.scalactic._

import scala.language.implicitConversions

class ApplicativeSpec extends UnitSpec {

  "Option" should "obey the applicative laws" in {
    new ApplicativeLaws[Option]().assert()
  }

  "List" should "obey the applicative laws" in {
    new ApplicativeLaws[List]().assert()
  }

  "The good nature of Or" should "obey the applicative laws" in {
    // generator used for verifying the Good nature of Or
    implicit def orArbGood[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] = Arbitrary(for (g <- Arbitrary.arbitrary[G]) yield Good(g))

    new ApplicativeLaws[OrWithBad[Int]#AndGood].assert()
  }

}
