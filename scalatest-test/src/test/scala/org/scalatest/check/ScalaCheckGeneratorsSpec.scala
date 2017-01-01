/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalatest.check

import org.scalacheck._
import org.scalatest._
import Arbitrary._
import Prop._
import org.scalatest.Matchers._
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException
import org.scalacheck.util.Pretty
import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest.exceptions.TestFailedException
import org.scalatest.prop.PropertyChecks

class ScalaCheckGeneratorsSpec extends FunSpec with PropertyChecks {

  describe("The ScalaCheckGenerators trait") {
    it("should provide an implicit Generator for a type given an implicit Arbitrary and Shrink for that type") {
      import ScalaCheckGenerators._
      case class Person(name: String, age: Int)
      val personGen: Gen[Person] =
        for {
          name <- implicitly[Arbitrary[String]].arbitrary
          age <- implicitly[Arbitrary[Int]].arbitrary
        } yield Person(name, age)

      implicit val personArb: Arbitrary[Person] = Arbitrary(personGen)
      forAll { (p: Person) =>
        p shouldEqual p
      }
    }
  }
}

