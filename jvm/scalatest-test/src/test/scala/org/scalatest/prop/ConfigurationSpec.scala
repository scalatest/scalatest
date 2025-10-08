/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalatest.prop

import org.scalactic.anyvals._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfigurationSpec extends AnyFunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("Configuration.Parameter") {
    implicit val configGen: Generator[Configuration.Parameter] =
      for {
        minSuccessful <- posInts
        maxDiscardedFactor <- posZDoubles
        minSize <- posZInts
        sizeRange <- posZIntsBetween(0, PosZInt.ensuringValid(PosZInt.MaxValue - minSize))
        workers <- posInts
        intialSeed <- Generator.longGenerator.map(Seed(_))
      } yield Configuration.Parameter(minSuccessful, maxDiscardedFactor, minSize, sizeRange, workers, intialSeed)

    it("should offer a maxSize method that is minSize + SizeRange") {
      forAll { (param: Configuration.Parameter) =>
        param.maxSize.value shouldEqual (param.minSize + param.sizeRange)
      }
    }

    it("should throw IllegalArgumentException when the result of minSize + sizeRange goes out of range") {
      assertThrows[IllegalArgumentException] {
        Configuration.Parameter(PosInt(5), PosZDouble(0.5), PosZInt.MaxValue, PosZInt(1), PosInt(1), Seed(2L))
      }
    }
  }
}
