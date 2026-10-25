/*
 * Copyright 2001-2026 Artima, Inc.
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
      } yield Configuration.Parameter(minSuccessful, maxDiscardedFactor, minSize, sizeRange, workers)

    it("should offer a maxSize method that is minSize + SizeRange") {
      forAll { (param: Configuration.Parameter) =>
        param.maxSize.value shouldEqual (param.minSize + param.sizeRange)
      }
    }

    it("should throw IllegalArgumentException when the result of minSize + sizeRange goes out of range") {
      assertThrows[IllegalArgumentException] {
        Configuration.Parameter(PosInt(5), PosZDouble(0.5), PosZInt.MaxValue, PosZInt(1), PosInt(1))
      }
    }

    it("should accept initSeed") {
      val param = Configuration.Parameter(PosInt(10), PosZDouble(5.0), PosZInt(0), PosZInt(100), PosInt(1), Some(123L))
      param.initSeed should be (Some(123L))
    }

  describe("Configuration.getParameter") {
    it("should accept Seed config param") {
      val param = Configuration.getParameter(Seq(Configuration.Seed(42L)), Configuration.PropertyCheckConfiguration())
      param.initSeed should be (Some(42L))
    }

    it("should accept initSeed from PropertyCheckConfiguration") {
      val param = Configuration.getParameter(Seq(), Configuration.PropertyCheckConfiguration(initSeed = Some(123L)))
      param.initSeed should be (Some(123L))
    }

    it("should give precedence to config param over configuration") {
      val param = Configuration.getParameter(Seq(Configuration.Seed(99L)), Configuration.PropertyCheckConfiguration(initSeed = Some(123L)))
      param.initSeed should be (Some(99L))
    }

    it("should throw if more than one Seed passed") {
      intercept[IllegalArgumentException] {
        Configuration.getParameter(Seq(Configuration.Seed(1L), Configuration.Seed(2L)), Configuration.PropertyCheckConfiguration())
      }
    }
  }
}
}
