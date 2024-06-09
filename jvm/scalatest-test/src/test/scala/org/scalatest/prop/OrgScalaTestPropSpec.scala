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
package org.scalatest.prop

import org.scalactic.anyvals._
import org.scalatest.exceptions.TestFailedException
import scala.annotation.tailrec
import org.scalatest.Resources
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OrgScalaTestPropSpec extends AnyWordSpec with Matchers with GeneratorDrivenPropertyChecks {

  ///////////////////////////////
  //
  // Support types for the test below
  //

  // A fairly arbitrary structure to test with. We don't much care what it is, just that it's
  // a vaguely interesting type that we can write a Generator for.
  case class Person(first: String, last: String, age: Int)
  implicit val personGen: Generator[Person] = for {
    first <- strings
    last <- strings
    age <- ints
  }
    yield Person(first, last, age)

  // Enumeration of types that we might pass into valueOf(). You can make the test below more thorough by
  // adding more types here, but there doesn't seem much reason to get horribly comprehensive about it.
  sealed abstract class TestType[R: Generator] {
    type Result = R
    def gen: Generator[Result] = implicitly[Generator[R]]
  }
  case object TestInt extends TestType[Int]
  case object TestString extends TestType[String]
  case object TestPosFloat extends TestType[PosFloat]
  case object TestStruct extends TestType[Person]

  type AnyTestType = TestType[_]

  implicit val typeGen: Generator[AnyTestType] = specificValues(TestInt, TestString, TestPosFloat, TestStruct)

  /**
    * Generates a single random value to use as a valueOf() parameter.
    *
    * @param tpe The TestType to generate.
    * @param rnd The current Randomizer.
    * @return A random value of the specified type.
    */
  private def mkVal(tpe: AnyTestType, rnd: Randomizer): (Any, List[Any], Randomizer) = {
    val (size, rnd2) = rnd.choosePosZInt(1, 100)
    val sizeParam = SizeParam(size, 0, size)
    tpe match {
      case TestInt => ints.next(sizeParam, List.empty, rnd2)
      case TestString => strings.next(sizeParam, List.empty, rnd2)
      case TestPosFloat => posFloats.next(sizeParam, List.empty, rnd2)
      case TestStruct => personGen.next(sizeParam, List.empty, rnd2)
    }
  }

  "valueOf" should {
    // This is a pretty hardcore stress test of both the valueOf function and the
    // property check system itself. It chooses a random output type for valueOf,
    // based on a lot of randomly-chosen parameters. It feeds all that into
    // valueOf() twice, and checks that it gets the same result both times.
    //
    // Parameters:
    //   - genTpe: the Type to output from valueOf
    //   - nValues: the number of parameters to pass into valueOf
    //   - multiplier: the multiplier to pass into valueOf
    //   - valueSeed: the seed to use for randomization inside this property check
    "work consistently with a wide variety of inputs" in {
      forAll(typeGen, posIntsBetween(1, 22), ints, longs) { (genTpe: AnyTestType, nValues: PosInt, multiplier: Int, valueSeed: Long) =>
        // Needed to make some of the AnyTestType stuff below work without warnings:
        import scala.language.existentials

        // The initial Randomizer that we will use to generate data internally:
        val vRand = Randomizer(valueSeed)

        // Generate nValues' worth of random *types*, for the valueOf parameters:
        @tailrec
        def chooseTypes(remaining: Int, tpes: List[AnyTestType], rnd: Randomizer): (List[AnyTestType], Randomizer) = {
          if (remaining <= 0)
            (tpes, rnd)
          else {
            val (tpe, _, rnd2) = typeGen.next(SizeParam(1, 0, 1), List.empty, rnd)
            chooseTypes(remaining - 1, tpe.value :: tpes, rnd2)
          }
        }
        val (tpes, rnd2) = chooseTypes(nValues, List.empty, vRand)

        // Now create a value for each of those types:
        val (vs, rnd3) = tpes.foldLeft((List.empty[Any], rnd2)) { case ((cur, rnd), tpe) =>
          val (v, _, nextRnd) = mkVal(tpe, rnd)
          (v :: cur, nextRnd)
        }

        // Now invoke valueOf() with all that, and confirm that we get the same value each time:
        def callValueOf(): genTpe.Result = {
          valueOf[genTpe.Result](vs.head, vs.tail:_*)(multiplier)(genTpe.gen)
        }
        val first = callValueOf()
        val second = callValueOf()

        first should be (second)
      }
    }
  }
}
