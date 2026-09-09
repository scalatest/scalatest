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
package org.scalactic

import org.scalatest._

class TypeCheckedTripleEqualsSpec extends funspec.AnyFunSpec with NonImplicitAssertions {

  case class Super(size: Int)
  class Sub(sz: Int) extends Super(sz)

  describe("TypeCheckedTripleEquals") {

    it("should construct an Equalizer") {

      new TypeCheckedTripleEquals {

        val e = convertToEqualizer[Int](1)
        assert(e.isInstanceOf[Equalizer[Int]])
      }
    }

    it("should construct a CheckingEqualizer") {

      new TypeCheckedTripleEquals {

        val ce = convertToCheckingEqualizer[Int](1)
        assert(ce.isInstanceOf[CheckingEqualizer[Int]])
      }
    }

    it("should construct the type-checked constraints") {

      new TypeCheckedTripleEquals {

        val eqSuper: Equivalence[Super] = Equivalence.default[Super]
        val subToSuper: Sub <:< Super = implicitly[Sub <:< Super]

        assert(typeCheckedConstraint[Super, Sub](eqSuper, subToSuper).isInstanceOf[TripleEqualsSupport.BToAEquivalenceConstraint[Super, Sub]])
        assert(convertEquivalenceToBToAConstraint[Super, Sub](eqSuper)(subToSuper).isInstanceOf[TripleEqualsSupport.BToAEquivalenceConstraint[Super, Sub]])
      }
    }

    it("should construct an unconstrained Equality constraint") {

      new TypeCheckedTripleEquals {

        implicit val eqInt: Equality[Int] = Equality.default[Int]
        val c = unconstrainedEquality[Int, Int]
        assert(c.isInstanceOf[TripleEqualsSupport.EqualityConstraint[Int, Int]])
      }
    }

    it("should construct the deprecated conversion constraints") {

      import scala.annotation.nowarn

      new TypeCheckedTripleEquals {

        val eqSuper: Equivalence[Super] = Equivalence.default[Super]
        val subToSuperFn: Sub => Super = (s: Sub) => s: Super

        val a = lowPriorityConversionCheckedConstraint[Sub, Super](eqSuper, subToSuperFn)
        val b = convertEquivalenceToAToBConversionConstraint[Sub, Super](eqSuper)(subToSuperFn)
        val c = conversionCheckedConstraint[Super, Sub](eqSuper, subToSuperFn)
        val d = convertEquivalenceToBToAConversionConstraint[Super, Sub](eqSuper)(subToSuperFn)

        assert(a != null)
        assert(b != null)
        assert(c != null)
        assert(d != null)
      }
    }
  }
}
