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
package org.scalatest.diagrams

import org.scalactic.source.Position
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DiagramsEnhancementSpec extends AnyFunSpec with Matchers with Diagrams {

  type IntToInt = Int => Int
  type AnnotatedIntToInt = (Int => Int) @scala.annotation.unchecked.uncheckedVariance

  def alwaysFalseAlias(f: IntToInt): Boolean = false
  def alwaysFalseAnnotatedAlias(f: AnnotatedIntToInt): Boolean = false

  describe("The diagrammed assertion macro") {

    it("should not render a function value passed to a type-aliased function parameter") {
      val f: IntToInt = _ + 1
      val e = intercept[TestFailedException] {
        assert(alwaysFalseAlias(f))
      }
      val message = e.message.get
      message should include ("assert(alwaysFalseAlias(f))")
      message should not include (f.toString)
    }

    it("should not render a function value passed to an annotated type-aliased function parameter") {
      val f: AnnotatedIntToInt = _ + 1
      val e = intercept[TestFailedException] {
        assert(alwaysFalseAnnotatedAlias(f))
      }
      val message = e.message.get
      message should include ("assert(alwaysFalseAnnotatedAlias(f))")
      message should not include (f.toString)
    }

    it("should still render function values that are ordinary operands") {
      val f: Int => Int = _ + 1
      val g: Int => Int = _ + 2
      val e = intercept[TestFailedException] {
        assert(f == g)
      }
      val message = e.message.get
      message should include ("assert(f == g)")
      message should include (f.toString)
      message should include (g.toString)
      message should include ("false")
    }
  }

  describe("The diagram renderer") {

    // SKIP-SCALATESTJS,NATIVE-START
    it("should prefer a regular value over a compiler-generated lambda that shares its anchor, and still keep a lambda that is alone at its anchor") {
      val sharedAnchorLambda: Any = (i: Int) => i + 1
      // Use a different arity so its rendering cannot collide with sharedAnchorLambda's
      // on Scala 2.11, where both function values render as <functionN>.
      val ownAnchorLambda: Any = (i: Int, j: Int) => i + j
      val expr = new DiagrammedExpr[Boolean] {
        val anchor = 7
        val value = false
        val anchorValues = List(
          AnchorValue(7, sharedAnchorLambda),
          AnchorValue(7, "regular"),
          AnchorValue(13, ownAnchorLambda)
        )
      }

      val e = intercept[TestFailedException] {
        Diagrams.diagrammedAssertionsHelper.macroAssert(expr, "", "assert(x, y)", Position.here)
      }
      val message = e.message.get
      message should include ("regular")
      message should not include (sharedAnchorLambda.toString)
      message should include (ownAnchorLambda.toString)
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }
}
