/*
 * Copyright 2001-2024 Artima, Inc.
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

import scala.quoted._
import org.scalactic._
import org.scalatest._

object DiagrammedAssertionsMacro {
  /**
   * The macro implementation, it'll try to detect if it is a multiline expression, if it is, it'll just fallback to use BooleanMacro.
   *
   */
  private def macroImpl(
    helper: Expr[(DiagrammedExpr[Boolean], Any, String, source.Position) => Assertion],
    fallback: Expr[(Bool, Any, source.Position) => Assertion],
    condition: Expr[Boolean], clue: Expr[Any],
    prettifier: Expr[Prettifier],
    pos: Expr[source.Position])(using Quotes): Expr[Assertion] = {

    val macroPos = quotes.reflect.Position.ofMacroExpansion
    val startLine = macroPos.startLine // Get the expression first line number
    val endLine = macroPos.endLine // Get the expression last line number

    if (startLine == endLine) // Only use diagram macro if it is one line, where startLine will be equaled to endLine
      DiagramsMacro.transform(helper, condition, pos, clue, macroPos.sourceCode.get)
    else // otherwise we'll just fallback to use BooleanMacro
      AssertionsMacro.transform(fallback, condition, prettifier, pos, clue)
  }

  def assert(condition: Expr[Boolean], prettifier: Expr[Prettifier], pos: Expr[source.Position], clue: Expr[Any])(using Quotes): Expr[Assertion] = {
    macroImpl(
      '{ Diagrams.diagrammedAssertionsHelper.macroAssert },
      '{ Assertions.assertionsHelper.macroAssert },
      condition, clue, prettifier, pos)
  }

  def assume(condition: Expr[Boolean], prettifier: Expr[Prettifier], pos: Expr[source.Position], clue: Expr[Any])(using Quotes): Expr[Assertion] = {
    macroImpl(
      '{ Diagrams.diagrammedAssertionsHelper.macroAssume },
      '{ Assertions.assertionsHelper.macroAssume },
      condition, clue, prettifier, pos)
  }
}
