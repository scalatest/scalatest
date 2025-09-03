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
package org.scalatest

import org.scalactic.*
import scala.quoted.*
import org.scalatest.compatible.Assertion

/**
 * Macro implementation that provides rich error message for boolean expression assertion.
 */
object AssertionsMacro {
  /**
   * Provides assertion implementation for <code>Assertions.assert(booleanExpr: Boolean)</code>, with rich error message.
   *
   * @param condition original condition expression
   * @return transformed expression that performs the assertion check and throw <code>TestFailedException</code> with rich error message if assertion failed
   */
  def assert(condition: Expr[Boolean], prettifier: Expr[Prettifier], pos: Expr[source.Position], clue: Expr[Any])(using Quotes): Expr[Assertion] =
    transform('{Assertions.assertionsHelper.macroAssert}, condition, prettifier, pos, clue)

  /**
   * Provides implementation for <code>Assertions.assume(booleanExpr: Boolean)</code>, with rich error message.
   *
   * @param context macro context
   * @param condition original condition expression
   * @return transformed expression that performs the assumption check and throw <code>TestCanceledException</code> with rich error message if assumption failed
   */
  def assume(condition: Expr[Boolean], prettifier: Expr[Prettifier], pos: Expr[source.Position], clue: Expr[Any])(using Quotes): Expr[Assertion] =
    transform('{Assertions.assertionsHelper.macroAssume}, condition, prettifier, pos, clue)

  def transform(
    helper:Expr[(Bool, Any, source.Position) => Assertion],
    condition: Expr[Boolean], prettifier: Expr[Prettifier],
    pos: Expr[source.Position], clue: Expr[Any]
  )
  (using Quotes): Expr[Assertion] = {
    val bool = BooleanMacro.parse(condition, prettifier)
    source.Position.withPosition[Assertion]('{(pos: source.Position) => ($helper)($bool, $clue, pos) })
  }
}