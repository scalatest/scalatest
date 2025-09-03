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
package org.scalatest.expectations

import org.scalactic._
import scala.quoted.*
import org.scalatest.Fact

/**
 * Macro implementation that provides rich error message for boolean expression assertion.
 */
object ExpectationsMacro {

  def expect(condition: Expr[Boolean])(prettifier: Expr[Prettifier])(using Quotes): Expr[Fact] = {
    val bool = BooleanMacro.parse(condition, prettifier)
    '{ Expectations.expectationsHelper.macroExpect($bool, "", $prettifier) }
  }

  def expectWithClue(condition: Expr[Boolean], clue: Expr[Any])(prettifier: Expr[Prettifier])(using Quotes): Expr[Fact] = {
    val bool = BooleanMacro.parse(condition, prettifier)
    '{ Expectations.expectationsHelper.macroExpect($bool, $clue, $prettifier) }
  }
}