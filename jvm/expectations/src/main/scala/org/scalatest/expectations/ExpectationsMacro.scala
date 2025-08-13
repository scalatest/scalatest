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
package expectations

import org.scalactic._
import scala.reflect.macros.whitebox.Context

/**
 * Macro implementation that provides rich error message for boolean expression assertion.
 */
private[scalatest] object ExpectationsMacro {

  def expect(context: Context)(expression: context.Expr[Boolean])(prettifier: context.Expr[Prettifier], pos: context.Expr[source.Position]): context.Expr[Fact] = {
    import context.universe._
    new BooleanMacro[context.type](context).genMacro[Fact](
      Select(
        Select(
          Select(
            Select(
              Select(
                Ident(TermName("_root_")),
                TermName("org")
              ),
              TermName("scalatest")
            ),
            TermName("expectations")
          ),
          TermName("Expectations")
        ),
        TermName("expectationsHelper")
      ),
      expression,
      "macroExpect",
      context.Expr[String](q"${""}"),
      prettifier,
      pos)
  }

  def expectWithClue(context: Context)(expression: context.Expr[Boolean], clue: context.Expr[Any])(prettifier: context.Expr[Prettifier], pos: context.Expr[source.Position]): context.Expr[Fact] = {
    import context.universe._
    new BooleanMacro[context.type](context).genMacro[Fact](
      Select(
        Select(
          Select(
            Select(
              Select(
                Ident(TermName("_root_")),
                TermName("org")
              ),
              TermName("scalatest")
            ),
            TermName("expectations")
          ),
          TermName("Expectations")
        ),
        TermName("expectationsHelper")
      ),
      expression,
      "macroExpect",
      clue,
      prettifier,
      pos)
  }

}
