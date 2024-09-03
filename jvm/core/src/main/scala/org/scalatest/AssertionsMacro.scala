/*
 * Copyright 2001-2012 Artima, Inc.
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

import org.scalactic._
import reflect.macros.Context

/**
 * Macro implementation that provides rich error message for boolean expression assertion.
 */
private[scalatest] object AssertionsMacro {

  /**
   * Provides assertion implementation for <code>Assertions.assert(booleanExpr: Boolean)</code>, with rich error message.
   *
   * @param context macro context
   * @param condition original condition expression
   * @return transformed expression that performs the assertion check and throw <code>TestFailedException</code> with rich error message if assertion failed
   */
  def assert(context: Context)(condition: context.Expr[Boolean])(prettifier: context.Expr[Prettifier], pos: context.Expr[source.Position]): context.Expr[Assertion] = {
    import context.universe._
    new BooleanMacro[context.type](context).genMacro[Assertion](
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalatest")
          ),
          TermName("Assertions")
        ),
        TermName("assertionsHelper")
      ),
      condition,
      "macroAssert",
      context.literal(""),
      prettifier,
      pos)
  }

  /**
   * Provides assertion implementation for <code>Assertions.assert(booleanExpr: Boolean, clue: Any)</code>, with rich error message.
   *
   * @param context macro context
   * @param condition original condition expression
   * @param clue original clue expression
   * @return transformed expression that performs the assertion check and throw <code>TestFailedException</code> with rich error message (clue included) if assertion failed
   */
  def assertWithClue(context: Context)(condition: context.Expr[Boolean], clue: context.Expr[Any])(prettifier: context.Expr[Prettifier], pos: context.Expr[source.Position]): context.Expr[Assertion] = {
    import context.universe._
    new BooleanMacro[context.type](context).genMacro[Assertion](
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalatest")
          ),
          TermName("Assertions")
        ),
        TermName("assertionsHelper")
      ),
      condition,
      "macroAssert",
      clue,
      prettifier,
      pos)
  }

  /**
   * Provides implementation for <code>Assertions.assume(booleanExpr: Boolean)</code>, with rich error message.
   *
   * @param context macro context
   * @param condition original condition expression
   * @return transformed expression that performs the assumption check and throw <code>TestCanceledException</code> with rich error message if assumption failed
   */
  def assume(context: Context)(condition: context.Expr[Boolean])(prettifier: context.Expr[Prettifier], pos: context.Expr[source.Position]): context.Expr[Assertion] = {
    import context.universe._
    new BooleanMacro[context.type](context).genMacro[Assertion](
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalatest")
          ),
          TermName("Assertions")
        ),
        TermName("assertionsHelper")
      ),
      condition,
      "macroAssume",
      context.literal(""),
      prettifier,
      pos)
  }

  /**
   * Provides implementation for <code>Assertions.assume(booleanExpr: Boolean, clue: Any)</code>, with rich error message.
   *
   * @param context macro context
   * @param condition original condition expression
   * @param clue original clue expression
   * @return transformed expression that performs the assumption check and throw <code>TestCanceledException</code> with rich error message (clue included) if assumption failed
   */
  def assumeWithClue(context: Context)(condition: context.Expr[Boolean], clue: context.Expr[Any])(prettifier: context.Expr[Prettifier], pos: context.Expr[source.Position]): context.Expr[Assertion] = {
    import context.universe._
    new BooleanMacro[context.type](context).genMacro[Assertion](
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalatest")
          ),
          TermName("Assertions")
        ),
        TermName("assertionsHelper")
      ),
      condition,
      "macroAssume",
      clue,
      prettifier,
      pos)
  }
}