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
package expectations

import org.scalactic._
import reflect.macros.Context

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
                Ident(newTermName("_root_")),
                newTermName("org")
              ),
              newTermName("scalatest")
            ),
            newTermName("expectations")
          ),
          newTermName("Expectations")
        ),
        newTermName("expectationsHelper")
      ),
      expression,
      "macroExpect",
      context.literal(""),
      prettifier,
      pos)
  }

}
