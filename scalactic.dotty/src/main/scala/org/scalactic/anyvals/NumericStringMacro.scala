/*
* Copyright 2001-2014 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalactic.anyvals

import org.scalactic.Resources
import scala.quoted._

import CompileTimeAssertions._

object NumericStringMacro {

  def isValid(s: String): Boolean = s.forall(c => c >= '0' && c <= '9')

  def apply(value: Expr[String])(implicit qctx: QuoteContext): Expr[NumericString] = {
    import qctx.tasty._

    val notValidMsg =
      "NumericString.apply can only be invoked on String literals that contain numeric characters, i.e., decimal digits '0' through '9', " +
      "like \"123\"."
    val notLiteralMsg =
      "NumericString.apply can only be invoked on String literals that contain only numeric characters, i.e., decimal digits '0' through '9', like \"123\"" +
      " Please use NumericString.from instead."
    ensureValidStringLiteral(value, notValidMsg, notLiteralMsg)(isValid)

    '{ NumericString.ensuringValid($value) }
  }
}
