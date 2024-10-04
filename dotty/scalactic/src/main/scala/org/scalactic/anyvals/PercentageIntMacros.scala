/*
* Copyright 2001-2024 Artima, Inc.
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

import scala.quoted._

import CompileTimeAssertions._

object PercentageIntMacro {

  def isValid(i: Int): Boolean = i >= 0 && i <= 100

  def apply(value: Expr[Int])(using Quotes): Expr[PercentageInt] = {
    val notValidMsg =
      "PercentageInt.apply can only be invoked on Int literals between 0 and 100, "+
      "inclusive, like PercentageInt(8)."
    val notLiteralMsg =
      "PercentageInt.apply can only be invoked on Int literals, like PercentageInt(8)."+
      " Please use PercentageInt.from instead."
    ensureValidIntLiteral(value, notValidMsg, notLiteralMsg)(isValid)
    '{ PercentageInt.ensuringValid($value) }
  }
}
