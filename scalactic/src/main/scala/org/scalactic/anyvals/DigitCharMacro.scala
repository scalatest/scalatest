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

import reflect.macros.Context
import org.scalactic.Resources
import CompileTimeAssertions._

private[scalactic] object DigitCharMacro {
  def apply(c: Context)(value: c.Expr[Char]): c.Expr[DigitChar] = {
    val notValidMsg =
      "DigitChar.apply can only be invoked on Char literals that are digits, " +
      "like '8'."
    val notLiteralMsg =
      "DigitChar.apply can only be invoked on Char literals that are digits, like '8'." +
      " Please use DigitChar.from instead."
    ensureValidCharLiteral(c)(value, notValidMsg, notLiteralMsg) { c =>
      c >= '0' && c <= '9'
    }
    c.universe.reify { DigitChar.from(value.splice).get }
  } 
}
