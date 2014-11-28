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

private[anyvals] object OddIntMacro extends CompileTimeAssertions {
  def apply(c: Context)(value: c.Expr[Int]): c.Expr[OddInt] = {
    val notValidMsg =
      "OddInt.apply can only be invoked on odd Int literals, like OddInt(3)."
    val notLiteralMsg =
      "OddInt.apply can only be invoked on Int literals, like " +
      "OddInt(3). Please use OddInt.from instead."
    ensureValidIntLiteral(c)(value, notValidMsg, notLiteralMsg) { i => i % 2 == 1 }
    c.universe.reify { OddInt.from(value.splice).get }
  } 
}

