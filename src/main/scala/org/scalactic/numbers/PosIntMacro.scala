/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalactic.numbers

import reflect.macros.Context
import org.scalactic.Resources

private[scalactic] object PosIntMacro extends NumericMacroHelpers {

  def apply(c: Context)(value: c.Expr[Int]): c.Expr[PosInt] = {
    val notValidMsg = Resources("notValidPosInt")
    val notLiteralMsg = Resources("notLiteralPosInt")

    import c.universe._

    ensureValidIntLiteral(c)(value, notValidMsg, notLiteralMsg) { i => i > 0 }
    reify { PosInt.from(value.splice).get }
  }
}
