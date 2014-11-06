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

private[scalactic] object PosLMacro {

  def apply(c: Context)(value: c.Expr[Long]): c.Expr[PosL] = {

    import c.universe._

    value.tree match {
      case Literal(longConst) =>
        if (longConst.value.toString.toLong > 0L)
          reify { PosL.from(value.splice).get }
        else
          c.abort(c.enclosingPosition, Resources("nonPositivePosL"))
      case _ =>
        c.abort(c.enclosingPosition, Resources("nonPositivePosL"))
    }
  }
}
