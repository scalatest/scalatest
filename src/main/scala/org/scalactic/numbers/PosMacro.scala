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

private[scalactic] object PosMacro {

  def apply(c: Context)(value: c.Expr[Int]): c.Expr[Pos] = {

    import c.universe._

    value.tree match {
      case Literal(intConst) =>
        if (intConst.value.toString.toInt > 0)
          reify { Pos.from(value.splice).get }
        else
          c.abort(c.enclosingPosition, "Pos.apply only works with positive integer literals.")
      case _ =>
        c.abort(c.enclosingPosition, "Pos.apply only works with positive integer literals.")
    }
  }

}