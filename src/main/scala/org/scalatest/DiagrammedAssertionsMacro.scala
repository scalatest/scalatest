/*
 * Copyright 2001-2013 Artima, Inc.
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

import org.scalautils.{BooleanMacro, DiagrammedBooleanMacro}
import reflect.macros.Context

object DiagrammedAssertionsMacro {

  private[this] def getFirstLine(context: Context)(expr: context.Tree): Int = {
    import context.universe._
    expr match {
      case apply: Apply => getFirstLine(context)(apply.fun)
      case typeApply: TypeApply => getFirstLine(context)(typeApply.fun)
      case select: Select => getFirstLine(context)(select.qualifier)
      case other => other.pos.asInstanceOf[scala.reflect.internal.util.Position].line
    }
  }

  private def getLastLine(context: Context)(tree: context.Tree): Int = {
    import context.universe._
    tree match {
      case apply: Apply =>
        if (apply.args.length > 0)
          getLastLine(context)(apply.args.last)
        else
          getLastLine(context)(apply.fun)
      case typeApply: TypeApply =>
        if (typeApply.args.length > 0)
          getLastLine(context)(typeApply.args.last)
        else
          getLastLine(context)(typeApply.fun)
      case other => other.pos.asInstanceOf[scala.reflect.internal.util.Position].line
    }
  }

  def assert(context: Context)(condition: context.Expr[Boolean]): context.Expr[Unit] = {

    import context.universe._

    val startLine = getFirstLine(context)(condition.tree)
    val endLine = getLastLine(context)(condition.tree)

    if (startLine == endLine) // Only use diagram macro if it is one line
      new DiagrammedBooleanMacro[context.type](context, "diagrammedAssertionsHelper").genMacro(condition, "macroAssert", context.literal(""))
    else
      new BooleanMacro[context.type](context, "diagrammedAssertionsHelper").genMacro(condition, "macroAssert", context.literal(""))
  }

}