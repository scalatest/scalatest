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

import org.scalactic.BooleanMacro
import reflect.macros.Context

private[scalatest] object DiagrammedAssertionsMacro {

  /**
   * Get first line number of the given expression.
   */
  private[this] def getFirstLine(context: Context)(expr: context.Tree): Int = {
    import context.universe._
    expr match {
      case apply: Apply => getFirstLine(context)(apply.fun) // If it is a Apply, we'll look into its apply.fun, which contains the qualifier (looking for the left-most).
      case typeApply: TypeApply => getFirstLine(context)(typeApply.fun) // If it is a TypedApply, we'll look into its apply.fun, which contains the qualifier (looking for the left-most)
      case select: Select => getFirstLine(context)(select.qualifier) // If it is a Select, we'll look at its qualifier (looking for the left)
      case Block(Nil, expr) => getFirstLine(context)(expr)
      case Block(stats, _) => getFirstLine(context)(stats.head)
      case other => other.pos.asInstanceOf[scala.reflect.internal.util.Position].line // For others, just the position line number
    }
  }

  /**
   * Get last line number of the given expression.
   */
  private def getLastLine(context: Context)(tree: context.Tree): Int = {
    import context.universe._
    tree match {
      case apply: Apply =>
        if (apply.args.length > 0) // If it is a Apply, we'll look into its last argument, which is the right-most expression.
          getLastLine(context)(apply.args.last)
        else // If there's no argument, then we look at the apply.fun then.
          getLastLine(context)(apply.fun)
      case typeApply: TypeApply =>
        if (typeApply.args.length > 0) // If it is a TypeApply, we'll look into its last argument, which is the right-most expression.
          getLastLine(context)(typeApply.args.last)
        else // If there's no argument, then we look at the apply.fun then.
          getLastLine(context)(typeApply.fun)
      case Block(_, expr) => getLastLine(context)(expr)
      case other => other.pos.asInstanceOf[scala.reflect.internal.util.Position].line // For others, just the position line number
    }
  }

  /**
   * Get source text of the expression, current it'll try to look for RangePosition (only available when -Yrange.pos is enabled),
   * else it will just get the line content.
   */
  private[this] def getSourceText(context: Context)(tree: context.Tree): String = {
    import context.universe._
    tree.pos.asInstanceOf[scala.reflect.internal.util.Position].lineContent
  }

  /**
   * The macro implementation, it'll try to detect if it is a multiline expression, if it is, it'll just fallback to use BooleanMacro.
   *
   */
  private def macroImpl(context: Context)(methodName: String, condition: context.Expr[Boolean], clue: context.Expr[Any], prettifier: context.Expr[_]): context.Expr[Assertion] = {
    import context.universe._

    val startLine = getFirstLine(context)(condition.tree) // Get the expression first line number
    val endLine = getLastLine(context)(condition.tree) // Get the expression last line number

    if (startLine == endLine) // Only use diagram macro if it is one line, where startLine will be equaled to endLine
      new DiagrammedExprMacro[context.type](context, "diagrammedAssertionsHelper").genMacro(condition, methodName, clue, getSourceText(context)(condition.tree))
    else // otherwise we'll just fallback to use BooleanMacro
      new BooleanMacro[context.type](context, "assertionsHelper").genMacro[Assertion](condition, methodName, clue, prettifier)
  }

  def assert(context: Context)(condition: context.Expr[Boolean])(prettifier: context.Expr[_]): context.Expr[Assertion] =
    macroImpl(context)("macroAssert", condition, context.literal(""), prettifier)

  def assertWithClue(context: Context)(condition: context.Expr[Boolean], clue: context.Expr[Any])(prettifier: context.Expr[_]): context.Expr[Assertion] =
    macroImpl(context)("macroAssert", condition, clue, prettifier)

  def assume(context: Context)(condition: context.Expr[Boolean])(prettifier: context.Expr[_]): context.Expr[Assertion] =
    macroImpl(context)("macroAssume", condition, context.literal(""), prettifier)

  def assumeWithClue(context: Context)(condition: context.Expr[Boolean], clue: context.Expr[Any])(prettifier: context.Expr[_]): context.Expr[Assertion] =
    macroImpl(context)("macroAssume", condition, clue, prettifier)
}