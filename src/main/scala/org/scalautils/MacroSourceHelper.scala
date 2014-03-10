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
package org.scalautils

import reflect.macros.Context

private[org] class MacroSourceHelper[C <: Context](context: C) {

  import context.universe._

  private def getPosition(expr: Tree): Int = {
    expr match {
      case apply: Apply => getPosition(apply.fun)
      case typeApply: TypeApply => getPosition(typeApply.fun)
      case select: Select => getPosition(select.qualifier)
      case other => other.pos.asInstanceOf[scala.reflect.internal.util.Position].point
    }
  }

  private def getEndOffset(tree: Tree): Int = {
    tree match {
      case apply: Apply =>
        if (apply.args.length > 0)
          getEndOffset(apply.args.last)
        else
          getEndOffset(apply.fun)
      case typeApply: TypeApply =>
        if (typeApply.args.length > 0)
          getEndOffset(typeApply.args.last)
        else
          getEndOffset(typeApply.fun)
      case other =>
        val otherPos = other.pos
        val otherOffset = otherPos.point
        val otherColumn = otherPos.column
        val lineContent = otherPos.lineContent
        val lineLength = lineContent.length
        otherOffset + lineLength - otherColumn
    }
  }

  def getSourceList(expressions: context.Expr[Any]*): List[String] = {
    val content = context.macroApplication.pos.source.content.mkString
    val offsetList =
      (expressions.map { expr =>
        getPosition(expr.tree)
      }).toList

    val rangeList = offsetList.sliding(2)
    val initSourceList =
      (rangeList.map { case List(start, end) =>
        val raw = content.substring(start, end).trim
        if (raw.endsWith(","))
          raw.substring(0, raw.length - 1)
        else
          raw
      }).toList

    // Let's get the last one, the last one is the most tricky one,
    // we'll double check the source we get by parse and type check it, and
    // compare it to the original tree's text (by calling show()).  If they are
    // different, we'll just fallback to use the show(original) text.
    val lastSource = content.substring(offsetList.last, getEndOffset(expressions.last.tree))
    val lastShowText = show(expressions.last.tree)
    try {
      if (show(context.typeCheck(context.parse(lastSource))) == lastShowText)
        initSourceList ++ List(lastSource)
      else
        initSourceList ++ List(show(lastShowText))
    }
    catch {
      case e: Throwable => initSourceList ++ List(lastShowText)
    }
  }

}