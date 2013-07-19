/*
 * Copyright 2001-2012 Artima, Inc.
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

import reflect.macros.Context
import collection.mutable.ListBuffer
import collection.immutable.TreeMap
import reflect.internal.util.{Position, OffsetPosition, RangePosition}

class AssertionsMacro[C <: Context](val context: C) {
  import context.universe._

  def apply(condition: Expr[Boolean]): Expr[Unit] = {
    val text: Expr[String] = context.literal(getErrorMessage(condition.tree))
    reify {
      if (!condition.splice) {
        throw Assertions.newAssertionFailedException(Some(text.splice), None, "Assertions.scala", "newAssert", 0)
      }
    }
  }

  def getErrorMessage(tree: Tree): String = {
    tree match {
      case apply: Apply =>
        if (apply.args.size == 1) {
          apply.fun match {
            case select: Select =>
              select.name.decoded match {
                case "==" =>
                  FailureMessages("wasNotEqualTo", select.qualifier, apply.args(0))
                case "!=" =>
                  FailureMessages("wasEqualTo", select.qualifier, apply.args(0))
                case ">" =>
                  FailureMessages("wasNotGreaterThan", select.qualifier, apply.args(0))
                case ">=" =>
                  FailureMessages("wasNotGreaterThanOrEqualTo", select.qualifier, apply.args(0))
                case "<" =>
                  FailureMessages("wasNotLessThan", select.qualifier, apply.args(0))
                case "<=" =>
                  FailureMessages("wasNotLessThanOrEqualTo", select.qualifier, apply.args(0))
                case _ =>
                  FailureMessages("expressionFailed", UnquotedString(getText(tree)))
              }
            case _ =>
              FailureMessages("expressionFailed", UnquotedString(getText(tree)))
          }
        }
        else
          FailureMessages("expressionFailed", UnquotedString(getText(tree)))
      case _ =>
        FailureMessages("expressionFailed", UnquotedString(getText(tree)))
    }
  }
  
  private[this] def getPosition(expr: Tree) = expr.pos.asInstanceOf[scala.reflect.internal.util.Position]

  def getText(expr: Tree): String = getPosition(expr) match {
    case p: RangePosition => context.echo(expr.pos, "RangePosition found!"); p.lineContent.slice(p.start, p.end).trim
    case p: reflect.internal.util.Position => p.lineContent.trim
  }
}

object AssertionsMacro {
  def apply(context: Context)(condition: context.Expr[Boolean]): context.Expr[Unit] = {
    new AssertionsMacro[context.type](context).apply(condition)
  }
}