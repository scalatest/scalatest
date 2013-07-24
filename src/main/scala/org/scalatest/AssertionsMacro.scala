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

  def apply(booleanExpr: Expr[Boolean]): Expr[Unit] = {
    
    val predicate = parsePredicate(booleanExpr.tree)
    predicate match {
      case Some(RecognizedPredicate(left, operator, right)) => 
        val args = 
          operator match {
            case "==" => List(left, right, booleanExpr.tree, context.literal("didNotEqual").tree)
            case "===" => List(left, right, booleanExpr.tree, context.literal("didNotEqual").tree)
            case "!=" => List(left, right, booleanExpr.tree, context.literal("equaled").tree)
            case "!==" => List(left, right, booleanExpr.tree, context.literal("equaled").tree)
            case ">" => List(left, right, booleanExpr.tree, context.literal("wasNotGreaterThan").tree)
            case ">=" => List(left, right, booleanExpr.tree, context.literal("wasNotGreaterThanOrEqualTo").tree)
            case "<" => List(left, right, booleanExpr.tree, context.literal("wasNotLessThan").tree)
            case "<=" => List(left, right, booleanExpr.tree, context.literal("wasNotLessThanOrEqualTo").tree)
            case _ => 
              val text: Expr[String] = context.literal(getText(booleanExpr.tree))
              List(booleanExpr.tree, text.tree)
          }
        context.Expr(
          Apply(
            Select(
              Ident("scalatestAssertionsHelper"), 
              newTermName("macroAssertTrue")
            ),
            args
          )  
        )
      case None => 
        val text: Expr[String] = context.literal(getText(booleanExpr.tree))
        context.Expr(
          Apply(
            Select(
              Ident("scalatestAssertionsHelper"), 
              newTermName("macroAssertTrue")
            ), 
            List(booleanExpr.tree, text.tree)
          )  
        )
    }
  }
  
  case class RecognizedPredicate(left: Tree, operator: String, right: Tree)
  
  def parsePredicate(tree: Tree): Option[RecognizedPredicate] = {
    tree match {
      case apply: Apply if apply.args.size == 1 =>
        apply.fun match {
          case select: Select => 
            Some(RecognizedPredicate(select.qualifier.duplicate, select.name.decoded, apply.args(0).duplicate))
          case funApply: Apply if funApply.args.size == 1 => // For === and !== that takes Equality
            funApply.fun match {
              case select: Select if select.name.decoded == "===" || select.name.decoded == "!==" => 
                Some(RecognizedPredicate(select.qualifier.duplicate, select.name.decoded, funApply.args(0).duplicate))
              case _ => None
            }
          case _ => None
        }
      case _ => None
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