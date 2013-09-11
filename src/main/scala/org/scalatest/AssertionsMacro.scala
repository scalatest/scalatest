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
    val text: Expr[String] = context.literal(getText(booleanExpr.tree))
    predicate match {
      case Some(RecognizedPredicate(left, operator, right, subsitutedExpr)) => 
        /* 
         * Translate the following:
         * 
         * assert(something.aMethod == 3)
         * 
         * to: 
         * 
         * {
         *   val $org_scalatest_assert_macro_left = something.aMethod
         *   val $org_scalatest_assert_macro_right = 3
         *   val $org_scalatest_assert_macro_result = $org_scalatest_assert_macro_left ==  $org_scalatest_assert_macro_result
         *   $org_scalatest_AssertionsHelper.macroAssertTrue($org_scalatest_assert_macro_left, "==", $org_scalatest_assert_macro_right, $org_scalatest_assert_macro_result, "something.aMethod == 3")
         * }
         * 
         */
        context.Expr(
          Block(
            ValDef(
              Modifiers(), 
              newTermName("$org_scalatest_assert_macro_left"), 
              TypeTree(), 
              left.duplicate
            ), 
            ValDef(
              Modifiers(), 
              newTermName("$org_scalatest_assert_macro_right"), 
              TypeTree(), 
              right.duplicate
            ), 
            ValDef(
              Modifiers(), 
              newTermName("$org_scalatest_assert_macro_result"), 
              TypeTree(), 
              subsitutedExpr
            ), 
            Apply(
              Select(
                Ident("$org_scalatest_AssertionsHelper"), 
                newTermName("macroAssertTrue")
              ),
              List(Ident(newTermName("$org_scalatest_assert_macro_left")), context.literal(operator).tree, Ident(newTermName("$org_scalatest_assert_macro_right")), Ident(newTermName("$org_scalatest_assert_macro_result")), text.tree)
            )
          )
        )
        
      case None => 
        /* 
         * Translate the following:
         * 
         * assert(validate(1, 2, 3))
         * 
         * to: 
         * 
         * $org_scalatest_AssertionsHelper.macroAssertTrue(validate(1, 2, 3), "validate(1, 2, 3)")
         * 
         */
        context.Expr(
          Apply(
            Select(
              Ident("$org_scalatest_AssertionsHelper"), 
              newTermName("macroAssertTrue")
            ), 
            List(booleanExpr.tree, text.tree)
          )  
        )
    }
  }

  def apply(booleanExpr: Expr[Boolean], clueExpr: Expr[Any]): Expr[Unit] = {

    val predicate = parsePredicate(booleanExpr.tree)
    val text: Expr[String] = context.literal(getText(booleanExpr.tree))
    predicate match {
      case Some(RecognizedPredicate(left, operator, right, subsitutedExpr)) =>
        /*
         * Translate the following:
         *
         * assert(something.aMethod == 3)
         *
         * to:
         *
         * {
         *   val $org_scalatest_assert_macro_left = something.aMethod
         *   val $org_scalatest_assert_macro_right = 3
         *   val $org_scalatest_assert_macro_result = $org_scalatest_assert_macro_left ==  $org_scalatest_assert_macro_result
         *   $org_scalatest_AssertionsHelper.macroAssertTrue($org_scalatest_assert_macro_left, "==", $org_scalatest_assert_macro_right, $org_scalatest_assert_macro_result, "something.aMethod == 3", clue)
         * }
         *
         */
        context.Expr(
          Block(
            ValDef(
              Modifiers(),
              newTermName("$org_scalatest_assert_macro_left"),
              TypeTree(),
              left.duplicate
            ),
            ValDef(
              Modifiers(),
              newTermName("$org_scalatest_assert_macro_right"),
              TypeTree(),
              right.duplicate
            ),
            ValDef(
              Modifiers(),
              newTermName("$org_scalatest_assert_macro_result"),
              TypeTree(),
              subsitutedExpr
            ),
            Apply(
              Select(
                Ident("$org_scalatest_AssertionsHelper"),
                newTermName("macroAssertTrue")
              ),
              List(Ident(newTermName("$org_scalatest_assert_macro_left")), context.literal(operator).tree, Ident(newTermName("$org_scalatest_assert_macro_right")), Ident(newTermName("$org_scalatest_assert_macro_result")), text.tree, clueExpr.tree)
            )
          )
        )

      case None =>
        /*
         * Translate the following:
         *
         * assert(validate(1, 2, 3))
         *
         * to:
         *
         * $org_scalatest_AssertionsHelper.macroAssertTrue(validate(1, 2, 3), "validate(1, 2, 3)")
         *
         */
        context.Expr(
          Apply(
            Select(
              Ident("$org_scalatest_AssertionsHelper"),
              newTermName("macroAssertTrue")
            ),
            List(booleanExpr.tree, text.tree, clueExpr.tree)
          )
        )
    }
  }
  
  case class RecognizedPredicate(left: Tree, operator: String, right: Tree, subsitutedExpr: Apply)
  
  def parsePredicate(tree: Tree): Option[RecognizedPredicate] = {
    tree match {
      case apply: Apply if apply.args.size == 1 =>
        apply.fun match {
          case select: Select => 
            val sExpr: Apply = 
              Apply(
                Select(
                  Ident("$org_scalatest_assert_macro_left"), 
                  select.name
                ), 
                List(Ident("$org_scalatest_assert_macro_right"))
              )
            Some(RecognizedPredicate(select.qualifier.duplicate, select.name.decoded, apply.args(0).duplicate, sExpr))
          case funApply: Apply if funApply.args.size == 1 => // For === and !== that takes Equality
            funApply.fun match {
              case select: Select =>
                val sExpr: Apply = 
                  Apply(
                    Apply(
                      Select(
                        Ident("$org_scalatest_assert_macro_left"), 
                        select.name
                      ), 
                      List(Ident("$org_scalatest_assert_macro_right"))
                    ), 
                    List(apply.args(0).duplicate)
                  )
                Some(RecognizedPredicate(select.qualifier.duplicate, select.name.decoded, funApply.args(0).duplicate, sExpr))
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

  def applyWithClue(context: Context)(condition: context.Expr[Boolean], clue: context.Expr[Any]): context.Expr[Unit] = {
    new AssertionsMacro[context.type](context).apply(condition, clue)
  }
}