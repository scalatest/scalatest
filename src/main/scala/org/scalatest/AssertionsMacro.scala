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

private[scalatest] class AssertionsMacro[C <: Context](val context: C) {

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
   *   $org_scalatest_AssertionsHelper.macroAssertTrue($org_scalatest_assert_macro_left, "==", $org_scalatest_assert_macro_right, $org_scalatest_assert_macro_result, None)
   * }
   *
   */

  /*
   * Translate the following:
   *
   * assert(validate(1, 2, 3))
   *
   * to:
   *
   * $org_scalatest_AssertionsHelper.macroAssertTrue(validate(1, 2, 3), None)
   *
   */

  import context.universe._

  def simpleAssertMacro(exprTree: Tree): Expr[Unit] =
    context.Expr(
      Apply(
        Select(
          Ident("$org_scalatest_AssertionsHelper"),
          newTermName("macroAssertTrue")
        ),
        List(exprTree, Ident("None"))
      )
    )

  def assertMacro(operator: String): Apply =
    Apply(
      Select(
        Ident("$org_scalatest_AssertionsHelper"),
        newTermName("macroAssertTrue")
      ),
      List(Ident(newTermName("$org_scalatest_assert_macro_left")), context.literal(operator).tree, Ident(newTermName("$org_scalatest_assert_macro_right")), Ident(newTermName("$org_scalatest_assert_macro_result")), Ident("None"))
    )

  def genClue(clueTree: Tree): Apply =
    Apply(
      Select(
        Ident("Some"),
        newTermName("apply")
      ),
      List(clueTree)
    )

  def simpleAssertMacroWithClue(exprTree: Tree, clueTree: Tree): Expr[Unit] =
    context.Expr(
      Apply(
        Select(
          Ident("$org_scalatest_AssertionsHelper"),
          newTermName("macroAssertTrue")
        ),
        List(exprTree, genClue(clueTree))
      )
    )

  def assertMacroWithClue(operator: String, clueTree: Tree): Apply =
    Apply(
      Select(
        Ident("$org_scalatest_AssertionsHelper"),
        newTermName("macroAssertTrue")
      ),
      List(Ident(newTermName("$org_scalatest_assert_macro_left")), context.literal(operator).tree, Ident(newTermName("$org_scalatest_assert_macro_right")), Ident(newTermName("$org_scalatest_assert_macro_result")), genClue(clueTree))
    )

  def simpleSubstitute(select: Select): Apply =
    Apply(
      Select(
        Ident("$org_scalatest_assert_macro_left"),
        select.name
      ),
      List(Ident("$org_scalatest_assert_macro_right"))
    )

  def nestedSubstitute(select: Select, apply: Apply): Apply =
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

  def genExpression(left: Tree, operator: String, right: Tree, subsitutedExpr: Apply, assertExpr: Apply): Expr[Unit] =
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
        assertExpr
      )
    )

  def apply(booleanExpr: Expr[Boolean]): Expr[Unit] = {
    val booleanTree = booleanExpr.tree
    booleanTree match {
      case apply: Apply if apply.args.size == 1 =>
        apply.fun match {
          case select: Select =>
            val sExpr: Apply = simpleSubstitute(select)
            val operator: String = select.name.decoded
            val assertExpr: Apply = assertMacro(operator)
            genExpression(select.qualifier.duplicate, operator, apply.args(0).duplicate, sExpr, assertExpr)
          case funApply: Apply if funApply.args.size == 1 => // For === and !== that takes Equality
            funApply.fun match {
              case select: Select =>
                val sExpr: Apply = nestedSubstitute(select, apply)
                val operator: String = select.name.decoded
                val assertExpr: Apply = assertMacro(operator)
                genExpression(select.qualifier.duplicate, operator, funApply.args(0).duplicate, sExpr, assertExpr)
              case _ => simpleAssertMacro(booleanTree)
            }
          case _ => simpleAssertMacro(booleanTree)
        }
      case _ => simpleAssertMacro(booleanTree)
    }
  }

  def apply(booleanExpr: Expr[Boolean], clueExpr: Expr[Any]): Expr[Unit] = {
    val booleanTree = booleanExpr.tree
    val clueTree = clueExpr.tree
    booleanTree match {
      case apply: Apply if apply.args.size == 1 =>
        apply.fun match {
          case select: Select =>
            val sExpr: Apply = simpleSubstitute(select)
            val operator: String = select.name.decoded
            val assertExpr: Apply = assertMacroWithClue(operator, clueTree)
            genExpression(select.qualifier.duplicate, operator, apply.args(0).duplicate, sExpr, assertExpr)
          case funApply: Apply if funApply.args.size == 1 => // For === and !== that takes Equality
            funApply.fun match {
              case select: Select =>
                val sExpr: Apply = nestedSubstitute(select, apply)
                val operator: String = select.name.decoded
                val assertExpr: Apply = assertMacroWithClue(operator, clueTree)
                genExpression(select.qualifier.duplicate, operator, funApply.args(0).duplicate, sExpr, assertExpr)
              case _ => simpleAssertMacroWithClue(booleanTree, clueTree)
            }
          case _ => simpleAssertMacroWithClue(booleanTree, clueTree)
        }
      case _ => simpleAssertMacroWithClue(booleanTree, clueTree)
    }
  }
  
  /*private[this] def getPosition(expr: Tree) = expr.pos.asInstanceOf[scala.reflect.internal.util.Position]

  def getText(expr: Tree): String = getPosition(expr) match {
    case p: RangePosition => context.echo(expr.pos, "RangePosition found!"); p.lineContent.slice(p.start, p.end).trim
    case p: reflect.internal.util.Position => p.lineContent.trim
  }*/
}

object AssertionsMacro {

  def apply(context: Context)(condition: context.Expr[Boolean]): context.Expr[Unit] = {
    new AssertionsMacro[context.type](context).apply(condition)
  }

  def applyWithClue(context: Context)(condition: context.Expr[Boolean], clue: context.Expr[Any]): context.Expr[Unit] = {
    new AssertionsMacro[context.type](context).apply(condition, clue)
  }
}