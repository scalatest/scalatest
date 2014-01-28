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
package org.scalautils

import reflect.macros.Context

private[org] class BooleanMacro[C <: Context](val context: C, helperName: String) {

  /*
   * Translate the following:
   *
   * assert(something.aMethod == 3)
   *
   * to:
   *
   * {
   *   val $org_scalautils_assert_macro_left = something.aMethod
   *   val $org_scalautils_assert_macro_right = 3
   *   val $org_scalautils_assert_macro_result = $org_scalautils_assert_macro_left ==  $org_scalautils_assert_macro_result
   *   assertionsHelper.macroAssert($org_scalautils_assert_macro_left, "==", $org_scalautils_assert_macro_right, $org_scalautils_assert_macro_result, None)
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
   * assertionsHelper.macroAssert(validate(1, 2, 3), None)
   *
   */

  import context.universe._

  // Generate AST for:
  // assertionsHelper.methodName(expression, clue)
  def genCallAssertionsHelperSimple(methodName: String, exprTree: Tree, clueTree: Tree): Expr[Unit] =
    context.Expr(
      Apply(
        Select(
          Ident(helperName),
          newTermName(methodName)
        ),
        List(exprTree, clueTree)
      )
    )

  // Generate AST for:
  // assertionsHelper.methodName($org_scalautils_assert_macro_left, operator, $org_scalautils_assert_macro_right, $org_scalautils_assert_macro_result, clue)
  def genCallAssertionsHelper(methodName: String, operator: String, clueTree: Tree): Apply =
    Apply(
      Select(
        Ident(helperName),
        newTermName(methodName)
      ),
      List(Ident(newTermName("$org_scalautils_assert_macro_left")), context.literal(operator).tree, Ident(newTermName("$org_scalautils_assert_macro_right")), Ident(newTermName("$org_scalautils_assert_macro_result")), clueTree)
    )

  // Generate AST for:
  // Some("message")
  def genClue(clueTree: Tree): Apply =
    Apply(
      Select(
        Ident("Some"),
        newTermName("apply")
      ),
      List(clueTree)
    )

  // Generate AST for:
  // $org_scalautils_assert_macro_left operator $org_scalautils_assert_macro_right
  def simpleSubstitute(select: Select): Apply =
    Apply(
      Select(
        Ident("$org_scalautils_assert_macro_left"),
        select.name
      ),
      List(Ident("$org_scalautils_assert_macro_right"))
    )

  // Generate AST for:
  // $org_scalautils_assert_macro_left.operator($org_scalautils_assert_macro_right)(arguments)
  def nestedSubstitute(select: Select, apply: GenericApply): Apply =
    Apply(
      Apply(
        Select(
          Ident("$org_scalautils_assert_macro_left"),
          select.name
        ),
        List(Ident("$org_scalautils_assert_macro_right"))
      ),
      apply.args
    )

  // Generate AST for:
  // val $org_scalautils_assert_macro_left = left
  // val $org_scalautils_assert_macro_right = right
  // val $org_scalautils_assert_macro_result = subsitutedExpr
  // assertExpr
  def genExpression(left: Tree, operator: String, right: Tree, subsitutedExpr: Apply, assertExpr: Apply): Expr[Unit] =
    context.Expr(
      Block(
        ValDef(
          Modifiers(),
          newTermName("$org_scalautils_assert_macro_left"),
          TypeTree(),
          left.duplicate
        ),
        ValDef(
          Modifiers(),
          newTermName("$org_scalautils_assert_macro_right"),
          TypeTree(),
          right.duplicate
        ),
        ValDef(
          Modifiers(),
          newTermName("$org_scalautils_assert_macro_result"),
          TypeTree(),
          subsitutedExpr
        ),
        assertExpr
      )
    )

  private val supportedOperations = Set("==", "!=", "===", "!==")

  def isSupported(operator: String) = supportedOperations.contains(operator)

  def genMacroCode(booleanExpr: Expr[Boolean], methodName: String, clueTree: Option[Tree]): Expr[Unit] = {
    val booleanTree = booleanExpr.tree
    val clueOption =
      clueTree match {
        case Some(clue) => genClue(clue)
        case _ => Ident("None")
      }

    booleanTree match {
      case apply: Apply =>
        apply.fun match {
          case select: Select if apply.args.size == 1 => // For simple assert(a == b)
            val operator: String = select.name.decoded
            if (isSupported(operator)) {
              val sExpr: Apply = simpleSubstitute(select)
              val assertExpr: Apply = genCallAssertionsHelper(methodName, operator, clueOption)
              genExpression(select.qualifier.duplicate, operator, apply.args(0).duplicate, sExpr, assertExpr)
            }
            else
              genCallAssertionsHelperSimple(methodName, booleanTree, clueOption)
          case funApply: Apply =>
            funApply.fun match {
              case select: Select if funApply.args.size == 1 => // For === that takes Equality
                val operator: String = select.name.decoded
                if (isSupported(operator)) {
                  val sExpr: Apply = nestedSubstitute(select, apply)
                  val assertExpr: Apply = genCallAssertionsHelper(methodName, operator, clueOption)
                  genExpression(select.qualifier.duplicate, operator, funApply.args(0).duplicate, sExpr, assertExpr)
                }
                else
                  genCallAssertionsHelperSimple(methodName, booleanTree, clueOption)
              case typeApply: TypeApply =>
                typeApply.fun match {
                  case select: Select if funApply.args.size == 1 => // For TypeCheckedTripleEquals
                    val operator: String = select.name.decoded
                    if (isSupported(operator)) {
                      val sExpr: Apply = nestedSubstitute(select, apply)
                      val assertExpr: Apply = genCallAssertionsHelper(methodName, operator, clueOption)
                      genExpression(select.qualifier.duplicate, operator, funApply.args(0).duplicate, sExpr, assertExpr)
                    }
                    else
                      genCallAssertionsHelperSimple(methodName, booleanTree, clueOption)
                  case _ => genCallAssertionsHelperSimple(methodName, booleanTree, clueOption)
                }
              case _ => genCallAssertionsHelperSimple(methodName, booleanTree, clueOption)
            }
          case _ => genCallAssertionsHelperSimple(methodName, booleanTree, clueOption)
        }
      case _ => genCallAssertionsHelperSimple(methodName, booleanTree, clueOption)
    }
  }

  /*private[this] def getPosition(expr: Tree) = expr.pos.asInstanceOf[scala.reflect.internal.util.Position]

  def getText(expr: Tree): String = getPosition(expr) match {
    case p: RangePosition => context.echo(expr.pos, "RangePosition found!"); p.lineContent.slice(p.start, p.end).trim
    case p: reflect.internal.util.Position => p.lineContent.trim
  }*/
}