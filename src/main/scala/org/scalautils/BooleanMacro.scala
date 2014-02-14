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
  // val name = rhs
  def valDef(name: String, rhs: Tree): ValDef =
    ValDef(
      Modifiers(),
      newTermName(name),
      TypeTree(),
      rhs
    )

  private val logicOperators = Set("&&", "||", "&", "|")
  private val supportedOperations = Set("==", "!=", "===", "!==", "<", ">", ">=", "<=") ++ logicOperators

  def isSupported(operator: String) = supportedOperations.contains(operator)

  private[this] def getPosition(expr: Tree) = expr.pos.asInstanceOf[scala.reflect.internal.util.Position]

  def getText(expr: Tree): String = {
    expr match {
      case literal: Literal =>
        getPosition(expr) match {
          case p: scala.reflect.internal.util.RangePosition => p.lineContent.slice(p.start, p.end).trim // this only available when -Yrangepos is enabled
          case p: reflect.internal.util.Position => ""
        }
      case _ => show(expr)
    }
  }

  def binaryMacroBool(select: Select): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("binaryMacroBool")
      ),
      List(
        Ident(newTermName("$org_scalatest_assert_macro_left")),
        context.literal(select.name.decoded).tree,
        Ident(newTermName("$org_scalatest_assert_macro_right")),
        Apply(
          Select(
            Ident(newTermName("$org_scalatest_assert_macro_left")),
            select.name
          ),
          List(Ident(newTermName("$org_scalatest_assert_macro_right")))
        )
      )
    )

  def binaryMacroBool(select: Select, secondArg: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("binaryMacroBool")
      ),
      List(
        Ident(newTermName("$org_scalatest_assert_macro_left")),
        context.literal(select.name.decoded).tree,
        Ident(newTermName("$org_scalatest_assert_macro_right")),
        Apply(
          Apply(
            Select(
              Ident("$org_scalatest_assert_macro_left"),
              select.name
            ),
            List(Ident("$org_scalatest_assert_macro_right"))
          ),
          List(secondArg)
        )
      )
    )

  def simpleMacroBool(expression: Tree, expressionText: String): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("simpleMacroBool")
      ),
      List(
        expression,
        context.literal(expressionText).tree
      )
    )

  def notBool(target: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("notBool")
      ),
      List(
        target.duplicate
      )
    )

  def simpleBool(expression: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("Bool")
        ),
        newTermName("simpleBool")
      ),
      List(
        expression
      )
    )

  def traverseSelect(select: Select, rightExpr: Tree): (Tree, Tree) = {
    val operator = select.name.decoded
    if (logicOperators.contains(operator)) {
      val leftTree =
        select.qualifier match {
          case selectApply: Apply => transformAst(selectApply.duplicate)
          case selectSelect: Select => transformAst(selectSelect.duplicate)
          case _ => select.qualifier.duplicate
        }
      val rightTree = {
        val evalBlock =
          rightExpr match {
            case argApply: Apply => transformAst(argApply.duplicate)
            case argSelect: Select => transformAst(argSelect.duplicate)
            case _ => rightExpr.duplicate
          }
        if (operator == "&&" || operator == "&")  {// generate if (left.value) {...} else false
          If(
            Select(
              Ident(newTermName("$org_scalatest_assert_macro_left")),
              newTermName("value")
            ),
            evalBlock,
            simpleBool(context.literal(false).tree)
          )
        }
        else if (operator == "||" || operator == "|") // || and |, generate if (left.value) true else {...}
          If(
            Select(
              Ident(newTermName("$org_scalatest_assert_macro_left")),
              newTermName("value")
            ),
            simpleBool(context.literal(true).tree),
            evalBlock
          )
        else
          evalBlock
      }
      (leftTree, rightTree)
    }
    else
      (select.qualifier.duplicate, rightExpr.duplicate)
  }

  def transformAst(tree: Tree): Tree =
    tree match {
      case apply: Apply if apply.args.size == 1 =>
        apply.fun match {
          case select: Select if isSupported(select.name.decoded) =>
            val (leftTree, rightTree) =  traverseSelect(select, apply.args(0))
            Block(
              valDef("$org_scalatest_assert_macro_left", leftTree),
              valDef("$org_scalatest_assert_macro_right", rightTree),
              binaryMacroBool(select.duplicate)
            )
          case funApply: Apply if funApply.args.size == 1 => // For === and !== that takes Equality
            funApply.fun match {
              case select: Select if select.name.decoded == "===" || select.name.decoded == "!==" =>
                val (leftTree, rightTree) = traverseSelect(select, funApply.args(0))
                Block(
                  valDef("$org_scalatest_assert_macro_left", leftTree),
                  valDef("$org_scalatest_assert_macro_right", rightTree),
                  binaryMacroBool(select.duplicate, apply.args(0).duplicate) // TODO: should apply.args(0) be traversed also?
                )
              case typeApply: TypeApply =>
                typeApply.fun match {
                  case select: Select if typeApply.args.size == 1 => // For TypeCheckedTripleEquals
                    val operator: String = select.name.decoded
                    if (operator == "===" || operator == "!==") {
                      val (leftTree, rightTree) = traverseSelect(select, funApply.args(0))
                      Block(
                        valDef("$org_scalatest_assert_macro_left", leftTree),
                        valDef("$org_scalatest_assert_macro_right", rightTree),
                        binaryMacroBool(select.duplicate, apply.args(0).duplicate) // TODO: should apply.args(0) be traversed also?
                      )
                    }
                    else
                      simpleMacroBool(tree.duplicate, getText(tree))
                  case _ => simpleMacroBool(tree.duplicate, getText(tree))
                }
              case _ => simpleMacroBool(tree.duplicate, getText(tree))
            }
          case _ => simpleMacroBool(tree.duplicate, getText(tree))
        }
      case select: Select if select.name.decoded == "unary_!" => // for !
        val leftTree =
          select.qualifier match {
            case selectApply: Apply => transformAst(selectApply.duplicate)
            case selectSelect: Select => transformAst(selectSelect.duplicate)
            case _ => simpleMacroBool(select.qualifier.duplicate, getText(select.qualifier))
          }
        notBool(leftTree.duplicate)
      case _ => simpleMacroBool(tree.duplicate, getText(tree))
    }

  // Generate AST for:
  // helper.methodName(expression, clue)
  def callHelper(methodName: String, clueTree: Tree): Apply =
    Apply(
      Select(
        Ident(newTermName(helperName)),
        newTermName(methodName)
      ),
      List(Ident(newTermName("$org_scalatest_assert_macro_expr")), clueTree)
    )

  def genMacro(booleanExpr: Expr[Boolean], methodName: String, clueExpr: Expr[Any]): Expr[Unit] =
    context.Expr(
      Block(
        valDef("$org_scalatest_assert_macro_expr", transformAst(booleanExpr.tree)),
        callHelper(methodName, clueExpr.tree)
      )
    )
}