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

  //###################################################

  def valDef(name: String, rhs: Tree): ValDef =
    ValDef(
      Modifiers(),
      newTermName(name),
      TypeTree(),
      rhs
    )

  def binaryMacroExpression(select: Select, expressionText: String): Apply = {
    val macroExpressionClass = context.mirror.staticClass(classOf[BinaryMacroExpression].getName)
    Apply(
      Select(
        New(Ident(macroExpressionClass)),
        newTermName("<init>")
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
        ),
        context.literal(expressionText).tree
      )
    )
  }

  def binaryMacroExpression(select: Select, expressionText: String, secondArg: Tree): Apply = {
    val macroExpressionClass = context.mirror.staticClass(classOf[BinaryMacroExpression].getName)
    Apply(
      Select(
        New(Ident(macroExpressionClass)),
        newTermName("<init>")
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
        ),
        context.literal(expressionText).tree
      )
    )
  }

  def simpleMacroExpression(expression: Tree, expressionText: String): Apply = {
    val macroExpressionClass = context.mirror.staticClass(classOf[SimpleMacroExpression].getName)
    Apply(
      Select(
        New(Ident(macroExpressionClass)),
        newTermName("<init>")
      ),
      List(
        expression,
        context.literal(expressionText).tree
      )
    )
  }

  def notMacroExpression(target: Tree, expressionText: String): Apply = {
    val macroExpressionClass = context.mirror.staticClass(classOf[NotMacroExpression].getName)
    Apply(
      Select(
        New(Ident(macroExpressionClass)),
        newTermName("<init>")
      ),
      List(
        target.duplicate,
        context.literal("!").tree,
        context.literal(expressionText).tree
      )
    )
  }

  private val logicOperators = Set("&&", "||", "&", "|")
  private val newSupportedOperations = Set("==", "!=", "===", "!==", "<", ">", ">=", "<=", "&&", "||", "&", "|")

  def newIsSupported(operator: String) = newSupportedOperations.contains(operator)

  def traverseSelect(select: Select, rightExpr: Tree): (Tree, Tree) =
    if (logicOperators.contains(select.name.decoded)) {
      val leftTree =
        select.qualifier match {
          case selectApply: Apply => transformAst(selectApply.duplicate)
          case selectSelect: Select => transformAst(selectSelect.duplicate)
          case _ => select.qualifier.duplicate
        }
      val rightTree =
        rightExpr match {
          case argApply: Apply => transformAst(argApply.duplicate)
          case argSelect: Select => transformAst(argSelect.duplicate)
          case _ => rightExpr.duplicate
        }
      (leftTree, rightTree)
    }
    else
      (select.qualifier.duplicate, rightExpr.duplicate)

  def transformAst(tree: Tree): Tree =
    tree match {
      case apply: Apply if apply.args.size == 1 =>
        apply.fun match {
          case select: Select if newIsSupported(select.name.decoded) =>
            val (leftTree, rightTree) =  traverseSelect(select, apply.args(0))
            Block(
              valDef("$org_scalatest_assert_macro_left", leftTree),
              valDef("$org_scalatest_assert_macro_right", rightTree),
              binaryMacroExpression(select.duplicate, getText(tree))
            )
          case funApply: Apply if funApply.args.size == 1 => // For === and !== that takes Equality
            funApply.fun match {
              case select: Select if select.name.decoded == "===" || select.name.decoded == "!==" =>
                val (leftTree, rightTree) = traverseSelect(select, funApply.args(0))
                Block(
                  valDef("$org_scalatest_assert_macro_left", leftTree),
                  valDef("$org_scalatest_assert_macro_right", rightTree),
                  binaryMacroExpression(select.duplicate, getText(tree), apply.args(0).duplicate) // TODO: should apply.args(0) be traversed also?
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
                        binaryMacroExpression(select.duplicate, getText(tree), apply.args(0).duplicate) // TODO: should apply.args(0) be traversed also?
                      )
                    }
                    else
                      simpleMacroExpression(tree.duplicate, getText(tree))
                  case _ => simpleMacroExpression(tree.duplicate, getText(tree))
                }
              case _ => simpleMacroExpression(tree.duplicate, getText(tree))
            }
          case _ => simpleMacroExpression(tree.duplicate, getText(tree))
        }
      case select: Select if select.name.decoded == "unary_!" => // for !
        val leftTree =
          select.qualifier match {
            case selectApply: Apply => transformAst(selectApply.duplicate)
            case selectSelect: Select => transformAst(selectSelect.duplicate)
            case _ => simpleMacroExpression(select.qualifier.duplicate, getText(select.qualifier))
          }
        notMacroExpression(leftTree.duplicate, getText(tree))
      case _ => simpleMacroExpression(tree.duplicate, getText(tree))
    }

  private[this] def getPosition(expr: Tree) = expr.pos.asInstanceOf[scala.reflect.internal.util.Position]

  def getText(expr: Tree): String = {
    expr match {
      case literal: Literal =>
        val rawText = getPosition(expr) match {
          case p: scala.reflect.internal.util.RangePosition => context.echo(expr.pos, "RangePosition found!"); p.lineContent.slice(p.start, p.end).trim
          case p: reflect.internal.util.Position => p.lineContent.trim
        }
        if (rawText.startsWith("assert("))
          rawText.substring(7, rawText.length - 1)
        else
          rawText
      case _ => show(expr)
    }
  }

  def macroAssert: Apply =
    Apply(
      Select(
        Ident(newTermName(helperName)),
        newTermName("macroAssert")
      ),
      List(Ident(newTermName("$org_scalatest_assert_macro_expr")))
    )

  def genAssert(booleanExpr: Expr[Boolean]): Expr[Unit] =
    context.Expr(
      Block(
        valDef("$org_scalatest_assert_macro_expr", transformAst(booleanExpr.tree)),
        macroAssert
      )
    )

  // ############################# Fact

  def binaryMacroFact(select: Select): Apply = {
    val macroFactClass = context.mirror.staticClass(classOf[BinaryMacroFact].getName)
    Apply(
      Select(
        New(Ident(macroFactClass)),
        newTermName("<init>")
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
  }

  def binaryMacroFact(select: Select, secondArg: Tree): Apply = {
    val macroFactClass = context.mirror.staticClass(classOf[BinaryMacroFact].getName)
    Apply(
      Select(
        New(Ident(macroFactClass)),
        newTermName("<init>")
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
  }

  def simpleMacroFact(expression: Tree, expressionText: String): Apply = {
    val macroFactClass = context.mirror.staticClass(classOf[SimpleMacroFact].getName)
    Apply(
      Select(
        New(Ident(macroFactClass)),
        newTermName("<init>")
      ),
      List(
        expression,
        context.literal(expressionText).tree
      )
    )
  }

  def notMacroFact(target: Tree): Apply = {
    val macroFactClass = context.mirror.staticClass(classOf[NotMacroFact].getName)
    Apply(
      Select(
        New(Ident(macroFactClass)),
        newTermName("<init>")
      ),
      List(
        target.duplicate
      )
    )
  }

  def traverseFactSelect(select: Select, rightExpr: Tree): (Tree, Tree) =
    if (logicOperators.contains(select.name.decoded)) {
      val leftTree =
        select.qualifier match {
          case selectApply: Apply => transformFactAst(selectApply.duplicate)
          case selectSelect: Select => transformFactAst(selectSelect.duplicate)
          case _ => select.qualifier.duplicate
        }
      val rightTree =
        rightExpr match {
          case argApply: Apply => transformFactAst(argApply.duplicate)
          case argSelect: Select => transformFactAst(argSelect.duplicate)
          case _ => rightExpr.duplicate
        }
      (leftTree, rightTree)
    }
    else
      (select.qualifier.duplicate, rightExpr.duplicate)

  def transformFactAst(tree: Tree): Tree =
    tree match {
      case apply: Apply if apply.args.size == 1 =>
        apply.fun match {
          case select: Select if newIsSupported(select.name.decoded) =>
            val (leftTree, rightTree) =  traverseFactSelect(select, apply.args(0))
            Block(
              valDef("$org_scalatest_assert_macro_left", leftTree),
              valDef("$org_scalatest_assert_macro_right", rightTree),
              binaryMacroFact(select.duplicate)
            )
          case funApply: Apply if funApply.args.size == 1 => // For === and !== that takes Equality
            funApply.fun match {
              case select: Select if select.name.decoded == "===" || select.name.decoded == "!==" =>
                val (leftTree, rightTree) = traverseFactSelect(select, funApply.args(0))
                Block(
                  valDef("$org_scalatest_assert_macro_left", leftTree),
                  valDef("$org_scalatest_assert_macro_right", rightTree),
                  binaryMacroFact(select.duplicate, apply.args(0).duplicate) // TODO: should apply.args(0) be traversed also?
                )
              case typeApply: TypeApply =>
                typeApply.fun match {
                  case select: Select if typeApply.args.size == 1 => // For TypeCheckedTripleEquals
                    val operator: String = select.name.decoded
                    if (operator == "===" || operator == "!==") {
                      val (leftTree, rightTree) = traverseFactSelect(select, funApply.args(0))
                      Block(
                        valDef("$org_scalatest_assert_macro_left", leftTree),
                        valDef("$org_scalatest_assert_macro_right", rightTree),
                        binaryMacroFact(select.duplicate, apply.args(0).duplicate) // TODO: should apply.args(0) be traversed also?
                      )
                    }
                    else
                      simpleMacroFact(tree.duplicate, getText(tree))
                  case _ => simpleMacroFact(tree.duplicate, getText(tree))
                }
              case _ => simpleMacroFact(tree.duplicate, getText(tree))
            }
          case _ => simpleMacroFact(tree.duplicate, getText(tree))
        }
      case select: Select if select.name.decoded == "unary_!" => // for !
        val leftTree =
          select.qualifier match {
            case selectApply: Apply => transformFactAst(selectApply.duplicate)
            case selectSelect: Select => transformFactAst(selectSelect.duplicate)
            case _ => simpleMacroFact(select.qualifier.duplicate, getText(select.qualifier))
          }
        notMacroFact(leftTree.duplicate)
      case _ => simpleMacroFact(tree.duplicate, getText(tree))
    }

  def macroFactAssert: Apply =
    Apply(
      Select(
        Ident(newTermName(helperName)),
        newTermName("macroAssert")
      ),
      List(Ident(newTermName("$org_scalatest_assert_macro_expr")))
    )

  def genFactAssert(booleanExpr: Expr[Boolean]): Expr[Unit] =
    context.Expr(
      Block(
        valDef("$org_scalatest_assert_macro_expr", transformFactAst(booleanExpr.tree)),
        macroFactAssert
      )
    )

  // ############################# Fact
}