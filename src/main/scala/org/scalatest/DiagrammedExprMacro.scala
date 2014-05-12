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
import org.scalactic.MacroOwnerRepair

private[org] class DiagrammedExprMacro[C <: Context](val context: C, helperName: String) {

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

  // this is taken from expecty
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

  // this is taken from expecty and modified
  private[this] def getAnchor(expr: Tree): Int = expr match {
    case Apply(x, ys) => getAnchor(x) + 0
    case TypeApply(x, ys) => getAnchor(x) + 0
    case _ => {
      getPosition(expr) match {
        case NoPosition => -1
        case pos => pos.point - pos.source.lineToOffset(pos.line - 1)
      }
    }
  }

  def list(elements: List[Tree]): Tree =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(newTermName("scala")),
              newTermName("collection")
            ),
            newTermName("immutable")
          ),
          newTermName("List")
        ),
        newTermName("apply")
      ),
      elements
    )

  def simpleExpr(tree: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalatest")
          ),
          newTermName("DiagrammedExpr")
        ),
        newTermName("simpleExpr")
      ),
      List(
        tree,
        Literal(Constant(getAnchor(tree)))
      )
    )

  case class ApplyInfo(select: Select, applyList: List[GenericApply])

  private def traverseApply(apply: GenericApply, accApplyList: List[GenericApply] = List.empty): ApplyInfo =
    apply.fun match {
      case select: Select => ApplyInfo(select, apply :: accApplyList)
      case funApply: GenericApply => traverseApply(funApply, apply :: accApplyList)
    }

  private def recursiveValueApply(applyList: List[GenericApply], currentApply: GenericApply): GenericApply =
    applyList match {
      case TypeApply(fun, args) :: tail =>
        recursiveValueApply(tail, TypeApply(currentApply, args))
      case Apply(fun, args) :: tail =>
        recursiveValueApply(tail, Apply(currentApply, args))
      case Nil => currentApply
    }

  def selectExpr(select: Select): Tree = {
    val qualifierValDef: Tree = valDef("$org_scalatest_macro_qualifier", transformAst(select.qualifier))

    val valueExpr =
      Select(
        Select(Ident(newTermName("$org_scalatest_macro_qualifier")), newTermName("value")),
        select.name
      )

    val resultExpr: Tree =
      Apply(
        Select(
          Select(
            Select(
              Ident(newTermName("org")),
              newTermName("scalatest")
            ),
            newTermName("DiagrammedExpr")
          ),
          newTermName("selectExpr")
        ),
        List(
          Ident(newTermName("$org_scalatest_macro_qualifier")),
          valueExpr,
          Literal(Constant(getAnchor(select)))
        )
      )

    val exprList: List[Tree] = List(qualifierValDef, resultExpr)

    Block(exprList: _*)
  }

  private def applyExpr(apply: GenericApply): Tree = {
    val applyInfo = traverseApply(apply)
    val qualifierValDef: Tree = valDef("$org_scalatest_macro_qualifier", transformAst(applyInfo.select.qualifier))

    val argsValDefList: List[ValDef] =
      applyInfo.applyList.zipWithIndex.flatMap { case (currentApply, i) =>
        val base = i * 100  // should be ok as maximum function arguments is 22
        currentApply match {
          case Apply(fun, args) =>
            args.zipWithIndex.map { case (arg, j) =>
              arg match {
                case func: Function => valDef("$org_scalatest_macro_arg_" + (base + j), simpleExpr(Literal(Constant("")))) // ignore function, create a dummy val.
                case byName if arg.tpe.typeSymbol.fullName == "scala.Nothing" => valDef("$org_scalatest_macro_arg_" + (base + j), simpleExpr(Literal(Constant("")))) // TODO: Is there a better way to detect a by-name?
                case other => valDef("$org_scalatest_macro_arg_" + (base + j), transformAst(arg))
              }
            }
          case _ => List.empty
        }
      }

    val substitutedArgsList: List[List[Tree]] =
      applyInfo.applyList.zipWithIndex.map { case (currentApply, i) =>
        val base = i * 100  // should be ok as maximum function arguments is 22
        currentApply match {
          case Apply(fun, args) =>
            args.zipWithIndex.map { case (arg, j) =>
              arg match {
                case func: Function => func  // for functions, just use back the original
                case byName if arg.tpe.typeSymbol.fullName == "scala.Nothing" => byName // for by-names, just use back the original
                case other => Select(Ident(newTermName("$org_scalatest_macro_arg_" + (base + j))), newTermName("value"))
              }
            }
          case _ => currentApply.args
        }

      }

    val currentValueApply =
      applyInfo.applyList.head match {
        case typeApply: TypeApply =>
          TypeApply(
            Select(
              Select(Ident(newTermName("$org_scalatest_macro_qualifier")), newTermName("value")),
              applyInfo.select.name
            ),
            applyInfo.applyList.head.args
          )
        case _ =>
          Apply(
            Select(
              Select(Ident(newTermName("$org_scalatest_macro_qualifier")), newTermName("value")),
              applyInfo.select.name
            ),
            applyInfo.applyList.head.args
          )
      }

    val valueExpr = recursiveValueApply(applyInfo.applyList.tail, currentValueApply)

    val argIdents: List[Tree] =
      argsValDefList.map { valDef =>
        Ident(valDef.name)
      }

    val resultExpr: Tree =
      Apply(
        Select(
          Select(
            Select(
              Ident(newTermName("org")),
              newTermName("scalatest")
            ),
            newTermName("DiagrammedExpr")
          ),
          newTermName("applyExpr")
        ),
        List(
          Ident(newTermName("$org_scalatest_macro_qualifier")),
          list(argIdents),
          valueExpr,
          Literal(Constant(getAnchor(apply)))
        )
      )

    val exprList: List[Tree] = {
      val funcName = applyInfo.select.name.decoded
      if ((funcName == "&&" || funcName == "&") && argIdents.length == 1) {
        // &&, try to be lazy
        val ifCheck =
          If(
            Select(
              Ident(newTermName("$org_scalatest_macro_qualifier")),
              newTermName("value")
            ),
            Block((argsValDefList ::: List(resultExpr)): _*),
            Ident(newTermName("$org_scalatest_macro_qualifier"))
          )
        List(qualifierValDef, ifCheck)
      }
      else if ((funcName == "||" || funcName == "|") && argIdents.length == 1) {
        // ||, try to be lazy
        val ifCheck =
          If(
            Select(
              Ident(newTermName("$org_scalatest_macro_qualifier")),
              newTermName("value")
            ),
            Ident(newTermName("$org_scalatest_macro_qualifier")),
            Block((argsValDefList ::: List(resultExpr)): _*)
          )
        List(qualifierValDef, ifCheck)
      }
      else
        qualifierValDef :: argsValDefList ::: List(resultExpr)
    }

    Block(exprList: _*)
  }

  def transformAst(tree: Tree): Tree = {
    tree match {
      case apply: GenericApply => applyExpr(apply)
      case Select(This(_), _) => simpleExpr(tree)
      case x: Select if x.symbol.isModule => simpleExpr(tree) // don't traverse packages
      case select: Select => selectExpr(select)
      case Block(stats, expr) => transformAst(expr)
      case other => simpleExpr(other)
    }
  }

  // Generate AST for:
  // helper.methodName(expression, clue)
  def callHelper(methodName: String, clueTree: Tree, sourceText: String): Apply =
    Apply(
      Select(
        Ident(newTermName(helperName)),
        newTermName(methodName)
      ),
      List(Ident(newTermName("$org_scalatest_assert_macro_expr")), clueTree, Literal(Constant(sourceText)))
    )

  def genMacro(booleanExpr: Expr[Boolean], methodName: String, clueExpr: Expr[Any], sourceText: String): Expr[Unit] = {
    val ownerRepair = new MacroOwnerRepair[context.type](context)
    val expandedCode =
      context.Expr(
        Block(
          valDef("$org_scalatest_assert_macro_expr", transformAst(booleanExpr.tree)),
          callHelper(methodName, clueExpr.tree, sourceText)
        )
      )
    ownerRepair.repairOwners(expandedCode)
  }
}