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

private[org] class DiagrammedBooleanMacro[C <: Context](val context: C, helperName: String) {

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

  private val supportedBinaryOperations =
    Set(
      "==",
      "!=",
      "===",
      "!==",
      "<",
      ">",
      ">=",
      "<=",
      "startsWith",
      "endsWith",
      "contains",
      "eq",
      "ne",
      "exists") ++ logicOperators

  private val supportedUnaryOperations =
    Set(
      "unary_!",
      "isEmpty"
    )

  private val lengthSizeOperations =
    Set(
      "length",
      "size"
    )

  def isSupportedBinaryOperator(operator: String) = supportedBinaryOperations.contains(operator)

  def isSupportedUnaryOperator(operator: String) = supportedUnaryOperations.contains(operator)

  def isSupportedLengthSizeOperator(operator: String) = lengthSizeOperations.contains(operator)

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

  def binaryMacroBool(select: Select, leftAnchor: Int, rightAnchor: Int): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("DiagrammedBool")
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
        ),
        context.literal(leftAnchor).tree,
        context.literal(rightAnchor).tree
      )
    )

  def typedBinaryMacroBool(select: Select, typeArgs: List[Tree], leftAnchor: Int, rightAnchor: Int): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("DiagrammedBool")
        ),
        newTermName("binaryMacroBool")
      ),
      List(
        Ident(newTermName("$org_scalatest_assert_macro_left")),
        context.literal(select.name.decoded).tree,
        Ident(newTermName("$org_scalatest_assert_macro_right")),
        Apply(
          TypeApply(
            Select(
              Ident(newTermName("$org_scalatest_assert_macro_left")),
              select.name
            ),
            typeArgs
          ),
          List(Ident(newTermName("$org_scalatest_assert_macro_right")))
        ),
        context.literal(leftAnchor).tree,
        context.literal(rightAnchor).tree
      )
    )

  def binaryMacroBool(select: Select, secondArg: Tree, leftAnchor: Int, rightAnchor: Int): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("DiagrammedBool")
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
        ),
        context.literal(leftAnchor).tree,
        context.literal(rightAnchor).tree
      )
    )

  def simpleMacroBool(expression: Tree, expressionText: String, anchor: Int): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("DiagrammedBool")
        ),
        newTermName("simpleMacroBool")
      ),
      List(
        expression,
        context.literal(expressionText).tree,
        context.literal(anchor).tree
      )
    )

  def notBool(target: Tree, anchor: Int): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("DiagrammedBool")
        ),
        newTermName("notBool")
      ),
      List(
        target.duplicate,
        context.literal(anchor).tree
      )
    )

  def unaryMacroBool(select: Select, leftAnchor: Int, anchor: Int): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("DiagrammedBool")
        ),
        newTermName("unaryMacroBool")
      ),
      List(
        Ident(newTermName("$org_scalatest_assert_macro_left")),
        context.literal(select.name.decoded).tree,
        Select(
          Ident(newTermName("$org_scalatest_assert_macro_left")),
          select.name
        ),
        context.literal(leftAnchor).tree,
        context.literal(anchor).tree
      )
    )

  def isInstanceOfMacroBool(select: Select, className: String, typeArg: Tree, leftAnchor: Int, anchor: Int): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("DiagrammedBool")
        ),
        newTermName("isInstanceOfMacroBool")
      ),
      List(
        Ident(newTermName("$org_scalatest_assert_macro_left")),
        context.literal(select.name.decoded).tree,
        context.literal(className).tree,
        TypeApply(
          Select(
            Ident(newTermName("$org_scalatest_assert_macro_left")),
            select.name
          ),
          List(typeArg)
        ),
        context.literal(leftAnchor).tree,
        context.literal(anchor).tree
      )
    )

  def lengthSizeMacroBool(select: Select, leftAnchor: Int, rightAnchor: Int): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("DiagrammedBool")
        ),
        newTermName("lengthSizeMacroBool")
      ),
      List(
        Ident(newTermName("$org_scalatest_assert_macro_left")),
        context.literal(select.name.decoded).tree,
        Select(
          Ident("$org_scalatest_assert_macro_left"),
          select.name
        ),
        Ident(newTermName("$org_scalatest_assert_macro_right")),
        context.literal(leftAnchor).tree,
        context.literal(rightAnchor).tree
      )
    )

  def existsMacroBool(select: Select, func: Function, leftAnchor: Int, anchor: Int): Apply =
    Apply(
      Select(
        Select(
          Select(
            Ident(newTermName("org")),
            newTermName("scalautils")
          ),
          newTermName("DiagrammedBool")
        ),
        newTermName("existsMacroBool")
      ),
      List(
        Ident(newTermName("$org_scalatest_assert_macro_left")),
        Ident(newTermName("$org_scalatest_assert_macro_right")),
        Apply(
          Select(
            Ident(newTermName("$org_scalatest_assert_macro_left")),
            select.name
          ),
          List(
            func
          )
        ),
        context.literal(leftAnchor).tree,
        context.literal(anchor).tree
      )
    )

  def traverseSelect(select: Select, rightExpr: Tree): (Tree, Tree) = {
    val operator = select.name.decoded
    if (logicOperators.contains(operator)) {
      val leftTree =
        select.qualifier match {
          case selectApply: Apply => transformAst(selectApply.duplicate)
          case selectSelect: Select => transformAst(selectSelect.duplicate)
          case _ => simpleMacroBool(select.qualifier.duplicate, getText(select.qualifier), getAnchor(select.qualifier))
        }
      val rightTree = {
        val evalBlock =
          rightExpr match {
            case argApply: Apply => transformAst(argApply.duplicate)
            case argSelect: Select => transformAst(argSelect)
            case argTypeApply: TypeApply => transformAst(argTypeApply.duplicate)
            case _ => simpleMacroBool(rightExpr.duplicate, getText(rightExpr), getAnchor(rightExpr))
          }
        if (operator == "&&" || operator == "&")  {// generate if (left.value) {...} else false
          If(
            Select(
              Ident(newTermName("$org_scalatest_assert_macro_left")),
              newTermName("value")
            ),
            evalBlock,
            simpleMacroBool(context.literal(false).tree, "", -1)
          )
        }
        else if (operator == "||" || operator == "|") // || and |, generate if (left.value) true else {...}
          If(
            Select(
              Ident(newTermName("$org_scalatest_assert_macro_left")),
              newTermName("value")
            ),
            simpleMacroBool(context.literal(true).tree, "", -1),
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

  private def isPlaceHolder(tree: Tree): Boolean =
    tree match {
      case valDef: ValDef => valDef.rhs == EmptyTree
      case _ => false
    }

  private[this] def getAnchor(expr: Tree): Int = expr match {
    case Apply(x, ys) => getAnchor(x) + 0
    case TypeApply(x, ys) => getAnchor(x) + 0
    case _ => {
      val pos = getPosition(expr)
      pos.point - pos.source.lineToOffset(pos.line - 1)
    }
  }

  def transformAst(tree: Tree): Tree = {
    tree match {
      case apply: Apply if apply.args.size == 1 =>
        apply.fun match {
          case select: Select if isSupportedBinaryOperator(select.name.decoded) =>
            val operator = select.name.decoded
            val (leftTree, rightTree) =  traverseSelect(select, apply.args(0))
            operator match {
              case "==" =>
                leftTree match {
                  case leftApply: Apply =>
                    leftApply.fun match {
                      case leftApplySelect: Select if isSupportedLengthSizeOperator(leftApplySelect.name.decoded) && leftApply.args.size == 0 => // support for a.length == xxx, a.size == xxxx
                        Block(
                          valDef("$org_scalatest_assert_macro_left", leftApplySelect.qualifier.duplicate),
                          valDef("$org_scalatest_assert_macro_right", rightTree),
                          lengthSizeMacroBool(leftApplySelect.duplicate, getAnchor(leftApplySelect.qualifier), getAnchor(rightTree))
                        )
                      case _ =>
                        Block(
                          valDef("$org_scalatest_assert_macro_left", leftTree),
                          valDef("$org_scalatest_assert_macro_right", rightTree),
                          binaryMacroBool(select.duplicate, getAnchor(leftTree), getAnchor(rightTree))
                        )
                    }

                  case leftSelect: Select if isSupportedLengthSizeOperator(leftSelect.name.decoded) => // support for a.length == xxx, a.size == xxxx
                    Block(
                      valDef("$org_scalatest_assert_macro_left", leftSelect.qualifier.duplicate),
                      valDef("$org_scalatest_assert_macro_right", rightTree),
                      lengthSizeMacroBool(leftSelect.duplicate, getAnchor(leftSelect.qualifier), getAnchor(rightTree))
                    )

                  case _ =>
                    Block(
                      valDef("$org_scalatest_assert_macro_left", leftTree),
                      valDef("$org_scalatest_assert_macro_right", rightTree),
                      binaryMacroBool(select.duplicate, getAnchor(leftTree), getAnchor(rightTree))
                    )
                }

              case "exists" =>
                rightTree match {
                  case func: Function if func.children.size == 2 && isPlaceHolder(func.children(0)) =>
                    val boolExpr = func.children(1)
                    boolExpr match {
                      case boolExprApply: Apply if boolExprApply.args.size == 1 =>
                        boolExprApply.fun match {
                          case boolExprApplySelect: Select if boolExprApplySelect.name.decoded == "==" =>
                            Block(
                              valDef("$org_scalatest_assert_macro_left", leftTree),
                              valDef("$org_scalatest_assert_macro_right", boolExprApply.args(0).duplicate),
                              existsMacroBool(select.duplicate, func.duplicate, getAnchor(leftTree), getAnchor(tree))
                            )
                          case _ => simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
                        }
                      case _ => simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
                    }

                  case _ => simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
                }

              case _ =>
                Block(
                  valDef("$org_scalatest_assert_macro_left", leftTree),
                  valDef("$org_scalatest_assert_macro_right", rightTree),
                  binaryMacroBool(select.duplicate, getAnchor(leftTree), getAnchor(rightTree))
                )
            }

          case funApply: Apply if funApply.args.size == 1 => // For === and !== that takes Equality
            funApply.fun match {
              case select: Select if select.name.decoded == "===" || select.name.decoded == "!==" =>
                val (leftTree, rightTree) = traverseSelect(select, funApply.args(0))
                Block(
                  valDef("$org_scalatest_assert_macro_left", leftTree),
                  valDef("$org_scalatest_assert_macro_right", rightTree),
                  binaryMacroBool(select.duplicate, apply.args(0).duplicate, getAnchor(leftTree), getAnchor(rightTree)) // TODO: should apply.args(0) be traversed also?
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
                        binaryMacroBool(select.duplicate, apply.args(0).duplicate, getAnchor(leftTree), getAnchor(rightTree)) // TODO: should apply.args(0) be traversed also?
                      )
                    }
                    else
                      simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
                  case _ => simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
                }
              case _ => simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
            }

          case funTypeApply: TypeApply =>
            funTypeApply.fun match {
              case select: Select if isSupportedBinaryOperator(select.name.decoded) =>
                val (leftTree, rightTree) = traverseSelect(select, apply.args(0))
                Block(
                  valDef("$org_scalatest_assert_macro_left", leftTree),
                  valDef("$org_scalatest_assert_macro_right", rightTree),
                  typedBinaryMacroBool(select.duplicate, funTypeApply.args, getAnchor(leftTree), getAnchor(rightTree))
                )

              case _ => simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
            }

          case _ => simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
        }
      case apply: Apply if apply.args.size == 0 => // for unary operation that takes 0 arguments
        apply.fun match {
          case select: Select if isSupportedUnaryOperator(select.name.decoded) =>
            Block(
              valDef("$org_scalatest_assert_macro_left", select.qualifier.duplicate),
              unaryMacroBool(select.duplicate, getAnchor(select.qualifier), getAnchor(select))
            )
          case _ => simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
        }
      case typeApply: TypeApply if typeApply.args.length == 1 => // for isInstanceOf
        typeApply.fun match {
          case select: Select =>
            val operator = select.name.decoded
            if (operator == "isInstanceOf") {
              typeApply.args(0).tpe match {
                case typeRef: TypeRef =>
                  Block(
                    valDef("$org_scalatest_assert_macro_left", select.qualifier.duplicate),
                    isInstanceOfMacroBool(select.duplicate, typeRef.sym.fullName, typeApply.args(0).duplicate, getAnchor(select.qualifier), getAnchor(typeApply.args(0)))
                  )
                case _ => simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
              }
            }
            else
              simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
          case _ => simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
        }
      case select: Select if supportedUnaryOperations.contains(select.name.decoded) => // for ! and unary operation that does not take any arguments
        if (select.name.decoded == "unary_!") {
          val leftTree =
            select.qualifier match {
              case selectApply: Apply => transformAst(selectApply.duplicate)
              case selectSelect: Select => transformAst(selectSelect.duplicate)
              case selectTypeApply: TypeApply => transformAst(selectTypeApply.duplicate)
              case _ => simpleMacroBool(select.qualifier.duplicate, getText(select.qualifier), getAnchor(select.qualifier))
            }
          notBool(leftTree.duplicate, getAnchor(leftTree))
        }
        else {
          Block(
            valDef("$org_scalatest_assert_macro_left", select.qualifier.duplicate),
            unaryMacroBool(select.duplicate, getAnchor(select.qualifier), getAnchor(select))
          )
        }
      case _ =>
        simpleMacroBool(tree.duplicate, getText(tree), getAnchor(tree))
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
      List(Ident(newTermName("$org_scalatest_assert_macro_expr")), clueTree, context.literal(sourceText).tree)
    )

  // This is needed to repair owner chain as encountered in the following issue:
  // https://github.com/scalatest/scalatest/issues/276
  class OwnerRepair[C <: reflect.macros.Context with Singleton](val c: C) {
    /**
     * If macro arguments are spliced into underneath DefTree that introduces
     * an entry into the symbol ownership chain, any symbols defined in the
     * spliced tree will be ill-owned.
     *
     * This method detects this situation, and corrects the owners.
     */
    def repairOwners[A](expr: c.Expr[A]): c.Expr[A] = {
      val symtab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
      val utils = new Utils[symtab.type](symtab)

      // Proactively typecheck the tree. This will assign symbols to
      // DefTrees introduced by the macro.
      val typed = c.typeCheck(expr.tree).asInstanceOf[symtab.Tree]

      // The current owner at the call site. Symbols owned by this may need
      // to be transplanted.
      import scala.reflect.macros.runtime.{Context => MRContext}
      val callsiteOwner =
        c.asInstanceOf[MRContext]
          .callsiteTyper.context.owner
          .asInstanceOf[symtab.Symbol]

      val repairedTree = utils.repairOwners(typed, callsiteOwner)
      c.Expr[A](repairedTree.asInstanceOf[c.universe.Tree])
    }

    private class Utils[U <: reflect.internal.SymbolTable](val u: U) {
      import u._

      class ChangeOwnerAndModuleClassTraverser(oldowner: Symbol, newowner: Symbol)
        extends ChangeOwnerTraverser(oldowner, newowner) {

        override def traverse(tree: Tree) {
          tree match {
            case _: DefTree => change(tree.symbol.moduleClass)
            case _          =>
          }
          super.traverse(tree)
        }
      }

      def repairOwners(t: Tree, macroCallSiteOwner: Symbol): Tree = {
        object repairer extends Transformer {
          override def transform(t: Tree): Tree = {
            t match {
              case (_: DefTree | _: Function | _: Import) if t.symbol.owner == macroCallSiteOwner && macroCallSiteOwner != currentOwner =>
                new ChangeOwnerAndModuleClassTraverser(macroCallSiteOwner, currentOwner)(t)
              case _ =>
                super.transform(t)
            }
          }
        }
        repairer.atOwner(macroCallSiteOwner) {
          repairer transform t
        }
      }
    }
  }

  def genMacro(booleanExpr: Expr[Boolean], methodName: String, clueExpr: Expr[Any], sourceText: String): Expr[Unit] = {
    val ownerRepair = new OwnerRepair[context.type](context)
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