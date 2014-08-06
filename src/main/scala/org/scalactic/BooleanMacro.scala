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
package org.scalactic

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
   *   val $org_scalactic_assert_macro_left = something.aMethod
   *   val $org_scalactic_assert_macro_right = 3
   *   val $org_scalactic_assert_macro_result = $org_scalactic_assert_macro_left ==  $org_scalactic_assert_macro_result
   *   assertionsHelper.macroAssert($org_scalactic_assert_macro_left, "==", $org_scalactic_assert_macro_right, $org_scalactic_assert_macro_result, None)
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

  /**
   * Get text of the expression, current it'll try to look for RangePosition (only available when -Yrange.pos is enabled),
   * else it will just call show to get the text.
   */
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

  /**
   * For binary method call, for example left.aMethodCall(right), it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.binaryMacroBool($org_scalatest_assert_macro_left, "aMethodCall", $org_scalatest_assert_macro_right, $org_scalatest_assert_macro_left.aMethodCall($org_scalatest_assert_macro_right))
   */
  def binaryMacroBool(select: Select): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(newTermName("_root_")),
              newTermName("org")
            ),
            newTermName("scalactic")
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

  /**
   * For === and !== method call that takes Equality, for example left === right, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.binaryMacroBool($org_scalatest_assert_macro_left, "===", $org_scalatest_assert_macro_right, $org_scalatest_assert_macro_left.===($org_scalatest_assert_macro_right)(defaultEquality))
   */
  def binaryMacroBool(select: Select, secondArg: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(newTermName("_root_")),
              newTermName("org")
            ),
            newTermName("scalactic")
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

  /**
   * For === and !== method call that takes Equality and uses TypeCheckedTripleEquals , for example left === right, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.binaryMacroBool($org_scalatest_assert_macro_left, "===", $org_scalatest_assert_macro_right, $org_scalatest_assert_macro_left.===[R]($org_scalatest_assert_macro_right)(defaultEquality))
   */
  def typedBinaryMacroBool(select: Select, typeArgs: List[Tree]): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(newTermName("_root_")),
              newTermName("org")
            ),
            newTermName("scalactic")
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
          TypeApply(
            Select(
              Ident(newTermName("$org_scalatest_assert_macro_left")),
              select.name
            ),
            typeArgs
          ),
          List(Ident(newTermName("$org_scalatest_assert_macro_right")))
        )
      )
    )

  /**
   * For unrecognized expression, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.simpleMacroBool(expression, expressionText)  // expressionText is currently the text returned from 'show'
   */
  def simpleMacroBool(expression: Tree, expressionText: String): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(newTermName("_root_")),
              newTermName("org")
            ),
            newTermName("scalactic")
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

  /**
   * For !a unary_! call, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.notBool(a)
   */
  def notBool(target: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(newTermName("_root_")),
              newTermName("org")
            ),
            newTermName("scalactic")
          ),
          newTermName("Bool")
        ),
        newTermName("notBool")
      ),
      List(
        target.duplicate
      )
    )

  /**
   * For a.isEmpty and method call that does not has any argument, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.unaryMacroBool($org_scalatest_assert_macro_left, "isEmpty", $org_scalatest_assert_macro_left.isEmpty)
   */
  def unaryMacroBool(select: Select): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(newTermName("_root_")),
              newTermName("org")
            ),
            newTermName("scalactic")
          ),
          newTermName("Bool")
        ),
        newTermName("unaryMacroBool")
      ),
      List(
        Ident(newTermName("$org_scalatest_assert_macro_left")),
        context.literal(select.name.decoded).tree,
        Select(
          Ident(newTermName("$org_scalatest_assert_macro_left")),
          select.name
        )
      )
    )

  /**
   * For a.isInstanceOf[String] == 1, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.lengthSizeMacroBool($org_scalatest_assert_macro_left, "==", $org_scalatest_assert_macro_left.length, 1)
   */
  def isInstanceOfMacroBool(select: Select, className: String, typeArg: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(newTermName("_root_")),
              newTermName("org")
            ),
            newTermName("scalactic")
          ),
          newTermName("Bool")
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
        )
      )
    )

  /**
   * For a.length == 1, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.lengthSizeMacroBool($org_scalatest_assert_macro_left, "==", $org_scalatest_assert_macro_left.length, 1)
   */
  def lengthSizeMacroBool(select: Select): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(newTermName("_root_")),
              newTermName("org")
            ),
            newTermName("scalactic")
          ),
          newTermName("Bool")
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
        Ident(newTermName("$org_scalatest_assert_macro_right"))
      )
    )

  /**
   * generate the AST for the following code:
   *
   * org.scalactic.Bool.existsMacroBool($org_scalatest_assert_macro_left, $org_scalatest_assert_macro_right, $org_scalatest_assert_macro_left.exists(func))
   */
  def existsMacroBool(select: Select, func: Function): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(newTermName("_root_")),
              newTermName("org")
            ),
            newTermName("scalactic")
          ),
          newTermName("Bool")
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
        )
      )
    )

  // traverse the given Select, both qualifier and the right expression.
  def traverseSelect(select: Select, rightExpr: Tree): (Tree, Tree) = {
    val operator = select.name.decoded
    if (logicOperators.contains(operator)) {
      val leftTree = // traverse the qualifier
        select.qualifier match {
          case selectApply: Apply => transformAst(selectApply.duplicate)
          case selectSelect: Select => transformAst(selectSelect.duplicate)
          case _ => simpleMacroBool(select.qualifier.duplicate, getText(select.qualifier))
        }
      val rightTree = {
        val evalBlock =
          rightExpr match {
            case argApply: Apply => transformAst(argApply.duplicate)
            case argSelect: Select => transformAst(argSelect)
            case argTypeApply: TypeApply => transformAst(argTypeApply.duplicate)
            case _ => simpleMacroBool(rightExpr.duplicate, getText(rightExpr))
          }
        if (operator == "&&" || operator == "&")  {// generate if (left.value) {...} else false
          If(
            Select(
              Ident(newTermName("$org_scalatest_assert_macro_left")),
              newTermName("value")
            ),
            evalBlock,
            simpleMacroBool(context.literal(false).tree, "")
          )
        }
        else if (operator == "||" || operator == "|") // || and |, generate if (left.value) true else {...}
          If(
            Select(
              Ident(newTermName("$org_scalatest_assert_macro_left")),
              newTermName("value")
            ),
            simpleMacroBool(context.literal(true).tree, ""),
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

  // helper method to check if the given tree represent a place holder syntax
  private def isPlaceHolder(tree: Tree): Boolean =
    tree match {
      case valDef: ValDef => valDef.rhs == EmptyTree
      case _ => false
    }

  /**
   * Transform the passed in boolean expression, see comment in different case for details
   */
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
                      case leftApplySelect: Select if isSupportedLengthSizeOperator(leftApplySelect.name.decoded) && leftApply.args.size == 0 =>
                        /**
                         * support for a.length() == xxx, a.size() == xxxx
                         *
                         * generate AST for the following code:
                         *
                         * {
                         *   val $org_scalatest_assert_macro_left = {qualifier of the method call}
                         *   val $org_scalatest_assert_macro_right = {first argument}
                         *   [code generated from lengthSizeMacroBool]
                         * }
                         */
                        Block(
                          valDef("$org_scalatest_assert_macro_left", leftApplySelect.qualifier.duplicate),
                          valDef("$org_scalatest_assert_macro_right", rightTree),
                          lengthSizeMacroBool(leftApplySelect.duplicate)
                        )
                      case _ =>
                        /**
                         * something else but still a binary macro bool, generate AST for the following code:
                         *
                         * {
                         *   val $org_scalatest_assert_macro_left = {qualifier of the method call}
                         *   val $org_scalatest_assert_macro_right = {first argument}
                         *   [code generated from binaryMacroBool]
                         * }
                         */
                        Block(
                          valDef("$org_scalatest_assert_macro_left", leftTree),
                          valDef("$org_scalatest_assert_macro_right", rightTree),
                          binaryMacroBool(select.duplicate)
                        )
                    }

                  case leftSelect: Select if isSupportedLengthSizeOperator(leftSelect.name.decoded) =>
                    /**
                     * support for a.length == xxx, a.size == xxxx
                     *
                     * generate AST for the following code:
                     *
                     * {
                     *   val $org_scalatest_assert_macro_left = {qualifier of the method call}
                     *   val $org_scalatest_assert_macro_right = {first argument}
                     *   [code generated from binaryMacroBool]
                     * }
                     */
                    Block(
                      valDef("$org_scalatest_assert_macro_left", leftSelect.qualifier.duplicate),
                      valDef("$org_scalatest_assert_macro_right", rightTree),
                      lengthSizeMacroBool(leftSelect.duplicate)
                    )

                  case _ =>
                    /**
                     * something else but still a binary macro bool, generate AST for the following code:
                     *
                     * {
                     *   val $org_scalatest_assert_macro_left = {qualifier of the method call}
                     *   val $org_scalatest_assert_macro_right = {first argument}
                     *   [code generated from binaryMacroBool]
                     * }
                     */
                    Block(
                      valDef("$org_scalatest_assert_macro_left", leftTree),
                      valDef("$org_scalatest_assert_macro_right", rightTree),
                      binaryMacroBool(select.duplicate)
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
                            /**
                             * support for a.exists(_ == 2), generate AST for the following code:
                             *
                             * {
                             *   val $org_scalatest_assert_macro_left = a
                             *   val $org_scalatest_assert_macro_right = _ == 2  // after desugar
                             *   [code generated from existsMacroBool]
                             * }
                             */
                            Block(
                              valDef("$org_scalatest_assert_macro_left", leftTree),
                              valDef("$org_scalatest_assert_macro_right", boolExprApply.args(0).duplicate),
                              existsMacroBool(select.duplicate, func.duplicate)
                            )
                          case _ => simpleMacroBool(tree.duplicate, getText(tree)) // something else, just call simpleMacroBool
                        }
                      case _ => simpleMacroBool(tree.duplicate, getText(tree)) // something else, just call simpleMacroBool
                    }

                  case _ => simpleMacroBool(tree.duplicate, getText(tree)) // something else, just call simpleMacroBool
                }

              case _ =>
                /**
                 * something else but still a binary macro bool, generate AST for the following code:
                 *
                 * {
                 *   val $org_scalatest_assert_macro_left = {qualifier of the method call}
                 *   val $org_scalatest_assert_macro_right = {first argument}
                 *   [code generated from binaryMacroBool]
                 * }
                 */
                Block(
                  valDef("$org_scalatest_assert_macro_left", leftTree),
                  valDef("$org_scalatest_assert_macro_right", rightTree),
                  binaryMacroBool(select.duplicate)
                )
            }

          case funApply: Apply if funApply.args.size == 1 =>
            funApply.fun match {
              case select: Select if select.name.decoded == "===" || select.name.decoded == "!==" =>
                val (leftTree, rightTree) = traverseSelect(select, funApply.args(0))
                /**
                 * For === and !== that takes Equality, for example a === b
                 *
                 * will generate AST for the following code:
                 *
                 * {
                 *   val $org_scalatest_assert_macro_left = a
                 *   val $org_scalatest_assert_macro_right = b
                 *   [code generated from binaryMacroBool]
                 * }
                 */
                Block(
                  valDef("$org_scalatest_assert_macro_left", leftTree),
                  valDef("$org_scalatest_assert_macro_right", rightTree),
                  binaryMacroBool(select.duplicate, apply.args(0).duplicate) // TODO: should apply.args(0) be traversed also?
                )
              case typeApply: TypeApply =>
                typeApply.fun match {
                  case select: Select if typeApply.args.size == 1 =>
                    /**
                     * For === and !== that takes Equality and uses TypeCheckedTripleEquals, for example a === b
                     *
                     * will generate AST for the following code:
                     *
                     * {
                     *   val $org_scalatest_assert_macro_left = a
                     *   val $org_scalatest_assert_macro_right = b
                     *   [code generated from binaryMacroBool]
                     * }
                     */
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
                      simpleMacroBool(tree.duplicate, getText(tree)) // something else, just call simpleMacroBool
                  case _ => simpleMacroBool(tree.duplicate, getText(tree)) // something else, just call simpleMacroBool
                }
              case _ => simpleMacroBool(tree.duplicate, getText(tree)) // something else, just call simpleMacroBool
            }

          case funTypeApply: TypeApply =>
            /**
             * For binary bool expression that takes typed parameter, for example the contains method in scala 2.11, a.contains(b)
             *
             * will generate AST for the following code:
             *
             * {
             *   val $org_scalatest_assert_macro_left = a
             *   val $org_scalatest_assert_macro_right = b
             *   [code generated from typedBinaryMacroBool]
             * }
             */
            funTypeApply.fun match {
              case select: Select if isSupportedBinaryOperator(select.name.decoded) =>
                val (leftTree, rightTree) = traverseSelect(select, apply.args(0))
                Block(
                  valDef("$org_scalatest_assert_macro_left", leftTree),
                  valDef("$org_scalatest_assert_macro_right", rightTree),
                  typedBinaryMacroBool(select.duplicate, funTypeApply.args)
                )

              case _ => simpleMacroBool(tree.duplicate, getText(tree)) // something else, just call simpleMacroBool
            }

          case _ => simpleMacroBool(tree.duplicate, getText(tree)) // something else, just call simpleMacroBool
        }
      case apply: Apply if apply.args.size == 0 =>
        /**
         * For unary operation that takes 0 arguments, for example a.isEmpty
         *
         * will generate AST for the following code:
         *
         * {
         *   val $org_scalatest_assert_macro_left = a
         *   [code generated from unaryMacroBool]
         * }
         */
        apply.fun match {
          case select: Select if isSupportedUnaryOperator(select.name.decoded) =>
            Block(
              valDef("$org_scalatest_assert_macro_left", select.qualifier.duplicate),
              unaryMacroBool(select.duplicate)
            )
          case _ => simpleMacroBool(tree.duplicate, getText(tree))
        }
      case typeApply: TypeApply if typeApply.args.length == 1 =>
        typeApply.fun match {
          case select: Select =>
            val operator = select.name.decoded
            if (operator == "isInstanceOf") {
              /**
               * For isInstanceOf support, for example a.isInstanceOf[String]
               *
               * will generate AST for the following code:
               *
               * {
               *   val $org_scalatest_assert_macro_left = a
               *   [code generated from isInstanceOfMacroBool]
               * }
               */
              typeApply.args(0).tpe match {
                case typeRef: TypeRef =>
                  Block(
                    valDef("$org_scalatest_assert_macro_left", select.qualifier.duplicate),
                    isInstanceOfMacroBool(select.duplicate, typeRef.sym.fullName, typeApply.args(0).duplicate)
                  )
                case _ => simpleMacroBool(tree.duplicate, getText(tree)) // something else, just call simpleMacroBool
              }
            }
            else
              simpleMacroBool(tree.duplicate, getText(tree)) // something else, just call simpleMacroBool
          case _ => simpleMacroBool(tree.duplicate, getText(tree)) // something else, just call simpleMacroBool
        }
      case select: Select if supportedUnaryOperations.contains(select.name.decoded) => // for ! and unary operation that does not take any arguments
          if (select.name.decoded == "unary_!") {
            /**
             * For unary_! operation, for example !a
             *
             * will generate AST for the following code:
             *
             * {
             *   val $org_scalatest_assert_macro_left = a
             *   [code generated from notBool]
             * }
             */
            val leftTree =
              select.qualifier match {
                case selectApply: Apply => transformAst(selectApply.duplicate)
                case selectSelect: Select => transformAst(selectSelect.duplicate)
                case selectTypeApply: TypeApply => transformAst(selectTypeApply.duplicate)
                case _ => simpleMacroBool(select.qualifier.duplicate, getText(select.qualifier))
              }
            notBool(leftTree.duplicate)
          }
          else {
            /**
             * For unary operation that does not take any arguments, for example a.isEmpty
             *
             * will generate AST for the following code:
             *
             * {
             *   val $org_scalatest_assert_macro_left = a
             *   [code generated from unaryMacroBool]
             * }
             */
            Block(
              valDef("$org_scalatest_assert_macro_left", select.qualifier.duplicate),
              unaryMacroBool(select.duplicate)
            )
          }
      case _ =>
        simpleMacroBool(tree.duplicate, getText(tree))// something else, just call simpleMacroBool
    }
  }

  /**
   * Generate AST for the following code:
   *
   * {helperName}.{methodName}($org_scalatest_assert_macro_expr, clue)
   */
  def callHelper(methodName: String, clueTree: Tree): Apply =
    Apply(
      Select(
        Ident(newTermName(helperName)),
        newTermName(methodName)
      ),
      List(Ident(newTermName("$org_scalatest_assert_macro_expr")), clueTree)
    )

  /**
   * Generate AST for the following code:
   *
   * {
   *   val $org_scalatest_assert_macro_expr = [code generated from transformAst]
   *   [code generated from callHelper]
   * }
   */
  def genMacro(booleanExpr: Expr[Boolean], methodName: String, clueExpr: Expr[Any]): Expr[Unit] = {
    val ownerRepair = new MacroOwnerRepair[context.type](context)
    val expandedCode =
      context.Expr(
        Block(
          valDef("$org_scalatest_assert_macro_expr", transformAst(booleanExpr.tree)),
          callHelper(methodName, clueExpr.tree)
        )
      )
    ownerRepair.repairOwners(expandedCode)
  }
}