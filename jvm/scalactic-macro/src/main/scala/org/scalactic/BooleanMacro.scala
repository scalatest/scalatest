/*
 * Copyright 2001-2024 Artima, Inc.
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

import scala.reflect.macros.whitebox.Context

private[org] class BooleanMacro[C <: Context](val context: C) {

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
      TermName(name),
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
      "isEmpty",
      "nonEmpty"
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
  def binaryMacroBool(select: Select, prettifier: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalactic")
          ),
          TermName("Bool")
        ),
        TermName("binaryMacroBool")
      ),
      List(
        Ident(TermName("$org_scalatest_assert_macro_left")),
        q"${select.name.decodedName.toString}",
        Ident(TermName("$org_scalatest_assert_macro_right")),
        Apply(
          Select(
            Ident(TermName("$org_scalatest_assert_macro_left")),
            select.name
          ),
          List(Ident(TermName("$org_scalatest_assert_macro_right")))
        ),
        prettifier.duplicate
      )
    )

  /**
   * For === and !== method call that takes Equality, for example left === right, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.binaryMacroBool($org_scalatest_assert_macro_left, "===", $org_scalatest_assert_macro_right, $org_scalatest_assert_macro_left.===($org_scalatest_assert_macro_right)(defaultEquality))
   */
  def binaryMacroBool(select: Select, secondArg: Tree, prettifier: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalactic")
          ),
          TermName("Bool")
        ),
        TermName("binaryMacroBool")
      ),
      List(
        Ident(TermName("$org_scalatest_assert_macro_left")),
        q"${select.name.decodedName.toString}",
        Ident(TermName("$org_scalatest_assert_macro_right")),
        Apply(
          Apply(
            Select(
              Ident(TermName("$org_scalatest_assert_macro_left")),
              select.name
            ),
            List(Ident(TermName("$org_scalatest_assert_macro_right")))
          ),
          List(secondArg)
        ),
        prettifier.duplicate
      )
    )

  /**
   * For === and !== method call that takes Equality and uses TypeCheckedTripleEquals , for example left === right, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.binaryMacroBool($org_scalatest_assert_macro_left, "===", $org_scalatest_assert_macro_right, $org_scalatest_assert_macro_left.===[R]($org_scalatest_assert_macro_right)(defaultEquality))
   */
  def typedBinaryMacroBool(select: Select, typeArgs: List[Tree], prettifier: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalactic")
          ),
          TermName("Bool")
        ),
        TermName("binaryMacroBool")
      ),
      List(
        Ident(TermName("$org_scalatest_assert_macro_left")),
        q"${select.name.decodedName.toString}",
        Ident(TermName("$org_scalatest_assert_macro_right")),
        Apply(
          TypeApply(
            Select(
              Ident(TermName("$org_scalatest_assert_macro_left")),
              select.name
            ),
            typeArgs
          ),
          List(Ident(TermName("$org_scalatest_assert_macro_right")))
        ),
        prettifier.duplicate
      )
    )

  /**
   * For unrecognized expression, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.simpleMacroBool(expression, expressionText)  // expressionText is currently the text returned from 'show'
   */
  def simpleMacroBool(expression: Tree, expressionText: String, prettifier: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalactic")
          ),
          TermName("Bool")
        ),
        TermName("simpleMacroBool")
      ),
      List(
        expression,
        q"${expressionText}",
        prettifier.duplicate
      )
    )

  /**
   * For !a unary_! call, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.notBool(a, prettifier)
   */
  def notBool(target: Tree, prettifierTree: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalactic")
          ),
          TermName("Bool")
        ),
        TermName("notBool")
      ),
      List(
        target.duplicate,
        prettifierTree.duplicate
      )
    )

  /**
   * For a.isEmpty and method call that does not has any argument, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.unaryMacroBool($org_scalatest_assert_macro_left, "isEmpty", $org_scalatest_assert_macro_left.isEmpty, prettifier)
   */
  def unaryMacroBool(select: Select, prettifier: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalactic")
          ),
          TermName("Bool")
        ),
        TermName("unaryMacroBool")
      ),
      List(
        Ident(TermName("$org_scalatest_assert_macro_left")),
        q"${select.name.decodedName.toString}",
        Select(
          Ident(TermName("$org_scalatest_assert_macro_left")),
          select.name
        ),
        prettifier.duplicate
      )
    )

  /**
   * For a.isEmpty() and method call that does not has any argument, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.unaryMacroBool($org_scalatest_assert_macro_left, "isEmpty", $org_scalatest_assert_macro_left.isEmpty(), prettifier)
   */
  def unaryApplyMacroBool(select: Select, prettifier: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalactic")
          ),
          TermName("Bool")
        ),
        TermName("unaryMacroBool")
      ),
      List(
        Ident(TermName("$org_scalatest_assert_macro_left")),
        q"${select.name.decodedName.toString}",
        Apply(
          Select(
            Ident(TermName("$org_scalatest_assert_macro_left")),
            select.name
          ), 
          List.empty
        ),
        prettifier.duplicate
      )
    )    

  /**
   * For a.isInstanceOf[String] == 1, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.isInstanceOfMacroBool($org_scalatest_assert_macro_left, "==", $org_scalatest_assert_macro_left.length, 1)
   */
  def isInstanceOfMacroBool(select: Select, className: String, typeArg: Tree, prettifier: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalactic")
          ),
          TermName("Bool")
        ),
        TermName("isInstanceOfMacroBool")
      ),
      List(
        Ident(TermName("$org_scalatest_assert_macro_left")),
        q"${select.name.decodedName.toString}",
        q"${className}",
        TypeApply(
          Select(
            Ident(TermName("$org_scalatest_assert_macro_left")),
            select.name
          ),
          List(typeArg)
        ),
        prettifier.duplicate
      )
    )

  /**
   * For a.length == 1, it'll generate the AST for the following code:
   *
   * org.scalactic.Bool.lengthSizeMacroBool($org_scalatest_assert_macro_left, "==", $org_scalatest_assert_macro_left.length, 1)
   */
  def lengthSizeMacroBool(select: Select, prettifier: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalactic")
          ),
          TermName("Bool")
        ),
        TermName("lengthSizeMacroBool")
      ),
      List(
        Ident(TermName("$org_scalatest_assert_macro_left")),
        q"${select.name.decodedName.toString}",
        Select(
          Ident(TermName("$org_scalatest_assert_macro_left")),
          select.name
        ),
        Ident(TermName("$org_scalatest_assert_macro_right")),
        prettifier.duplicate
      )
    )

  def lengthSizeApplyMacroBool(select: Select, prettifier: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalactic")
          ),
          TermName("Bool")
        ),
        TermName("lengthSizeMacroBool")
      ),
      List(
        Ident(TermName("$org_scalatest_assert_macro_left")),
        q"${select.name.decodedName.toString}",
        Apply(
          Select(
            Ident(TermName("$org_scalatest_assert_macro_left")),
            select.name
          ), 
          List.empty
        ),
        Ident(TermName("$org_scalatest_assert_macro_right")),
        prettifier.duplicate
      )
    )  

  /**
   * generate the AST for the following code:
   *
   * org.scalactic.Bool.existsMacroBool($org_scalatest_assert_macro_left, $org_scalatest_assert_macro_right, $org_scalatest_assert_macro_left.exists(func))
   */
  def existsMacroBool(select: Select, func: Function, prettifier: Tree): Apply =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("_root_")),
              TermName("org")
            ),
            TermName("scalactic")
          ),
          TermName("Bool")
        ),
        TermName("existsMacroBool")
      ),
      List(
        Ident(TermName("$org_scalatest_assert_macro_left")),
        Ident(TermName("$org_scalatest_assert_macro_right")),
        Apply(
          Select(
            Ident(TermName("$org_scalatest_assert_macro_left")),
            select.name
          ),
          List(
            func
          )
        ),
        prettifier.duplicate
      )
    )

  // traverse the given Select, both qualifier and the right expression.
  def traverseSelect(select: Select, rightExpr: Tree, prettifierTree: Tree): (Tree, Tree) = {
    val operator = select.name.decodedName.toString
    if (logicOperators.contains(operator)) {
      val leftTree = // traverse the qualifier
        select.qualifier match {
          case selectApply: Apply => transformAst(selectApply.duplicate, prettifierTree)
          case selectSelect: Select => transformAst(selectSelect.duplicate, prettifierTree)
          case _ => simpleMacroBool(select.qualifier.duplicate, getText(select.qualifier), prettifierTree)
        }
      val rightTree = {
        val evalBlock =
          rightExpr match {
            case argApply: Apply => transformAst(argApply.duplicate, prettifierTree)
            case argSelect: Select => transformAst(argSelect, prettifierTree)
            case argTypeApply: TypeApply => transformAst(argTypeApply.duplicate, prettifierTree)
            case _ => simpleMacroBool(rightExpr.duplicate, getText(rightExpr), prettifierTree)
          }
        if (operator == "&&")  {// generate if (left.value) {...} else false
          If(
            Select(
              Ident(TermName("$org_scalatest_assert_macro_left")),
              TermName("value")
            ),
            evalBlock,
            simpleMacroBool(q"${false}", "", prettifierTree)
          )
        }
        else if (operator == "||") // || and |, generate if (left.value) true else {...}
          If(
            Select(
              Ident(TermName("$org_scalatest_assert_macro_left")),
              TermName("value")
            ),
            simpleMacroBool(q"${true}", "", prettifierTree),
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
  private def isPlaceHolder(tree: Tree): Boolean = {
    tree match {
      case valDef: ValDef => valDef.rhs == EmptyTree
      case _ => false
    }
  }

  /**
   * Transform the passed in boolean expression, see comment in different case for details
   */
  def transformAst(tree: Tree, prettifierTree: Tree): Tree = {
    tree match {
      case apply: Apply if apply.args.size == 1 =>
        apply.fun match {
          case select: Select if isSupportedBinaryOperator(select.name.decodedName.toString) =>
            val operator = select.name.decodedName.toString
            val (leftTree, rightTree) =  traverseSelect(select, apply.args(0), prettifierTree)
            operator match {
              case "==" =>
                leftTree match {
                  case leftApply: Apply =>
                    leftApply.fun match {
                      case leftApplySelect: Select if isSupportedLengthSizeOperator(leftApplySelect.name.decodedName.toString) && leftApply.args.isEmpty =>
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
                        q"""
                          ${valDef("$org_scalatest_assert_macro_left", leftApplySelect.qualifier.duplicate)}
                          ${valDef("$org_scalatest_assert_macro_right", rightTree)}
                          ${lengthSizeApplyMacroBool(leftApplySelect.duplicate, prettifierTree)}
                        """
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
                        q"""
                          ${valDef("$org_scalatest_assert_macro_left", leftTree)}
                          ${valDef("$org_scalatest_assert_macro_right", rightTree)}
                          ${binaryMacroBool(select.duplicate, prettifierTree)}
                        """
                    }

                  case leftSelect: Select if isSupportedLengthSizeOperator(leftSelect.name.decodedName.toString) =>
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
                    q"""
                      ${valDef("$org_scalatest_assert_macro_left", leftSelect.qualifier.duplicate)}
                      ${valDef("$org_scalatest_assert_macro_right", rightTree)}
                      ${lengthSizeMacroBool(leftSelect.duplicate, prettifierTree)}
                    """

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
                    q"""
                      ${valDef("$org_scalatest_assert_macro_left", leftTree)}
                      ${valDef("$org_scalatest_assert_macro_right", rightTree)}
                      ${binaryMacroBool(select.duplicate, prettifierTree)}
                    """
                }

              case "exists" =>
                rightTree match {
                  case func: Function if func.children.size == 2 && isPlaceHolder(func.children(0)) =>
                    val boolExpr = func.children(1)
                    boolExpr match {
                      case boolExprApply: Apply if boolExprApply.args.size == 1 =>
                        boolExprApply.fun match {
                          case Select(qualifier, equalEqual) if equalEqual.decodedName.toString == "==" =>
                            val generatedValName = func.children(0).asInstanceOf[ValDef].name.decodedName.toString  // safe cast because it already passed isPlaceHolder

                            qualifier match {
                              /**
                               * support for a.exists(_ == 2), generate AST for the following code:
                               *
                               * {
                               *   val $org_scalatest_assert_macro_left = a
                               *   val $org_scalatest_assert_macro_right = 2
                               *   [code generated from existsMacroBool]
                               * }
                               */
                              case Ident(name) if name.decodedName.toString == generatedValName =>
                                q"""
                                  ${valDef("$org_scalatest_assert_macro_left", leftTree)}
                                  ${valDef("$org_scalatest_assert_macro_right", boolExprApply.args(0).duplicate)}
                                  ${existsMacroBool(select.duplicate, func.duplicate, prettifierTree)}
                                """

                              case _ =>
                                boolExprApply.args(0) match {
                                  /**
                                   * support for a.exists(2 == _), generate AST for the following code:
                                   *
                                   * {
                                   *   val $org_scalatest_assert_macro_left = a
                                   *   val $org_scalatest_assert_macro_right = 2
                                   *   [code generated from existsMacroBool]
                                   * }
                                   */
                                  case Ident(name) if name.decodedName.toString == generatedValName =>
                                    q"""
                                      ${valDef("$org_scalatest_assert_macro_left", leftTree)}
                                      ${valDef("$org_scalatest_assert_macro_right", qualifier.duplicate)}
                                      ${existsMacroBool(select.duplicate, func.duplicate, prettifierTree)}
                                    """

                                  case _ =>
                                    simpleMacroBool(tree.duplicate, getText(tree), prettifierTree)
                                }
                            }

                          case _ => simpleMacroBool(tree.duplicate, getText(tree), prettifierTree) // something else, just call simpleMacroBool
                        }
                      case _ => simpleMacroBool(tree.duplicate, getText(tree), prettifierTree) // something else, just call simpleMacroBool
                    }

                  case _ => simpleMacroBool(tree.duplicate, getText(tree), prettifierTree) // something else, just call simpleMacroBool
                }

              case "<=" | "<" | ">" | ">=" =>
                val wrappedTree: Tree = 
                  leftTree match {
                    case Apply(
                           Apply(
                             TypeApply(
                               Select(
                                 Select(
                                   Select(
                                     Select(
                                       Ident(scalaPackage), 
                                       mathPackage
                                     ), 
                                     orderingClass
                                   ), 
                                   implicitsClass
                                 ), 
                                 infixOrderingOps
                               ), 
                               List(TypeTree())
                             ), 
                             List(wrapped)
                           ), 
                           List(_)
                         ) 
                         if infixOrderingOps.decodedName.toString() == "infixOrderingOps" &&
                            implicitsClass.decodedName.toString() == "Implicits" &&
                            orderingClass.decodedName.toString() == "Ordering" &&
                            mathPackage.decodedName.toString() == "math" && 
                            scalaPackage.decodedName.toString() == "scala" =>  
                      wrapped

                    case other => other
                  }

                q"""
                  ${valDef("$org_scalatest_assert_macro_left", wrappedTree)}
                  ${valDef("$org_scalatest_assert_macro_right", rightTree)}
                  ${binaryMacroBool(select.duplicate, prettifierTree)}
                """

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
                q"""
                  ${valDef("$org_scalatest_assert_macro_left", leftTree)}
                  ${valDef("$org_scalatest_assert_macro_right", rightTree)}
                  ${binaryMacroBool(select.duplicate, prettifierTree)}
                """
            }

          case funApply: Apply if funApply.args.size == 1 =>
            funApply.fun match {
              case select: Select if select.name.decodedName.toString == "===" || select.name.decodedName.toString == "!==" =>
                val (leftTree, rightTree) = traverseSelect(select, funApply.args(0), prettifierTree)
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
                q"""
                  ${valDef("$org_scalatest_assert_macro_left", leftTree)}
                  ${valDef("$org_scalatest_assert_macro_right", rightTree)}
                  ${binaryMacroBool(select.duplicate, apply.args(0).duplicate, prettifierTree)} // TODO: should apply.args(0) be traversed also?
                """
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
                    val operator: String = select.name.decodedName.toString
                    if (operator == "===" || operator == "!==") {
                      val (leftTree, rightTree) = traverseSelect(select, funApply.args(0), prettifierTree)
                      q"""
                        ${valDef("$org_scalatest_assert_macro_left", leftTree)}
                        ${valDef("$org_scalatest_assert_macro_right", rightTree)}
                        ${binaryMacroBool(select.duplicate, apply.args(0).duplicate, prettifierTree)} // TODO: should apply.args(0) be traversed also?
                      """
                    }
                    else
                      simpleMacroBool(tree.duplicate, getText(tree), prettifierTree) // something else, just call simpleMacroBool
                  case _ => simpleMacroBool(tree.duplicate, getText(tree), prettifierTree) // something else, just call simpleMacroBool
                }
              case _ => simpleMacroBool(tree.duplicate, getText(tree), prettifierTree) // something else, just call simpleMacroBool
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
              case select: Select if isSupportedBinaryOperator(select.name.decodedName.toString) =>
                val (leftTree, rightTree) = traverseSelect(select, apply.args(0), prettifierTree)
                q"""
                  ${valDef("$org_scalatest_assert_macro_left", leftTree)}
                  ${valDef("$org_scalatest_assert_macro_right", rightTree)}
                  ${typedBinaryMacroBool(select.duplicate, funTypeApply.args, prettifierTree)}
                """

              case _ => simpleMacroBool(tree.duplicate, getText(tree), prettifierTree) // something else, just call simpleMacroBool
            }

          case _ => simpleMacroBool(tree.duplicate, getText(tree), prettifierTree) // something else, just call simpleMacroBool
        }
      case apply: Apply if apply.args.isEmpty =>
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
          case select: Select if isSupportedUnaryOperator(select.name.decodedName.toString) =>
            q"""
              ${valDef("$org_scalatest_assert_macro_left", select.qualifier.duplicate)}
              ${unaryApplyMacroBool(select.duplicate, prettifierTree)}
            """
          case _ => simpleMacroBool(tree.duplicate, getText(tree), prettifierTree)
        }
      case typeApply: TypeApply if typeApply.args.length == 1 =>
        typeApply.fun match {
          case select: Select =>
            val operator = select.name.decodedName.toString
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
                  q"""
                    ${valDef("$org_scalatest_assert_macro_left", select.qualifier.duplicate)}
                    ${isInstanceOfMacroBool(select.duplicate, typeRef.sym.fullName, typeApply.args(0).duplicate, prettifierTree)}
                  """
                case _ => simpleMacroBool(tree.duplicate, getText(tree), prettifierTree) // something else, just call simpleMacroBool
              }
            }
            else
              simpleMacroBool(tree.duplicate, getText(tree), prettifierTree) // something else, just call simpleMacroBool
          case _ => simpleMacroBool(tree.duplicate, getText(tree), prettifierTree) // something else, just call simpleMacroBool
        }
      case select: Select if supportedUnaryOperations.contains(select.name.decodedName.toString) => // for ! and unary operation that does not take any arguments
          if (select.name.decodedName.toString == "unary_!") {
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
                case selectApply: Apply => transformAst(selectApply.duplicate, prettifierTree)
                case selectSelect: Select => transformAst(selectSelect.duplicate, prettifierTree)
                case selectTypeApply: TypeApply => transformAst(selectTypeApply.duplicate, prettifierTree)
                case _ => simpleMacroBool(select.qualifier.duplicate, getText(select.qualifier), prettifierTree)
              }
            notBool(leftTree.duplicate, prettifierTree)
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
            q"""
              ${valDef("$org_scalatest_assert_macro_left", select.qualifier.duplicate)}
              ${unaryMacroBool(select.duplicate, prettifierTree)}
            """
          }
      case _ =>
        simpleMacroBool(tree.duplicate, getText(tree), prettifierTree)// something else, just call simpleMacroBool
    }
  }

  /**
   * Generate AST for the following code:
   *
   * {helper}.{methodName}($org_scalatest_assert_macro_expr, clue, prettifier, pos)
   */
  def callHelper(helper: Tree, methodName: String, clueTree: Tree, prettifierTree: Tree, posTree: Tree): Apply =
    Apply(
      Select(
        helper,
        TermName(methodName)
      ),
      List(Ident(TermName("$org_scalatest_assert_macro_expr")), clueTree, prettifierTree, posTree)
    )

  /**
    * Generate AST for the following code:
    *
    * {helper}.{methodName}($org_scalatest_assert_macro_expr, clue)
    */
  def callHelper(helper: Tree, methodName: String, clueTree: Tree): Apply =
    Apply(
      Select(
        helper,
        TermName(methodName)
      ),
      List(Ident(TermName("$org_scalatest_assert_macro_expr")), clueTree)
    )

  /**
   * Generate AST for the following code:
   *
   * {
   *   val $org_scalatest_assert_macro_expr = [code generated from transformAst]
   *   [code generated from callHelper]
   * }
   */
  def genMacro[T](helper: Tree, booleanExpr: Expr[Boolean], methodName: String, clueExpr: Expr[Any], prettifierExpr: Expr[_], posExpr: Expr[_]): Expr[T] = {
    val ownerRepair = new MacroOwnerRepair[context.type](context)
    val expandedCode =
      context.Expr(
        q"""
          ${valDef("$org_scalatest_assert_macro_expr", transformAst(booleanExpr.tree, prettifierExpr.tree))}
          ${callHelper(helper, methodName, clueExpr.tree, prettifierExpr.tree, posExpr.tree)}
        """
      )
    ownerRepair.repairOwners(expandedCode)
  }

  /**
    * Generate AST for the following code:
    *
    * {
    *   val $org_scalatest_assert_macro_expr = [code generated from transformAst]
    *   [code generated from callHelper]
    * }
    */
  def genMacro[T](helper: Tree, booleanExpr: Expr[Boolean], methodName: String, clueExpr: Expr[Any], prettifierExpr: Expr[_]): Expr[T] = {
    val ownerRepair = new MacroOwnerRepair[context.type](context)
    val expandedCode =
      context.Expr(
        q"""
          ${valDef("$org_scalatest_assert_macro_expr", transformAst(booleanExpr.tree, prettifierExpr.tree))}
          ${callHelper(helper, methodName, clueExpr.tree)}
        """
      )
    ownerRepair.repairOwners(expandedCode)
  }
}