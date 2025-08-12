/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalatest.diagrams

import org.scalactic._
import org.scalatest.Assertion
import scala.reflect.macros.whitebox.Context
import scala.annotation.tailrec

private[diagrams] class DiagrammedExprMacro[C <: Context](val context: C) {

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

  // this is taken from expecty
  private[this] def getPosition(expr: Tree) = expr.pos.asInstanceOf[scala.reflect.internal.util.Position]

  // this is taken from expecty and modified, the purpose is to get the anchor for the given expression
  private[this] def getAnchor(expr: Tree): Int = {
    expr match {
      case apply @ Apply(x, ys) if isXmlSugar(apply) => 
        val anchor = getAnchor(x)
        val adjustment = 
          getPosition(expr) match {
            case NoPosition => 0
            case pos =>
              val line = pos.source.lineToString(pos.line - 1)
              val c = line.charAt(anchor)
              if (c == '<') 0 else 1
          }
        anchor - adjustment
      case Apply(x, ys) => getAnchor(x) + 0
      case TypeApply(x, ys) => ;getAnchor(x) + 0
      case _ => 
        getPosition(expr) match {
          case NoPosition => -1
          case pos =>
            pos.point - pos.source.lineToOffset(pos.line - 1)
        }
    }
  }

  /**
   * This generates AST for a List constructions, like the following code:
   *
   * scala.collection.immutable.List.apply(a, b, c)
   */
  def list(elements: List[Tree]): Tree =
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("scala")),
              TermName("collection")
            ),
            TermName("immutable")
          ),
          TermName("List")
        ),
        TermName("apply")
      ),
      elements
    )

  def byNameExpr(tree: Tree): Apply = {
    Apply(
      Select(
        Select(
          Select(
            Select(
              Select(
                Ident(TermName("_root_")),
                TermName("org")
              ),
              TermName("scalatest")
            ),
            TermName("diagrams")
          ), 
          TermName("DiagrammedExpr")
        ),
        TermName("byNameExpr")
      ),
      List(
        tree,
        Literal(Constant(getAnchor(tree)))
      )
    )
  }  

  /**
   * For a given expression (passed in as tree), generate AST for the following code:
   *
   * org.scalatest.DiagrammedExpr.simpleExpr(expr, anchorOfExpr)
   */
  def simpleExpr(tree: Tree): Apply = {
    Apply(
      Select(
        Select(
          Select(
            Select(
              Select(
                Ident(TermName("_root_")),
                TermName("org")
              ),
              TermName("scalatest")
            ),
            TermName("diagrams")
          ), 
          TermName("DiagrammedExpr")
        ),
        TermName("simpleExpr")
      ),
      List(
        tree,
        Literal(Constant(getAnchor(tree)))
      )
    )
  }

  // A data holder for result of traverseApply
  case class ApplyInfo(select: Select, applyList: List[GenericApply])

  // Traverse a GenericApply until it finds a Select that contains the qualifier
  private def traverseApply(apply: GenericApply, accApplyList: List[GenericApply] = List.empty): ApplyInfo =
    apply.fun match {
      case select: Select => ApplyInfo(select, apply :: accApplyList)
      case funApply: GenericApply => traverseApply(funApply, apply :: accApplyList)
    }

  // Rebuild the value expression by going backward the list of GenericApply (tail of applyList traverseApply),
  // our aim is to extract the qualifier and assign it to a val, and rebuild the invocation using by referring
  // to the val.
  private def recursiveValueApply(applyList: List[GenericApply], currentApply: GenericApply): GenericApply = {
    applyList match {
      case TypeApply(fun, args) :: tail =>
        recursiveValueApply(tail, TypeApply(currentApply, args))
      case Apply(fun, args) :: tail =>
        recursiveValueApply(tail, Apply(currentApply, args))
      case Nil => currentApply
    }
  }

  /**
   * Given a Select, e.g. a.isEmpty generate AST for the following code:
   *
   * {
   *   val $org_scalatest_macro_qualifier = a
   *   org.scalatest.DiagrammedExpr.selectExpr($org_scalatest_macro_qualifier, $org_scalatest_macro_qualifier.value.isEmpty, anchorOfSelect)
   * }
   */
  def selectExpr(select: Select): Tree = {
    val qualifierValDef: Tree = valDef("$org_scalatest_macro_qualifier", transformAst(select.qualifier))

    val valueExpr =
      Select(
        Select(Ident(TermName("$org_scalatest_macro_qualifier")), TermName("value")),
        select.name
      )

    val resultExpr: Tree =
      Apply(
        Select(
          Select(
            Select(
              Select(
                Select(
                  Ident(TermName("_root_")),
                  TermName("org")
                ),
                TermName("scalatest")
              ),
              TermName("diagrams")
            ),
            TermName("DiagrammedExpr")
          ),
          TermName("selectExpr")
        ),
        List(
          Ident(TermName("$org_scalatest_macro_qualifier")),
          valueExpr,
          Literal(Constant(getAnchor(select)))
        )
      )

    val exprList: List[Tree] = List(qualifierValDef, resultExpr)

    q"..${exprList}"
  }

  def paramss(tpe: Type): List[List[Symbol]] = {
    @tailrec
    def loop(tpe: Type, paramss: List[List[Symbol]]): List[List[Symbol]] = tpe match {
      case PolyType(_, tpe) => loop(tpe, paramss)
      case MethodType(tparams, tpe) => loop(tpe, paramss :+ tparams)
      case _ => paramss
    }
    loop(tpe, Nil)
  }

  // inspired from https://github.com/scala/async/blob/master/src/main/scala/scala/async/internal/TransformUtils.scala#L112-L127
  private def isByName(fun: Tree, i: Int, j: Int): Boolean = {
    val byNamess = fun.tpe.paramLists.map(_.map(_.asTerm.isByNameParam))
    util.Try(byNamess(i)(j)).getOrElse(false)
  }

  /**
   * Given a Apply, e.g. a == b, generate AST for the following code:
   *
   * {
   *   val $org_scalatest_macro_qualifier = a
   *   val $org_scalatest_macro_arg_0 = b
   *   org.scalatest.DiagrammedExpr.applyExpr($org_scalatest_macro_qualifier, List($org_scalatest_macro_arg_0), $org_scalatest_macro_qualifier.==($org_scalatest_macro_arg_0), anchorOfApply)
   * }
   */
  private def applyExpr(apply: GenericApply): Tree = {
    val applyInfo = traverseApply(apply)

    // val $org_scalatest_macro_qualifier = a
    val qualifierValDef: Tree = valDef("$org_scalatest_macro_qualifier", transformAst(applyInfo.select.qualifier))

    // Build up the arguments val
    // In the above example, it will generate:
    // val $org_scalatest_macro_arg_0 = b
    val argsValDefList: List[ValDef] =
      applyInfo.applyList.zipWithIndex.flatMap { case (currentApply, i) =>
        val base = i * 100  // should be ok as maximum function arguments is 22
        currentApply match {
          case Apply(fun, args) =>
            args.zipWithIndex.map { case (arg, j) =>
              arg match {
                case func: Function => valDef("$org_scalatest_macro_arg_" + (base + j), byNameExpr(arg)) // ignore function, create a dummy val.
                case other =>
                  if (isByName(fun, 0, j))
                    valDef("$org_scalatest_macro_arg_" + (base + j), byNameExpr(arg))
                  else
                    valDef("$org_scalatest_macro_arg_" + (base + j), transformAst(arg))
              }
            }
          case _ => List.empty
        }
      }

    // Build up the List($org_scalatest_macro_arg_0) in the above example
    val substitutedArgsList: List[(GenericApply, List[Tree])] =
      applyInfo.applyList.zipWithIndex.map { case (currentApply, i) =>
        val base = i * 100  // should be ok as maximum function arguments is 22
        (
          currentApply, 
          currentApply match {
            case Apply(fun, args) =>
              args.zipWithIndex.map { case (arg, j) =>
                arg match {
                  case func: Function =>  
                    Apply(
                      Select(Select(Ident(TermName("$org_scalatest_macro_arg_" + (base + j))), TermName("value")), TermName("apply")),
                      List.empty 
                    )
                    
                  case byName if isByName(fun, 0, j) => //byName
                    Apply(
                      Select(Select(Ident(TermName("$org_scalatest_macro_arg_" + (base + j))), TermName("value")), TermName("apply")),
                      List.empty 
                    ) 
                  case _ => Select(Ident(TermName("$org_scalatest_macro_arg_" + (base + j))), TermName("value"))
                }
              }
            case _ => currentApply.args
          }
        )
        

      }

    // and this is:
    // $org_scalatest_macro_qualifier.==($org_scalatest_macro_arg_0)
    val currentValueApply =
      applyInfo.applyList.head match {
        case typeApply: TypeApply =>
          TypeApply(
            Select(
              Select(Ident(TermName("$org_scalatest_macro_qualifier")), TermName("value")),
              applyInfo.select.name
            ),
            substitutedArgsList.head._2
          )
        case _ =>
          Apply(
            Select(
              Select(Ident(TermName("$org_scalatest_macro_qualifier")), TermName("value")),
              applyInfo.select.name
            ),
            substitutedArgsList.head._2
          )
      }  

    //val valueExpr = recursiveValueApply(applyInfo.applyList.tail, currentValueApply)
    val valueExpr = 
      substitutedArgsList.tail.foldLeft(currentValueApply) { case (res, ele) =>
        ele match {
          case (Apply(_, _), args) => Apply(res, args)
          case (TypeApply(_, _), args) => TypeApply(res, args)
        }
      }

    val argIdents: List[Tree] =
      argsValDefList.map { valDef =>
        Ident(valDef.name)
      }

    // and now the final result:
    // org.scalatest.DiagrammedExpr.applyExpr($org_scalatest_macro_qualifier, List($org_scalatest_macro_arg_0), $org_scalatest_macro_qualifier.==($org_scalatest_macro_arg_0), anchorOfApply)
    val resultExpr: Tree =
      Apply(
        Select(
          Select(
            Select(
              Select(
                Select(
                  Ident(TermName("_root_")),
                  TermName("org")
                ),
                TermName("scalatest")
              ),
              TermName("diagrams")
            ),
            TermName("DiagrammedExpr")
          ),
          TermName("applyExpr")
        ),
        List(
          Ident(TermName("$org_scalatest_macro_qualifier")),
          list(argIdents),
          valueExpr,
          Literal(Constant(getAnchor(apply)))
        )
      )

    // Special handle if the method invocation is logical expression &&, &, || and |
    val exprList: List[Tree] = {
      val funcName = applyInfo.select.name.decodedName.toString
      if ((funcName == "&&" || funcName == "&") && argIdents.length == 1) {
        /**
         * If it is && or &, try to be lazy by doing:
         *
         * if ($org_scalatest_macro_qualifier.value) { // only evaluate the right when the left/qualifier is true
         *   val $org_scalatest_macro_arg_0 = ...
         *   org.scalatest.DiagrammedExpr.applyExpr(...)
         * }
         * else
         *   $org_scalatest_macro_qualifier
         */
        val ifCheck =
          If(
            Select(
              Ident(TermName("$org_scalatest_macro_qualifier")),
              TermName("value")
            ),
            q"..${(argsValDefList ::: List(resultExpr))}",
            Ident(TermName("$org_scalatest_macro_qualifier"))
          )
        List(qualifierValDef, ifCheck)
      }
      else if ((funcName == "||" || funcName == "|") && argIdents.length == 1) {
        // ||, try to be lazy
        /**
         * If it is || or |, ry to be lazy by doing:
         *
         * if ($org_scalatest_macro_qualifier.value)
         *   $org_scalatest_macro_qualifier
         * else { // only evaluate the right when left/qualifier is false
         *   val $org_scalatest_macro_arg_0 = ...
         *   org.scalatest.DiagrammedExpr.applyExpr(...)
         * }
         */
        val ifCheck =
          If(
            Select(
              Ident(TermName("$org_scalatest_macro_qualifier")),
              TermName("value")
            ),
            Ident(TermName("$org_scalatest_macro_qualifier")),
            q"..${(argsValDefList ::: List(resultExpr))}"
          )
        List(qualifierValDef, ifCheck)
      }
      else
        qualifierValDef :: argsValDefList ::: List(resultExpr)
    }

    q"""..${exprList}"""
  }

  def isXmlSugar(apply: Apply): Boolean =
    apply match {
      case Apply(
             Select(
               New(
                 Select(
                   Select(
                     Ident(scalaName),
                     xmlName
                   ),
                   xmlElemName
                 )
               ),
               constructorName
             ),
             _
           ) if scalaName.decodedName.toString == "scala" && xmlName.decodedName.toString == "xml" && xmlElemName.decodedName.toString == "Elem" && constructorName.decodedName.toString == "<init>" => true
      case _ => false
    }

  def isJavaStatic(tree: Tree): Boolean = tree.symbol.isJava && tree.symbol.isStatic

  // Transform the input expression by parsing out the anchor and generate expression that can support diagram rendering
  def transformAst(tree: Tree): Tree = {
    tree match {
      case Apply(Select(New(_), _), _) => simpleExpr(tree)  // delegate to simpleExpr if it is a New expression
      case apply: Apply if isXmlSugar(apply) => simpleExpr(tree)
      case apply: Apply if isJavaStatic(apply) => simpleExpr(apply)
      case apply: GenericApply => applyExpr(apply) // delegate to applyExpr if it is Apply
      case Select(This(_), _) => simpleExpr(tree) // delegate to simpleExpr if it is a Select for this, e.g. referring a to instance member.
      case x: Select if x.symbol.isModule => simpleExpr(tree) // don't traverse packages
      case x: Select if isJavaStatic(x) => simpleExpr(tree)
      case select: Select => selectExpr(select) // delegate to selectExpr if it is a Select
      case Block(stats, expr) => Block(stats, transformAst(expr)) // call transformAst recursively using the expr argument if it is a block
      case other => simpleExpr(other) // for others, just delegate to simpleExpr
    }
  }

  // Generate AST for:
  // helper.methodName($org_scalatest_assert_macro_expr, clue, sourceText)
  def callHelper(helper: Tree, methodName: String, clueTree: Tree, sourceText: String, pos: Tree): Apply =
    Apply(
      Select(
        helper,
        TermName(methodName)
      ),
      List(Ident(TermName("$org_scalatest_assert_macro_expr")), clueTree, Literal(Constant(sourceText)), pos)
    )

  /**
   * Generate macro code that does:
   *
   * {
   *   val $org_scalatest_assert_macro_expr = [code generated from transformAst]
   *   [code generated from callHelper]
   * }
   */
  def genMacro(helper: Tree, booleanExpr: Expr[Boolean], methodName: String, clueExpr: Expr[Any], sourceText: String, pos: Expr[source.Position]): Expr[Assertion] = {
    val ownerRepair = new MacroOwnerRepair[context.type](context)
    val expandedCode =
      context.Expr(
        q"""
          ${valDef("$org_scalatest_assert_macro_expr", transformAst(booleanExpr.tree))}
          ${callHelper(helper, methodName, clueExpr.tree, sourceText, pos.tree)}
        """
      )  
    ownerRepair.repairOwners(expandedCode)
  }
}