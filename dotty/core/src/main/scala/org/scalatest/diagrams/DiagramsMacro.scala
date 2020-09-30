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
package org.scalatest.diagrams

import org.scalactic._
import scala.quoted._
import org.scalatest.{Assertions, DiagrammedExpr}
import org.scalatest.compatible.Assertion

object DiagramsMacro {
  // Transform the input expression by parsing out the anchor and generate expression that can support diagram rendering
  def parse(using qctx: QuoteContext)(expr: qctx.tasty.Term): qctx.tasty.Term = {
    import qctx.tasty._
    import util._

    type R
    implicit val resTp: quoted.Type[R] = expr.tpe.seal.asInstanceOf[quoted.Type[R]]

    def isXmlSugar(apply: Apply): Boolean = apply.tpe <:< typeOf[scala.xml.Elem]
    def isJavaStatic(tree: Tree): Boolean = tree.symbol.flags.is(Flags.Static)
    def isImplicitMethodType(tp: Type): Boolean = tp match {
      case tp: MethodType => tp.isImplicit
      case _ => false
    }

    def selectField(o: Term, name: String): Term = Select.unique(o, name)

    def default(term: Term): Term = {
      type T
      implicit val resTp: quoted.Type[T] = term.tpe.seal.asInstanceOf[quoted.Type[T]]
      '{ DiagrammedExpr.simpleExpr[T](${term.seal.cast[T]}, ${ getAnchor(term) } ) }.unseal
    }

    def getAnchorForSelect(sel: Select): Expr[Int] = {
      if (sel.name == "unary_!")
        Expr(sel.pos.startColumn - rootPosition.startColumn)
      else {
        val selOffset = sel.pos.endColumn - sel.qualifier.pos.endColumn - sel.name.length
        Expr(sel.qualifier.pos.endColumn + selOffset - rootPosition.startColumn)
      }
    }

    def getAnchor(expr: Term): Expr[Int] = {
      // -1 to match scala2 position
      // Expr((expr.unseal.pos.endColumn + expr.unseal.pos.startColumn - 1) / 2 - rootPosition.startColumn)
      Expr(expr.pos.startColumn - rootPosition.startColumn)
    }

    def handleArgs(argTps: List[Type], args: List[Term]): (List[Term], List[Term]) =
      args.zip(argTps).foldLeft(Nil -> Nil : (List[Term], List[Term])) { case ((diagrams, others), pair) =>
        pair match {
          case (arg, ByNameType(_)) =>
            (diagrams, others :+ arg)
          case (arg, tp) =>
            if (tp.widen.typeSymbol.show.startsWith("scala.Function")) (diagrams, others :+ arg)
            else (diagrams :+ parse(arg), others)
        }
      }

    expr match {
      case Apply(Select(New(_), _), _) => default(expr)

      case apply: Apply if isXmlSugar(apply) => default(expr)

      case apply: Apply if isJavaStatic(apply) => default(expr)

      case Select(This(_), _) => default(expr)

      case x: Select if x.symbol.flags.is(Flags.Object) => default(expr)

      case x: Select if isJavaStatic(x) => default(expr)

      case sel @ Select(qual, name) =>
        type T
        implicit val objTp: quoted.Type[T] = qual.tpe.seal.asInstanceOf[quoted.Type[T]]
        val obj = parse(qual).seal.cast[DiagrammedExpr[T]]
        val anchor = getAnchorForSelect(sel.asInstanceOf[Select])

        '{
          val o = $obj
          DiagrammedExpr.selectExpr[R](o, ${ selectField('{o.value}.unseal, name).seal.cast[R] }, $anchor)
        }.unseal

      case Block(stats, expr) =>
        // call parse recursively using the expr argument if it is a block
        Block(stats, parse(expr))
      case Apply(sel @ Select(lhs, op), rhs :: Nil) =>
        val anchor = getAnchorForSelect(sel.asInstanceOf[Select])
        op match {
          case "||" | "|" =>
            val left = parse(lhs).seal.cast[DiagrammedExpr[Boolean]]
            val right = parse(rhs).seal.cast[DiagrammedExpr[Boolean]]

            '{
              val l = $left
              if (l.value) l
              else {
                val r = $right
                DiagrammedExpr.applyExpr[Boolean](l, r :: Nil, r.value, $anchor)
              }
            }.unseal
          case "&&" | "&" =>
            val left = parse(lhs).seal.cast[DiagrammedExpr[Boolean]]
            val right = parse(rhs).seal.cast[DiagrammedExpr[Boolean]]
            '{
              val l = $left
              if (!l.value) l
              else {
                val r = $right
                DiagrammedExpr.applyExpr[Boolean](l, r :: Nil, r.value, $anchor)
              }
            }.unseal
          case _ =>
            type T
            implicit val tpT: quoted.Type[T] = lhs.tpe.seal.asInstanceOf[quoted.Type[T]]
            val left = parse(lhs)

            val methTp = sel.tpe.widen.asInstanceOf[MethodType]
            val (diagrams, others) = handleArgs(methTp.paramTypes, rhs :: Nil)

            let(left) { l =>
              lets(diagrams) { rs =>
                val left = l.seal.cast[DiagrammedExpr[T]]
                val rights = rs.map(_.seal.cast[DiagrammedExpr[_]])
                val res = Select.unique(l, "value").select(sel.symbol).appliedToArgs(diagrams.map(r => Select.unique(r, "value")) ++ others).seal.cast[R]
                '{ DiagrammedExpr.applyExpr[R]($left, ${Expr.ofList(rights)}, $res, $anchor) }.unseal
              }
            }
        }

      case Apply(sel @ Select(lhs, op), args) =>
        type T
        implicit val tpT: quoted.Type[T] = lhs.tpe.seal.asInstanceOf[quoted.Type[T]]

        val left = parse(lhs)
        val anchor = getAnchorForSelect(sel.asInstanceOf[Select])

        val methTp = sel.tpe.widen.asInstanceOf[MethodType]
        val (diagrams, others) = handleArgs(methTp.paramTypes, args)

        let(left) { l =>
          lets(diagrams) { rs =>
            val left = l.seal.cast[DiagrammedExpr[T]]
            val rights = rs.map(_.seal.cast[DiagrammedExpr[_]])
            val res = Select.unique(l, "value").select(sel.symbol).appliedToArgs(diagrams.map(r => Select.unique(r, "value")) ++ others).seal.cast[R]
            '{ DiagrammedExpr.applyExpr[R]($left, ${Expr.ofList(rights)}, $res, $anchor) }.unseal
          }
        }

      case Apply(f @ Apply(sel @ Select(Apply(qual, lhs :: Nil), op @ ("===" | "!==")), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        type T
        implicit val tpT: quoted.Type[T] = lhs.tpe.seal.asInstanceOf[quoted.Type[T]]
        val left = parse(lhs)
        val right = parse(rhs)

        val anchor = getAnchorForSelect(sel.asInstanceOf[Select])

        let(left) { left =>
          let(right) { right =>
            val app = qual.appliedTo(Select.unique(left, "value")).select(sel.symbol)
                          .appliedTo(Select.unique(right, "value")).appliedToArgs(implicits)
            let(app) { result =>
              val l = left.seal.cast[DiagrammedExpr[_]]
              val r = right.seal.cast[DiagrammedExpr[_]]
              val b = result.seal.cast[Boolean]
              '{ DiagrammedExpr.applyExpr[Boolean]($l, $r :: Nil, $b, $anchor) }.unseal
            }
          }
        }

      case Apply(fun @ TypeApply(sel @ Select(lhs, op), targs), args) =>
        type T
        implicit val tpT: quoted.Type[T] = lhs.tpe.seal.asInstanceOf[quoted.Type[T]]

        val left = parse(lhs)
        val anchor = getAnchorForSelect(sel.asInstanceOf[Select])

        val methTp = fun.tpe.widen.asInstanceOf[MethodType]
        val (diagrams, others) = handleArgs(methTp.paramTypes, args)

        let(left) { l =>
          lets(diagrams) { rs =>
            val left = l.seal.cast[DiagrammedExpr[T]]
            val rights = rs.map(_.seal.cast[DiagrammedExpr[_]])
            val res = Select.unique(l, "value").select(sel.symbol).appliedToTypes(targs.map(_.tpe))
                            .appliedToArgs(diagrams.map(r => Select.unique(r, "value")) ++ others).seal.cast[R]
            '{ DiagrammedExpr.applyExpr[R]($left, ${Expr.ofList(rights)}, $res, $anchor) }.unseal
          }
        }

      case TypeApply(sel @ Select(lhs, op), targs) =>
        type T
        implicit val tpT: quoted.Type[T] = lhs.tpe.seal.asInstanceOf[quoted.Type[T]]

        val left = parse(lhs)
        val anchor = getAnchorForSelect(sel.asInstanceOf[Select])

        let(left) { l =>
          val left = l.seal.cast[DiagrammedExpr[T]]
          val res = Select.unique(l, "value").select(sel.symbol).appliedToTypes(targs.map(_.tpe)).seal.cast[R]
          '{ DiagrammedExpr.applyExpr[R]($left, Nil, $res, $anchor) }.unseal
        }

      case _ =>
        default(expr)
    }
  }

  def transform(
    helper: Expr[(DiagrammedExpr[Boolean], Any, String, source.Position) => Assertion],
    condition: Expr[Boolean], pos: Expr[source.Position], clue: Expr[Any], sourceText: String
  )(using qctx: QuoteContext): Expr[Assertion] = {
    import qctx.tasty._
    val diagExpr = parse(condition.unseal.underlyingArgument).seal.cast[DiagrammedExpr[Boolean]]
    '{ $helper($diagExpr, $clue, ${Expr(sourceText)}, $pos) }
  }
}
