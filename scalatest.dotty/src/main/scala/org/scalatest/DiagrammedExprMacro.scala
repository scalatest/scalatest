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

import org.scalactic._
import scala.tasty._
import scala.quoted._


object DiagrammedExprMacro {
  // Transform the input expression by parsing out the anchor and generate expression that can support diagram rendering
  def parse(refl: Reflection)(expr: refl.Term): refl.Term = {
    import refl._
    import util._

    type R
    implicit val resTp: quoted.Type[R] = expr.tpe.seal.asInstanceOf[quoted.Type[R]]

    def isXmlSugar(apply: Apply): Boolean = apply.tpe <:< typeOf[scala.xml.Elem]
    def isJavaStatic(tree: Tree): Boolean = tree.symbol.flags.is(Flags.Static)

    def apply(l: Term, name: String, targs: List[TypeTree], args: List[Term]): Term =
      Select.overloaded(l, name, targs.map(_.tpe), args)

    def selectField(o: Term, name: String): Term = Select.unique(o, name)

    def default: Term =
      '{ DiagrammedExpr.simpleExpr[R](${expr.seal.cast[R]}, ${ getAnchor(expr) } ) }.unseal

    def lets(xs: List[Term])(body: List[Term] => Term): Term = {
      def rec(xs: List[Term], acc: List[Term]): Term = xs match {
        case Nil => body(acc)
        case x :: xs => let(x) { (x: Term) => rec(xs, x :: acc) }
      }
      rec(xs, Nil)
    }

    def getAnchorForSelect(sel: Select): Expr[Int] = {
      if (sel.name == "unary_!")
        (sel.pos.startColumn - rootPosition.startColumn).toExpr
      else {
        val selOffset = sel.pos.endColumn - sel.qualifier.pos.endColumn - sel.name.length
        (sel.qualifier.pos.endColumn + selOffset - rootPosition.startColumn).toExpr
      }
    }

    def getAnchor(expr: Term): Expr[Int] = {
      // -1 to match scala2 position
      // ((expr.unseal.pos.endColumn + expr.unseal.pos.startColumn - 1) / 2 - rootPosition.startColumn).toExpr
      (expr.pos.startColumn - rootPosition.startColumn).toExpr
    }

    expr match {
      case Apply(Select(New(_), _), _) => default

      case IsApply(apply) if isXmlSugar(apply) => default

      case IsApply(apply) if isJavaStatic(apply) => default

      case Select(This(_), _) => default

      case IsSelect(x) if x.symbol.flags.is(Flags.Object) => default

      case IsSelect(x) if isJavaStatic(x) => default

      case sel @ Select(qual, name) =>
        type T
        implicit val objTp: quoted.Type[T] = qual.tpe.seal.asInstanceOf[quoted.Type[T]]
        val obj = parse(refl)(qual).seal.cast[DiagrammedExpr[T]]
        val anchor = getAnchorForSelect(sel.asInstanceOf[Select])

        '{
          val o = $obj
          DiagrammedExpr.selectExpr[R](o, ${ selectField('{o.value}.unseal, name).seal.cast[R] }, $anchor)
        }.unseal

      case Block(stats, expr) =>
        // call parse recursively using the expr argument if it is a block
        Block(stats, parse(refl)(expr))

      case Apply(sel @ Select(lhs, op), rhs :: Nil) =>
        val anchor = getAnchorForSelect(sel.asInstanceOf[Select])
        op match {
          case "||" | "|" =>
            val left = parse(refl)(lhs).seal.cast[DiagrammedExpr[Boolean]]
            val right = parse(refl)(rhs).seal.cast[DiagrammedExpr[Boolean]]

            '{
              val l = $left
              if (l.value) l
              else {
                val r = $right
                DiagrammedExpr.applyExpr[Boolean](l, r :: Nil, r.value, $anchor)
              }
            }.unseal
          case "&&" | "&" =>
            val left = parse(refl)(lhs).seal.cast[DiagrammedExpr[Boolean]]
            val right = parse(refl)(rhs).seal.cast[DiagrammedExpr[Boolean]]
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
            val left = parse(refl)(lhs)

            val right = parse(refl)(rhs)

            let(left) { l =>
              let(right) { r =>
                val left = l.seal.cast[DiagrammedExpr[T]]
                val right = r.seal.cast[DiagrammedExpr[_]]
                val res = apply('{$left.value}.unseal, op, Nil, '{$right.value}.unseal :: Nil).seal.cast[R]
                '{ DiagrammedExpr.applyExpr[R]($left, $right :: Nil, $res, $anchor) }.unseal
              }
            }
        }

      case Apply(sel @ Select(lhs, op), args) =>
        type T
        implicit val tpT: quoted.Type[T] = lhs.tpe.seal.asInstanceOf[quoted.Type[T]]

        val left = parse(refl)(lhs)
        val anchor = getAnchorForSelect(sel.asInstanceOf[Select])

        val rights = args.map { arg => parse(refl)(arg) }

        let(left) { l =>
          lets(rights) { rs =>
            val left = l.seal.cast[DiagrammedExpr[T]]
            val rights = rs.map(_.seal.cast[DiagrammedExpr[_]])
            val res = Select.overloaded('{$left.value}.unseal, op, Nil, rs).seal.cast[R]
            '{ DiagrammedExpr.applyExpr[R]($left, ${rights.toExprOfList}, $res, $anchor) }.unseal
          }
        }

      case Apply(TypeApply(sel @ Select(lhs, op), targs), args) =>
        type T
        implicit val tpT: quoted.Type[T] = lhs.tpe.seal.asInstanceOf[quoted.Type[T]]

        val left = parse(refl)(lhs)
        val anchor = getAnchorForSelect(sel.asInstanceOf[Select])

        val rights = args.map { arg => parse(refl)(arg) }

        let(left) { l =>
          lets(rights) { rs =>
            val left = l.seal.cast[DiagrammedExpr[T]]
            val rights = rs.map(_.seal.cast[DiagrammedExpr[_]])
            val res = Select.overloaded('{$left.value}.unseal, op, targs.map(_.tpe), rs).seal.cast[R]
            '{ DiagrammedExpr.applyExpr[R]($left, ${rights.toExprOfList}, $res, $anchor) }.unseal
          }
        }

      case _ =>
        default
    }
  }

  def transform(
    helper: Expr[(DiagrammedExpr[Boolean], Any, String, source.Position) => Assertion],
    condition: Expr[Boolean], pos: Expr[source.Position], clue: Expr[Any], sourceText: String
  )(implicit refl: Reflection): Expr[Assertion] = {
    import refl._
    val diagExpr = parse(refl)(condition.unseal.underlyingArgument).seal.cast[DiagrammedExpr[Boolean]]
    '{ $helper($diagExpr, $clue, ${sourceText.toExpr}, $pos) }
  }
}