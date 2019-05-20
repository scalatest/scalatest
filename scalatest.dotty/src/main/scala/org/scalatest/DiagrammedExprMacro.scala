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
  def let[S: Type, T: Type](expr: Expr[S])(body: Expr[S] => Expr[T]): Expr[T] =
    '{
      val x = $expr
      ${ body('{x}) }
    }

  def lets[S: Type, T: Type](xs: List[Expr[S]])(body: List[Expr[S]] => Expr[T]): Expr[T] = {
    def rec(xs: List[Expr[S]], acc: List[Expr[S]]): Expr[T] = xs match {
      case Nil => body(acc)
      case x :: xs => let(x) { (x: Expr[S]) => rec(xs, x :: acc) }
    }
    rec(xs, Nil)
  }

  // Transform the input expression by parsing out the anchor and generate expression that can support diagram rendering
  def parse[T:Type](expr: Expr[T])(implicit refl: Reflection): Expr[DiagrammedExpr[T]] = {
    import refl._

    def isXmlSugar(apply: Apply): Boolean = apply.tpe <:< typeOf[scala.xml.Elem]
    def isJavaStatic(tree: Tree): Boolean = tree.symbol.flags.is(Flags.Static)

    def apply(l: Expr[_], name: String, targs: List[TypeTree], r: List[Expr[_]]): Expr[T] =
      Select.overloaded(l.unseal, name, targs.map(_.tpe), r.map(_.unseal)).seal.cast[T]

    def selectField(o: Expr[_], name: String): Expr[T] = Select.unique(o.unseal, name).seal.cast[T]

    def default: Expr[DiagrammedExpr[T]] = '{ DiagrammedExpr.simpleExpr($expr, ${ getAnchor(expr) } ) }

    expr.unseal.underlyingArgument match {
      case Apply(Select(New(_), _), _) => default

      case IsApply(apply) if isXmlSugar(apply) => default

      case IsApply(apply) if isJavaStatic(apply) => default

      case Select(This(_), _) => default

      case IsSelect(x) if x.symbol.flags.is(Flags.Object) => default

      case IsSelect(x) if isJavaStatic(x) => default

      case sel @ Select(qual, name) =>
        type S
        implicit val tpS: quoted.Type[S] = qual.tpe.seal.asInstanceOf[quoted.Type[S]]
        val obj = parse[S](qual.seal.asInstanceOf[Expr[S]])
        val anchor = getAnchorForSelect(refl)(sel.asInstanceOf[Select])

        '{
          val o = $obj
          DiagrammedExpr.selectExpr(o, ${ selectField('{o.value}, name) }, $anchor)
        }

      case Block(stats, expr) =>
        Block(stats, parse(expr.seal.cast[T]).unseal).seal.cast[DiagrammedExpr[T]] // call parse recursively using the expr argument if it is a block

      case Apply(sel @ Select(lhs, op), rhs :: Nil) =>
        val anchor = getAnchorForSelect(refl)(sel.asInstanceOf[Select])
        op match {
          case "||" | "|" =>
            val left = parse(lhs.seal.cast[Boolean & T])
            val right = parse(rhs.seal.cast[Boolean & T])

            '{
              val l = $left
              if (l.value) l
              else {
                val r = $right
                DiagrammedExpr.applyExpr(l, r :: Nil, r.value, $anchor)
              }
            }
          case "&&" | "&" =>
            val left = parse(lhs.seal.cast[Boolean & T])
            val right = parse(rhs.seal.cast[Boolean & T])
            '{
              val l = $left
              if (!l.value) l
              else {
                val r = $right
                DiagrammedExpr.applyExpr(l, r :: Nil, r.value, $anchor)
              }
            }
          case _ =>
            type S
            implicit val tpS: quoted.Type[S] = lhs.tpe.seal.asInstanceOf[quoted.Type[S]]
            val left = parse[S](lhs.seal.asInstanceOf[Expr[S]])

            type V
            implicit val tpV: quoted.Type[V] = rhs.tpe.seal.asInstanceOf[quoted.Type[V]]
            val right = parse[V](rhs.seal.asInstanceOf[Expr[V]])
            '{
              val l = $left
              val r = $right
              val res = ${ apply('{l.value}, op, Nil, '{r.value} :: Nil) }
              DiagrammedExpr.applyExpr(l, r :: Nil, res, $anchor)
            }
        }

      case Apply(sel @ Select(lhs, op), args) =>
        type S
        implicit val tpS: quoted.Type[S] = lhs.tpe.seal.asInstanceOf[quoted.Type[S]]
        val left = parse[S](lhs.seal.asInstanceOf[Expr[S]])
        val anchor = getAnchorForSelect(refl)(sel.asInstanceOf[Select])

        val rights = args.map { arg =>
          type V
          implicit val tpV: quoted.Type[V] = arg.tpe.seal.asInstanceOf[quoted.Type[V]]
          parse(arg.seal)
          parse[V](arg.seal.asInstanceOf[Expr[V]])
        }

        let(left) { (l: Expr[DiagrammedExpr[_]]) =>
          lets(rights) { (rs: List[Expr[DiagrammedExpr[_]]]) =>
            val res = apply('{($l).value}, op, Nil, rs)
            '{ DiagrammedExpr.applyExpr($l, ${rs.toExprOfList}, $res, $anchor) }
          }
        }

      // TODO: Dotty produces a confusing error message about `let`
      // case Apply(TypeApply(sel @ Select(lhs, op), targs), args) =>
      //   type S
      //   implicit val tpS: quoted.Type[S] = lhs.tpe.seal.asInstanceOf[quoted.Type[S]]
      //   val left = parse[S](lhs.seal.asInstanceOf[Expr[S]])
      //   val anchor = getAnchorForSelect(refl)(sel.asInstanceOf[Select])

      //   val rights = args.map { arg =>
      //     type V
      //     implicit val tpV: quoted.Type[V] = arg.tpe.seal.asInstanceOf[quoted.Type[V]]
      //     parse(arg.seal)
      //     parse[V](arg.seal.asInstanceOf[Expr[V]])
      //   }

      //   let(left) { (l: Expr[DiagrammedExpr[_]]) =>
      //     lets(rights) { (rs: List[Expr[DiagrammedExpr[_]]]) =>
      //       val res = apply('{($l).value}, op, targs, rs)
      //       '{ DiagrammedExpr.applyExpr($l, ${rs.toExprOfList}, $res, $anchor) }
      //     }
      //   }

      case _ =>
        default
    }
  }

  def transform(
    helper: Expr[(DiagrammedExpr[Boolean], Any, String, source.Position) => Assertion],
    condition: Expr[Boolean], pos: Expr[source.Position], clue: Expr[Any], sourceText: String
  )(implicit refl: Reflection): Expr[Assertion] = {
    val diagExpr = parse(condition)
    '{ $helper($diagExpr, $clue, ${sourceText.toExpr}, $pos) }
  }

  def getAnchorForSelect(refl: Reflection)(sel: refl.Select): Expr[Int] = {
    import refl._
    if (sel.name == "unary_!")
      (sel.pos.startColumn - rootPosition.startColumn).toExpr
    else {
      val selOffset = sel.pos.endColumn - sel.qualifier.pos.endColumn - sel.name.length
      (sel.qualifier.pos.endColumn + selOffset - rootPosition.startColumn).toExpr
    }
  }

  def getAnchor(expr: Expr[_])(implicit refl: Reflection): Expr[Int] = {
    import refl._
    // -1 to match scala2 position
    // ((expr.unseal.pos.endColumn + expr.unseal.pos.startColumn - 1) / 2 - rootPosition.startColumn).toExpr
    (expr.unseal.pos.startColumn - rootPosition.startColumn).toExpr
  }
}