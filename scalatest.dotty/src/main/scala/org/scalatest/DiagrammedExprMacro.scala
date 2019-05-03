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

    expr.unseal match {
      case Apply(Select(New(_), _), _) => simpleExpr(expr)  // delegate to simpleExpr if it is a New expression
      case IsApply(apply) if isXmlSugar(apply) => simpleExpr(expr)
      case IsApply(apply) if isJavaStatic(apply) => simpleExpr(expr)
      case IsApply(apply) => applyExpr(expr) // delegate to applyExpr if it is Apply
      case IsTypeApply(apply) => applyExpr(expr) // delegate to applyExpr if it is Apply
      case Select(This(_), _) => simpleExpr(expr) // delegate to simpleExpr if it is a Select for this, e.g. referring a to instance member.
      case IsSelect(x) if x.symbol.flags.is(Flags.Object) => simpleExpr(expr) // don't traverse packages
      case IsSelect(x) if isJavaStatic(x) => simpleExpr(expr)
      case IsSelect(select) => selectExpr(expr) // delegate to selectExpr if it is a Select
      case Block(stats, expr) =>
        Block(stats, parse(expr.seal.cast[T]).unseal).seal.cast[DiagrammedExpr[T]] // call parse recursively using the expr argument if it is a block
      case _ => simpleExpr(expr) // for others, just delegate to simpleExpr
    }
  }

  def applyExpr[T:Type](expr: Expr[T])(implicit refl: Reflection): Expr[DiagrammedExpr[T]] = {
    import refl._

    def apply(l: Expr[_], name: String, r: List[Expr[_]]): Expr[T] =
      Select.overloaded(l.unseal, name, Nil, r.map(_.unseal)).seal.cast[T]

    expr.unseal.underlyingArgument match {
      case Apply(Select(lhs, op), rhs :: Nil) =>
        op match {
          case "||" | "|" =>
            val left = parse(lhs.seal.cast[T & Boolean])
            val right = parse(rhs.seal.cast[T & Boolean])
            '{
              val l = $left
              val r = $right
              if (l.value) l
              else DiagrammedExpr.applyExpr(l, r :: Nil, r.value, ${ getAnchor(expr) })
            }
          case "&&" | "&" =>
            val left = parse(lhs.seal.cast[T & Boolean])
            val right = parse(rhs.seal.cast[T & Boolean])
            '{
              val l = $left
              val r = $right
              if (l.value) DiagrammedExpr.applyExpr(l, r :: Nil, r.value, ${ getAnchor(expr) })
              else l
            }
          case _ =>
            val left = parse(lhs.seal.cast[Any])
            val right = parse(rhs.seal.cast[Any])
            '{
              val l = $left
              val r = $right
              val res = ${ apply('{l.value}, op, '{r.value} :: Nil) }
              DiagrammedExpr.applyExpr(l, r :: Nil, res, ${ getAnchor(expr) })
            }
        }
      case Apply(Select(lhs, op), args) =>
        val left = parse(lhs.seal)
        val rights = args.map(arg => parse(arg.seal))

        let(left) { (l: Expr[DiagrammedExpr[_]]) =>
          lets(rights) { (rs: List[Expr[DiagrammedExpr[_]]]) =>
            val res = apply('{($l).value}, op, rs)
            '{ DiagrammedExpr.applyExpr($l, ${rs.toExprOfList}, $res, ${getAnchor(expr)}) }
          }
        }
      case _ =>
        simpleExpr(expr)
    }
  }

  def selectExpr[T:Type](expr: Expr[T])(implicit refl: Reflection): Expr[DiagrammedExpr[T]] = {
    import refl._

    def selectField(o: Expr[_], name: String): Expr[T] = ???

    expr.unseal match {
      case Select(qual, name) =>
        val obj = parse(qual.seal)

        '{
          val o = $obj
          DiagrammedExpr.selectExpr(o, ${ selectField('{o.value}, name) }, ${ getAnchor(expr) })
        }
    }
  }

  def transform(
    helper: Expr[(DiagrammedExpr[Boolean], Any, String, source.Position) => Assertion],
    condition: Expr[Boolean], pos: Expr[source.Position], clue: Expr[Any], sourceText: String
  )(implicit refl: Reflection): Expr[Assertion] = {
    val diagExpr = parse(condition)
    '{ $helper($diagExpr, $clue, ${sourceText.toExpr}, $pos) }
  }


  /**
   * For a given expression (passed in as tree), generate AST for the following code:
   *
   * org.scalatest.DiagrammedExpr.simpleExpr(expr, anchorOfExpr)
   */
  def simpleExpr[T:Type](expr: Expr[T])(implicit refl: Reflection): Expr[DiagrammedExpr[T]] = {
    '{ DiagrammedExpr.simpleExpr($expr, ${ getAnchor(expr) } ) }
  }

  def getAnchor(expr: Expr[_])(implicit refl: Reflection): Expr[Int] = {
    import refl._
    (expr.unseal.pos.endColumn - expr.unseal.pos.startColumn).toExpr
  }
}