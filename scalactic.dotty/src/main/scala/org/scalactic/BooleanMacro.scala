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

import scala.quoted._

object BooleanMacro {
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

  def parse(condition: Expr[Boolean], prettifier: Expr[Prettifier])(implicit qctx: QuoteContext): Expr[Bool] = {
    import qctx.tasty._
    import util._

    def exprStr: String = condition.show
    def defaultCase = '{ Bool.simpleMacroBool($condition, ${Expr(exprStr)}, $prettifier) }
    def isImplicitMethodType(tp: Type): Boolean = tp match {
      case tp: MethodType => tp.isImplicit
      case _ => false
    }

    def isByNameMethodType(tp: Type): Boolean =  tp.widen match {
      case MethodType(_, ByNameType(_) :: Nil, _) => true
      case _ => false
    }

    // use in `exists(_ == e)` or `exists(e == _)`
    //
    // Note: Scala2 implementation implicitly assumes `e` is side effect free,
    //       we do the same. A better approach would be to check `e` is Ident or
    //       Literal.
    //
    // {
    //   def $anonfun(_$12: Int): Boolean = _$12.==(2)
    //   closure($anonfun)
    // }
    object AnonFunction {
      def unapply(t: Term): Option[Term] = t match {
        case Block(
          ddef @
            DefDef(_, Nil, (ValDef(name, _, _) :: Nil) :: Nil, _,
              Some(Apply(Select(lhs, "=="), rhs :: Nil))
            ) :: Nil,
          clos
        ) if (clos.tpe.isFunctionType) => // walkaround: https://github.com/lampepfl/dotty/issues/6720
          (lhs, rhs) match {
            case (Ident(refName), _) if refName == name => Some(rhs)
            case (_, Ident(refName)) if refName == name => Some(lhs)
            case _ => None
          }
        case _ => None
      }
    }

    condition.unseal.underlyingArgument match {
      case Apply(sel @ Select(Apply(qual, lhs :: Nil), op @ ("===" | "!==")), rhs :: Nil) =>
        let(lhs) { left =>
          let(rhs) { right =>
            let(qual.appliedTo(left).select(sel.symbol).appliedTo(right)) { result =>
              val l = left.seal
              val r = right.seal
              val b = result.seal.cast[Boolean]
              val code = '{ Bool.binaryMacroBool($l, ${ Expr(op) }, $r, $b, $prettifier) }
              code.unseal
            }
          }
        }.seal.cast[Bool]

      case Apply(sel @ Select(lhs, op), rhs :: Nil) =>
        def binaryDefault =
          if (isByNameMethodType(sel.tpe)) defaultCase
          else if (supportedBinaryOperations.contains(op))
            let(lhs) { left =>
              let(rhs) { right =>
                val app = left.select(sel.symbol).appliedTo(right)
                let(app) { result =>
                  val l = left.seal
                  val r = right.seal
                  val b = result.seal.cast[Boolean]
                  val code = '{ Bool.binaryMacroBool($l, ${Expr(op)}, $r, $b, $prettifier) }
                  code.unseal
                }
              }
            }.seal.cast[Bool]
          else defaultCase

        op match {
          case "||" =>
            val left = parse(lhs.seal.cast[Boolean], prettifier)
            val right = parse(rhs.seal.cast[Boolean], prettifier)
            '{ $left || $right }
          case "|" =>
            val left = parse(lhs.seal.cast[Boolean], prettifier)
            val right = parse(rhs.seal.cast[Boolean], prettifier)
            '{ $left | $right }
          case "&&" =>
            val left = parse(lhs.seal.cast[Boolean], prettifier)
            val right = parse(rhs.seal.cast[Boolean], prettifier)
            '{ $left && $right }
          case "&" =>
            val left = parse(lhs.seal.cast[Boolean], prettifier)
            val right = parse(rhs.seal.cast[Boolean], prettifier)
            '{ $left & $right }
          case "==" =>
            lhs match {
              case Apply(sel @ Select(lhs0, op @ ("length" | "size")), Nil) =>
                let(lhs0) { left =>
                  let(rhs) { right =>
                    val actual = left.select(sel.symbol).appliedToArgs(Nil)
                    let(actual) { result =>
                      val l = left.seal
                      val r = right.seal
                      val res = result.seal
                      val code = '{ Bool.lengthSizeMacroBool($l, ${Expr(op)}, $res, $r, $prettifier) }
                      code.unseal
                    }
                  }
                }.seal.cast[Bool]

              case sel @ Select(lhs0, op @ ("length" | "size")) =>
                let(lhs0) { left =>
                  let(rhs) { right =>
                    val actual = left.select(sel.symbol)
                    let(actual) { result =>
                      val l = left.seal
                      val r = right.seal
                      val res = result.seal
                      val code = '{ Bool.lengthSizeMacroBool($l, ${Expr(op)}, $res, $r, $prettifier) }
                      code.unseal
                    }
                  }
                }.seal.cast[Bool]

              case _ =>
                binaryDefault
            }
          case "exists" =>
            rhs match {
              case AnonFunction(rhsInner) => // see the assumption for `rhsInner` in `AnonFunction`
                let(lhs) { left =>
                  val app = left.select(sel.symbol).appliedTo(rhs)
                  let(app) { result =>
                    val l = left.seal
                    val r = rhsInner.seal
                    val res = result.seal.cast[Boolean]
                    val code = '{ Bool.existsMacroBool($l, $r, $res, $prettifier) }
                    code.unseal
                  }
                }.seal.cast[Bool]
              case _ => defaultCase
            }
          case _ =>
            binaryDefault
        }

      case Apply(f @ Apply(sel @ Select(Apply(qual, lhs :: Nil), op @ ("===" | "!==")), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        let(lhs) { left =>
          let(rhs) { right =>
            val app = qual.appliedTo(left).select(sel.symbol).appliedTo(right).appliedToArgs(implicits)
            let(app) { result =>
              val l = left.seal
              val r = right.seal
              val b = result.seal.cast[Boolean]
              val code = '{ Bool.binaryMacroBool($l, ${ Expr(op) }, $r, $b, $prettifier) }
              code.unseal
            }
          }
        }.seal.cast[Bool]

      case Apply(TypeApply(sel @ Select(lhs, op), targs), rhs :: Nil) =>
        let(lhs) { left =>
          let(rhs) { right =>
            val app = left.select(sel.symbol).appliedToTypes(targs.map(_.tpe)).appliedTo(right)
            let(app) { result =>
              val l = left.seal
              val r = right.seal
              val b = result.seal.cast[Boolean]
              val code = '{ Bool.binaryMacroBool($l, ${Expr(op)}, $r, $b, $prettifier) }
              code.unseal
            }
          }
        }.seal.cast[Bool]

      case Apply(sel @ Select(lhs, op @ ("isEmpty" | "nonEmpty")), Nil) =>
        let(lhs) { l =>
          val res = l.select(sel.symbol).appliedToArgs(Nil).seal.cast[Boolean]
          '{ Bool.unaryMacroBool(${l.seal}, ${ Expr(op) }, $res, $prettifier) }.unseal
        }.seal.cast[Bool]

      case Select(left, "unary_!") =>
        val receiver = parse(left.seal.cast[Boolean], prettifier)
        '{ !($receiver) }

      case sel @ Select(left, op @ ("isEmpty" | "nonEmpty")) =>
        let(left) { l =>
          val res = l.select(sel.symbol).seal.cast[Boolean]
          '{ Bool.unaryMacroBool(${l.seal}, ${ Expr(op) }, $res, $prettifier) }.unseal
        }.seal.cast[Bool]

      case TypeApply(sel @ Select(lhs, "isInstanceOf"), targs) =>
        let(lhs) { l =>
          val res = l.select(sel.symbol).appliedToTypeTrees(targs).seal.cast[Boolean]
          val name = Expr(targs.head.tpe.show)
          '{ Bool.isInstanceOfMacroBool(${l.seal}, "isInstanceOf", $name, $res, $prettifier) }.unseal
        }.seal.cast[Bool]

      case Literal(_) =>
        '{ Bool.simpleMacroBool($condition, "", $prettifier) }

      case _ =>
        defaultCase
    }
  }
}
