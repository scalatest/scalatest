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
package org.scalactic

import scala.quoted._

object BooleanMacro {
  // Reference: https://www.scala-lang.org/api/3.0.0/scala/quoted/Quotes$reflectModule.html

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

  def parse(condition: Expr[Boolean], prettifier: Expr[Prettifier])(using Quotes): Expr[Bool] = {
    import quotes.reflect._
    import quotes.reflect.ValDef.let
    import util._

    def exprStr: String = condition.show
    def defaultCase = '{ Bool.simpleMacroBool($condition, ${Expr(exprStr)}, $prettifier) }
    def isImplicitMethodType(tp: TypeRepr): Boolean = tp match {
      case tp: MethodType => tp.isImplicit
      case _ => false
    }

    def isByNameMethodType(tp: TypeRepr): Boolean =  tp.widen match {
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
            DefDef(_, (ValDef(name, _, _) :: Nil) :: Nil, _,
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

    condition.asTerm.underlyingArgument match {
      case Apply(sel @ Select(Apply(qual, lhs :: Nil), op @ ("===" | "!==")), rhs :: Nil) =>
        let(Symbol.spliceOwner, lhs) { left =>
          let(Symbol.spliceOwner, rhs) { right =>
            let(Symbol.spliceOwner, qual.appliedTo(left).select(sel.symbol).appliedTo(right)) { result =>
              val l = left.asExpr
              val r = right.asExpr
              val b = result.asExprOf[Boolean]
              val code = '{ Bool.binaryMacroBool($l, ${ Expr(op) }, $r, $b, $prettifier) }
              code.asTerm
            }
          }
        }.asExprOf[Bool]

      case Apply(Apply(TypeApply(sel @ Select(lhs, op @ ("===" | "!==")), targs), rhs :: Nil), implicitArgs) =>
        let(Symbol.spliceOwner, lhs) { left =>
          let(Symbol.spliceOwner, rhs) { right =>
            let(Symbol.spliceOwner, lhs.select(sel.symbol).appliedToTypeTrees(targs).appliedTo(right).appliedToArgs(implicitArgs)) { result =>
              val l = left.asExpr
              val r = right.asExpr
              val b = result.asExprOf[Boolean]
              val code = '{ Bool.binaryMacroBool($l, ${ Expr(op) }, $r, $b, $prettifier) }
              code.asTerm
            }
          }
        }.asExprOf[Bool]

      case Apply(sel @ Select(lhs, op), rhs :: Nil) =>
        def binaryDefault =
          if (isByNameMethodType(sel.tpe)) defaultCase
          else if (supportedBinaryOperations.contains(op)) {
            let(Symbol.spliceOwner, lhs) { left =>
              let(Symbol.spliceOwner, rhs) { right =>
                val app = left.select(sel.symbol).appliedTo(right)
                let(Symbol.spliceOwner, app) { result =>
                  val l = left.asExpr
                  val r = right.asExpr
                  val b = result.asExprOf[Boolean]
                  val code = '{ Bool.binaryMacroBool($l, ${Expr(op)}, $r, $b, $prettifier) }
                  code.asTerm
                }
              }
            }.asExprOf[Bool]
          }
          else defaultCase

        op match {
          case "||" =>
            val left = parse(lhs.asExprOf[Boolean], prettifier)
            val right = parse(rhs.asExprOf[Boolean], prettifier)
            '{ $left || $right }
          case "|" =>
            val left = parse(lhs.asExprOf[Boolean], prettifier)
            val right = parse(rhs.asExprOf[Boolean], prettifier)
            '{ $left | $right }
          case "&&" =>
            val left = parse(lhs.asExprOf[Boolean], prettifier)
            val right = parse(rhs.asExprOf[Boolean], prettifier)
            '{ $left && $right }
          case "&" =>
            val left = parse(lhs.asExprOf[Boolean], prettifier)
            val right = parse(rhs.asExprOf[Boolean], prettifier)
            '{ $left & $right }
          case "==" =>
            lhs match {
              case Apply(sel @ Select(lhs0, op @ ("length" | "size")), Nil) =>
                let(Symbol.spliceOwner, lhs0) { left =>
                  let(Symbol.spliceOwner, rhs) { right =>
                    val actual = left.select(sel.symbol).appliedToArgs(Nil)
                    let(Symbol.spliceOwner, actual) { result =>
                      val l = left.asExpr
                      val r = right.asExpr
                      val res = result.asExpr
                      val code = '{ Bool.lengthSizeMacroBool($l, ${Expr(op)}, $res, $r, $prettifier) }
                      code.asTerm
                    }
                  }
                }.asExprOf[Bool]

              case sel @ Select(lhs0, op @ ("length" | "size")) =>
                let(Symbol.spliceOwner, lhs0) { left =>
                  let(Symbol.spliceOwner, rhs) { right =>
                    val actual = left.select(sel.symbol)
                    let(Symbol.spliceOwner, actual) { result =>
                      val l = left.asExpr
                      val r = right.asExpr
                      val res = result.asExpr
                      val code = '{ Bool.lengthSizeMacroBool($l, ${Expr(op)}, $res, $r, $prettifier) }
                      code.asTerm
                    }
                  }
                }.asExprOf[Bool]

              case _ =>
                binaryDefault
            }

          case ">" | "<" | ">=" | "<=" =>
            lhs match {
              case Apply(
                     Apply(
                       TypeApply(
                         Ident(infixOrderingOps), 
                         List(_)
                       ), 
                       List(wrapped)
                     ), 
                     List(_)
                   ) if infixOrderingOps == "infixOrderingOps" =>
                let(Symbol.spliceOwner, lhs) { left =>
                  let(Symbol.spliceOwner, rhs) { right =>
                    val app = left.select(sel.symbol).appliedTo(right)
                    let(Symbol.spliceOwner, app) { result =>
                      val l = wrapped.asExpr
                      val r = right.asExpr
                      val b = result.asExprOf[Boolean]
                      val code = '{ Bool.binaryMacroBool($l, ${Expr(op)}, $r, $b, $prettifier) }
                      code.asTerm
                    }
                  }
                }.asExprOf[Bool]
                        
              case _ => binaryDefault
            }

          case "exists" =>
            rhs match {
              case AnonFunction(rhsInner) => // see the assumption for `rhsInner` in `AnonFunction`
                let(Symbol.spliceOwner, lhs) { left =>
                  val app = left.select(sel.symbol).appliedTo(rhs)
                  let(Symbol.spliceOwner, app) { result =>
                    val l = left.asExpr
                    val r = rhsInner.asExpr
                    val res = result.asExprOf[Boolean]
                    val code = '{ Bool.existsMacroBool($l, $r, $res, $prettifier) }
                    code.asTerm
                  }
                }.asExprOf[Bool]
              case _ => defaultCase
            }
          case _ =>
            binaryDefault
        }

      case Apply(f @ Apply(sel @ Select(Apply(qual, lhs :: Nil), op @ ("===" | "!==")), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        let(Symbol.spliceOwner, lhs) { left =>
          let(Symbol.spliceOwner, rhs) { right =>
            val app = qual.appliedTo(left).select(sel.symbol).appliedTo(right).appliedToArgs(implicits)
            let(Symbol.spliceOwner, app) { result =>
              val l = left.asExpr
              val r = right.asExpr
              val b = result.asExprOf[Boolean]
              val code = '{ Bool.binaryMacroBool($l, ${ Expr(op) }, $r, $b, $prettifier) }
              code.asTerm
            }
          }
        }.asExprOf[Bool]

      case Apply(TypeApply(sel @ Select(lhs, op), targs), rhs :: Nil) =>
        let(Symbol.spliceOwner, lhs) { left =>
          let(Symbol.spliceOwner, rhs) { right =>
            val app = left.select(sel.symbol).appliedToTypes(targs.map(_.tpe)).appliedTo(right)
            let(Symbol.spliceOwner, app) { result =>
              val l = left.asExpr
              val r = right.asExpr
              val b = result.asExprOf[Boolean]
              val code = '{ Bool.binaryMacroBool($l, ${Expr(op)}, $r, $b, $prettifier) }
              code.asTerm
            }
          }
        }.asExprOf[Bool]

      case Apply(sel @ Select(lhs, op @ ("isEmpty" | "nonEmpty")), Nil) =>
        let(Symbol.spliceOwner, lhs) { l =>
          val res = l.select(sel.symbol).appliedToArgs(Nil).asExprOf[Boolean]
          '{ Bool.unaryMacroBool(${l.asExpr}, ${ Expr(op) }, $res, $prettifier) }.asTerm
        }.asExprOf[Bool]

      case Select(left, "unary_!") =>
        val receiver = parse(left.asExprOf[Boolean], prettifier)
        '{ !($receiver) }

      case sel @ Select(left, op @ ("isEmpty" | "nonEmpty")) =>
        let(Symbol.spliceOwner, left) { l =>
          val res = l.select(sel.symbol).asExprOf[Boolean]
          '{ Bool.unaryMacroBool(${l.asExpr}, ${ Expr(op) }, $res, $prettifier) }.asTerm
        }.asExprOf[Bool]

      case TypeApply(sel @ Select(lhs, "isInstanceOf"), targs) =>
        let(Symbol.spliceOwner, lhs) { l =>
          val res = l.select(sel.symbol).appliedToTypeTrees(targs).asExprOf[Boolean]
          val name = Expr(targs.head.tpe.show)
          '{ Bool.isInstanceOfMacroBool(${l.asExpr}, "isInstanceOf", $name, $res, $prettifier) }.asTerm
        }.asExprOf[Bool]

      case Literal(_) =>
        '{ Bool.simpleMacroBool($condition, "", $prettifier) }

      case _ =>
        defaultCase
    }
  }
}
