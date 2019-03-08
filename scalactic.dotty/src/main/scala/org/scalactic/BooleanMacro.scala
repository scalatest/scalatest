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
import scala.tasty._

object BooleanMacro {
  def parse(condition: Expr[Boolean], prettifier: Expr[Prettifier])(implicit refl: Reflection): Expr[Bool] = {
    import refl._
    import quoted.Toolbox.Default._
    import util._

    def exprStr: String = condition.show
    def defaultCase = '{ Bool.simpleMacroBool($condition, ${exprStr.toExpr}, $prettifier) }
    def isImplicitMethodType(tp: Type): Boolean =
      Type.IsMethodType.unapply(tp).flatMap(tp => if tp.isImplicit then Some(true) else None).nonEmpty

    condition.unseal.underlyingArgument match {
      case Term.Apply(Term.Select(Term.Apply(qual, lhs :: Nil), op @ ("===" | "!==")), rhs :: Nil) =>
        let(lhs) { left =>
          let(rhs) { right =>
            let(Term.Select.overloaded(Term.Apply(qual, left :: Nil), op, Nil, right :: Nil)) { result =>
              val l = left.seal[Any]
              val r = right.seal[Any]
              val b = result.seal[Boolean]
              val code = '{ Bool.binaryMacroBool($l, ${ op.toExpr }, $r, $b, $prettifier) }
              code.unseal
            }
          }
        }.seal[Bool]
      case Term.Apply(sel @ Term.Select(lhs, op), rhs :: Nil) =>
        op match {
          case "||" =>
            val left = parse(lhs.seal[Boolean], prettifier)
            val right = parse(rhs.seal[Boolean], prettifier)
            '{ $left || $right }
          case "|" =>
            val left = parse(lhs.seal[Boolean], prettifier)
            val right = parse(rhs.seal[Boolean], prettifier)
            '{ $left | $right }
          case "&&" =>
            val left = parse(lhs.seal[Boolean], prettifier)
            val right = parse(rhs.seal[Boolean], prettifier)
            '{ $left && $right }
          case "&" =>
            val left = parse(lhs.seal[Boolean], prettifier)
            val right = parse(rhs.seal[Boolean], prettifier)
            '{ $left & $right }
          case _ =>
            sel.tpe.widen match {
              case Type.MethodType(_, Type.ByNameType(_) :: Nil, _) =>
                defaultCase
              case _ =>
                let(lhs) { left =>
                  let(rhs) { right =>
                    val app = Term.Select.overloaded(left, op, Nil, right :: Nil)
                    assert(app.symbol == sel.symbol, app.symbol.fullName -> sel.symbol.fullName)
                    let(app) { result =>
                      val l = left.seal[Any]
                      val r = right.seal[Any]
                      val b = result.seal[Boolean]
                      val code = '{ Bool.binaryMacroBool($l, ${op.toExpr}, $r, $b, $prettifier) }
                      code.unseal
                    }
                  }
                }.seal[Bool]
            }
        }
      case Term.Apply(f @ Term.Apply(Term.Select(Term.Apply(qual, lhs :: Nil), op @ ("===" | "!==")), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        let(lhs) { left =>
          let(rhs) { right =>
            let(Term.Apply(Term.Select.overloaded(Term.Apply(qual, left :: Nil), op, Nil, right :: Nil), implicits)) { result =>
              val l = left.seal[Any]
              val r = right.seal[Any]
              val b = result.seal[Boolean]
              val code = '{ Bool.binaryMacroBool($l, ${ op.toExpr }, $r, $b, $prettifier) }
              code.unseal
            }
          }
        }.seal[Bool]
      case Term.Select(left, "unary_!") =>
        val receiver = parse(left.seal[Boolean], prettifier)
        '{ !($receiver) }
      case Term.Literal(_) =>
        '{ Bool.simpleMacroBool($condition, "", $prettifier) }
      case _ =>
        defaultCase
    }
  }
}
