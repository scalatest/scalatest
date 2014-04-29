/*
 * Copyright 2001-2014 Artima, Inc.
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

import reflect.macros.Context

private[scalatest] object MatchPatternMacro {

  def checkCaseDefinitions(context: Context)(tree: context.Tree) {
    import context.universe._

    def defaultCase(t: Tree): Boolean =
      t match {
        case Bind(TermName("defaultCase$"), Ident(termNames.WILDCARD)) => true
        case _ => false
      }

    tree match {
      case Typed(Block(List(ClassDef(_, _, _, Template(_, _, List(_, DefDef(_, TermName("applyOrElse"), _, _, _, Match(_, caseDefList)), _)))), _), _) =>
        caseDefList.foreach {
          case CaseDef(pat, _, body) if !defaultCase(pat) =>
            body match {
              case Literal(Constant(())) => // ok, empty body
              case _ => context.abort(body.pos, "case definition must contain empty body.")
            }

          case _ =>
        }

      case _ =>
    }
  }

  def matchPatternMatcher(context: Context)(right: context.Expr[PartialFunction[Any, _]]): context.Expr[_] = {
    import context.universe._

    val tree = right.tree

    checkCaseDefinitions(context)(tree)

    val callHelper =
      Apply(
        Select(
          Ident(newTermName("matchPatternHelper")),
          newTermName("matchPatternMatcher")
        ),
        List(tree)
      )

    context.Expr(callHelper)
  }

  def matchPattern(context: Context)(right: context.Expr[PartialFunction[Any, _]]): context.Expr[_] = {
    import context.universe._

    val tree = right.tree

    checkCaseDefinitions(context)(tree)

    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          Apply(
            Select(
              Ident(newTermName("matchPatternHelper")),
              newTermName("checkPatternMatcher")
            ),
            List(qualifier, tree)
          )
        case _ => context.abort(context.macroApplication.pos, "This macro should be used with should not syntax only.")
      }

    context.Expr(callHelper)
  }

}