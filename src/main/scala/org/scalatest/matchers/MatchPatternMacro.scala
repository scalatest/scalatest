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
package org.scalatest.matchers

import reflect.macros.Context
import org.scalatest.Resources

private[scalatest] object MatchPatternMacro {

  def checkCaseDefinitions(context: Context)(tree: context.Tree) {
    import context.universe._

    def defaultCase(t: Tree): Boolean =
      t match {
        case Bind(defaultCaseTermName, Ident(nme.WILDCARD)) if defaultCaseTermName.decoded == "defaultCase$" => true
        case _ => false
      }

    tree match {
      case Typed(Block(List(ClassDef(_, _, _, Template(_, _, List(_, DefDef(_, applyOrElseTermName, _, _, _, Match(_, caseDefList)), _)))), _), _) if applyOrElseTermName.decoded == "applyOrElse" =>
        caseDefList.foreach {
          case CaseDef(pat, _, body) if !defaultCase(pat) =>
            body match {
              case Literal(Constant(())) => // ok, empty body
              case _ => context.abort(body.pos, Resources("nonEmptyMatchPatternCase"))
            }

          case _ =>
        }

      case _ =>
    }
  }

  def matchPatternMatcher(context: Context)(right: context.Expr[PartialFunction[Any, _]]): context.Expr[Matcher[Any]] = {
    import context.universe._

    val tree = right.tree

    checkCaseDefinitions(context)(tree)

    val callHelper =
      Apply(
        Select(
          Select(
            Select(
              Select(
                Ident(newTermName("org")),
                newTermName("scalatest")
              ),
              newTermName("matchers")
            ),
            newTermName("MatchPatternHelper")
          ),
          newTermName("matchPatternMatcher")
        ),
        List(tree)
      )

    context.Expr(callHelper)
  }

  def notMatchPatternMatcher(context: Context)(right: context.Expr[PartialFunction[Any, _]]): context.Expr[Matcher[Any]] = {
    import context.universe._

    val tree = right.tree

    checkCaseDefinitions(context)(tree)

    val callHelper =
      Apply(
        Select(
          Select(
            Select(
              Select(
                Ident(newTermName("org")),
                newTermName("scalatest")
              ),
              newTermName("matchers")
            ),
            newTermName("MatchPatternHelper")
          ),
          newTermName("notMatchPatternMatcher")
        ),
        List(tree)
      )

    context.Expr(callHelper)
  }

  def andNotMatchPatternMatcher(context: Context)(right: context.Expr[PartialFunction[Any, _]]): context.Expr[Matcher[Any]] = {
    import context.universe._

    val tree = right.tree

    checkCaseDefinitions(context)(tree)

    val notMatcher =
      Apply(
        Select(
          Select(
            Select(
              Select(
                Ident(newTermName("org")),
                newTermName("scalatest")
              ),
              newTermName("matchers")
            ),
            newTermName("MatchPatternHelper")
          ),
          newTermName("notMatchPatternMatcher")
        ),
        List(tree)
      )

    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          Apply(
            Select(
              Select(
                qualifier,
                "owner"
              ),
              newTermName("and")
            ),
            List(notMatcher)
          )
        case _ => context.abort(context.macroApplication.pos, "This macro should be used with 'and not' syntax only.")
      }

    context.Expr(callHelper)
  }

  def orNotMatchPatternMatcher(context: Context)(right: context.Expr[PartialFunction[Any, _]]): context.Expr[Matcher[Any]] = {
    import context.universe._

    val tree = right.tree

    checkCaseDefinitions(context)(tree)

    val notMatcher =
      Apply(
        Select(
          Select(
            Select(
              Select(
                Ident(newTermName("org")),
                newTermName("scalatest")
              ),
              newTermName("matchers")
            ),
            newTermName("MatchPatternHelper")
          ),
          newTermName("notMatchPatternMatcher")
        ),
        List(tree)
      )

    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          Apply(
            Select(
              Select(
                qualifier,
                "owner"
              ),
              newTermName("or")
            ),
            List(notMatcher)
          )
        case _ => context.abort(context.macroApplication.pos, "This macro should be used with 'or not' syntax only.")
      }

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
              Select(
                Select(
                  Select(
                    Ident(newTermName("org")),
                    newTermName("scalatest")
                  ),
                  newTermName("matchers")
                ),
                newTermName("MatchPatternHelper")
              ),
              newTermName("checkPatternMatcher")
            ),
            List(qualifier, tree)
          )

        case _ => context.abort(context.macroApplication.pos, "This macro should be used with should not syntax only.")
      }

    context.Expr(callHelper)
  }

}
