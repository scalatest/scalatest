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

import org.scalatest.Resources
import scala.reflect.macros.whitebox.Context

private[scalatest] object MatchPatternMacro {

  /**
   * Check the case definition AST, raise an compiler error if the body is not empty.
   */
  def checkCaseDefinitions(context: Context)(tree: context.Tree): Unit = {
    import context.universe._

    // Check if it is a default case
    def defaultCase(t: Tree): Boolean =
      t match {
        case Bind(defaultCaseTermName, Ident(termNames.WILDCARD)) if defaultCaseTermName.decodedName.toString == "defaultCase$" => true  // default case
        case _ => false // not default case
      }

    tree match {
      case Typed(Block(List(ClassDef(_, _, _, Template(_, _, List(_, DefDef(_, applyOrElseTermName, _, _, _, Match(_, caseDefList)), _)))), _), _) if applyOrElseTermName.decodedName.toString == "applyOrElse" =>
        // We got a case definition list, let's go through them to check
        caseDefList.foreach {
          case CaseDef(pat, _, body) if !defaultCase(pat) => // case definition, and not default case
            body match {
              case Literal(Constant(())) => // ok, empty body
              case _ => context.abort(body.pos, Resources.nonEmptyMatchPatternCase)  // not empty body, raise compiler error
            }

          case _ => // other thing, just do nothing
        }

      case _ => // other thing, just do nothing
    }
  }

  // Do checking on case definition and generate AST that returns a match pattern matcher
  def matchPatternMatcher(context: Context)(right: context.Expr[PartialFunction[Any, _]]): context.Expr[Matcher[Any]] = {
    import context.universe._

    val tree = right.tree

    // check case definitions
    checkCaseDefinitions(context)(tree)

    /**
     * Generate AST for the following code:
     *
     * org.scalatest.matchers.MatchPatternHelper.matchPatternHelper(partialFunction)
     */
    val callHelper =
      Apply(
        Select(
          Select(
            Select(
              Select(
                Ident(TermName("org")),
                TermName("scalatest")
              ),
              TermName("matchers")
            ),
            TermName("MatchPatternHelper")
          ),
          TermName("matchPatternMatcher")
        ),
        List(tree)
      )

    context.Expr(callHelper)
  }

  // Do checking on case definition and generate AST that returns a negated match pattern matcher
  def notMatchPatternMatcherTree(context: Context)(right: context.Expr[PartialFunction[Any, _]]): context.Tree = {
    import context.universe._

    val tree = right.tree

    // check case definitions
    checkCaseDefinitions(context)(tree)

    /**
     * Generate AST for the following code:
     *
     * org.scalatest.matchers.MatchPatternHelper.notMatchPatternMatcher(partialFunction)
     */
    Apply(
      Select(
        Select(
          Select(
            Select(
              Ident(TermName("org")),
              TermName("scalatest")
            ),
            TermName("matchers")
          ),
          TermName("MatchPatternHelper")
        ),
        TermName("notMatchPatternMatcher")
      ),
      List(tree)
    )
  }

  // Generate AST that returns a negated match pattern matcher expression
  def notMatchPatternMatcher(context: Context)(right: context.Expr[PartialFunction[Any, _]]): context.Expr[Matcher[Any]] =
    context.Expr(notMatchPatternMatcherTree(context)(right))

  // Do checking on case definition and generate AST that does a 'and not' logical expression matcher.
  def andNotMatchPatternMatcher(context: Context)(right: context.Expr[PartialFunction[Any, _]]): context.Expr[Matcher[Any]] = {
    import context.universe._

    val tree = right.tree

    // Generate a negated matcher by calling notMatchPatternMatcher
    val notMatcher = notMatchPatternMatcherTree(context)(right)

    /**
     * Generate AST for code that call the 'and' method on the Matcher instance (reference through 'owner'):
     *
     * owner.and(notMatcher)
     */
    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          Apply(
            Select(
              Select(
                qualifier,
                TermName("owner")
              ),
              TermName("and")
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

    // Generate a negated matcher by calling notMatchPatternMatcher
    val notMatcher = notMatchPatternMatcherTree(context)(right)

    /**
     * Generate AST for code that call the 'and' method on the Matcher instance (reference through 'owner'):
     *
     * owner.or(notMatcher)
     */
    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          Apply(
            Select(
              Select(
                qualifier,
                TermName("owner")
              ),
              TermName("or")
            ),
            List(notMatcher)
          )
        case _ => context.abort(context.macroApplication.pos, "This macro should be used with 'or not' syntax only.")
      }

    context.Expr(callHelper)
  }

  /**
   * Check case definitions and generate AST for code that check that the left match the pattern given on the right, which code looks like this:
   *
   * org.scalatest.matchers.MatchPatternHelper.checkPatternMatcher(left, right)
   */
  def matchPattern(context: Context)(right: context.Expr[PartialFunction[Any, _]]): context.Expr[Unit] = {
    import context.universe._

    val tree = right.tree

    // check case definitions
    checkCaseDefinitions(context)(tree)

    /**
     * Generate AST for the following code:
     *
     * org.scalatest.matchers.MatchPatternHelper.checkPatternMatcher(left, right)
     */
    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          Apply(
            Select(
              Select(
                Select(
                  Select(
                    Ident(TermName("org")),
                    TermName("scalatest")
                  ),
                  TermName("matchers")
                ),
                TermName("MatchPatternHelper")
              ),
              TermName("checkMatchPattern")
            ),
            List(qualifier, tree)
          )

        case _ => context.abort(context.macroApplication.pos, "This macro should be used with should not syntax only.")
      }

    context.Expr(callHelper)
  }

}
