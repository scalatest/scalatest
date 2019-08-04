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
import org.scalatest.matchers.dsl.ResultOfNotWordForAny
import scala.quoted._
import scala.tasty._

private[scalatest] object MatchPatternMacro {

//   /**
//    * Check the case definition AST, raise an compiler error if the body is not empty.
//    */
//   def checkCaseDefinitions(context: Context)(tree: context.Tree): Unit = {
//     import context.universe._

//     // Check if it is a default case
//     def defaultCase(t: Tree): Boolean =
//       t match {
//         case Bind(defaultCaseTermName, Ident(nme.WILDCARD)) if defaultCaseTermName.decoded == "defaultCase$" => true  // default case
//         case _ => false // not default case
//       }

//     tree match {
//       case Typed(Block(List(ClassDef(_, _, _, Template(_, _, List(_, DefDef(_, applyOrElseTermName, _, _, _, Match(_, caseDefList)), _)))), _), _) if applyOrElseTermName.decoded == "applyOrElse" =>
//         // We got a case definition list, let's go through them to check
//         caseDefList.foreach {
//           case CaseDef(pat, _, body) if !defaultCase(pat) => // case definition, and not default case
//             body match {
//               case Literal(Constant(())) => // ok, empty body
//               case _ => context.abort(body.pos, Resources.nonEmptyMatchPatternCase)  // not empty body, raise compiler error
//             }

//           case _ => // other thing, just do nothing
//         }

//       case _ => // other thing, just do nothing
//     }
//   }

  // Do checking on case definition and generate AST that returns a match pattern matcher
  def matchPatternMatcher(right: Expr[PartialFunction[Any, _]])(implicit qctx: QuoteContext): Expr[Matcher[Any]] = {
    // checkCaseDefinitions(context)(tree)

    '{ MatchPatternHelper.matchPatternMatcher($right) }
  }

  /**
   * Check case definitions and generate AST for code that check that the left match the pattern given on the right, which code looks like this:
   *
   * org.scalatest.matchers.MatchPatternHelper.checkMatchPattern(left, right)
   */
  def matchPattern(left: Expr[ResultOfNotWordForAny[_]], right: Expr[PartialFunction[Any, _]])(implicit qctx: QuoteContext): Expr[Unit] = {
    // checkCaseDefinitions(context)(tree)

    '{ MatchPatternHelper.checkMatchPattern($left, $right) }
  }

  def andNotMatchPatternMatcher[T:Type](self: Expr[Matcher[T]#AndNotWord], right: Expr[PartialFunction[Any, _]])(implicit qctx: QuoteContext): Expr[Matcher[T]] = {
    import qctx.tasty._

    val notMatcher = '{ MatchPatternHelper.notMatchPatternMatcher($right) }
    '{ ($self).owner.and($notMatcher) }
  }

  def orNotMatchPatternMatcher[T:Type](self: Expr[Matcher[T]#OrNotWord], right: Expr[PartialFunction[Any, _]])(implicit qctx: QuoteContext): Expr[Matcher[T]] = {
    import qctx.tasty._

    val notMatcher = '{ MatchPatternHelper.notMatchPatternMatcher($right) }
    '{ ($self).owner.or($notMatcher) }
  }
}
