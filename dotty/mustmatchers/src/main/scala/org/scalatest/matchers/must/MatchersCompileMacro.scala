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
package org.scalatest.matchers.must

import org.scalactic._
import org.scalatest.verbs.{TypeCheckWord, CompileWord}
import org.scalatest.exceptions._
import org.scalatest._

import scala.quoted._

object MatchersCompileMacro {

  // used by mustNot compile syntax, delegate to assertNotCompileImpl to generate code
  def mustNotCompileImpl(self: Expr[org.scalatest.matchers.must.Matchers#AnyMustWrapper[_]], compileWord: Expr[CompileWord])(pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Assertion] =
    org.scalatest.matchers.MatchersCompileMacro.assertNotCompileImpl(self, compileWord, pos)("must")

  // check that a code snippet does not compile
  def assertNotTypeCheckImpl(self: Expr[org.scalatest.matchers.must.Matchers#AnyMustWrapper[_]], typeCheckWord: Expr[TypeCheckWord], pos: Expr[source.Position])(shouldOrMust: String)(implicit qctx: QuoteContext): Expr[Assertion] = {
    import qctx.tasty._

    // parse and type check a code snippet, generate code to throw TestFailedException if both parse and type check succeeded
    def checkNotTypeCheck(code: String): Expr[Assertion] =
      if (/*!typing.typeChecks(code)*/ true) '{ Succeeded } // FIXME
      else '{
        val messageExpr = Resources.expectedTypeErrorButGotNone(${ code.toExpr })
        throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
      }

    val methodName = shouldOrMust + "Not"

    self.unseal.underlyingArgument match {
      case Apply(
             Apply(
               Select(_, shouldOrMustTerconvertToStringShouldOrMustWrapperTermName),
               List(
                 Literal(code)
               )
             ),
             _
           ) if shouldOrMustTerconvertToStringShouldOrMustWrapperTermName ==  "convertToString" + shouldOrMust.capitalize + "Wrapper" =>
        // LHS is a normal string literal, call checkNotTypeCheck with the extracted code string to generate code
        checkNotTypeCheck(code.toString)

      case Apply(
             Apply(
               Ident(shouldOrMustTerconvertToStringShouldOrMustWrapperTermName),
               List(
                 Literal(Constant(code: String))
               )
             ),
             _
           ) if shouldOrMustTerconvertToStringShouldOrMustWrapperTermName ==  "convertToString" + shouldOrMust.capitalize + "Wrapper" =>
        // LHS is a normal string literal, call checkNotTypeCheck with the extracted code string to generate code
        checkNotTypeCheck(code.toString)

      case _ =>
        qctx.error("The '" + shouldOrMust + "Not typeCheck' syntax only works with String literals.")
        '{???}
    }
  }

  // used by mustNot typeCheck syntax, delegate to assertNotTypeCheckImpl to generate code
  def mustNotTypeCheckImpl(self: Expr[org.scalatest.matchers.must.Matchers#AnyMustWrapper[_]], typeCheckWord: Expr[TypeCheckWord])(pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Assertion] =
    assertNotTypeCheckImpl(self, typeCheckWord, pos)("must")

  // used by should compile syntax, delegate to assertCompileImpl to generate code
  def mustCompileImpl(self: Expr[org.scalatest.matchers.must.Matchers#AnyMustWrapper[_]], compileWord: Expr[CompileWord])(pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Assertion] =
    org.scalatest.matchers.MatchersCompileMacro.assertCompileImpl(self, compileWord, pos)("must")
}
