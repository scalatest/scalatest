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
package org.scalatest.matchers

import org.scalactic._
import org.scalatest.{Assertion, Succeeded, Resources}
import org.scalatest.exceptions.{TestFailedException, StackDepthException}
import org.scalatest.verbs.{CompileWord, TypeCheckWord}

import scala.quoted._

object CompileMacro {

  // check that a code snippet compiles
  def assertCompileImpl[T](self: Expr[T], compileWord: Expr[CompileWord], pos: Expr[source.Position])(shouldOrMust: String)(implicit qctx: QuoteContext): Expr[Assertion] = {
    import qctx.tasty._

    // parse and type check a code snippet, generate code to throw TestFailedException if both parse and type check succeeded
    def checkCompile(code: String): Expr[Assertion] =
      if (/*typeChecks(code)*/ true) '{ Succeeded } // FIXME
      else '{
        val messageExpr = Resources.expectedNoErrorButGotTypeError("", ${ Expr(code) })
        throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
      }

    self.unseal.underlyingArgument match {

      case Apply(
             Apply(
               Select(_, shouldOrMustTerconvertToStringShouldOrMustWrapperTermName),
               List(
                 Literal(Constant(code: String))
               )
             ),
             _
           ) if shouldOrMustTerconvertToStringShouldOrMustWrapperTermName ==  "convertToString" + shouldOrMust.capitalize + "Wrapper" =>
        // LHS is a normal string literal, call checkCompile with the extracted code string to generate code
        checkCompile(code.toString)

      case Apply(
             Apply(
               Ident(shouldOrMustTerconvertToStringShouldOrMustWrapperTermName),
               List(
                 Literal(Constant(code: String))
               )
             ),
             _
           ) if shouldOrMustTerconvertToStringShouldOrMustWrapperTermName ==  "convertToString" + shouldOrMust.capitalize + "Wrapper" =>
        // LHS is a normal string literal, call checkCompile with the extracted code string to generate code
        checkCompile(code.toString)

      case other =>
        qctx.error("The '" + shouldOrMust + " compile' syntax only works with String literals.")
        '{???}
    }
  }

  // check that a code snippet does not compile
  def assertNotCompileImpl[T](self: Expr[T], compileWord: Expr[CompileWord], pos: Expr[source.Position])(shouldOrMust: String)(implicit qctx: QuoteContext): Expr[Assertion] = {
    import qctx.tasty._

    // parse and type check a code snippet, generate code to throw TestFailedException if both parse and type check succeeded
    def checkNotCompile(code: String): Expr[Assertion] =
      if (/*!typeChecks(code)*/ true) '{ Succeeded } // FIXME
      else '{
        val messageExpr = Resources.expectedCompileErrorButGotNone(${ Expr(code) })
        throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
      }

    self.unseal.underlyingArgument match {

      case Apply(
             Apply(
               Select(_, shouldOrMustTerconvertToStringShouldOrMustWrapperTermName),
               List(
                 Literal(Constant(code: String))
               )
             ),
             _
           ) if shouldOrMustTerconvertToStringShouldOrMustWrapperTermName ==  "convertToString" + shouldOrMust.capitalize + "Wrapper" =>
        // LHS is a normal string literal, call checkCompile with the extracted code string to generate code
        checkNotCompile(code)

      case Apply(
             Apply(
               Ident(shouldOrMustTerconvertToStringShouldOrMustWrapperTermName),
               List(
                 Literal(Constant(code: String))
               )
             ),
             _
           ) if shouldOrMustTerconvertToStringShouldOrMustWrapperTermName ==  "convertToString" + shouldOrMust.capitalize + "Wrapper" =>
        checkNotCompile(code)

      case other =>
        qctx.error("The '" + shouldOrMust + " compile' syntax only works with String literals.")
        '{???}
    }
  }

  // check that a code snippet does not compile
  def assertNotTypeCheckImpl(self: Expr[_], typeCheckWord: Expr[TypeCheckWord], pos: Expr[source.Position])(shouldOrMust: String)(implicit qctx: QuoteContext): Expr[Assertion] = {
    import qctx.tasty._

    // parse and type check a code snippet, generate code to throw TestFailedException if both parse and type check succeeded
    def checkNotTypeCheck(code: String): Expr[Assertion] =
      if (/*!typeChecks(code)*/ true) '{ Succeeded } // FIXME
      else '{
        val messageExpr = Resources.expectedTypeErrorButGotNone(${ Expr(code) })
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

}