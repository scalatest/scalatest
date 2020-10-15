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
import org.scalatest.exceptions._

import scala.quoted._

object CompileMacro {

  // parse and type check a code snippet, generate code to throw TestFailedException when type check passes or parse error
  def assertTypeErrorImpl(code: Expr[String], typeChecked: Expr[Boolean], pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Assertion] = {
    import qctx.tasty._

    if (!typeChecked.unliftOrError) '{ Succeeded }
    else '{
      val messageExpr = Resources.expectedTypeErrorButGotNone($code)
      throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
    }
  }

  def expectTypeErrorImpl(code: Expr[String], typeChecked: Expr[Boolean], prettifier: Expr[Prettifier], pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Fact] = {
    import qctx.tasty._

    if (typeChecked.unliftOrError)
      '{
          val messageExpr = Resources.expectedTypeErrorButGotNone($code)
          Fact.No(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty
          )($prettifier)
       }
    else
      '{
          val messageExpr = Resources.gotTypeErrorAsExpected($code)

          Fact.Yes(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty
          )($prettifier)
       }
  }

  // parse and type check a code snippet, generate code to throw TestFailedException when both parse and type check succeeded
  def assertDoesNotCompileImpl(code: Expr[String], typeChecked: Expr[Boolean], pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Assertion] = {
    import qctx.tasty._

    if (!typeChecked.unliftOrError) '{ Succeeded }
    else '{
      val messageExpr = Resources.expectedCompileErrorButGotNone($code)
      throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
    }
  }

  // parse and type check a code snippet, generate code to return Fact (Yes or No).
  def expectDoesNotCompileImpl(code: Expr[String], typeChecked: Expr[Boolean], prettifier: Expr[Prettifier], pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Fact] = {
    import qctx.tasty._

    if (typeChecked.unliftOrError)
      '{
          val messageExpr = Resources.expectedCompileErrorButGotNone($code)
          Fact.No(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty
          )($prettifier)
       }
    else
      '{
          val messageExpr = Resources.didNotCompile($code)

          Fact.Yes(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty
          )($prettifier)
       }
  }

  // parse and type check a code snippet, generate code to throw TestFailedException when either parse or type check fails.
  def assertCompilesImpl(code: Expr[String], typeChecked: Expr[Boolean], pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Assertion] = {
    import qctx.tasty._

    if (typeChecked.unliftOrError) '{ Succeeded }
    else '{
      val messageExpr = Resources.expectedNoErrorButGotTypeError("unknown", $code)
      throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
    }
  }

  def expectCompilesImpl(code: Expr[String], typeChecked: Expr[Boolean], prettifier: Expr[Prettifier], pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Fact] = {
    import qctx.tasty._

    if (typeChecked.unliftOrError)
      '{
          val messageExpr = Resources.compiledSuccessfully($code)
          Fact.Yes(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty
          )($prettifier)
       }
    else
      '{
          val messageExpr = Resources.expectedNoErrorButGotTypeError("", $code)

          Fact.No(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty
          )($prettifier)
       }
  }
}
