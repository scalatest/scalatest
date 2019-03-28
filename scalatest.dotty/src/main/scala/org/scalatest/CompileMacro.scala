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
import org.scalatest.words.{TypeCheckWord, CompileWord}
import org.scalatest.exceptions._

import scala.quoted._
import scala.tasty._

object CompileMacro {

  // parse and type check a code snippet, generate code to throw TestFailedException when type check passes or parse error
  def assertTypeErrorImpl(code: String, pos: Expr[source.Position])(implicit refl: Reflection): Expr[Assertion] = {
    import refl._
    import quoted.Toolbox.Default._

    if (!typing.typeChecks(code)) '{ Succeeded }
    else '{
      val messageExpr = Resources.expectedCompileErrorButGotNone(${ code.toExpr })
      throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
    }
  }

  def expectTypeErrorImpl(code: String, prettifier: Expr[Prettifier], pos: Expr[source.Position])(implicit refl: Reflection): Expr[Fact] = {
    import refl._

    if (typing.typeChecks(code))
      '{
          val messageExpr = Resources.expectedTypeErrorButGotNone(${ code.toExpr })
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
          val messageExpr = Resources.expectedTypeErrorButGotNone(${ code.toExpr })

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
  def assertDoesNotCompileImpl(code: String, pos: Expr[source.Position])(implicit refl: Reflection): Expr[Assertion] = {
    import refl._

    if (!typing.typeChecks(code)) '{ Succeeded }
    else '{
      val messageExpr = Resources.expectedCompileErrorButGotNone(${ code.toExpr })
      throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
    }
  }

  // parse and type check a code snippet, generate code to return Fact (Yes or No).
  def expectDoesNotCompileImpl(code: String, prettifier: Expr[Prettifier], pos: Expr[source.Position])(implicit refl: Reflection): Expr[Fact] = {
    import refl._

    if (typing.typeChecks(code))
      '{
          val messageExpr = Resources.expectedTypeErrorButGotNone(${ code.toExpr })
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
          val messageExpr = Resources.expectedTypeErrorButGotNone(${ code.toExpr })

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
  def assertCompilesImpl(code: String, pos: Expr[source.Position])(implicit refl: Reflection): Expr[Assertion] = {
    import refl._

    if (typing.typeChecks(code)) '{ Succeeded }
    else '{
      val messageExpr = Resources.expectedCompileErrorButGotNone(${ code.toExpr })
      throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
    }
  }

  def expectCompilesImpl(code: String, prettifier: Expr[Prettifier], pos: Expr[source.Position])(implicit refl: Reflection): Expr[Fact] = {
    import refl._

    if (typing.typeChecks(code))
      '{
          val messageExpr = Resources.expectedTypeErrorButGotNone(${ code.toExpr })
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
          val messageExpr = Resources.expectedTypeErrorButGotNone(${ code.toExpr })

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

  // check that a code snippet does not compile
  def assertNotCompileImpl(compileWord: Expr[CompileWord], pos: Expr[source.Position])(shouldOrMust: String)(implicit refl: Reflection): Expr[Assertion] = {
    import refl._

    // parse and type check a code snippet, generate code to throw TestFailedException if both parse and type check succeeded
    def checkNotCompile(code: String): Expr[Assertion] =
      if (!typing.typeChecks(code)) '{ Succeeded }
      else '{
        val messageExpr = Resources.expectedCompileErrorButGotNone(${ code.toExpr })
        throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
      }

    ???
  }

  // used by shouldNot compile syntax, delegate to assertNotCompileImpl to generate code
  def shouldNotCompileImpl(compileWord: Expr[CompileWord])(pos: Expr[source.Position])(implicit refl: Reflection): Expr[Assertion] =
    assertNotCompileImpl(compileWord, pos)("should")

  // check that a code snippet does not compile
  def assertNotTypeCheckImpl(typeCheckWord: Expr[TypeCheckWord], pos: Expr[source.Position])(shouldOrMust: String)(implicit refl: Reflection): Expr[Assertion] = {
    import refl._

    // parse and type check a code snippet, generate code to throw TestFailedException if both parse and type check succeeded
    def checkNotCompile(code: String): Expr[Assertion] =
      if (!typing.typeChecks(code)) '{ Succeeded }
      else '{
        val messageExpr = Resources.expectedCompileErrorButGotNone(${ code.toExpr })
        throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
      }

    ???
  }

  // used by shouldNot typeCheck syntax, delegate to assertNotTypeCheckImpl to generate code
  def shouldNotTypeCheckImpl(typeCheckWord: Expr[TypeCheckWord])(pos: Expr[source.Position])(implicit refl: Reflection): Expr[Assertion] =
    assertNotTypeCheckImpl(typeCheckWord, pos)("should")

  // used by mustNot typeCheck syntax, delegate to assertNotTypeCheckImpl to generate code
  def mustNotTypeCheckImpl(typeCheckWord: Expr[TypeCheckWord])(pos: Expr[source.Position])(implicit refl: Reflection): Expr[Assertion] =
    assertNotTypeCheckImpl(typeCheckWord, pos)("must")

  // check that a code snippet compiles
  def assertCompileImpl(compileWord: Expr[CompileWord], pos: Expr[source.Position])(shouldOrMust: String)(implicit refl: Reflection): Expr[Assertion] = {
    import refl._

    // parse and type check a code snippet, generate code to throw TestFailedException if both parse and type check succeeded
    def checkCompile(code: String): Expr[Assertion] =
      if (typing.typeChecks(code)) '{ Succeeded }
      else '{
        val messageExpr = Resources.expectedNoErrorButGotTypeError("", ${ code.toExpr })
        throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
      }

    ???
  }

  // used by should compile syntax, delegate to assertCompileImpl to generate code
  def shouldCompileImpl(compileWord: Expr[CompileWord])(pos: Expr[source.Position])(implicit refl: Reflection): Expr[Assertion] =
    assertCompileImpl(compileWord, pos)("should")
}
