/*
 * Copyright 2001-2024 Artima, Inc.
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
import scala.compiletime.testing.{Error, ErrorKind}

object CompileMacro {

  // check that a code snippet compiles
  def assertCompileImpl[T](self: Expr[T], typeChecked: Expr[List[Error]], compileWord: Expr[CompileWord], pos: Expr[source.Position])(shouldOrMust: String)(using Quotes): Expr[Assertion] = {
    import quotes.reflect._

    // parse and type check a code snippet, generate code to throw TestFailedException if both parse and type check succeeded
    def checkCompile(code: String): Expr[Assertion] = {
      // For some reason `typeChecked.valueOrError` is failing here, so instead we grab
      // the varargs argument to List.apply and use that to extract the list of errors
      val errors = typeChecked.asTerm.underlyingArgument match {
        case Apply(TypeApply(Select(Ident("List"), "apply"), _), List(seq)) =>
          seq.asExprOf[Seq[Error]].valueOrError.toList
      }

      errors match {
        case Error(msg, _, _, ErrorKind.Typer) :: _ => '{
          val messageExpr = Resources.expectedNoErrorButGotTypeError(${ Expr(msg) }, ${ Expr(code) })
          throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
        }
        case Error(msg, _, _, ErrorKind.Parser) :: _ => '{
          val messageExpr = Resources.expectedNoErrorButGotParseError(${ Expr(msg) }, ${ Expr(code) })
          throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
        }
        case Nil => '{ Succeeded }
      }
    }

    self.asTerm.underlyingArgument match {

      case Literal(StringConstant(code)) =>  
        // LHS is a normal string literal, call checkCompile with the extracted code string to generate code
        checkCompile(code.toString)

      case Apply(Select(_, "stripMargin"), List(Literal(StringConstant(code)))) =>
        checkCompile(code.stripMargin.toString)  

      case other =>
        report.throwError("The '" + shouldOrMust + " compile' syntax only works with String literals.")
    }
  }

  // check that a code snippet does not compile
  def assertNotCompileImpl[T](self: Expr[T], typeChecked: Expr[Boolean], compileWord: Expr[CompileWord], pos: Expr[source.Position])(shouldOrMust: String)(using Quotes): Expr[Assertion] = {
    import quotes.reflect._

    // parse and type check a code snippet, generate code to throw TestFailedException if both parse and type check succeeded
    def checkNotCompile(code: String): Expr[Assertion] =
      if (!typeChecked.valueOrError) '{ Succeeded }
      else '{
        val messageExpr = Resources.expectedCompileErrorButGotNone(${ Expr(code) })
        throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
      }

    self.asTerm.underlyingArgument match {

      case Literal(StringConstant(code)) =>  
        // LHS is a normal string literal, call checkCompile with the extracted code string to generate code
        checkNotCompile(code.toString)

      case Apply(Select(_, "stripMargin"), List(Literal(StringConstant(code)))) =>
        checkNotCompile(code.stripMargin.toString)  

      case other =>
        report.throwError("The '" + shouldOrMust + " compile' syntax only works with String literals.")
    }
  }

  given FromExpr[ErrorKind] with {
    def unapply(expr: Expr[ErrorKind])(using Quotes) = expr match {
      case '{ ErrorKind.Parser } => Some(ErrorKind.Parser)
      case '{ ErrorKind.Typer }  => Some(ErrorKind.Typer)
      case _ => None
    }
  }

  given FromExpr[Error] with {
    def unapply(expr: Expr[Error])(using Quotes) = expr match {
      case '{ Error(${Expr(msg)}, ${Expr(line)}, ${Expr(col)}, ${Expr(kind)}) } => Some(Error(msg, line, col, kind))
      case _ => None
    }
  }

  // check that a code snippet does not type check
  def assertNotTypeCheckImpl(self: Expr[_], typeChecked: Expr[List[Error]], typeCheckWord: Expr[TypeCheckWord], pos: Expr[source.Position])(shouldOrMust: String)(using Quotes): Expr[Assertion] = {
    import quotes.reflect._

    // parse and type check a code snippet
    // generate code to throw TestFailedException if there is a parse error or type checking succeeds
    def checkNotTypeCheck(code: String): Expr[Assertion] = {
      // For some reason `typeChecked.valueOrError` is failing here, so instead we grab
      // the varargs argument to List.apply and use that to extract the list of errors
      val errors = typeChecked.asTerm.underlyingArgument match {
        case Apply(TypeApply(Select(Ident("List"), "apply"), _), List(seq)) =>
          seq.asExprOf[Seq[Error]].valueOrError.toList
      }

      errors match {
        case Error(_, _, _, ErrorKind.Typer) :: _ => '{ Succeeded }
        case Error(msg, _, _, ErrorKind.Parser) :: _ => '{
          val messageExpr = Resources.expectedTypeErrorButGotParseError(${ Expr(msg) }, ${ Expr(code) })
          throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
        }
        case Nil => '{
          val messageExpr = Resources.expectedTypeErrorButGotNone(${ Expr(code) })
          throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, $pos)
        }
      }
    }

    self.asTerm.underlyingArgument match {

      case Literal(StringConstant(code)) =>
        checkNotTypeCheck(code.toString)

      case Apply(Select(_, "stripMargin"), List(Literal(StringConstant(code)))) =>
        checkNotTypeCheck(code.stripMargin)

      case _ =>
        report.throwError("The '" + shouldOrMust + "Not typeCheck' syntax only works with String literals.")
    }
  }

}
