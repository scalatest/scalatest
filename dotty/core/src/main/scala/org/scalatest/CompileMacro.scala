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
package org.scalatest

import org.scalactic._
import org.scalatest.exceptions._

import scala.quoted._
import scala.compiletime.testing.{Error, ErrorKind}

object CompileMacro {

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

  def assertTypeErrorImpl(self: Expr[_], typeChecked: Expr[List[Error]])(using Quotes): Expr[Assertion] = 
    assertOnTypeErrorImpl(self, typeChecked)('{ _ => Succeeded })

  // parse and type check a code snippet, generate code to throw TestFailedException when type check passes or parse error
  def assertOnTypeErrorImpl(self: Expr[_], typeChecked: Expr[List[Error]])(assertion: Expr[String => Assertion])(using Quotes): Expr[Assertion] = {

    import quotes.reflect._

    def checkNotTypeCheck(code: String): Expr[Assertion] = {
      // For some reason `typeChecked.valueOrError` is failing here, so instead we grab
      // the varargs argument to List.apply and use that to extract the list of errors
      val errors = typeChecked.asTerm.underlyingArgument match {
        case Apply(TypeApply(Select(Ident("List"), "apply"), _), List(seq)) =>
          seq.asExprOf[Seq[Error]].valueOrError.toList
      }

      errors match {
        case Error(msg, _, _, ErrorKind.Typer) :: _ => '{ $assertion(${Expr(msg)}) }
        case Error(msg, _, _, ErrorKind.Parser) :: _ => '{
          val messageExpr = Resources.expectedTypeErrorButGotParseError(${ Expr(msg) }, ${ Expr(code) })
          ${
            source.Position.withPosition[Assertion]('{(pos: source.Position) => 
              throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, pos)
            })
          }
        }
        case Nil => '{
          val messageExpr = Resources.expectedTypeErrorButGotNone(${ Expr(code) })
          ${
            source.Position.withPosition[Assertion]('{(pos: source.Position) => 
              throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, pos)
            })
          }
        }
      }
    }

    self.asTerm.underlyingArgument match {

      case Literal(StringConstant(code)) =>
        checkNotTypeCheck(code.toString)

      case Apply(Select(_, "stripMargin"), List(Literal(StringConstant(code)))) =>
        checkNotTypeCheck(code.toString.stripMargin)

      case _ =>
        report.throwError("The 'assertTypeError' function only works with String literals.")
    }
  }

  def expectTypeErrorImpl(self: Expr[_], typeChecked: Expr[List[Error]], prettifier: Expr[Prettifier])(using Quotes): Expr[Fact] = {
    
    import quotes.reflect._
    
    def checkNotTypeCheck(code: String): Expr[Fact] = {
      // For some reason `typeChecked.valueOrError` is failing here, so instead we grab
      // the varargs argument to List.apply and use that to extract the list of errors
      val errors = typeChecked.asTerm.underlyingArgument match {
        case Apply(TypeApply(Select(Ident("List"), "apply"), _), List(seq)) =>
          seq.asExprOf[Seq[Error]].valueOrError.toList
      }

      errors match {
        case Error(_, _, _, ErrorKind.Typer) :: _ => '{
          val messageExpr = Resources.gotTypeErrorAsExpected(${ Expr(code) })
          Fact.Yes(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            $prettifier
          ) 
        }
        case Error(msg, _, _, ErrorKind.Parser) :: _ => '{
          val messageExpr = Resources.expectedTypeErrorButGotParseError(${ Expr(msg) }, ${ Expr(code) })
          Fact.No(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            $prettifier
          )
        }
        case Nil => '{
          val messageExpr = Resources.expectedTypeErrorButGotNone(${ Expr(code) })
          Fact.No(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            $prettifier
          )
        }
      }
    }

    self.asTerm.underlyingArgument match {

      case Literal(StringConstant(code)) =>
        checkNotTypeCheck(code.toString)

      case Apply(Select(_, "stripMargin"), List(Literal(StringConstant(code)))) =>
        checkNotTypeCheck(code.toString.stripMargin)

      case _ =>
        report.throwError("The 'assertTypeError' function only works with String literals.")
    }
  }

  // parse and type check a code snippet, generate code to throw TestFailedException when both parse and type check succeeded
  def assertDoesNotCompileImpl(code: Expr[String], typeChecked: Expr[Boolean])(using Quotes): Expr[Assertion] = {
    if (!typeChecked.valueOrError) '{ Succeeded }
    else '{
      val messageExpr = Resources.expectedCompileErrorButGotNone($code)
      ${
        source.Position.withPosition[Assertion]('{(pos: source.Position) => 
          throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, pos)
        })
      }
    }
  }

  // parse and type check a code snippet, generate code to return Fact (Yes or No).
  def expectDoesNotCompileImpl(code: Expr[String], typeChecked: Expr[Boolean], prettifier: Expr[Prettifier])(using Quotes): Expr[Fact] = {
    if (typeChecked.valueOrError)
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
            Vector.empty, 
            $prettifier
          )
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
            Vector.empty, 
            $prettifier
          )
       }
  }

  // parse and type check a code snippet, generate code to throw TestFailedException when either parse or type check fails.
  def assertCompilesImpl(self: Expr[_], typeChecked: Expr[List[Error]])(using Quotes): Expr[Assertion] = {

    import quotes.reflect._

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
          ${
            source.Position.withPosition[Assertion]('{(pos: source.Position) => 
              throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, pos)
            })
          }
        }
        case Error(msg, _, _, ErrorKind.Parser) :: _ => '{
          val messageExpr = Resources.expectedNoErrorButGotParseError(${ Expr(msg) }, ${ Expr(code) })
          ${
            source.Position.withPosition[Assertion]('{(pos: source.Position) => 
              throw new TestFailedException((_: StackDepthException) => Some(messageExpr), None, pos)
            })
          }
        }
        case Nil => '{ Succeeded }
      }
    }

    self.asTerm.underlyingArgument match {

      case Literal(StringConstant(code)) =>
        checkCompile(code.toString)

      case Apply(Select(_, "stripMargin"), List(Literal(StringConstant(code)))) =>
        checkCompile(code.toString.stripMargin)

      case _ =>
        report.throwError("The 'assertCompiles' function only works with String literals.")
    }
  }

  def expectCompilesImpl(self: Expr[_], typeChecked: Expr[List[Error]], prettifier: Expr[Prettifier])(using Quotes): Expr[Fact] = {
    
    import quotes.reflect._

    def checkCompile(code: String): Expr[Fact] = {
      // For some reason `typeChecked.valueOrError` is failing here, so instead we grab
      // the varargs argument to List.apply and use that to extract the list of errors
      val errors = typeChecked.asTerm.underlyingArgument match {
        case Apply(TypeApply(Select(Ident("List"), "apply"), _), List(seq)) =>
          seq.asExprOf[Seq[Error]].valueOrError.toList
      }

      errors match {
        case Error(msg, _, _, ErrorKind.Typer) :: _ => '{
          val messageExpr = Resources.expectedNoErrorButGotTypeError(${ Expr(msg) }, ${ Expr(code) })
          Fact.No(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            $prettifier
          )
        }
        case Error(msg, _, _, ErrorKind.Parser) :: _ => '{
          val messageExpr = Resources.expectedNoErrorButGotParseError(${ Expr(msg) }, ${ Expr(code) })
          Fact.No(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            $prettifier
          )
        }
        case Nil => '{ 
          val messageExpr = Resources.compiledSuccessfully(${ Expr(code) })
          Fact.Yes(
            messageExpr,
            messageExpr,
            messageExpr,
            messageExpr,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            $prettifier
          )
        }
      }
    }

    self.asTerm.underlyingArgument match {

      case Literal(StringConstant(code)) =>
        checkCompile(code.toString)

      case Apply(Select(_, "stripMargin"), List(Literal(StringConstant(code)))) =>
        checkCompile(code.toString.stripMargin)

      case _ =>
        report.throwError("The 'expectCompiles' function only works with String literals.")
    }
  }
}
