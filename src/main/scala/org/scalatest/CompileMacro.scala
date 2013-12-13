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

import scala.language.experimental.macros
import scala.reflect.macros.{ Context, TypecheckException, ParseException }
import org.scalatest.exceptions.StackDepthException._
import org.scalatest.exceptions.StackDepthExceptionHelper._
import org.scalatest.words.CompileWord

private[scalatest] object CompileMacro {

  def assertTypeErrorImpl(c: Context)(code: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._

    val Expr(Literal(Constant(codeStr: String))) = code

    try {
      c.typeCheck(c.parse("{ "+codeStr+" }"))
      val messageExpr = c.literal("Expected a type error, but got none for: " + codeStr)
      reify {
        throw new exceptions.TestFailedException(messageExpr.splice, 0)
      }
    } catch {
      case e: TypecheckException =>
        reify {
          // Do nothing
        }
      case e: ParseException =>
        val messageExpr = c.literal("Expected type error, but get parse error: " + e.getMessage + "\nfor: " + codeStr)
        reify {
          throw new TestFailedException(messageExpr.splice, 0)
        }
    }
  }

  def shouldNotCompileImpl(c: Context)(compileWord: c.Expr[CompileWord]): c.Expr[Unit] = {
    import c.universe._

    c.macroApplication match {
      case Apply(Select(Apply(_, List(Literal(Constant(codeStr)))), _), _) =>
        val code = codeStr.toString

        try {
          c.typeCheck(c.parse("{ " + code + " }"))
          val messageExpr = c.literal("Expected a type error, but got none for: " + code)
          reify {
            throw new exceptions.TestFailedException(messageExpr.splice, 0)
          }
        } catch {
          case e: TypecheckException =>
            reify {
              // Do nothing
            }
          case e: ParseException =>
            val messageExpr = c.literal("Expected type error, but get parse error: " + e.getMessage + "\nfor: " + code)
            reify {
              throw new TestFailedException(messageExpr.splice, 0)
            }
        }
      case _ => c.abort(c.enclosingPosition, "The 'shouldNot compile' syntax only works with String literal only.")
    }
  }

  def mustNotCompileImpl(c: Context)(compileWord: c.Expr[CompileWord]): c.Expr[Unit] = {
    import c.universe._

    c.macroApplication match {
      case Apply(Select(Apply(_, List(Literal(Constant(codeStr)))), _), _) =>
        //val codeStr = c.macroApplication.asInstanceOf[Apply].fun.asInstanceOf[Select].qualifier.asInstanceOf[Apply].args(0).toString
        val code = codeStr.toString

        try {
          c.typeCheck(c.parse("{ " + code + " }"))
          val messageExpr = c.literal("Expected a type error, but got none for: " + code)
          reify {
            throw new exceptions.TestFailedException(messageExpr.splice, 0)
          }
        } catch {
          case e: TypecheckException =>
            reify {
              // Do nothing
            }
          case e: ParseException =>
            val messageExpr = c.literal("Expected type error, but get parse error: " + e.getMessage + "\nfor: " + code)
            reify {
              throw new TestFailedException(messageExpr.splice, 0)
            }
        }
      case _ => c.abort(c.enclosingPosition, "The 'mustNot compile' syntax only works with String literal only.")
    }
  }

}