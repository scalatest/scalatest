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

  def assertNoTypeErrorImpl(c: Context)(code: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._

    val Expr(Literal(Constant(codeStr: String))) = code

    try {
      c.typeCheck(c.parse("{ "+codeStr+" }"))
      val messageExpr = c.literal("Expected a type error, but got none for: " + codeStr)
      reify {
        // Do nothing
      }
    } catch {
      case e: TypecheckException =>
        val messageExpr = c.literal(codeStr + " encountered a type error: " + e.getMessage)
        reify {
          throw new exceptions.TestFailedException(messageExpr.splice, 0)
        }
      case e: ParseException =>
        val messageExpr = c.literal(codeStr + " encountered a parse error: " + e.getMessage)
        reify {
          throw new TestFailedException(messageExpr.splice, 0)
        }
    }
  }

  def notCompileImpl(c: Context)(compileWord: c.Expr[CompileWord])(shouldOrMust: String): c.Expr[Unit] = {

    import c.universe._

    def checkNotCompile(code: String): c.Expr[Unit] = {
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
    }

    val methodName = shouldOrMust + "Not"

    c.macroApplication match {
      case Apply(
             Select(
               Apply(
                 _,
                 List(
                   Literal(Constant(code))
                 )
               ),
               methodNameTermName
             ),
             _
           ) if methodNameTermName.decoded == methodName =>

        val codeStr = code.toString
        checkNotCompile(codeStr)

      case Apply(
             Select(
               Apply(
                 _,
                 List(
                   Select(
                     Apply(
                       Select(
                         _,
                         augmentStringTermName
                       ),
                       List(
                         Literal(
                           Constant(code)
                         )
                       )
                     ),
                     stripMarginTermName
                   )
                 )
               ),
               methodNameTermName
             ),
             _
           ) if augmentStringTermName.decoded == "augmentString" && stripMarginTermName.decoded == "stripMargin" && methodNameTermName.decoded == methodName =>

        val codeStr = code.toString.stripMargin
        checkNotCompile(codeStr)

      case _ => c.abort(c.enclosingPosition, "The '" + shouldOrMust + "Not compile' syntax only works with String literal only.")
    }
  }

  def shouldNotCompileImpl(c: Context)(compileWord: c.Expr[CompileWord]): c.Expr[Unit] =
    notCompileImpl(c)(compileWord)("should")

  def mustNotCompileImpl(c: Context)(compileWord: c.Expr[CompileWord]): c.Expr[Unit] =
    notCompileImpl(c)(compileWord)("must")

  def compileImpl(c: Context)(compileWord: c.Expr[CompileWord])(shouldOrMust: String): c.Expr[Unit] = {
    import c.universe._

    def checkCompile(code: String): c.Expr[Unit] = {
      import c.universe._
      try {
        c.typeCheck(c.parse("{ " + code + " }"))
        reify {
          // Do nothing
        }
      } catch {
        case e: TypecheckException =>
          val messageExpr = c.literal(code + " encountered a type error: " + e.getMessage)
          reify {
            throw new exceptions.TestFailedException(messageExpr.splice, 0)
          }
        case e: ParseException =>
          val messageExpr = c.literal(code + " encountered a parse error: " + e.getMessage)
          reify {
            throw new exceptions.TestFailedException(messageExpr.splice, 0)
          }
      }
    }

    c.macroApplication match {
      case Apply(
             Select(
               Apply(
                 _,
                 List(
                   Literal(
                     Constant(code)
                   )
                 )
               ),
               shouldOrMustTermName
             ),
             _
           ) if shouldOrMustTermName.decoded == shouldOrMust =>

        val codeStr = code.toString
        checkCompile(codeStr)

      case Apply(
             Select(
               Apply(
                 _,
                 List(
                   Select(
                     Apply(
                       Select(
                         _,
                       augmentStringTermName
                       ),
                       List(
                         Literal(
                           Constant(code)
                         )
                       )
                     ),
                   stripMarginTermName
                   )
                 )
               ),
               shouldOrMustTermName
             ),
             _
           ) if augmentStringTermName.decoded == "augmentString" && stripMarginTermName.decoded == "stripMargin" && shouldOrMustTermName.decoded == shouldOrMust =>

        val codeStr = code.toString.stripMargin
        checkCompile(codeStr)

      case _ => c.abort(c.enclosingPosition, "The '" + shouldOrMust + " compile' syntax only works with String literal only.")
    }
  }

  def shouldCompileImpl(c: Context)(compileWord: c.Expr[CompileWord]): c.Expr[Unit] =
    compileImpl(c)(compileWord)("should")

  def mustCompileImpl(c: Context)(compileWord: c.Expr[CompileWord]): c.Expr[Unit] =
    compileImpl(c)(compileWord)("must")

}
