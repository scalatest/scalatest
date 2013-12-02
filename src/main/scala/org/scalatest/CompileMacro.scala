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
import scala.reflect.macros.{ Context, TypecheckException }
import org.scalatest.exceptions.StackDepthException._
import org.scalatest.exceptions.StackDepthExceptionHelper._

object CompileMacro {

  //def compile(code: String): Unit = macro compileImpl

  def assertTypeCheckImpl(c: Context)(code: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._

    val Expr(Literal(Constant(codeStr: String))) = code

    val (result, message) =
      try {
        c.typeCheck(c.parse("{ "+codeStr+" }"))
        (true, "")
      } catch {
        case e: Throwable =>
          (false, e.getMessage)
      }

    val resultExpr = c.literal(result)
    val messageExpr = c.literal(message)
    val fileNameExpr = c.literal("CompileMacro.scala")
    val methodNameExpr = c.literal("newAssertionFailedException")
    reify {
      if (!resultExpr.splice)
        //throw newAssertionFailedException(Some(messageExpr.splice), fileNameExpr.splice, methodNameExpr.splice)
        throw new exceptions.TestFailedException(messageExpr.splice, 4)
    }

  }

  def newAssertionFailedException(optionalMessage: Option[String], fileName: String, methodName: String): Throwable =
    new exceptions.TestFailedException(toExceptionFunction(optionalMessage), None, getStackDepthFun(fileName, methodName, 0))

}