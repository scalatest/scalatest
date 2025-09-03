/*
* Copyright 2001-2025 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalactic.anyvals

import java.util.regex.Pattern
// SKIP-SCALATESTJS,NATIVE-START
import java.util.regex.PatternSyntaxException
// SKIP-SCALATESTJS,NATIVE-END

import scala.quoted.*

import CompileTimeAssertions.*

object RegexStringMacro {

  def isValid(s: String): Boolean =
    checkIsValid(s)._1

  private def checkIsValid(s: String): (Boolean, String) =
    try {
      Pattern.compile(s)
      (true, "")
    }
    catch {
// SKIP-SCALATESTJS,NATIVE-START
      case e: PatternSyntaxException =>
// SKIP-SCALATESTJS,NATIVE-END
//SCALATESTJS,NATIVE-ONLY case e: Exception => // TODO: Figure out exactly what exception JS throws in this case
        (false, "\n" + e.getMessage)
    }

  def apply(value: Expr[String])(using Quotes): Expr[RegexString] = {
    import quotes.reflect.*

    val notValidExceptionMsg: String = {
      value.asTerm match {
          case Literal(stringConst) =>
            checkIsValid(stringConst.value.toString)._2
          case _ =>
            ""
        }
    }

    val notValidMsg =
      "RegexString.apply can only be invoked on String literals that " +
      "represent valid regular expressions." + notValidExceptionMsg
    val notLiteralMsg =
      "RegexString.apply can only be invoked on String literals that " +
      "represent valid regular expressions. Please use RegexString.from " +
      "instead."
    ensureValidStringLiteral(value, notValidMsg, notLiteralMsg)(isValid)

    '{ RegexString.ensuringValid($value) }
  }
}
