/*
 * Copyright 2001-2013 Artima, Inc.
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

import org.scalautils.DiagrammedBool

trait DiagrammedAssertions extends Assertions {

  import language.experimental.macros

  class DiagrammedAssertionsHelper {

    private def append(currentMessage: Option[String], clue: Any) = {
      val clueStr = clue.toString
      if (clueStr.isEmpty)
        currentMessage
      else {
        currentMessage match {
          case Some(msg) =>
            // clue.toString.head is guaranteed to work, because the previous if check that clue.toString != ""
            val firstChar = clueStr.head
            if (firstChar.isWhitespace || firstChar == '.' || firstChar == ',' || firstChar == ';')
              Some(msg + clueStr)
            else
              Some(msg + " " + clueStr)
          case None => Some(clueStr)
        }
      }
    }

    def macroAssert(bool: DiagrammedBool, clue: Any) {
      if (clue == null)
        throw new NullPointerException("clue was null")
      if (!bool.value) {
        val failureMessage = if (DiagrammedBool.isSimpleWithoutExpressionText(bool)) None else Some(bool.failureMessage)
        throw newAssertionFailedException(append(failureMessage, clue), None, "Assertions.scala", "macroAssert", 2)
      }
    }

    def macroAssume(bool: DiagrammedBool, clue: Any) {
      if (clue == null)
        throw new NullPointerException("clue was null")
      if (!bool.value) {
        val failureMessage = if (DiagrammedBool.isSimpleWithoutExpressionText(bool)) None else Some(bool.failureMessage)
        throw newTestCanceledException(append(failureMessage, clue), None, "Assertions.scala", "macroAssume", 2)
      }
    }
  }

  val diagrammedAssertionsHelper = new DiagrammedAssertionsHelper

  override def assert(condition: Boolean): Unit = macro DiagrammedAssertionsMacro.assert

}

object DiagrammedAssertions extends DiagrammedAssertions