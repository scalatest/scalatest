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

import org.scalautils.{AnchorValue, DiagrammedExpr, Prettifier}
import scala.collection.mutable.ListBuffer
import collection.immutable.TreeMap

trait DiagrammedAssertions extends Assertions {

  import language.experimental.macros

  class DiagrammedAssertionsHelper {

    private[this] def fits(line: StringBuilder, str: String, anchor: Int): Boolean =
      line.slice(anchor, anchor + str.length + 1).forall(_.isWhitespace)

    private[this] def placeString(line: StringBuilder, str: String, anchor: Int) {
      val diff = anchor - line.length
      for (i <- 1 to diff) line.append(' ')
      line.replace(anchor, anchor + str.length(), str)
    }

    private[this] def renderValue(value: Any): String = {
      value match {
        case aEqualizer: org.scalautils.TripleEqualsSupport#Equalizer[_] => Prettifier.default(aEqualizer.leftSide)
        case aEqualizer: org.scalautils.TripleEqualsSupport#CheckingEqualizer[_] => Prettifier.default(aEqualizer.leftSide)
        case _ => Prettifier.default(value)
      }
    }

    private[this] def placeValue(lines: ListBuffer[StringBuilder], value: Any, col: Int) {
      val str = renderValue(value)

      placeString(lines(0), "|", col)

      for (line <- lines.drop(1)) {
        if (fits(line, str, col)) {
          placeString(line, str, col)
          return
        }
        placeString(line, "|", col)
      }

      val newLine = new StringBuilder()
      placeString(newLine, str, col)
      lines.append(newLine)
    }

    private[this] def filterAndSortByAnchor(anchorValues: List[AnchorValue]): Traversable[AnchorValue] = {
      var map = TreeMap[Int, AnchorValue]()(Ordering.by(-_))
      // values stemming from compiler generated code often have the same anchor as regular values
      // and get recorded before them; let's filter them out
      for (value <- anchorValues) if (!map.contains(value.anchor)) map += (value.anchor -> value)
      map.values
    }

    private[this] def renderDiagram(sourceText: String, anchorValues: List[AnchorValue]): String = {
      val offset = sourceText.prefixLength(_.isWhitespace)
      val intro = new StringBuilder().append(sourceText.trim())
      val lines = ListBuffer(new StringBuilder)

      val rightToLeft = filterAndSortByAnchor(anchorValues)
      for (anchorValue <- rightToLeft) placeValue(lines, anchorValue.value, anchorValue.anchor - offset)

      lines.prepend(intro)
      lines.append(new StringBuilder)
      lines.mkString("\n")
    }

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

    def macroAssert(bool: DiagrammedExpr[Boolean], clue: Any, sourceText: String) {
      if (clue == null)
        throw new NullPointerException("clue was null")
      if (!bool.value) {
        //val failureMessage = if (DiagrammedBool.isSimpleWithoutExpressionText(bool)) None else Some(bool.failureMessage)
        val failureMessage =
          Some(
            "\n" +
            renderDiagram(sourceText, bool.anchorValues)
          )
        throw newAssertionFailedException(append(failureMessage, clue), None, "Assertions.scala", "macroAssert", 2)
      }
    }
  }

  val diagrammedAssertionsHelper = new DiagrammedAssertionsHelper

  override def assert(condition: Boolean): Unit = macro DiagrammedAssertionsMacro.assert

}

object DiagrammedAssertions extends DiagrammedAssertions