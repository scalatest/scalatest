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

import scala.xml.Elem
import Suite.reportMarkupProvided
import Doc.stripMargin
import Doc.trimMarkup
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import collection.mutable.ListBuffer

// TODO: Will need a finder.
private[scalatest] abstract class DocSpec extends DocSpecLike {

  /**
   * Returns a user friendly string for this suite, composed of the
   * simple name of the class (possibly simplified further by removing dollar signs if
   * added by the Scala interpeter) and, if this suite
   * contains nested suites, the result of invoking <code>toString</code> on each
   * of the nested suites, separated by commas and surrounded by parentheses.
   *
   * @return a user-friendly string for this suite
   */
  override def toString: String = Suite.suiteToString(None, this)
}

private[scalatest] object DocSpec {

  def trimMarkup(text: String): String = {
    val lines = text.linesIterator.toList
    val zipLines = lines.zipWithIndex
    val firstNonWhiteLine = zipLines.find { case (line, _) => !line.trim.isEmpty }
    val lastNonWhiteLine = zipLines.reverse.find { case (line, _) => !line.trim.isEmpty }
    (firstNonWhiteLine, lastNonWhiteLine) match {
      case (Some((_, frontIdx)), Some((_, backIdx))) => lines.take(backIdx + 1).drop(frontIdx).mkString("\n")
      case _ => text.trim // Will be either (None, None) or (Some, Some)
    }
  }

  def stripMargin(text: String): String = {
    val lines = text.linesIterator.toList
    val firstNonWhiteLine = lines.find(!_.trim.isEmpty)
    firstNonWhiteLine match {
      case None => text.trim
      case Some(nonWhiteLine) =>
        val initialWhite = nonWhiteLine.dropWhile(_.isWhitespace)
        val margin =  nonWhiteLine.length - initialWhite.length
        val choppedLines = lines map { line =>
          val strip = if (line.length > margin) margin else line.length
          line.substring(strip)
        }
        choppedLines.mkString("\n")
    }
  }
}

