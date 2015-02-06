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
package org.scalatest.tools

import Fragment.countLeadingEOLs
import Fragment.countTrailingEOLs
import PrintReporter.ansiReset

private[scalatest] case class Fragment(text: String, ansiColor: AnsiColor) {
  
  def toPossiblyColoredText(presentInColor: Boolean): String =
    if (!presentInColor || text.trim.isEmpty) text
    else {
      ("\n" * countLeadingEOLs(text)) +
      text.split("\n").dropWhile(_.isEmpty).map(ansiColor.code + _ + ansiReset).mkString("\n") +
      ("\n" * countTrailingEOLs(text))
    }
}

private[scalatest] object Fragment {

  def countTrailingEOLs(s: String): Int = s.length - s.lastIndexWhere(_ != '\n') - 1

  def countLeadingEOLs(s: String): Int = {
    val idx = s.indexWhere(_ != '\n')
    if (idx != -1) idx else 0
  }
}

