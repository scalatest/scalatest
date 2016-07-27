/*
 * Copyright 2001-2016 Artima, Inc.
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

import org.scalactic.source.CaseClassMeta

trait Differ[T] {

  def difference(a: T, b: Any): Difference

}

object DefaultDiffer extends Differ[Any] {

  def difference(a: Any, b: Any): Difference =
    (a, b) match {
      case (s1: String, s2: String) => StringDiffer.difference(s1, s2)
      case _ =>
        if (CaseClassMeta.isCaseClass(a) && CaseClassMeta.isCaseClass(b))
          CaseClassDiffer.difference(a, b)
        else
          Difference.empty
    }

}

object StringDiffer extends Differ[String] {

  def difference(a: String, b: Any): Difference =
    new Difference {

      def diffStrings(s: String, t: String): Tuple2[String, String] = {
        def findCommonPrefixLength(s: String, t: String): Int = {
          val max = s.length.min(t.length) // the maximum potential size of the prefix
          var i = 0
          var found = false
          while (i < max & !found) {
            found = (s.charAt(i) != t.charAt(i))
            if (!found)
              i = i + 1
          }
          i
        }
        def findCommonSuffixLength(s: String, t: String): Int = {
          val max = s.length.min(t.length) // the maximum potential size of the suffix
          var i = 0
          var found = false
          while (i < max & !found) {
            found = (s.charAt(s.length - 1 - i) != t.charAt(t.length - 1 - i))
            if (!found)
              i = i + 1
          }
          i
        }
        if (s != t) {
          val commonPrefixLength = findCommonPrefixLength(s, t)
          val commonSuffixLength = findCommonSuffixLength(s.substring(commonPrefixLength), t.substring(commonPrefixLength))
          val prefix = s.substring(0, commonPrefixLength)
          val suffix = if (s.length - commonSuffixLength < 0) "" else s.substring(s.length - commonSuffixLength)
          val sMiddleEnd = s.length - commonSuffixLength
          val tMiddleEnd = t.length - commonSuffixLength
          val sMiddle = s.substring(commonPrefixLength, sMiddleEnd)
          val tMiddle = t.substring(commonPrefixLength, tMiddleEnd)
          val MaxContext = 20
          val shortPrefix = if (commonPrefixLength > MaxContext) "..." + prefix.substring(prefix.length - MaxContext) else prefix
          val shortSuffix = if (commonSuffixLength > MaxContext) suffix.substring(0, MaxContext) + "..." else suffix
          (shortPrefix + "[" + sMiddle + "]" + shortSuffix, shortPrefix + "[" + tMiddle + "]" + shortSuffix)
        }
        else
          (s, t)
      }

      def inlineDiff = {
        b match {
          case bStr: String => Some(diffStrings(a, bStr))
          case _ => None
        }
      }

      def sideBySideDiff = None

      def analysis = None
    }

}

object CaseClassDiffer extends Differ[Any] {

  def difference(a: Any, b: Any): Difference = {
    new Difference {
      def inlineDiff = None

      def sideBySideDiff = None

      def analysis = {
        val leftMeta = CaseClassMeta(a)
        val rightMeta = CaseClassMeta(b)

        val diffSet =
          leftMeta.caseAccessorNames.flatMap { name =>
            val leftValue = leftMeta.value(name)
            try {
              val rightValue = rightMeta.value(name)
              if (leftValue != rightValue) {
                val nestedDiff = DefaultDiffer.difference(leftValue, rightValue)
                if (nestedDiff == Difference.empty)
                  Some(name + ": " + leftValue + " -> " + rightValue)
                else {
                  nestedDiff.inlineDiff match {
                    case Some((leftee, rightee)) => Some(name + ": " + leftee + " -> " + rightee)
                    case _ =>
                      nestedDiff.analysis match {
                        case Some(analysis) =>
                          Some(name + ": " + analysis + "")

                        case None => Some(name + ": " + leftValue + " -> " + rightValue)
                      }
                  }
                }

              }
              else
                None
            }
            catch {
              case iae: IllegalArgumentException => None
            }
          }

        if (diffSet.isEmpty)
          None
        else
          Some("(" + diffSet.toList.sorted.mkString(scala.compat.Platform.EOL) + ")")
      }
    }

  }

}