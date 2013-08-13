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
package org.scalautils

import scala.collection.mutable.WrappedArray

trait Prettifier extends (IndexedSeq[Any] => IndexedSeq[String])

class DiffStringPrettifier(left: Any, right: Any) extends Prettifier {
  
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
  
  lazy val (leftee, rightee) = {
    (left, right) match {
      case (leftString: String, rightString: String) => 
        if (leftString == rightString) // Don't diff if both are equal
          (left, right)
        else
          diffStrings(leftString, rightString)
      case _ => (left, right)
    }
  }
  
  def apply(raws: IndexedSeq[Any]): IndexedSeq[String] = 
    Prettifier.default.apply(Vector(leftee, rightee))
}

object Prettifier {
  val default: Prettifier =
    new Prettifier {
      def apply(raws: IndexedSeq[Any]): IndexedSeq[String] =
        raws map defaultPrettify
    }
  
  def defaultPrettify(o: Any): String = 
    o match {
      case null => "null"
      case aUnit: Unit => "<(), the Unit value>"
      case aString: String => "\"" + aString + "\""
      case aChar: Char =>  "\'" + aChar + "\'"
      case anArray: Array[_] =>  prettifyArrays(anArray)
      case aWrappedArray: WrappedArray[_] => prettifyArrays(aWrappedArray)
      case anythingElse => anythingElse.toString
    }

  private def prettifyArrays(o: Any): String = {
    o match {
      case arr: Array[_] => "Array(" + (arr map (a => prettifyArrays(a))).mkString(", ") + ")"
      case wrappedArr: WrappedArray[_] => "Array(" + (wrappedArr map (a => prettifyArrays(a))).mkString(", ") + ")"
      case _ => if (o != null) o.toString else "null"
    }
  }
  
  def apply(fun: => IndexedSeq[Any]) = new Prettifier {
    def apply(raws: IndexedSeq[Any]): IndexedSeq[String] = 
      fun map defaultPrettify
  }
}
