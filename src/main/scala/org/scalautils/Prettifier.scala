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

trait Prettifier extends (Any => String)

object Prettifier {
  val default: Prettifier =
    new Prettifier {
      def apply(o: Any): String =
        o match {
          case null => "null"
          case aUnit: Unit => "<(), the Unit value>"
          case aString: String => "\"" + aString + "\""
          case aChar: Char =>  "\'" + aChar + "\'"
          case anArray: Array[_] =>  prettifyArrays(anArray)
          case aWrappedArray: WrappedArray[_] => prettifyArrays(aWrappedArray)
          case anythingElse => anythingElse.toString
        }
    }

  private def prettifyArrays(o: Any): String = {
    o match {
      case arr: Array[_] => "Array(" + (arr map (a => prettifyArrays(a))).mkString(", ") + ")"
      case wrappedArr: WrappedArray[_] => "Array(" + (wrappedArr map (a => prettifyArrays(a))).mkString(", ") + ")"
      case _ => if (o != null) o.toString else "null"
    }
  }
}
