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
package org.scalactic

import annotation.tailrec
import scala.xml.{Elem,Node,NodeSeq}

trait XmlEquality {

  implicit val xmlElemEquality: Equality[Elem] = {
    new Equality[Elem] {
      val xUni: Uniformity[Elem] = XmlNormalization.normalizedXmlElem
      def areEqual(a: Elem, b: Any): Boolean = {
        xUni.normalized(a) == xUni.normalizedOrSame(b)
      }
    }
  }
  implicit val xmlNodeEquality: Equality[Node] = {
    new Equality[Node] {
      val xUni: Uniformity[Node] = XmlNormalization.normalizedXmlNode
      def areEqual(a: Node, b: Any): Boolean = {
        xUni.normalized(a) == xUni.normalizedOrSame(b)
      }
    }
  }
  implicit val xmlNodeSeqEquality: Equality[NodeSeq] = {
    new Equality[NodeSeq] {
      val xUni: Uniformity[NodeSeq] = XmlNormalization.normalizedXmlNodeSeq
      def areEqual(a: NodeSeq, b: Any): Boolean = {
        xUni.normalized(a) == xUni.normalizedOrSame(b)
      }
    }
  }
}

object XmlEquality extends XmlEquality
