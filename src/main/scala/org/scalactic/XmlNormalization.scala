/*
 * Copyright 2001-2014 Artima, Inc.
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

import org.scalactic.Normalization
import scala.xml.{Text, Node, Elem}

trait XmlNormalization {

  private def trimTextZappingEmpty(node: Node): Seq[Node] =
    node match {
      case Text(text) if (text.trim.isEmpty) => Nil
      case Text(text) => List(Text(text.trim))
      case Elem(pre, lab, md, scp, children @ _*) =>
        Elem(pre, lab, md, scp, false, (children.flatMap(trimTextZappingEmpty)):_*)
      case _ => List(node)
    }

  implicit val normalizedXmlElem: Normalization[Elem] = {

    new Normalization[Elem] {
      def normalized(elem: Elem): Elem =
        elem match {
          case Elem(pre, lab, md, scp, children @ _*) =>
            val mergedTextNodes = // Merge adjacent text nodes
              children.foldLeft(Nil: List[Node]) { (acc, ele) =>
                ele match {
                  case eleTxt: Text =>
                    acc.headOption match {
                      case Some(accTxt: Text) =>
                        Text(accTxt.text + eleTxt.text) :: acc.tail
                      case _ => ele :: acc
                    }
                  case _ => ele :: acc
                }
              }
            Elem(pre, lab, md, scp, false, (mergedTextNodes.flatMap(trimTextZappingEmpty)):_*)
        }
    }
  }
  implicit val normalizedXmlNode: Normalization[Node] = {

    new Normalization[Node] {
      def normalized(node: Node): Node =
        node match {
          case elem: Elem => normalizedXmlElem.normalized(elem)
          case _ => node
        }
    }
  }
}

object XmlNormalization extends XmlNormalization
