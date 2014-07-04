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

trait CompressedXmlEquality {

  implicit def compressedXmlEquality[T <: NodeSeq]: Equality[T] = {
    new Equality[T] {
      val xu: Uniformity[T] = XmlCompression.compressed[T]
      def areEqual(a: T, b: Any): Boolean = {
        xu.normalized(a) == xu.normalizedOrSame(b)
      }
    }
  }
}

object CompressedXmlEquality extends CompressedXmlEquality

