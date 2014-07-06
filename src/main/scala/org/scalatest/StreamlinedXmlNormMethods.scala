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
package org.scalatest

import annotation.tailrec
import scala.xml.{Elem,Node,NodeSeq}
import org.scalactic.{NormMethods, Uniformity}

trait StreamlinedXmlNormMethods extends StreamlinedXml with NormMethods {
  implicit override def streamlined[T <: NodeSeq]: Uniformity[T] = super.streamlined[T]
}

object StreamlinedXmlNormMethods extends StreamlinedXmlNormMethods

