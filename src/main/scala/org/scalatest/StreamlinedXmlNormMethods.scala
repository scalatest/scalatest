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

/**
 * Subtrait of <a href="../scalactic/NormMethods.html"><code>NormMethods</code></a> that provides
 * an implicit <code>Uniformity[T]</code> for subtypes of <code>scala.xml.NodeSeq</code> that enables
 * you to streamline XML by invoking <code>.norm</code> on it.
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; &lt;good&gt;&lt;day&gt;sunshine&lt;/day&gt;&lt;/good&gt; == &lt;good&gt;
 *      |   &lt;day&gt;
 *      |     sunshine
 *      |   &lt;/day&gt;
 *      | &lt;/good&gt;
 * res1: Boolean = false
 * 
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 * 
 * scala&gt; import TripleEquals._
 * import TripleEquals._
 * 
 * scala&gt; import org.scalatest.StreamlinedXmlNormMethods._
 * import org.scalatest.StreamlinedXmlNormMethods._
 *
 * scala&gt; &lt;good&gt;&lt;day&gt;sunshine&lt;/day&gt;&lt;/good&gt; === &lt;good&gt;
 *      |   &lt;day&gt;
 *      |     sunshine
 *      |   &lt;/day&gt;
 *      | &lt;/good&gt;.norm
 * res2: Boolean = true
 * </pre>
 */
trait StreamlinedXmlNormMethods extends StreamlinedXml with NormMethods {

  /**
   * Provides an implicit <a href="../scalactic/Uniformity.html"><code>Uniformity[T]</code></a>
   * instance for any subtype of <code>scala.xml.NodeSeq</code> that will normalize the XML by removing empty text nodes and trimming
   * non-empty text nodes.
   *
   * <p>
   * This <code>Uniformity[T]</code> enables normalization of XML by invoking the <code>.norm</code>
   * method on subtypes of <code>scala.xml.NodeSeq</code>.  See the main documentation for this trait for more
   * details and examples.
   * </p>
   *
   * @return a <code>Uniformity[T]</code> instance that normalizes XML for testing
   */
  implicit override def streamlined[T <: NodeSeq]: Uniformity[T] = super.streamlined[T]
}

/**
 * Companion object that facilitates the importing of <code>StreamlinedXmlNormMethods</code> members as 
 * an alternative to mixing it the trait. One use case is to import <code>StreamlinedXmlNormMethods</code>'s implicit so you can use
 * it in the Scala interpreter.
 *
 * @author Bill Venners
 */
object StreamlinedXmlNormMethods extends StreamlinedXmlNormMethods

