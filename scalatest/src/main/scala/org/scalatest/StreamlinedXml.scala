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

import scala.xml.{Text, Node, Elem, EntityRef, NodeSeq}
import org.scalactic.Uniformity

/**
 * Trait providing a <code>streamlined</code> method that returns a <a href="../scalactic/Uniformity.html"><code>Uniformity[T]</code></a>
 * instance for any subtype of <code>scala.xml.NodeSeq</code> that will normalize the XML by removing empty text nodes and trimming
 * non-empty text nodes.
 *
 * <p>
 * Here's an example of some unnormalized XML:
 * </p>
 *
 * <pre class="stHighlight">
 * &lt;summer&gt;
 *   &lt;day&gt;&lt;/day&gt;
 *   &lt;night&gt;
 *     with lots of stars
 *   &lt;/night&gt;
 * &lt;/summer&gt;
 * </pre>
 *
 * <p>
 * The <code>Uniformity</code> returned by this trait's <code>streamlined</code> method would transform
 * the above XML to:
 * </p>
 *
 * <pre class="stHighlight">
 * &lt;summer&gt;&lt;day&gt;&lt;/day&gt;&lt;night&gt;with lots of stars&lt;/night&gt;&lt;/summer&gt;
 * </pre>
 *
 * <p>
 * The <code>streamlined</code> method can be used with the <a href="../scalactic/Explicitly.html"><code>Explicitly</code></a> DSL, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * xmlElem should equal (
 *   &lt;summer&gt;
 *     &lt;day&gt;&lt;/day&gt;
 *     &lt;night&gt;
 *       with lots of stars
 *     &lt;/night&gt;
 *   &lt;/summer&gt;
 * ) (after being streamlined[Elem])
 * </pre>
 *
 * <p>
 * The goal of this trait is to provide a normalization for XML that makes it easier to test XML objects for equality.
 * White space is significant in XML, and is taken into account by the default equality for XML, accessed
 * by invoking the <code>==</code> method on an XML <code>NodeSeq</code>. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val xmlElem = &lt;summer&gt;&lt;day&gt;&lt;/day&gt;&lt;night&gt;with lots of stars&lt;/night&gt;&lt;/summer&gt;
 * xmlElem: scala.xml.Elem = &lt;summer&gt;&lt;day&gt;&lt;/day&gt;&lt;night&gt;with lots of stars&lt;/night&gt;&lt;/summer&gt;
 *
 * scala&gt; xmlElem == &lt;summer&gt;
 *      |   &lt;day&gt;&lt;/day&gt;
 *      |   &lt;night&gt;
 *      |     with lots of stars
 *      |   &lt;/night&gt;
 *      | &lt;/summer&gt;
 * res1: Boolean = false
 * </pre>
 *
 * <p>
 * The above equality comparison produces false because of whitespace differences in the XML.
 * When such whitespace differences are unimportant to the actual application, it can make it
 * easier to write readable test code if you can compare XML for equality without taking
 * into account empty text nodes, or leading and trailing whitespace in nonempty text nodes.
 * The <code>streamlined</code> method of this trait provides a <code>Uniformity</code>
 * instance that does just that:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import Explicitly._
 * import Explicitly._
 *
 * scala&gt; import TripleEquals._
 * import TripleEquals._
 *
 * scala&gt; import org.scalatest.StreamlinedXml._
 * import StreamlinedXml._
 *
 * scala&gt; import scala.xml.Elem
 * import scala.xml.Elem * 
 * 
 * scala&gt; (xmlElem === &lt;summer&gt;
 *      |   &lt;day&gt;&lt;/day&gt;
 *      |   &lt;night&gt;
 *      |     with lots of stars
 *      |   &lt;/night&gt;
 *      | &lt;/summer&gt;) (after being streamlined[Elem])
 * res9: Boolean = true
 * </pre>
 *
 * @author Bill Venners
 */
trait StreamlinedXml {

  /**
   * Provides a <a href="../scalactic/Uniformity.html"><code>Uniformity[T]</code></a>
   * instance for any subtype of <code>scala.xml.NodeSeq</code> that will normalize the XML by removing empty text nodes and trimming
   * non-empty text nodes.
   *
   * <p>
   * The purpose of this <code>Uniformity</code> is to make it easier to write readable test
   * code that compares XML for equality. See the main documentation for this trait for more
   * details and examples.
   * </p>
   *
   * @return a <code>Uniformity[T]</code> instance that normalizes XML for testing
   */
  def streamlined[T <: NodeSeq]: Uniformity[T] = {

    def trimTextZappingEmpty(data: String): Seq[Node] =
      data.trim match {
        case "" => NodeSeq.Empty
        case trimmed => Text(trimmed)
      }

    def mergeAdjacentTextNodes(children: Seq[Node]) =
      children.foldRight[List[Node]](Nil) { (ele, acc) =>
        ele match {
          case eleTxt if eleTxt.isAtom || eleTxt.isInstanceOf[EntityRef] =>
            acc match {
              case Text(accTxt) :: tail =>
                Text(eleTxt.text + accTxt) :: tail
              case _ => Text(eleTxt.text) :: acc
            }
          case _ => ele :: acc
        }
      }

    def normalizedXml(nodeSeq: NodeSeq): NodeSeq =
      mergeAdjacentTextNodes(nodeSeq).flatMap {
        case Elem(pre, lab, md, scp, children @ _*) =>
          Elem(pre, lab, md, scp, false, normalizedXml(children): _*)
        case Text(data) =>
          trimTextZappingEmpty(data)
        case x => x
      }

    new Uniformity[T] {
      def normalized(nodeSeq: T): T = {
        normalizedXml(nodeSeq) match {
          case NodeSeq.Empty => nodeSeq
          case normalized if normalized.size == 1 => normalized.head
          case normalized => normalized
        }
      }.asInstanceOf[T]

      /**
       * Returns true if the passed <code>Any</code> is a <code>Elem</code>.
       *
       * @return true if the passed <code>Any</code> is a <code>Elem</code>.
       */
      final def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[NodeSeq]
    
      /**
       * Normalizes the passed object if it is a <code>Elem</code>.
       *
       * <p>
       * This method returns either:
       * </p>
       *
       * <ul>
       * <li>if the passed object is a <code>Elem</code>, the result of passing that string to <code>normalized</code></li>
       * <li>else, the same exact object that was passed
       * </p>
       *
       * @return a normalized form of any passed <code>Elem</code>, or the same object if not a <code>Elem</code>.
       */
      final def normalizedOrSame(b: Any): Any =
        b match {
          case s: NodeSeq => StreamlinedXml.streamlined[NodeSeq].normalized(s)
          case _ => b
       }
    }
  }
}

/**
 * Companion object that facilitates the importing of <code>StreamlinedXml</code> members as 
 * an alternative to mixing it the trait. One use case is to import <code>StreamlinedXml</code> members so you can use
 * them in the Scala interpreter.
 *
 * @author Bill Venners
 */
object StreamlinedXml extends StreamlinedXml
