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
package org.scalatest

import enablers.Collecting

/**
 * Trait that provides an implicit conversion that adds to collection types a <code>loneElement</code> method, which
 * will return the value of the lone element if the collection does
 * indeed contain one and only one element, or throw <a href="TestFailedException.html"><code>TestFailedException</code></a> if not.
 *
 * <p>
 * This construct allows you to express in one statement that a collection should contain one and only one element
 * and that the element value should meet some expectation. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * set.loneElement should be &gt; 9
 * </pre>
 *
 * <p>
 * Or, using an assertion instead of a matcher expression:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(set.loneElement &gt; 9)
 * </pre>
 *
 * <p>
 * The <code>loneElement</code> syntax can be used with any collection type <code>C</code> for which an
 * implicit <a href="enablers/Collecting.html"><code>Collecting[C]</code></a> is available. ScalaTest provides
 * implicit <code>Collecting</code> instances for <code>scala.collection.GenTraversable</code>, <code>Array</code>,
 * and <code>java.util.Collection</code>. You can enable the <code>loneElement</code>
 * syntax on other collection types by defining an implicit <code>Collecting</code> instances for those types.
 * </p>
 *
 * <p>
 * If you want to use <code>loneElement</code> with a <code>java.util.Map</code>, first transform it to a
 * set of entries with <code>entrySet</code>, and if helpful, use ScalaTest's <a href="Entry.html"><code>Entry</code></a> class:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import LoneElement._
 * import LoneElement._
 *
 * scala&gt; import Matchers._
 * import Matchers._
 *
 * scala&gt; val jmap = new java.util.HashMap[String, Int]
 * jmap: java.util.HashMap[String,Int] = {}
 *
 * scala&gt; jmap.put("one", 1)
 * res0: Int = 0
 *
 * scala&gt; jmap.entrySet.loneElement should be (Entry("one", 1))
 * </pre>
 *
 * @author Bill Venners
 */
trait LoneElement {

  import scala.language.higherKinds

  /**
   * Wrapper class that adds a <code>loneElement</code> method to any collection type <code>C</code> for which 
   * an implicit <code>Collecting[C]</code> is available.
   *
   * <p>
   * Through the implicit conversion provided by trait <code>LoneElement</code>, this class allows you to make statements like:
   * </p>
   *
   * <pre class="stHighlight">
   * trav.loneElement should be &gt; 9
   * </pre>
   *
   * @tparam E the element type of the collection on which to add the <code>loneElement</code> method
   * @tparam CTC the "collection type constructor" for the collection on which to add the <code>loneElement</code> method
   * @param collection a collection to wrap in a <code>LoneElementCollectionWrapper</code>, which provides the <code>loneElement</code> method.
   * @param collecting a typeclass that enables the <code>loneElement</code> syntax
   */
  final class LoneElementCollectionWrapper[E, CTC[_]](collection: CTC[E])(implicit collecting: Collecting[E, CTC[E]]) {

    /**
     * Returns the value contained in the wrapped collection, if it contains one and only one element, else throws <code>TestFailedException</code> with
     * a detail message describing the problem.
     *
     * <p>
     * This method enables syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * trav.loneElement should be &gt; 9
     *      ^
     * </pre>
     */
    def loneElement: E = {
      collecting.loneElementOf(collection) match {
        case Some(ele) => ele
        case None =>
          throw new exceptions.TestFailedException(
            Some(FailureMessages(
                 "notLoneElement",
                 collection,
                 collecting.sizeOf(collection))), 
            None, 
            1
          )
      }
    }
  }

  import scala.language.implicitConversions
  
  /**
   * Implicit conversion that adds a <code>loneElement</code> method to any collection type <code>C</code> for which an
   * implicit <code>Collecting[C]</code> is available.
   *
   * @tparam E the element type of the collection on which to add the <code>loneElement</code> method
   * @tparam CTC the "collection type constructor" for the collection on which to add the <code>loneElement</code> method
   * @param collection the collection on which to add the <code>loneElement</code> method
   * @param collecting a typeclass that enables the <code>loneElement</code> syntax
   */
  implicit def convertToCollectionLoneElementWrapper[E, CTC[_]](collection: CTC[E])(implicit collecting: Collecting[E, CTC[E]]): LoneElementCollectionWrapper[E, CTC] = new LoneElementCollectionWrapper[E, CTC](collection)

  /**
   * Wrapper class that adds a <code>loneElement</code> method to Java Map for which
   * an implicit <code>Collecting[org.scalatest.Entry, java.util.Map]</code> is available.
   *
   * <p>
   * Through the implicit conversion provided by trait <code>LoneElement</code>, this class allows you to make statements like:
   * </p>
   *
   * <pre class="stHighlight">
   * jmap.loneElement.getKey should be &gt; 9
   * </pre>
   *
   * @tparam K the element type of the Java Map key on which to add the <code>loneElement</code> method
   * @tparam V the element type of the Java Map value on which to add the <code>loneElement</code> method
   * @tparam JMAP the "Java Map type constructor" for the collection on which to add the <code>loneElement</code> method
   * @param collecting a typeclass that enables the <code>loneElement</code> syntax
   */
  final class LoneElementJavaMapWrapper[K, V, JMAP[_, _] <: java.util.Map[_, _]](jmap: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]) {

    def loneElement: org.scalatest.Entry[K, V] = {
      collecting.loneElementOf(jmap) match {
        case Some(ele) => ele
        case None =>
          throw new exceptions.TestFailedException(
            Some(FailureMessages(
                 "notLoneElement",
                 jmap,
                 collecting.sizeOf(jmap))), 
            None, 
            1
          )
      }
    }
  }

  // Needed for Java Map to work, any better solution?
  implicit def convertJavaMapToCollectionLoneElementWrapper[K, V, JMAP[_, _] <: java.util.Map[_, _]](jmap: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]): LoneElementJavaMapWrapper[K, V, JMAP]  = {
    new LoneElementJavaMapWrapper[K, V, JMAP](jmap)(collecting)
  }

  /**
   * Wrapper class that adds a <code>loneElement</code> method to <code>String</code> for which an
   * implicit <code>Collecting[C]</code> is available.
   *
   * <p>
   * Through the implicit conversion provided by trait <code>LoneElement</code>, this class allows you to make statements like:
   * </p>
   *
   * <pre class="stHighlight">
   * "9".loneElement should be ('9')
   * </pre>
   *
   * @param s the <code>String</code> to wrap
   * @param collecting a typeclass that enables the <code>loneElement</code> syntax
   */
  final class LoneElementStringWrapper(s: String)(implicit collecting: Collecting[Char, String]) {

    def loneElement: Char = {
      if (s.length == 1)
        s.charAt(0)
      else
        throw new exceptions.TestFailedException(
          Some(FailureMessages(
            "notLoneElement",
            s,
            collecting.sizeOf(s))),
          None,
          1
        )
    }

  }

  /**
   * Implicit conversion that adds a <code>loneElement</code> method to String for which an
   * implicit <code>Collecting[C]</code> is available.
   *
   * @param s the <code>String</code> to wrap
   * @param collecting a typeclass that enables the <code>loneElement</code> syntax
   */
  implicit def convertToStringLoneElementWrapper(s: String)(implicit collecting: Collecting[Char, String]): LoneElementStringWrapper =
    new LoneElementStringWrapper(s)(collecting)
}

/**
 * Companion object that facilitates the importing of <code>LoneElement</code> members as 
 * an alternative to mixing it in. One use case is to import <code>LoneElement</code>'s members so you can use
 * <code>loneElement</code> in the Scala interpreter.
 */
object LoneElement extends LoneElement

