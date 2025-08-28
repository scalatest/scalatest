/*
 * Copyright 2001-2024 Artima, Inc.
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

import org.scalactic.{FailureMessages => _, _}
import enablers.Collecting
import org.scalatest.exceptions._

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
 * implicit <code>Collecting</code> instances for <code>org.scalactic.ColCompatHelper.Iterable</code>, <code>Array</code>,
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
  final class LoneElementCollectionWrapper[E, CTC[_]](collection: CTC[E], collecting: Collecting[E, CTC[E]], prettifier: Prettifier, pos: source.Position) {

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
          throw new TestFailedException(
            (_: StackDepthException) => Some(FailureMessages.notLoneElement(prettifier,
                 collection,
                 collecting.sizeOf(collection))),
            None,
            pos
          )
      }
    }
  }

  // SKIP-DOTTY-START
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
  implicit def convertToCollectionLoneElementWrapper[E, CTC[_]](collection: CTC[E])(using collecting: Collecting[E, CTC[E]], prettifier: Prettifier, pos: source.Position): LoneElementCollectionWrapper[E, CTC] = new LoneElementCollectionWrapper[E, CTC](collection, collecting, prettifier, pos)
  // SKIP-DOTTY-END

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Convert a collection to <code>LoneElementCollectionWrapper[E, CTC]</code> for which an
  //DOTTY-ONLY  * implicit <code>Collecting[E, CTC[E]]</code> is available.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @tparam E the element type of the collection to be converted
  //DOTTY-ONLY  * @tparam CTC the "collection type constructor" for the collection to be converted
  //DOTTY-ONLY  * @param collection the collection to be converted
  //DOTTY-ONLY  * @param collecting a typeclass that enables the conversion
  //DOTTY-ONLY  */
  //DOTTY-ONLY def convertToCollectionLoneElementWrapper[E, CTC[_]](collection: CTC[E])(implicit collecting: Collecting[E, CTC[E]], prettifier: Prettifier, pos: source.Position): LoneElementCollectionWrapper[E, CTC] = new LoneElementCollectionWrapper[E, CTC](collection, collecting, prettifier, pos)

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Extension to add <code>loneElement</code> method to collections.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @tparam E the element type of the collection on which to add the <code>loneElement</code> method
  //DOTTY-ONLY  * @tparam CTC the "collection type constructor" for the collection on which to add the <code>loneElement</code> method
  //DOTTY-ONLY  * @param collection the collection on which to add the <code>loneElement</code> method
  //DOTTY-ONLY  * @param collecting a typeclass that enables the <code>loneElement</code> syntax
  //DOTTY-ONLY  */
  //DOTTY-ONLY extension [E, CTC[_]](collection: CTC[E])(using collecting: Collecting[E, CTC[E]], prettifier: Prettifier, pos: source.Position) {
  //DOTTY-ONLY   def loneElement: E = 
  //DOTTY-ONLY     convertToCollectionLoneElementWrapper[E, CTC](collection)(collecting, prettifier, pos).loneElement
  //DOTTY-ONLY }

  /**
    * Wrapper class that adds a <code>loneElement</code> method to Map for which
    * an implicit <code>Collecting[(K, V), org.scalactic.ColCompatHelper.Iterable[(K, V)]]</code> is available.
    *
    * <p>
    * Through the implicit conversion provided by trait <code>LoneElement</code>, this class allows you to make statements like:
    * </p>
    *
    * <pre class="stHighlight">
    * map.loneElement._1 should be &gt; 9
    * </pre>
    *
    * @tparam K the element type of the Map key on which to add the <code>loneElement</code> method
    * @tparam V the element type of the Map value on which to add the <code>loneElement</code> method
    * @tparam MAP the "Map type constructor" for the collection on which to add the <code>loneElement</code> method
    * @param collecting a typeclass that enables the <code>loneElement</code> syntax
    */
  final class LoneElementMapWrapper[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](map: MAP[K, V], collecting: Collecting[(K, V), org.scalactic.ColCompatHelper.Iterable[(K, V)]], prettifier: Prettifier, pos: source.Position) {

    def loneElement: (K, V) = {
      collecting.loneElementOf(map) match {
        case Some(ele) => ele
        case None =>
          throw new TestFailedException(
            (_: StackDepthException) => Some(FailureMessages.notLoneElement(prettifier,
              map,
              collecting.sizeOf(map))),
            None,
            pos
          )
      }
    }
  }

  // SKIP-DOTTY-START
  /**
   * Implicit conversion that adds a <code>loneElement</code> method to <code>Map</code> type <code>(K, V)</code> for which an
   * implicit <code>Collecting[(K, V)]</code> is available.
   *
   * @tparam K the key type of the <code>Map</code> on which to add the <code>loneElement</code> method
   * @tparam V the value type of the <code>Map</code> on which to add the <code>loneElement</code> method
   * @tparam MAP the "map type constructor" for the map on which to add the <code>loneElement</code> method
   * @param map the map on which to add the <code>loneElement</code> method
   * @param collecting a typeclass that enables the <code>loneElement</code> syntax
   */
  implicit def convertMapToCollectionLoneElementWrapper[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](map: MAP[K, V])(implicit collecting: Collecting[(K, V), org.scalactic.ColCompatHelper.Iterable[(K, V)]], prettifier: Prettifier, pos: source.Position): LoneElementMapWrapper[K, V, MAP] = {
    new LoneElementMapWrapper[K, V, MAP](map, collecting, prettifier, pos)
  }
  // SKIP-DOTTY-END

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Convert a map to <code>LoneElementMapWrapper[K, V, MAP]</code> for which an
  //DOTTY-ONLY  * implicit <code>Collecting[(K, V), org.scalactic.ColCompatHelper.Iterable[(K, V)]]</code> is available.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @tparam K the key type of the <code>Map</code> on which to convert to <code>LoneElementMapWrapper[K, V, MAP]</code>
  //DOTTY-ONLY  * @tparam V the value type of the <code>Map</code> on which to convert to <code>LoneElementMapWrapper[K, V, MAP]</code>
  //DOTTY-ONLY  * @tparam MAP the "map type constructor" for the collection on which to convert to <code>LoneElementMapWrapper[K, V, MAP]</code>
  //DOTTY-ONLY  * @param map the map on which to convert to <code>LoneElementMapWrapper[K, V, MAP]</code>
  //DOTTY-ONLY  * @param collecting a typeclass that enables the <code>loneElement</code> syntax
  //DOTTY-ONLY  */
  //DOTTY-ONLY def convertMapToCollectionLoneElementWrapper[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](map: MAP[K, V])(implicit collecting: Collecting[(K, V), org.scalactic.ColCompatHelper.Iterable[(K, V)]], prettifier: Prettifier, pos: source.Position): LoneElementMapWrapper[K, V, MAP] = {
  //DOTTY-ONLY   new LoneElementMapWrapper[K, V, MAP](map, collecting, prettifier, pos)
  //DOTTY-ONLY }

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Extension to add <code>loneElement</code> method to <code>Map</code>s.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @tparam K the key type of the <code>Map</code> on which to add the <code>loneElement</code> method
  //DOTTY-ONLY  * @tparam V the value type of the <code>Map</code> on which to add the <code>loneElement</code> method
  //DOTTY-ONLY  * @tparam MAP the "map type constructor" for the map on which to add the <code>loneElement</code> method
  //DOTTY-ONLY  * @param map the map on which to add the <code>loneElement</code> method
  //DOTTY-ONLY  * @param collecting a typeclass that enables the <code>loneElement</code> syntax
  //DOTTY-ONLY  */
  //DOTTY-ONLY extension [K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](map: MAP[K, V])(using collecting: Collecting[(K, V), org.scalactic.ColCompatHelper.Iterable[(K, V)]], prettifier: Prettifier, pos: source.Position) {
  //DOTTY-ONLY   def loneElement: (K, V) = 
  //DOTTY-ONLY     convertMapToCollectionLoneElementWrapper(map)(collecting, prettifier, pos).loneElement
  //DOTTY-ONLY }

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
  final class LoneElementJavaMapWrapper[K, V, JMAP[_, _] <: java.util.Map[_, _]](jmap: JMAP[K, V], collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], prettifier: Prettifier, pos: source.Position) {

    def loneElement: org.scalatest.Entry[K, V] = {
      collecting.loneElementOf(jmap) match {
        case Some(ele) => ele
        case None =>
          throw new TestFailedException(
            (_: StackDepthException) => Some(FailureMessages.notLoneElement(prettifier,
                 jmap,
                 collecting.sizeOf(jmap))), 
            None,
            pos
          )
      }
    }
  }

  // SKIP-DOTTY-START
  /**
   * Implicit conversion that adds a <code>loneElement</code> method to Java map type <code>Entry[K, V]</code> for which an
   * implicit <code>Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]</code> is available.
   *
   * @tparam K the key type of the Java map on which to add the <code>loneElement</code> method
   * @tparam V the value type of the Java map on which to add the <code>loneElement</code> method
   * @tparam MAP the "map type constructor" for the map on which to add the <code>loneElement</code> method
   * @param map the map on which to add the <code>loneElement</code> method
   * @param collecting a typeclass that enables the <code>loneElement</code> syntax
   */
  implicit def convertJavaMapToCollectionLoneElementWrapper[K, V, JMAP[_, _] <: java.util.Map[_, _]](jmap: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], prettifier: Prettifier, pos: source.Position): LoneElementJavaMapWrapper[K, V, JMAP] = {
    new LoneElementJavaMapWrapper[K, V, JMAP](jmap, collecting, prettifier, pos)
  }
  // SKIP-DOTTY-END

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Convert Java map type <code>Entry[K, V]</code> for which an implicit <code>Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]</code> is available to
  //DOTTY-ONLY  * <code>LoneElementJavaMapWrapper[K, V, JMAP]</code>.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @tparam K the key type of the Java map to convert
  //DOTTY-ONLY  * @tparam V the value type of the Java map to convert
  //DOTTY-ONLY  * @tparam MAP the "map type constructor" for the map to convert
  //DOTTY-ONLY  * @param map the map to convert
  //DOTTY-ONLY  * @param collecting a typeclass that enables the <code>loneElement</code> syntax
  //DOTTY-ONLY  */
  //DOTTY-ONLY def convertJavaMapToCollectionLoneElementWrapper[K, V, JMAP[_, _] <: java.util.Map[_, _]](jmap: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], prettifier: Prettifier, pos: source.Position): LoneElementJavaMapWrapper[K, V, JMAP] = {
  //DOTTY-ONLY   new LoneElementJavaMapWrapper[K, V, JMAP](jmap, collecting, prettifier, pos)
  //DOTTY-ONLY }

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Extension to add <code>loneElement</code> method to Java maps.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @tparam K the key type of the Java map on which to add the <code>loneElement</code> method
  //DOTTY-ONLY  * @tparam V the value type of the Java map on which to add the <code>loneElement</code> method
  //DOTTY-ONLY  * @tparam MAP the "map type constructor" for the Java map on which to add the <code>loneElement</code> method
  //DOTTY-ONLY  * @param map the Java map on which to add the <code>loneElement</code> method
  //DOTTY-ONLY  * @param collecting a typeclass that enables the <code>loneElement</code> syntax
  //DOTTY-ONLY  */
  //DOTTY-ONLY extension [K, V, JMAP[k, v] <: java.util.Map[k, v]](jmap: JMAP[K, V])(using collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], prettifier: Prettifier, pos: source.Position) {
  //DOTTY-ONLY   def loneElement: org.scalatest.Entry[K, V] = 
  //DOTTY-ONLY     convertJavaMapToCollectionLoneElementWrapper(jmap)(collecting, prettifier, pos).loneElement
  //DOTTY-ONLY }

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
  final class LoneElementStringWrapper(s: String, prettifier: Prettifier, pos: source.Position) {

    def loneElement: Char = {
      if (s.length == 1)
        s.charAt(0)
      else
        throw new TestFailedException(
          (_: StackDepthException) => Some(FailureMessages.notLoneElement(prettifier,
            s,
            s.length)),
          None,
          pos
        )
    }
  }

  // SKIP-DOTTY-START
  /**
   * Implicit conversion that adds a <code>loneElement</code> method to String.
   *
   * @param s the <code>String</code> to wrap
   * @param collecting a typeclass that enables the <code>loneElement</code> syntax
   */
  implicit def convertToStringLoneElementWrapper(s: String)(implicit prettifier: Prettifier, pos: source.Position): LoneElementStringWrapper =
    new LoneElementStringWrapper(s, prettifier, pos)
  // SKIP-DOTTY-END

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Convert String to <code>LoneElementStringWrapper</code>.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @param s the <code>String</code> to wrap
  //DOTTY-ONLY  */
  //DOTTY-ONLY def convertToStringLoneElementWrapper(s: String)(implicit prettifier: Prettifier, pos: source.Position): LoneElementStringWrapper =
  //DOTTY-ONLY   new LoneElementStringWrapper(s, prettifier, pos)

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Extension to add <code>loneElement</code> method to <code>String</code>.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @param s the <code>String</code> to add the <code>loneElement</code> method
  //DOTTY-ONLY  */
  //DOTTY-ONLY extension (s: String)(using prettifier: Prettifier, pos: source.Position) {
  //DOTTY-ONLY   def loneElement: Char = 
  //DOTTY-ONLY     convertToStringLoneElementWrapper(s)(prettifier, pos).loneElement
  //DOTTY-ONLY }
}

/**
 * Companion object that facilitates the importing of <code>LoneElement</code> members as 
 * an alternative to mixing it in. One use case is to import <code>LoneElement</code>'s members so you can use
 * <code>loneElement</code> in the Scala interpreter.
 */
object LoneElement extends LoneElement

