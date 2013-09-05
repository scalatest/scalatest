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
package org.scalatest.enablers

import org.scalautils.Equality
import org.scalatest.words.ArrayWrapper
import scala.collection.GenTraversable
import org.scalatest.FailureMessages
import scala.annotation.tailrec
// import scala.collection.JavaConverters._

/**
 * Supertrait for typeclasses that enable <code>contain</code> matcher syntax for aggregations.
 *
 * <p>
 * An <code>Collecting[A]</code> provides access to the "aggregating nature" of type <code>A</code> in such
 * a way that relevant <code>contain</code> matcher syntax can be used with type <code>A</code>. An <code>A</code>
 * can be any type of "aggregation," a type that in some way aggregates or brings together other types. ScalaTest provides
 * implicit implementations for several types. You can enable the <code>contain</code> matcher syntax on your own
 * type <code>U</code> by defining an <code>Collecting[U]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides implicit <code>Collecting</code> instances for <code>scala.collection.GenTraversable</code>,
 * <code>java.util.Collection</code>, <code>java.util.Map</code>, <code>String</code>, and <code>Array</code> in the
 * <code>Collecting</code> companion object.
 * </p>
 *
 * <p>
 * Note, for an explanation of the difference between <code>Containing</code> and <code>Collecting</code>, both of which
 * enable <code>contain</code> matcher syntax, see the <a href="Containing.html#containingVersusCollecting">Containing
 * versus Collecting</a> section of the main documentation for trait <code>Containing</code>.
 * </p>
 */
trait Collecting[E, C] {

  /**
   * Implements the <code>loneElement</code> syntax of trait <code>LoneElement</code>.
   *
   * <p>
   * Returns the lone element contained in a collection, wrapped in a <code>Some</code>, or <code>None</code>  
   * if the collection contains either no elements or more than one element.
   * </p>
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements at least one of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains at least one of the passed elements
   */
  def loneElementOf(collection: C): Option[E]

  def sizeOf(collection: C): Int
}

object Collecting {

  implicit def collectingNatureOfGenTraversable[E, TRAV[e] <: scala.collection.GenTraversable[e]]: Collecting[E, TRAV[E]] = 
    new Collecting[E, TRAV[E]] {
      def loneElementOf(trav: TRAV[E]): Option[E] = {
        if (trav.size == 1) Some(trav.head) else None
      }
      def sizeOf(trav: TRAV[E]): Int = trav.size
    }

  implicit def collectingNatureOfArray[E]: Collecting[E, Array[E]] = 
    new Collecting[E, Array[E]] {
      def loneElementOf(array: Array[E]): Option[E] = {
        if (array.size == 1) Some(array.head) else None
      }
      def sizeOf(array: Array[E]): Int = array.length
    }

  implicit def collectingNatureOfJavaCollection[E, JCOL[e] <: java.util.Collection[e]]: Collecting[E, JCOL[E]] = 
    new Collecting[E, JCOL[E]] {
      def loneElementOf(coll: JCOL[E]): Option[E] = {
        if (coll.size == 1) Some(coll.iterator.next) else None
      }
      def sizeOf(coll: JCOL[E]): Int = coll.size
    }

  // Wrap the extracted entry in an org.scalatest.Entry so people can call key and value methods instead of getKey and getValue
  implicit def collectingNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v]]: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]] = 
    new Collecting[org.scalatest.Entry[K, V], JMAP[K, V]] {
      def loneElementOf(jmap: JMAP[K, V]): Option[org.scalatest.Entry[K, V]] = {
        if (jmap.size == 1) {
          val loneEntry = jmap.entrySet.iterator.next
          Some(org.scalatest.Entry(loneEntry.getKey, loneEntry.getValue))
        } else None
      }
      def sizeOf(jmap: JMAP[K, V]): Int = jmap.size
    }
}
