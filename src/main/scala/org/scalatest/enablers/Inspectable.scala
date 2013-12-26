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
import scala.language.higherKinds

/**
 * Supertrait for typeclasses that enable <code>loneElement</code> matcher syntax for collections.
 *
 * <p>
 * A <code>Inspectable[E, C]</code> provides access to the "inspectable nature" of type <code>C</code> in such
 * a way that <code>loneElement</code> syntax can be used with type <code>C</code>. A <code>C</code>
 * can be any type of "inspectable", a type that in some way collects or brings together elements of type <code>E</code>.
 * ScalaTest provides implicit implementations for several types. You can enable the <code>contain</code> matcher syntax
 * on your own type <code>U</code> by defining an <code>Inspectable[U, E]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides implicit <code>Inspectable</code> instances for <code>scala.collection.GenTraversable</code>,
 * <code>Array</code>, <code>java.util.Collection</code> and <code>java.util.Map</code> in the
 * <code>Inspectable</code> companion object.
 * </p>
 *
 */
trait Inspectable[C] {

  type Element

  /**
   * Returns a <code>GenTraversable[E]</code> containing the same elements (in the same
   * order, if the original collection had a defined order), as the passed <code>collection</code> .
   *
   * @param collection a <code>collection</code> to check the size of
   * @return a <code>GenTraversable[E]</code> containing the same elements as the passed <code>collection</code>
   */
  def genTraversableFrom(collection: C): GenTraversable[Element]
}

/**
 * Companion object for <code>Inspectable</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>scala.collection.GenTraversable</code></li>
 * <li><code>Array</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li><code>java.util.Map</code></li>
 * </ul>
 */
object Inspectable {

  /**
   * Implicit to support <code>Inspectable</code> nature of <code>GenTraversable</code>.
   *
   * @tparam E the type of the element in the <code>GenTraversable</code>
   * @tparam TRAV any subtype of <code>GenTraversable</code>
   * @return <code>Inspectable[TRAV[E]]</code> that supports <code>GenTraversable</code> in <code>loneElement</code> syntax
   */
  implicit def inspectableNatureOfGenTraversable[E, TRAV[e] <: scala.collection.GenTraversable[e]]: Inspectable[TRAV[E]] { type Element = E } = 
    new Inspectable[TRAV[E]] {
      type Element = E
      def genTraversableFrom(collection: TRAV[E]): GenTraversable[E] = collection
    }

  /**
   * Implicit to support <code>Inspectable</code> nature of <code>Array</code>.
   *
   * @tparam E the type of the element in the <code>Array</code>
   * @return <code>Inspectable[Array[E]]</code> that supports <code>Array</code> in <code>loneElement</code> syntax
   */
  implicit def inspectableNatureOfArray[E]: Inspectable[Array[E]] { type Element = E } = 
    new Inspectable[Array[E]] {
      type Element = E
      def genTraversableFrom(collection: Array[E]): GenTraversable[E] = collection
    }

  /**
   * Implicit to support <code>Inspectable</code> nature of <code>java.util.Collection</code>.
   *
   * @tparam E the type of the element in the <code>java.util.Collection</code>
   * @tparam JCOL any subtype of <code>java.util.Collection</code>
   * @return <code>Inspectable[JCOL[E]]</code> that supports <code>java.util.Collection</code> in <code>loneElement</code> syntax
   */
  implicit def inspectableNatureOfJavaCollection[E, JCOL[e] <: java.util.Collection[e]]: Inspectable[JCOL[E]] { type Element = E } = 
    new Inspectable[JCOL[E]] {
      type Element = E
      def genTraversableFrom(collection: JCOL[E]): GenTraversable[E] = {
        import scala.collection.JavaConverters._
        /*
        This is what asScala does, to make sure it keeps the order of Lists
        scala.collection.mutable.Buffer <=> java.util.List
        scala.collection.mutable.Set <=> java.util.Set
        */
        collection match {
          case jList: java.util.List[E @unchecked] => jList.asScala
          case jSet: java.util.Set[E @unchecked] => jSet.asScala
          case _ => collection.asScala
        }
      }
    }

  // Wrap the extracted entry in an org.scalatest.Entry so people can call key and value methods instead of getKey and getValue
  /**
   * Implicit to support <code>Inspectable</code> nature of <code>java.util.Map</code>.
   *
   * @tparam K the type of the key in the <code>java.util.Map</code>
   * @tparam V the type of the value in the <code>java.util.Map</code>
   * @tparam JMAP any subtype of <code>java.util.Map</code>
   * @return <code>Inspectable[JMAP[K, V]]</code> that supports <code>java.util.Map</code> in <code>loneElement</code> syntax
   */
  implicit def inspectableNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v]]: Inspectable[JMAP[K, V]] { type Element = org.scalatest.Entry[K, V] } = 
    new Inspectable[JMAP[K, V]] {
      type Element = org.scalatest.Entry[K, V]
        /*
        This is what .asScala does on a Set
        scala.collection.mutable.Set <=> java.util.Set
        */
      def genTraversableFrom(collection: JMAP[K, V]): scala.collection.Set[org.scalatest.Entry[K, V]] = {
        import scala.collection.JavaConverters._
        collection.keySet.asScala.map(k => org.scalatest.Entry(k, collection.get(k)))
      }
    }
}
