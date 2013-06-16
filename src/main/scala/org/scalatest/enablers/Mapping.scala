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
import scala.collection.JavaConverters._

/**
 * Supertrait for typeclasses that enable <code>contain</code> matcher syntax for aggregations.
 *
 * <p>
 * An <code>Mapping[A]</code> provides access to the "aggregating nature" of type <code>A</code> in such
 * a way that relevant <code>contain</code> matcher syntax can be used with type <code>A</code>. An <code>A</code>
 * can be any type of "aggregation," a type that in some way aggregates or brings together other types. ScalaTest provides
 * implicit implementations for several types. You can enable the <code>contain</code> matcher syntax on your own
 * type <code>U</code> by defining an <code>Mapping[U}</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides implicit <code>Mapping</code> instances for <code>scala.collection.GenTraversable</code>,
 * <code>java.util.Collection</code>, <code>java.util.Map</code>, <code>String</code>, and <code>Array</code> in the
 * <code>Mapping</code> companion object.
 * </p>
 */
trait Mapping[M] {

  /**
   * Implements <code>contain</code> <code>atLeastOneOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements at least one of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains at least one of the passed elements
   */
  def containsKey(map: M, key: Any): Boolean

  def containsValue(map: M, value: Any): Boolean
/*
  // TODO: Write tests that a NotAllowedException is thrown when no elements are passed, maybe if only one element is passed, and 
  // likely if an object is repeated in the list.
  def containsKeys(mapping: M, keys: Seq[Any]): Boolean

  def containsValues(mapping: M, values: Seq[Any]): Boolean
*/
}

object Mapping {

/*
  implicit def mappingNatureOfGenMap[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](implicit equality: Equality[(K, V)]): Mapping[MAP[K, V]] = 
    new Mapping[MAP[K, V]] {
      def containsKey(map: MAP[K, V], key: Any): Boolean = {
        val genMap = map.asInstanceOf[scala.collection.GenMap[K, V]]
        genMap.keySet.exists((e: Any) => equality.areEqual(e.asInstanceOf[E], key))
      }
      def containsValue(map: MAP[K, V], value: Any): Boolean = {
        val genMap = map.asInstanceOf[scala.collection.GenMap[K, V]]
        genMap.values.exists((e: Any) => equality.areEqual(e.asInstanceOf[E], value))
      }
    }

  implicit def convertEqualityToGenMapMapping[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](equality: Equality[(K, V)]): Mapping[MAP[K, V]] = 
    mappingNatureOfGenMap(equality)
    
  implicit def mappingNatureOfJavaMap[K, V, JMAP[_, _] <: java.util.Map[_, _]](implicit equality: Equality[(K, V)]): Mapping[JMAP[K, V]] = 
    new Mapping[JMAP[K, V]] {
      def containsKey(map: JMAP[K, V], key: Any): Boolean = {
      }
      def containsValue(map: JMAP[K, V], value: Any): Boolean = {
      }
    }

  implicit def convertEqualityToJavaMapMapping[K, V, JMAP[_, _] <: java.util.Map[_, _]](equality: Equality[(K, V)]): Mapping[JMAP[K, V]] = 
    mappingNatureOfJavaMap(equality)
*/
}
