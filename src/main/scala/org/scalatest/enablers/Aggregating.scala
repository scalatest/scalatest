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

trait Aggregating[A] {
  def containsAtLeastOneOf(aggregation: A, eles: Seq[Any]): Boolean
/*
  def containsTheSameElementsAs(aggregation: A, it: Iterator[Any]): Boolean
  def containsTheSameElementsInOrderAs(aggregation: A, it: Iterator[Any]): Boolean
  def containsAllOf(aggregation: A, eles: Seq[Any]): Boolean
  def containsAtMostOneOf(aggregation: A, eles: Seq[Any]): Boolean
  def containsOnly(aggregation: A, eles: Seq[Any]): Boolean
  def containsInOrderOnly(aggregation: A, eles: Seq[Any]): Boolean
*/
}

object Aggregating {

  implicit def withGenTraversableElementEquality[E, TRAV[_] <: scala.collection.GenTraversable[_]](implicit equality: Equality[E]): Aggregating[TRAV[E]] = 
    new Aggregating[TRAV[E]] {
      def containsAtLeastOneOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        trav.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToGenTraversableAggregating[E, TRAV[_] <: scala.collection.GenTraversable[_]](equality: Equality[E]): Aggregating[TRAV[E]] = 
    withGenTraversableElementEquality(equality)
    
  implicit def withArrayElementEquality[E](implicit equality: Equality[E]): Aggregating[Array[E]] = 
    new Aggregating[Array[E]] {
      def containsAtLeastOneOf(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        new ArrayWrapper(array).exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToArrayAggregating[E](equality: Equality[E]): Aggregating[Array[E]] = 
    withArrayElementEquality(equality)
  
  implicit def withStringCharacterEquality(implicit equality: Equality[Char]): Aggregating[String] = 
    new Aggregating[String] {
      def containsAtLeastOneOf(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        s.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[Char], ele)))
      }
    }

  implicit def convertEqualityToStringAggregating(equality: Equality[Char]): Aggregating[String] = 
    withStringCharacterEquality(equality)
    
  implicit def withGenMapElementEquality[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](implicit equality: Equality[(K, V)]): Aggregating[MAP[K, V]] = 
    new Aggregating[MAP[K, V]] {
      def containsAtLeastOneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        map.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)))
      }
    }

  implicit def convertEqualityToGenMapAggregating[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](equality: Equality[(K, V)]): Aggregating[MAP[K, V]] = 
    withGenMapElementEquality(equality)
    
  implicit def withJavaCollectionElementEquality[E, JCOL[_] <: java.util.Collection[_]](implicit equality: Equality[E]): Aggregating[JCOL[E]] = 
    new Aggregating[JCOL[E]] {
      def containsAtLeastOneOf(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        import scala.collection.JavaConverters._
        col.asInstanceOf[java.util.Collection[E]].asScala.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
    }

  implicit def convertEqualityToJavaCollectionAggregating[E, JCOL[_] <: java.util.Collection[_]](equality: Equality[E]): Aggregating[JCOL[E]] = 
    withJavaCollectionElementEquality(equality)
    
  implicit def withJavaMapElementEquality[K, V, JMAP[_, _] <: java.util.Map[_, _]](implicit equality: Equality[(K, V)]): Aggregating[JMAP[K, V]] = 
    new Aggregating[JMAP[K, V]] {
      def containsAtLeastOneOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        import scala.collection.JavaConverters._
        map.asInstanceOf[java.util.Map[K, V]].asScala.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)))
      }
    }

  implicit def convertEqualityToJavaMapAggregating[K, V, JMAP[_, _] <: java.util.Map[_, _]](equality: Equality[(K, V)]): Aggregating[JMAP[K, V]] = 
    withJavaMapElementEquality(equality)
}
