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

trait Aggregation[A] {
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

object Aggregation {

  implicit def withGenTraversableElementEquality[E, TRAV[_] <: scala.collection.GenTraversable[_]](implicit equality: Equality[E]): Aggregation[TRAV[E]] = 
    new Aggregation[TRAV[E]] {
      def containsAtLeastOneOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        trav.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToGenTraversableAggregation[E, TRAV[_] <: scala.collection.GenTraversable[_]](equality: Equality[E]): Aggregation[TRAV[E]] = 
    withGenTraversableElementEquality(equality)
    
  implicit def withArrayElementEquality[E](implicit equality: Equality[E]): Aggregation[Array[E]] = 
    new Aggregation[Array[E]] {
      def containsAtLeastOneOf(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        new ArrayWrapper(array).exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToArrayAggregation[E](equality: Equality[E]): Aggregation[Array[E]] = 
    withArrayElementEquality(equality)
  
  implicit def withStringCharacterEquality(implicit equality: Equality[Char]): Aggregation[String] = 
    new Aggregation[String] {
      def containsAtLeastOneOf(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        s.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[Char], ele)))
      }
    }

  implicit def convertEqualityToStringAggregation(equality: Equality[Char]): Aggregation[String] = 
    withStringCharacterEquality(equality)
    
  implicit def withGenMapElementEquality[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](implicit equality: Equality[(K, V)]): Aggregation[MAP[K, V]] = 
    new Aggregation[MAP[K, V]] {
      def containsAtLeastOneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        map.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)))
      }
    }

  implicit def convertEqualityToGenMapAggregation[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](equality: Equality[(K, V)]): Aggregation[MAP[K, V]] = 
    withGenMapElementEquality(equality)
    
  implicit def withJavaCollectionElementEquality[E, JCOL[_] <: java.util.Collection[_]](implicit equality: Equality[E]): Aggregation[JCOL[E]] = 
    new Aggregation[JCOL[E]] {
      def containsAtLeastOneOf(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        import scala.collection.JavaConverters._
        col.asInstanceOf[java.util.Collection[E]].asScala.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
    }

  implicit def convertEqualityToJavaCollectionAggregation[E, JCOL[_] <: java.util.Collection[_]](equality: Equality[E]): Aggregation[JCOL[E]] = 
    withJavaCollectionElementEquality(equality)
    
  implicit def withJavaMapElementEquality[K, V, JMAP[_, _] <: java.util.Map[_, _]](implicit equality: Equality[(K, V)]): Aggregation[JMAP[K, V]] = 
    new Aggregation[JMAP[K, V]] {
      def containsAtLeastOneOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        import scala.collection.JavaConverters._
        map.asInstanceOf[java.util.Map[K, V]].asScala.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)))
      }
    }

  implicit def convertEqualityToJavaMapAggregation[K, V, JMAP[_, _] <: java.util.Map[_, _]](equality: Equality[(K, V)]): Aggregation[JMAP[K, V]] = 
    withJavaMapElementEquality(equality)
}
