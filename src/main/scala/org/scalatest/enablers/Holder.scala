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
import org.scalautils.NormalizingEquality

trait Holder[A] {
  def containsElement(holder: A, element: Any): Boolean
/*
  def containsOneOf(holder: A, elements: scala.collection.Seq[Any]): Boolean
  def containsNoneOf(holder: A, elements: scala.collection.Seq[Any]): Boolean
*/
}

object Holder {

  implicit def withJavaCollectionElementEquality[E, JCOL[_] <: java.util.Collection[_]](implicit equality: Equality[E]): Holder[JCOL[E]] = 
    new Holder[JCOL[E]] {
      def containsElement(javaColl: JCOL[E], ele: Any): Boolean = {
        val it: java.util.Iterator[E] = javaColl.iterator.asInstanceOf[java.util.Iterator[E]]
        var found = false
        while (!found && it.hasNext) {
          found = equality.areEqual(it.next , ele)
        }
        found
      }
    }

  implicit def convertEqualityToJavaCollectionHolder[E, JCOL[_] <: java.util.Collection[_]](equality: Equality[E]): Holder[JCOL[E]] = 
    withJavaCollectionElementEquality(equality)

  implicit def withGenTraversableElementEquality[E, TRAV[_] <: scala.collection.GenTraversable[_]](implicit equality: Equality[E]): Holder[TRAV[E]] = 
    new Holder[TRAV[E]] {
      def containsElement(trav: TRAV[E], ele: Any): Boolean = {
        equality match {
          case normEq: NormalizingEquality[_] => 
            val normRight = normEq.normalizedIfInstanceOfA(ele)
            trav.exists((e: Any) => normEq.afterNormalizationEquality.areEqual(normEq.normalized(e.asInstanceOf[E]), normRight)) // Don't know why the compiler requires e to be type Any. Should be E.
          case _ => trav.exists((e: Any) => equality.areEqual(e.asInstanceOf[E], ele)) // Don't know why the compiler requires e to be type Any. Should be E.
        }
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToGenTraversableHolder[E, TRAV[_] <: scala.collection.GenTraversable[_]](equality: Equality[E]): Holder[TRAV[E]] = 
    withGenTraversableElementEquality(equality)

  // OPT so that it will work with Some also, but it doesn't work with None
  implicit def withOptionValueEquality[E, OPT[_] <: Option[_]](implicit equality: Equality[E]): Holder[OPT[E]] = 
    new Holder[OPT[E]] {
      def containsElement(opt: OPT[E], ele: Any): Boolean = {
        opt.exists((e: Any) => equality.areEqual(e.asInstanceOf[E], ele)) // Don't know why the compiler requires e to be type Any. Should be E.
      }
    }

  // supports (some should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToOptionHolder[E, OPT[_] <: Option[_]](equality: Equality[E]): Holder[OPT[E]] = 
    withOptionValueEquality(equality)

  implicit def withGenMapElementEquality[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](implicit equality: Equality[(K, V)]): Holder[MAP[K, V]] = 
    new Holder[MAP[K, V]] {
      def containsElement(map: MAP[K, V], ele: Any): Boolean = {
        map.exists((e: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)) // Don't know why the compiler requires e to be type Any. Should be E.
      }
    }

  implicit def convertEqualityToGenMapHolder[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](equality: Equality[(K, V)]): Holder[MAP[K, V]] = 
    withGenMapElementEquality(equality)

  implicit def withArrayElementEquality[E](implicit equality: Equality[E]): Holder[Array[E]] = 
    new Holder[Array[E]] {
      def containsElement(arr: Array[E], ele: Any): Boolean =
        arr.exists((e: E) => equality.areEqual(e, ele))
    }

  implicit def convertEqualityToArrayHolder[E](equality: Equality[E]): Holder[Array[E]] = 
    withArrayElementEquality(equality)

  implicit def withStringCharacterEquality(implicit equality: Equality[Char]): Holder[String] = 
    new Holder[String] {
      def containsElement(str: String, ele: Any): Boolean =
        str.exists((e: Char) => equality.areEqual(e, ele))
    }

  implicit def convertEqualityToStringHolder(equality: Equality[Char]): Holder[String] = 
    withStringCharacterEquality(equality)
}

