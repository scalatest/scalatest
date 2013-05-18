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
  def contains(holder: A, element: Any): Boolean
  def containsOneOf(holder: A, elements: scala.collection.Seq[Any]): Boolean
/*
  def containsNoneOf(holder: A, elements: scala.collection.Seq[Any]): Boolean
*/
}

/*
  @tailrec
  def containsOneOf[T](left: T, rightItr: Iterator[Any])(implicit holder: Holder[T]): Boolean = {
    if (rightItr.hasNext) {
      val nextRight = rightItr.next
      if (holder.contains(left, nextRight)) // Found one of right in left, can succeed early
        true
      else
        containsOneOf(left, rightItr)
    }
    else // No more elements in right, left does not contain one of right.
      false
  }
*/
  
object Holder {

  implicit def withJavaCollectionElementEquality[E, JCOL[_] <: java.util.Collection[_]](implicit equality: Equality[E]): Holder[JCOL[E]] = 
    new Holder[JCOL[E]] {
      def contains(javaColl: JCOL[E], ele: Any): Boolean = {
        val it: java.util.Iterator[E] = javaColl.iterator.asInstanceOf[java.util.Iterator[E]]
        var found = false
        while (!found && it.hasNext) {
          found = equality.areEqual(it.next , ele)
        }
        found
      }
      def containsOneOf(javaColl: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = throw new Exception("Not Yet Implemented")
    }

  implicit def convertEqualityToJavaCollectionHolder[E, JCOL[_] <: java.util.Collection[_]](equality: Equality[E]): Holder[JCOL[E]] = 
    withJavaCollectionElementEquality(equality)

  implicit def withGenTraversableElementEquality[E, TRAV[_] <: scala.collection.GenTraversable[_]](implicit equality: Equality[E]): Holder[TRAV[E]] = 
    new Holder[TRAV[E]] {
      def contains(trav: TRAV[E], ele: Any): Boolean = {
        equality match {
          case normEq: NormalizingEquality[_] => 
            val normRight = normEq.normalizedIfInstanceOfA(ele)
            trav.exists((e: Any) => normEq.afterNormalizationEquality.areEqual(normEq.normalized(e.asInstanceOf[E]), normRight)) // Don't know why the compiler requires e to be type Any. Should be E.
          case _ => trav.exists((e: Any) => equality.areEqual(e.asInstanceOf[E], ele)) // Don't know why the compiler requires e to be type Any. Should be E.
        }
      }
      def containsOneOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        trav.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToGenTraversableHolder[E, TRAV[_] <: scala.collection.GenTraversable[_]](equality: Equality[E]): Holder[TRAV[E]] = 
    withGenTraversableElementEquality(equality)

  // OPT so that it will work with Some also, but it doesn't work with None
  implicit def withOptionValueEquality[E, OPT[_] <: Option[_]](implicit equality: Equality[E]): Holder[OPT[E]] = 
    new Holder[OPT[E]] {
      def contains(opt: OPT[E], ele: Any): Boolean = {
        opt.exists((e: Any) => equality.areEqual(e.asInstanceOf[E], ele)) // Don't know why the compiler requires e to be type Any. Should be E.
      }
      def containsOneOf(opt: OPT[E], elements: scala.collection.Seq[Any]): Boolean = {
        opt.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
    }

  // supports (some should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToOptionHolder[E, OPT[_] <: Option[_]](equality: Equality[E]): Holder[OPT[E]] = 
    withOptionValueEquality(equality)

  implicit def withGenMapElementEquality[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](implicit equality: Equality[(K, V)]): Holder[MAP[K, V]] = 
    new Holder[MAP[K, V]] {
      def contains(map: MAP[K, V], ele: Any): Boolean = {
        map.exists((e: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)) // Don't know why the compiler requires e to be type Any. Should be E.
      }
      def containsOneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = throw new Exception("Not Yet Implemented")
    }

  implicit def convertEqualityToGenMapHolder[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](equality: Equality[(K, V)]): Holder[MAP[K, V]] = 
    withGenMapElementEquality(equality)

  implicit def withArrayElementEquality[E](implicit equality: Equality[E]): Holder[Array[E]] = 
    new Holder[Array[E]] {
      def contains(arr: Array[E], ele: Any): Boolean =
        arr.exists((e: E) => equality.areEqual(e, ele))
      def containsOneOf(arr: Array[E], elements: scala.collection.Seq[Any]): Boolean = throw new Exception("Not Yet Implemented")
    }

  implicit def convertEqualityToArrayHolder[E](equality: Equality[E]): Holder[Array[E]] = 
    withArrayElementEquality(equality)

  implicit def withStringCharacterEquality(implicit equality: Equality[Char]): Holder[String] = 
    new Holder[String] {
      def contains(str: String, ele: Any): Boolean =
        str.exists((e: Char) => equality.areEqual(e, ele))
      def containsOneOf(str: String, elements: scala.collection.Seq[Any]): Boolean = throw new Exception("Not Yet Implemented")
    }

  implicit def convertEqualityToStringHolder(equality: Equality[Char]): Holder[String] = 
    withStringCharacterEquality(equality)
}

