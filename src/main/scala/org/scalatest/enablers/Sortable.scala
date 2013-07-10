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
import Aggregating.tryEquality

/**
 * Supertrait for typeclasses that enable the <code>be</code> <code>sorted</code> matcher syntax.
 *
 * <p>
 * A <code>Sortable[S]</code> provides access to the "sortable nature" of type <code>S</code> in such
 * a way that <code>be</code> <code>sorted</code> matcher syntax can be used with type <code>S</code>. An <code>S</code>
 * can be any type for which the concept of being sorted makes sense, such as sequences. ScalaTest provides
 * implicit implementations for several types. You can enable the <code>be</code> <code>sorted</code> matcher syntax on your own
 * type <code>U</code> by defining a <code>Sortable[U]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides an implicit <code>Sortable</code> instance for <code>scala.collection.Seq</code>
 * in the <code>Sortable</code> companion object.
 * </p>
 */
trait Sortable[S] {

  /**
   * Determines whether the passed sequence is sorted, <em>i.e.</em>, the elements of the passed sequence are in sorted order.
   */
  def isSorted(sequence: S): Boolean
}

object Sortable {

// Sliding doesn't exist on GenSeq, and this is inherently sequential, so make them say .seq if they have a parallel Seq
// Actually on second thought, I think just do a .seq on it.
  implicit def sortableNatureOfSeq[E, SEQ[e] <: scala.collection.GenSeq[e]](implicit ordering: Ordering[E]): Sortable[SEQ[E]] =
    new Sortable[SEQ[E]] {
      def isSorted(o: SEQ[E]): Boolean = o.seq.sliding(2).forall { duo => ordering.lteq(duo(0), duo(1)) }
    }

  implicit def sortableNatureOfArray[E](implicit ordering: Ordering[E]): Sortable[Array[E]] = 
    new Sortable[Array[E]] {
      def isSorted(o: Array[E]): Boolean = o.sliding(2).forall { duo => ordering.lteq(duo(0), duo(1)) }
    }

  implicit def sortableNatureOfJavaList[E, JLIST[e] <: java.util.List[e]](implicit ordering: Ordering[E]): Sortable[JLIST[E]] = 
    new Sortable[JLIST[E]] {
      def isSorted(o: JLIST[E]): Boolean = o.asScala.sliding(2).forall { duo => ordering.lteq(duo(0), duo(1)) }
    }
}

