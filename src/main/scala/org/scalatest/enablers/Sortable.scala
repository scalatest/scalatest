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

trait Sortable[E] {
  def isSorted(seq: E): Boolean
}

object Sortable {
  
  implicit def sortableForSeq[E, SEQ[_] <: scala.collection.Seq[_]](implicit ordering: Ordering[E]): Sortable[SEQ[E]] =
    new Sortable[SEQ[E]] {
      def isSorted(o: SEQ[E]) = o.sliding(2).forall { duo => ordering.lteq(duo(0).asInstanceOf[E], duo(1).asInstanceOf[E]) }
    }
  
  /*implicit def sortEnablersForSeq[E, SEQ[E] <: scala.collection.SeqLike[E, _]](implicit ordering: Ordering[E]): Sortable[SEQ[E]] =
    new Sortable[SEQ[E]] {
      def isSorted(seq: SEQ[E]): Boolean = {
        @tailrec
        def checkSort(current: E, itr: Iterator[E]): Boolean = {
          if (itr.hasNext) {
            val next = itr.next
            if (ordering.lteq(current, next))
              checkSort(next, itr)
            else
              false
          }
          else
            true
        }
        val itr = seq.iterator
        if (itr.hasNext)
          checkSort(itr.next, itr)
        else
          true
      }
    }*/
  
}