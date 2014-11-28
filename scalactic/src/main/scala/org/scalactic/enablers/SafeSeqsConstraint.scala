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
package org.scalactic.enablers

import org.scalactic.{Equality, NormalizingEquality, Every, EqualityConstraint}
import scala.collection.{GenTraversableOnce, GenTraversable}
import org.scalactic.EqualityPolicy.BasicEqualityConstraint
import annotation.implicitNotFound
import scala.language.higherKinds

@implicitNotFound(msg = "Could not find evidence that ${R} can be contained in ${C}; the missing implicit parameter is of type org.scalactic.enablers.SafeSeqsConstraint[${C},${R}]")
trait SafeSeqsConstraint[-C, R] {

  def contains(container: C, element: R): Boolean
  def indexOf(container: C, element: R, from: Int): Int
  def lastIndexOf(container: C, element: R, end: Int): Int
  def indexOfSlice(container: C, slice: scala.collection.GenSeq[R], from: Int): Int
  def lastIndexOfSlice(container: C, slice: scala.collection.GenSeq[R], from: Int): Int

}


object SafeSeqsConstraint {

  implicit def containingNatureOfGenSeq[E, GENSEQ[e] <: scala.collection.GenSeq[e], R](implicit constraint: R <:< E): SafeSeqsConstraint[GENSEQ[E], R] = 
    new SafeSeqsConstraint[GENSEQ[E], R] {
      def contains(genSeq: GENSEQ[E], ele: R): Boolean = {
        genSeq match {
          case seq: Seq[_] => seq.contains(ele)
          case _ => genSeq.exists(_ == ele)
        }
      }
      def indexOf(genSeq: GENSEQ[E], element: R, from: Int): Int =
        genSeq.indexOf(element, from)
      def lastIndexOf(genSeq: GENSEQ[E], element: R, end: Int): Int =
        genSeq.lastIndexOf(element, end)
      def indexOfSlice(genSeq: GENSEQ[E], slice: scala.collection.GenSeq[R], from: Int): Int =
        genSeq.toStream.indexOfSlice(slice, if (from < 0) 0 else from)
      def lastIndexOfSlice(genSeq: GENSEQ[E], slice: scala.collection.GenSeq[R], from: Int): Int =
        genSeq.toStream.lastIndexOfSlice(slice, if (from < 0) 0 else from)
    }
  implicit def containingNatureOfArray[E, ARRAY[e] <: Array[e], R](implicit constraint: R <:< E): SafeSeqsConstraint[ARRAY[E], R] = 
    new SafeSeqsConstraint[ARRAY[E], R] {
      def contains(array: ARRAY[E], ele: R): Boolean = {
        array.contains(ele)
      }
      def indexOf(array: ARRAY[E], element: R, from: Int): Int =
        array.indexOf(element, from)
      def lastIndexOf(array: ARRAY[E], element: R, end: Int): Int =
        array.lastIndexOf(element, end)
      def indexOfSlice(array: ARRAY[E], slice: scala.collection.GenSeq[R], from: Int): Int =
        array.indexOfSlice(slice, if (from < 0) 0 else from)
      def lastIndexOfSlice(array: ARRAY[E], slice: scala.collection.GenSeq[R], from: Int): Int =
        array.lastIndexOfSlice(slice, if (from < 0) 0 else from)
    }
  implicit def containingNatureOfEvery[E, EVERY[e] <: Every[e], R](implicit constraint: R <:< E): SafeSeqsConstraint[EVERY[E], R] = 
    new SafeSeqsConstraint[EVERY[E], R] {
      def contains(every: EVERY[E], ele: R): Boolean = {
        every.toVector.contains(ele)
      }
      def indexOf(every: EVERY[E], element: R, from: Int): Int =
        every.toVector.indexOf(element, from)
      def lastIndexOf(every: EVERY[E], element: R, end: Int): Int =
        every.toVector.lastIndexOf(element, end)
      def indexOfSlice(every: EVERY[E], slice: scala.collection.GenSeq[R], from: Int): Int =
        every.toStream.indexOfSlice(slice, if (from < 0) 0 else from)
      def lastIndexOfSlice(every: EVERY[E], slice: scala.collection.GenSeq[R], from: Int): Int =
        every.toStream.lastIndexOfSlice(slice, if (from < 0) 0 else from)
    }
}


