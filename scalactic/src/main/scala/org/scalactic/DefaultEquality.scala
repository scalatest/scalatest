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
package org.scalactic

import org.scalactic.anyvals.NonEmptyArray

/**
 * A default <code>Equality</code> type class implementation (which can be used for any type) whose
 * <code>areEqual</code> method compares the passed objects with <code>==</code>, calling <code>.deep</code>
 * first on any passed object that is an array.
 * </p>
 */
private[scalactic] final class DefaultEquality[A] extends Equality[A] {

  private[scalactic] def areEqualComparingArraysStructurally(left: Any, right: Any): Boolean = {
    // Prior to 2.0 this only called .deep if both sides were arrays. Loosened it
    // when nearing 2.0.M6 to call .deep if either left or right side is an array.
    // TODO: this is the same algo as in scalactic.DefaultEquality. Put that one in
    // a singleton and use it in both places.
    left match {
      case leftArray: Array[_] =>
        right match {
          case rightArray: Array[_] => leftArray.deep == rightArray.deep
          case rightNonEmptyArray: NonEmptyArray[_] => leftArray.deep == rightNonEmptyArray.toArray.deep
          case _ => leftArray.deep == right
        }
      case leftNonEmptyArray: NonEmptyArray[_] =>
        right match {
          case rightArray: Array[_] => leftNonEmptyArray.toArray.deep == rightArray.deep
          case rightNonEmptyArray: NonEmptyArray[_] => leftNonEmptyArray.toArray.deep == rightNonEmptyArray.toArray.deep
          case _ => leftNonEmptyArray.toArray.deep == right
        }

      case other => {
        right match {
          case rightArray: Array[_] => left == rightArray.deep
          case rightNonEmptyArray: NonEmptyArray[_] => left == rightNonEmptyArray.toArray.deep
          case _ => left == right
        }
      }
    }
  }

  /**
   * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal by invoking <code>==</code> on <code>a</code>
   * passing in <code>b</code>, treating arrays specially by invoking <code>.deep</code> on <code>a</code> and/or </code>b</code> if they
   * are arrays, and using the result or results of invoking <code>.deep</code> in the equality check.
   *
   * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   */
  def areEqual(a: A, b: Any): Boolean = {
    areEqualComparingArraysStructurally(a, b)
    /*a match {
      case arr: Array[_] =>
        b match {
          case brr: Array[_] => arr.deep == brr.deep
          case brr: NonEmptyArray[_] => arr.deep == brr.toArray.deep
          case _ => arr.deep == b
        }

      case arr: NonEmptyArray[_] =>
        b match {
          case brr: Array[_] => arr.toArray.deep == brr.deep
          case brr: NonEmptyArray[_] => arr.toArray.deep == brr.toArray.deep
          case _ => arr.toArray.deep == b
        }

      case Some(elem) if b.isInstanceOf[Some[_]] =>
        areEqual(elem, right.asInstanceOf[Some[_]].get)

      case _ => {
        b match {
          case brr: Array[_] => a == brr.deep
          case _ => a == b
        }
      }
    }*/
  }

  override def toString: String = "Equality.default"
}

