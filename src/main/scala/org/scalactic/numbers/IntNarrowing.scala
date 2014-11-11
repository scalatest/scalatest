/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalactic.numbers

trait IntNarrowing[T] {
  type ResultType
  def add(x: Int, y: T): ResultType
}

object IntNarrowing {
  implicit def intNarrowingNatureOfInt: IntNarrowing[Int] { type ResultType = Int } =
    new IntNarrowing[Int] {
      type ResultType = Int 
      def add(x: Int, y: Int): Int = x + y
    }
  implicit def intNarrowingNatureOfLong: IntNarrowing[Long] { type ResultType = Long } =
    new IntNarrowing[Long] {
      type ResultType = Long 
      def add(x: Int, y: Long): Long = x + y
    }
  implicit def intNarrowingNatureOfFloat: IntNarrowing[Float] { type ResultType = Float } =
    new IntNarrowing[Float] {
      type ResultType = Float 
      def add(x: Int, y: Float): Float = x + y
    }
  implicit def intNarrowingNatureOfDouble: IntNarrowing[Double] { type ResultType = Double } =
    new IntNarrowing[Double] {
      type ResultType = Double 
      def add(x: Int, y: Double): Double = x + y
    }

  implicit def intNarrowingNatureOfBoundedInt[T <: BoundedInt]: IntNarrowing[T] { type ResultType = Int } =
    new IntNarrowing[T] {
      type ResultType = Int 
      def add(x: Int, y: T): Int = x + y.value
    }
  implicit def intNarrowingNatureOfBoundedLong[T <: BoundedLong]: IntNarrowing[T] { type ResultType = Long } =
    new IntNarrowing[T] {
      type ResultType = Long 
      def add(x: Int, y: T): Long = x + y.value
    }
  implicit def intNarrowingNatureOfBoundedFloat[T <: BoundedFloat]: IntNarrowing[T] { type ResultType = Float } =
    new IntNarrowing[T] {
      type ResultType = Float 
      def add(x: Int, y: T): Float = x + y.value
    }
  implicit def intNarrowingNatureOfBoundedDouble[T <: BoundedDouble]: IntNarrowing[T] { type ResultType = Double } =
    new IntNarrowing[T] {
      type ResultType = Double 
      def add(x: Int, y: T): Double = x + y.value
    }
}
