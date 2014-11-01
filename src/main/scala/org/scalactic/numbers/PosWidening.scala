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

trait PosWidening[T] {
  type ResultType
  def add(x: Int, y: T): ResultType
}

object PosWidening {
  implicit def posWideningNatureOfInt: PosWidening[Int] { type ResultType = Int } =
    new PosWidening[Int] {
      type ResultType = Int 
      def add(x: Int, y: Int): Int = x + y
    }
  implicit def posWideningNatureOfLong: PosWidening[Long] { type ResultType = Long } =
    new PosWidening[Long] {
      type ResultType = Long 
      def add(x: Int, y: Long): Long = x + y
    }
  implicit def posWideningNatureOfFloat: PosWidening[Float] { type ResultType = Float } =
    new PosWidening[Float] {
      type ResultType = Float 
      def add(x: Int, y: Float): Float = x + y
    }
  implicit def posWideningNatureOfDouble: PosWidening[Double] { type ResultType = Double } =
    new PosWidening[Double] {
      type ResultType = Double 
      def add(x: Int, y: Double): Double = x + y
    }

  implicit def posWideningNatureOfPos: PosWidening[Pos] { type ResultType = Int } =
    new PosWidening[Pos] {
      type ResultType = Int 
      def add(x: Int, y: Pos): Int = x + y.value
    }
  implicit def posWideningNatureOfLPos: PosWidening[LPos] { type ResultType = Long } =
    new PosWidening[LPos] {
      type ResultType = Long 
      def add(x: Int, y: LPos): Long = x + y.value
    }
  implicit def posWideningNatureOfFPos: PosWidening[FPos] { type ResultType = Float } =
    new PosWidening[FPos] {
      type ResultType = Float 
      def add(x: Int, y: FPos): Float = x + y.value
    }
  implicit def posWideningNatureOfDPos: PosWidening[DPos] { type ResultType = Double } =
    new PosWidening[DPos] {
      type ResultType = Double 
      def add(x: Int, y: DPos): Double = x + y.value
    }

  implicit def posWideningNatureOfPoz: PosWidening[Poz] { type ResultType = Int } =
    new PosWidening[Poz] {
      type ResultType = Int 
      def add(x: Int, y: Poz): Int = x + y.value
    }
  implicit def posWideningNatureOfLPoz: PosWidening[LPoz] { type ResultType = Long } =
    new PosWidening[LPoz] {
      type ResultType = Long 
      def add(x: Int, y: LPoz): Long = x + y.value
    }
  implicit def posWideningNatureOfFPoz: PosWidening[FPoz] { type ResultType = Float } =
    new PosWidening[FPoz] {
      type ResultType = Float 
      def add(x: Int, y: FPoz): Float = x + y.value
    }
  implicit def posWideningNatureOfDPoz: PosWidening[DPoz] { type ResultType = Double } =
    new PosWidening[DPoz] {
      type ResultType = Double 
      def add(x: Int, y: DPoz): Double = x + y.value
    }
}
