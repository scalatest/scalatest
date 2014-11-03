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
  implicit def posWideningNatureOfPosL: PosWidening[PosL] { type ResultType = Long } =
    new PosWidening[PosL] {
      type ResultType = Long 
      def add(x: Int, y: PosL): Long = x + y.value
    }
  implicit def posWideningNatureOfPosF: PosWidening[PosF] { type ResultType = Float } =
    new PosWidening[PosF] {
      type ResultType = Float 
      def add(x: Int, y: PosF): Float = x + y.value
    }
  implicit def posWideningNatureOfPosD: PosWidening[PosD] { type ResultType = Double } =
    new PosWidening[PosD] {
      type ResultType = Double 
      def add(x: Int, y: PosD): Double = x + y.value
    }

  implicit def posWideningNatureOfPoz: PosWidening[Poz] { type ResultType = Int } =
    new PosWidening[Poz] {
      type ResultType = Int 
      def add(x: Int, y: Poz): Int = x + y.value
    }
  implicit def posWideningNatureOfPozL: PosWidening[PozL] { type ResultType = Long } =
    new PosWidening[PozL] {
      type ResultType = Long 
      def add(x: Int, y: PozL): Long = x + y.value
    }
  implicit def posWideningNatureOfPozF: PosWidening[PozF] { type ResultType = Float } =
    new PosWidening[PozF] {
      type ResultType = Float 
      def add(x: Int, y: PozF): Float = x + y.value
    }
  implicit def posWideningNatureOfPozD: PosWidening[PozD] { type ResultType = Double } =
    new PosWidening[PozD] {
      type ResultType = Double 
      def add(x: Int, y: PozD): Double = x + y.value
    }
}
