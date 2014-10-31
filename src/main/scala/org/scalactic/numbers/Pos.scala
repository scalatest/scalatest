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

//
// Numbers greater than zero.
//

final class Pos private (val value: Int) extends AnyVal {
  override def toString: String = s"Pos($value)"
}

object Pos {
  def from(value: Int): Option[Pos] =
    if (value > 0) Some(new Pos(value)) else None
}


final class LPos private (val value: Long) extends AnyVal {
  override def toString: String = s"LPos($value)"
}

object LPos {
  def from(value: Long): Option[LPos] =
    if (value > 0L) Some(new LPos(value)) else None
}


final class DPos private (val value: Double) extends AnyVal {
  override def toString: String = s"DPos($value)"
}

object DPos {
  def from(value: Double): Option[DPos] =
    if (value > 0.0) Some(new DPos(value)) else None
}


final class FPos private (val value: Float) extends AnyVal {
  override def toString: String = s"FPos($value)"
}

object FPos {
  def from(value: Float): Option[FPos] =
    if (value > 0.0F) Some(new FPos(value)) else None
}

