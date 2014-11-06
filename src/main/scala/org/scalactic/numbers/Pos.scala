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

import scala.language.implicitConversions

//
// Numbers greater than zero.
//

final class Pos private (val value: Int) extends AnyVal with BoundedInt {
  override def toString: String = s"Pos($value)"
  def +[T](x: T)(implicit ev: PosWidening[T]): ev.ResultType = ev.add(value, x)
}

class LowPriorityPosDoubleImplicits {
  implicit def widenToDouble(pos: Pos): Double = pos.value
  implicit def widenToPozD(pos: Pos): PozD = PozD.from(pos.value).get
}

class LowPriorityPosFloatImplicits extends LowPriorityPosDoubleImplicits {
  implicit def widenToFloat(pos: Pos): Float = pos.value
  implicit def widenToPozF(pos: Pos): PozF = PozF.from(pos.value).get
}

class LowPriorityPosLongImplicits extends LowPriorityPosFloatImplicits {
  implicit def widenToLong(pos: Pos): Long = pos.value
  implicit def widenToPozL(pos: Pos): PozL = PozL.from(pos.value).get
}

object Pos extends LowPriorityPosLongImplicits {
  def from(value: Int): Option[Pos] =
    if (value > 0) Some(new Pos(value)) else None

  import language.experimental.macros

  implicit def apply(value: Int): Pos = macro PosMacro.apply

  implicit def widenToInt(pos: Pos): Int = pos.value
  implicit def widenToPoz(pos: Pos): Poz = Poz.from(pos.value).get
}

final class PosL private (val value: Long) extends AnyVal with BoundedLong {
  override def toString: String = s"PosL($value)"
}

object PosL {
  def from(value: Long): Option[PosL] =
    if (value > 0L) Some(new PosL(value)) else None
  import language.experimental.macros
  implicit def apply(value: Long): PosL = macro PosLMacro.apply
}

final class PosD private (val value: Double) extends AnyVal with BoundedDouble {
  override def toString: String = s"PosD($value)"
}

object PosD {
  def from(value: Double): Option[PosD] =
    if (value > 0.0) Some(new PosD(value)) else None
}

final class PosF private (val value: Float) extends AnyVal with BoundedFloat {
  override def toString: String = s"PosF($value)"
}

object PosF {
  def from(value: Float): Option[PosF] =
    if (value > 0.0F) Some(new PosF(value)) else None
}

