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

final class PosInt private (val value: Int) extends AnyVal with BoundedInt with BoundedIntOps {
  override def toString: String = s"PosInt($value)"
}

class LowPriorityPosDoubleImplicits {
  implicit def widenToDouble(pos: PosInt): Double = pos.value
  implicit def widenToPozDouble(pos: PosInt): PozDouble = PozDouble.from(pos.value).get
}

class LowPriorityPosFloatImplicits extends LowPriorityPosDoubleImplicits {
  implicit def widenToFloat(pos: PosInt): Float = pos.value
  implicit def widenToPozFloat(pos: PosInt): PozFloat = PozFloat.from(pos.value).get
}

class LowPriorityPosLongImplicits extends LowPriorityPosFloatImplicits {
  implicit def widenToLong(pos: PosInt): Long = pos.value
  implicit def widenToPozLong(pos: PosInt): PozLong = PozLong.from(pos.value).get
}

object PosInt extends LowPriorityPosLongImplicits {
  def from(value: Int): Option[PosInt] =
    if (value > 0) Some(new PosInt(value)) else None

  import language.experimental.macros
  implicit def apply(value: Int): PosInt = macro PosIntMacro.apply

  implicit def widenToInt(pos: PosInt): Int = pos.value
  implicit def widenToPozInt(pos: PosInt): PozInt = PozInt.from(pos.value).get
}
