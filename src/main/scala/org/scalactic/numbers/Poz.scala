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
// Numbers greater than or equal to zero.
//
// (Pronounced like "pose".)
//

final class Poz private (val value: Int) extends AnyVal {
  override def toString: String = s"Poz($value)"
}

object Poz {
  def from(value: Int): Option[Poz] =
    if (value >= 0) Some(new Poz(value)) else None
}


final class LPoz private (val value: Long) extends AnyVal {
  override def toString: String = s"LPoz($value)"
}

object LPoz {
  def from(value: Long): Option[LPoz] =
    if (value >= 0L) Some(new LPoz(value)) else None
}


final class DPoz private (val value: Double) extends AnyVal {
  override def toString: String = s"DPoz($value)"
}

object DPoz {
  def from(value: Double): Option[DPoz] =
    if (value >= 0.0) Some(new DPoz(value)) else None
}


final class FPoz private (val value: Float) extends AnyVal {
  override def toString: String = s"FPoz($value)"
}

object FPoz {
  def from(value: Float): Option[FPoz] =
    if (value >= 0.0F) Some(new FPoz(value)) else None
}

