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

final class Poz private (val value: Int) extends AnyVal with BoundedInt {
  override def toString: String = s"Poz($value)"
}

object Poz {
  def from(value: Int): Option[Poz] =
    if (value >= 0) Some(new Poz(value)) else None
}


final class PozL private (val value: Long) extends AnyVal with BoundedLong {
  override def toString: String = s"PozL($value)"
}

object PozL {
  def from(value: Long): Option[PozL] =
    if (value >= 0L) Some(new PozL(value)) else None
}


final class PozD private (val value: Double) extends AnyVal with BoundedDouble {
  override def toString: String = s"PozD($value)"
}

object PozD {
  def from(value: Double): Option[PozD] =
    if (value >= 0.0) Some(new PozD(value)) else None
}


final class PozF private (val value: Float) extends AnyVal with BoundedFloat {
  override def toString: String = s"PozF($value)"
}

object PozF {
  def from(value: Float): Option[PozF] =
    if (value >= 0.0F) Some(new PozF(value)) else None
}

