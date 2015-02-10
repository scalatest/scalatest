/*
* Copyright 2001-2014 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalactic.anyvals

// These are here emporaritly for testing, because need to have the compile separate. Can't I just move them?
private[scalactic] final class Percent private (val value: Int) extends AnyVal {
  override def toString: String = s"Percent($value)"
}

private[scalactic] object Percent {
  def from(value: Int): Option[Percent] =
    if (value >= 0 && value <= 100) Some(new Percent(value)) else None
  import scala.language.experimental.macros
  def apply(value: Int): Percent = macro PercentMacro.apply
}

private[scalactic] final class LPercent private (val value: Long) extends AnyVal {
  override def toString: String = s"LPercent($value)"
}

private[scalactic] object LPercent {
  def from(value: Long): Option[LPercent] =
    if (value >= 0L && value <= 100L) Some(new LPercent(value)) else None
}

private[scalactic] final class FPercent private (val value: Float) extends AnyVal {
  override def toString: String = s"FPercent($value)"
}

private[scalactic] object FPercent {
  def from(value: Float): Option[FPercent] =
    if (value >= 0.0F && value <= 100.0F) Some(new FPercent(value)) else None
}

private[scalactic] final class DPercent private (val value: Double) extends AnyVal {
  override def toString: String = s"DPercent($value)"
}

private[scalactic] object DPercent {
  def from(value: Double): Option[DPercent] =
    if (value >= 0.0 && value <= 100.0) Some(new DPercent(value)) else None
}

private[scalactic] final class TLA private (val value: String) extends AnyVal {
  override def toString: String = s"TLA($value)"
}

private[scalactic] object TLA {
  def from(value: String): Option[TLA] =
    if (value.length == 3) Some(new TLA(value)) else None
  import scala.language.experimental.macros
  def apply(value: String): TLA = macro TLAMacro.apply
}

private[scalactic] final class Digit private (val value: Char) extends AnyVal {
  override def toString: String = s"Digit($value)"
}
private[scalactic] object Digit {
  def from(value: Char): Option[Digit] =
    if (value >= '0' && value <= '9') Some(new Digit(value)) else None
  import scala.language.experimental.macros
  def apply(value: Char): Digit = macro DigitMacro.apply
}

