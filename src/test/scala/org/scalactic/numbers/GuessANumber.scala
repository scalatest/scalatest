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
package org.scalactic.numbers

final class GuessANumber private (val value: Int) extends AnyVal {
  override def toString: String = s"GuessANumber($value)"
}

object GuessANumber {
	  def from(value: Int): Option[GuessANumber] =
	    if (value >= 1 && value <= 10) Some(new GuessANumber(value)) else None
}

final class LGuessANumber private (val value: Long) extends AnyVal {
  override def toString: String = s"LGuessANumber($value)"
}

object LGuessANumber {
  def from(value: Long): Option[LGuessANumber] =
    if (value >= 1L && value <= 10L) Some(new LGuessANumber(value)) else None
}

final class FGuessANumber private (val value: Float) extends AnyVal {
  override def toString: String = s"FGuessANumber($value)"
}

object FGuessANumber {
  def from(value: Float): Option[FGuessANumber] =
    if (value >= 1.0F && value <= 10.0F) Some(new FGuessANumber(value)) else None
}

final class DGuessANumber private (val value: Double) extends AnyVal {
  override def toString: String = s"DGuessANumber($value)"
}

object DGuessANumber {
  def from(value: Double): Option[DGuessANumber] =
    if (value >= 1.0 && value <= 10.0) Some(new DGuessANumber(value)) else None
}

