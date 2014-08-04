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
package org.scalactic

import Requirements._

case class DigitString(digits: String) {
  require(!digits.exists(c => c != '0' && c != '1' && c != '2' && c != '3' && c != '4' && c != '5' && c != '6' && c != '7' && c != '8' && c != '9' && c != '-' ), "a character was not a digit in: " + digits)
  val toInt: Int =
    try digits.toInt
    catch {
      case ex: NumberFormatException =>
        throw new IllegalArgumentException("passed digits were greater than Int.MaxValue or less than Int.MinValue")
    }
}

object DigitString {
  import scala.language.implicitConversions
  implicit def convert(d: DigitString): Int = d.digits.toInt
}
