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

case class Complex(real: Double, imaginary: Double)

object Complex {
  import scala.language.implicitConversions
  implicit def convertDouble(d: Double): Complex = Complex(d, 0.0)
  implicit def convertInt(i: Int): Complex = Complex(i, 0.0)
  implicit def convertDigitString(digits: DigitString): Complex = Complex(digits.toInt, 0.0)
}
