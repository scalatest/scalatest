/*
 * Copyright 2001-2013 Artima, Inc.
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

sealed class CooperatingNumeric[T]

object CooperatingNumeric {
  implicit val cooperatingNumericNatureOfByte: CooperatingNumeric[Byte] = new CooperatingNumeric[Byte]
  implicit val cooperatingNumericNatureOfShort: CooperatingNumeric[Short] = new CooperatingNumeric[Short]
  implicit val cooperatingNumericNatureOfChar: CooperatingNumeric[Char] = new CooperatingNumeric[Char]
  implicit val cooperatingNumericNatureOfInt: CooperatingNumeric[Int] = new CooperatingNumeric[Int]
  implicit val cooperatingNumericNatureOfLong: CooperatingNumeric[Long] = new CooperatingNumeric[Long]
  implicit val cooperatingNumericNatureOfFloat: CooperatingNumeric[Float] = new CooperatingNumeric[Float]
  implicit val cooperatingNumericNatureOfDouble: CooperatingNumeric[Double] = new CooperatingNumeric[Double]
  implicit val cooperatingNumericNatureOfBigInt: CooperatingNumeric[BigInt] = new CooperatingNumeric[BigInt]
  implicit val cooperatingNumericNatureOfBigDecimal: CooperatingNumeric[BigDecimal] = new CooperatingNumeric[BigDecimal]
}
