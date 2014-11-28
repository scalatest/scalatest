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

sealed abstract class CooperatingNumeric[T]

object CooperatingNumeric {

  private object CooperatingByte extends CooperatingNumeric[Byte] {
    override def toString: String = "CooperatingNumeric[Byte]"
  }
  private object CooperatingShort extends CooperatingNumeric[Short] {
    override def toString: String = "CooperatingNumeric[Short]"
  }
  private object CooperatingChar extends CooperatingNumeric[Char] {
    override def toString: String = "CooperatingNumeric[Char]"
  }
  private object CooperatingInt extends CooperatingNumeric[Int] {
    override def toString: String = "CooperatingNumeric[Int]"
  }
  private object CooperatingLong extends CooperatingNumeric[Long] {
    override def toString: String = "CooperatingNumeric[Long]"
  }
  private object CooperatingFloat extends CooperatingNumeric[Float] {
    override def toString: String = "CooperatingNumeric[Float]"
  }
  private object CooperatingDouble extends CooperatingNumeric[Double] {
    override def toString: String = "CooperatingNumeric[Doubl]"
  }
  private object CooperatingBigInt extends CooperatingNumeric[BigInt] {
    override def toString: String = "CooperatingNumeric[BigInt]"
  }
  private object CooperatingBigDecimal extends CooperatingNumeric[BigDecimal] {
    override def toString: String = "CooperatingNumeric[BigDecimal]"
  }
  private object CooperatingJavaLangByte extends CooperatingNumeric[java.lang.Byte] {
    override def toString: String = "CooperatingNumeric[java.lang.Byte]"
  }
  private object CooperatingJavaLangShort extends CooperatingNumeric[java.lang.Short] {
    override def toString: String = "CooperatingNumeric[java.lang.Short]"
  }
  private object CooperatingJavaLangCharacter extends CooperatingNumeric[java.lang.Character] {
    override def toString: String = "CooperatingNumeric[java.lang.Character]"
  }
  private object CooperatingJavaLangInteger extends CooperatingNumeric[java.lang.Integer] {
    override def toString: String = "CooperatingNumeric[java.lang.Integer]"
  }
  private object CooperatingJavaLangLong extends CooperatingNumeric[java.lang.Long] {
    override def toString: String = "CooperatingNumeric[java.lang.Long]"
  }
  private object CooperatingJavaLangFloat extends CooperatingNumeric[java.lang.Float] {
    override def toString: String = "CooperatingNumeric[java.lang.Float]"
  }
  private object CooperatingJavaLangDouble extends CooperatingNumeric[java.lang.Double] {
    override def toString: String = "CooperatingNumeric[java.lang.Double]"
  }

  implicit val cooperatingNumericNatureOfByte: CooperatingNumeric[Byte] = CooperatingByte
  implicit val cooperatingNumericNatureOfShort: CooperatingNumeric[Short] = CooperatingShort
  implicit val cooperatingNumericNatureOfChar: CooperatingNumeric[Char] = CooperatingChar
  implicit val cooperatingNumericNatureOfInt: CooperatingNumeric[Int] = CooperatingInt
  implicit val cooperatingNumericNatureOfLong: CooperatingNumeric[Long] = CooperatingLong
  implicit val cooperatingNumericNatureOfFloat: CooperatingNumeric[Float] = CooperatingFloat
  implicit val cooperatingNumericNatureOfDouble: CooperatingNumeric[Double] = CooperatingDouble
  implicit val cooperatingNumericNatureOfBigInt: CooperatingNumeric[BigInt] = CooperatingBigInt
  implicit val cooperatingNumericNatureOfBigDecimal: CooperatingNumeric[BigDecimal] = CooperatingBigDecimal
  implicit val cooperatingNumericNatureOfJavaByte: CooperatingNumeric[java.lang.Byte] = CooperatingJavaLangByte
  implicit val cooperatingNumericNatureOfJavaShort: CooperatingNumeric[java.lang.Short] = CooperatingJavaLangShort
  implicit val cooperatingNumericNatureOfJavaCharacter: CooperatingNumeric[java.lang.Character] = CooperatingJavaLangCharacter
  implicit val cooperatingNumericNatureOfJavaInteger: CooperatingNumeric[java.lang.Integer] = CooperatingJavaLangInteger
  implicit val cooperatingNumericNatureOfJavaLong: CooperatingNumeric[java.lang.Long] = CooperatingJavaLangLong
  implicit val cooperatingNumericNatureOfJavaFloat: CooperatingNumeric[java.lang.Float] = CooperatingJavaLangFloat
  implicit val cooperatingNumericNatureOfJavaDouble: CooperatingNumeric[java.lang.Double] = CooperatingJavaLangDouble
}
