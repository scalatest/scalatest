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

import EqualityPolicy._

trait StrictEnabledEquality extends EnabledEquality {

  import scala.language.implicitConversions

  // Inherit the Scaladoc for these methods

  override def numericEqualityConstraint[A, B](implicit equalityOfA: Equality[A], numA: CooperatingNumeric[A], numB: CooperatingNumeric[B]): EqualityConstraint[A, B] with NativeSupport = new BasicEqualityConstraint[A, B](equalityOfA)
  override def booleanEqualityConstraint[A, B](implicit equalityOfA: Equality[A], boolA: CooperatingBoolean[A], boolB: CooperatingBoolean[B]): EqualityConstraint[A, B] with NativeSupport = new BasicEqualityConstraint[A, B](equalityOfA)

  implicit def enabledEqualityForChar: EnabledEqualityFor[Char] = EnabledEqualityFor[Char]
  implicit def enabledEqualityForByte: EnabledEqualityFor[Byte] = EnabledEqualityFor[Byte]
  implicit def enabledEqualityForShort: EnabledEqualityFor[Short] = EnabledEqualityFor[Short]
  implicit def enabledEqualityForInt: EnabledEqualityFor[Int] = EnabledEqualityFor[Int]
  implicit def enabledEqualityForLong: EnabledEqualityFor[Long] = EnabledEqualityFor[Long]
  implicit def enabledEqualityForFloat: EnabledEqualityFor[Float] = EnabledEqualityFor[Float]
  implicit def enabledEqualityForDouble: EnabledEqualityFor[Double] = EnabledEqualityFor[Double]
  implicit def enabledEqualityForBigInt: EnabledEqualityFor[BigInt] = EnabledEqualityFor[BigInt]
  implicit def enabledEqualityForBigDecimal: EnabledEqualityFor[BigDecimal] = EnabledEqualityFor[BigDecimal]
  implicit def enabledEqualityForJavaCharacter: EnabledEqualityFor[java.lang.Character] = EnabledEqualityFor[java.lang.Character]
  implicit def enabledEqualityForJavaByte: EnabledEqualityFor[java.lang.Byte] = EnabledEqualityFor[java.lang.Byte]
  implicit def enabledEqualityForJavaShort: EnabledEqualityFor[java.lang.Short] = EnabledEqualityFor[java.lang.Short]
  implicit def enabledEqualityForJavaInteger: EnabledEqualityFor[java.lang.Integer] = EnabledEqualityFor[java.lang.Integer]
  implicit def enabledEqualityForJavaLong: EnabledEqualityFor[java.lang.Long] = EnabledEqualityFor[java.lang.Long]
  implicit def enabledEqualityForJavaFloat: EnabledEqualityFor[java.lang.Float] = EnabledEqualityFor[java.lang.Float]
  implicit def enabledEqualityForJavaDouble: EnabledEqualityFor[java.lang.Double] = EnabledEqualityFor[java.lang.Double]
  implicit def enabledEqualityForBoolean: EnabledEqualityFor[Boolean] = EnabledEqualityFor[Boolean]
  implicit def enabledEqualityForJavaBoolean: EnabledEqualityFor[java.lang.Boolean] = EnabledEqualityFor[java.lang.Boolean]
}

object StrictEnabledEquality extends StrictEnabledEquality

