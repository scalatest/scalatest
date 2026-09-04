/*
 * Copyright 2001-2026 Artima, Inc.
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

/** Package-level aliases for opaque numeric types and companions.
  *
  * These aliases allow concise usage such as `NegInt(...)` and `NegZInt(...)`
  * within `org.scalactic.opaquetypes`.
  */
package object opaquetypes {
  /** Type alias for [[org.scalactic.opaquetypes.NegInts.NegInt]]. */
  type NegInt = NegInts.NegInt
  /** Value alias for the [[org.scalactic.opaquetypes.NegInts.NegInt]] companion. */
  val NegInt: NegInts.NegInt.type = NegInts.NegInt

  /** Type alias for [[org.scalactic.opaquetypes.NegInts.NegZInt]]. */
  type NegZInt = NegInts.NegZInt
  /** Value alias for the [[org.scalactic.opaquetypes.NegInts.NegZInt]] companion. */
  val NegZInt: NegInts.NegZInt.type = NegInts.NegZInt

  /** Type alias for [[org.scalactic.opaquetypes.NegLongs.NegLong]]. */
  type NegLong = NegLongs.NegLong
  /** Value alias for the [[org.scalactic.opaquetypes.NegLongs.NegLong]] companion. */
  val NegLong: NegLongs.NegLong.type = NegLongs.NegLong
  export NegLongs.NegLong.*

  /** Type alias for [[org.scalactic.opaquetypes.NegLongs.NegZLong]]. */
  type NegZLong = NegLongs.NegZLong
  /** Value alias for the [[org.scalactic.opaquetypes.NegLongs.NegZLong]] companion. */
  val NegZLong: NegLongs.NegZLong.type = NegLongs.NegZLong

  /** Type alias for [[org.scalactic.opaquetypes.NegDoubles.NegZDouble]]. */
  type NegZDouble = NegDoubles.NegZDouble
  /** Value alias for the [[org.scalactic.opaquetypes.NegDoubles.NegZDouble]] companion. */
  val NegZDouble: NegDoubles.NegZDouble.type = NegDoubles.NegZDouble

  /** Type alias for [[org.scalactic.opaquetypes.PosDoubles.PosZDouble]]. */
  type PosZDouble = PosDoubles.PosZDouble
  /** Value alias for the [[org.scalactic.opaquetypes.PosDoubles.PosZDouble]] companion. */
  val PosZDouble: PosDoubles.PosZDouble.type = PosDoubles.PosZDouble

  /** Type alias for [[org.scalactic.opaquetypes.PosDoubles.PosDouble]]. */
  type PosDouble = PosDoubles.PosDouble
  /** Value alias for the [[org.scalactic.opaquetypes.PosDoubles.PosDouble]] companion. */
  val PosDouble: PosDoubles.PosDouble.type = PosDoubles.PosDouble

  /** Type alias for [[org.scalactic.opaquetypes.PosDoubles.PosZFiniteDouble]]. */
  type PosZFiniteDouble = PosDoubles.PosZFiniteDouble
  /** Value alias for the [[org.scalactic.opaquetypes.PosDoubles.PosZFiniteDouble]] companion. */
  val PosZFiniteDouble: PosDoubles.PosZFiniteDouble.type = PosDoubles.PosZFiniteDouble

  /** Type alias for [[org.scalactic.opaquetypes.PosDoubles.PosFiniteDouble]]. */
  type PosFiniteDouble = PosDoubles.PosFiniteDouble
  /** Value alias for the [[org.scalactic.opaquetypes.PosDoubles.PosFiniteDouble]] companion. */
  val PosFiniteDouble: PosDoubles.PosFiniteDouble.type = PosDoubles.PosFiniteDouble

  /** Type alias for [[org.scalactic.opaquetypes.NonZeroDoubles.NonZeroDouble]]. */
  type NonZeroDouble = NonZeroDoubles.NonZeroDouble
  /** Value alias for the [[org.scalactic.opaquetypes.NonZeroDoubles.NonZeroDouble]] companion. */
  val NonZeroDouble: NonZeroDoubles.NonZeroDouble.type = NonZeroDoubles.NonZeroDouble

  /** Type alias for [[org.scalactic.opaquetypes.NonZeroLongs.NonZeroLong]]. */
  type NonZeroLong = NonZeroLongs.NonZeroLong
  /** Value alias for the [[org.scalactic.opaquetypes.NonZeroLongs.NonZeroLong]] companion. */
  val NonZeroLong: NonZeroLongs.NonZeroLong.type = NonZeroLongs.NonZeroLong

  /** Type alias for [[org.scalactic.opaquetypes.NonZeroFloats.NonZeroFloat]]. */
  type NonZeroFloat = NonZeroFloats.NonZeroFloat
  /** Value alias for the [[org.scalactic.opaquetypes.NonZeroFloats.NonZeroFloat]] companion. */
  val NonZeroFloat: NonZeroFloats.NonZeroFloat.type = NonZeroFloats.NonZeroFloat

  /** Type alias for [[org.scalactic.opaquetypes.NonZeroFloats.NonZeroFiniteFloat]]. */
  type NonZeroFiniteFloat = NonZeroFloats.NonZeroFiniteFloat
  /** Value alias for the [[org.scalactic.opaquetypes.NonZeroFloats.NonZeroFiniteFloat]] companion. */
  val NonZeroFiniteFloat: NonZeroFloats.NonZeroFiniteFloat.type = NonZeroFloats.NonZeroFiniteFloat

  /** Type alias for [[org.scalactic.opaquetypes.NonZeroDoubles.NonZeroFiniteDouble]]. */
  type NonZeroFiniteDouble = NonZeroDoubles.NonZeroFiniteDouble
  /** Value alias for the [[org.scalactic.opaquetypes.NonZeroDoubles.NonZeroFiniteDouble]] companion. */
  val NonZeroFiniteDouble: NonZeroDoubles.NonZeroFiniteDouble.type = NonZeroDoubles.NonZeroFiniteDouble

  /** Type alias for [[org.scalactic.opaquetypes.PosInts.PosInt]]. */
  type PosInt = PosInts.PosInt
  /** Value alias for the [[org.scalactic.opaquetypes.PosInts.PosInt]] companion. */
  val PosInt: PosInts.PosInt.type = PosInts.PosInt

  /** Type alias for [[org.scalactic.opaquetypes.PosLongs.PosLong]]. */
  type PosLong = PosLongs.PosLong
  /** Value alias for the [[org.scalactic.opaquetypes.PosLongs.PosLong]] companion. */
  val PosLong: PosLongs.PosLong.type = PosLongs.PosLong

  /** Type alias for [[org.scalactic.opaquetypes.PosInts.PosZInt]]. */
  type PosZInt = PosInts.PosZInt
  /** Value alias for the [[org.scalactic.opaquetypes.PosInts.PosZInt]] companion. */
  val PosZInt: PosInts.PosZInt.type = PosInts.PosZInt

  /** Type alias for [[org.scalactic.opaquetypes.PosLongs.PosZLong]]. */
  type PosZLong = PosLongs.PosZLong
  /** Value alias for the [[org.scalactic.opaquetypes.PosLongs.PosZLong]] companion. */
  val PosZLong: PosLongs.PosZLong.type = PosLongs.PosZLong

  /** Type alias for [[org.scalactic.opaquetypes.NonZeroInts.NonZeroInt]]. */
  type NonZeroInt = NonZeroInts.NonZeroInt
  /** Value alias for the [[org.scalactic.opaquetypes.NonZeroInts.NonZeroInt]] companion. */
  val NonZeroInt: NonZeroInts.NonZeroInt.type = NonZeroInts.NonZeroInt

  /** Type alias for [[org.scalactic.opaquetypes.NegDoubles.NegDouble]]. */
  type NegDouble = NegDoubles.NegDouble
  /** Value alias for the [[org.scalactic.opaquetypes.NegDoubles.NegDouble]] companion. */
  val NegDouble: NegDoubles.NegDouble.type = NegDoubles.NegDouble

  /** Type alias for [[org.scalactic.opaquetypes.NegDoubles.NegZFiniteDouble]]. */
  type NegZFiniteDouble = NegDoubles.NegZFiniteDouble
  /** Value alias for the [[org.scalactic.opaquetypes.NegDoubles.NegZFiniteDouble]] companion. */
  val NegZFiniteDouble: NegDoubles.NegZFiniteDouble.type = NegDoubles.NegZFiniteDouble

  /** Type alias for [[org.scalactic.opaquetypes.NegDoubles.NegFiniteDouble]]. */
  type NegFiniteDouble = NegDoubles.NegFiniteDouble
  /** Value alias for the [[org.scalactic.opaquetypes.NegDoubles.NegFiniteDouble]] companion. */
  val NegFiniteDouble: NegDoubles.NegFiniteDouble.type = NegDoubles.NegFiniteDouble

  /** Type alias for [[org.scalactic.opaquetypes.PosFloats.PosZFloat]]. */
  type PosZFloat = PosFloats.PosZFloat
  /** Value alias for the [[org.scalactic.opaquetypes.PosFloats.PosZFloat]] companion. */
  val PosZFloat: PosFloats.PosZFloat.type = PosFloats.PosZFloat

  /** Type alias for [[org.scalactic.opaquetypes.PosFloats.PosFloat]]. */
  type PosFloat = PosFloats.PosFloat
  /** Value alias for the [[org.scalactic.opaquetypes.PosFloats.PosFloat]] companion. */
  val PosFloat: PosFloats.PosFloat.type = PosFloats.PosFloat

  /** Type alias for [[org.scalactic.opaquetypes.PosFloats.PosZFiniteFloat]]. */
  type PosZFiniteFloat = PosFloats.PosZFiniteFloat
  /** Value alias for the [[org.scalactic.opaquetypes.PosFloats.PosZFiniteFloat]] companion. */
  val PosZFiniteFloat: PosFloats.PosZFiniteFloat.type = PosFloats.PosZFiniteFloat

  /** Type alias for [[org.scalactic.opaquetypes.PosFloats.PosFiniteFloat]]. */
  type PosFiniteFloat = PosFloats.PosFiniteFloat
  /** Value alias for the [[org.scalactic.opaquetypes.PosFloats.PosFiniteFloat]] companion. */
  val PosFiniteFloat: PosFloats.PosFiniteFloat.type = PosFloats.PosFiniteFloat

  /** Type alias for [[org.scalactic.opaquetypes.Finites.FiniteFloat]]. */
  type FiniteFloat = Finites.FiniteFloat
  /** Value alias for the [[org.scalactic.opaquetypes.Finites.FiniteFloat]] companion. */
  val FiniteFloat: Finites.FiniteFloat.type = Finites.FiniteFloat

  /** Type alias for [[org.scalactic.opaquetypes.Finites.FiniteDouble]]. */
  type FiniteDouble = Finites.FiniteDouble
  /** Value alias for the [[org.scalactic.opaquetypes.Finites.FiniteDouble]] companion. */
  val FiniteDouble: Finites.FiniteDouble.type = Finites.FiniteDouble

  /** Type alias for [[org.scalactic.opaquetypes.NegFloats.NegZFloat]]. */
  type NegZFloat = NegFloats.NegZFloat
  /** Value alias for the [[org.scalactic.opaquetypes.NegFloats.NegZFloat]] companion. */
  val NegZFloat: NegFloats.NegZFloat.type = NegFloats.NegZFloat

  /** Type alias for [[org.scalactic.opaquetypes.NegFloats.NegFloat]]. */
  type NegFloat = NegFloats.NegFloat
  /** Value alias for the [[org.scalactic.opaquetypes.NegFloats.NegFloat]] companion. */
  val NegFloat: NegFloats.NegFloat.type = NegFloats.NegFloat

  /** Type alias for [[org.scalactic.opaquetypes.NegFloats.NegZFiniteFloat]]. */
  type NegZFiniteFloat = NegFloats.NegZFiniteFloat
  /** Value alias for the [[org.scalactic.opaquetypes.NegFloats.NegZFiniteFloat]] companion. */
  val NegZFiniteFloat: NegFloats.NegZFiniteFloat.type = NegFloats.NegZFiniteFloat

  /** Type alias for [[org.scalactic.opaquetypes.NegFloats.NegFiniteFloat]]. */
  type NegFiniteFloat = NegFloats.NegFiniteFloat
  /** Value alias for the [[org.scalactic.opaquetypes.NegFloats.NegFiniteFloat]] companion. */
  val NegFiniteFloat: NegFloats.NegFiniteFloat.type = NegFloats.NegFiniteFloat

  /** Type alias for [[org.scalactic.opaquetypes.Numerics.NumericChar]]. */
  type NumericChar = Numerics.NumericChar
  /** Value alias for the [[org.scalactic.opaquetypes.Numerics.NumericChar]] companion. */
  val NumericChar: Numerics.NumericChar.type = Numerics.NumericChar

  // NonEmpty types

  /** Type alias for [[org.scalactic.opaquetypes.NonEmpties.NonEmptyArray]]. */
  type NonEmptyArray[T] = NonEmpties.NonEmptyArray[T]
  /** Value alias for the [[org.scalactic.opaquetypes.NonEmpties.NonEmptyArray]] companion. */
  val NonEmptyArray: NonEmpties.NonEmptyArray.type = NonEmpties.NonEmptyArray

  /** Type alias for [[org.scalactic.opaquetypes.NonEmpties.NonEmptyList]]. */
  type NonEmptyList[T] = NonEmpties.NonEmptyList[T]
  /** Value alias for the [[org.scalactic.opaquetypes.NonEmpties.NonEmptyList]] companion. */
  val NonEmptyList: NonEmpties.NonEmptyList.type = NonEmpties.NonEmptyList

  /** Type alias for [[org.scalactic.opaquetypes.NonEmpties.NonEmptyVector]]. */
  type NonEmptyVector[T] = NonEmpties.NonEmptyVector[T]
  /** Value alias for the [[org.scalactic.opaquetypes.NonEmpties.NonEmptyVector]] companion. */
  val NonEmptyVector: NonEmpties.NonEmptyVector.type = NonEmpties.NonEmptyVector

  /** Type alias for [[org.scalactic.opaquetypes.NonEmpties.NonEmptySet]]. */
  type NonEmptySet[T] = NonEmpties.NonEmptySet[T]
  /** Value alias for the [[org.scalactic.opaquetypes.NonEmpties.NonEmptySet]] companion. */
  val NonEmptySet: NonEmpties.NonEmptySet.type = NonEmpties.NonEmptySet

  /** Type alias for [[org.scalactic.opaquetypes.NonEmpties.NonEmptyMap]]. */
  type NonEmptyMap[K, +V] = NonEmpties.NonEmptyMap[K, V]
  /** Value alias for the [[org.scalactic.opaquetypes.NonEmpties.NonEmptyMap]] companion. */
  val NonEmptyMap: NonEmpties.NonEmptyMap.type = NonEmpties.NonEmptyMap

  /** Type alias for [[org.scalactic.opaquetypes.NonEmpties.NonEmptyString]]. */
  type NonEmptyString = NonEmpties.NonEmptyString
  /** Value alias for the [[org.scalactic.opaquetypes.NonEmpties.NonEmptyString]] companion. */
  val NonEmptyString: NonEmpties.NonEmptyString.type = NonEmpties.NonEmptyString
}