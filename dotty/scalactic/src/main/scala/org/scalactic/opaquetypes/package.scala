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
}