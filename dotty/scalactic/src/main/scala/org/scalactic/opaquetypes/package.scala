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
}