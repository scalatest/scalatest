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
}