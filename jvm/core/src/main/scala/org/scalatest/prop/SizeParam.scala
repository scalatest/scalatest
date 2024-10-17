/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.prop

import org.scalactic.anyvals.{PosInt,PosZInt}
import org.scalactic.Requirements._

/**
  * Describes the "size" to use in order to generate a value.
  *
  * The [[Generator.next()]] function takes a `SizeParam` as a parameter, and uses it if it
  * is relevant to this [[Generator]].
  *
  * The semantics of "size" depend on the [[Generator]] and the type it is producing. For a
  * simple scalar such as an [[Int]] or a [[Float]], "size" is irrelevant, and the parameter
  * is ignored. For a [[String]], the "size" is the length of the desired `String`. For a
  * [[List]], the "size" is the length of the desired `List`. And so on -- in general, the
  * meaning of "size" is usually pretty intuitive.
  *
  * The [[SizeParam]] data structure represents ''both'' a target size range and a specific
  * size.
  *
  * The `minSize` member says the smallest allowed size; the `sizeRange` is added to
  * `minSize` to get the largest allowed size. So if `minSize` is 10 and `sizeRange` is 0,
  * that means that 10 is the only size desired; if `minSize` is 10 and `sizeRange` is 10,
  * then values from 10 to 20 are desired.
  *
  * The `size` member gives the desired size for ''this'' particular invocation of [[Generator.next()]].
  * Most [[Generator]] instances will create a result of that size. However, it is up to each individual
  * [[Generator]] to determine how it interprets [[SizeParam]].
  *
  * You usually do not need to create a [[SizeParam]] directly -- most of the time, you
  * should be able to use the [[HavingSize]] or [[HavingLength]] traits to describe the
  * desired sizes of your [[Generator]]s. You may occasionally need to manipulate [[SizeParam]]
  * directly if you want to use [[HavingSize.havingSizesDeterminedBy()]], or if you want to
  * call [[Generator.next()]] directly.
  *
  * @param minSize the minimum desired size for this [[Generator]] instance or invocation
  * @param sizeRange the range above [[minSize]] to consider allowable
  * @param size the actual size to use for this specific invocation of [[Generator.next()]]
  */
case class SizeParam(minSize: PosZInt, sizeRange: PosZInt, size: PosZInt) {
  require(size >= minSize, s"the passed size ($size.value) must be greater than or equal to the passed minSize ($minSize.value)")
  require(size.value <= minSize + sizeRange, s"the passed size (${size.value}) must be less than or equal to passed minSize plus the passed sizeRange ($minSize + $sizeRange = ${minSize + sizeRange})")

  /**
    * The maximum size to allow, calculated from [[minSize]] and [[sizeRange]].
    */
  val maxSize: PosZInt = PosZInt.ensuringValid(minSize + sizeRange)
}
