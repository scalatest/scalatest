/*
 * Copyright 2001-2017 Artima, Inc.
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

// 0 + 10 is 10
case class SizeParam(minSize: PosZInt, sizeRange: PosZInt, size: PosZInt) {
  require(size >= minSize, "the passed size ($size) must be greater than or equal to the passed minSize ($minSize)")
  require(size <= minSize + sizeRange, "the passed size ($size) must be less than or equal to passed minSize plus the passed sizeRange ($minSize + $sizeRange = ${minSize + sizeRange})")
}

