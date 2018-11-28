/*
 * Copyright 2001-2016 Artima, Inc.
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

import org.scalactic.anyvals.PosZInt
import org.scalatest.Resources

trait HavingLength[T] extends HavingSize[T] {
  final def havingLength(len: PosZInt): Generator[T] = havingSize(len)
  final def havingLengthsBetween(from: PosZInt, to: PosZInt): Generator[T] = {
    require(from != to, Resources.fromEqualToToHavingLengthsBetween(from))
    require(from < to, Resources.fromGreaterThanToHavingLengthsBetween(from, to))
    havingSizesBetween(from, to)
  }
  final def havingLengthsDeterminedBy(f: SizeParam => SizeParam): Generator[T] = havingSizesDeterminedBy(f)
}

