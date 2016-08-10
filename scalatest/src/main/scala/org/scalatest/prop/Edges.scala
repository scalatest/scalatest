
/*
 * Copyright 2001-2015 Artima, Inc.
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

import org.scalactic.anyvals._

case class Edges(
  byteEdges: List[Byte],
  shortEdges: List[Short],
  charEdges: List[Char],
  intEdges: List[Int],
  longEdges: List[Long],
  floatEdges: List[Float],
  doubleEdges: List[Double],
  posIntEdges: List[PosInt],
  posZIntEdges: List[PosZInt],
  posLongEdges: List[PosLong],
  posZLongEdges: List[PosZLong],
  posFloatEdges: List[PosFloat],
  posZFloatEdges: List[PosZFloat],
  posDoubleEdges: List[PosDouble],
  posZDoubleEdges: List[PosZDouble]
)
