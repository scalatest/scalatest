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
package org.scalactic.views

import org.scalactic.Collections
import org.scalactic.SortedCollections

trait SortedSetView[+T] extends SetView[T] {
  def collect[U](pf: PartialFunction[T, U]): TreeSetView[U]

  def map[U](f: T => U): TreeSetView[U]
  def flatMap[U](f: T => SetView[U]): TreeSetView[U]
  def force[U >: T](toPath: Collections[U]): toPath.immutable.Set[U]
  def toSet[U >: T](toPath: Collections[U]): toPath.immutable.Set[U]
  def force[U >: T](toPath: SortedCollections[U]): toPath.immutable.SortedSet[U]
  def toSortedSet[U >: T](toPath: SortedCollections[U]): toPath.immutable.SortedSet[U]
  def toList: List[T]

  def scan[U >: T](z: U)(op: (U, U) ⇒ U): TreeSetView[U]
  def scanLeft[U](z: U)(op: (U, T) => U): TreeSetView[U]
  def scanRight[U](z: U)(op: (T, U) => U): TreeSetView[U]

  def size: Int
  def unzip[U1, U2](implicit asPair: T => (U1, U2)): (TreeSetView[U1], TreeSetView[U2])
  def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (TreeSetView[U1], TreeSetView[U2], TreeSetView[U3])
  def zip[U](that: SetView[U]): TreeSetView[(T, U)]
  def zipAll[U, T1 >: T](that: SetView[U], thisElem: T1, thatElem: U): TreeSetView[(T1, U)]
  def zipWithIndex: TreeSetView[(T, Int)]
}

