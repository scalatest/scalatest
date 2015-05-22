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
package org.scalactic

trait SortedEquaSetView[+T] extends EquaSetView[T] {
  def collect[U](pf: PartialFunction[T, U]): TreeEquaSetView[U]

  def map[U](f: T => U): TreeEquaSetView[U]
  def flatMap[U](f: T => EquaSetView[U]): TreeEquaSetView[U]
  def force[U >: T](toPath: Collections[U]): toPath.immutable.EquaSet[U]
  def toEquaSet[U >: T](toPath: Collections[U]): toPath.immutable.EquaSet[U]
  def force[U >: T](toPath: SortedCollections[U]): toPath.immutable.SortedEquaSet[U]
  def toSortedEquaSet[U >: T](toPath: SortedCollections[U]): toPath.immutable.SortedEquaSet[U]
  def toList: List[T]

  def scan[U >: T](z: U)(op: (U, U) ⇒ U): TreeEquaSetView[U]
  def scanLeft[U](z: U)(op: (U, T) => U): TreeEquaSetView[U]
  def scanRight[U](z: U)(op: (T, U) => U): TreeEquaSetView[U]

  def size: Int
  def unzip[U1, U2](implicit asPair: T => (U1, U2)): (TreeEquaSetView[U1], TreeEquaSetView[U2])
  def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (TreeEquaSetView[U1], TreeEquaSetView[U2], TreeEquaSetView[U3])
  def zip[U](that: EquaSetView[U]): TreeEquaSetView[(T, U)]
  def zipAll[U, T1 >: T](that: EquaSetView[U], thisElem: T1, thatElem: U): TreeEquaSetView[(T1, U)]
  def zipWithIndex: TreeEquaSetView[(T, Int)]
}

