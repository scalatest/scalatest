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

trait LazySortedEquaSet[+T] extends LazyEquaSet[T] {
  def collect[U](pf: PartialFunction[T, U]): LazyTreeEquaSet[U]

  def map[U](f: T => U): LazyTreeEquaSet[U]
  def flatMap[U](f: T => LazyEquaSet[U]): LazyTreeEquaSet[U]
  def toStrict[U >: T](toPath: EquaPath[U]): toPath.EquaSet
  def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.EquaSet
  def toStrict[U >: T](toPath: SortedEquaPath[U]): toPath.SortedEquaSet
  def toSortedEquaSet[U >: T](toPath: SortedEquaPath[U]): toPath.SortedEquaSet
  def toList: List[T]

  def scan[U >: T](z: U)(op: (U, U) ⇒ U): LazyTreeEquaSet[U]
  def scanLeft[U](z: U)(op: (U, T) => U): LazyTreeEquaSet[U]
  def scanRight[U](z: U)(op: (T, U) => U): LazyTreeEquaSet[U]

  def size: Int
  def unzip[U1, U2](implicit asPair: T => (U1, U2)): (LazyTreeEquaSet[U1], LazyTreeEquaSet[U2])
  def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (LazyTreeEquaSet[U1], LazyTreeEquaSet[U2], LazyTreeEquaSet[U3])
  def zip[U](that: LazyEquaSet[U]): LazyTreeEquaSet[(T, U)]
  def zipAll[U, T1 >: T](that: LazyEquaSet[U], thisElem: T1, thatElem: U): LazyTreeEquaSet[(T1, U)]
  def zipWithIndex: LazyTreeEquaSet[(T, Int)]
}

