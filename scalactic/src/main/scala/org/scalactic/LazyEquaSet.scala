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

trait LazyEquaSet[+T] {
  def map[U](f: T => U): LazyEquaSet[U]
  def flatMap[U](f: T => LazyEquaSet[U]): LazyEquaSet[U]
  def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.EquaSet
  def toSortedEquaSet[U >: T](toPath: SortedEquaPath[U]): toPath.SortedEquaSet
  def toStrict[U >: T](toPath: EquaPath[U]): toPath.EquaSet
  def toList: List[T]
  def size: Int
  /**
   * Builds a new collection by applying a partial function to all elements of this `LazyEquaSet`
   * on which the function is defined.
   *
   * @param pf the partial function which filters and maps the `LazyEquaSet`.
   * @return a new collection of type `That` resulting from applying the partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   *
   * @return a new `LazyEquaSet` resulting from applying the given partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   */
  def collect[U](pf: PartialFunction[T, U]): LazyEquaSet[U]

  def scan[U >: T](z: U)(op: (U, U) ⇒ U): LazyEquaSet[U]

  /**
   * Produces a collection containing cumulative results of applying the
   * operator going left to right.
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `LazyEquaSet` with intermediate results
   */
  def scanLeft[U](z: U)(op: (U, T) => U): LazyEquaSet[U]

  /**
   * Produces a collection containing cumulative results of applying the operator going right to left.
   * The head of the collection is the last cumulative result.
   *
   * Example:
   * {{{
   * `LazyEquaSet`(1, 2, 3, 4).scanRight(0)(_ + _) == `LazyEquaSet`(10, 9, 7, 4, 0)
   * }}}
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `LazyEquaSet` with intermediate results
   */
  def scanRight[U](z: U)(op: (T, U) => U): LazyEquaSet[U]

  /**
   * Converts this `LazyEquaSet` of pairs into two collections of the first and second
   * half of each pair.
   *
   * {{{
   * val xs = `LazyEquaSet`(
   * (1, "one"),
   * (2, "two"),
   * (3, "three")).unzip
   * // xs == (`LazyEquaSet`(1, 2, 3),
   * // `LazyEquaSet`(one, two, three))
   * }}}
   *
   * @tparam U1 the type of the first half of the element pairs
   * @tparam U2 the type of the second half of the element pairs
   * @param asPair an implicit conversion which asserts that the element type
   * of this `LazyEquaSet` is a pair.
   * @return a pair of `LazyEquaSet`s, containing the first, respectively second
   * half of each element pair of this `LazyEquaSet`.
   */
  def unzip[U1, U2](implicit asPair: T => (U1, U2)): (LazyEquaSet[U1], LazyEquaSet[U2])

  /**
   * Converts this `LazyEquaSet` of triples into three collections of the first, second,
   * and third element of each triple.
   *
   * {{{
   * val xs = `LazyEquaSet`(
   * (1, "one", '1'),
   * (2, "two", '2'),
   * (3, "three", '3')).unzip3
   * // xs == (`LazyEquaSet`(1, 2, 3),
   * // `LazyEquaSet`(one, two, three),
   * // `LazyEquaSet`(1, 2, 3))
   * }}}
   *
   * @tparam U1 the type of the first member of the element triples
   * @tparam U2 the type of the second member of the element triples
   * @tparam U3 the type of the third member of the element triples
   * @param asTriple an implicit conversion which asserts that the element type
   * of this `LazyEquaSet` is a triple.
   * @return a triple of `LazyEquaSet`s, containing the first, second, respectively
   * third member of each element triple of this `LazyEquaSet`.
   */
  def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (LazyEquaSet[U1], LazyEquaSet[U2], LazyEquaSet[U3])


  /**
   * Returns a `LazyEquaSet` formed from this `LazyEquaSet` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   * @param that The iterable providing the second half of each result pair
   * @tparam U the type of the second half of the returned pairs
   * @return a `Set` containing pairs consisting of
   * corresponding elements of this `LazyEquaSet` and that`. The length
   * of the returned collection is the minimum of the lengths of this `LazyEquaSet` and `that`.
   *
   */
  def zip[U](that: LazyEquaSet[U]): LazyEquaSet[(T, U)]

  /**
   * Returns a `LazyEquaSet` formed from this `LazyEquaSet` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is shorter than the other,
   * placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   * @param that the iterable providing the second half of each result pair
   * @param thisElem the element to be used to fill up the result if this `LazyEquaSet` is shorter than `that`.
   * @param thatElem the element to be used to fill up the result if `that` is shorter than this `LazyEquaSet`.
   * @return a new collection of type `That` containing pairs consisting of
   * corresponding elements of this `LazyEquaSet` and `that`. The length
   * of the returned collection is the maximum of the lengths of this `LazyEquaSet` and `that`.
   * If this `LazyEquaSet` is shorter than `that`, `thisElem` values are used to pad the result.
   * If `that` is shorter than this `LazyEquaSet`, `thatElem` values are used to pad the result.
   *
   */
  def zipAll[U, T1 >: T](that: LazyEquaSet[U], thisElem: T1, thatElem: U): LazyEquaSet[(T1, U)]

  /**
   * Zips this `LazyEquaSet` with its indices.
   *
   * @return A `Set` containing pairs consisting of all elements of this
   * `EquaSet` paired with their index. Indices start at `0`.
   *
   * @return A new `EquaSet` containing pairs consisting of all elements of this
   * `EquaSet` paired with their index. Indices start at `0`.
   * @example
   * `List("a", "b", "c").zipWithIndex = List(("a", 0), ("b", 1), ("c", 2))`
   *
   */

  def zipWithIndex: LazyEquaSet[(T, Int)]
}

