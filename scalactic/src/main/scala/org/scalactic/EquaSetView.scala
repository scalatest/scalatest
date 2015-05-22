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

trait EquaSetView[+T] {
  def map[U](f: T => U): EquaSetView[U]
  def flatMap[U](f: T => EquaSetView[U]): EquaSetView[U]
  def toEquaSet[U >: T](toPath: Collections[U]): toPath.immutable.EquaSet[U]
  def toSortedEquaSet[U >: T](toPath: SortedCollections[U]): toPath.immutable.SortedEquaSet[U]
  def force[U >: T](toPath: Collections[U]): toPath.immutable.EquaSet[U]
  def toList: List[T]
  def size: Int
  /**
   * Builds a new collection by applying a partial function to all elements of this `EquaSetView`
   * on which the function is defined.
   *
   * @param pf the partial function which filters and maps the `EquaSetView`.
   * @return a new collection of type `That` resulting from applying the partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   *
   * @return a new `EquaSetView` resulting from applying the given partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   */
  def collect[U](pf: PartialFunction[T, U]): EquaSetView[U]

  def scan[U >: T](z: U)(op: (U, U) ⇒ U): EquaSetView[U]

  /**
   * Produces a collection containing cumulative results of applying the
   * operator going left to right.
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `EquaSetView` with intermediate results
   */
  def scanLeft[U](z: U)(op: (U, T) => U): EquaSetView[U]

  /**
   * Produces a collection containing cumulative results of applying the operator going right to left.
   * The head of the collection is the last cumulative result.
   *
   * Example:
   * {{{
   * `EquaSetView`(1, 2, 3, 4).scanRight(0)(_ + _) == `EquaSetView`(10, 9, 7, 4, 0)
   * }}}
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `EquaSetView` with intermediate results
   */
  def scanRight[U](z: U)(op: (T, U) => U): EquaSetView[U]

  /**
   * Converts this `EquaSetView` of pairs into two collections of the first and second
   * half of each pair.
   *
   * {{{
   * val xs = `EquaSetView`(
   * (1, "one"),
   * (2, "two"),
   * (3, "three")).unzip
   * // xs == (`EquaSetView`(1, 2, 3),
   * // `EquaSetView`(one, two, three))
   * }}}
   *
   * @tparam U1 the type of the first half of the element pairs
   * @tparam U2 the type of the second half of the element pairs
   * @param asPair an implicit conversion which asserts that the element type
   * of this `EquaSetView` is a pair.
   * @return a pair of `EquaSetView`s, containing the first, respectively second
   * half of each element pair of this `EquaSetView`.
   */
  def unzip[U1, U2](implicit asPair: T => (U1, U2)): (EquaSetView[U1], EquaSetView[U2])

  /**
   * Converts this `EquaSetView` of triples into three collections of the first, second,
   * and third element of each triple.
   *
   * {{{
   * val xs = `EquaSetView`(
   * (1, "one", '1'),
   * (2, "two", '2'),
   * (3, "three", '3')).unzip3
   * // xs == (`EquaSetView`(1, 2, 3),
   * // `EquaSetView`(one, two, three),
   * // `EquaSetView`(1, 2, 3))
   * }}}
   *
   * @tparam U1 the type of the first member of the element triples
   * @tparam U2 the type of the second member of the element triples
   * @tparam U3 the type of the third member of the element triples
   * @param asTriple an implicit conversion which asserts that the element type
   * of this `EquaSetView` is a triple.
   * @return a triple of `EquaSetView`s, containing the first, second, respectively
   * third member of each element triple of this `EquaSetView`.
   */
  def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (EquaSetView[U1], EquaSetView[U2], EquaSetView[U3])


  /**
   * Returns a `EquaSetView` formed from this `EquaSetView` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   * @param that The iterable providing the second half of each result pair
   * @tparam U the type of the second half of the returned pairs
   * @return a `Set` containing pairs consisting of
   * corresponding elements of this `EquaSetView` and that`. The length
   * of the returned collection is the minimum of the lengths of this `EquaSetView` and `that`.
   *
   */
  def zip[U](that: EquaSetView[U]): EquaSetView[(T, U)]

  /**
   * Returns a `EquaSetView` formed from this `EquaSetView` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is shorter than the other,
   * placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   * @param that the iterable providing the second half of each result pair
   * @param thisElem the element to be used to fill up the result if this `EquaSetView` is shorter than `that`.
   * @param thatElem the element to be used to fill up the result if `that` is shorter than this `EquaSetView`.
   * @return a new collection of type `That` containing pairs consisting of
   * corresponding elements of this `EquaSetView` and `that`. The length
   * of the returned collection is the maximum of the lengths of this `EquaSetView` and `that`.
   * If this `EquaSetView` is shorter than `that`, `thisElem` values are used to pad the result.
   * If `that` is shorter than this `EquaSetView`, `thatElem` values are used to pad the result.
   *
   */
  def zipAll[U, T1 >: T](that: EquaSetView[U], thisElem: T1, thatElem: U): EquaSetView[(T1, U)]

  /**
   * Zips this `EquaSetView` with its indices.
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

  def zipWithIndex: EquaSetView[(T, Int)]
}

