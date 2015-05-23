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

trait SetView[+T] {
  def map[U](f: T => U): SetView[U]
  def flatMap[U](f: T => SetView[U]): SetView[U]
  def toSet[U >: T](toPath: Collections[U]): toPath.immutable.Set[U]
  def toSortedSet[U >: T](toPath: SortedCollections[U]): toPath.immutable.SortedSet[U]
  def force[U >: T](toPath: Collections[U]): toPath.immutable.Set[U]
  def toList: List[T]
  def size: Int
  /**
   * Builds a new collection by applying a partial function to all elements of this `SetView`
   * on which the function is defined.
   *
   * @param pf the partial function which filters and maps the `SetView`.
   * @return a new collection of type `That` resulting from applying the partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   *
   * @return a new `SetView` resulting from applying the given partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   */
  def collect[U](pf: PartialFunction[T, U]): SetView[U]

  def scan[U >: T](z: U)(op: (U, U) ⇒ U): SetView[U]

  /**
   * Produces a collection containing cumulative results of applying the
   * operator going left to right.
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `SetView` with intermediate results
   */
  def scanLeft[U](z: U)(op: (U, T) => U): SetView[U]

  /**
   * Produces a collection containing cumulative results of applying the operator going right to left.
   * The head of the collection is the last cumulative result.
   *
   * Example:
   * {{{
   * `SetView`(1, 2, 3, 4).scanRight(0)(_ + _) == `SetView`(10, 9, 7, 4, 0)
   * }}}
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `SetView` with intermediate results
   */
  def scanRight[U](z: U)(op: (T, U) => U): SetView[U]

  /**
   * Converts this `SetView` of pairs into two collections of the first and second
   * half of each pair.
   *
   * {{{
   * val xs = `SetView`(
   * (1, "one"),
   * (2, "two"),
   * (3, "three")).unzip
   * // xs == (`SetView`(1, 2, 3),
   * // `SetView`(one, two, three))
   * }}}
   *
   * @tparam U1 the type of the first half of the element pairs
   * @tparam U2 the type of the second half of the element pairs
   * @param asPair an implicit conversion which asserts that the element type
   * of this `SetView` is a pair.
   * @return a pair of `SetView`s, containing the first, respectively second
   * half of each element pair of this `SetView`.
   */
  def unzip[U1, U2](implicit asPair: T => (U1, U2)): (SetView[U1], SetView[U2])

  /**
   * Converts this `SetView` of triples into three collections of the first, second,
   * and third element of each triple.
   *
   * {{{
   * val xs = `SetView`(
   * (1, "one", '1'),
   * (2, "two", '2'),
   * (3, "three", '3')).unzip3
   * // xs == (`SetView`(1, 2, 3),
   * // `SetView`(one, two, three),
   * // `SetView`(1, 2, 3))
   * }}}
   *
   * @tparam U1 the type of the first member of the element triples
   * @tparam U2 the type of the second member of the element triples
   * @tparam U3 the type of the third member of the element triples
   * @param asTriple an implicit conversion which asserts that the element type
   * of this `SetView` is a triple.
   * @return a triple of `SetView`s, containing the first, second, respectively
   * third member of each element triple of this `SetView`.
   */
  def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (SetView[U1], SetView[U2], SetView[U3])


  /**
   * Returns a `SetView` formed from this `SetView` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   * @param that The iterable providing the second half of each result pair
   * @tparam U the type of the second half of the returned pairs
   * @return a `Set` containing pairs consisting of
   * corresponding elements of this `SetView` and that`. The length
   * of the returned collection is the minimum of the lengths of this `SetView` and `that`.
   *
   */
  def zip[U](that: SetView[U]): SetView[(T, U)]

  /**
   * Returns a `SetView` formed from this `SetView` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is shorter than the other,
   * placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   * @param that the iterable providing the second half of each result pair
   * @param thisElem the element to be used to fill up the result if this `SetView` is shorter than `that`.
   * @param thatElem the element to be used to fill up the result if `that` is shorter than this `SetView`.
   * @return a new collection of type `That` containing pairs consisting of
   * corresponding elements of this `SetView` and `that`. The length
   * of the returned collection is the maximum of the lengths of this `SetView` and `that`.
   * If this `SetView` is shorter than `that`, `thisElem` values are used to pad the result.
   * If `that` is shorter than this `SetView`, `thatElem` values are used to pad the result.
   *
   */
  def zipAll[U, T1 >: T](that: SetView[U], thisElem: T1, thatElem: U): SetView[(T1, U)]

  /**
   * Zips this `SetView` with its indices.
   *
   * @return A `Set` containing pairs consisting of all elements of this
   * `Set` paired with their index. Indices start at `0`.
   *
   * @return A new `Set` containing pairs consisting of all elements of this
   * `Set` paired with their index. Indices start at `0`.
   * @example
   * `List("a", "b", "c").zipWithIndex = List(("a", 0), ("b", 1), ("c", 2))`
   *
   */

  def zipWithIndex: SetView[(T, Int)]
}

