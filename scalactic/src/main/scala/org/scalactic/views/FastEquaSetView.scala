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

trait FastSetView[+T] extends SetView[T] {
  def map[U](f: T => U): FastSetView[U]
  def flatMap[U](f: T => SetView[U]): FastSetView[U]
  def toSet[U >: T](toPath: Collections[U]): toPath.immutable.Set[U]
  def force[U >: T](toPath: Collections[U]): toPath.immutable.Set[U]
  def toSortedSet[U >: T](toPath: SortedCollections[U]): toPath.immutable.SortedSet[U]
  def toStandardList: List[T]
  def size: Int
  /**
   * Builds a new collection by applying a partial function to all elements of this `FastSetView`
   * on which the function is defined.
   *
   * @param pf the partial function which filters and maps the `FastSetView`.
   * @return a new collection of type `That` resulting from applying the partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   *
   * @return a new `FastSetView` resulting from applying the given partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   */
  def collect[U](pf: PartialFunction[T, U]): FastSetView[U]

  def scan[U >: T](z: U)(op: (U, U) ⇒ U): FastSetView[U]

  /**
   * Produces a collection containing cumulative results of applying the
   * operator going left to right.
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `FastSetView` with intermediate results
   */
  def scanLeft[U](z: U)(op: (U, T) => U): FastSetView[U]

  /**
   * Produces a collection containing cumulative results of applying the operator going right to left.
   * The head of the collection is the last cumulative result.
   *
   * Example:
   * {{{
   * `FastSetView`(1, 2, 3, 4).scanRight(0)(_ + _) == `FastSetView`(10, 9, 7, 4, 0)
   * }}}
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `FastSetView` with intermediate results
   */
  def scanRight[U](z: U)(op: (T, U) => U): FastSetView[U]

  /**
   * Converts this `FastSetView` of pairs into two collections of the first and second
   * half of each pair.
   *
   * {{{
   * val xs = `FastSetView`(
   * (1, "one"),
   * (2, "two"),
   * (3, "three")).unzip
   * // xs == (`FastSetView`(1, 2, 3),
   * // `FastSetView`(one, two, three))
   * }}}
   *
   * @tparam U1 the type of the first half of the element pairs
   * @tparam U2 the type of the second half of the element pairs
   * @param asPair an implicit conversion which asserts that the element type
   * of this `FastSetView` is a pair.
   * @return a pair of `FastSetView`s, containing the first, respectively second
   * half of each element pair of this `FastSetView`.
   */
  def unzip[U1, U2](implicit asPair: T => (U1, U2)): (FastSetView[U1], FastSetView[U2])

  /**
   * Converts this `FastSetView` of triples into three collections of the first, second,
   * and third element of each triple.
   *
   * {{{
   * val xs = `FastSetView`(
   * (1, "one", '1'),
   * (2, "two", '2'),
   * (3, "three", '3')).unzip3
   * // xs == (`FastSetView`(1, 2, 3),
   * // `FastSetView`(one, two, three),
   * // `FastSetView`(1, 2, 3))
   * }}}
   *
   * @tparam U1 the type of the first member of the element triples
   * @tparam U2 the type of the second member of the element triples
   * @tparam U3 the type of the third member of the element triples
   * @param asTriple an implicit conversion which asserts that the element type
   * of this `FastSetView` is a triple.
   * @return a triple of `FastSetView`s, containing the first, second, respectively
   * third member of each element triple of this `FastSetView`.
   */
  def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (FastSetView[U1], FastSetView[U2], FastSetView[U3])


  /**
   * Returns a `FastSetView` formed from this `FastSetView` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   * @param that The iterable providing the second half of each result pair
   * @tparam U the type of the second half of the returned pairs
   * @return a `Set` containing pairs consisting of
   * corresponding elements of this `FastSetView` and that`. The length
   * of the returned collection is the minimum of the lengths of this `FastSetView` and `that`.
   *
   */
  def zip[U](that: SetView[U]): FastSetView[(T, U)]

  /**
   * Returns a `FastSetView` formed from this `FastSetView` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is shorter than the other,
   * placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   * @param that the iterable providing the second half of each result pair
   * @param thisElem the element to be used to fill up the result if this `FastSetView` is shorter than `that`.
   * @param thatElem the element to be used to fill up the result if `that` is shorter than this `FastSetView`.
   * @return a new collection of type `That` containing pairs consisting of
   * corresponding elements of this `FastSetView` and `that`. The length
   * of the returned collection is the maximum of the lengths of this `FastSetView` and `that`.
   * If this `FastSetView` is shorter than `that`, `thisElem` values are used to pad the result.
   * If `that` is shorter than this `FastSetView`, `thatElem` values are used to pad the result.
   *
   */
  def zipAll[U, T1 >: T](that: SetView[U], thisElem: T1, thatElem: U): FastSetView[(T1, U)]

  /**
   * Zips this `FastSetView` with its indices.
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

  def zipWithIndex: FastSetView[(T, Int)]
}

object FastSetView {
  private class BasicFastSetView[T](private val args: List[T]) extends FastSetView[T] { thisFastSetView =>
    def collect[U](pf: PartialFunction[T, U]): FastSetView[U] = new CollectFastSetView(thisFastSetView, pf)
    def map[U](f: T => U): FastSetView[U] = new MapFastSetView(thisFastSetView, f)
    def flatMap[U](f: T => SetView[U]): FastSetView[U] = new FlatMapFastSetView(thisFastSetView, f)
    def toSet[U >: T](toPath: Collections[U]): toPath.immutable.FastSet[U] = force(toPath)
    def force[U >: T](toPath: Collections[U]): toPath.immutable.FastSet[U] = toPath.immutable.FastSet[U](args: _*)
    def toSortedSet[U >: T](toPath: SortedCollections[U]): toPath.immutable.SortedSet[U] = ???
    def toStandardList: List[T] = args

    def scan[U >: T](z: U)(op: (U, U) ⇒ U): FastSetView[U] = new ScanFastSetView(thisFastSetView, z, op)
    def scanLeft[U](z: U)(op: (U, T) => U): FastSetView[U] = new ScanLeftFastSetView(thisFastSetView, z, op)
    def scanRight[U](z: U)(op: (T, U) => U): FastSetView[U] = new ScanRightFastSetView(thisFastSetView, z, op)

    def size: Int = args.size

    def unzip[U1, U2](implicit asPair: T => (U1, U2)): (FastSetView[U1], FastSetView[U2]) = (
      new UnzipLeftFastSetView[T, U1, U2](thisFastSetView)(asPair),
      new UnzipRightFastSetView[T, U1, U2](thisFastSetView)(asPair)
    )

    def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (FastSetView[U1], FastSetView[U2], FastSetView[U3]) = (
      new Unzip3LeftFastSetView(thisFastSetView),
      new Unzip3MiddleFastSetView(thisFastSetView),
      new Unzip3RightFastSetView(thisFastSetView)
    )

    def zip[U](thatFastSetView: SetView[U]): FastSetView[(T, U)] = new ZipFastSetView(thisFastSetView, thatFastSetView)
    def zipAll[U, T1 >: T](that: SetView[U], thisElem: T1, thatElem: U): FastSetView[(T1, U)] =
      new ZipAllFastSetView(thisFastSetView, that, thisElem, thatElem)
    def zipWithIndex: FastSetView[(T, Int)] = new ZipWithIndex(thisFastSetView)
    override def toString = args.mkString("FastSetView(", ",", ")")
/*  // Don't uncomment unless have a failing test
    override def equals(other: Any): Boolean =
      other match {
        case otherFastSetView: FastSetView[_] => 
          thisFastSetView.toStandardList.groupBy(o => o) == otherFastSetView.toStandardList.groupBy(o => o)
        case _ => false
      }
    override def hashCode: Int = thisFastSetView.toStandardList.groupBy(o => o).hashCode
*/
  }

  private abstract class TransformFastSetView[T, U] extends FastSetView[U] { thisFastSetView =>
    def collect[V](pf: PartialFunction[U, V]): FastSetView[V] = new CollectFastSetView(thisFastSetView, pf)
    def map[V](g: U => V): FastSetView[V] = new MapFastSetView[U, V](thisFastSetView, g)
    def flatMap[V](f: U => SetView[V]): FastSetView[V] = ???
    def toSet[V >: U](toPath: Collections[V]): toPath.immutable.FastSet[V] = force(toPath)
    def force[V >: U](toPath: Collections[V]): toPath.immutable.FastSet[V] = {
      toPath.immutable.FastSet[V](toStandardList: _*)
    }
    def toSortedSet[V >: U](toPath: SortedCollections[V]): toPath.immutable.SortedSet[V] = ???
    def toStandardList: List[U] // This is the lone abstract method

    def scan[V >: U](z: V)(op: (V, V) ⇒ V): FastSetView[V] = new ScanFastSetView(thisFastSetView, z, op)
    def scanLeft[V](z: V)(op: (V, U) => V): FastSetView[V] = new ScanLeftFastSetView(thisFastSetView, z, op)
    def scanRight[V](z: V)(op: (U, V) => V): FastSetView[V] = new ScanRightFastSetView(thisFastSetView, z, op)

    def size: Int = toStandardList.size

    def unzip[V1, V2](implicit asPair: U => (V1, V2)): (FastSetView[V1], FastSetView[V2]) =
      (new UnzipLeftFastSetView[U, V1, V2](thisFastSetView)(asPair), new UnzipRightFastSetView[U, V1, V2](thisFastSetView)(asPair))

    def unzip3[V1, V2, V3](implicit asTriple: U => (V1, V2, V3)): (FastSetView[V1], FastSetView[V2], FastSetView[V3]) =
      (new Unzip3LeftFastSetView(thisFastSetView), new Unzip3MiddleFastSetView(thisFastSetView), new Unzip3RightFastSetView(thisFastSetView))

    def zip[V](that: SetView[V]): FastSetView[(U, V)] = new ZipFastSetView[U, V](thisFastSetView, that)
    def zipAll[V, U1 >: U](that: SetView[V], thisElem: U1, thatElem: V): FastSetView[(U1, V)] =
      new ZipAllFastSetView(thisFastSetView, that, thisElem, thatElem)
    def zipWithIndex: FastSetView[(U, Int)] = new ZipWithIndex(thisFastSetView)
    override def toString: String = toStandardList.mkString("FastSetView(", ",", ")")
    override def equals(other: Any): Boolean =
      other match {
        case otherFastSetView: FastSetView[_] => 
          thisFastSetView.toStandardList.groupBy(o => o) == otherFastSetView.toStandardList.groupBy(o => o)
        case _ => false
      }
    override def hashCode: Int = thisFastSetView.toStandardList.groupBy(o => o).hashCode
  }

  private class CollectFastSetView[T, U](lazyBag: FastSetView[T], pf: PartialFunction[T, U]) extends TransformFastSetView[T, U] {
    def toStandardList: List[U] = lazyBag.toStandardList.collect(pf)
  }

  private class MapFastSetView[T, U](lazyBag: FastSetView[T], f: T => U) extends TransformFastSetView[T, U] {
    def toStandardList: List[U] = lazyBag.toStandardList.map(f)
  }

  private class FlatMapFastSetView[T, U](lazyBag: FastSetView[T], f: T => SetView[U]) extends TransformFastSetView[T, U] {
    def toStandardList: List[U] = lazyBag.toStandardList.flatMap(f.andThen(_.toStandardList))
  }

  private class ScanFastSetView[T](lazyBag: FastSetView[T], z: T, op: (T, T) ⇒ T) extends TransformFastSetView[T, T] {
    def toStandardList: List[T] = lazyBag.toStandardList.scan(z)(op)
  }

  private class ScanLeftFastSetView[T, U](lazyBag: FastSetView[T], z: U, op: (U, T) ⇒ U) extends TransformFastSetView[T, U] {
    def toStandardList: List[U] = lazyBag.toStandardList.scanLeft(z)(op)
  }

  private class ScanRightFastSetView[T, U](lazyBag: FastSetView[T], z: U, op: (T, U) ⇒ U) extends TransformFastSetView[T, U] {
    def toStandardList: List[U] = lazyBag.toStandardList.scanRight(z)(op)
  }

  private class UnzipLeftFastSetView[T, U1, U2](lazyBag: FastSetView[T])(implicit asPair: T => (U1, U2)) extends TransformFastSetView[T, U1] {
    def toStandardList: List[U1] = lazyBag.toStandardList.unzip._1.toList
  }

  private class UnzipRightFastSetView[T, U1, U2](lazyBag: FastSetView[T])(implicit asPair: T => (U1, U2)) extends TransformFastSetView[T, U2] {
    def toStandardList: List[U2] = lazyBag.toStandardList.unzip._2.toList
  }

  private class Unzip3LeftFastSetView[T, U1, U2, U3](lazyBag: FastSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformFastSetView[T, U1] {
    def toStandardList: List[U1] = lazyBag.toStandardList.unzip3._1.toList
  }

  private class Unzip3MiddleFastSetView[T, U1, U2, U3](lazyBag: FastSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformFastSetView[T, U2] {
    def toStandardList: List[U2] = lazyBag.toStandardList.unzip3._2.toList
  }

  private class Unzip3RightFastSetView[T, U1, U2, U3](lazyBag: FastSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformFastSetView[T, U3] {
    def toStandardList: List[U3] = lazyBag.toStandardList.unzip3._3.toList
  }

  private class ZipFastSetView[T, U](lazyBag: SetView[T], that: SetView[U]) extends TransformFastSetView[T, (T, U)] {
    def toStandardList: List[(T, U)] = lazyBag.toStandardList.zip(that.toStandardList)
  }

  private class ZipAllFastSetView[T, U](thisBag: SetView[T], thatBag: SetView[U], thisElem: T, thatElem: U) extends TransformFastSetView[T, (T, U)] {
    def toStandardList: List[(T, U)] = thisBag.toStandardList.zipAll(thatBag.toStandardList, thisElem, thatElem)
  }

  private class ZipWithIndex[T, U](thisBag: FastSetView[T]) extends TransformFastSetView[T, (T, Int)] {
    def toStandardList: List[(T, Int)] = thisBag.toStandardList.zipWithIndex
  }

  def apply[T](args: T*): FastSetView[T] = new BasicFastSetView(args.toList)
}

