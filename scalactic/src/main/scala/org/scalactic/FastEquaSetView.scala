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

trait FastEquaSetView[+T] extends EquaSetView[T] {
  def map[U](f: T => U): FastEquaSetView[U]
  def flatMap[U](f: T => EquaSetView[U]): FastEquaSetView[U]
  def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.immutable.EquaSet[U]
  def force[U >: T](toPath: EquaPath[U]): toPath.immutable.EquaSet[U]
  def toSortedEquaSet[U >: T](toPath: SortedEquaPath[U]): toPath.immutable.SortedEquaSet[U]
  def toList: List[T]
  def size: Int
  /**
   * Builds a new collection by applying a partial function to all elements of this `FastEquaSetView`
   * on which the function is defined.
   *
   * @param pf the partial function which filters and maps the `FastEquaSetView`.
   * @return a new collection of type `That` resulting from applying the partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   *
   * @return a new `FastEquaSetView` resulting from applying the given partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   */
  def collect[U](pf: PartialFunction[T, U]): FastEquaSetView[U]

  def scan[U >: T](z: U)(op: (U, U) ⇒ U): FastEquaSetView[U]

  /**
   * Produces a collection containing cumulative results of applying the
   * operator going left to right.
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `FastEquaSetView` with intermediate results
   */
  def scanLeft[U](z: U)(op: (U, T) => U): FastEquaSetView[U]

  /**
   * Produces a collection containing cumulative results of applying the operator going right to left.
   * The head of the collection is the last cumulative result.
   *
   * Example:
   * {{{
   * `FastEquaSetView`(1, 2, 3, 4).scanRight(0)(_ + _) == `FastEquaSetView`(10, 9, 7, 4, 0)
   * }}}
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `FastEquaSetView` with intermediate results
   */
  def scanRight[U](z: U)(op: (T, U) => U): FastEquaSetView[U]

  /**
   * Converts this `FastEquaSetView` of pairs into two collections of the first and second
   * half of each pair.
   *
   * {{{
   * val xs = `FastEquaSetView`(
   * (1, "one"),
   * (2, "two"),
   * (3, "three")).unzip
   * // xs == (`FastEquaSetView`(1, 2, 3),
   * // `FastEquaSetView`(one, two, three))
   * }}}
   *
   * @tparam U1 the type of the first half of the element pairs
   * @tparam U2 the type of the second half of the element pairs
   * @param asPair an implicit conversion which asserts that the element type
   * of this `FastEquaSetView` is a pair.
   * @return a pair of `FastEquaSetView`s, containing the first, respectively second
   * half of each element pair of this `FastEquaSetView`.
   */
  def unzip[U1, U2](implicit asPair: T => (U1, U2)): (FastEquaSetView[U1], FastEquaSetView[U2])

  /**
   * Converts this `FastEquaSetView` of triples into three collections of the first, second,
   * and third element of each triple.
   *
   * {{{
   * val xs = `FastEquaSetView`(
   * (1, "one", '1'),
   * (2, "two", '2'),
   * (3, "three", '3')).unzip3
   * // xs == (`FastEquaSetView`(1, 2, 3),
   * // `FastEquaSetView`(one, two, three),
   * // `FastEquaSetView`(1, 2, 3))
   * }}}
   *
   * @tparam U1 the type of the first member of the element triples
   * @tparam U2 the type of the second member of the element triples
   * @tparam U3 the type of the third member of the element triples
   * @param asTriple an implicit conversion which asserts that the element type
   * of this `FastEquaSetView` is a triple.
   * @return a triple of `FastEquaSetView`s, containing the first, second, respectively
   * third member of each element triple of this `FastEquaSetView`.
   */
  def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (FastEquaSetView[U1], FastEquaSetView[U2], FastEquaSetView[U3])


  /**
   * Returns a `FastEquaSetView` formed from this `FastEquaSetView` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   * @param that The iterable providing the second half of each result pair
   * @tparam U the type of the second half of the returned pairs
   * @return a `Set` containing pairs consisting of
   * corresponding elements of this `FastEquaSetView` and that`. The length
   * of the returned collection is the minimum of the lengths of this `FastEquaSetView` and `that`.
   *
   */
  def zip[U](that: EquaSetView[U]): FastEquaSetView[(T, U)]

  /**
   * Returns a `FastEquaSetView` formed from this `FastEquaSetView` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is shorter than the other,
   * placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   * @param that the iterable providing the second half of each result pair
   * @param thisElem the element to be used to fill up the result if this `FastEquaSetView` is shorter than `that`.
   * @param thatElem the element to be used to fill up the result if `that` is shorter than this `FastEquaSetView`.
   * @return a new collection of type `That` containing pairs consisting of
   * corresponding elements of this `FastEquaSetView` and `that`. The length
   * of the returned collection is the maximum of the lengths of this `FastEquaSetView` and `that`.
   * If this `FastEquaSetView` is shorter than `that`, `thisElem` values are used to pad the result.
   * If `that` is shorter than this `FastEquaSetView`, `thatElem` values are used to pad the result.
   *
   */
  def zipAll[U, T1 >: T](that: EquaSetView[U], thisElem: T1, thatElem: U): FastEquaSetView[(T1, U)]

  /**
   * Zips this `FastEquaSetView` with its indices.
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

  def zipWithIndex: FastEquaSetView[(T, Int)]
}

object FastEquaSetView {
  private class BasicFastEquaSetView[T](private val args: List[T]) extends FastEquaSetView[T] { thisFastEquaSetView =>
    def collect[U](pf: PartialFunction[T, U]): FastEquaSetView[U] = new CollectFastEquaSetView(thisFastEquaSetView, pf)
    def map[U](f: T => U): FastEquaSetView[U] = new MapFastEquaSetView(thisFastEquaSetView, f)
    def flatMap[U](f: T => EquaSetView[U]): FastEquaSetView[U] = new FlatMapFastEquaSetView(thisFastEquaSetView, f)
    def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.immutable.FastEquaSet[U] = force(toPath)
    def force[U >: T](toPath: EquaPath[U]): toPath.immutable.FastEquaSet[U] = toPath.immutable.FastEquaSet[U](args: _*)
    def toSortedEquaSet[U >: T](toPath: SortedEquaPath[U]): toPath.immutable.SortedEquaSet[U] = ???
    def toList: List[T] = args

    def scan[U >: T](z: U)(op: (U, U) ⇒ U): FastEquaSetView[U] = new ScanFastEquaSetView(thisFastEquaSetView, z, op)
    def scanLeft[U](z: U)(op: (U, T) => U): FastEquaSetView[U] = new ScanLeftFastEquaSetView(thisFastEquaSetView, z, op)
    def scanRight[U](z: U)(op: (T, U) => U): FastEquaSetView[U] = new ScanRightFastEquaSetView(thisFastEquaSetView, z, op)

    def size: Int = args.size

    def unzip[U1, U2](implicit asPair: T => (U1, U2)): (FastEquaSetView[U1], FastEquaSetView[U2]) = (
      new UnzipLeftFastEquaSetView[T, U1, U2](thisFastEquaSetView)(asPair),
      new UnzipRightFastEquaSetView[T, U1, U2](thisFastEquaSetView)(asPair)
    )

    def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (FastEquaSetView[U1], FastEquaSetView[U2], FastEquaSetView[U3]) = (
      new Unzip3LeftFastEquaSetView(thisFastEquaSetView),
      new Unzip3MiddleFastEquaSetView(thisFastEquaSetView),
      new Unzip3RightFastEquaSetView(thisFastEquaSetView)
    )

    def zip[U](thatFastEquaSetView: EquaSetView[U]): FastEquaSetView[(T, U)] = new ZipFastEquaSetView(thisFastEquaSetView, thatFastEquaSetView)
    def zipAll[U, T1 >: T](that: EquaSetView[U], thisElem: T1, thatElem: U): FastEquaSetView[(T1, U)] =
      new ZipAllFastEquaSetView(thisFastEquaSetView, that, thisElem, thatElem)
    def zipWithIndex: FastEquaSetView[(T, Int)] = new ZipWithIndex(thisFastEquaSetView)
    override def toString = args.mkString("FastEquaSetView(", ",", ")")
/*  // Don't uncomment unless have a failing test
    override def equals(other: Any): Boolean =
      other match {
        case otherFastEquaSetView: FastEquaSetView[_] => 
          thisFastEquaSetView.toList.groupBy(o => o) == otherFastEquaSetView.toList.groupBy(o => o)
        case _ => false
      }
    override def hashCode: Int = thisFastEquaSetView.toList.groupBy(o => o).hashCode
*/
  }

  private abstract class TransformFastEquaSetView[T, U] extends FastEquaSetView[U] { thisFastEquaSetView =>
    def collect[V](pf: PartialFunction[U, V]): FastEquaSetView[V] = new CollectFastEquaSetView(thisFastEquaSetView, pf)
    def map[V](g: U => V): FastEquaSetView[V] = new MapFastEquaSetView[U, V](thisFastEquaSetView, g)
    def flatMap[V](f: U => EquaSetView[V]): FastEquaSetView[V] = ???
    def toEquaSet[V >: U](toPath: EquaPath[V]): toPath.immutable.FastEquaSet[V] = force(toPath)
    def force[V >: U](toPath: EquaPath[V]): toPath.immutable.FastEquaSet[V] = {
      toPath.immutable.FastEquaSet[V](toList: _*)
    }
    def toSortedEquaSet[V >: U](toPath: SortedEquaPath[V]): toPath.immutable.SortedEquaSet[V] = ???
    def toList: List[U] // This is the lone abstract method

    def scan[V >: U](z: V)(op: (V, V) ⇒ V): FastEquaSetView[V] = new ScanFastEquaSetView(thisFastEquaSetView, z, op)
    def scanLeft[V](z: V)(op: (V, U) => V): FastEquaSetView[V] = new ScanLeftFastEquaSetView(thisFastEquaSetView, z, op)
    def scanRight[V](z: V)(op: (U, V) => V): FastEquaSetView[V] = new ScanRightFastEquaSetView(thisFastEquaSetView, z, op)

    def size: Int = toList.size

    def unzip[V1, V2](implicit asPair: U => (V1, V2)): (FastEquaSetView[V1], FastEquaSetView[V2]) =
      (new UnzipLeftFastEquaSetView[U, V1, V2](thisFastEquaSetView)(asPair), new UnzipRightFastEquaSetView[U, V1, V2](thisFastEquaSetView)(asPair))

    def unzip3[V1, V2, V3](implicit asTriple: U => (V1, V2, V3)): (FastEquaSetView[V1], FastEquaSetView[V2], FastEquaSetView[V3]) =
      (new Unzip3LeftFastEquaSetView(thisFastEquaSetView), new Unzip3MiddleFastEquaSetView(thisFastEquaSetView), new Unzip3RightFastEquaSetView(thisFastEquaSetView))

    def zip[V](that: EquaSetView[V]): FastEquaSetView[(U, V)] = new ZipFastEquaSetView[U, V](thisFastEquaSetView, that)
    def zipAll[V, U1 >: U](that: EquaSetView[V], thisElem: U1, thatElem: V): FastEquaSetView[(U1, V)] =
      new ZipAllFastEquaSetView(thisFastEquaSetView, that, thisElem, thatElem)
    def zipWithIndex: FastEquaSetView[(U, Int)] = new ZipWithIndex(thisFastEquaSetView)
    override def toString: String = toList.mkString("FastEquaSetView(", ",", ")")
    override def equals(other: Any): Boolean =
      other match {
        case otherFastEquaSetView: FastEquaSetView[_] => 
          thisFastEquaSetView.toList.groupBy(o => o) == otherFastEquaSetView.toList.groupBy(o => o)
        case _ => false
      }
    override def hashCode: Int = thisFastEquaSetView.toList.groupBy(o => o).hashCode
  }

  private class CollectFastEquaSetView[T, U](lazyBag: FastEquaSetView[T], pf: PartialFunction[T, U]) extends TransformFastEquaSetView[T, U] {
    def toList: List[U] = lazyBag.toList.collect(pf)
  }

  private class MapFastEquaSetView[T, U](lazyBag: FastEquaSetView[T], f: T => U) extends TransformFastEquaSetView[T, U] {
    def toList: List[U] = lazyBag.toList.map(f)
  }

  private class FlatMapFastEquaSetView[T, U](lazyBag: FastEquaSetView[T], f: T => EquaSetView[U]) extends TransformFastEquaSetView[T, U] {
    def toList: List[U] = lazyBag.toList.flatMap(f.andThen(_.toList))
  }

  private class ScanFastEquaSetView[T](lazyBag: FastEquaSetView[T], z: T, op: (T, T) ⇒ T) extends TransformFastEquaSetView[T, T] {
    def toList: List[T] = lazyBag.toList.scan(z)(op)
  }

  private class ScanLeftFastEquaSetView[T, U](lazyBag: FastEquaSetView[T], z: U, op: (U, T) ⇒ U) extends TransformFastEquaSetView[T, U] {
    def toList: List[U] = lazyBag.toList.scanLeft(z)(op)
  }

  private class ScanRightFastEquaSetView[T, U](lazyBag: FastEquaSetView[T], z: U, op: (T, U) ⇒ U) extends TransformFastEquaSetView[T, U] {
    def toList: List[U] = lazyBag.toList.scanRight(z)(op)
  }

  private class UnzipLeftFastEquaSetView[T, U1, U2](lazyBag: FastEquaSetView[T])(implicit asPair: T => (U1, U2)) extends TransformFastEquaSetView[T, U1] {
    def toList: List[U1] = lazyBag.toList.unzip._1.toList
  }

  private class UnzipRightFastEquaSetView[T, U1, U2](lazyBag: FastEquaSetView[T])(implicit asPair: T => (U1, U2)) extends TransformFastEquaSetView[T, U2] {
    def toList: List[U2] = lazyBag.toList.unzip._2.toList
  }

  private class Unzip3LeftFastEquaSetView[T, U1, U2, U3](lazyBag: FastEquaSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformFastEquaSetView[T, U1] {
    def toList: List[U1] = lazyBag.toList.unzip3._1.toList
  }

  private class Unzip3MiddleFastEquaSetView[T, U1, U2, U3](lazyBag: FastEquaSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformFastEquaSetView[T, U2] {
    def toList: List[U2] = lazyBag.toList.unzip3._2.toList
  }

  private class Unzip3RightFastEquaSetView[T, U1, U2, U3](lazyBag: FastEquaSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformFastEquaSetView[T, U3] {
    def toList: List[U3] = lazyBag.toList.unzip3._3.toList
  }

  private class ZipFastEquaSetView[T, U](lazyBag: EquaSetView[T], that: EquaSetView[U]) extends TransformFastEquaSetView[T, (T, U)] {
    def toList: List[(T, U)] = lazyBag.toList.zip(that.toList)
  }

  private class ZipAllFastEquaSetView[T, U](thisBag: EquaSetView[T], thatBag: EquaSetView[U], thisElem: T, thatElem: U) extends TransformFastEquaSetView[T, (T, U)] {
    def toList: List[(T, U)] = thisBag.toList.zipAll(thatBag.toList, thisElem, thatElem)
  }

  private class ZipWithIndex[T, U](thisBag: FastEquaSetView[T]) extends TransformFastEquaSetView[T, (T, Int)] {
    def toList: List[(T, Int)] = thisBag.toList.zipWithIndex
  }

  def apply[T](args: T*): FastEquaSetView[T] = new BasicFastEquaSetView(args.toList)
}

