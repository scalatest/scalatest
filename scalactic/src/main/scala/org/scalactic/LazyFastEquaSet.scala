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

trait LazyFastEquaSet[+T] extends LazyEquaSet[T] {
  def map[U](f: T => U): LazyFastEquaSet[U]
  def flatMap[U](f: T => LazyEquaSet[U]): LazyFastEquaSet[U]
  def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.EquaSet
  def toStrict[U >: T](toPath: EquaPath[U]): toPath.EquaSet
  def toSortedEquaSet[U >: T](toPath: SortedEquaPath[U]): toPath.SortedEquaSet
  def toList: List[T]
  def size: Int
  /**
   * Builds a new collection by applying a partial function to all elements of this `LazyFastEquaSet`
   * on which the function is defined.
   *
   * @param pf the partial function which filters and maps the `LazyFastEquaSet`.
   * @return a new collection of type `That` resulting from applying the partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   *
   * @return a new `LazyFastEquaSet` resulting from applying the given partial function
   * `pf` to each element on which it is defined and collecting the results.
   * The order of the elements is preserved.
   */
  def collect[U](pf: PartialFunction[T, U]): LazyFastEquaSet[U]

  def scan[U >: T](z: U)(op: (U, U) ⇒ U): LazyFastEquaSet[U]

  /**
   * Produces a collection containing cumulative results of applying the
   * operator going left to right.
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `LazyFastEquaSet` with intermediate results
   */
  def scanLeft[U](z: U)(op: (U, T) => U): LazyFastEquaSet[U]

  /**
   * Produces a collection containing cumulative results of applying the operator going right to left.
   * The head of the collection is the last cumulative result.
   *
   * Example:
   * {{{
   * `LazyFastEquaSet`(1, 2, 3, 4).scanRight(0)(_ + _) == `LazyFastEquaSet`(10, 9, 7, 4, 0)
   * }}}
   *
   * @param z the initial value
   * @param op the binary operator applied to the intermediate result and the element
   * @return `LazyFastEquaSet` with intermediate results
   */
  def scanRight[U](z: U)(op: (T, U) => U): LazyFastEquaSet[U]

  /**
   * Converts this `LazyFastEquaSet` of pairs into two collections of the first and second
   * half of each pair.
   *
   * {{{
   * val xs = `LazyFastEquaSet`(
   * (1, "one"),
   * (2, "two"),
   * (3, "three")).unzip
   * // xs == (`LazyFastEquaSet`(1, 2, 3),
   * // `LazyFastEquaSet`(one, two, three))
   * }}}
   *
   * @tparam U1 the type of the first half of the element pairs
   * @tparam U2 the type of the second half of the element pairs
   * @param asPair an implicit conversion which asserts that the element type
   * of this `LazyFastEquaSet` is a pair.
   * @return a pair of `LazyFastEquaSet`s, containing the first, respectively second
   * half of each element pair of this `LazyFastEquaSet`.
   */
  def unzip[U1, U2](implicit asPair: T => (U1, U2)): (LazyFastEquaSet[U1], LazyFastEquaSet[U2])

  /**
   * Converts this `LazyFastEquaSet` of triples into three collections of the first, second,
   * and third element of each triple.
   *
   * {{{
   * val xs = `LazyFastEquaSet`(
   * (1, "one", '1'),
   * (2, "two", '2'),
   * (3, "three", '3')).unzip3
   * // xs == (`LazyFastEquaSet`(1, 2, 3),
   * // `LazyFastEquaSet`(one, two, three),
   * // `LazyFastEquaSet`(1, 2, 3))
   * }}}
   *
   * @tparam U1 the type of the first member of the element triples
   * @tparam U2 the type of the second member of the element triples
   * @tparam U3 the type of the third member of the element triples
   * @param asTriple an implicit conversion which asserts that the element type
   * of this `LazyFastEquaSet` is a triple.
   * @return a triple of `LazyFastEquaSet`s, containing the first, second, respectively
   * third member of each element triple of this `LazyFastEquaSet`.
   */
  def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (LazyFastEquaSet[U1], LazyFastEquaSet[U2], LazyFastEquaSet[U3])


  /**
   * Returns a `LazyFastEquaSet` formed from this `LazyFastEquaSet` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   * @param that The iterable providing the second half of each result pair
   * @tparam U the type of the second half of the returned pairs
   * @return a `Set` containing pairs consisting of
   * corresponding elements of this `LazyFastEquaSet` and that`. The length
   * of the returned collection is the minimum of the lengths of this `LazyFastEquaSet` and `that`.
   *
   */
  def zip[U](that: LazyEquaSet[U]): LazyFastEquaSet[(T, U)]

  /**
   * Returns a `LazyFastEquaSet` formed from this `LazyFastEquaSet` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is shorter than the other,
   * placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   * @param that the iterable providing the second half of each result pair
   * @param thisElem the element to be used to fill up the result if this `LazyFastEquaSet` is shorter than `that`.
   * @param thatElem the element to be used to fill up the result if `that` is shorter than this `LazyFastEquaSet`.
   * @return a new collection of type `That` containing pairs consisting of
   * corresponding elements of this `LazyFastEquaSet` and `that`. The length
   * of the returned collection is the maximum of the lengths of this `LazyFastEquaSet` and `that`.
   * If this `LazyFastEquaSet` is shorter than `that`, `thisElem` values are used to pad the result.
   * If `that` is shorter than this `LazyFastEquaSet`, `thatElem` values are used to pad the result.
   *
   */
  def zipAll[U, T1 >: T](that: LazyEquaSet[U], thisElem: T1, thatElem: U): LazyFastEquaSet[(T1, U)]

  /**
   * Zips this `LazyFastEquaSet` with its indices.
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

  def zipWithIndex: LazyFastEquaSet[(T, Int)]
}

object LazyFastEquaSet {
  private class BasicLazyFastEquaSet[T](private val args: List[T]) extends LazyFastEquaSet[T] { thisLazyFastEquaSet =>
    def collect[U](pf: PartialFunction[T, U]): LazyFastEquaSet[U] = new CollectLazyFastEquaSet(thisLazyFastEquaSet, pf)
    def map[U](f: T => U): LazyFastEquaSet[U] = new MapLazyFastEquaSet(thisLazyFastEquaSet, f)
    def flatMap[U](f: T => LazyEquaSet[U]): LazyFastEquaSet[U] = new FlatMapLazyFastEquaSet(thisLazyFastEquaSet, f)
    def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.FastEquaSet = toStrict(toPath)
    def toStrict[U >: T](toPath: EquaPath[U]): toPath.FastEquaSet = toPath.FastEquaSet(args: _*)
    def toSortedEquaSet[U >: T](toPath: SortedEquaPath[U]): toPath.SortedEquaSet = ???
    def toList: List[T] = args

    def scan[U >: T](z: U)(op: (U, U) ⇒ U): LazyFastEquaSet[U] = new ScanLazyFastEquaSet(thisLazyFastEquaSet, z, op)
    def scanLeft[U](z: U)(op: (U, T) => U): LazyFastEquaSet[U] = new ScanLeftLazyFastEquaSet(thisLazyFastEquaSet, z, op)
    def scanRight[U](z: U)(op: (T, U) => U): LazyFastEquaSet[U] = new ScanRightLazyFastEquaSet(thisLazyFastEquaSet, z, op)

    def size: Int = args.size

    def unzip[U1, U2](implicit asPair: T => (U1, U2)): (LazyFastEquaSet[U1], LazyFastEquaSet[U2]) = (
      new UnzipLeftLazyFastEquaSet[T, U1, U2](thisLazyFastEquaSet)(asPair),
      new UnzipRightLazyFastEquaSet[T, U1, U2](thisLazyFastEquaSet)(asPair)
    )

    def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (LazyFastEquaSet[U1], LazyFastEquaSet[U2], LazyFastEquaSet[U3]) = (
      new Unzip3LeftLazyFastEquaSet(thisLazyFastEquaSet),
      new Unzip3MiddleLazyFastEquaSet(thisLazyFastEquaSet),
      new Unzip3RightLazyFastEquaSet(thisLazyFastEquaSet)
    )

    def zip[U](thatLazyFastEquaSet: LazyEquaSet[U]): LazyFastEquaSet[(T, U)] = new ZipLazyFastEquaSet(thisLazyFastEquaSet, thatLazyFastEquaSet)
    def zipAll[U, T1 >: T](that: LazyEquaSet[U], thisElem: T1, thatElem: U): LazyFastEquaSet[(T1, U)] =
      new ZipAllLazyFastEquaSet(thisLazyFastEquaSet, that, thisElem, thatElem)
    def zipWithIndex: LazyFastEquaSet[(T, Int)] = new ZipWithIndex(thisLazyFastEquaSet)
    override def toString = args.mkString("LazyFastEquaSet(", ",", ")")
/*  // Don't uncomment unless have a failing test
    override def equals(other: Any): Boolean =
      other match {
        case otherLazyFastEquaSet: LazyFastEquaSet[_] => 
          thisLazyFastEquaSet.toList.groupBy(o => o) == otherLazyFastEquaSet.toList.groupBy(o => o)
        case _ => false
      }
    override def hashCode: Int = thisLazyFastEquaSet.toList.groupBy(o => o).hashCode
*/
  }

  private abstract class TransformLazyFastEquaSet[T, U] extends LazyFastEquaSet[U] { thisLazyFastEquaSet =>
    def collect[V](pf: PartialFunction[U, V]): LazyFastEquaSet[V] = new CollectLazyFastEquaSet(thisLazyFastEquaSet, pf)
    def map[V](g: U => V): LazyFastEquaSet[V] = new MapLazyFastEquaSet[U, V](thisLazyFastEquaSet, g)
    def flatMap[V](f: U => LazyEquaSet[V]): LazyFastEquaSet[V] = ???
    def toEquaSet[V >: U](toPath: EquaPath[V]): toPath.FastEquaSet = toStrict(toPath)
    def toStrict[V >: U](toPath: EquaPath[V]): toPath.FastEquaSet = {
      toPath.FastEquaSet(toList: _*)
    }
    def toSortedEquaSet[V >: U](toPath: SortedEquaPath[V]): toPath.SortedEquaSet = ???
    def toList: List[U] // This is the lone abstract method

    def scan[V >: U](z: V)(op: (V, V) ⇒ V): LazyFastEquaSet[V] = new ScanLazyFastEquaSet(thisLazyFastEquaSet, z, op)
    def scanLeft[V](z: V)(op: (V, U) => V): LazyFastEquaSet[V] = new ScanLeftLazyFastEquaSet(thisLazyFastEquaSet, z, op)
    def scanRight[V](z: V)(op: (U, V) => V): LazyFastEquaSet[V] = new ScanRightLazyFastEquaSet(thisLazyFastEquaSet, z, op)

    def size: Int = toList.size

    def unzip[V1, V2](implicit asPair: U => (V1, V2)): (LazyFastEquaSet[V1], LazyFastEquaSet[V2]) =
      (new UnzipLeftLazyFastEquaSet[U, V1, V2](thisLazyFastEquaSet)(asPair), new UnzipRightLazyFastEquaSet[U, V1, V2](thisLazyFastEquaSet)(asPair))

    def unzip3[V1, V2, V3](implicit asTriple: U => (V1, V2, V3)): (LazyFastEquaSet[V1], LazyFastEquaSet[V2], LazyFastEquaSet[V3]) =
      (new Unzip3LeftLazyFastEquaSet(thisLazyFastEquaSet), new Unzip3MiddleLazyFastEquaSet(thisLazyFastEquaSet), new Unzip3RightLazyFastEquaSet(thisLazyFastEquaSet))

    def zip[V](that: LazyEquaSet[V]): LazyFastEquaSet[(U, V)] = new ZipLazyFastEquaSet[U, V](thisLazyFastEquaSet, that)
    def zipAll[V, U1 >: U](that: LazyEquaSet[V], thisElem: U1, thatElem: V): LazyFastEquaSet[(U1, V)] =
      new ZipAllLazyFastEquaSet(thisLazyFastEquaSet, that, thisElem, thatElem)
    def zipWithIndex: LazyFastEquaSet[(U, Int)] = new ZipWithIndex(thisLazyFastEquaSet)
    override def toString: String = toList.mkString("LazyFastEquaSet(", ",", ")")
    override def equals(other: Any): Boolean =
      other match {
        case otherLazyFastEquaSet: LazyFastEquaSet[_] => 
          thisLazyFastEquaSet.toList.groupBy(o => o) == otherLazyFastEquaSet.toList.groupBy(o => o)
        case _ => false
      }
    override def hashCode: Int = thisLazyFastEquaSet.toList.groupBy(o => o).hashCode
  }

  private class CollectLazyFastEquaSet[T, U](lazyBag: LazyFastEquaSet[T], pf: PartialFunction[T, U]) extends TransformLazyFastEquaSet[T, U] {
    def toList: List[U] = lazyBag.toList.collect(pf)
  }

  private class MapLazyFastEquaSet[T, U](lazyBag: LazyFastEquaSet[T], f: T => U) extends TransformLazyFastEquaSet[T, U] {
    def toList: List[U] = lazyBag.toList.map(f)
  }

  private class FlatMapLazyFastEquaSet[T, U](lazyBag: LazyFastEquaSet[T], f: T => LazyEquaSet[U]) extends TransformLazyFastEquaSet[T, U] {
    def toList: List[U] = lazyBag.toList.flatMap(f.andThen(_.toList))
  }

  private class ScanLazyFastEquaSet[T](lazyBag: LazyFastEquaSet[T], z: T, op: (T, T) ⇒ T) extends TransformLazyFastEquaSet[T, T] {
    def toList: List[T] = lazyBag.toList.scan(z)(op)
  }

  private class ScanLeftLazyFastEquaSet[T, U](lazyBag: LazyFastEquaSet[T], z: U, op: (U, T) ⇒ U) extends TransformLazyFastEquaSet[T, U] {
    def toList: List[U] = lazyBag.toList.scanLeft(z)(op)
  }

  private class ScanRightLazyFastEquaSet[T, U](lazyBag: LazyFastEquaSet[T], z: U, op: (T, U) ⇒ U) extends TransformLazyFastEquaSet[T, U] {
    def toList: List[U] = lazyBag.toList.scanRight(z)(op)
  }

  private class UnzipLeftLazyFastEquaSet[T, U1, U2](lazyBag: LazyFastEquaSet[T])(implicit asPair: T => (U1, U2)) extends TransformLazyFastEquaSet[T, U1] {
    def toList: List[U1] = lazyBag.toList.unzip._1.toList
  }

  private class UnzipRightLazyFastEquaSet[T, U1, U2](lazyBag: LazyFastEquaSet[T])(implicit asPair: T => (U1, U2)) extends TransformLazyFastEquaSet[T, U2] {
    def toList: List[U2] = lazyBag.toList.unzip._2.toList
  }

  private class Unzip3LeftLazyFastEquaSet[T, U1, U2, U3](lazyBag: LazyFastEquaSet[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformLazyFastEquaSet[T, U1] {
    def toList: List[U1] = lazyBag.toList.unzip3._1.toList
  }

  private class Unzip3MiddleLazyFastEquaSet[T, U1, U2, U3](lazyBag: LazyFastEquaSet[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformLazyFastEquaSet[T, U2] {
    def toList: List[U2] = lazyBag.toList.unzip3._2.toList
  }

  private class Unzip3RightLazyFastEquaSet[T, U1, U2, U3](lazyBag: LazyFastEquaSet[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformLazyFastEquaSet[T, U3] {
    def toList: List[U3] = lazyBag.toList.unzip3._3.toList
  }

  private class ZipLazyFastEquaSet[T, U](lazyBag: LazyEquaSet[T], that: LazyEquaSet[U]) extends TransformLazyFastEquaSet[T, (T, U)] {
    def toList: List[(T, U)] = lazyBag.toList.zip(that.toList)
  }

  private class ZipAllLazyFastEquaSet[T, U](thisBag: LazyEquaSet[T], thatBag: LazyEquaSet[U], thisElem: T, thatElem: U) extends TransformLazyFastEquaSet[T, (T, U)] {
    def toList: List[(T, U)] = thisBag.toList.zipAll(thatBag.toList, thisElem, thatElem)
  }

  private class ZipWithIndex[T, U](thisBag: LazyFastEquaSet[T]) extends TransformLazyFastEquaSet[T, (T, Int)] {
    def toList: List[(T, Int)] = thisBag.toList.zipWithIndex
  }

  def apply[T](args: T*): LazyFastEquaSet[T] = new BasicLazyFastEquaSet(args.toList)
}

