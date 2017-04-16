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

trait LazyBag[+T] {
  def map[U](f: T => U): LazyBag[U]
  def flatMap[U](f: T => LazyBag[U]): LazyBag[U]
  def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.EquaSet
  def toSortedEquaSet[U >: T](toPath: SortedEquaPath[U]): toPath.SortedEquaSet
  def toList: List[T]
  def size: Int
  /**
   * Converts this `LazyBag` of pairs into two collections of the first and second
   * half of each pair.
   *
   * {{{
   * val xs = `LazyBag`(
   * (1, "one"),
   * (2, "two"),
   * (3, "three")).unzip
   * // xs == (`LazyBag`(1, 2, 3),
   * // `LazyBag`(one, two, three))
   * }}}
   *
   * @tparam U1 the type of the first half of the element pairs
   * @tparam U2 the type of the second half of the element pairs
   * @param asPair an implicit conversion which asserts that the element type
   * of this `LazyBag` is a pair.
   * @return a pair of `LazyBag`s, containing the first, respectively second
   * half of each element pair of this `LazyBag`.
   */
  def unzip[U1, U2](implicit asPair: T => (U1, U2)): (LazyBag[U1], LazyBag[U2])

  /**
   * Converts this `LazyBag` of triples into three collections of the first, second,
   * and third element of each triple.
   *
   * {{{
   * val xs = `LazyBag`(
   * (1, "one", '1'),
   * (2, "two", '2'),
   * (3, "three", '3')).unzip3
   * // xs == (`LazyBag`(1, 2, 3),
   * // `LazyBag`(one, two, three),
   * // `LazyBag`(1, 2, 3))
   * }}}
   *
   * @tparam U1 the type of the first member of the element triples
   * @tparam U2 the type of the second member of the element triples
   * @tparam U3 the type of the third member of the element triples
   * @param asTriple an implicit conversion which asserts that the element type
   * of this `LazyBag` is a triple.
   * @return a triple of `LazyBag`s, containing the first, second, respectively
   * third member of each element triple of this `LazyBag`.
   */
  def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (LazyBag[U1], LazyBag[U2], LazyBag[U3])


  /**
   * Returns a `LazyBag` formed from this `LazyBag` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   * @param that The iterable providing the second half of each result pair
   * @tparam U the type of the second half of the returned pairs
   * @return a `Set` containing pairs consisting of
   * corresponding elements of this `LazyBag` and that`. The length
   * of the returned collection is the minimum of the lengths of this `LazyBag` and `that`.
   *
   */
  def zip[U](that: LazyBag[U]): LazyBag[(T, U)]

  /**
   * Returns a `LazyBag` formed from this `LazyBag` and another iterable collection
   * by combining corresponding elements in pairs.
   * If one of the two collections is shorter than the other,
   * placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   * @param that the iterable providing the second half of each result pair
   * @param thisElem the element to be used to fill up the result if this `LazyBag` is shorter than `that`.
   * @param thatElem the element to be used to fill up the result if `that` is shorter than this `LazyBag`.
   * @return a new collection of type `That` containing pairs consisting of
   * corresponding elements of this `LazyBag` and `that`. The length
   * of the returned collection is the maximum of the lengths of this `LazyBag` and `that`.
   * If this `LazyBag` is shorter than `that`, `thisElem` values are used to pad the result.
   * If `that` is shorter than this `LazyBag`, `thatElem` values are used to pad the result.
   *
   */
  def zipAll[U, T1 >: T](that: LazyBag[U], thisElem: T1, thatElem: U): LazyBag[(T1, U)]

  /**
   * Zips this `LazyBag` with its indices.
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

  def zipWithIndex: LazyBag[(T, Int)]
}

object LazyBag {
  private class BasicLazyBag[T](private val args: List[T]) extends LazyBag[T] { thisLazyBag =>
    def map[U](f: T => U): LazyBag[U] = new MapLazyBag(thisLazyBag, f)
    def flatMap[U](f: T => LazyBag[U]): LazyBag[U] = new FlatMapLazyBag(thisLazyBag, f)
    def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.FastEquaSet = toPath.FastEquaSet(args: _*)
    def toSortedEquaSet[U >: T](toPath: SortedEquaPath[U]): toPath.SortedEquaSet = ???
    def toList: List[T] = args
    def size: Int = args.size

    def unzip[U1, U2](implicit asPair: T => (U1, U2)): (LazyBag[U1], LazyBag[U2]) = (
      new UnzipLeftLazyBag[T, U1, U2](thisLazyBag)(asPair),
      new UnzipRightLazyBag[T, U1, U2](thisLazyBag)(asPair)
    )

    def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (LazyBag[U1], LazyBag[U2], LazyBag[U3]) = (
      new Unzip3LeftLazyBag(thisLazyBag),
      new Unzip3MiddleLazyBag(thisLazyBag),
      new Unzip3RightLazyBag(thisLazyBag)
    )

    def zip[U](thatLazyBag: LazyBag[U]): LazyBag[(T, U)] = new ZipLazyBag(thisLazyBag, thatLazyBag)
    def zipAll[U, T1 >: T](that: LazyBag[U], thisElem: T1, thatElem: U): LazyBag[(T1, U)] =
      new ZipAllLazyBag(thisLazyBag, that, thisElem, thatElem)
    def zipWithIndex: LazyBag[(T, Int)] = new ZipWithIndex(thisLazyBag)
    override def toString = args.mkString("LazyBag(", ",", ")")
/*  // Don't uncomment unless have a failing test
    override def equals(other: Any): Boolean =
      other match {
        case otherLazyBag: LazyBag[_] => 
          thisLazyBag.toList.groupBy(o => o) == otherLazyBag.toList.groupBy(o => o)
        case _ => false
      }
    override def hashCode: Int = thisLazyBag.toList.groupBy(o => o).hashCode
*/
  }

  private abstract class TransformLazyBag[T, U] extends LazyBag[U] { thisLazyBag =>
    def map[V](g: U => V): LazyBag[V] = new MapLazyBag[U, V](thisLazyBag, g)
    def flatMap[V](f: U => LazyBag[V]): LazyBag[V] = ???
    def toEquaSet[V >: U](toPath: EquaPath[V]): toPath.FastEquaSet = {
      toPath.FastEquaSet(toList: _*)
    }
    def toSortedEquaSet[V >: U](toPath: SortedEquaPath[V]): toPath.SortedEquaSet = ???
    def toList: List[U] // This is the lone abstract method
    def size: Int = toList.size

    def unzip[V1, V2](implicit asPair: U => (V1, V2)): (LazyBag[V1], LazyBag[V2]) =
      (new UnzipLeftLazyBag[U, V1, V2](thisLazyBag)(asPair), new UnzipRightLazyBag[U, V1, V2](thisLazyBag)(asPair))

    def unzip3[V1, V2, V3](implicit asTriple: U => (V1, V2, V3)): (LazyBag[V1], LazyBag[V2], LazyBag[V3]) =
      (new Unzip3LeftLazyBag(thisLazyBag), new Unzip3MiddleLazyBag(thisLazyBag), new Unzip3RightLazyBag(thisLazyBag))

    def zip[V](that: LazyBag[V]): LazyBag[(U, V)] = new ZipLazyBag[U, V](thisLazyBag, that)
    def zipAll[V, U1 >: U](that: LazyBag[V], thisElem: U1, thatElem: V): LazyBag[(U1, V)] =
      new ZipAllLazyBag(thisLazyBag, that, thisElem, thatElem)
    def zipWithIndex: LazyBag[(U, Int)] = new ZipWithIndex(thisLazyBag)
    override def toString: String = toList.mkString("LazyBag(", ",", ")")
    override def equals(other: Any): Boolean =
      other match {
        case otherLazyBag: LazyBag[_] => 
          thisLazyBag.toList.groupBy(o => o) == otherLazyBag.toList.groupBy(o => o)
        case _ => false
      }
    override def hashCode: Int = thisLazyBag.toList.groupBy(o => o).hashCode
  }

  private class MapLazyBag[T, U](lazyBag: LazyBag[T], f: T => U) extends TransformLazyBag[T, U] {
    def toList: List[U] = lazyBag.toList.map(f)
  }

  private class FlatMapLazyBag[T, U](lazyBag: LazyBag[T], f: T => LazyBag[U]) extends TransformLazyBag[T, U] {
    def toList: List[U] = lazyBag.toList.flatMap(f.andThen(_.toList))
  }

  private class UnzipLeftLazyBag[T, U1, U2](lazyBag: LazyBag[T])(implicit asPair: T => (U1, U2)) extends TransformLazyBag[T, U1] {
    def toList: List[U1] = lazyBag.toList.unzip._1.toList
  }

  private class UnzipRightLazyBag[T, U1, U2](lazyBag: LazyBag[T])(implicit asPair: T => (U1, U2)) extends TransformLazyBag[T, U2] {
    def toList: List[U2] = lazyBag.toList.unzip._2.toList
  }

  private class Unzip3LeftLazyBag[T, U1, U2, U3](lazyBag: LazyBag[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformLazyBag[T, U1] {
    def toList: List[U1] = lazyBag.toList.unzip3._1.toList
  }

  private class Unzip3MiddleLazyBag[T, U1, U2, U3](lazyBag: LazyBag[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformLazyBag[T, U2] {
    def toList: List[U2] = lazyBag.toList.unzip3._2.toList
  }

  private class Unzip3RightLazyBag[T, U1, U2, U3](lazyBag: LazyBag[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformLazyBag[T, U3] {
    def toList: List[U3] = lazyBag.toList.unzip3._3.toList
  }

  private class ZipLazyBag[T, U](lazyBag: LazyBag[T], that: LazyBag[U]) extends TransformLazyBag[T, (T, U)] {
    def toList: List[(T, U)] = lazyBag.toList.zip(that.toList)
  }

  private class ZipAllLazyBag[T, U](thisBag: LazyBag[T], thatBag: LazyBag[U], thisElem: T, thatElem: U) extends TransformLazyBag[T, (T, U)] {
    def toList: List[(T, U)] = thisBag.toList.zipAll(thatBag.toList, thisElem, thatElem)
  }

  private class ZipWithIndex[T, U](thisBag: LazyBag[T]) extends TransformLazyBag[T, (T, Int)] {
    def toList: List[(T, Int)] = thisBag.toList.zipWithIndex
  }

  def apply[T](args: T*): LazyBag[T] = new BasicLazyBag(args.toList)
}

