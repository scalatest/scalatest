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

trait LazyTreeEquaSet[+T] extends LazySortedEquaSet[T] {
  def collect[U](pf: PartialFunction[T, U]): LazyTreeEquaSet[U]

  def map[U](f: T => U): LazyTreeEquaSet[U]
  def flatMap[U](f: T => LazyEquaSet[U]): LazyTreeEquaSet[U]
  def toStrict[U >: T](toPath: EquaPath[U]): toPath.EquaSet
  def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.EquaSet
  def toStrict[U >: T](toPath: SortedEquaPath[U]): toPath.TreeEquaSet
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

object LazyTreeEquaSet {
  private class BasicLazyTreeEquaSet[T](private val args: List[T]) extends LazyTreeEquaSet[T] { thisLazyTreeEquaSet =>
    def collect[U](pf: PartialFunction[T, U]): LazyTreeEquaSet[U] = new CollectLazyTreeEquaSet(thisLazyTreeEquaSet, pf)
    def map[U](f: T => U): LazyTreeEquaSet[U] = new MapLazyTreeEquaSet(thisLazyTreeEquaSet, f)
    def flatMap[U](f: T => LazyEquaSet[U]): LazyTreeEquaSet[U] = new FlatMapLazyTreeEquaSet(thisLazyTreeEquaSet, f)
    def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.FastEquaSet = toStrict(toPath)
    def toStrict[U >: T](toPath: EquaPath[U]): toPath.FastEquaSet = toPath.FastEquaSet(args: _*)
    def toSortedEquaSet[U >: T](toPath: SortedEquaPath[U]): toPath.SortedEquaSet = toStrict(toPath)
    def toStrict[U >: T](toPath: SortedEquaPath[U]): toPath.TreeEquaSet = toPath.TreeEquaSet(args: _*)
    def toList: List[T] = args

    def scan[U >: T](z: U)(op: (U, U) ⇒ U): LazyTreeEquaSet[U] = new ScanLazyTreeEquaSet(thisLazyTreeEquaSet, z, op)
    def scanLeft[U](z: U)(op: (U, T) => U): LazyTreeEquaSet[U] = new ScanLeftLazyTreeEquaSet(thisLazyTreeEquaSet, z, op)
    def scanRight[U](z: U)(op: (T, U) => U): LazyTreeEquaSet[U] = new ScanRightLazyTreeEquaSet(thisLazyTreeEquaSet, z, op)

    def size: Int = args.size

    def unzip[U1, U2](implicit asPair: T => (U1, U2)): (LazyTreeEquaSet[U1], LazyTreeEquaSet[U2]) = (
      new UnzipLeftLazyTreeEquaSet[T, U1, U2](thisLazyTreeEquaSet)(asPair),
      new UnzipRightLazyTreeEquaSet[T, U1, U2](thisLazyTreeEquaSet)(asPair)
    )

    def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (LazyTreeEquaSet[U1], LazyTreeEquaSet[U2], LazyTreeEquaSet[U3]) = (
      new Unzip3LeftLazyTreeEquaSet[T, U1, U2, U3](thisLazyTreeEquaSet),
      new Unzip3MiddleLazyTreeEquaSet[T, U1, U2, U3](thisLazyTreeEquaSet),
      new Unzip3RightLazyTreeEquaSet[T, U1, U2, U3](thisLazyTreeEquaSet)
    )

    def zip[U](that: LazyEquaSet[U]): LazyTreeEquaSet[(T, U)] = new ZipLazyTreeEquaSet(thisLazyTreeEquaSet, that)
    def zipAll[U, T1 >: T](that: LazyEquaSet[U], thisElem: T1, thatElem: U): LazyTreeEquaSet[(T1, U)] =
      new ZipAllLazyTreeEquaSet(thisLazyTreeEquaSet, that, thisElem, thatElem)
    def zipWithIndex: LazyTreeEquaSet[(T, Int)] = new ZipWithIndex(thisLazyTreeEquaSet)

    override def toString = args.mkString("LazyTreeEquaSet(", ",", ")")
    override def equals(other: Any): Boolean = ???
    override def hashCode: Int = ???
  }

  private abstract class TransformLazyTreeEquaSet[T, U] extends LazyTreeEquaSet[U] { thisLazyTreeEquaSet =>
    def collect[V](pf: PartialFunction[U, V]): LazyTreeEquaSet[V] = new CollectLazyTreeEquaSet(thisLazyTreeEquaSet, pf)
    def map[V](g: U => V): LazyTreeEquaSet[V] = new MapLazyTreeEquaSet[U, V](thisLazyTreeEquaSet, g)
    def flatMap[V](f: U => LazyEquaSet[V]): LazyTreeEquaSet[V] = new FlatMapLazyTreeEquaSet(thisLazyTreeEquaSet, f)
    def toStrict[V >: U](toPath: EquaPath[V]): toPath.FastEquaSet = {
      toPath.FastEquaSet(toList: _*)
    }
    def toEquaSet[V >: U](toPath: EquaPath[V]): toPath.FastEquaSet = toStrict(toPath)
    def toStrict[V >: U](toPath: SortedEquaPath[V]): toPath.TreeEquaSet = {
      toPath.TreeEquaSet(toList: _*)
    }
    def toSortedEquaSet[V >: U](toPath: SortedEquaPath[V]): toPath.SortedEquaSet = toStrict(toPath)
    def toList: List[U]

    def scan[V >: U](z: V)(op: (V, V) ⇒ V): LazyTreeEquaSet[V] = new ScanLazyTreeEquaSet(thisLazyTreeEquaSet, z, op)
    def scanLeft[V](z: V)(op: (V, U) => V): LazyTreeEquaSet[V] = new ScanLeftLazyTreeEquaSet(thisLazyTreeEquaSet, z, op)
    def scanRight[V](z: V)(op: (U, V) => V): LazyTreeEquaSet[V] = new ScanRightLazyTreeEquaSet(thisLazyTreeEquaSet, z, op)

    def size: Int = toList.size

    def unzip[V1, V2](implicit asPair: U => (V1, V2)): (LazyTreeEquaSet[V1], LazyTreeEquaSet[V2]) = (
      new UnzipLeftLazyTreeEquaSet[U, V1, V2](thisLazyTreeEquaSet)(asPair),
      new UnzipRightLazyTreeEquaSet[U, V1, V2](thisLazyTreeEquaSet)(asPair)
    )

    def unzip3[V1, V2, V3](implicit asTriple: U => (V1, V2, V3)): (LazyTreeEquaSet[V1], LazyTreeEquaSet[V2], LazyTreeEquaSet[V3]) = (
      new Unzip3LeftLazyTreeEquaSet[U, V1, V2, V3](thisLazyTreeEquaSet),
      new Unzip3MiddleLazyTreeEquaSet[U, V1, V2, V3](thisLazyTreeEquaSet),
      new Unzip3RightLazyTreeEquaSet[U, V1, V2, V3](thisLazyTreeEquaSet)
    )

    def zip[V](that: LazyEquaSet[V]): LazyTreeEquaSet[(U, V)] = new ZipLazyTreeEquaSet(thisLazyTreeEquaSet, that)
    def zipAll[V, U1 >: U](that: LazyEquaSet[V], thisElem: U1, thatElem: V): LazyTreeEquaSet[(U1, V)] =
      new ZipAllLazyTreeEquaSet(thisLazyTreeEquaSet, that, thisElem, thatElem)
    def zipWithIndex: LazyTreeEquaSet[(U, Int)] = new ZipWithIndex(thisLazyTreeEquaSet)

    override def toString: String = toList.mkString("LazyTreeEquaSet(", ",", ")")
    override def equals(other: Any): Boolean =
      other match {
        case otherLazyTreeEquaSet: LazyTreeEquaSet[_] =>
          thisLazyTreeEquaSet.toList == otherLazyTreeEquaSet.toList
        case _ => false
      }
    override def hashCode: Int = thisLazyTreeEquaSet.toList.hashCode
  }

  private class CollectLazyTreeEquaSet[T, U](lazyBag: LazyTreeEquaSet[T], pf: PartialFunction[T, U]) extends TransformLazyTreeEquaSet[T, U] {
    def toList: List[U] = lazyBag.toList.collect(pf)
  }

  private class MapLazyTreeEquaSet[T, U](lazySeq: LazyTreeEquaSet[T], f: T => U) extends TransformLazyTreeEquaSet[T, U] { thisLazyTreeEquaSet =>
    def toList: List[U] = lazySeq.toList.map(f)
  }

  private class FlatMapLazyTreeEquaSet[T, U](lazySeq: LazyTreeEquaSet[T], f: T => LazyEquaSet[U]) extends TransformLazyTreeEquaSet[T, U] { thisLazyTreeEquaSet =>
    def toList: List[U] = lazySeq.toList.flatMap(f.andThen(_.toList))
  }

  private class ScanLazyTreeEquaSet[T](lazySeq: LazyTreeEquaSet[T], z: T, op: (T, T) ⇒ T) extends TransformLazyTreeEquaSet[T, T] {
    def toList: List[T] = lazySeq.toList.scan(z)(op)
  }

  private class ScanLeftLazyTreeEquaSet[T, U](lazySeq: LazyTreeEquaSet[T], z: U, op: (U, T) ⇒ U) extends TransformLazyTreeEquaSet[T, U] {
    def toList: List[U] = lazySeq.toList.scanLeft(z)(op)
  }

  private class ScanRightLazyTreeEquaSet[T, U](lazySeq: LazyTreeEquaSet[T], z: U, op: (T, U) ⇒ U) extends TransformLazyTreeEquaSet[T, U] {
    def toList: List[U] = lazySeq.toList.scanRight(z)(op)
  }

  private class ZipLazyTreeEquaSet[T, U](thisSeq: LazyTreeEquaSet[T], that: LazyEquaSet[U]) extends TransformLazyTreeEquaSet[T, (T, U)] {
    def toList: List[(T, U)] = thisSeq.toList.zip(that.toList)
  }

  private class ZipAllLazyTreeEquaSet[T, U](thisSeq: LazyTreeEquaSet[T], thatBag: LazyEquaSet[U], thisElem: T, thatElem: U) extends TransformLazyTreeEquaSet[T, (T, U)] {
    def toList: List[(T, U)] = thisSeq.toList.zipAll(thatBag.toList, thisElem, thatElem)
  }

  private class ZipWithIndex[T, U](thisSeq: LazyTreeEquaSet[T]) extends TransformLazyTreeEquaSet[T, (T, Int)] {
    def toList: List[(T, Int)] = thisSeq.toList.zipWithIndex
  }

  private class UnzipLeftLazyTreeEquaSet[T, U1, U2](lazySeq: LazyTreeEquaSet[T])(implicit asPair: T => (U1, U2)) extends TransformLazyTreeEquaSet[T, U1] {
    def toList: List[U1] = lazySeq.toList.unzip._1.toList
  }

  private class UnzipRightLazyTreeEquaSet[T, U1, U2](lazySeq: LazyTreeEquaSet[T])(implicit asPair: T => (U1, U2)) extends TransformLazyTreeEquaSet[T, U2] {
    def toList: List[U2] = lazySeq.toList.unzip._2.toList
  }

  private class Unzip3LeftLazyTreeEquaSet[T, U1, U2, U3](lazySeq: LazyTreeEquaSet[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformLazyTreeEquaSet[T, U1] {
    def toList: List[U1] = lazySeq.toList.unzip3._1.toList
  }

  private class Unzip3MiddleLazyTreeEquaSet[T, U1, U2, U3](lazySeq: LazyTreeEquaSet[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformLazyTreeEquaSet[T, U2] {
    def toList: List[U2] = lazySeq.toList.unzip3._2.toList
  }

  private class Unzip3RightLazyTreeEquaSet[T, U1, U2, U3](lazySeq: LazyTreeEquaSet[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformLazyTreeEquaSet[T, U3] {
    def toList: List[U3] = lazySeq.toList.unzip3._3.toList
  }

  def apply[T](args: T*): LazyTreeEquaSet[T] = new BasicLazyTreeEquaSet(args.toList)
}
