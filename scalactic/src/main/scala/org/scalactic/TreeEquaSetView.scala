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

trait TreeEquaSetView[+T] extends SortedEquaSetView[T] {
  def collect[U](pf: PartialFunction[T, U]): TreeEquaSetView[U]

  def map[U](f: T => U): TreeEquaSetView[U]
  def flatMap[U](f: T => EquaSetView[U]): TreeEquaSetView[U]
  def force[U >: T](toPath: EquaPath[U]): toPath.immutable.EquaSet[U]
  def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.immutable.EquaSet[U]
  def force[U >: T](toPath: SortedEquaPath[U]): toPath.immutable.TreeEquaSet[U]
  def toSortedEquaSet[U >: T](toPath: SortedEquaPath[U]): toPath.immutable.SortedEquaSet[U]
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

object TreeEquaSetView {
  private class BasicTreeEquaSetView[T](private val args: List[T]) extends TreeEquaSetView[T] { thisTreeEquaSetView =>
    def collect[U](pf: PartialFunction[T, U]): TreeEquaSetView[U] = new CollectTreeEquaSetView(thisTreeEquaSetView, pf)
    def map[U](f: T => U): TreeEquaSetView[U] = new MapTreeEquaSetView(thisTreeEquaSetView, f)
    def flatMap[U](f: T => EquaSetView[U]): TreeEquaSetView[U] = new FlatMapTreeEquaSetView(thisTreeEquaSetView, f)
    def toEquaSet[U >: T](toPath: EquaPath[U]): toPath.immutable.FastEquaSet[U] = force(toPath)
    def force[U >: T](toPath: EquaPath[U]): toPath.immutable.FastEquaSet[U] = toPath.immutable.FastEquaSet(args: _*)
    def toSortedEquaSet[U >: T](toPath: SortedEquaPath[U]): toPath.immutable.SortedEquaSet[U] = force(toPath)
    def force[U >: T](toPath: SortedEquaPath[U]): toPath.immutable.TreeEquaSet[U] = toPath.immutable.TreeEquaSet(args: _*)
    def toList: List[T] = args

    def scan[U >: T](z: U)(op: (U, U) ⇒ U): TreeEquaSetView[U] = new ScanTreeEquaSetView(thisTreeEquaSetView, z, op)
    def scanLeft[U](z: U)(op: (U, T) => U): TreeEquaSetView[U] = new ScanLeftTreeEquaSetView(thisTreeEquaSetView, z, op)
    def scanRight[U](z: U)(op: (T, U) => U): TreeEquaSetView[U] = new ScanRightTreeEquaSetView(thisTreeEquaSetView, z, op)

    def size: Int = args.size

    def unzip[U1, U2](implicit asPair: T => (U1, U2)): (TreeEquaSetView[U1], TreeEquaSetView[U2]) = (
      new UnzipLeftTreeEquaSetView[T, U1, U2](thisTreeEquaSetView)(asPair),
      new UnzipRightTreeEquaSetView[T, U1, U2](thisTreeEquaSetView)(asPair)
    )

    def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (TreeEquaSetView[U1], TreeEquaSetView[U2], TreeEquaSetView[U3]) = (
      new Unzip3LeftTreeEquaSetView[T, U1, U2, U3](thisTreeEquaSetView),
      new Unzip3MiddleTreeEquaSetView[T, U1, U2, U3](thisTreeEquaSetView),
      new Unzip3RightTreeEquaSetView[T, U1, U2, U3](thisTreeEquaSetView)
    )

    def zip[U](that: EquaSetView[U]): TreeEquaSetView[(T, U)] = new ZipTreeEquaSetView(thisTreeEquaSetView, that)
    def zipAll[U, T1 >: T](that: EquaSetView[U], thisElem: T1, thatElem: U): TreeEquaSetView[(T1, U)] =
      new ZipAllTreeEquaSetView(thisTreeEquaSetView, that, thisElem, thatElem)
    def zipWithIndex: TreeEquaSetView[(T, Int)] = new ZipWithIndex(thisTreeEquaSetView)

    override def toString = args.mkString("TreeEquaSetView(", ",", ")")
    override def equals(other: Any): Boolean = ???
    override def hashCode: Int = ???
  }

  private abstract class TransformTreeEquaSetView[T, U] extends TreeEquaSetView[U] { thisTreeEquaSetView =>
    def collect[V](pf: PartialFunction[U, V]): TreeEquaSetView[V] = new CollectTreeEquaSetView(thisTreeEquaSetView, pf)
    def map[V](g: U => V): TreeEquaSetView[V] = new MapTreeEquaSetView[U, V](thisTreeEquaSetView, g)
    def flatMap[V](f: U => EquaSetView[V]): TreeEquaSetView[V] = new FlatMapTreeEquaSetView(thisTreeEquaSetView, f)
    def force[V >: U](toPath: EquaPath[V]): toPath.immutable.FastEquaSet[V] = {
      toPath.immutable.FastEquaSet[V](toList: _*)
    }
    def toEquaSet[V >: U](toPath: EquaPath[V]): toPath.immutable.FastEquaSet[V] = force(toPath)
    def force[V >: U](toPath: SortedEquaPath[V]): toPath.immutable.TreeEquaSet[V] = {
      toPath.immutable.TreeEquaSet[V](toList: _*)
    }
    def toSortedEquaSet[V >: U](toPath: SortedEquaPath[V]): toPath.immutable.SortedEquaSet[V] = force(toPath)
    def toList: List[U]

    def scan[V >: U](z: V)(op: (V, V) ⇒ V): TreeEquaSetView[V] = new ScanTreeEquaSetView(thisTreeEquaSetView, z, op)
    def scanLeft[V](z: V)(op: (V, U) => V): TreeEquaSetView[V] = new ScanLeftTreeEquaSetView(thisTreeEquaSetView, z, op)
    def scanRight[V](z: V)(op: (U, V) => V): TreeEquaSetView[V] = new ScanRightTreeEquaSetView(thisTreeEquaSetView, z, op)

    def size: Int = toList.size

    def unzip[V1, V2](implicit asPair: U => (V1, V2)): (TreeEquaSetView[V1], TreeEquaSetView[V2]) = (
      new UnzipLeftTreeEquaSetView[U, V1, V2](thisTreeEquaSetView)(asPair),
      new UnzipRightTreeEquaSetView[U, V1, V2](thisTreeEquaSetView)(asPair)
    )

    def unzip3[V1, V2, V3](implicit asTriple: U => (V1, V2, V3)): (TreeEquaSetView[V1], TreeEquaSetView[V2], TreeEquaSetView[V3]) = (
      new Unzip3LeftTreeEquaSetView[U, V1, V2, V3](thisTreeEquaSetView),
      new Unzip3MiddleTreeEquaSetView[U, V1, V2, V3](thisTreeEquaSetView),
      new Unzip3RightTreeEquaSetView[U, V1, V2, V3](thisTreeEquaSetView)
    )

    def zip[V](that: EquaSetView[V]): TreeEquaSetView[(U, V)] = new ZipTreeEquaSetView(thisTreeEquaSetView, that)
    def zipAll[V, U1 >: U](that: EquaSetView[V], thisElem: U1, thatElem: V): TreeEquaSetView[(U1, V)] =
      new ZipAllTreeEquaSetView(thisTreeEquaSetView, that, thisElem, thatElem)
    def zipWithIndex: TreeEquaSetView[(U, Int)] = new ZipWithIndex(thisTreeEquaSetView)

    override def toString: String = toList.mkString("TreeEquaSetView(", ",", ")")
    override def equals(other: Any): Boolean =
      other match {
        case otherTreeEquaSetView: TreeEquaSetView[_] =>
          thisTreeEquaSetView.toList == otherTreeEquaSetView.toList
        case _ => false
      }
    override def hashCode: Int = thisTreeEquaSetView.toList.hashCode
  }

  private class CollectTreeEquaSetView[T, U](lazyBag: TreeEquaSetView[T], pf: PartialFunction[T, U]) extends TransformTreeEquaSetView[T, U] {
    def toList: List[U] = lazyBag.toList.collect(pf)
  }

  private class MapTreeEquaSetView[T, U](lazySeq: TreeEquaSetView[T], f: T => U) extends TransformTreeEquaSetView[T, U] { thisTreeEquaSetView =>
    def toList: List[U] = lazySeq.toList.map(f)
  }

  private class FlatMapTreeEquaSetView[T, U](lazySeq: TreeEquaSetView[T], f: T => EquaSetView[U]) extends TransformTreeEquaSetView[T, U] { thisTreeEquaSetView =>
    def toList: List[U] = lazySeq.toList.flatMap(f.andThen(_.toList))
  }

  private class ScanTreeEquaSetView[T](lazySeq: TreeEquaSetView[T], z: T, op: (T, T) ⇒ T) extends TransformTreeEquaSetView[T, T] {
    def toList: List[T] = lazySeq.toList.scan(z)(op)
  }

  private class ScanLeftTreeEquaSetView[T, U](lazySeq: TreeEquaSetView[T], z: U, op: (U, T) ⇒ U) extends TransformTreeEquaSetView[T, U] {
    def toList: List[U] = lazySeq.toList.scanLeft(z)(op)
  }

  private class ScanRightTreeEquaSetView[T, U](lazySeq: TreeEquaSetView[T], z: U, op: (T, U) ⇒ U) extends TransformTreeEquaSetView[T, U] {
    def toList: List[U] = lazySeq.toList.scanRight(z)(op)
  }

  private class ZipTreeEquaSetView[T, U](thisSeq: TreeEquaSetView[T], that: EquaSetView[U]) extends TransformTreeEquaSetView[T, (T, U)] {
    def toList: List[(T, U)] = thisSeq.toList.zip(that.toList)
  }

  private class ZipAllTreeEquaSetView[T, U](thisSeq: TreeEquaSetView[T], thatBag: EquaSetView[U], thisElem: T, thatElem: U) extends TransformTreeEquaSetView[T, (T, U)] {
    def toList: List[(T, U)] = thisSeq.toList.zipAll(thatBag.toList, thisElem, thatElem)
  }

  private class ZipWithIndex[T, U](thisSeq: TreeEquaSetView[T]) extends TransformTreeEquaSetView[T, (T, Int)] {
    def toList: List[(T, Int)] = thisSeq.toList.zipWithIndex
  }

  private class UnzipLeftTreeEquaSetView[T, U1, U2](lazySeq: TreeEquaSetView[T])(implicit asPair: T => (U1, U2)) extends TransformTreeEquaSetView[T, U1] {
    def toList: List[U1] = lazySeq.toList.unzip._1.toList
  }

  private class UnzipRightTreeEquaSetView[T, U1, U2](lazySeq: TreeEquaSetView[T])(implicit asPair: T => (U1, U2)) extends TransformTreeEquaSetView[T, U2] {
    def toList: List[U2] = lazySeq.toList.unzip._2.toList
  }

  private class Unzip3LeftTreeEquaSetView[T, U1, U2, U3](lazySeq: TreeEquaSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformTreeEquaSetView[T, U1] {
    def toList: List[U1] = lazySeq.toList.unzip3._1.toList
  }

  private class Unzip3MiddleTreeEquaSetView[T, U1, U2, U3](lazySeq: TreeEquaSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformTreeEquaSetView[T, U2] {
    def toList: List[U2] = lazySeq.toList.unzip3._2.toList
  }

  private class Unzip3RightTreeEquaSetView[T, U1, U2, U3](lazySeq: TreeEquaSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformTreeEquaSetView[T, U3] {
    def toList: List[U3] = lazySeq.toList.unzip3._3.toList
  }

  def apply[T](args: T*): TreeEquaSetView[T] = new BasicTreeEquaSetView(args.toList)
}
