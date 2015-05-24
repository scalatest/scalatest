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

trait TreeSetView[+T] extends SortedSetView[T] {
  def collect[U](pf: PartialFunction[T, U]): TreeSetView[U]

  def map[U](f: T => U): TreeSetView[U]
  def flatMap[U](f: T => SetView[U]): TreeSetView[U]
  def force[U >: T](toPath: Collections[U]): toPath.immutable.Set[U]
  def toSet[U >: T](toPath: Collections[U]): toPath.immutable.Set[U]
  def force[U >: T](toPath: SortedCollections[U]): toPath.immutable.TreeSet[U]
  def toSortedSet[U >: T](toPath: SortedCollections[U]): toPath.immutable.SortedSet[U]
  def toList: List[T]

  def scan[U >: T](z: U)(op: (U, U) ⇒ U): TreeSetView[U]
  def scanLeft[U](z: U)(op: (U, T) => U): TreeSetView[U]
  def scanRight[U](z: U)(op: (T, U) => U): TreeSetView[U]

  def size: Int
  def unzip[U1, U2](implicit asPair: T => (U1, U2)): (TreeSetView[U1], TreeSetView[U2])
  def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (TreeSetView[U1], TreeSetView[U2], TreeSetView[U3])
  def zip[U](that: SetView[U]): TreeSetView[(T, U)]
  def zipAll[U, T1 >: T](that: SetView[U], thisElem: T1, thatElem: U): TreeSetView[(T1, U)]
  def zipWithIndex: TreeSetView[(T, Int)]
}

object TreeSetView {
  private class BasicTreeSetView[T](private val args: List[T]) extends TreeSetView[T] { thisTreeSetView =>
    def collect[U](pf: PartialFunction[T, U]): TreeSetView[U] = new CollectTreeSetView(thisTreeSetView, pf)
    def map[U](f: T => U): TreeSetView[U] = new MapTreeSetView(thisTreeSetView, f)
    def flatMap[U](f: T => SetView[U]): TreeSetView[U] = new FlatMapTreeSetView(thisTreeSetView, f)
    def toSet[U >: T](toPath: Collections[U]): toPath.immutable.FastSet[U] = force(toPath)
    def force[U >: T](toPath: Collections[U]): toPath.immutable.FastSet[U] = toPath.immutable.FastSet(args: _*)
    def toSortedSet[U >: T](toPath: SortedCollections[U]): toPath.immutable.SortedSet[U] = force(toPath)
    def force[U >: T](toPath: SortedCollections[U]): toPath.immutable.TreeSet[U] = toPath.immutable.TreeSet(args: _*)
    def toList: List[T] = args

    def scan[U >: T](z: U)(op: (U, U) ⇒ U): TreeSetView[U] = new ScanTreeSetView(thisTreeSetView, z, op)
    def scanLeft[U](z: U)(op: (U, T) => U): TreeSetView[U] = new ScanLeftTreeSetView(thisTreeSetView, z, op)
    def scanRight[U](z: U)(op: (T, U) => U): TreeSetView[U] = new ScanRightTreeSetView(thisTreeSetView, z, op)

    def size: Int = args.size

    def unzip[U1, U2](implicit asPair: T => (U1, U2)): (TreeSetView[U1], TreeSetView[U2]) = (
      new UnzipLeftTreeSetView[T, U1, U2](thisTreeSetView)(asPair),
      new UnzipRightTreeSetView[T, U1, U2](thisTreeSetView)(asPair)
    )

    def unzip3[U1, U2, U3](implicit asTriple: T => (U1, U2, U3)): (TreeSetView[U1], TreeSetView[U2], TreeSetView[U3]) = (
      new Unzip3LeftTreeSetView[T, U1, U2, U3](thisTreeSetView),
      new Unzip3MiddleTreeSetView[T, U1, U2, U3](thisTreeSetView),
      new Unzip3RightTreeSetView[T, U1, U2, U3](thisTreeSetView)
    )

    def zip[U](that: SetView[U]): TreeSetView[(T, U)] = new ZipTreeSetView(thisTreeSetView, that)
    def zipAll[U, T1 >: T](that: SetView[U], thisElem: T1, thatElem: U): TreeSetView[(T1, U)] =
      new ZipAllTreeSetView(thisTreeSetView, that, thisElem, thatElem)
    def zipWithIndex: TreeSetView[(T, Int)] = new ZipWithIndex(thisTreeSetView)

    override def toString = args.mkString("TreeSetView(", ",", ")")
    override def equals(other: Any): Boolean = ???
    override def hashCode: Int = ???
  }

  private abstract class TransformTreeSetView[T, U] extends TreeSetView[U] { thisTreeSetView =>
    def collect[V](pf: PartialFunction[U, V]): TreeSetView[V] = new CollectTreeSetView(thisTreeSetView, pf)
    def map[V](g: U => V): TreeSetView[V] = new MapTreeSetView[U, V](thisTreeSetView, g)
    def flatMap[V](f: U => SetView[V]): TreeSetView[V] = new FlatMapTreeSetView(thisTreeSetView, f)
    def force[V >: U](toPath: Collections[V]): toPath.immutable.FastSet[V] = {
      toPath.immutable.FastSet[V](toList: _*)
    }
    def toSet[V >: U](toPath: Collections[V]): toPath.immutable.FastSet[V] = force(toPath)
    def force[V >: U](toPath: SortedCollections[V]): toPath.immutable.TreeSet[V] = {
      toPath.immutable.TreeSet[V](toList: _*)
    }
    def toSortedSet[V >: U](toPath: SortedCollections[V]): toPath.immutable.SortedSet[V] = force(toPath)
    def toList: List[U]

    def scan[V >: U](z: V)(op: (V, V) ⇒ V): TreeSetView[V] = new ScanTreeSetView(thisTreeSetView, z, op)
    def scanLeft[V](z: V)(op: (V, U) => V): TreeSetView[V] = new ScanLeftTreeSetView(thisTreeSetView, z, op)
    def scanRight[V](z: V)(op: (U, V) => V): TreeSetView[V] = new ScanRightTreeSetView(thisTreeSetView, z, op)

    def size: Int = toList.size

    def unzip[V1, V2](implicit asPair: U => (V1, V2)): (TreeSetView[V1], TreeSetView[V2]) = (
      new UnzipLeftTreeSetView[U, V1, V2](thisTreeSetView)(asPair),
      new UnzipRightTreeSetView[U, V1, V2](thisTreeSetView)(asPair)
    )

    def unzip3[V1, V2, V3](implicit asTriple: U => (V1, V2, V3)): (TreeSetView[V1], TreeSetView[V2], TreeSetView[V3]) = (
      new Unzip3LeftTreeSetView[U, V1, V2, V3](thisTreeSetView),
      new Unzip3MiddleTreeSetView[U, V1, V2, V3](thisTreeSetView),
      new Unzip3RightTreeSetView[U, V1, V2, V3](thisTreeSetView)
    )

    def zip[V](that: SetView[V]): TreeSetView[(U, V)] = new ZipTreeSetView(thisTreeSetView, that)
    def zipAll[V, U1 >: U](that: SetView[V], thisElem: U1, thatElem: V): TreeSetView[(U1, V)] =
      new ZipAllTreeSetView(thisTreeSetView, that, thisElem, thatElem)
    def zipWithIndex: TreeSetView[(U, Int)] = new ZipWithIndex(thisTreeSetView)

    override def toString: String = toList.mkString("TreeSetView(", ",", ")")
    override def equals(other: Any): Boolean =
      other match {
        case otherTreeSetView: TreeSetView[_] =>
          thisTreeSetView.toList == otherTreeSetView.toList
        case _ => false
      }
    override def hashCode: Int = thisTreeSetView.toList.hashCode
  }

  private class CollectTreeSetView[T, U](lazyBag: TreeSetView[T], pf: PartialFunction[T, U]) extends TransformTreeSetView[T, U] {
    def toList: List[U] = lazyBag.toList.collect(pf)
  }

  private class MapTreeSetView[T, U](lazySeq: TreeSetView[T], f: T => U) extends TransformTreeSetView[T, U] { thisTreeSetView =>
    def toList: List[U] = lazySeq.toList.map(f)
  }

  private class FlatMapTreeSetView[T, U](lazySeq: TreeSetView[T], f: T => SetView[U]) extends TransformTreeSetView[T, U] { thisTreeSetView =>
    def toList: List[U] = lazySeq.toList.flatMap(f.andThen(_.toList))
  }

  private class ScanTreeSetView[T](lazySeq: TreeSetView[T], z: T, op: (T, T) ⇒ T) extends TransformTreeSetView[T, T] {
    def toList: List[T] = lazySeq.toList.scan(z)(op)
  }

  private class ScanLeftTreeSetView[T, U](lazySeq: TreeSetView[T], z: U, op: (U, T) ⇒ U) extends TransformTreeSetView[T, U] {
    def toList: List[U] = lazySeq.toList.scanLeft(z)(op)
  }

  private class ScanRightTreeSetView[T, U](lazySeq: TreeSetView[T], z: U, op: (T, U) ⇒ U) extends TransformTreeSetView[T, U] {
    def toList: List[U] = lazySeq.toList.scanRight(z)(op)
  }

  private class ZipTreeSetView[T, U](thisSeq: TreeSetView[T], that: SetView[U]) extends TransformTreeSetView[T, (T, U)] {
    def toList: List[(T, U)] = thisSeq.toList.zip(that.toList)
  }

  private class ZipAllTreeSetView[T, U](thisSeq: TreeSetView[T], thatBag: SetView[U], thisElem: T, thatElem: U) extends TransformTreeSetView[T, (T, U)] {
    def toList: List[(T, U)] = thisSeq.toList.zipAll(thatBag.toList, thisElem, thatElem)
  }

  private class ZipWithIndex[T, U](thisSeq: TreeSetView[T]) extends TransformTreeSetView[T, (T, Int)] {
    def toList: List[(T, Int)] = thisSeq.toList.zipWithIndex
  }

  private class UnzipLeftTreeSetView[T, U1, U2](lazySeq: TreeSetView[T])(implicit asPair: T => (U1, U2)) extends TransformTreeSetView[T, U1] {
    def toList: List[U1] = lazySeq.toList.unzip._1.toList
  }

  private class UnzipRightTreeSetView[T, U1, U2](lazySeq: TreeSetView[T])(implicit asPair: T => (U1, U2)) extends TransformTreeSetView[T, U2] {
    def toList: List[U2] = lazySeq.toList.unzip._2.toList
  }

  private class Unzip3LeftTreeSetView[T, U1, U2, U3](lazySeq: TreeSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformTreeSetView[T, U1] {
    def toList: List[U1] = lazySeq.toList.unzip3._1.toList
  }

  private class Unzip3MiddleTreeSetView[T, U1, U2, U3](lazySeq: TreeSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformTreeSetView[T, U2] {
    def toList: List[U2] = lazySeq.toList.unzip3._2.toList
  }

  private class Unzip3RightTreeSetView[T, U1, U2, U3](lazySeq: TreeSetView[T])(implicit asTriple: T => (U1, U2, U3)) extends TransformTreeSetView[T, U3] {
    def toList: List[U3] = lazySeq.toList.unzip3._3.toList
  }

  def apply[T](args: T*): TreeSetView[T] = new BasicTreeSetView(args.toList)
}
