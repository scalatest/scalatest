package org.scalautils

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.GenTraversableOnce
import scala.reflect.ClassTag
import scala.collection.mutable.Buffer
import scala.collection.GenSeq
import scala.collection.GenIterable
import scala.collection.generic.CanBuildFrom
import Every.fromNonEmptyVector
import scala.annotation.unchecked.{ uncheckedVariance => uV }

// Can't be an IndexedSeq[T] because Builder would be able to create an empty one.
sealed abstract class Every[+T] protected (underlying: Vector[T]) extends PartialFunction[Int, T] {
  def ++[U >: T](other: Every[U]): Many[U]
  def ++[U >: T](other: GenTraversableOnce[U]): Every[U]
  final def /:[B](z: B)(op: (B, T) => B): B = underlying./:(z)(op)
  final def :\[B](z: B)(op: (T, B) => B): B = underlying.:\(z)(op)
  final def +:[U >: T](element: U): Many[U] = Many(element, underlying.head, underlying.tail: _*)
  def :+[U >: T](element: U): Many[U]
  final def addString(sb: StringBuilder): StringBuilder = underlying.addString(sb)
  final def addString(sb: StringBuilder, sep: String): StringBuilder = underlying.addString(sb, sep)
  final def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.addString(sb, start, sep, end)
  final def apply(idx: Int): T = underlying(idx)
  final def collectFirst[U](pf: PartialFunction[T, U]): Option[U] = underlying.collectFirst(pf)
  final def contains(elem: Any): Boolean = underlying.contains(elem)
  final def containsSlice[B](that: GenSeq[B]): Boolean = underlying.containsSlice(that)
  final def containsSlice[B](that: Every[B]): Boolean = underlying.containsSlice(that.toVector)
  final def copyToArray[U >: T](arr: Array[U]): Unit = underlying.copyToArray(arr)
  final def copyToArray[U >: T](arr: Array[U], start: Int): Unit = underlying.copyToArray(arr, start)
  final def copyToArray[U >: T](arr: Array[U], start: Int, len: Int): Unit = underlying.copyToArray(arr, start, len)
  final def copyToBuffer[U >: T](buf: Buffer[U]): Unit = underlying.copyToBuffer(buf)
  final def corresponds[B](that: GenSeq[B])(p: (T, B) => Boolean): Boolean = underlying.corresponds(that)(p)
  final def corresponds[B](that: Every[B])(p: (T, B) => Boolean): Boolean = underlying.corresponds(that.toVector)(p)
  final def count(p: T => Boolean): Int = underlying.count(p)
  final def distinct: Every[T] = {
    val eles = underlying.distinct
    val head = eles.head
    val tail = eles.tail
    if (tail.isEmpty) One(head) else Many(head, tail.head, tail.tail: _*)
  }
  final def endsWith[B](that: GenSeq[B]): Boolean = underlying.endsWith(that)
  final def endsWith[B](that: Every[B]): Boolean = underlying.endsWith(that.toVector)
  final def exists(p: T => Boolean): Boolean = underlying.exists(p)
  final def find(p: T => Boolean): Option[T] = underlying.find(p)
  final def flatMap[U](f: T => Every[U]): Every[U] = {
    val buf = new ArrayBuffer[U]
    for (ele <- underlying)
      buf ++= f(ele).toVector
    val vec = buf.toVector
    Every(vec.head, vec.tail: _*)
  }
  final def flatten[B](implicit ev: T <:< Every[B]): Every[B] = flatMap(ev)
  final def fold[U >: T](z: U)(op: (U, U) => U): U = underlying.fold(z)(op)
  final def foldLeft[B](z: B)(op: (B, T) => B): B = underlying.foldLeft(z)(op)
  final def foldRight[B](z: B)(op: (T, B) => B): B = underlying.foldRight(z)(op)
  final def forall(p: T => Boolean): Boolean = underlying.forall(p)
  final def foreach(f: T => Unit): Unit = underlying.foreach(f)
  final def groupBy[K](f: T => K): Map[K, Every[T]] = {
    val mapKToVec = underlying.groupBy(f)
    mapKToVec.mapValues { vec => Every(vec.head, vec.tail: _*) }
  }
  final def grouped(size: Int): Iterator[Every[T]] = {
    val itOfVec = underlying.grouped(size)
    itOfVec.map { vec => Every(vec.head, vec.tail: _*) }
  }
  final def hasDefiniteSize: Boolean = true
  final def head: T = underlying.head
  final def headOption: Option[T] = underlying.headOption
  final def indexOf[U >: T](elem: U): Int = underlying.indexOf(elem)
  final def indexOf[U >: T](elem: U, from: Int): Int = underlying.indexOf(elem, from)
  final def indexOfSlice[U >: T](that: GenSeq[U]): Int = underlying.indexOfSlice(that)
  final def indexOfSlice[U >: T](that: GenSeq[U], from: Int): Int = underlying.indexOfSlice(that, from)
  final def indexOfSlice[U >: T](that: Every[U]): Int = underlying.indexOfSlice(that.toVector)
  final def indexOfSlice[U >: T](that: Every[U], from: Int): Int = underlying.indexOfSlice(that.toVector, from)
  final def indexWhere(p: T => Boolean): Int = underlying.indexWhere(p)
  final def indexWhere(p: T => Boolean, from: Int): Int = underlying.indexWhere(p, from)
  final def indices: Range = underlying.indices
  final def isDefinedAt(idx: Int): Boolean = underlying.isDefinedAt(idx)
  final def isEmpty: Boolean = false
  final def isTraversableAgain: Boolean = true
  final def iterator: Iterator[T] = underlying.iterator
  final def last: T = underlying.last
  final def lastIndexOf[U >: T](elem: U): Int = underlying.lastIndexOf(elem)
  final def lastIndexOf[U >: T](elem: U, end: Int): Int = underlying.lastIndexOf(elem, end)
  final def lastIndexOfSlice[U >: T](that: GenSeq[U]): Int = underlying.lastIndexOfSlice(that)
  final def lastIndexOfSlice[U >: T](that: GenSeq[U], end: Int): Int = underlying.lastIndexOfSlice(that, end)
  final def lastIndexOfSlice[U >: T](that: Every[U]): Int = underlying.lastIndexOfSlice(that.toVector)
  final def lastIndexOfSlice[U >: T](that: Every[U], end: Int): Int = underlying.lastIndexOfSlice(that.toVector, end)
  final def lastIndexWhere(p: T => Boolean): Int = underlying.lastIndexWhere(p)
  final def lastIndexWhere(p: T => Boolean, end: Int): Int = underlying.lastIndexWhere(p, end)
  final def lastOption: Option[T] = underlying.lastOption // Will always return a Some
  final def length: Int = underlying.length
  final def lengthCompare(len: Int): Int = underlying.lengthCompare(len)
  final def map[U](f: T => U): Every[U] = {
    val vec = underlying.map(f)
    Every(vec.head, vec.tail: _*)
  }
  final def max[U >: T](implicit cmp: Ordering[U]): T = underlying.max(cmp)
  final def maxBy[U](f: T => U)(implicit cmp: Ordering[U]): T = underlying.maxBy(f)(cmp)
  final def min[U >: T](implicit cmp: Ordering[U]): T = underlying.min(cmp)
  final def minBy[U](f: T => U)(implicit cmp: Ordering[U]): T = underlying.minBy(f)(cmp)
  final def mkString: String = underlying.mkString
  final def mkString(sep: String): String = underlying.mkString(sep)
  final def mkString(start: String, sep: String, end: String): String = underlying.mkString(start, sep, end)
  final def nonEmpty: Boolean = true
  final def padTo[U >: T](len: Int, elem: U): Every[U] = {
    val vec = underlying.padTo(len, elem)
    Every(vec.head, vec.tail: _*)
  }
  final def patch[U >: T](from: Int, that: Every[U], replaced: Int): Every[U] = {
    val vec = underlying.patch(from, that.toVector, replaced)
    Every(vec.head, vec.tail: _*)
  }
  final def permutations: Iterator[Every[T]] = {
    val it = underlying.permutations
    it map { v => Every(v.head, v.tail: _*) }
  }
  final def prefixLength(p: T => Boolean): Int = underlying.prefixLength(p)
  final def product[U >: T](implicit num: Numeric[U]): U = underlying.product(num)
  final def reduce[U >: T](op: (U, U) => U): U = underlying.reduce(op)
  final def reduceLeft[U >: T](op: (U, T) => U): U = underlying.reduceLeft(op)
  final def reduceLeftOption[U >: T](op: (U, T) => U): Option[U] = underlying.reduceLeftOption(op)
  final def reduceOption[U >: T](op: (U, U) => U): Option[U] = underlying.reduceOption(op)
  final def reduceRight[U >: T](op: (T, U) => U): U = underlying.reduceRight(op)
  final def reduceRightOption[U >: T](op: (T, U) => U): Option[U] = underlying.reduceRightOption(op)
  final def reverse: Every[T] = {
    val vec = underlying.reverse
    Every(vec.head, vec.tail: _*)
  }
  final def reverseIterator: Iterator[T] = underlying.reverseIterator
  final def reverseMap[U](f: T => U): Every[U] = {
    val vec = underlying.reverseMap(f)
    Every(vec.head, vec.tail: _*)
  }
  final def sameElements[U >: T](that: GenIterable[U]): Boolean = underlying.sameElements(that)
  final def sameElements[U >: T](that: Every[U]): Boolean = underlying.sameElements(that.toVector)
  final def segmentLength(p: T => Boolean, from: Int): Int = underlying.segmentLength(p, from)
  final def sliding(size: Int): Iterator[Every[T]] = underlying.sliding(size).map(fromNonEmptyVector(_))
  final def sliding(size: Int, step: Int): Iterator[Every[T]] = underlying.sliding(size, step).map(fromNonEmptyVector(_))
  final def size: Int = underlying.size
  final def sortBy[U](f: T => U)(implicit ord: math.Ordering[U]): Every[T] = fromNonEmptyVector(underlying.sortBy(f))
  final def sortWith(lt: (T, T) => Boolean): Every[T] = fromNonEmptyVector(underlying.sortWith(lt))
  final def sorted[U >: T](implicit ord: math.Ordering[U]): Every[U] = fromNonEmptyVector(underlying.sorted(ord))
  final def startsWith[B](that: GenSeq[B]): Boolean = underlying.startsWith(that)
  final def startsWith[B](that: GenSeq[B], offset: Int): Boolean = underlying.startsWith(that, offset)
  final def startsWith[B](that: Every[B]): Boolean = underlying.startsWith(that.toVector)
  final def startsWith[B](that: Every[B], offset: Int): Boolean = underlying.startsWith(that.toVector, offset)
  def stringPrefix: String
  final def sum[U >: T](implicit num: Numeric[U]): U = underlying.sum(num)
  final def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, T, Col[T @uV]]): Col[T @uV] = underlying.to[Col](cbf)
  final def toArray[U >: T](implicit classTag: ClassTag[U]): Array[U] = underlying.toArray
  final def toVector: Vector[T] = underlying
  final def toBuffer[U >: T]: Buffer[U] = underlying.toBuffer
  final def toIndexedSeq: IndexedSeq[T] = underlying.toIndexedSeq
  final def toIterable: Iterable[T] = underlying.toIterable
  final def toIterator: Iterator[T] = underlying.toIterator
  final def toList: List[T] = underlying.toList
  final def toMap[K, V](implicit ev: T <:< (K, V)): Map[K, V] = underlying.toMap
  final def toSeq: Seq[T] = underlying.toSeq
  final def toSet[U >: T]: Set[U] = underlying.toSet
  final def toStream: Stream[T] = underlying.toStream
  final def toTraversable: Traversable[T] = underlying.toTraversable
  final def transpose[U](implicit asTraversable: T => GenTraversableOnce[U]): Every[Every[U]] = {
    val asVecs = underlying.map(asTraversable)
    val vec = asVecs.transpose
    fromNonEmptyVector(vec map fromNonEmptyVector)
  }
  final def union[U >: T](that: Every[U]): Every[U] = fromNonEmptyVector(underlying union that.toVector)
  final def union[U >: T](that: GenSeq[U])(implicit cbf: CanBuildFrom[Vector[T], U, Vector[U]]): Every[U] = fromNonEmptyVector(underlying.union(that)(cbf))
  final def unzip[L, R](implicit asPair: T => (L, R)): (Every[L], Every[R]) = {
    val unzipped = underlying.unzip
    (fromNonEmptyVector(unzipped._1), fromNonEmptyVector(unzipped._2))
  }
  final def unzip3[L, M, R](implicit asTriple: T => (L, M, R)): (Every[L], Every[M], Every[R]) = {
    val unzipped = underlying.unzip3
    (fromNonEmptyVector(unzipped._1), fromNonEmptyVector(unzipped._2), fromNonEmptyVector(unzipped._3))
  }
  final def updated[U >: T](index: Int, elem: U): Every[U] = fromNonEmptyVector(underlying.updated(index, elem))
  final def zipAll[O, U >: T](other: collection.Iterable[O], thisElem: U, otherElem: O): Every[(U, O)] =
    Every.from(underlying.zipAll(other, thisElem, otherElem)).get
  final def zipWithIndex: Every[(T, Int)] = fromNonEmptyVector(underlying.zipWithIndex)
}

object Every {
  def apply[T](firstElement: T, otherElements: T*): Every[T] = 
    if (otherElements.isEmpty) One(firstElement) else Many(firstElement, otherElements.head, otherElements.tail: _*)
  def unapplySeq[T](every: Every[T]): Option[Seq[T]] = Some(every.toVector)
  def from[T](seq: GenSeq[T]): Option[Every[T]] =
    seq.headOption match {
      case None => None
      case Some(first) =>
        seq.tail.headOption match {
          case None => Some(One(first))
          case Some(second) => Some(Many(first, second, seq.tail.tail.seq: _*)) 
        }
    }
  // Can be flattened: Vector(Every(1, 2, 3), Every(1, 2, 3)).flatten shouldBe Vector(1, 2, 3, 1, 2, 3)
  implicit def everyToGenTraversableOnce[E](every: Every[E]): Vector[E] = every.toVector
  private def fromNonEmptyVector[E](vec: Vector[E]): Every[E] = Every(vec.head, vec.tail: _*)
}

final case class One[+T](loneElement: T) extends Every[T](Vector(loneElement)) {
  def ++[U >: T](other: Every[U]): Many[U] = Many(loneElement, other.toVector.head, other.toVector.tail: _*)
  def ++[U >: T](other: GenTraversableOnce[U]): Every[U] =
    if (other.isEmpty) this else Many(loneElement, other.toVector.head, other.toVector.tail: _*)
  def :+[U >: T](element: U): Many[U] = Many(loneElement, element)
  def stringPrefix: String = "One"
  override def toString: String = "One(" + loneElement + ")"
}

final case class Many[+T](firstElement: T, secondElement: T, otherElements: T*) extends Every[T](firstElement +: secondElement +: Vector(otherElements: _*)) {
  def ++[U >: T](other: Every[U]): Many[U] = Many(firstElement, secondElement, (otherElements.toVector ++ other.toVector): _*)
  def ++[U >: T](other: GenTraversableOnce[U]): Every[U] =
    if (other.isEmpty) this else Many(firstElement, secondElement, otherElements ++ other.toVector: _*)
  def :+[U >: T](element: U): Many[U] = Many(firstElement, secondElement, (otherElements :+ element): _*)
  def stringPrefix: String = "Many"
  override def toString: String = "Many(" + toVector.mkString(", ") + ")"
}



