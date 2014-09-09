/*
 * Copyright 2001-2013 Artima, Inc.
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

import scala.collection.{GenMap, mutable, GenTraversableOnce}
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet

class SortedEquaSets[T](override val equality: OrderingEquality[T]) extends EquaSets[T](equality) { thisEquaSets =>

  val ordering: Ordering[thisEquaSets.EquaBox] =
    new Ordering[thisEquaSets.EquaBox] {
      def compare(a: thisEquaSets.EquaBox, b: thisEquaSets.EquaBox): Int =
        equality.compare(a.value, b.value)
    }

  class SortedEquaBridge[S](from: List[S]) extends EquaBridge[S](from) {
    override def collect(pf: PartialFunction[S, T]): thisEquaSets.SortedEquaSet =
      thisEquaSets.SortedEquaSet.empty ++ (from collect pf)
  }

  trait SortedEquaSet extends EquaSet {

    /**
     * Creates a new `SortedEquaSet` with an additional element, unless the element is
     * already present.
     *
     * @param elem the element to be added
     * @return a new `SortedEquaSet` that contains all elements of this `SortedEquaSet` and that also
     * contains `elem`.
     */
    def + (elem: T): thisEquaSets.SortedEquaSet

    /**
     * Creates a new `SortedEquaSet` with additional elements.
     *
     * This method takes two or more elements to be added. Another overloaded
     * variant of this method handles the case where a single element is added.
     *
     * @param elem1 the first element to add.
     * @param elem2 the second element to add.
     * @param elems the remaining elements to add.
     * @return a new `SortedEquaSet` with the given elements added.
     */
    def + (elem1: T, elem2: T, elems: T*): thisEquaSets.SortedEquaSet

    /** Creates a new `SortedEquaSet` by adding all elements contained in another collection to this `SortedEquaSet`.
      *
      *  @param elems     the collection containing the added elements.
      *  @return          a new `SortedEquaSet` with the given elements added.
      */
    def ++ (elems: GenTraversableOnce[T]): thisEquaSets.SortedEquaSet

    /**
     * Creates a new `SortedEquaSet` by adding elements contained in another `EquaSet`.
     *
     * @param that     the other `EquaSet` containing the added elements.
     * @return         a new `SortedEquaSet` with the given elements added.
     */
    def ++ (that: EquaSet): thisEquaSets.SortedEquaSet

    /**
     * Creates a new `SortedEquaSet` with a given element removed from this `SortedEquaSet`.
     *
     * @param elem the element to be removed
     * @return a new `SortedEquaSet` that contains all elements of this `SortedEquaSet` but that does not
     * contain `elem`.
     */
    def - (elem: T): thisEquaSets.SortedEquaSet

    /* * USE LATER
     * Creates a new `SortedEquaSet` from this `SortedEquaSet` by removing all elements of another
     * collection.
     *
     * @param xs the collection containing the removed elements.
     * @return a new `SortedEquaSet` that contains all elements of the current `SortedEquaSet`
     * except one less occurrence of each of the elements of `elems`.
     */

    /**
     * Creates a new `SortedEquaSet` from this `SortedEquaSet` with some elements removed.
     *
     * This method takes two or more elements to be removed. Another overloaded
     * variant of this method handles the case where a single element is
     * removed.
     * @param elem1 the first element to remove.
     * @param elem2 the second element to remove.
     * @param elems the remaining elements to remove.
     * @return a new `SortedEquaSet` that contains all elements of the current `SortedEquaSet`
     * except one less occurrence of each of the given elements.
     */
    def - (elem1: T, elem2: T, elems: T*): thisEquaSets.SortedEquaSet

    /**
     * Creates a new `SortedEquaSet` from this $coll by removing all elements of another
     *  collection.
     *
     *  @param elems     the collection containing the removed elements.
     *  @return a new `SortedEquaSet` that contains all elements of the current `SortedEquaSet`
     *  except one less occurrence of each of the elements of `elems`.
     */
    def --(elems: GenTraversableOnce[T]): thisEquaSets.EquaSet

    /**
     * Creates a new `SortedEquaSet` from this `SortedEquaSet` by removing all elements of another `EquaSet`
     *
     * @param that       the other `EquaSet` containing the removed elements.
     * @return a new `SortedEquaSet` that contains all elements of the current `EquaSet` minus elements contained in the passed in `EquaSet`.
     */
    def --(that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

    /**
     * Applies a binary operator to a start value and all elements of this `SortedEquaSet`,
     *  going left to right.
     *
     *  Note: `/:` is alternate syntax for `foldLeft`; `z /: xs` is the same as
     *  `xs foldLeft z`.
     *
     *  Examples:
     *
     *  Note that the folding function used to compute b is equivalent to that used to compute c.
     *  {{{
     *      scala> val a = List(1,2,3,4)
     *      a: List[Int] = List(1, 2, 3, 4)
     *
     *      scala> val b = (5 /: a)(_+_)
     *      b: Int = 15
     *
     *      scala> val c = (5 /: a)((x,y) => x + y)
     *      c: Int = 15
     *  }}}
     *
     *  $willNotTerminateInf
     *  $orderDependentFold
     *
     *  @param   z    the start value.
     *  @param   op   the binary operator.
     *  @tparam  B    the result type of the binary operator.
     *  @return  the result of inserting `op` between consecutive elements of this $coll,
     *           going left to right with the start value `z` on the left:
     *           {{{
     *             op(...op(op(z, x_1), x_2), ..., x_n)
     *           }}}
     *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
     */
    def /:[B](z: B)(op: (B, T) => B): B

    /**
     * Applies a binary operator to all elements of this `SortedEquaSet` and a start value,
     *  going right to left.
     *
     *  Note: `:\` is alternate syntax for `foldRight`; `xs :\ z` is the same as
     *  `xs foldRight z`.
     *  $willNotTerminateInf
     *  $orderDependentFold
     *
     *  Examples:
     *
     *  Note that the folding function used to compute b is equivalent to that used to compute c.
     *  {{{
     *      scala> val a = List(1,2,3,4)
     *      a: List[Int] = List(1, 2, 3, 4)
     *
     *      scala> val b = (a :\ 5)(_+_)
     *      b: Int = 15
     *
     *      scala> val c = (a :\ 5)((x,y) => x + y)
     *      c: Int = 15
     *
     *  }}}
     *
     *  @param   z    the start value
     *  @param   op   the binary operator
     *  @tparam  B    the result type of the binary operator.
     *  @return  the result of inserting `op` between consecutive elements of this $coll,
     *           going right to left with the start value `z` on the right:
     *           {{{
     *             op(x_1, op(x_2, ... op(x_n, z)...))
     *           }}}
     *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
     */
    def :\[B](z: B)(op: (T, B) => B): B

    /**
     * Computes the union between this `SortedEquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `union`.
     * @param that the `EquaSet` to form the union with.
     * @return a new `SortedEquaSet` consisting of all elements that are in this
     * `SortedEquaSet` or in the given `EquaSet` `that`.
     */
    def | (that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

    /**
     * Computes the intersection between this `SortedEquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `intersect`.
     * @param that the `EquaSet` to intersect with.
     * @return a new `SortedEquaSet` consisting of all elements that are both in this
     * `SortedEquaSet` and in the given `EquaSet` `that`.
     */
    def & (that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

    /**
     * The difference of this `SortedEquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `diff`.
     * @param that the `EquaSet` of elements to exclude.
     * @return a `SortedEquaSet` containing those elements of this
     * `SortedEquaSet` that are not also contained in the given `EquaSet` `that`.
     */
    def &~ (that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

    /**
     * Builds a new collection by applying a partial function to all elements of this `SortedEquaSet`
     * on which the function is defined.
     *
     * @param pf the partial function which filters and maps the `SortedEquaSet`.
     * @return a new collection of type `That` resulting from applying the partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     *
     * @return a new `SortedEquaSet` resulting from applying the given partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     */
    def collect(pf: PartialFunction[T, T]): thisEquaSets.SortedEquaSet

    def collectInto[U](thatEquaSets: EquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.EquaSet
    def collectInto[U](thatEquaSets: SortedEquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.SortedEquaSet

    /**
     * Computes the difference of this `SortedEquaSet` and another `SortedEquaSet`.
     *
     * @param that the `EquaSet` of elements to exclude.
     * @return a `SortedEquaSet` containing those elements of this
     * `SortedEquaSet` that are not also contained in the given `EquaSet` `that`.
     */
    def diff(that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

    /**
     * Selects all elements except first ''n'' ones.
     *
     * @param n the number of elements to drop from this `SortedEquaSet`.
     * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet` except the first `n` ones, or else the
     * empty `SortedEquaSet`, if this `EquaSet` has less than `n` elements.
     */
    def drop(n: Int): thisEquaSets.SortedEquaSet

    /** Selects all elements except last ''n'' ones.
      *
      * @param n The number of elements to take
      * @return a `SortedEquiSet` consisting of all elements of this `SortedEquiSet` except the last `n` ones, or else the
      * empty `SortedEquiSet`, if this `SortedEquiSet` has less than `n` elements.
      */
    def dropRight(n: Int): thisEquaSets.SortedEquaSet

    /**
     * Drops longest prefix of elements that satisfy a predicate.
     *
     * @param pred The predicate used to test elements.
     * @return the longest suffix of this `SortedEquiSet` whose first element
     * does not satisfy the predicate `p`.
     */
    def dropWhile(pred: T => Boolean): thisEquaSets.EquaSet

    /** Selects all elements of this `SortedEquaSet` which satisfy a predicate.
      *
      * @param pred the predicate used to test elements.
      * @return a new `SortedEquaSet` consisting of all elements of this `SortedEquaSet` that satisfy the given
      * predicate <code>pred</code>.
      */
    def filter(pred: T => Boolean): thisEquaSets.SortedEquaSet

    /** Selects all elements of this `SortedEquaSets` which do not satisfy a predicate.
      *
      * @param pred the predicate used to test elements.
      * @return a new `SortedEquaSets` consisting of all elements of this `SortedEquaSets` that do not satisfy the given
      * predicate <code>pred</code>.
      */
    def filterNot(pred: T => Boolean): thisEquaSets.SortedEquaSet

    /**
     * Partitions this $coll into a map of `SortedEquaSet`s according to some discriminator function.
     *
     * Note: this method is not re-implemented by views. This means
     * when applied to a view it will always force the view and
     * return a new `SortedEquaSet`.
     *
     * @param f the discriminator function.
     * @tparam K the type of keys returned by the discriminator function.
     * @return A map from keys to `SortedEquaSet`s such that the following invariant holds:
     * {{{
     * (xs groupBy f)(k) = xs filter (x => f(x) == k)
     * }}}
     * That is, every key `k` is bound to a `SortedEquaSet` of those elements `x`
     * for which `f(x)` equals `k`.
     *
     */
    def groupBy[K](f: T => K): GenMap[K, thisEquaSets.SortedEquaSet]

    /**
     * Partitions elements in fixed size `SortedEquaSet`s.
     * @see [[scala.collection.Iterator]], method `grouped`
     *
     * @param size the number of elements per group
     * @return An iterator producing `SortedEquaSet`s of size `size`, except the
     * last will be less than size `size` if the elements don't divide evenly.
     */
    def grouped(size: Int): Iterator[thisEquaSets.SortedEquaSet]

    /**
     * Computes the intersection between this `SortedEquaSet` and another `EquaSet`.
     *
     * @param that the `EquaSet` to intersect with.
     * @return a new `SortedEquaSet` consisting of all elements that are both in this
     * `SortedEquaSet` and in the given `EquaSet` `that`.
     */
    def intersect(that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet
    def into[U](thatEquaSets: EquaSets[U]): thatEquaSets.EquaBridge[T]
    def into[U](thatEquaSets: SortedEquaSets[U]): thatEquaSets.SortedEquaBridge[T]
    def isEmpty: Boolean
    def iterator: Iterator[T]
    def size: Int
    def toSet: SortedSet[thisEquaSets.EquaBox]
    def union(that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

    private[scalactic] override def owner: SortedEquaSets[T] = thisEquaSets
  }

  class TreeEquaBridge[S](from: List[S]) extends SortedEquaBridge[S](from) {
    override def collect(pf: PartialFunction[S, T]): thisEquaSets.TreeEquaSet =
      thisEquaSets.TreeEquaSet.empty ++ (from collect pf)
  }

  class TreeEquaSet private[scalactic] (private val underlying: TreeSet[EquaBox]) extends SortedEquaSet {

    def + (elem: T): thisEquaSets.TreeEquaSet = new TreeEquaSet(underlying + EquaBox(elem))
    def + (elem1: T, elem2: T, elems: T*): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying + (EquaBox(elem1), EquaBox(elem2), elems.map(EquaBox(_)): _*))
    def ++ (elems: GenTraversableOnce[T]): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying ++ elems.toSeq.map(EquaBox(_)))
    def ++ (that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet = new TreeEquaSet(underlying ++ that.toSet)
    def - (elem: T): thisEquaSets.TreeEquaSet = new TreeEquaSet(underlying - EquaBox(elem))
    def - (elem1: T, elem2: T, elems: T*): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying - (EquaBox(elem1), EquaBox(elem2), elems.map(EquaBox(_)): _*))
    def --(elems: GenTraversableOnce[T]): thisEquaSets.EquaSet =
      new TreeEquaSet(underlying -- elems.toSeq.map(EquaBox(_)))
    def --(that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet =
      new TreeEquaSet(underlying -- that.toSet)
    def /:[B](z: B)(op: (B, T) => B): B =
      underlying./:(z)((b: B, e: EquaBox) => op(b, e.value))
    def :\[B](z: B)(op: (T, B) => B): B =
      underlying.:\(z)((e: EquaBox, b: B) => op(e.value, b))
    def | (that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet = this union that
    def & (that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet = this intersect that
    def &~ (that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet = this diff that
    def addString(b: StringBuilder): StringBuilder = underlying.map(_.value).addString(b)
    def addString(b: StringBuilder, sep: String): StringBuilder = underlying.map(_.value).addString(b, sep)
    def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.map(_.value).addString(b, start, sep, end)
    def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B = underlying.aggregate(z)((b: B, e: EquaBox) => seqop(b, e.value), combop)
    def apply(elem: T): Boolean = underlying.apply(EquaBox(elem))
    def canEqual(that: Any): Boolean = that.isInstanceOf[thisEquaSets.EquaSet] && equality == that.asInstanceOf[thisEquaSets.EquaSet].owner.equality
    def collect(pf: PartialFunction[T, T]): thisEquaSets.SortedEquaSet = {
      implicit val ord: Ordering[thisEquaSets.EquaBox] = ordering
      new TreeEquaSet(underlying collect { case hb: thisEquaSets.EquaBox if pf.isDefinedAt(hb.value) => EquaBox(pf(hb.value)) })
    }
    def collectInto[U](thatEquaSets: EquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.EquaSet =
      new thatEquaSets.FastEquaSet(underlying collect { case hb: thisEquaSets.EquaBox if pf.isDefinedAt(hb.value) => thatEquaSets.EquaBox(pf(hb.value)) })
    def collectInto[U](thatEquaSets: SortedEquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.SortedEquaSet =
      new thatEquaSets.TreeEquaSet(TreeSet.empty(thatEquaSets.ordering) ++ (underlying collect { case hb: thisEquaSets.EquaBox if pf.isDefinedAt(hb.value) => thatEquaSets.EquaBox(pf(hb.value)) }))
    def copyToArray(xs: Array[thisEquaSets.EquaBox]): Unit = underlying.copyToArray(xs)
    def copyToArray(xs: Array[thisEquaSets.EquaBox], start: Int): Unit = underlying.copyToArray(xs, start)
    def copyToArray(xs: Array[thisEquaSets.EquaBox], start: Int, len: Int): Unit = underlying.copyToArray(xs, start, len)
    def copyToBuffer(dest: mutable.Buffer[thisEquaSets.EquaBox]): Unit = underlying.copyToBuffer(dest)
    def count(p: T => Boolean): Int = underlying.map(_.value).count(p)
    def diff(that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying diff that.toSet.map((eb: EquaBox) => EquaBox(eb.value)))
    def drop(n: Int): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.drop(n))
    def dropRight(n: Int): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.dropRight(n))
    def dropWhile(pred: T => Boolean): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.dropWhile((p: EquaBox) => pred(p.value)))
    override def equals(other: Any): Boolean =
      other match {
        case equiSet: thisEquaSets.EquaSet => 
          underlying == equiSet.toSet
        case _ => false
      }
    def exists(pred: T => Boolean): Boolean = underlying.exists((box: EquaBox) => pred(box.value))
    def filter(pred: T => Boolean): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.filter((box: EquaBox) => pred(box.value)))
    def filterNot(pred: T => Boolean): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.filterNot((box: EquaBox) => pred(box.value)))
    def find(pred: T => Boolean): Option[EquaBox] = underlying.find((box: EquaBox) => pred(box.value))
    def fold[T1 >: T](z: T1)(op: (T1, T1) => T1): T1 = underlying.map(_.value).fold[T1](z)(op)
    def foldLeft[B](z: B)(op: (B, T) => B): B = underlying.map(_.value).foldLeft[B](z)(op)
    def foldRight[B](z: B)(op: (T, B) => B): B = underlying.map(_.value).foldRight[B](z)(op)
    def forall(pred: T => Boolean): Boolean = underlying.map(_.value).forall(pred)
    def foreach[U](f: T => U): Unit = underlying.map(_.value).foreach(f)
    def groupBy[K](f: T => K): GenMap[K, thisEquaSets.SortedEquaSet] = underlying.groupBy((box: EquaBox) => f(box.value)).map(t => (t._1, new TreeEquaSet(t._2)))
    def grouped(size: Int): Iterator[thisEquaSets.SortedEquaSet] = underlying.grouped(size).map(new TreeEquaSet(_))
    override def hashCode: Int = underlying.hashCode
    def intersect(that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying intersect that.toSet.map((eb: EquaBox) => EquaBox(eb.value)))
    def into[U](thatEquaSets: EquaSets[U]): thatEquaSets.EquaBridge[T] = new thatEquaSets.FastEquaBridge[T](underlying.toList.map(_.value))
    def into[U](thatEquaSets: SortedEquaSets[U]): thatEquaSets.TreeEquaBridge[T] = new thatEquaSets.TreeEquaBridge[T](underlying.toList.map(_.value))
    def isEmpty: Boolean = underlying.isEmpty
    def iterator: Iterator[T] = underlying.iterator.map(_.value)
    def size: Int = underlying.size
    def toSet: TreeSet[thisEquaSets.EquaBox] = underlying
    override def toString: String = s"TreeEquaSet(${underlying.toVector.map(_.value).mkString(", ")})"
    def union(that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying union that.toSet.map((eb: EquaBox) => EquaBox(eb.value)))
  }
  object SortedEquaSet {
    def empty: SortedEquaSet = TreeEquaSet.empty
    def apply(elems: T*): SortedEquaSet = TreeEquaSet(elems: _*)
  }
  object TreeEquaSet {
    def empty: TreeEquaSet = new TreeEquaSet(TreeSet.empty(ordering))
    def apply(elems: T*): TreeEquaSet = 
      new TreeEquaSet(TreeSet(elems.map(EquaBox(_)): _*)(ordering))
  }
}

object SortedEquaSets {
  def apply[T](equality: OrderingEquality[T]): SortedEquaSets[T] = new SortedEquaSets(equality)
}

