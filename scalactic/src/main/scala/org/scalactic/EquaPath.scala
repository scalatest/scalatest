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

import scala.Iterator
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.FilterMonadic
import scala.collection.immutable._
import scala.collection.mutable
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenIterable
import scala.collection.TraversableView
import scala.collection.parallel.mutable.ParArray
import scala.language.higherKinds
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.reflect.ClassTag

class EquaPath[T](val equality: HashingEquality[T]) { thisEquaPath =>

  case class EquaBox(value: T) {
    override def equals(o: Any): Boolean = 
      o match {
        case other: EquaPath[_]#EquaBox if equality eq other.path.equality =>
          equality.areEqual(value, other.value)
        case _ => false
      }
    override def hashCode: Int = equality.hashCodeFor(value)
    override def toString: String = s"EquaBox(${value.toString})"
    val path: thisEquaPath.type = thisEquaPath
    // def copyInto(thatEquaPath: EquaPath[T]): thatEquaPath.EquaBox
  }
  object EquaBox {
    import scala.language.implicitConversions
    implicit def equaBoxToGenTraversableOnce(equaBox: EquaBox): scala.collection.immutable.IndexedSeq[T] =
      Vector(equaBox.value)
  }

  class EquaBridge[S](from: List[S]) {
    def collect(pf: PartialFunction[S, T]): thisEquaPath.EquaSet =
      thisEquaPath.EquaSet.empty ++ (from collect pf)
    def map(f: S => T): thisEquaPath.EquaSet =
      thisEquaPath.EquaSet.empty ++ (from map f)
    def flatMap(f: S => thisEquaPath.EquaSet): thisEquaPath.EquaSet =
      thisEquaPath.EquaSet((from flatMap ((s: S) => f(s).toList)): _*)
    def flatten(implicit cvt: S <:< thisEquaPath.EquaSet): thisEquaPath.EquaSet =
      flatMap((s: S) => cvt(s))
    def scanLeft(z: T)(op: (T, S) => T): thisEquaPath.EquaSet =
      thisEquaPath.EquaSet(from.scanLeft(z)((t: T, s: S) => op(t, s)).toSeq: _*)
    def scanRight(z: T)(op: (S, T) => T): thisEquaPath.EquaSet =
      thisEquaPath.EquaSet(from.scanRight(z)((s: S, t: T) => op(s, t)).toSeq: _*)
    val path: thisEquaPath.type = thisEquaPath
    def foreach[U](f: S => U): Unit = from.foreach(f)
    def filter(pred: S => Boolean): thisEquaPath.EquaBridge[S] =
      new thisEquaPath.EquaBridge(from.filter(pred))
    def withFilter(pred: S => Boolean): WithFilter = new WithFilter(pred)

    /**
     * A class supporting filtered operations. Instances of this class are
     * returned by method `withFilter`.
     */
    class WithFilter(p: S => Boolean) {

      /**
       * Applies a function `f` to all elements of the outer `EquaBridge` containing
       * this `WithFilter` instance that satisfy predicate `p`.
       *
       * @param f the function that is applied for its side-effect to every element.
       * The result of function `f` is discarded.
       *
       * @tparam U the type parameter describing the result of function `f`.
       * This result will always be ignored. Typically `U` is `Unit`,
       * but this is not necessary.
       *
       */
      def foreach[U](f: S => U): Unit =
        filter(p).foreach(f)

      /**
       * Builds a new `EquaSet` by applying a function to all elements of the
       * outer `EquaBridge` containing this `WithFilter` instance that satisfy predicate `p`.
       *
       * @param f the function to apply to each element.
       * @return a new `EquaSet` resulting from applying
       * the given function `f` to each element of the outer `EquaBridge`
       * that satisfies predicate `p` and collecting the results.
       *
       * @return a new `EquaSet` resulting from applying the given function
       * `f` to each element of the outer `EquaBridge` that satisfies
       * predicate `p` and collecting the results.
       */
      def map(f: S => T): thisEquaPath.EquaSet =
        filter(p).map(f)

      /**
       * Builds a new `EquaSet` by applying a function to all elements of the
       * outer `EquaBridge` containing this `WithFilter` instance that satisfy
       * predicate `p` and concatenating the results.
       *
       * @param f the function to apply to each element.
       * @return a new `EquaSet` resulting from applying
       * the given `EquaSet`-valued function `f` to each element
       * of the outer `EquaBridge` that satisfies predicate `p` and
       * concatenating the results.
       *
       * @return a new `EquaSet` resulting from applying the given
       * `EquaSet`-valued function `f` to each element of the
       * outer `EquaBridge` that satisfies predicate `p` and concatenating
       * the results.
       */
      def flatMap(f: S => thisEquaPath.EquaSet): thisEquaPath.EquaSet =
        filter(p).flatMap(f)

      /**
       * Further refines the filter for this `EquaBridge`.
       *
       * @param q the predicate used to test elements.
       * @return an object of class `WithFilter`, which supports
       * `map`, `flatMap`, `foreach`, and `withFilter` operations.
       * All these operations apply to those elements of this `EquaBridge` which
       * satisfy the predicate `q` in addition to the predicate `p`.
       */
      def withFilter(q: S => Boolean): WithFilter =
        new WithFilter(x => p(x) && q(x))
    }
  }

    // I think we can just put this flatten on EquaSet itself, and possibly have a flatten
    // method here that works if S is a GenTraversable[T]. That means that they have
    // say a val xss = x.EquaSet(List(1), List(2, 3)). Since the element type is Int, then they
    // can say xss.into(number).flatten and you'd end up with a number.EquaSet(1, 2, 3)
    // for consistency I'd probably say flattenTrav for this one.
    //
    // The above then makes me wonder if there's not another potential method here that is
    // def flatMapTrav(f: S => GenTraversable[T]): thisEquaPath.EquaSet
    // that way you can say something like:
    // val ss = lower.EquaSet("hi", "ha")
    // ss.into(number).flatMapTrav((s: String) => s.toList.map(_.toInt)))
    // and you'd end up with number.EquaSet(104, 97, 105)
    //
    // scala> 'h'.toInt
    // res1: Int = 104
    //
    // scala> 'a'.toInt
    // res2: Int = 97
    //
    // scala> 'i'.toInt
    // res3: Int = 105

    // What's missing is a flatten on Bridge that let's you go to a different EquaPath than the
    // current one so long as the type parameter is the same. So if you have an x.EquaSet[number.EquaSet], and
    // since number's type parameter is Int, you could say into(anotherIntOne).flatten. Boy that seems like
    // it would be never invoked.

  trait EquaSet extends Function1[T, Boolean] with Equals {

    /**
     * Creates a new `EquaSet` with an additional element, unless the element is
     * already present.
     *
     * @param elem the element to be added
     * @return a new `EquaSet` that contains all elements of this `EquaSet` and that also
     * contains `elem`.
     */
    def + (elem: T): thisEquaPath.EquaSet

    /**
     * Creates a new `EquaSet` with additional elements.
     *
     * This method takes two or more elements to be added. Another overloaded
     * variant of this method handles the case where a single element is added.
     *
     * @param elem1 the first element to add.
     * @param elem2 the second element to add.
     * @param elems the remaining elements to add.
     * @return a new `EquaSet` with the given elements added.
     */
    def + (elem1: T, elem2: T, elems: T*): thisEquaPath.EquaSet

    /** Creates a new `EquaSet` by adding all elements contained in another collection to this `EquaSet`.
      *
      *  @param elems     the collection containing the added elements.
      *  @return          a new `EquaSet` with the given elements added.
      */
    def ++ (elems: GenTraversableOnce[T]): thisEquaPath.EquaSet

    /**
     * Creates a new `EquaSet` by adding elements contained in another `EquaSet`.
     *
     * @param that     the other `EquaSet` containing the added elements.
     * @return         a new `EquaSet` with the given elements added.
     */
    def ++ (that: thisEquaPath.EquaSet): thisEquaPath.EquaSet

    /**
     * Creates a new `EquaSet` with a given element removed from this `EquaSet`.
     *
     * @param elem the element to be removed
     * @return a new `EquaSet` that contains all elements of this `EquaSet` but that does not
     * contain `elem`.
     */
    def - (elem: T): thisEquaPath.EquaSet

    /* * USE LATER
     * Creates a new `EquaSet` from this `EquaSet` by removing all elements of another
     * collection.
     *
     * @param xs the collection containing the removed elements.
     * @return a new `EquaSet` that contains all elements of the current `EquaSet`
     * except one less occurrence of each of the elements of `elems`.
     */

    /**
     * Creates a new `EquaSet` from this `EquaSet` with some elements removed.
     *
     * This method takes two or more elements to be removed. Another overloaded
     * variant of this method handles the case where a single element is
     * removed.
     * @param elem1 the first element to remove.
     * @param elem2 the second element to remove.
     * @param elems the remaining elements to remove.
     * @return a new `EquaSet` that contains all elements of the current `EquaSet`
     * except one less occurrence of each of the given elements.
     */
    def - (elem1: T, elem2: T, elems: T*): thisEquaPath.EquaSet

    /**
     * Creates a new `EquaSet` from this `EquaSet` by removing all elements of another
     *  collection.
     *
     *  @param elems     the collection containing the removed elements.
     *  @return a new `EquaSet` that contains all elements of the current `EquaSet`
     *  except one less occurrence of each of the elements of `elems`.
     */
    def --(elems: GenTraversableOnce[T]): thisEquaPath.EquaSet

    /**
     * Creates a new `EquaSet` from this `EquaSet` by removing all elements of another `EquaSet`
     *
     * @param that       the other `EquaSet` containing the removed elements.
     * @return a new `EquaSet` that contains all elements of the current `EquaSet` minus elements contained in the passed in `EquaSet`.
     */
    def --(that: thisEquaPath.EquaSet): thisEquaPath.EquaSet

    /**
     * Applies a binary operator to a start value and all elements of this `EquaSet`,
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
     *  @return  the result of inserting `op` between consecutive elements of this `EquaSet`,
     *           going left to right with the start value `z` on the left:
     *           {{{
     *             op(...op(op(z, x_1), x_2), ..., x_n)
     *           }}}
     *           where `x,,1,,, ..., x,,n,,` are the elements of this `EquaSet`.
     */
    def /:[B](z: B)(op: (B, T) => B): B

    /**
     * Applies a binary operator to all elements of this `EquaSet` and a start value,
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
     *  @return  the result of inserting `op` between consecutive elements of this `EquaSet`,
     *           going right to left with the start value `z` on the right:
     *           {{{
     *             op(x_1, op(x_2, ... op(x_n, z)...))
     *           }}}
     *           where `x,,1,,, ..., x,,n,,` are the elements of this `EquaSet`.
     */
    def :\[B](z: B)(op: (T, B) => B): B

    /**
     * Computes the union between this `EquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `union`.
     * @param that the `EquaSet` to form the union with.
     * @return a new `EquaSet` consisting of all elements that are in this
     * `EquaSet` or in the given `EquaSet` `that`.
     */
    def | (that: thisEquaPath.EquaSet): thisEquaPath.EquaSet

    /**
     * Computes the intersection between this `EquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `intersect`.
     * @param that the `EquaSet` to intersect with.
     * @return a new `EquaSet` consisting of all elements that are both in this
     * `EquaSet` and in the given `EquaSet` `that`.
     */
    def & (that: thisEquaPath.EquaSet): thisEquaPath.EquaSet

    /**
     * The difference of this `EquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `diff`.
     * @param that the `EquaSet` of elements to exclude.
     * @return an `EquaSet` containing those elements of this
     * `EquaSet` that are not also contained in the given `EquaSet` `that`.
     */
    def &~ (that: thisEquaPath.EquaSet): thisEquaPath.EquaSet

    /**
     * Appends all elements of this `EquaSet` to a string builder.
     *  The written text consists of the string representations (w.r.t. the method
     * `toString`) of all elements of this `EquaSet` without any separator string.
     *
     * Example:
     *
     * {{{
     *      scala> val a = List(1,2,3,4)
     *      a: List[Int] = List(1, 2, 3, 4)
     *
     *      scala> val b = new StringBuilder()
     *      b: StringBuilder =
     *
     *      scala> val h = a.addString(b)
     *      h: StringBuilder = 1234
     * }}}
     *
     *  @param  b    the string builder to which elements are appended.
     *  @return      the string builder `b` to which elements were appended.
     */
    def addString(b: StringBuilder): StringBuilder

    /**
     * Appends all elements of this `EquaSet` to a string builder using a separator string.
     *  The written text consists of the string representations (w.r.t. the method `toString`)
     *  of all elements of this `EquaSet`, separated by the string `sep`.
     *
     * Example:
     *
     * {{{
     *      scala> val a = List(1,2,3,4)
     *      a: List[Int] = List(1, 2, 3, 4)
     *
     *      scala> val b = new StringBuilder()
     *      b: StringBuilder =
     *
     *      scala> a.addString(b, ", ")
     *      res0: StringBuilder = 1, 2, 3, 4
     * }}}
     *
     *  @param  b    the string builder to which elements are appended.
     *  @param sep   the separator string.
     *  @return      the string builder `b` to which elements were appended.
     */
    def addString(b: StringBuilder, sep: String): StringBuilder

    /** Appends all elements of this `EquaSet` to a string builder using start, end, and separator strings.
     *  The written text begins with the string `start` and ends with the string `end`.
     *  Inside, the string representations (w.r.t. the method `toString`)
     *  of all elements of this `EquaSet` are separated by the string `sep`.
     *
     * Example:
     *
     * {{{
     *      scala> val a = List(1,2,3,4)
     *      a: List[Int] = List(1, 2, 3, 4)
     *
     *      scala> val b = new StringBuilder()
     *      b: StringBuilder =
     *
     *      scala> a.addString(b , "List(" , ", " , ")")
     *      res5: StringBuilder = List(1, 2, 3, 4)
     * }}}
     *
     *  @param  b    the string builder to which elements are appended.
     *  @param start the starting string.
     *  @param sep   the separator string.
     *  @param end   the ending string.
     *  @return      the string builder `b` to which elements were appended.
     */
    def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder

    /**
     * Aggregates the results of applying an operator to subsequent elements.
     *
     *  This is a more general form of `fold` and `reduce`. It has similar
     *  semantics, but does not require the result to be a supertype of the
     *  element type. It traverses the elements in different partitions
     *  sequentially, using `seqop` to update the result, and then applies
     *  `combop` to results from different partitions. The implementation of
     *  this operation may operate on an arbitrary number of collection
     *  partitions, so `combop` may be invoked an arbitrary number of times.
     *
     *  For example, one might want to process some elements and then produce
     *  a `Set`. In this case, `seqop` would process an element and append it
     *  to the list, while `combop` would concatenate two lists from different
     *  partitions together. The initial value `z` would be an empty set.
     *  {{{
     *    pc.aggregate(Set[Int]())(_ += process(_), _ ++ _)
     *  }}}
     *
     *  Another example is calculating geometric mean from a collection of doubles
     *  (one would typically require big doubles for this).
     *
     *  @tparam B        the type of accumulated results
     *  @param z         the initial value for the accumulated result of the partition - this
     *                   will typically be the neutral element for the `seqop` operator (e.g.
     *                   `Nil` for list concatenation or `0` for summation) and may be evaluated
     *                   more than once
     *  @param seqop     an operator used to accumulate results within a partition
     *  @param combop    an associative operator used to combine results from different partitions
     */
    def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B

    /**
     * Tests if some element is contained in this set.
     *
     *  This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
     *  @param elem the element to test for membership.
     *  @return  `true` if `elem` is contained in this set, `false` otherwise.
     */
    def apply(elem: T): Boolean

    /**
     * Builds a new collection by applying a partial function to all elements of this `EquaSet`
     * on which the function is defined.
     *
     * @param pf the partial function which filters and maps the `EquaSet`.
     * @return a new collection of type `That` resulting from applying the partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     *
     * @return a new `EquaSet` resulting from applying the given partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     */
    def collect(pf: PartialFunction[T, T]): thisEquaPath.EquaSet

    /*
      The reason I don't just do this:

      def contains(elem: T): Boolean

      Is because that indeed fails to type check, but before the compiler gives up, it looks around
      for an implicit that solves the problem and finds the one to IndexedSeq[T] named,
      equaSetToGenTraversableOnce, which works so it compiles.
    */
    def contains[U](elem: U)(implicit ev: U <:< T): Boolean

    /**
     * Copies values of this `EquaSet` to an array.
     * Fills the given array `xs` with values of this `EquaSet`.
     * Copying will stop once either the end of the current `EquaSet` is reached,
     * or the end of the array is reached.
     *
     * @param xs the array to fill.
     *
     */
    def copyToArray(xs: Array[thisEquaPath.EquaBox]): Unit

    /**
     * Copies values of this `EquaSet` to an array.
     * Fills the given array `xs` with values of this `EquaSet`, beginning at index `start`.
     * Copying will stop once either the end of the current `EquaSet` is reached,
     * or the end of the array is reached.
     *
     * @param xs the array to fill.
     * @param start the starting index.
     *
     */
    def copyToArray(xs: Array[thisEquaPath.EquaBox], start: Int): Unit

    /**
     * Copies values of this `EquaSet` to an array.
     * Fills the given array `xs` with values of this `EquaSet`, beginning at index `start`.
     * Copying will stop once the count of element copied reach <code>len</code>.
     *
     * @param xs the array to fill.
     * @param start the starting index.
     * @param len the length of elements to copy
     *
     */
    def copyToArray(xs: Array[thisEquaPath.EquaBox], start: Int, len: Int): Unit

    /**
     * Copies all elements of this `EquaSet` to a buffer.
     *
     * @param dest The buffer to which elements are copied.
     */
    def copyToBuffer(dest: mutable.Buffer[thisEquaPath.EquaBox]): Unit

    /**
     * Counts the number of elements in the `EquaSet` which satisfy a predicate.
     *
     * @param p the predicate used to test elements.
     * @return the number of elements satisfying the predicate `p`.
     */
    def count(p: T => Boolean): Int

    /**
     * Computes the difference of this `EquaSet` and another `EquaSet`.
     *
     * @param that the `EquaSet` of elements to exclude.
     * @return an `EquaSet` containing those elements of this
     * `EquaSet` that are not also contained in the given `EquaSet` `that`.
     */
    def diff(that: thisEquaPath.EquaSet): thisEquaPath.EquaSet

    /**
     * Selects all elements except first ''n'' ones.
     *
     * @param n the number of elements to drop from this `EquaSet`.
     * @return an `EquaSet` consisting of all elements of this `EquaSet` except the first `n` ones, or else the
     * empty `EquaSet`, if this `EquaSet` has less than `n` elements.
     */
    def drop(n: Int): thisEquaPath.EquaSet

    /**
     * Selects all elements except last ''n'' ones.
     *
     * @param n The number of elements to take
     * @return an `EquaSet` consisting of all elements of this `EquaSet` except the last `n` ones, or else the
     * empty `EquaSet`, if this `EquaSet` has less than `n` elements.
     */
    def dropRight(n: Int): thisEquaPath.EquaSet

    /**
     * Drops longest prefix of elements that satisfy a predicate.
     *
     * @param pred The predicate used to test elements.
     * @return the longest suffix of this `EquaSet` whose first element
     * does not satisfy the predicate `p`.
     */
    def dropWhile(pred: T => Boolean): thisEquaPath.EquaSet

    /**
     * Check if this `EquaSet` contains element which satisfy a predicate.
     *
     * @param pred predicate predicate used to test elements
     * @return <code>true</code> if there's at least one element satisfy the given predicate <code>pred</code>
     */
    def exists(pred: T => Boolean): Boolean

    /**
     * Selects all elements of this `EquaSet` which satisfy a predicate.
     *
     * @param pred the predicate used to test elements.
     * @return a new `EquaSet` consisting of all elements of this `EquaSet` that satisfy the given
     * predicate <code>pred</code>. Their order may not be preserved.
     */
    def filter(pred: T => Boolean): thisEquaPath.EquaSet

    /**
     * Selects all elements of this `EquaPath` which do not satisfy a predicate.
     *
     * @param pred the predicate used to test elements.
     * @return a new `EquaPath` consisting of all elements of this `EquaPath` that do not satisfy the given
     * predicate <code>pred</code>. Their order may not be preserved.
     */
    def filterNot(pred: T => Boolean): thisEquaPath.EquaSet

    /**
     * Finds the first element of the `EquaSet` satisfying a predicate, if any.
     *
     *
     * @param pred the predicate used to test elements.
     * @return an option value containing the first element in the `EquaSet`
     * that satisfies <code>pred</code>, or <code>None</code> if none exists.
     */
    def find(pred: T => Boolean): Option[T]

    /**
     * Builds a new `EquaSet` by applying a function to all elements of this `EquaSet`
     * and using the elements of the resulting `EquaSet`.
     *
     * @param f the function to apply to each element.
     * @return a new `EquaSet` resulting from applying the given `EquaSet`-valued function
     * `f` to each element of this `EquaSet` and concatenating the results.
     *
     * For example:
     *
     * {{{
     * def getWords(lines: EquaSet[String]): EquaSet[String] = lines flatMap (line => equaSets.EquaSet(line.split("\\W+"): _*))
     * }}}
     *
     * @return a new `EquaSet` resulting from applying the given `EquaSet`-valued function
     * `f` to each element of this `EquaSet` and concatenating the results.
     */
    def flatMap(f: T => thisEquaPath.EquaSet): thisEquaPath.EquaSet

    /**
     * Converts this `EquaSet` of `EquaSet` into
     * an `EquaSet` formed by the elements of these `EquaSet`.
     *
     *
     * @return a new `EquaSet` resulting from concatenating all element `EquaSet`s.
     *
     * The resulting collection's type will be guided by the
     * static type of `EquaSet`. For example:
     *
     * {{{
     *
     * val ys = `EquaSet`(
     * `EquaSet`(1, 2, 3),
     * `EquaSet`(3, 2, 1)
     * ).flatten
     * // ys == `EquaSet`(1, 2, 3)
     * }}}
     */
    //def flatten[S, T >: EquaPath[S]#EquaSet]: EquaPath[S]#EquaSet

    /**
     * Folds the elements of this `EquaSet` using the specified associative
     * binary operator.
     *
     * @tparam T1 a type parameter for the binary operator, a supertype of `A`.
     * @param z a neutral element for the fold operation; may be added to the result
     * an arbitrary number of times, and must not change the result (e.g., `Nil` for list concatenation,
     * 0 for addition, or 1 for multiplication.)
     * @param op a binary operator that must be associative
     * @return the result of applying fold operator `op` between all the elements and `z`
     */
    def fold[T1 >: T](z: T1)(op: (T1, T1) => T1): T1

    /**
     * Applies a binary operator to a start value and all elements of this `EquaSet`,
     * going left to right.
     *
     * @param z the start value.
     * @param op the binary operator.
     * @tparam B the result type of the binary operator.
     * @return the result of inserting `op` between consecutive elements of this `EquaSet`,
     * going left to right with the start value `z` on the left:
     * {{{
     * op(...op(z, x_1), x_2, ..., x_n)
     * }}}
     * where `x,,1,,, ..., x,,n,,` are the elements of this `EquaSet`.
     */
    def foldLeft[B](z: B)(op: (B, T) => B): B

    /**
     * Applies a binary operator to all elements of this `EquaSet` and a start value,
     * going right to left.
     *
     * @param z the start value.
     * @param op the binary operator.
     * @tparam B the result type of the binary operator.
     * @return the result of inserting `op` between consecutive elements of this `EquaSet`,
     * going right to left with the start value `z` on the right:
     * {{{
     * op(x_1, op(x_2, ... op(x_n, z)...))
     * }}}
     * where `x,,1,,, ..., x,,n,,` are the elements of this `EquaSet`.
     */
    def foldRight[B](z: B)(op: (T, B) => B): B

    /**
     * Check if all elements in this `EquaSet` satisfy the predicate.
     *
     * @param pred the predicate to check for
     * @return <code>true</code> if all elements satisfy the predicate, <code>false</code> otherwise.
     */
    def forall(pred: T => Boolean): Boolean

    /**
     * Applies a function `f` to all elements of this `EquaSet`.
     *
     * @param f the function that is applied for its side-effect to every element.
     * The result of function `f` is discarded.
     *
     * @tparam U the type parameter describing the result of function `f`.
     * This result will always be ignored. Typically `U` is `Unit`,
     * but this is not necessary.
     *
     */
    def foreach[U](f: T => U): Unit

    /**
     * Partitions this `EquaSet` into a map of `EquaSet`s according to some discriminator function.
     *
     * Note: this method is not re-implemented by views. This means
     * when applied to a view it will always force the view and
     * return a new `EquaSet`.
     *
     * @param f the discriminator function.
     * @tparam K the type of keys returned by the discriminator function.
     * @return A map from keys to `EquaSet`s such that the following invariant holds:
     * {{{
     * (xs groupBy f)(k) = xs filter (x => f(x) == k)
     * }}}
     * That is, every key `k` is bound to an `EquaSet` of those elements `x`
     * for which `f(x)` equals `k`.
     *
     */
    def groupBy[K](f: T => K): GenMap[K, thisEquaPath.EquaSet]

    /**
     * Partitions elements in fixed size `EquaSet`s.
     * @see [[scala.collection.Iterator]], method `grouped`
     *
     * @param size the number of elements per group
     * @return An iterator producing `EquaSet`s of size `size`, except the
     * last will be less than size `size` if the elements don't divide evenly.
     */
    def grouped(size: Int): Iterator[thisEquaPath.EquaSet]

    def hasDefiniteSize: Boolean

    /** Selects the first element of this `EquaSet`.
      *
      * @return the first element of this `EquaSet`.
      * @throws `NoSuchElementException` if the `EquaSet` is empty.
      */
    def head: T

    /** Optionally selects the first element.
      *
      * @return the first element of this `EquaSet` if it is nonempty,
      * `None` if it is empty.
      */
    def headOption: Option[T]

    /**
     * Selects all elements except the last.
     *
     * @return an `EquaSet` consisting of all elements of this `EquaSet`
     * except the last one.
     * @throws `UnsupportedOperationException` if the `EquaSet` is empty.
     */
    def init: thisEquaPath.EquaSet

    /**
     * Iterates over the inits of this `EquaSet`. The first value will be this
     * `EquaSet` and the final one will be an empty `EquaSet`, with the intervening
     * values the results of successive applications of `init`.
     *
     * @return an iterator over all the inits of this `EquaSet`
     * @example EquaSet(1,2,3).inits = Iterator(EquaSet(1,2,3), EquaSet(1,2), EquaSet(1), EquaSet())
     */
    def inits: Iterator[thisEquaPath.EquaSet]

    /**
     * Computes the intersection between this `EquaSet` and another `EquaSet`.
     *
     * @param that the `EquaSet` to intersect with.
     * @return a new `EquaSet` consisting of all elements that are both in this
     * `EquaSet` and in the given `EquaSet` `that`.
     */
    def intersect(that: thisEquaPath.EquaSet): thisEquaPath.EquaSet

    /**
     * Make an `EquaBridge` between this `EquaSet` and the given `thatEquaPath`.
     * `EquaBridge` enables this `EquaSet` to transform into `thatEquaPath`.`EquaSet`
     * through `collect`, `map`, `flatMap`, `flatten`, `scanLeft`, `scanRight`.
     *
     * @param thatEquaPath that `EquaPath` to bridge to
     * @tparam U the type of `thatEquaPath`
     * @return an instance of `thatEquaPath`.`EquaBridge`
     */
    def into[U](thatEquaPath: EquaPath[U]): thatEquaPath.EquaBridge[T]

    /**
     * Tests if this `EquaSet` is empty.
     *
     * @return `true` if there is no element in the set, `false` otherwise.
     */
    def isEmpty: Boolean

    /**
     * Tests whether this `EquaSet` can be repeatedly traversed. Always
     * true for `EquaSet` unless overridden.
     *
     * @return `true` unless overriden.
     */
    def isTraversableAgain: Boolean = true

    /**
     * Get an instance of `Iterator` for elements of this `EquaSet`.
     *
     * @return an instance of `Iterator` for elements of this `EquaSet`
     */
    def iterator: Iterator[T]

    /**
     * Selects the last element.
     *
     * @return The last element of this `EquaSet`.
     * @throws NoSuchElementException If the `EquaSet` is empty.
     */
    def last: T

    /**
     * Optionally selects the last element.
     *
     * @return the last element of this `EquaSet` if it is nonempty,
     * `None` if it is empty.
     */
    def lastOption: Option[T]

    /**
     * Builds a new `EquaSet` by applying a function to all elements of this `EquaSet`.
     *
     * @param f the function to apply to each element.
     * @return a new `EquaSet` resulting from applying the given function
     * `f` to each element of this `EquaSet` and collecting the results.
     *
     * @return a new `EquaSet` resulting from applying the given function
     * `f` to each element of this `EquaSet` and collecting the results.
     */
    def map(f: T => T): thisEquaPath.EquaSet

    /**
     * Finds the largest element.
      *
      * @param ord An ordering to be used for comparing elements.
      * @tparam T1 The type over which the ordering is defined.
      * @return the largest element of this `EquaSet` with respect to the ordering `ord`.
      *
      * @return the largest element of this `EquaSet`.
      */
    def max[T1 >: T](implicit ord: Ordering[T1]): T

    /**
     * Finds the first element which yields the largest value measured by function f.
     *
     * @param cmp An ordering to be used for comparing elements.
     * @tparam B The result type of the function f.
     * @param f The measuring function.
     * @return the first element of this `EquaSet` with the largest value measured by function f
     * with respect to the ordering `cmp`.
     *
     * @return the first element of this `EquaSet` with the largest value measured by function f.
     */
    def maxBy[B](f: T => B)(implicit cmp: Ordering[B]): T

    /**
     * Finds the smallest element.
     *
     * @param ord An ordering to be used for comparing elements.
     * @tparam T1 The type over which the ordering is defined.
     * @return the smallest element of this `EquaSet` with respect to the ordering `ord`.
     *
     * @return the smallest element of this `EquaSet`
     */
    def min[T1 >: T](implicit ord: Ordering[T1]): T

    /**
     * Finds the first element which yields the smallest value measured by function f.
     *
     * @param cmp An ordering to be used for comparing elements.
     * @tparam B The result type of the function f.
     * @param f The measuring function.
     * @return the first element of this `EquaSet` with the smallest value measured by function f
     * with respect to the ordering `cmp`.
     *
     * @return the first element of this `EquaSet` with the smallest value measured by function f.
     */
    def minBy[B](f: T => B)(implicit cmp: Ordering[B]): T

    /**
     * Displays all elements of this `EquaSet` in a string using start, end, and
     * separator strings.
     *
     * @param start the starting string.
     * @param sep the separator string.
     * @param end the ending string.
     * @return a string representation of this `EquaSet`. The resulting string
     * begins with the string `start` and ends with the string
     * `end`. Inside, the string representations (w.r.t. the method
     * `toString`) of all elements of this `EquaSet` are separated by
     * the string `sep`.
     *
     * @example `EquaSet(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"`
     */
    def mkString(start: String, sep: String, end: String): String
    /**
     * Displays all elements of this `EquaSet` in a string using a separator string.
     *
     * @param sep the separator string.
     * @return a string representation of this `EquaSet`. In the resulting string
     * the string representations (w.r.t. the method `toString`)
     * of all elements of this `EquaSet` are separated by the string `sep`.
     *
     * @example `EquaSet(1, 2, 3).mkString("|") = "1|2|3"`
     */
    def mkString(sep: String): String
    /**
     * Displays all elements of this `EquaSet` in a string.
     *
     * @return a string representation of this `EquaSet`. In the resulting string
     * the string representations (w.r.t. the method `toString`)
     * of all elements of this `EquaSet` follow each other without any
     * separator string.
     */
    def mkString: String

    /** Tests whether the `EquaSet` is not empty.
      *
      * @return `true` if the `EquaSet` contains at least one element, `false` otherwise.
      */
    def nonEmpty: Boolean

    /**
     * Partitions this `EquaSet` in two `EquaSet`s according to a predicate.
     *
     * @param pred the predicate on which to partition.
     * @return a pair of `EquaSet`s: the first `EquaSet` consists of all elements that
     * satisfy the predicate `p` and the second `EquaSet` consists of all elements
     * that don't. The relative order of the elements in the resulting `EquaSet`s
     * may not be preserved.
     */
    def partition(pred: T => Boolean): (thisEquaPath.EquaSet, thisEquaPath.EquaSet)

    /**
     * Multiplies up the elements of this collection.
     *
     * @param num an implicit parameter defining a set of numeric operations
     * which includes the `*` operator to be used in forming the product.
     * @tparam T1 the result type of the `*` operator.
     * @return the product of all elements of this `EquaSet` with respect to the `*` operator in `num`.
     *
     * @return the product of all elements in this `EquaSet` of numbers of type `Int`.
     * Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
     * can be used as element type of the `EquaSet` and as result type of `product`.
     * Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
     */
    def product[T1 >: T](implicit num: Numeric[T1]): T1

    /**
     * Reduces the elements of this `EquaSet` using the specified associative binary operator.
     *
     * @tparam T1 A type parameter for the binary operator, a supertype of `T`.
     * @param op A binary operator that must be associative.
     * @return The result of applying reduce operator `op` between all the elements if the `EquaSet` is nonempty.
     * @throws UnsupportedOperationException
     * if this `EquaSet` is empty.
     */
    def reduce[T1 >: T](op: (T1, T1) => T1): T1

    /**
     * Applies a binary operator to all elements of this `EquaSet`,
     * going left to right.
     *
     * @param op the binary operator.
     * @tparam T1 the result type of the binary operator.
     * @return the result of inserting `op` between consecutive elements of this `EquaSet`,
     * going left to right:
     * {{{
     * op( op( ... op(x_1, x_2) ..., x_{n-1}), x_n)
     * }}}
     * where `x,,1,,, ..., x,,n,,` are the elements of this `EquaSet`.
     * @throws `UnsupportedOperationException` if this `EquaSet` is empty. */
    def reduceLeft[T1 >: T](op: (T1, T) => T1): T1

    /**
     * Optionally applies a binary operator to all elements of this `EquaSet`, going left to right.
     *
     * @param op the binary operator.
     * @tparam T1 the result type of the binary operator.
     * @return an option value containing the result of `reduceLeft(op)` is this `EquaSet` is nonempty,
     * `None` otherwise.
     */
    def reduceLeftOption[T1 >: T](op: (T1, T) => T1): Option[T1]

    /**
     * Reduces the elements of this `EquaSet`, if any, using the specified
     * associative binary operator.
     *
     * @tparam T1 A type parameter for the binary operator, a supertype of `T`.
     * @param op A binary operator that must be associative.
     * @return An option value containing result of applying reduce operator `op` between all
     * the elements if the collection is nonempty, and `None` otherwise.
     */
    def reduceOption[T1 >: T](op: (T1, T1) => T1): Option[T1]

    /**
     * Applies a binary operator to all elements of this `EquaSet`, going right to left.
     *
     * @param op the binary operator.
     * @tparam T1 the result type of the binary operator.
     * @return the result of inserting `op` between consecutive elements of this `EquaSet`,
     * going right to left:
     * {{{
     * op(x_1, op(x_2, ..., op(x_{n-1}, x_n)...))
     * }}}
     * where `x,,1,,, ..., x,,n,,` are the elements of this `EquaSet`.
     * @throws `UnsupportedOperationException` if this `EquaSet` is empty.
     */
    def reduceRight[T1 >: T](op: (T, T1) => T1): T1

    /**
     * Optionally applies a binary operator to all elements of this `EquaSet`, going
     * right to left.
     *
     * @param op the binary operator.
     * @tparam T1 the result type of the binary operator.
     * @return an option value containing the result of `reduceRight(op)` is this `EquaSet` is nonempty,
     * `None` otherwise.
     */
    def reduceRightOption[T1 >: T](op: (T, T1) => T1): Option[T1]

    /**
     * The `Set[EquaBox]` underlying this `EquaSet` object.
     */
    def repr: Set[EquaBox]

    /**
     * Checks if the other iterable collection contains the same elements in the same order as this `EquaSet`.
     *
     * @param that the collection to compare with.
     * @tparam T1 the type of the elements of collection `that`.
     * @return `true`, if both collections contain the same elements in the same order, `false` otherwise.
     */
    def sameElements[T1 >: T](that: GenIterable[T1]): Boolean

    /**
     * Produces a collection containing cumulative results of applying the
     * operator going left to right.
     *
     * @param z the initial value
     * @param op the binary operator applied to the intermediate result and the element
     * @return `EquaSet` with intermediate results
     */
    def scanLeft(z: T)(op: (T, T) => T): thisEquaPath.EquaSet

    /**
     * Produces a collection containing cumulative results of applying the operator going right to left.
     * The head of the collection is the last cumulative result.
     *
     * Example:
     * {{{
     * `EquaSet`(1, 2, 3, 4).scanRight(0)(_ + _) == `EquaSet`(10, 9, 7, 4, 0)
     * }}}
     *
     * @param z the initial value
     * @param op the binary operator applied to the intermediate result and the element
     * @return `EquaSet` with intermediate results
     */
    def scanRight(z: T)(op: (T, T) => T): thisEquaPath.EquaSet

    /**
     * The size of this `EquaSet`.
     *
     * @return the number of elements in this `EquaSet`.
     */
    def size: Int

    /**
     * Selects an interval of elements. The returned collection is made up
     * of all elements `x` which satisfy the invariant:
     * {{{
     * from <= indexOf(x) < until
     * }}}
     *
     * @param unc_from the lowest index to include from this `EquaSet`.
     * @param unc_until the lowest index to EXCLUDE from this `EquaSet`.
     * @return an `EquaSet` containing the elements greater than or equal to
     * index `from` extending up to (but not including) index `until`
     * of this `EquaSet`.
     */
    def slice(unc_from: Int, unc_until: Int): thisEquaPath.EquaSet

    /**
     * Groups elements in fixed size blocks by passing a "sliding window"
     * over them (as opposed to partitioning them, as is done in grouped.)
     * @see [[scala.collection.Iterator]], method `sliding`
     *
     * @param size the number of elements per group
     * @return An iterator producing `SortedEquaSet`s of size `size`, except the
     * last and the only element will be truncated if there are
     * fewer elements than size.
     */
    def sliding(size: Int): Iterator[thisEquaPath.EquaSet]

    /**
     * Groups elements in fixed size blocks by passing a "sliding window"
     * over them (as opposed to partitioning them, as is done in grouped.)
     * @see [[scala.collection.Iterator]], method `sliding`
     *
     * @param size the number of elements per group
     * @param step the distance between the first elements of successive
     * groups (defaults to 1)
     * @return An iterator producing `EquaSet`s of size `size`, except the
     * last and the only element will be truncated if there are
     * fewer elements than size.
     */
    def sliding(size: Int, step: Int): Iterator[thisEquaPath.EquaSet]

    /**
     * Splits this `EquaSet` into a prefix/suffix pair according to a predicate.
     *
     * Note: `c span p` is equivalent to (but possibly more efficient than)
     * `(c takeWhile p, c dropWhile p)`, provided the evaluation of the
     * predicate `p` does not cause any side-effects.
     *
     *
     * @param pred the test predicate
     * @return a pair consisting of the longest prefix of this `EquaSet` whose
     * elements all satisfy `p`, and the rest of this `EquaSet`.
     */
    def span(pred: T => Boolean): (thisEquaPath.EquaSet, thisEquaPath.EquaSet)

    /**
     * Splits this `EquaSet` into two at a given position.
     * Note: `c splitAt n` is equivalent to (but possibly more efficient than)
     * `(c take n, c drop n)`.
     *
     *
     * @param n the position at which to split.
     * @return a pair of `EquaSet`s consisting of the first `n`
     * elements of this `EquaSet`, and the other elements.
     */
    def splitAt(n: Int): (thisEquaPath.EquaSet, thisEquaPath.EquaSet)

    /**
     * Defines the prefix of this object's `toString` representation.
     *
     * @return a string representation which starts the result of `toString`
     * applied to this `EquaSet`. By default the string prefix is the
     * simple name of the collection class `EquaSet`.
     */
    def stringPrefix: String

    /**
     * Tests whether this set is a subset of another set.
     *
     * @param that the set to test.
     * @return `true` if this set is a subset of `that`, i.e. if
     * every element of this set is also an element of `that`.
     */
    def subsetOf(that: thisEquaPath.EquaSet): Boolean

    /**
     * An iterator over all subsets of this set of the given size.
     * If the requested size is impossible, an empty iterator is returned.
     *
     * @param len the size of the subsets.
     * @return the iterator.
     */
    def subsets(len: Int): Iterator[thisEquaPath.EquaSet]

    /**
     * An iterator over all subsets of this set.
     *
     * @return the iterator.
     */
    def subsets: Iterator[thisEquaPath.EquaSet]

    /** Sums up the elements of this collection.
      *
      * @param num an implicit parameter defining a set of numeric operations
      * which includes the `+` operator to be used in forming the sum.
      * @tparam T1 the result type of the `+` operator.
      * @return the sum of all elements of this `EquaSet` with respect to the `+` operator in `num`.
      *
      * @return the sum of all elements in this `EquaSet` of numbers of type `Int`.
      * Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
      * can be used as element type of the `EquaSet` and as result type of `sum`.
      * Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
      *
      */
    def sum[T1 >: T](implicit num: Numeric[T1]): T1

    /**
     * Selects all elements except the first.
     *
     * @return an `EquaSet` consisting of all elements of this `EquaSet`
     * except the first one.
     * @throws `UnsupportedOperationException` if the `EquaSet` is empty.
     */
    def tail: thisEquaPath.EquaSet

    /**
     * Iterates over the tails of this `EquaSet`. The first value will be this
     * `EquaSet` and the final one will be an empty `EquaSet`, with the intervening
     * values the results of successive applications of `tail`.
     *
     * @return an iterator over all the tails of this `EquaSet`
     * @example `EquaSet(1,2,3).tails = Iterator(EquaSet(1,2,3), EquaSet(2,3), EquaSet(3), EquaSet())`
     */
    def tails: Iterator[thisEquaPath.EquaSet]

    /**
     * Selects first ''n'' elements.
     *
     * @param n the number of elements to take from this `EquaSet`.
     * @return an `EquaSet` consisting only of the first `n` elements of this `EquaSet`,
     * or else the whole `EquaSet`, if it has less than `n` elements.
     */
    def take(n: Int): thisEquaPath.EquaSet

    /**
     * Selects last ''n'' elements.
     *
     *
     * @param n the number of elements to take
     * @return an `EquaSet` consisting only of the last `n` elements of this `EquaSet`, or else the
     * whole `EquaSet`, if it has less than `n` elements.
     */
    def takeRight(n: Int): thisEquaPath.EquaSet

    /**
     * Converts this `EquaSet` into another by copying all elements.
     * @tparam Col The collection type to build.
     * @return a new collection containing all elements of this `EquaSet`.
     */
    def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, thisEquaPath.EquaBox, Col[thisEquaPath.EquaBox @uV]]): Col[thisEquaPath.EquaBox @uV]

    /**
     * Converts this `EquaSet` to an array.
     *
     * @return an array containing all elements of this `EquaSet`.
     */
    def toArray: Array[T]

    /**
     * Converts this `EquaSet` to an array of `EquaBox`es containing the elements.
     *
     * @return an array containing all elements of this `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxArray: Array[thisEquaPath.EquaBox]

    /**
     * Uses the contents of this `EquaSet` to create a new mutable buffer.
     *
     * @return a buffer containing all elements of this `EquaSet`.
     */
    def toBuffer: scala.collection.mutable.Buffer[T]

    /**
     * Uses the contents of this `EquaSet` to create a new mutable buffer containing `EquaBox`es of elements.
     *
     * @return a buffer containing all elements of this `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxBuffer: scala.collection.mutable.Buffer[thisEquaPath.EquaBox]

    /**
     * Converts this `EquaSet` to an indexed sequence.
     *
     * @return an indexed sequence containing all elements of this `EquaSet`.
     */
    def toIndexedSeq: scala.collection.immutable.IndexedSeq[T]

    /**
     * Converts this `EquaSet` to an indexed sequence containing `EquaBox`es of elements.
     *
     * @return an indexed sequence containing all elements of this `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxIndexedSeq: scala.collection.immutable.IndexedSeq[thisEquaPath.EquaBox]

    /**
     * Converts this `EquaSet` to an iterable collection. Note that
     * the choice of target `Iterable` is lazy in this default implementation
     * as this `TraversableOnce` may be lazy and unevaluated (i.e. it may
     * be an iterator which is only traversable once).
     *
     * @return an `Iterable` containing all elements of this `EquaSet`.
     */
    def toIterable: GenIterable[T]

    /**
     * Converts this `EquaSet` to an iterable collection of `EquaBox`es containing the elements. Note that
     * the choice of target `Iterable` is lazy in this default implementation as this `TraversableOnce` may
     * be lazy and unevaluated (i.e. it may be an iterator which is only traversable once).
     *
     * @return an `Iterable` containing all elements of this `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxIterable: GenIterable[thisEquaPath.EquaBox]

    /**
     * Returns an Iterator over the elements in this `EquaSet`.  Will return
     * the same Iterator if this instance is already an Iterator.
     *
     * @return an Iterator containing all elements of this  `EquaSet`.
     */
    def toIterator: Iterator[T]

    /**
     * Returns an Iterator over the `EquaBox`es in this `EquaSet`.  Will return
     * the same Iterator if this instance is already an Iterator.
     *
     * @return an Iterator containing all elements of this  `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxIterator: Iterator[thisEquaPath.EquaBox]

    /**
     * Converts this `EquaSet` to a list of `EquaBox`.
     *
     * @return a list containing all elements of this `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxList: List[thisEquaPath.EquaBox]

    /**
     * Converts this `EquaSet` to a list.
     *
     * @return a list containing all elements of this `EquaSet`.
     */
    def toList: List[T]

    /**
     * Converts this `EquaSet` to a map. This method is unavailable unless
     * the elements are members of Tuple2, each ((K, V)) becoming a key-value
     * pair in the map. Duplicate keys will be overwritten by later keys:
     * if this is an unordered `EquaSet`, which key is in the resulting map
     * is undefined.
     * @return a map containing all elements of this `EquaSet`.
     *
     * @return a map of type `immutable.Map[K, V]`
     * containing all key/value pairs of type `(K, V)` of this `EquaSet`.
     */
    def toMap[K, V](implicit ev: T <:< (K, V)): Map[K, V]

    /**
     * Converts this `EquaSet` to a `ParArray`.
     *
     * @return a `ParArray` containing all elements of this `EquaSet`.
     */
    def toParArray: ParArray[T]

    /**
     * Converts this `EquaSet` to a `ParArray` containing `EquaBox`es of elements.
     *
     * @return a `ParArray` containing all elements of this `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxParArray: ParArray[thisEquaPath.EquaBox]

    /**
     * Converts this `EquaSet` to a sequence. As with `toIterable`, it's lazy
     * in this default implementation, as this `TraversableOnce` may be
     * lazy and unevaluated.
     *
     * @return a sequence containing all elements of this `EquaSet`.
     */
    def toSeq: GenSeq[T]

    /**
     * Converts this `EquaSet` to a sequence containing `EquaBox`es of elements.
     * As with `toIterable`, it's lazy in this default implementation, as this
     * `TraversableOnce` may be lazy and unevaluated.
     *
     * @return a sequence containing all elements of this `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxSeq: GenSeq[thisEquaPath.EquaBox]

    /**
     * Converts this `EquaSet` to a set.
     *
     * @return a set containing all elements of this `EquaSet`.
     */
    def toSet: Set[T]

    /**
     * Converts this `EquaSet` to a set of `EquaBox`.
     *
     * @return a set containing all elements of this `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxSet: Set[thisEquaPath.EquaBox]

    /**
     * Converts this `EquaSet` to a stream.
     *
     * @return a stream containing all elements of this `EquaSet`.
     */
    def toStream: Stream[T]

    /**
     * Converts this `EquaSet` to a stream of `EquaBox`es containing the elements.
     *
     * @return a stream containing all elements of this `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxStream: Stream[thisEquaPath.EquaBox]

    /**
     * Converts this `EquaSet` to a `Traversable`.
     *
     * @return a Traversable containing all elements of this `EquaSet`.
     */
    def toTraversable: GenTraversable[T]

    /**
     * Converts this `EquaSet` to a `Traversable` of `EquaBox`es containing the elements.
     *
     * @return a Traversable containing all elements of this `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxTraversable: GenTraversable[thisEquaPath.EquaBox]

    /**
     * Converts this `EquaSet` to a Vector.
     *
     * @return a vector containing all elements of this `EquaSet`.
     */
    def toVector: Vector[T]

    /**
     * Converts this `EquaSet` to a Vector of `EquaBox`es containing the elements.
     *
     * @return a vector containing all elements of this `EquaSet`, boxed in `EquaBox`.
     */
    def toEquaBoxVector: Vector[thisEquaPath.EquaBox]

    /**
     * Transposes this `EquaSet` of traversable collections into
     * an `EquaSet` of `EquaSet`s.
     *
     * The resulting collection's type will be guided by the
     * static type of `EquaSet`. For example:
     *
     * {{{
     * val xs = EquaSet(
     * EquaSet(1, 2, 3),
     * EquaSet(4, 5, 6)).transpose
     * // xs == List(
     * // List(1, 4),
     * // List(2, 5),
     * // List(3, 6))
     *
     * val ys = Vector(
     * List(1, 2, 3),
     * List(4, 5, 6)).transpose
     * // ys == Vector(
     * // Vector(1, 4),
     * // Vector(2, 5),
     * // Vector(3, 6))
     * }}}
     *
     * @tparam B the type of the elements of each traversable collection.
     * @param asTraversable an implicit conversion which asserts that the
     * element type of this `EquaSet` is a `Traversable`.
     * @return a two-dimensional `EquaSet` of ${coll}s which has as ''n''th row
     * the ''n''th column of this `EquaSet`.
     * @throws `IllegalArgumentException` if all collections in this `EquaSet`
     * are not of the same size.
     */
    def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisEquaPath.EquaSet

    /**
     * Computes the union between of set and another set.
     *
     * @param that the set to form the union with.
     * @return a new set consisting of all elements that are in this
     * set or in the given set `that`.
     */
    def union(that: thisEquaPath.EquaSet): thisEquaPath.EquaSet

    /**
     * Converts this `EquaSet` of pairs into two collections of the first and second
     * half of each pair.
     *
     * {{{
     * val xs = `EquaSet`(
     * (1, "one"),
     * (2, "two"),
     * (3, "three")).unzip
     * // xs == (`EquaSet`(1, 2, 3),
     * // `EquaSet`(one, two, three))
     * }}}
     *
     * @tparam T1 the type of the first half of the element pairs
     * @tparam T2 the type of the second half of the element pairs
     * @param asPair an implicit conversion which asserts that the element type
     * of this `EquaSet` is a pair.
     * @return a pair of `EquaSet`s, containing the first, respectively second
     * half of each element pair of this `EquaSet`.
     */
    def unzip[T1, T2](t1EquaPath: EquaPath[T1], t2EquaPath: EquaPath[T2])(implicit asPair: T => (T1, T2)): (t1EquaPath.EquaSet, t2EquaPath.EquaSet)

    /**
     * Converts this `EquaSet` of triples into three collections of the first, second,
     * and third element of each triple.
     *
     * {{{
     * val xs = `EquaSet`(
     * (1, "one", '1'),
     * (2, "two", '2'),
     * (3, "three", '3')).unzip3
     * // xs == (`EquaSet`(1, 2, 3),
     * // `EquaSet`(one, two, three),
     * // `EquaSet`(1, 2, 3))
     * }}}
     *
     * @tparam T1 the type of the first member of the element triples
     * @tparam T2 the type of the second member of the element triples
     * @tparam T3 the type of the third member of the element triples
     * @param asTriple an implicit conversion which asserts that the element type
     * of this `EquaSet` is a triple.
     * @return a triple of `EquaSet`s, containing the first, second, respectively
     * third member of each element triple of this `EquaSet`.
     */
    def unzip3[T1, T2, T3](t1EquaPath: EquaPath[T1], t2EquaPath: EquaPath[T2], t3EquaPath: EquaPath[T3])(implicit asTriple: T => (T1, T2, T3)): (t1EquaPath.EquaSet, t2EquaPath.EquaSet, t3EquaPath.EquaSet)

    /**
     * Creates a non-strict view of this `EquaSet`.
     *
     * @return a non-strict view of this `EquaSet`.
     */
    def view: TraversableView[thisEquaPath.EquaBox, Set[thisEquaPath.EquaBox]]

    /**
     * Creates a non-strict view of a slice of this `EquaSet`.
     *
     * Note: the difference between `view` and `slice` is that `view` produces
     * a view of the current `EquaSet`, whereas `slice` produces a new `EquaSet`.
     *
     * Note: `view(from, to)` is equivalent to `view.slice(from, to)`
     * $orderDependent
     *
     * @param from the index of the first element of the view
     * @param until the index of the element following the view
     * @return a non-strict view of a slice of this `EquaSet`, starting at index `from`
     * and extending up to (but not including) index `until`.
     */
    def view(from: Int, until: Int): TraversableView[thisEquaPath.EquaBox, Set[thisEquaPath.EquaBox]]

    /**
     * Creates a non-strict filter of this `EquaSet`.
     *
     * Note: the difference between `c filter p` and `c withFilter p` is that
     * the former creates a new `EquaSet`, whereas the latter only
     * restricts the domain of subsequent `map`, `flatMap`, `foreach`,
     * and `withFilter` operations.
     *
     * @param p the predicate used to test elements.
     * @return an object of class `FilterMonadic`, which supports
     * `map`, `flatMap`, `foreach`, and `withFilter` operations.
     * All these operations apply to those elements of this `EquaSet`
     * which satisfy the predicate `p`.
     */
    def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)

    /**
     * A class supporting filtered operations. Instances of this class are
     * returned by method `withFilter`.
     */
    class WithFilter(p: T=> Boolean) {

      /**
       * Applies a function `f` to all elements of the outer `EquaSet` containing
       * this `WithFilter` instance that satisfy predicate `p`.
       *
       * @param f the function that is applied for its side-effect to every element.
       * The result of function `f` is discarded.
       *
       * @tparam U the type parameter describing the result of function `f`.
       * This result will always be ignored. Typically `U` is `Unit`,
       * but this is not necessary.
       *
       */
      def foreach[U](f: T => U): Unit =
        filter(p).foreach(f)

      /**
       * Builds a new `EquaSet` by applying a function to all elements of the
       * outer `EquaSet` containing this `WithFilter` instance that satisfy predicate `p`.
       *
       * @param f the function to apply to each element.
       * @return a new `EquaSet` resulting from applying
       * the given function `f` to each element of the outer `EquaSet`
       * that satisfies predicate `p` and collecting the results.
       *
       * @return a new `EquaSet` resulting from applying the given function
       * `f` to each element of the outer `EquaSet` that satisfies
       * predicate `p` and collecting the results.
       */
      def map(f: T => T): thisEquaPath.EquaSet =
        filter(p).map(f)

      /**
       * Builds a new `EquaSet` by applying a function to all elements of the
       * outer `EquaSet` containing this `WithFilter` instance that satisfy
       * predicate `p` and concatenating the results.
       *
       * @param f the function to apply to each element.
       * @return a new `EquaSet` resulting from applying
       * the given `EquaSet`-valued function `f` to each element
       * of the outer `EquaSet` that satisfies predicate `p` and
       * concatenating the results.
       *
       * @return a new `EquaSet` resulting from applying the given
       * `EquaSet`-valued function `f` to each element of the
       * outer `EquaSet` that satisfies predicate `p` and concatenating
       * the results.
       */
      def flatMap(f: T => thisEquaPath.EquaSet): thisEquaPath.EquaSet =
        filter(p).flatMap(f)

      /**
       * Further refines the filter for this `EquaSet`.
       *
       * @param q the predicate used to test elements.
       * @return an object of class `WithFilter`, which supports
       * `map`, `flatMap`, `foreach`, and `withFilter` operations.
       * All these operations apply to those elements of this `EquaSet` which
       * satisfy the predicate `q` in addition to the predicate `p`.
       */
      def withFilter(q: T => Boolean): WithFilter =
        new WithFilter(x => p(x) && q(x))
    }

    /**
     * Returns an `EquaSet` formed from this `EquaSet` and another iterable collection
     * by combining corresponding elements in pairs.
     * If one of the two collections is longer than the other, its remaining elements are ignored.
     *
     * @param that The iterable providing the second half of each result pair
     * @tparam U the type of the second half of the returned pairs
     * @return a `Set` containing pairs consisting of
     * corresponding elements of this `EquaSet` and that`. The length
     * of the returned collection is the minimum of the lengths of this `EquaSet` and `that`.
     *
     */
    def zip[U](that: GenIterable[U]): Set[(T, U)]

    /**
     * Returns an `EquaSet` formed from this `EquaSet` and another iterable collection
     * by combining corresponding elements in pairs.
     * If one of the two collections is shorter than the other,
     * placeholder elements are used to extend the shorter collection to the length of the longer.
     *
     * @param that the iterable providing the second half of each result pair
     * @param thisElem the element to be used to fill up the result if this `EquaSet` is shorter than `that`.
     * @param thatElem the element to be used to fill up the result if `that` is shorter than this `EquaSet`.
     * @return a new collection of type `That` containing pairs consisting of
     * corresponding elements of this `EquaSet` and `that`. The length
     * of the returned collection is the maximum of the lengths of this `EquaSet` and `that`.
     * If this `EquaSet` is shorter than `that`, `thisElem` values are used to pad the result.
     * If `that` is shorter than this `EquaSet`, `thatElem` values are used to pad the result.
     *
     */
    def zipAll[U, T1 >: T](that: GenIterable[U], thisElem: T1, thatElem: U): Set[(T1, U)]

    /**
     * Zips this `EquaSet` with its indices.
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
    def zipWithIndex: Set[(T, Int)]

    val path: thisEquaPath.type

    def copyInto(thatEquaPath: EquaPath[T]): thatEquaPath.EquaSet
  }

  class FastEquaBridge[S](from: List[S]) extends EquaBridge[S](from) {
    override def collect(pf: PartialFunction[S, T]): thisEquaPath.FastEquaSet =
      thisEquaPath.FastEquaSet.empty ++ (from collect pf)
    override def map(f: S => T): thisEquaPath.FastEquaSet =
      thisEquaPath.FastEquaSet.empty ++ (from map f)
    override def flatMap(f: S => thisEquaPath.EquaSet): thisEquaPath.FastEquaSet =
      thisEquaPath.FastEquaSet((from flatMap ((s: S) => f(s).toList)): _*)
    override def flatten(implicit cvt: S <:< thisEquaPath.EquaSet): thisEquaPath.FastEquaSet =
      flatMap((s: S) => cvt(s))
    override def scanLeft(z: T)(op: (T, S) => T): thisEquaPath.FastEquaSet =
      thisEquaPath.FastEquaSet(from.scanLeft(z)((t: T, s: S) => op(t, s)).toSeq: _*)
    override def scanRight(z: T)(op: (S, T) => T): thisEquaPath.FastEquaSet =
      thisEquaPath.FastEquaSet(from.scanRight(z)((s: S, t: T) => op(s, t)).toSeq: _*)
    override def filter(pred: S => Boolean): thisEquaPath.FastEquaBridge[S] =
      new thisEquaPath.FastEquaBridge(from.filter(pred))
    override def withFilter(pred: S => Boolean): FastWithFilter = new FastWithFilter(pred)

    /**
     * A class supporting filtered operations. Instances of this class are
     * returned by method `withFilter`.
     */
    class FastWithFilter(p: S => Boolean) extends WithFilter(p) {

      /**
       * Builds a new `FastEquaSet` by applying a function to all elements of the
       * outer `EquaBridge` containing this `FastWithFilter` instance that satisfy predicate `p`.
       *
       * @param f the function to apply to each element.
       * @return a new `FastEquaSet` resulting from applying
       * the given function `f` to each element of the outer `EquaBridge`
       * that satisfies predicate `p` and collecting the results.
       *
       * @return a new `FastEquaSet` resulting from applying the given function
       * `f` to each element of the outer `EquaBridge` that satisfies
       * predicate `p` and collecting the results.
       */
      override def map(f: S => T): thisEquaPath.FastEquaSet =
        filter(p).map(f)

      /**
       * Builds a new `FastEquaSet` by applying a function to all elements of the
       * outer `EquaBridge` containing this `FastWithFilter` instance that satisfy
       * predicate `p` and concatenating the results.
       *
       * @param f the function to apply to each element.
       * @return a new `FastEquaSet` resulting from applying
       * the given `EquaSet`-valued function `f` to each element
       * of the outer `EquaBridge` that satisfies predicate `p` and
       * concatenating the results.
       *
       * @return a new `FastEquaSet` resulting from applying the given
       * `EquaSet`-valued function `f` to each element of the
       * outer `EquaBridge` that satisfies predicate `p` and concatenating
       * the results.
       */
      override def flatMap(f: S => thisEquaPath.EquaSet): thisEquaPath.FastEquaSet =
        filter(p).flatMap(f)

      /**
       * Further refines the filter for this `EquaSet`.
       *
       * @param q the predicate used to test elements.
       * @return an object of class `FastWithFilter`, which supports
       * `map`, `flatMap`, `foreach`, and `withFilter` operations.
       * All these operations apply to those elements of this `EquaBridge` which
       * satisfy the predicate `q` in addition to the predicate `p`.
       */
      override def withFilter(q: S => Boolean): FastWithFilter =
        new FastWithFilter(x => p(x) && q(x))
    }
  }

  class FastEquaSet private[scalactic] (private val underlying: Set[EquaBox]) extends EquaSet { thisFastEquaSet =>
    def + (elem: T): thisEquaPath.FastEquaSet = new FastEquaSet(underlying + EquaBox(elem))
    def + (elem1: T, elem2: T, elem3: T*): thisEquaPath.FastEquaSet =
      new FastEquaSet(underlying + (EquaBox(elem1), EquaBox(elem2), elem3.map(EquaBox(_)): _*))
    def ++ (elems: GenTraversableOnce[T]): thisEquaPath.FastEquaSet =
      new FastEquaSet(underlying ++ elems.toList.map(EquaBox(_)))
    def ++ (that: thisEquaPath.EquaSet): thisEquaPath.FastEquaSet = new FastEquaSet(underlying ++ that.toEquaBoxSet)
    def - (elem: T): thisEquaPath.FastEquaSet = new FastEquaSet(underlying - EquaBox(elem))
    def - (elem1: T, elem2: T, elem3: T*): thisEquaPath.FastEquaSet =
      new FastEquaSet(underlying - (EquaBox(elem1), EquaBox(elem2), elem3.map(EquaBox(_)): _*))
    def --(elems: GenTraversableOnce[T]): thisEquaPath.FastEquaSet =
      new FastEquaSet(underlying -- elems.toList.map(EquaBox(_)))
    def --(that: thisEquaPath.EquaSet): thisEquaPath.FastEquaSet =
      new FastEquaSet(underlying -- that.toEquaBoxSet)
    def /:[B](z: B)(op: (B, T) => B): B =
      underlying./:(z)((b: B, e: EquaBox) => op(b, e.value))
    def :\[B](z: B)(op: (T, B) => B): B =
      underlying.:\(z)((e: EquaBox, b: B) => op(e.value, b))
    def | (that: thisEquaPath.EquaSet): thisEquaPath.FastEquaSet = this union that
    def & (that: thisEquaPath.EquaSet): thisEquaPath.FastEquaSet = this intersect that
    def &~ (that: thisEquaPath.EquaSet): thisEquaPath.FastEquaSet = this diff that
    def addString(b: StringBuilder): StringBuilder = underlying.toList.map(_.value).addString(b)
    def addString(b: StringBuilder, sep: String): StringBuilder = underlying.toList.map(_.value).addString(b, sep)
    def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.toList.map(_.value).addString(b, start, sep, end)
    def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B = underlying.aggregate(z)((b: B, e: EquaBox) => seqop(b, e.value), combop)
    def apply(elem: T): Boolean = underlying.apply(EquaBox(elem))
    def canEqual(that: Any): Boolean =
      that match {
        case thatEquaSet: EquaPath[_]#EquaSet => thatEquaSet.path.equality eq thisEquaPath.equality
        case _ => false
      }
    def collect(pf: PartialFunction[T, T]): thisEquaPath.FastEquaSet =
      new FastEquaSet(underlying collect { case hb: thisEquaPath.EquaBox if pf.isDefinedAt(hb.value) => EquaBox(pf(hb.value)) })
    def contains[U](elem: U)(implicit ev: U <:< T): Boolean = underlying.contains(EquaBox(elem))

    def copyToArray(xs: Array[thisEquaPath.EquaBox]): Unit = underlying.copyToArray(xs)
    def copyToArray(xs: Array[thisEquaPath.EquaBox], start: Int): Unit = underlying.copyToArray(xs, start)
    def copyToArray(xs: Array[thisEquaPath.EquaBox], start: Int, len: Int): Unit = underlying.copyToArray(xs, start, len)
    def copyToBuffer(dest: mutable.Buffer[thisEquaPath.EquaBox]): Unit = underlying.copyToBuffer(dest)
    def count(p: T => Boolean): Int = underlying.map(_.value).count(p)
    def diff(that: thisEquaPath.EquaSet): thisEquaPath.FastEquaSet =
      new FastEquaSet(underlying diff that.toEquaBoxSet)
    def drop(n: Int): thisEquaPath.FastEquaSet = new FastEquaSet(underlying.drop(n))
    def dropRight(n: Int): thisEquaPath.FastEquaSet = new FastEquaSet(underlying.dropRight(n))
    def dropWhile(pred: T => Boolean): thisEquaPath.FastEquaSet = new FastEquaSet(underlying.dropWhile((p: EquaBox) => pred(p.value)))
    override def equals(other: Any): Boolean = { 
      other match {
        case thatEquaSet: EquaPath[_]#EquaSet => 
          (thisEquaPath.equality eq thatEquaSet.path.equality) && underlying == thatEquaSet.toEquaBoxSet
        case _ => false
      }
    }
    def exists(pred: T => Boolean): Boolean = underlying.exists((box: EquaBox) => pred(box.value))
    def filter(pred: T => Boolean): thisEquaPath.FastEquaSet = new FastEquaSet(underlying.filter((box: EquaBox) => pred(box.value)))
    def filterNot(pred: T => Boolean): thisEquaPath.FastEquaSet = new FastEquaSet(underlying.filterNot((box: EquaBox) => pred(box.value)))
    def find(pred: T => Boolean): Option[T] = underlying.find((box: EquaBox) => pred(box.value)).map(_.value)
    def flatMap(f: T => thisEquaPath.EquaSet): thisEquaPath.FastEquaSet = new FastEquaSet(underlying.flatMap((box: EquaBox) => f(box.value).toEquaBoxList))
    /*
    // This is the problem with using an implicit EquaSet. We need the path defined before we can use
    // it in the function. So into is the only way to get for expressions.
    def potentialFlatMap[U](f: T => thatEquaPath.EquaSet)(implicit thatEquaPath: EquaPath[U]): thatEquaPath.EquaSet
    */
    /*def flatten[T >: EquaPath[].EquaSet]: EquaSet = {
      new FastEquaSet(underlying.toList.map(_.value.asInstanceOf[EquaSet].toList).reduce(_ ++ _).toSet)
    }*/
    /*def flatten[S, T >: EquaPath[S]#EquaSet]: EquaPath[S]#EquaSet = {
      if (underlying.isEmpty)
        new EquaPath()

      EquaPath[S]#EquaSet(underlying.toList.map(_.value.asInstanceOf[EquaSet].toList).reduce(_ ++ _): _*)
    }*/
    def fold[T1 >: T](z: T1)(op: (T1, T1) => T1): T1 = underlying.toList.map(_.value).fold[T1](z)(op)
    def foldLeft[B](z: B)(op: (B, T) => B): B = underlying.toList.map(_.value).foldLeft[B](z)(op)
    def foldRight[B](z: B)(op: (T, B) => B): B = underlying.toList.map(_.value).foldRight[B](z)(op)
    def forall(pred: T => Boolean): Boolean = underlying.toList.map(_.value).forall(pred)
    def foreach[U](f: T => U): Unit = underlying.toList.map(_.value).foreach(f)
    def groupBy[K](f: T => K): GenMap[K, thisEquaPath.FastEquaSet] = underlying.groupBy((box: EquaBox) => f(box.value)).map(t => (t._1, new FastEquaSet(t._2)))
    def grouped(size: Int): Iterator[thisEquaPath.FastEquaSet] = underlying.grouped(size).map(new FastEquaSet(_))
    def hasDefiniteSize: Boolean = underlying.hasDefiniteSize
    override def hashCode: Int = underlying.hashCode
    def head: T = underlying.head.value
    def headOption: Option[T] =
      underlying.headOption match {
        case Some(head) => Some(head.value)
        case None => None
      }
    def init: thisEquaPath.FastEquaSet = new FastEquaSet(underlying.init)
    def inits: Iterator[thisEquaPath.FastEquaSet] = underlying.inits.map(new FastEquaSet(_))
    def intersect(that: thisEquaPath.EquaSet): thisEquaPath.FastEquaSet =
      new FastEquaSet(underlying intersect that.toEquaBoxSet)
    def into[U](thatEquaPath: EquaPath[U]): thatEquaPath.FastEquaBridge[T] = new thatEquaPath.FastEquaBridge[T](underlying.toList.map(_.value))
    def isEmpty: Boolean = underlying.isEmpty
    def iterator: Iterator[T] = underlying.iterator.map(_.value)
    def last: T = underlying.last.value
    def lastOption: Option[T] =
      underlying.lastOption match {
        case Some(last) => Some(last.value)
        case None => None
      }
    def map(f: T => T): thisEquaPath.FastEquaSet = FastEquaSet(underlying.map((box: EquaBox) => f(box.value)).toList: _*)
    def max[T1 >: T](implicit ord: Ordering[T1]): T = underlying.toList.map(_.value).max(ord)
    def maxBy[B](f: T => B)(implicit cmp: Ordering[B]): T = underlying.toList.map(_.value).maxBy(f)
    def min[T1 >: T](implicit ord: Ordering[T1]): T = underlying.toList.map(_.value).min(ord)
    def minBy[B](f: T => B)(implicit cmp: Ordering[B]): T = underlying.toList.map(_.value).minBy(f)
    def mkString(start: String, sep: String, end: String): String = underlying.toList.map(_.value).mkString(start, sep, end)
    def mkString(sep: String): String = underlying.toList.map(_.value).mkString(sep)
    def mkString: String = underlying.toList.map(_.value).mkString
    def nonEmpty: Boolean = underlying.nonEmpty
    def partition(pred: T => Boolean): (thisEquaPath.FastEquaSet, thisEquaPath.FastEquaSet) = {
      val tuple2 = underlying.partition((box: EquaBox) => pred(box.value))
      (new FastEquaSet(tuple2._1), new FastEquaSet(tuple2._2))
    }
    def product[T1 >: T](implicit num: Numeric[T1]): T1 = underlying.toList.map(_.value).product(num)
    def reduce[T1 >: T](op: (T1, T1) => T1): T1 = underlying.toList.map(_.value).reduce(op)
    def reduceLeft[T1 >: T](op: (T1, T) => T1): T1 = underlying.toList.map(_.value).reduceLeft(op)
    def reduceLeftOption[T1 >: T](op: (T1, T) => T1): Option[T1] = underlying.toList.map(_.value).reduceLeftOption(op)
    def reduceOption[T1 >: T](op: (T1, T1) => T1): Option[T1] = underlying.toList.map(_.value).reduceOption(op)
    def reduceRight[T1 >: T](op: (T, T1) => T1): T1 = underlying.toList.map(_.value).reduceRight(op)
    def reduceRightOption[T1 >: T](op: (T, T1) => T1): Option[T1] = underlying.toList.map(_.value).reduceRightOption(op)
    def repr: Set[EquaBox] = underlying
    def sameElements[T1 >: T](that: GenIterable[T1]): Boolean = underlying.toList.map(_.value).sameElements(that)
    def scanLeft(z: T)(op: (T, T) => T): thisEquaPath.FastEquaSet = {
      val set = underlying.scanLeft(EquaBox(z))((b1: EquaBox, b2: EquaBox) => EquaBox(op(b1.value, b2.value)))
      new FastEquaSet(set)
    }
    def scanRight(z: T)(op: (T, T) => T): thisEquaPath.FastEquaSet = {
      val set = underlying.scanRight(EquaBox(z))((b1: EquaBox, b2: EquaBox) => EquaBox(op(b1.value, b2.value)))
      new FastEquaSet(set)
    }
    def size: Int = underlying.size
    def slice(unc_from: Int, unc_until: Int): thisEquaPath.FastEquaSet = new FastEquaSet(underlying.slice(unc_from, unc_until))
    def sliding(size: Int): Iterator[thisEquaPath.FastEquaSet] = underlying.sliding(size).map(new FastEquaSet(_))
    def sliding(size: Int, step: Int): Iterator[thisEquaPath.FastEquaSet] = underlying.sliding(size, step).map(new FastEquaSet(_))
    def span(pred: T => Boolean): (thisEquaPath.FastEquaSet, thisEquaPath.FastEquaSet) = {
      val (trueSet, falseSet) = underlying.span((box: EquaBox) => pred(box.value))
      (new FastEquaSet(trueSet), new FastEquaSet(falseSet))
    }
    def splitAt(n: Int): (thisEquaPath.FastEquaSet, thisEquaPath.FastEquaSet) = {
      val (trueSet, falseSet) = underlying.splitAt(n)
      (new FastEquaSet(trueSet), new FastEquaSet(falseSet))
    }
    def stringPrefix: String = "EquaSet"
    def subsetOf(that: thisEquaPath.EquaSet): Boolean = underlying.subsetOf(that.toEquaBoxSet)
    def subsets(len: Int): Iterator[thisEquaPath.FastEquaSet] = underlying.subsets(len).map(new FastEquaSet(_))
    def subsets: Iterator[thisEquaPath.FastEquaSet] = underlying.subsets.map(new FastEquaSet(_))
    def sum[T1 >: T](implicit num: Numeric[T1]): T1 = underlying.map(_.value).sum(num)
    def tail: thisEquaPath.FastEquaSet = new FastEquaSet(underlying.tail)
    def tails: Iterator[thisEquaPath.FastEquaSet] = underlying.tails.map(new FastEquaSet(_))
    def take(n: Int): thisEquaPath.FastEquaSet = new FastEquaSet(underlying.take(n))
    def takeRight(n: Int): thisEquaPath.FastEquaSet = new FastEquaSet(underlying.takeRight(n))
    def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, thisEquaPath.EquaBox, Col[thisEquaPath.EquaBox @uV]]): Col[thisEquaPath.EquaBox @uV] = underlying.to[Col]
    def toArray: Array[T] = {
      // A workaround becauase underlying.map(_.value).toArray does not work due to this weird error message:
      // No ClassTag available for T
      val arr = new Array[Any](underlying.size)
      underlying.map(_.value).copyToArray(arr)
      arr.asInstanceOf[Array[T]]
    }
    def toEquaBoxArray: Array[thisEquaPath.EquaBox] = underlying.toArray
    def toBuffer: scala.collection.mutable.Buffer[T] = underlying.map(_.value).toBuffer
    def toEquaBoxBuffer: scala.collection.mutable.Buffer[thisEquaPath.EquaBox] = underlying.toBuffer
    def toIndexedSeq: scala.collection.immutable.IndexedSeq[T] = underlying.map(_.value).toIndexedSeq
    def toEquaBoxIndexedSeq: scala.collection.immutable.IndexedSeq[thisEquaPath.EquaBox] = underlying.toIndexedSeq
    def toIterable: GenIterable[T] = underlying.toIterable.map(_.value)
    def toEquaBoxIterable: GenIterable[thisEquaPath.EquaBox] = underlying.toIterable
    def toIterator: Iterator[T] = underlying.toIterator.map(_.value)
    def toEquaBoxIterator: Iterator[thisEquaPath.EquaBox] = underlying.toIterator
    def toEquaBoxList: List[thisEquaPath.EquaBox] = underlying.toList
    def toList: List[T] = underlying.toList.map(_.value)
    def toMap[K, V](implicit ev: T <:< (K, V)): Map[K, V] = underlying.map(_.value).toMap
    def toParArray: ParArray[T] = underlying.toParArray.map(_.value)
    def toEquaBoxParArray: ParArray[thisEquaPath.EquaBox] = underlying.toParArray
    def toSeq: GenSeq[T] = underlying.toSeq.map(_.value)
    def toEquaBoxSeq: GenSeq[thisEquaPath.EquaBox] = underlying.toSeq
    def toSet: Set[T] = underlying.map(_.value)
    def toEquaBoxSet: Set[thisEquaPath.EquaBox] = underlying
    def toStream: Stream[T] = underlying.toStream.map(_.value)
    def toEquaBoxStream: Stream[thisEquaPath.EquaBox] = underlying.toStream
    def toTraversable: GenTraversable[T] = underlying.map(_.value)
    def toEquaBoxTraversable: GenTraversable[thisEquaPath.EquaBox] = underlying.toTraversable
    def toVector: Vector[T] = underlying.toVector.map(_.value)
    def toEquaBoxVector: Vector[thisEquaPath.EquaBox] = underlying.toVector
    // Be consistent with standard library. HashSet's toString is Set(1, 2, 3)
    override def toString: String = s"$stringPrefix(${underlying.toVector.map(_.value).mkString(", ")})"
    def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisEquaPath.FastEquaSet = {
      val listList: List[T] = underlying.toList.map(_.value).transpose.asInstanceOf[List[T]]  // should be safe cast
      new FastEquaSet(listList.map(EquaBox(_)).toSet)
    }
    def union(that: thisEquaPath.EquaSet): thisEquaPath.FastEquaSet =
      new FastEquaSet(underlying union that.toEquaBoxSet)
    def unzip[T1, T2](t1EquaPath: EquaPath[T1], t2EquaPath: EquaPath[T2])(implicit asPair: T => (T1, T2)): (t1EquaPath.FastEquaSet, t2EquaPath.FastEquaSet) = {
      val (t1, t2) =  underlying.toList.map(_.value).unzip(asPair)
      (t1EquaPath.FastEquaSet(t1: _*), t2EquaPath.FastEquaSet(t2: _*))
    }
    def unzip3[T1, T2, T3](t1EquaPath: EquaPath[T1], t2EquaPath: EquaPath[T2], t3EquaPath: EquaPath[T3])(implicit asTriple: T => (T1, T2, T3)): (t1EquaPath.FastEquaSet, t2EquaPath.FastEquaSet, t3EquaPath.FastEquaSet) = {
      val (t1, t2, t3) =  underlying.toList.map(_.value).unzip3(asTriple)
      (t1EquaPath.FastEquaSet(t1: _*), t2EquaPath.FastEquaSet(t2: _*), t3EquaPath.FastEquaSet(t3: _*))
    }
    def view: TraversableView[thisEquaPath.EquaBox, Set[thisEquaPath.EquaBox]] = underlying.toList.toSet.view
    def view(from: Int, until: Int): TraversableView[thisEquaPath.EquaBox, Set[thisEquaPath.EquaBox]] = underlying.toList.toSet.view(from, until)
    def zip[U](that: GenIterable[U]): Set[(T, U)] = underlying.toList.map(_.value).zip(that).toSet
    def zipAll[U, T1 >: T](that: GenIterable[U], thisElem: T1, thatElem: U): Set[(T1, U)] = underlying.toList.map(_.value).zipAll(that, thisElem, thatElem).toSet
    def zipWithIndex: Set[(T, Int)] = underlying.toList.map(_.value).zipWithIndex.toSet

    val path: thisEquaPath.type = thisEquaPath
    def copyInto(thatEquaPath: EquaPath[T]): thatEquaPath.FastEquaSet =
      if (thatEquaPath eq thisEquaPath)
        thisFastEquaSet.asInstanceOf[thatEquaPath.FastEquaSet]
      else
        thisFastEquaSet.into(thatEquaPath).map(t => t)
  }
  object FastEquaSet {
    def empty: FastEquaSet = new FastEquaSet(Set.empty)
    def apply(elems: T*): FastEquaSet = 
      new FastEquaSet(Set(elems.map(EquaBox(_)): _*))
  }
  object EquaSet {
    def empty: EquaSet = FastEquaSet.empty
    def apply(elems: T*): EquaSet = FastEquaSet(elems: _*)
    import scala.language.implicitConversions
    implicit def equaSetToGenTraversableOnce(equaSet: EquaSet): scala.collection.immutable.IndexedSeq[T] = equaSet.toVector
    // TODO: Study this one...
    implicit def flattenTraversableOnce[A, CC[_]](travs: TraversableOnce[EquaSet])(implicit ev: EquaSet => TraversableOnce[A]) =
      new scala.collection.TraversableOnce.FlattenOps[A](travs map ev)
  }
}

object EquaPath {
  def apply[T](equality: HashingEquality[T]): EquaPath[T] = new EquaPath(equality)
}
