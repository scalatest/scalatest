/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalactic.opaques

import scala.collection.{GenSeq, StringOps}
import scala.collection.mutable.Buffer
import org.scalactic.Every
import scala.annotation.targetName
import org.scalactic.Resources

object NonEmptyStrings {
  /**
    * A non-empty list: an ordered, immutable, non-empty collection of elements with <code>LinearSeq</code> performance characteristics.
    *
    * <p>
    * The purpose of <code>NonEmptyString</code> is to allow you to express in a type that a <code>String</code> is non-empty, thereby eliminating the
    * need for (and potential exception from) a run-time check for non-emptiness. For a non-empty sequence with <code>IndexedSeq</code>
    * performance, see <a href="Every.html"><code>Every</code></a>.
    * </p>
    *
    * <h2>Constructing <code>NonEmptyString</code>s</h2>
    *
    * <p>
    * You can construct a <code>NonEmptyString</code> by passing one or more elements to the <code>NonEmptyString.apply</code> factory method:
    * </p>
    *
    * <pre class="stHighlight">
    * scala&gt; NonEmptyString(1, 2, 3)
    * res0: org.scalactic.anyvals.NonEmptyString[Int] = NonEmptyString(1, 2, 3)
    * </pre>
    *
    * <p>
    * Alternatively you can <em>cons</em> elements onto the <code>End</code> singleton object, similar to making a <code>String</code> starting with <code>Nil</code>:
    * </p>
    *
    * <pre class="stHighlight">
    * scala&gt; 1 :: 2 :: 3 :: Nil
    * res0: String[Int] = String(1, 2, 3)
    *
    * scala&gt; 1 :: 2 :: 3 :: End
    * res1: org.scalactic.NonEmptyString[Int] = NonEmptyString(1, 2, 3)
    * </pre>
    *
    * <p>
    * Note that although <code>Nil</code> is a <code>String[Nothing]</code>, <code>End</code> is
    * not a <code>NonEmptyString[Nothing]</code>, because no empty <code>NonEmptyString</code> exists. (A non-empty list is a series
    * of connected links; if you have no links, you have no non-empty list.)
    * </p>
    *
    * <pre class="stHighlight">
    * scala&gt; val nil: String[Nothing] = Nil
    * nil: String[Nothing] = String()
    *
    * scala&gt; val nada: NonEmptyString[Nothing] = End
    * &lt;console&gt;:16: error: type mismatch;
    * found   : org.scalactic.anyvals.End.type
    * required: org.scalactic.anyvals.NonEmptyString[Nothing]
    *        val nada: NonEmptyString[Nothing] = End
    *                                          ^
    * </pre>
    *
    * <h2>Working with <code>NonEmptyString</code>s</h2>
    *
    * <p>
    * <code>NonEmptyString</code> does not extend Scala's <code>Seq</code> or <code>Traversable</code> traits because these require that
    * implementations may be empty. For example, if you invoke <code>tail</code> on a <code>Seq</code> that contains just one element,
    * you'll get an empty <code>Seq</code>:
    * </p>
    *
    * <pre class="stREPL">
    * scala&gt; String(1).tail
    * res6: String[Int] = String()
    * </pre>
    *
    * <p>
    * On the other hand, many useful methods exist on <code>Seq</code> that when invoked on a non-empty <code>Seq</code> are guaranteed
    * to not result in an empty <code>Seq</code>. For convenience, <code>NonEmptyString</code> defines a method corresponding to every such <code>Seq</code>
    * method. Here are some examples:
    * </p>
    *
    * <pre class="stHighlight">
    * NonEmptyString(1, 2, 3).map(_ + 1)                        // Result: NonEmptyString(2, 3, 4)
    * NonEmptyString(1).map(_ + 1)                              // Result: NonEmptyString(2)
    * NonEmptyString(1, 2, 3).containsSlice(NonEmptyString(2, 3)) // Result: true
    * NonEmptyString(1, 2, 3).containsSlice(NonEmptyString(3, 4)) // Result: false
    * NonEmptyString(-1, -2, 3, 4, 5).minBy(_.abs)              // Result: -1
    * </pre>
    *
    * <p>
    * <code>NonEmptyString</code> does <em>not</em> currently define any methods corresponding to <code>Seq</code> methods that could result in
    * an empty <code>Seq</code>. However, an implicit converison from <code>NonEmptyString</code> to <code>String</code>
    * is defined in the <code>NonEmptyString</code> companion object that will be applied if you attempt to call one of the missing methods. As a
    * result, you can invoke <code>filter</code> on an <code>NonEmptyString</code>, even though <code>filter</code> could result
    * in an empty sequence&mdash;but the result type will be <code>String</code> instead of <code>NonEmptyString</code>:
    * </p>
    *
    * <pre class="stHighlight">
    * NonEmptyString(1, 2, 3).filter(_ &lt; 10) // Result: String(1, 2, 3)
    * NonEmptyString(1, 2, 3).filter(_ &gt; 10) // Result: String()
    * </pre>
    *
    *
    * <p>
    * You can use <code>NonEmptyString</code>s in <code>for</code> expressions. The result will be an <code>NonEmptyString</code> unless
    * you use a filter (an <code>if</code> clause). Because filters are desugared to invocations of <code>filter</code>, the
    * result type will switch to a <code>String</code> at that point. Here are some examples:
    * </p>
    *
    * <pre class="stREPL">
    * scala&gt; import org.scalactic.anyvals._
    * import org.scalactic.anyvals._
    *
    * scala&gt; for (i &lt;- NonEmptyString(1, 2, 3)) yield i + 1
    * res0: org.scalactic.anyvals.NonEmptyString[Int] = NonEmptyString(2, 3, 4)
    *
    * scala&gt; for (i &lt;- NonEmptyString(1, 2, 3) if i &lt; 10) yield i + 1
    * res1: String[Int] = String(2, 3, 4)
    *
    * scala&gt; for {
    *      |   i &lt;- NonEmptyString(1, 2, 3)
    *      |   j &lt;- NonEmptyString('a', 'b', 'c')
    *      | } yield (i, j)
    * res3: org.scalactic.anyvals.NonEmptyString[(Int, Char)] =
    *         NonEmptyString((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
    *
    * scala&gt; for {
    *      |   i &lt;- NonEmptyString(1, 2, 3) if i &lt; 10
    *      |   j &lt;- NonEmptyString('a', 'b', 'c')
    *      | } yield (i, j)
    * res6: String[(Int, Char)] =
    *         String((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
    * </pre>
    *
    */
  opaque type NonEmptyString = String

  /**
    * Companion object for class <code>NonEmptyString</code>.
    */
  object NonEmptyString {
    /**
      * Constructs a new <code>NonEmptyString</code> given at least one element.
      *
      * @param s the <code>String</code> represented by this <code>NonEmptyString</code>
      */
    def apply(s: String): NonEmptyString = s

    /**
      * Constructs a new <code>NonEmptyString</code> given at least one character.
      *
      * @param firstChar the first character (with index 0) contained in this <code>NonEmptyString</code>
      * @param otherChars a varargs of zero or more other characters (with index 1, 2, 3, ...) contained in this <code>NonEmptyString</code>
      */
    def apply(firstChar: Char, otherChars: Char*): NonEmptyString = firstChar + otherChars.mkString

    /**
      * Variable argument extractor for <code>NonEmptyString</code>s.
      *
      * @param nonEmptyString: the <code>NonEmptyString</code> containing the elements to extract
      * @return an <code>Seq</code> containing this <code>NonEmptyString</code>s elements, wrapped in a <code>Some</code> 
      */
    def unapplySeq(nonEmptyString: NonEmptyString): Option[Seq[String]] = Some(Seq(nonEmptyString))

    /**
      * Optionally construct a <code>NonEmptyString</code> containing the characters, if any, of a given <code>GenSeq</code>.
      *
      * @param seq the <code>GenSeq</code> of <code>Char</code> with which to construct a <code>NonEmptyString</code>
      * @return a <code>NonEmptyString</code> containing the elements of the given <code>GenSeq</code>, if non-empty, wrapped in
      *     a <code>Some</code>; else <code>None</code> if the <code>GenSeq</code> is empty
      */
    def from[T](seq: GenSeq[Char]): Option[NonEmptyString] =
      seq.headOption match {
        case None => None
        case Some(first) => Some(NonEmptyString(seq.mkString))
      }  

    given Conversion[NonEmptyString, PartialFunction[Int, Char]] with {
      def apply(nonEmptyString: NonEmptyString): PartialFunction[Int, Char] =
        new PartialFunction[Int, Char] {
          def isDefinedAt(idx: Int): Boolean = idx >= 0 && idx < nonEmptyString.length
          def apply(idx: Int): Char = nonEmptyString.charAt(idx)
        }
    }

    given Conversion[NonEmptyString, IterableOnce[Char]] with {
      def apply(nonEmptyString: NonEmptyString): IterableOnce[Char] =
        new IterableOnce[Char] {
          def iterator: Iterator[Char] = new StringOps(nonEmptyString).iterator
        }
    }

    extension [T] (other: Char) {
      /**
        * Returns a new <code>NonEmptyString</code> containing the passed <code>Char</code> followed by this <code>NonEmptyString</code>.
        *
        * @param theString the <code>NonEmptyString</code> to append
        * @return a new <code>NonEmptyString</code> that contains <code>other</code> followed by this <code>NonEmptyString</code>.
        */
      def +:(theString: NonEmptyString): NonEmptyString = NonEmptyString(other.toString ++ theString.toString)
    }  

    extension (nonEmptyString: NonEmptyString) {

      /**
        * Returns a new <code>NonEmptyString</code> containing this <code>NonEmptyString</code> followed by the passed <code>NonEmptyString</code>.
        *
        * @param other the <code>NonEmptyString</code> to append
        * @return a new <code>NonEmptyString</code> that contains this <code>NonEmptyString</code> followed by <code>other</code>.
        */
      def ++(other: IterableOnce[Char]): NonEmptyString = nonEmptyString + other.mkString

      /**
        * Returns a new <code>NonEmptyString</code> with the given character appended.
        *
        * <p>
        * Note a mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
        * </p>
        *
        * @param c the character to append to this <code>NonEmptyString</code>
        * @return a new <code>NonEmptyString</code> consisting of all characters of this <code>NonEmptyString</code> followed by the given <code>c</code>.
        */
      def :+(c: Char): NonEmptyString = new NonEmptyString(new StringOps(nonEmptyString) :+ c)

      /**
        * Appends all characters of this <code>NonEmptyString</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
        * on of every element of this <code>NonEmptyString</code>, without any separator string.
        *
        * @param sb the string builder to which characters will be appended
        * @return the string builder, <code>sb</code>, to which elements were appended.
        */
      def addString(sb: StringBuilder): StringBuilder = new StringOps(nonEmptyString).addString(sb)

      /**
        * Appends all characters of this <code>NonEmptyString</code> to a string builder using a separator string. The written text will consist of a concatenation of the
        * result of invoking <code>toString</code>
        * on of every character of this <code>NonEmptyString</code>, separated by the string <code>sep</code>.
        *
        * @param sb the string builder to which characters will be appended
        * @param sep the separator string
        * @return the string builder, <code>sb</code>, to which characters were appended.
        */
      def addString(sb: StringBuilder, sep: String): StringBuilder = new StringOps(nonEmptyString).addString(sb, sep)

      /**
        * Appends all characters of this <code>NonEmptyString</code> to a string builder using start, end, and separator strings. The written text will consist of a concatenation of
        * the string <code>start</code>; the result of invoking <code>toString</code> on all characters of this <code>NonEmptyString</code>,
        * separated by the string <code>sep</code>; and the string <code>end</code>
        *
        * @param sb the string builder to which characters will be appended
        * @param start the starting string
        * @param sep the separator string
        * @param end the ending string
        * @return the string builder, <code>sb</code>, to which characters were appended.
        */
      def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = new StringOps(nonEmptyString).addString(sb, start, sep, end)

      /**
        * Tests whether this <code>NonEmptyString</code> can be compared for equality with the given object.
        *
        * @param that the object to test
        * @return true if this <code>NonEmptyString</code> can be compared for equality with <code>that</code>, false otherwise.
        */
      def canEqual(that: Any): Boolean = that.isInstanceOf[NonEmptyString] || that.isInstanceOf[String]

      /**
        * Gets a character by its index in the <code>NonEmptyString</code>.
        *
        * @return the character of this <code>NonEmptyString</code> at index <code>idx</code>, where 0 indicates the first element.
        */
      def charAt(idx: Int): Char = nonEmptyString.charAt(idx)

      /**
        * Finds the first character of this <code>NonEmptyString</code> for which the given partial function is defined, if any, and applies the partial function to it.
        *
        * @param pf the partial function
        * @return an <code>Option</code> containing <code>pf</code> applied to the first character for which it is defined, or <code>None</code> if
        *    the partial function was not defined for any character.
        */
      def collectFirst[U](pf: PartialFunction[Char, U]): Option[U] = 
        nonEmptyString.find(c => pf.isDefinedAt(c)).map(c => pf(c))

      /**
        * Indicates whether this <code>NonEmptyString</code> contains a given value as an character.
        *
        * @param c the element to look for
        * @return true if this <code>NonEmptyString</code> has an character that is equal (as determined by <code>==)</code> to <code>c</code>, false otherwise.
        */
      def contains(c: Char): Boolean = nonEmptyString.indexOf(c) >= 0  

      /**
        * Indicates whether this <code>NonEmptyString</code> contains a given <code>IterableOnce</code> of characters as a slice.
        *
        * @param that the <code>IterableOnce</code> character slice to look for
        * @return true if this <code>NonEmptyString</code> contains a slice with the same characters as <code>that</code>, otherwise <code>false</code>.
        */
      def containsSlice(that: IterableOnce[Char]): Boolean = nonEmptyString.indexOf(that.mkString) >= 0

      /**
        * Copies characters of this <code>NonEmptyString</code> to an array. Fills the given array <code>arr</code> with characters of this <code>NonEmptyString</code>. Copying
        * will stop once either the end of the current <code>NonEmptyString</code> is reached, or the end of the array is reached.
        *
        * @param arr the array to fill
        */
      def copyToArray(arr: Array[Char]): Unit = new StringOps(nonEmptyString).copyToArray(arr, 0)

      /**
        * Copies characters of this <code>NonEmptyString</code> to an array. Fills the given array <code>arr</code> with characters of this <code>NonEmptyString</code>, beginning at
        * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyString</code> is reached, or the end of the array is reached.
        *
        * @param arr the array to fill
        * @param start the starting index
        */
      def copyToArray(arr: Array[Char], start: Int): Unit = new StringOps(nonEmptyString).copyToArray(arr, start)

      /**
        * Copies characters of this <code>NonEmptyString</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> characters of this <code>NonEmptyString</code>, beginning at
        * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyString</code> is reached, the end of the array is reached, or
        * <code>len</code> elements have been copied.
        *
        * @param arr the array to fill
        * @param start the starting index
        * @param len the maximum number of elements to copy
        */
      def copyToArray(arr: Array[Char], start: Int, len: Int): Unit = new StringOps(nonEmptyString).copyToArray(arr, start, len)

      /**
        * Copies all characters of this <code>NonEmptyString</code> to a buffer.
        *
        * @param buf the buffer to which characters are copied
        */
      def copyToBuffer(buf: Buffer[Char]): Unit = nonEmptyString.toList.copyToBuffer(buf)

      /**
        * Indicates whether every character of this <code>NonEmptyString</code> relates to the corresponding element of a given <code>IterableOnce</code> by satisfying a given predicate.
        *
        * @tparam B the type of the elements of <code>that</code>
        * @param that the <code>IterableOnce</code> to compare for correspondence
        * @param p the predicate, which relates elements from this <code>NonEmptyString</code> and the passed <code>IterableOnce</code>
        * @return true if this <code>NonEmptyString</code> and the passed <code>IterableOnce</code> have the same length and <code>p(x, y)</code> is <code>true</code>
        *     for all corresponding elements <code>x</code> of this <code>NonEmptyString</code> and <code>y</code> of that, otherwise <code>false</code>.
        */
      def corresponds[B](that: IterableOnce[B])(p: (Char, B) => Boolean): Boolean = nonEmptyString.toList.corresponds(that)(p)

      /**
        * Counts the number of characters in this <code>NonEmptyString</code> that satisfy a predicate.
        *
        * @param p the predicate used to test characters.
        * @return the number of characters satisfying the predicate <code>p</code>.
        */
      def count(p: Char => Boolean): Int = new StringOps(nonEmptyString).count(p)

      /**
        * Builds a new <code>NonEmptyString</code> from this <code>NonEmptyString</code> without any duplicate characters.
        *
        * @return A new <code>NonEmptyString</code> that contains the first occurrence of every character of this <code>NonEmptyString</code>.
        */
      def distinct: NonEmptyString = new StringOps(nonEmptyString).distinct

      /**
        * Selects a character by its index in the <code>NonEmptyString</code>.
        *
        * @return the character of this <code>NonEmptyString</code> at index <code>idx</code>, where 0 indicates the first element.
        */
      //def apply(idx: Int): Char = nonEmptyString(idx)

      /**
        * The length of this <code>NonEmptyString</code>.
        *
        * <p>
        * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
        * </p>
        *
        * @return the number of characters in this <code>NonEmptyString</code>.
        */
      def length: Int = nonEmptyString.toString.length

      /**
        * Indicates whether this <code>NonEmptyString</code> ends with the given <code>IterableOnce</code>.
        *
        * @param that the <code>IterableOnce</code> to test
        * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
        */
      def endsWith(that: IterableOnce[Char]): Boolean = nonEmptyString.endsWith(that.mkString)

      /**
        * Indicates whether a predicate holds for at least one of the characters of this <code>NonEmptyString</code>.
        *
        * @param p the predicate used to test characters.
        * @return <code>true</code> if the given predicate <code>p</code> holds for some of the elements of this <code>NonEmptyString</code>, otherwise <code>false</code>. 
        */
      def exists(p: Char => Boolean): Boolean = new StringOps(nonEmptyString).exists(p)

      /**
        * Finds the first character of this <code>NonEmptyString</code> that satisfies the given predicate, if any.
        *
        * @param p the predicate used to test characters
        * @return an <code>Some</code> containing the first character in this <code>NonEmptyString</code> that satisfies <code>p</code>, or <code>None</code> if none exists.
        */
      def find(p: Char => Boolean): Option[Char] = new StringOps(nonEmptyString).find(p)

      /**
        * Builds a new <code>NonEmptyString</code> by applying a function to all characters of this <code>NonEmptyString</code> and using the characters of the resulting <code>NonEmptyString</code>s.
        *
        * @param f the function to apply to each character.
        * @return a new <code>NonEmptyString</code> containing characters obtained by applying the given function <code>f</code> to each character of this <code>NonEmptyString</code> and concatenating
        *    the characters of resulting <code>NonEmptyString</code>s.
        */
      def flatMap(f: Char => NonEmptyString): NonEmptyString = new StringOps(nonEmptyString).flatMap(f)

      /**
        * Builds a new <code>NonEmptyString</code> by applying a function to all characters of this <code>NonEmptyString</code>.
        *
        * @tparam U the character type of the returned <code>NonEmptyString</code>.
        * @param f the function to apply to each character.
        * @return a new <code>NonEmptyString</code> resulting from applying the given function <code>f</code> to each character of this <code>NonEmptyString</code> and collecting the results.
        */
      def map[U](f: Char => U): NonEmptyString =
        new StringOps(nonEmptyString).map { c =>
          f(c).toString
        }.mkString

      /**
        * Folds the characters of this <code>NonEmptyString</code> using the specified associative binary operator.
        *
        * <p>
        * The order in which operations are performed on characters is unspecified and may be nondeterministic.
        * </p>
        *
        * @param z a neutral character for the fold operation; may be added to the result an arbitrary number of
        *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
        *     0 for addition, or 1 for multiplication.)
        * @param op a binary operator that must be associative
        * @return the result of applying fold operator <code>op</code> between all the elements and <code>z</code>
        */
      def fold(z: Char)(op: (Char, Char) => Char): Char = new StringOps(nonEmptyString).fold(z)(op)

      /**
        * Applies a binary operator to a start value and all characters of this <code>NonEmptyString</code>, going left to right.
        *
        * @tparam B the result type of the binary operator.
        * @param z the start value.
        * @param op the binary operator.
        * @return the result of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>, going left to right, with the start value,
        *     <code>z</code>, on the left:
        *
        * <pre>
        * op(...op(op(z, x_1), x_2), ..., x_n)
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyString</code>. 
        * </p>
        */
      def foldLeft[B](z: B)(op: (B, Char) => B): B = new StringOps(nonEmptyString).foldLeft(z)(op)

      /**
        * Applies a binary operator to all characters of this <code>NonEmptyString</code> and a start value, going right to left.
        *
        * @tparam B the result of the binary operator
        * @param z the start value
        * @param op the binary operator
        * @return the result of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>, going right to left, with the start value,
        *     <code>z</code>, on the right:
        *
        * <pre>
        * op(x_1, op(x_2, ... op(x_n, z)...))
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyString</code>. 
        * </p>
        */
      def foldRight[B](z: B)(op: (Char, B) => B): B = new StringOps(nonEmptyString).foldRight(z)(op)  

      /**
        * Indicates whether a predicate holds for all characters of this <code>NonEmptyString</code>.
        *
        * @param p the predicate used to test characters.
        * @return <code>true</code> if the given predicate <code>p</code> holds for all characters of this <code>NonEmptyString</code>, otherwise <code>false</code>.
        */
      def forall(p: Char => Boolean): Boolean = new StringOps(nonEmptyString).forall(p)

      /**
        * Applies a function <code>f</code> to all characters of this <code>NonEmptyString</code>.
        *
        * @param f the function that is applied for its side-effect to every character. The result of function <code>f</code> is discarded.
        */
      def foreach(f: Char => Unit): Unit = new StringOps(nonEmptyString).foreach(f)

      /**
        * Partitions this <code>NonEmptyString</code> into a map of <code>NonEmptyString</code>s according to some discriminator function.
        *
        * @tparam K the type of keys returned by the discriminator function.
        * @param f the discriminator function.
        * @return A map from keys to <code>NonEmptyString</code>s such that the following invariant holds:
        *
        * <pre>
        * (nonEmptyString.toString partition f)(k) = xs filter (x =&gt; f(x) == k)
        * </pre>
        *
        * <p>
        * That is, every key <code>k</code> is bound to a <code>NonEmptyString</code> of those elements <code>x</code> for which <code>f(x)</code> equals <code>k</code>.
        * </p>
        */
      def groupBy[K](f: Char => K): Map[K, NonEmptyString] = {
        val mapKToString = new StringOps(nonEmptyString).groupBy(f)
        mapKToString.mapValues { list => new NonEmptyString(list) }.toMap
      }

      /**
        * Partitions characters into fixed size <code>NonEmptyString</code>s.
        *
        * @param size the number of characters per group
        * @return An iterator producing <code>NonEmptyString</code>s of size <code>size</code>, except the last will be truncated if the characters don't divide evenly.
        */
      def grouped(size: Int): Iterator[NonEmptyString] = {
        if (size > 0) {
          val itOfString = new StringOps(nonEmptyString).grouped(size)
          itOfString.map { list => new NonEmptyString(list) }
        }
        else
          throw new IllegalArgumentException(Resources.invalidSize(size))
      }

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyString</code> has a definite size, since all <code>NonEmptyString</code>s are strict collections.
        */
      def hasDefiniteSize: Boolean = true

      // override def hashCode: Int = toString.hashCode

      /**
        * Selects the first character of this <code>NonEmptyString</code>.
        *
        * @return the first character of this <code>NonEmptyString</code>.
        */
      def head: Char = nonEmptyString.charAt(0) // Can never be empty, so safe

      // Methods like headOption I can't get rid of because of the implicit conversion to Iterable.
      // Users can call any of the methods I've left out on a NonEmptyString, and get whatever String would return
      // for that method call. Eventually I'll probably implement them all to save the implicit conversion.

      /**
        * Selects the first character of this <code>NonEmptyString</code> and returns it wrapped in a <code>Some</code>.
        *
        * @return the first character of this <code>NonEmptyString</code>, wrapped in a <code>Some</code>.
        */
      def headOption: Option[Char] = Some(head)

      /**
        * Finds index of first occurrence of some value in this <code>NonEmptyString</code>.
        *
        * @param c the character value to search for.
        * @return the index of the first character of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>) to <code>c</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexOf(c: Char): Int = nonEmptyString.toList.indexOf(c, 0)

      /**
        * Finds index of first occurrence of some value in this <code>NonEmptyString</code> after or at some start index.
        *
        * @param c the character value to search for.
        * @param from the start index
        * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexOf(c: Char, from: Int): Int = nonEmptyString.toList.indexOf(c, from)

      /**
        * Finds first index where this <code>NonEmptyString</code> contains a given <code>IterableOnce[Char]</code> as a slice.
        *
        * @param that the <code>IterableOnce[Char]</code> defining the slice to look for
        * @return the first index at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
        *     <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists.
        */
      def indexOfSlice(that: IterableOnce[Char]): Int = nonEmptyString.toIndexedSeq.indexOfSlice(that.mkString)

      /**
        * Finds first index after or at a start index where this <code>NonEmptyString</code> contains a given <code>IterableOnce[Char]</code> as a slice.
        *
        * @param that the <code>IterableOnce[Char]</code> defining the slice to look for
        * @param from the start index
        * @return the first index <code>&gt;=</code> <code>from</code> at which the characters of this <code>NonEmptyString</code> starting at that index match the characters of
        *     <code>IterableOnce[Char]</code> <code>that</code>, or <code>-1</code> of no such subsequence exists.
        */
      def indexOfSlice(that: IterableOnce[Char], from: Int): Int = {
        val normalizedFrom = math.max(0, from)
        val seq: Seq[Char] = nonEmptyString.toSeq
        val searchSeq: Seq[Char] = that.iterator.toSeq
        seq.indexOfSlice(searchSeq, normalizedFrom)
      }

      /**
        * Finds index of the first character satisfying some predicate.
        *
        * @param p the predicate used to test characters.
        * @return the index of the first character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexWhere(p: Char => Boolean): Int = new StringOps(nonEmptyString).indexWhere(p)

      /**
        * Finds index of the first character satisfying some predicate after or at some start index.
        *
        * @param p the predicate used to test characters.
        * @param from the start index
        * @return the index <code>&gt;=</code> <code>from</code> of the first character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexWhere(p: Char => Boolean, from: Int): Int = new StringOps(nonEmptyString).indexWhere(p, from)

      /**
        * Produces the range of all indices of this <code>NonEmptyString</code>. 
        *
        * @return a <code>Range</code> value from <code>0</code> to one less than the length of this <code>NonEmptyString</code>. 
        */
      def indices: Range = new StringOps(nonEmptyString).indices

      /**
        * Returns <code>false</code> to indicate this <code>NonEmptyString</code>, like all <code>NonEmptyString</code>s, is non-empty.
        *
        * @return false
        */
      def isEmpty: Boolean = false

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyString</code>, like all <code>NonEmptyString</code>s, can be traversed repeatedly.
        *
        * @return true
        */
      def isTraversableAgain: Boolean = true

      /**
        * Selects the last character of this <code>NonEmptyString</code>.
        *
        * @return the last character of this <code>NonEmptyString</code>.
        */
      def last: Char = if (length > 1) nonEmptyString.charAt(length - 1) else nonEmptyString.charAt(0) // Can never be empty, so safe

      /**
        * Finds the index of the last occurrence of some value in this <code>NonEmptyString</code>.
        *
        * @param c the character value to search for.
        * @return the index of the last character of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>) to <code>c</code>,
        *     or <code>-1</code>, if none exists.
        */
      def lastIndexOf(c: Char): Int = nonEmptyString.toList.lastIndexOf(c)

      /**
        * Finds the index of the last occurrence of some value in this <code>NonEmptyString</code> before or at a given <code>end</code> index.
        *
        * @param c the character value to search for.
        * @param end the end index. 
        * @return the index <code>&gt;=</code> <code>end</code> of the last character of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>)
        *     to <code>elem</code>, or <code>-1</code>, if none exists.
        */
      def lastIndexOf(c: Char, end: Int): Int = nonEmptyString.toList.lastIndexOf(c, end)

      /**
        * Finds the last index where this <code>NonEmptyString</code> contains a given <code>Every</code> as a slice. 
        *
        * @param that the <code>Every</code> defining the slice to look for
        * @return the last index at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
        *    <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def lastIndexOfSlice(that: IterableOnce[Char]): Int = nonEmptyString.toIndexedSeq.lastIndexOfSlice(that.toVector)

      /**
        * Finds the last index before or at a given end index where this <code>NonEmptyString</code> contains a given <code>Every</code> as a slice. 
        *
        * @param that the <code>Every</code> defining the slice to look for
        * @param end the end index
        * @return the last index <code>&gt;=</code> <code>end</code> at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
        *    <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def lastIndexOfSlice(that: IterableOnce[Char], end: Int): Int = nonEmptyString.toIndexedSeq.lastIndexOfSlice(that.toVector, end)

       /**
        * Finds index of last character satisfying some predicate.
        *
        * @param p the predicate used to test characters.
        * @return the index of the last character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>, or <code>-1</code>, if none exists.
        */
      def lastIndexWhere(p: Char => Boolean): Int = nonEmptyString.toList.lastIndexWhere(p)

      /**
        * Finds index of last character satisfying some predicate before or at given end index.
        *
        * @param p the predicate used to test characters.
        * @param end the end index
        * @return the index <code>&gt;=</code> <code>end</code> of the last character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists. 
        */
      def lastIndexWhere(p: Char => Boolean, end: Int): Int = nonEmptyString.toList.lastIndexWhere(p, end)

      /**
        * Returns the last element of this <code>NonEmptyString</code>, wrapped in a <code>Some</code>. 
        *
        * @return the last element, wrapped in a <code>Some</code>. 
        */
      def lastOption: Option[Char] = nonEmptyString.toList.lastOption // Will always return a Some

      /**
        * Compares the length of this <code>NonEmptyString</code> to a test value. 
        *
        * @param len the test value that gets compared with the length.
        * @return a value <code>x</code> where
        *
        * <pre>
        * x &lt; 0 if this.length &lt; len
        * x == 0 if this.length == len
        * x &gt; 0 if this.length &gt; len
        * </pre>
        */
      def lengthCompare(len: Int): Int = new StringOps(nonEmptyString).lengthCompare(len)

      /**
        * Finds the largest character.
        *
        * @return the largest element of this <code>NonEmptyString</code>. 
        */
      def max(using cmp: Ordering[Char]): Char = nonEmptyString.toList.max(cmp)

      /**
        * Finds the largest result after applying the given function to every character.
        *
        * @return the largest result of applying the given function to every character of this <code>NonEmptyString</code>.
        */
      def maxBy[U](f: Char => U)(using cmp: Ordering[U]): Char = nonEmptyString.toList.maxBy(f)(cmp)

      /**
        * Finds the smallest character.
        *
        * @return the smallest character of this <code>NonEmptyString</code>.
        */
      def min(using cmp: Ordering[Char]): Char = nonEmptyString.toList.min(cmp)

      /**
        * Finds the smallest result after applying the given function to every character.
        *
        * @return the smallest result of applying the given function to every character of this <code>NonEmptyString</code>.
        */
      def minBy[U](f: Char => U)(using cmp: Ordering[U]): Char = nonEmptyString.toList.minBy(f)(cmp)

      /**
        * Displays all characters of this <code>NonEmptyString</code> in a string.
        *
        * @return a string representation of this <code>NonEmptyString</code>. In the resulting string, the result of invoking <code>toString</code> on all characters of this
        *     <code>NonEmptyString</code> follow each other without any separator string. 
        */
      def mkString: NonEmptyString = new StringOps(nonEmptyString).mkString

      /**
        * Displays all elements of this <code>NonEmptyString</code> in a string using a separator string. 
        *
        * @param sep the separator string
        * @return a string representation of this <code>NonEmptyString</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
        *     <code>NonEmptyString</code> are separated by the string <code>sep</code>. 
        */
      def mkString(sep: String): NonEmptyString = new StringOps(nonEmptyString).mkString(sep)

      /**
        * Displays all characters of this <code>NonEmptyString</code> in a string using start, end, and separator strings.
        *
        * @param start the starting string.
        * @param sep the separator string.
        * @param end the ending string.
        * @return a string representation of this <code>NonEmptyString</code>. The resulting string begins with the string <code>start</code> and ends with the string
        *     <code>end</code>. Inside, In the resulting string, the result of invoking <code>toString</code> on all characters of this <code>NonEmptyString</code> are
        *     separated by the string <code>sep</code>. 
        */
      def mkString(start: String, sep: String, end: String): NonEmptyString = new StringOps(nonEmptyString).mkString(start, sep, end)

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyString</code>, like all <code>NonEmptyString</code>s, is non-empty.
        *
        * @return true
        */
      def nonEmpty: Boolean = true

      /**
        * A copy of this <code>NonEmptyString</code> with an element value appended until a given target length is reached.
        *
        * @param len the target length 
        * @param c the padding character
        * @return a new <code>NonEmptyString</code> consisting of all characters of this <code>NonEmptyString</code> followed by the minimal number of occurrences
        *     of <code>elem</code> so that the resulting <code>NonEmptyString</code> has a length of at least <code>len</code>. 
        */
      def padTo(len: Int, c: Char): NonEmptyString =
        new StringOps(nonEmptyString).padTo(len, c)

      /**
        * Produces a new <code>NonEmptyString</code> where a slice of characters in this <code>NonEmptyString</code> is replaced by another <code>NonEmptyString</code>
        *
        * @param from the index of the first replaced character
        * @param that the <code>NonEmptyString</code> whose characters should replace a slice in this <code>NonEmptyString</code>
        * @param replaced the number of characters to drop in the original <code>NonEmptyString</code>
        */
      def patch(from: Int, that: NonEmptyString, replaced: Int): NonEmptyString =
        new StringOps(nonEmptyString).patch(from, that, replaced)

      /**
        * Iterates over distinct permutations. 
        *
        * <p>
        * Here's an example:
        * </p>
        *
        * <pre class="stHighlight">
        * NonEmptyString("abb").permutations.toList == list(NonEmptyString("abb"), NonEmptyString("bab"), NonEmptyString("bba"))
        * </pre>
        *
        * @return an iterator that traverses the distinct permutations of this <code>NonEmptyString</code>.
        */
      def permutations: Iterator[NonEmptyString] = {
        val it = new StringOps(nonEmptyString).permutations
        it map { list => new NonEmptyString(list) }
      }

      /**
        * Returns the length of the longest prefix whose characters all satisfy some predicate.
        *
        * @param p the predicate used to test characters.
        * @return the length of the longest prefix of this <code>NonEmptyString</code> such that every characters
        *     of the segment satisfies the predicate <code>p</code>. 
        */
      def prefixLength(p: Char => Boolean): Int = nonEmptyString.toList.prefixLength(p)

      /**
        * The result of multiplying all the characters of this <code>NonEmptyString</code>.
        *
        * <p>
        * This method can be invoked for any <code>NonEmptyString</code> for which an implicit <code>Numeric[T]</code> exists.
        * </p>
        *
        * @return the product of all elements
        */
      def product(using num: Numeric[Char]): Char = nonEmptyString.toList.product(num)

      /**
        * Reduces the elements of this <code>NonEmptyString</code> using the specified associative binary operator.
        *
        * <p>
        * The order in which operations are performed on characters is unspecified and may be nondeterministic.
        * </p>
        *
        * @param op a binary operator that must be associative.
        * @return the result of applying reduce operator <code>op</code> between all the characters of this <code>NonEmptyString</code>.
        */
      def reduce(op: (Char, Char) => Char): Char = nonEmptyString.toList.reduce(op)

      /**
        * Applies a binary operator to all characters of this <code>NonEmptyString</code>, going left to right.
        *
        * @param op the binary operator.
        * @return the result of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>, going left to right:
        *
        * <pre>
        * op(...op(op(x_1, x_2), x_3), ..., x_n)
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the characters of this <code>NonEmptyString</code>.
        * </p>
        */
      def reduceLeft(op: (Char, Char) => Char): Char = nonEmptyString.toList.reduceLeft(op)

      /**
        * Applies a binary operator to all characters of this <code>NonEmptyString</code>, going left to right, returning the result in a <code>Some</code>.
        *
        * @param op the binary operator.
        * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
        * </p>
        */
      def reduceLeftOption(op: (Char, Char) => Char): Option[Char] = nonEmptyString.toList.reduceLeftOption(op)

      /** 
        * If this <code>String</code> is nonempty, reduces it with the given binary operator `op`.
        *
        *  The behavior is the same as <code>reduce</code> except that the value is `None` if the <code>String</code>
        *  is empty.
        *
        *  @param op      A binary operator; must be associative for the result to always be the
        *                 same across runs.
        *  @return        The result of reducing this <code>String</code> with `op` if the <code>String</code> is nonempty,
        *                 inside a `Some`, and `None` otherwise.
        */
      def reduceOption(op: (Char, Char) => Char): Option[Char] = nonEmptyString.toList.reduceOption(op)

      /**
        * Applies a binary operator to all characters of this <code>NonEmptyString</code>, going right to left.
        *
        * @param op the binary operator
        * @return the result of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>, going right to left:
        *
        * <pre>
        * op(x_1, op(x_2, ... op(x_{n-1}, x_n)...))
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyString</code>. 
        * </p>
        */
      def reduceRight(op: (Char, Char) => Char): Char = nonEmptyString.toList.reduceRight(op)

      /**
        * Applies a binary operator to all elements of this <code>NonEmptyString</code>, going right to left, returning the result in a <code>Some</code>.
        *
        * @param op the binary operator
        * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
        */
      def reduceRightOption(op: (Char, Char) => Char): Option[Char] = nonEmptyString.toList.reduceRightOption(op)

      /**
        * Returns new <code>NonEmptyString</code> with characters in reverse order.
        *
        * @return a new <code>NonEmptyString</code> with all characters of this <code>NonEmptyString</code> in reversed order.
        */
      def reverse: NonEmptyString =
        new StringOps(nonEmptyString).reverse

      /**
        * An iterator yielding characters in reverse order.
        *
        * <p>
        * Note: <code>nonEmptyString.reverseIterator</code> is the same as <code>nonEmptyString.reverse.iterator</code>, but might be more efficient. 
        * </p>
        *
        * @return an iterator yielding the characters of this <code>NonEmptyString</code> in reversed order
        */
      def reverseIterator: Iterator[Char] = new StringOps(nonEmptyString).reverseIterator

      /**
        * Builds a new <code>GenIterable</code> by applying a function to all characters of this <code>NonEmptyString</code> and collecting the results in reverse order.
        *
        * <p>
        * Note: <code>nonEmptyString.reverseMap(f)</code> is the same as <code>nonEmptyString.reverse.map(f)</code>, but might be more efficient. 
        * </p>
        *
        * @tparam U the element type of the returned <code>GenIterable</code>.
        * @param f the function to apply to each character.
        * @return a new <code>GenIterable</code> resulting from applying the given function <code>f</code> to each character of this <code>NonEmptyString</code>
        *     and collecting the results in reverse order. 
        */
      def reverseMap[U](f: Char => U): Iterable[U] = nonEmptyString.toList.reverseMap(f)
    }
  }
}