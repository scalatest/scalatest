/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalactic.anyvals

import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.GenSeq
import org.scalactic.ColCompatHelper.{IterableOnce, Iterable, GenIterable}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Buffer
import scala.reflect.ClassTag
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import org.scalactic.Every
import org.scalactic.Resources


// Can't be a LinearSeq[T] because Builder would be able to create an empty one.
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
final class NonEmptyString private (val theString: String) extends AnyVal {

  /**
    * Returns a new <code>NonEmptyString</code> containing this <code>NonEmptyString</code> followed by the passed <code>NonEmptyString</code>.
    *
    * @param other the <code>NonEmptyString</code> to append
    * @return a new <code>NonEmptyString</code> that contains this <code>NonEmptyString</code> followed by <code>other</code>.
    */
  def ++(other: NonEmptyString): NonEmptyString = new NonEmptyString(theString ++ other.theString)

  /**
    * Returns a new <code>NonEmptyString</code> containing this <code>NonEmptyString</code> followed by the characters of the passed <code>Every</code>.
    *
    * @param other the <code>Every</code> of <code>Char</code> to append
    * @return a new <code>NonEmptyString</code> that contains this <code>NonEmptyString</code> followed by all characters of <code>other</code>.
    */
  def ++(other: Every[Char]): NonEmptyString = new NonEmptyString(theString ++ other.mkString)

  // TODO: Have I added these extra ++, etc. methods to Every that take a NonEmptyString?

  /**
    * Returns a new <code>NonEmptyString</code> containing this <code>NonEmptyString</code> followed by the characters of the passed <code>IterableOnce</code>.
    *
    * @param other the <code>IterableOnce</code> of <code>Char</code> to append
    * @return a new <code>NonEmptyString</code> that contains this <code>NonEmptyString</code> followed by all characters of <code>other</code>.
    */
  def ++(other: IterableOnce[Char]): NonEmptyString =
    if (other.isEmpty) this else new NonEmptyString(theString ++ other.mkString)

  /**
    * Returns a new <code>NonEmptyString</code> with the given character prepended.
    *
    * <p>
    * Note that :-ending operators are right associative. A mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
    * </p>
    *
    * @param c the character to prepend to this <code>NonEmptyString</code>
    * @return a new <code>NonEmptyString</code> consisting of <code>c</code> followed by all characters of this <code>NonEmptyString</code>.
    */
  final def +:(c: Char): NonEmptyString = new NonEmptyString(c +: theString)

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
  def :+(c: Char): NonEmptyString = new NonEmptyString(theString :+ c)

  /**
    * Appends all characters of this <code>NonEmptyString</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
    * on of every element of this <code>NonEmptyString</code>, without any separator string.
    *
    * @param sb the string builder to which characters will be appended
    * @return the string builder, <code>sb</code>, to which elements were appended.
    */
  final def addString(sb: StringBuilder): StringBuilder = theString.addString(sb)

  /**
    * Appends all characters of this <code>NonEmptyString</code> to a string builder using a separator string. The written text will consist of a concatenation of the
    * result of invoking <code>toString</code>
    * on of every character of this <code>NonEmptyString</code>, separated by the string <code>sep</code>.
    *
    * @param sb the string builder to which characters will be appended
    * @param sep the separator string
    * @return the string builder, <code>sb</code>, to which characters were appended.
    */
  final def addString(sb: StringBuilder, sep: String): StringBuilder = theString.addString(sb, sep)

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
  final def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = theString.addString(sb, start, sep, end)

  /**
    * Selects a character by its index in the <code>NonEmptyString</code>.
    *
    * @return the character of this <code>NonEmptyString</code> at index <code>idx</code>, where 0 indicates the first element.
    */
  final def apply(idx: Int): Char = theString(idx)

  /**
    * Gets a character by its index in the <code>NonEmptyString</code>.
    *
    * @return the character of this <code>NonEmptyString</code> at index <code>idx</code>, where 0 indicates the first element.
    */
  final def charAt(idx: Int): Char = theString.charAt(idx)

  /**
    * Finds the first character of this <code>NonEmptyString</code> for which the given partial function is defined, if any, and applies the partial function to it.
    *
    * @param pf the partial function
    * @return an <code>Option</code> containing <code>pf</code> applied to the first character for which it is defined, or <code>None</code> if
    *    the partial function was not defined for any character.
    */
  final def collectFirst[U](pf: PartialFunction[Char, U]): Option[U] = theString.collectFirst(pf)

  /**
    * Indicates whether this <code>NonEmptyString</code> contains a given value as an character.
    *
    * @param c the element to look for
    * @return true if this <code>NonEmptyString</code> has an character that is equal (as determined by <code>==)</code> to <code>c</code>, false otherwise.
    */
  final def contains(c: Char): Boolean = theString.contains(c)

  /**
    * Indicates whether this <code>NonEmptyString</code> contains a given <code>GenSeq</code> of characters as a slice.
    *
    * @param that the <code>GenSeq</code> character slice to look for
    * @return true if this <code>NonEmptyString</code> contains a slice with the same characters as <code>that</code>, otherwise <code>false</code>.
    */
  final def containsSlice(that: GenSeq[Char]): Boolean = theString.containsSlice(that)

  /**
    * Indicates whether this <code>NonEmptyString</code> contains a given <code>Every</code> of character as a slice.
    *
    * @param that the <code>Every</code> character slice to look for
    * @return true if this <code>NonEmptyString</code> contains a character slice with the same characters as <code>that</code>, otherwise <code>false</code>.
    */
  final def containsSlice(that: Every[Char]): Boolean = theString.containsSlice(that.toVector)

  /**
    * Indicates whether this <code>NonEmptyString</code> contains a given <code>NonEmptyString</code> as a slice.
    *
    * @param that the <code>NonEmptyString</code> slice to look for
    * @return true if this <code>NonEmptyString</code> contains a slice with the same characters as <code>that</code>, otherwise <code>false</code>.
    */
  final def containsSlice(that: NonEmptyString): Boolean = theString.containsSlice(that.theString)

  /**
    * Copies characters of this <code>NonEmptyString</code> to an array. Fills the given array <code>arr</code> with characters of this <code>NonEmptyString</code>. Copying
    * will stop once either the end of the current <code>NonEmptyString</code> is reached, or the end of the array is reached.
    *
    * @param arr the array to fill
    */
  final def copyToArray(arr: Array[Char]): Unit = theString.copyToArray(arr, 0)

  /**
    * Copies characters of this <code>NonEmptyString</code> to an array. Fills the given array <code>arr</code> with characters of this <code>NonEmptyString</code>, beginning at
    * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyString</code> is reached, or the end of the array is reached.
    *
    * @param arr the array to fill
    * @param start the starting index
    */
  final def copyToArray(arr: Array[Char], start: Int): Unit = theString.copyToArray(arr, start)

  /**
    * Copies characters of this <code>NonEmptyString</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> characters of this <code>NonEmptyString</code>, beginning at
    * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyString</code> is reached, the end of the array is reached, or
    * <code>len</code> elements have been copied.
    *
    * @param arr the array to fill
    * @param start the starting index
    * @param len the maximum number of elements to copy
    */
  final def copyToArray(arr: Array[Char], start: Int, len: Int): Unit = theString.copyToArray(arr, start, len)

  /**
    * Copies all characters of this <code>NonEmptyString</code> to a buffer.
    *
    * @param buf the buffer to which characters are copied
    */
  final def copyToBuffer(buf: Buffer[Char]): Unit = theString.copyToBuffer(buf)

  /**
    * Indicates whether every character of this <code>NonEmptyString</code> relates to the corresponding element of a given <code>GenSeq</code> by satisfying a given predicate.
    *
    * @tparam B the type of the elements of <code>that</code>
    * @param that the <code>GenSeq</code> to compare for correspondence
    * @param p the predicate, which relates elements from this <code>NonEmptyString</code> and the passed <code>GenSeq</code>
    * @return true if this <code>NonEmptyString</code> and the passed <code>GenSeq</code> have the same length and <code>p(x, y)</code> is <code>true</code>
    *     for all corresponding elements <code>x</code> of this <code>NonEmptyString</code> and <code>y</code> of that, otherwise <code>false</code>.
    */
  final def corresponds[B](that: GenSeq[B])(p: (Char, B) => Boolean): Boolean = theString.corresponds(that)(p)

  /**
    * Indicates whether every character of this <code>NonEmptyString</code> relates to the corresponding element of a given <code>Every</code> by satisfying a given predicate.
    *
    * @tparam B the type of the elements of <code>that</code>
    * @param that the <code>Every</code> to compare for correspondence
    * @param p the predicate, which relates elements from this <code>NonEmptyString</code> and the passed <code>Every</code>
    * @return true if this <code>NonEmptyString</code> and the passed <code>Every</code> have the same length and <code>p(x, y)</code> is <code>true</code>
    *     for all corresponding elements <code>x</code> of this <code>NonEmptyString</code> and <code>y</code> of that, otherwise <code>false</code>.
    */
  final def corresponds[B](that: Every[B])(p: (Char, B) => Boolean): Boolean = theString.corresponds(that.toVector)(p)

  /**
    * Indicates whether every character of this <code>NonEmptyString</code> relates to the corresponding character of a given <code>NonEmptyString</code> by satisfying a given predicate.
    *
    * @param that the <code>NonEmptyString</code> to compare for correspondence
    * @param p the predicate, which relates elements from this and the passed <code>NonEmptyString</code>
    * @return true if this and the passed <code>NonEmptyString</code> have the same length and <code>p(x, y)</code> is <code>true</code>
    *     for all corresponding characters <code>x</code> of this <code>NonEmptyString</code> and <code>y</code> of that, otherwise <code>false</code>.
    */
  final def corresponds(that: NonEmptyString)(p: (Char, Char) => Boolean): Boolean = theString.corresponds(that.theString)(p)

  /**
    * Counts the number of characters in this <code>NonEmptyString</code> that satisfy a predicate.
    *
    * @param p the predicate used to test characters.
    * @return the number of characters satisfying the predicate <code>p</code>.
    */
  final def count(p: Char => Boolean): Int = theString.count(p)

  /**
    * Builds a new <code>NonEmptyString</code> from this <code>NonEmptyString</code> without any duplicate characters.
    *
    * @return A new <code>NonEmptyString</code> that contains the first occurrence of every character of this <code>NonEmptyString</code>.
    */
  final def distinct: NonEmptyString = new NonEmptyString(theString.distinct)

  /**
    * Indicates whether this <code>NonEmptyString</code> ends with the given <code>GenSeq</code> of Char.
    *
    * @param that the sequence to test
    * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
    */
  final def endsWith[B](that: GenSeq[B]): Boolean = theString.endsWith(that)

  /**
    * Indicates whether this <code>NonEmptyString</code> ends with the given <code>Every</code>.
    *
    * @param that the <code>Every</code> to test
    * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
    */
  final def endsWith[B](that: Every[B]): Boolean = theString.endsWith(that.toVector)

  // TODO: Search for that: Every in here and add a that: NonEmptyString in Every.
  /**
    * Indicates whether this <code>NonEmptyString</code> ends with the given <code>NonEmptyString</code>.
    *
    * @param that the <code>NonEmptyString</code> to test
    * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
    */
  final def endsWith(that: NonEmptyString): Boolean = theString.endsWith(that.theString)

  /*
    override def equals(o: Any): Boolean =
      o match {
        case nonEmptyString: NonEmptyString[_] => toString == nonEmptyString.toString
        case _ => false
      }
  */

  /**
    * Indicates whether a predicate holds for at least one of the characters of this <code>NonEmptyString</code>.
    *
    * @param p the predicate used to test characters.
    * @return <code>true</code> if the given predicate <code>p</code> holds for some of the elements of this <code>NonEmptyString</code>, otherwise <code>false</code>. 
    */
  final def exists(p: Char => Boolean): Boolean = theString.exists(p)

  /**
    * Finds the first character of this <code>NonEmptyString</code> that satisfies the given predicate, if any.
    *
    * @param p the predicate used to test characters
    * @return an <code>Some</code> containing the first character in this <code>NonEmptyString</code> that satisfies <code>p</code>, or <code>None</code> if none exists.
    */
  final def find(p: Char => Boolean): Option[Char] = theString.find(p)

  /**
    * Builds a new <code>NonEmptyString</code> by applying a function to all characters of this <code>NonEmptyString</code> and using the characters of the resulting <code>NonEmptyString</code>s.
    *
    * @param f the function to apply to each character.
    * @return a new <code>NonEmptyString</code> containing characters obtained by applying the given function <code>f</code> to each character of this <code>NonEmptyString</code> and concatenating
    *    the characters of resulting <code>NonEmptyString</code>s.
    */
  final def flatMap(f: Char => NonEmptyString): NonEmptyString = {
    val buf = new ArrayBuffer[Char]
    for (c <- theString)
      buf ++= f(c).theString
    new NonEmptyString(buf.mkString)
  }

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
  final def fold(z: Char)(op: (Char, Char) => Char): Char = theString.fold(z)(op)

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
  final def foldLeft[B](z: B)(op: (B, Char) => B): B = theString.foldLeft(z)(op)

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
  final def foldRight[B](z: B)(op: (Char, B) => B): B = theString.foldRight(z)(op)

  /**
    * Indicates whether a predicate holds for all characters of this <code>NonEmptyString</code>.
    *
    * @param p the predicate used to test characters.
    * @return <code>true</code> if the given predicate <code>p</code> holds for all characters of this <code>NonEmptyString</code>, otherwise <code>false</code>.
    */
  final def forall(p: Char => Boolean): Boolean = theString.forall(p)

  /**
    * Applies a function <code>f</code> to all characters of this <code>NonEmptyString</code>.
    *
    * @param f the function that is applied for its side-effect to every character. The result of function <code>f</code> is discarded.
    */
  final def foreach(f: Char => Unit): Unit = theString.foreach(f)

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
  final def groupBy[K](f: Char => K): Map[K, NonEmptyString] = {
    val mapKToString = theString.groupBy(f)
    mapKToString.mapValues { list => new NonEmptyString(list) }.toMap
  }

  /**
    * Partitions characters into fixed size <code>NonEmptyString</code>s.
    *
    * @param size the number of characters per group
    * @return An iterator producing <code>NonEmptyString</code>s of size <code>size</code>, except the last will be truncated if the characters don't divide evenly.
    */
  final def grouped(size: Int): Iterator[NonEmptyString] = {
    if (size > 0) {
      val itOfString = theString.grouped(size)
      itOfString.map { list => new NonEmptyString(list) }
    }
    else
      throw new IllegalArgumentException(Resources.invalidSize(size))
  }

  /**
    * Returns <code>true</code> to indicate this <code>NonEmptyString</code> has a definite size, since all <code>NonEmptyString</code>s are strict collections.
    */
  final def hasDefiniteSize: Boolean = true

  // override def hashCode: Int = toString.hashCode

  /**
    * Selects the first character of this <code>NonEmptyString</code>.
    *
    * @return the first character of this <code>NonEmptyString</code>.
    */
  final def head: Char = theString.head

  // Methods like headOption I can't get rid of because of the implicit conversion to Iterable.
  // Users can call any of the methods I've left out on a NonEmptyString, and get whatever String would return
  // for that method call. Eventually I'll probably implement them all to save the implicit conversion.

  /**
    * Selects the first character of this <code>NonEmptyString</code> and returns it wrapped in a <code>Some</code>.
    *
    * @return the first character of this <code>NonEmptyString</code>, wrapped in a <code>Some</code>.
    */
  final def headOption: Option[Char] = theString.headOption

  /**
    * Finds index of first occurrence of some value in this <code>NonEmptyString</code>.
    *
    * @param c the character value to search for.
    * @return the index of the first character of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>) to <code>c</code>,
    *     or <code>-1</code>, if none exists.
    */
  final def indexOf(c: Char): Int = theString.indexOf(c, 0)

  /**
    * Finds index of first occurrence of some value in this <code>NonEmptyString</code> after or at some start index.
    *
    * @param c the character value to search for.
    * @param from the start index
    * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
    *     or <code>-1</code>, if none exists.
    */
  final def indexOf(c: Char, from: Int): Int = theString.indexOf(c, from)

  /**
    * Finds first index where this <code>NonEmptyString</code> contains a given <code>GenSeq[Char]</code> as a slice.
    *
    * @param that the <code>GenSeq[Char]</code> defining the slice to look for
    * @return the first index at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
    *     <code>GenSeq</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
    */
  final def indexOfSlice(that: GenSeq[Char]): Int = theString.indexOfSlice(that)

  /**
    * Finds first index after or at a start index where this <code>NonEmptyString</code> contains a given <code>GenSeq[Char]</code> as a slice.
    *
    * @param that the <code>GenSeq[Char]</code> defining the slice to look for
    * @param from the start index
    * @return the first index <code>&gt;=</code> <code>from</code> at which the characters of this <code>NonEmptyString</code> starting at that index match the characters of
    *     <code>GenSeq[Char]</code> <code>that</code>, or <code>-1</code> of no such subsequence exists.
    */
  final def indexOfSlice(that: GenSeq[Char], from: Int): Int = theString.indexOfSlice(that, from)

  /**
    * Finds first index where this <code>NonEmptyString</code> contains a given <code>Every</code> as a slice.
    *
    * @param that the <code>Every</code> defining the slice to look for
    * @return the first index such that the characters of this <code>NonEmptyString</code> starting at this index match the characters of
    *     <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
    */
  final def indexOfSlice(that: Every[Char]): Int = theString.indexOfSlice(that.toVector)

  /**
    * Finds first index where this <code>NonEmptyString</code> contains a given <code>NonEmptyString</code> as a slice.
    *
    * @param that the <code>NonEmptyString</code> defining the slice to look for
    * @return the first index such that the characters of this <code>NonEmptyString</code> starting at this index match the characters of
    *     <code>NonEmptyString</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
    */
  final def indexOfSlice(that: NonEmptyString): Int = theString.indexOfSlice(that.theString)

  /**
    * Finds first index after or at a start index where this <code>NonEmptyString</code> contains a given <code>Every</code> as a slice.
    *
    * @param that the <code>Every</code> defining the slice to look for
    * @param from the start index
    * @return the first index <code>&gt;=</code> <code>from</code> such that the characters of this <code>NonEmptyString</code> starting at this index match the characters of
    *     <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
    */
  final def indexOfSlice(that: Every[Char], from: Int): Int = theString.indexOfSlice(that.toVector, from)

  /**
    * Finds first index after or at a start index where this <code>NonEmptyString</code> contains a given <code>NonEmptyString</code> as a slice.
    *
    * @param that the <code>NonEmptyString</code> defining the slice to look for
    * @param from the start index
    * @return the first index <code>&gt;=</code> <code>from</code> such that the characters of this <code>NonEmptyString</code> starting at this index match the characters of
    *     <code>NonEmptyString</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
    */
  final def indexOfSlice(that: NonEmptyString, from: Int): Int = theString.indexOfSlice(that.theString, from)

  /**
    * Finds index of the first character satisfying some predicate.
    *
    * @param p the predicate used to test characters.
    * @return the index of the first character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>,
    *     or <code>-1</code>, if none exists.
    */
  final def indexWhere(p: Char => Boolean): Int = theString.indexWhere(p)

  /**
    * Finds index of the first character satisfying some predicate after or at some start index.
    *
    * @param p the predicate used to test characters.
    * @param from the start index
    * @return the index <code>&gt;=</code> <code>from</code> of the first character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>,
    *     or <code>-1</code>, if none exists.
    */
  final def indexWhere(p: Char => Boolean, from: Int): Int = theString.indexWhere(p, from)

  /**
    * Produces the range of all indices of this <code>NonEmptyString</code>. 
    *
    * @return a <code>Range</code> value from <code>0</code> to one less than the length of this <code>NonEmptyString</code>. 
    */
  final def indices: Range = theString.indices

  /**
    * Tests whether this <code>NonEmptyString</code> contains given index.
    *
    * @param idx the index to test
    * @return true if this <code>NonEmptyString</code> contains an character at position <code>idx</code>, <code>false</code> otherwise.
    */
  final def isDefinedAt(idx: Int): Boolean = theString.isDefinedAt(idx)

  /**
    * Returns <code>false</code> to indicate this <code>NonEmptyString</code>, like all <code>NonEmptyString</code>s, is non-empty.
    *
    * @return false
    */
  final def isEmpty: Boolean = false

  /**
    * Returns <code>true</code> to indicate this <code>NonEmptyString</code>, like all <code>NonEmptyString</code>s, can be traversed repeatedly.
    *
    * @return true
    */
  final def isTraversableAgain: Boolean = true

  /**
    * Creates and returns a new iterator over all characters contained in this <code>NonEmptyString</code>.
    *
    * @return the new iterator
    */
  final def iterator: Iterator[Char] = theString.iterator

  /**
    * Selects the last character of this <code>NonEmptyString</code>.
    *
    * @return the last character of this <code>NonEmptyString</code>.
    */
  final def last: Char = theString.last

  /**
    * Finds the index of the last occurrence of some value in this <code>NonEmptyString</code>.
    *
    * @param c the character value to search for.
    * @return the index of the last character of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>) to <code>c</code>,
    *     or <code>-1</code>, if none exists.
    */
  final def lastIndexOf(c: Char): Int = theString.lastIndexOf(c)

  /**
    * Finds the index of the last occurrence of some value in this <code>NonEmptyString</code> before or at a given <code>end</code> index.
    *
    * @param c the character value to search for.
    * @param end the end index. 
    * @return the index <code>&gt;=</code> <code>end</code> of the last character of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>)
    *     to <code>elem</code>, or <code>-1</code>, if none exists.
    */
  final def lastIndexOf(c: Char, end: Int): Int = theString.lastIndexOf(c, end)

  /**
    * Finds the last index where this <code>NonEmptyString</code> contains a given <code>GenSeq</code> as a slice. 
    *
    * @param that the <code>GenSeq</code> defining the slice to look for
    * @return the last index at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
    *    <code>GenSeq</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
    */
  final def lastIndexOfSlice(that: GenSeq[Char]): Int = theString.lastIndexOfSlice(that)

  /**
    * Finds the last index before or at a given end index where this <code>NonEmptyString</code> contains a given <code>GenSeq</code> as a slice. 
    *
    * @param that the <code>GenSeq</code> defining the slice to look for
    * @param end the end index
    * @return the last index <code>&gt;=</code> <code>end</code> at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
    *    <code>GenSeq</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
    */
  final def lastIndexOfSlice(that: GenSeq[Char], end: Int): Int = theString.lastIndexOfSlice(that, end)

  /**
    * Finds the last index where this <code>NonEmptyString</code> contains a given <code>Every</code> as a slice. 
    *
    * @param that the <code>Every</code> defining the slice to look for
    * @return the last index at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
    *    <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
    */
  final def lastIndexOfSlice(that: Every[Char]): Int = theString.lastIndexOfSlice(that.toVector)

  /**
    * Finds the last index where this <code>NonEmptyString</code> contains a given <code>NonEmptyString</code> as a slice. 
    *
    * @param that the <code>NonEmptyString</code> defining the slice to look for
    * @return the last index at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
    *    <code>NonEmptyString</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
    */
  final def lastIndexOfSlice(that: NonEmptyString): Int = theString.lastIndexOfSlice(that.theString)

  /**
    * Finds the last index before or at a given end index where this <code>NonEmptyString</code> contains a given <code>Every</code> as a slice. 
    *
    * @param that the <code>Every</code> defining the slice to look for
    * @param end the end index
    * @return the last index <code>&gt;=</code> <code>end</code> at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
    *    <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
    */
  final def lastIndexOfSlice(that: Every[Char], end: Int): Int = theString.lastIndexOfSlice(that.toVector, end)

  /**
    * Finds the last index before or at a given end index where this <code>NonEmptyString</code> contains a given <code>NonEmptyString</code> as a slice. 
    *
    * @param that the <code>NonEmptyString</code> defining the slice to look for
    * @param end the end index
    * @return the last index <code>&gt;=</code> <code>end</code> at which the characters of this <code>NonEmptyString</code> starting at that index match the characters of
    *    <code>NonEmptyString</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
    */
  final def lastIndexOfSlice(that: NonEmptyString, end: Int): Int = theString.lastIndexOfSlice(that.theString, end)

  /**
    * Finds index of last character satisfying some predicate.
    *
    * @param p the predicate used to test characters.
    * @return the index of the last character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>, or <code>-1</code>, if none exists.
    */
  final def lastIndexWhere(p: Char => Boolean): Int = theString.lastIndexWhere(p)

  /**
    * Finds index of last character satisfying some predicate before or at given end index.
    *
    * @param p the predicate used to test characters.
    * @param end the end index
    * @return the index <code>&gt;=</code> <code>end</code> of the last character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>,
    *     or <code>-1</code>, if none exists. 
    */
  final def lastIndexWhere(p: Char => Boolean, end: Int): Int = theString.lastIndexWhere(p, end)

  /**
    * Returns the last element of this <code>NonEmptyString</code>, wrapped in a <code>Some</code>. 
    *
    * @return the last element, wrapped in a <code>Some</code>. 
    */
  final def lastOption: Option[Char] = theString.lastOption // Will always return a Some

  /**
    * The length of this <code>NonEmptyString</code>.
    *
    * <p>
    * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
    * </p>
    *
    * @return the number of characters in this <code>NonEmptyString</code>.
    */
  final def length: Int = theString.length

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
  final def lengthCompare(len: Int): Int = theString.lengthCompare(len)

  /**
    * Builds a new <code>NonEmptyString</code> by applying a function to all characters of this <code>NonEmptyString</code>.
    *
    * @tparam U the character type of the returned <code>NonEmptyString</code>.
    * @param f the function to apply to each character.
    * @return a new <code>NonEmptyString</code> resulting from applying the given function <code>f</code> to each character of this <code>NonEmptyString</code> and collecting the results.
    */
  final def map[U](f: Char => U): NonEmptyString = {
    //NonEmptyString("test")
    new NonEmptyString(
      theString.map { c =>
        f(c).toString
      }.mkString
    )
  }

  /**
    * Finds the largest character.
    *
    * @return the largest element of this <code>NonEmptyString</code>. 
    */
  final def max(implicit cmp: Ordering[Char]): Char = theString.max(cmp)

  /**
    * Finds the largest result after applying the given function to every character.
    *
    * @return the largest result of applying the given function to every character of this <code>NonEmptyString</code>.
    */
  final def maxBy[U](f: Char => U)(implicit cmp: Ordering[U]): Char = theString.maxBy(f)(cmp)

  /**
    * Finds the smallest character.
    *
    * @return the smallest character of this <code>NonEmptyString</code>.
    */
  final def min(implicit cmp: Ordering[Char]): Char = theString.min(cmp)

  /**
    * Finds the smallest result after applying the given function to every character.
    *
    * @return the smallest result of applying the given function to every character of this <code>NonEmptyString</code>.
    */
  final def minBy[U](f: Char => U)(implicit cmp: Ordering[U]): Char = theString.minBy(f)(cmp)

  /**
    * Displays all characters of this <code>NonEmptyString</code> in a string.
    *
    * @return a string representation of this <code>NonEmptyString</code>. In the resulting string, the result of invoking <code>toString</code> on all characters of this
    *     <code>NonEmptyString</code> follow each other without any separator string. 
    */
  final def mkString: String = theString.mkString

  /**
    * Displays all elements of this <code>NonEmptyString</code> in a string using a separator string. 
    *
    * @param sep the separator string
    * @return a string representation of this <code>NonEmptyString</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
    *     <code>NonEmptyString</code> are separated by the string <code>sep</code>. 
    */
  final def mkString(sep: String): String = theString.mkString(sep)

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
  final def mkString(start: String, sep: String, end: String): String = theString.mkString(start, sep, end)

  /**
    * Returns <code>true</code> to indicate this <code>NonEmptyString</code>, like all <code>NonEmptyString</code>s, is non-empty.
    *
    * @return true
    */
  final def nonEmpty: Boolean = true

  /**
    * A copy of this <code>NonEmptyString</code> with an element value appended until a given target length is reached.
    *
    * @param len the target length 
    * @param c the padding character
    * @return a new <code>NonEmptyString</code> consisting of all characters of this <code>NonEmptyString</code> followed by the minimal number of occurrences
    *     of <code>elem</code> so that the resulting <code>NonEmptyString</code> has a length of at least <code>len</code>. 
    */
  final def padTo(len: Int, c: Char): NonEmptyString =
    new NonEmptyString(theString.padTo(len, c))

  /**
    * Produces a new <code>NonEmptyString</code> where a slice of characters in this <code>NonEmptyString</code> is replaced by another <code>NonEmptyString</code>
    *
    * @param from the index of the first replaced character
    * @param that the <code>NonEmptyString</code> whose characters should replace a slice in this <code>NonEmptyString</code>
    * @param replaced the number of characters to drop in the original <code>NonEmptyString</code>
    */
  final def patch(from: Int, that: NonEmptyString, replaced: Int): NonEmptyString =
    new NonEmptyString(theString.patch(from, that.theString, replaced))

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
  final def permutations: Iterator[NonEmptyString] = {
    val it = theString.permutations
    it map { list => new NonEmptyString(list) }
  }

  /**
    * Returns the length of the longest prefix whose characters all satisfy some predicate.
    *
    * @param p the predicate used to test characters.
    * @return the length of the longest prefix of this <code>NonEmptyString</code> such that every characters
    *     of the segment satisfies the predicate <code>p</code>. 
    */
  final def prefixLength(p: Char => Boolean): Int = theString.prefixLength(p)

  /**
    * The result of multiplying all the characters of this <code>NonEmptyString</code>.
    *
    * <p>
    * This method can be invoked for any <code>NonEmptyString</code> for which an implicit <code>Numeric[T]</code> exists.
    * </p>
    *
    * @return the product of all elements
    */
  final def product(implicit num: Numeric[Char]): Char = theString.product(num)

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
  final def reduce(op: (Char, Char) => Char): Char = theString.reduce(op)

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
  final def reduceLeft(op: (Char, Char) => Char): Char = theString.reduceLeft(op)

  /**
    * Applies a binary operator to all characters of this <code>NonEmptyString</code>, going left to right, returning the result in a <code>Some</code>.
    *
    * @param op the binary operator.
    * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
    * </p>
    */
  final def reduceLeftOption(op: (Char, Char) => Char): Option[Char] = theString.reduceLeftOption(op)

  final def reduceOption(op: (Char, Char) => Char): Option[Char] = theString.reduceOption(op)

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
  final def reduceRight(op: (Char, Char) => Char): Char = theString.reduceRight(op)

  /**
    * Applies a binary operator to all elements of this <code>NonEmptyString</code>, going right to left, returning the result in a <code>Some</code>.
    *
    * @param op the binary operator
    * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
    */
  final def reduceRightOption(op: (Char, Char) => Char): Option[Char] = theString.reduceRightOption(op)

  /**
    * Returns new <code>NonEmptyString</code> with characters in reverse order.
    *
    * @return a new <code>NonEmptyString</code> with all characters of this <code>NonEmptyString</code> in reversed order.
    */
  final def reverse: NonEmptyString =
    new NonEmptyString(theString.reverse)

  /**
    * An iterator yielding characters in reverse order.
    *
    * <p>
    * Note: <code>nonEmptyString.reverseIterator</code> is the same as <code>nonEmptyString.reverse.iterator</code>, but might be more efficient. 
    * </p>
    *
    * @return an iterator yielding the characters of this <code>NonEmptyString</code> in reversed order
    */
  final def reverseIterator: Iterator[Char] = theString.reverseIterator

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
  final def reverseMap[U](f: Char => U): GenIterable[U] = theString.reverseMap(f)

  /**
    * Checks if the given <code>GenIterable</code> contains the same characters in the same order as this <code>NonEmptyString</code>.
    *
    * @param that the <code>GenIterable</code> with which to compare
    * @return <code>true</code>, if both this <code>NonEmptyString</code> and the given <code>GenIterable</code> contain the same characters
    *     in the same order, <code>false</code> otherwise. 
    */
  final def sameElements(that: GenIterable[Char]): Boolean = theString.sameElements(that)

  /**
    * Checks if the given <code>Every</code> contains the same characters in the same order as this <code>NonEmptyString</code>.
    *
    * @param that the <code>Every</code> with which to compare
    * @return <code>true</code>, if both this and the given <code>Every</code> contain the same characters
    *     in the same order, <code>false</code> otherwise. 
    */
  final def sameElements(that: Every[Char]): Boolean = theString.sameElements(that.toVector)

  /**
    * Checks if the given <code>NonEmptyString</code> contains the same characters in the same order as this <code>NonEmptyString</code>.
    *
    * @param that the <code>NonEmptyString</code> with which to compare
    * @return <code>true</code>, if both this and the given <code>NonEmptyString</code> contain the same characters
    *     in the same order, <code>false</code> otherwise. 
    */
  final def sameElements(that: NonEmptyString): Boolean = theString.sameElements(that.theString)

  /**
    * Computes a prefix scan of the characters of this <code>NonEmptyString</code>.
    *
    * <p>
    * Note: The neutral character z may be applied more than once.
    * </p>
    *
    * <p>
    * Here are some examples:
    * </p>
    *
    * <pre class="stHighlight">
    * NonEmptyString("123").scan(0)(_ + _) == NonEmptyString(0, 1, 3, 6)
    * NonEmptyString("123").scan("z")(_ + _.toString) == NonEmptyString("z", "z1", "z12", "z123")
    * </pre>
    *
    * @param z a neutral element for the scan operation; may be added to the result an arbitrary number of
    *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
    *     0 for addition, or 1 for multiplication.)
    * @param op a binary operator that must be associative
    * @return a new <code>NonEmptyString</code> containing the prefix scan of the elements in this <code>NonEmptyString</code> 
    */
  final def scan(z: Char)(op: (Char, Char) => Char): NonEmptyString = new NonEmptyString(theString.scan(z)(op).mkString)

  /**
    * Produces a <code>NonEmptyString</code> containing cumulative results of applying the operator going left to right.
    *
    * <p>
    * Here are some examples:
    * </p>
    *
    * <pre class="stHighlight">
    * NonEmptyString("123").scanLeft(0)(_ + _.toString.toInt) == Vector(0, 1, 3, 6)
    * NonEmptyString("123").scanLeft("z")(_ + _) == Vector("z", "z1", "z12", "z123")
    * </pre>
    *
    * @tparam B the result type of the binary operator and type of the resulting <code>NonEmptyString</code>
    * @param z the start value.
    * @param op the binary operator.
    * @return a new <code>NonEmptyString</code> containing the intermediate results of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>,
    *     going left to right, with the start value, <code>z</code>, on the left.
    */
  final def scanLeft[B](z: B)(op: (B, Char) => B): GenIterable[B] = theString.scanLeft(z)(op)

  /**
    * Produces a <code>NonEmptyString</code> containing cumulative results of applying the operator going right to left.
    *
    * <p>
    * Here are some examples:
    * </p>
    *
    * <pre class="stHighlight">
    * NonEmptyString("123").scanRight(0)(_.toString.toInt + _) == NonEmptyString(6, 5, 3, 0)
    * NonEmptyString("123").scanRight("z")(_ + _) == NonEmptyString("123z", "23z", "3z", "z")
    * </pre>
    *
    * @tparam B the result of the binary operator and type of the resulting <code>NonEmptyString</code>
    * @param z the start value
    * @param op the binary operator
    * @return a new <code>NonEmptyString</code> containing the intermediate results of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>,
    *     going right to left, with the start value, <code>z</code>, on the right.
    */
  final def scanRight[B](z: B)(op: (Char, B) => B): GenIterable[B] = theString.scanRight(z)(op)

  /**
    * Computes length of longest segment whose characters all satisfy some predicate.
    *
    * @param p the predicate used to test elements.
    * @param from the index where the search starts.
    * @return the length of the longest segment of this <code>NonEmptyString</code> starting from index <code>from</code> such that every character of the
    *     segment satisfies the predicate <code>p</code>. 
    */
  final def segmentLength(p: Char => Boolean, from: Int): Int = theString.segmentLength(p, from)

  /**
    * Groups characters in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
    *
    * @param size the number of characters per group
    * @return an iterator producing <code>NonEmptyString</code>s of size <code>size</code>, except the last and the only element will be truncated
    *     if there are fewer characters than <code>size</code>.
    */
  final def sliding(size: Int): Iterator[NonEmptyString] = theString.sliding(size).map(new NonEmptyString(_))

  /**
    * Groups characters in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
    * moving the sliding window by a given <code>step</code> each time.
    *
    * @param size the number of characters per group
    * @param step the distance between the first characters of successive groups
    * @return an iterator producing <code>NonEmptyString</code>s of size <code>size</code>, except the last and the only character will be truncated
    *     if there are fewer characters than <code>size</code>.
    */
  final def sliding(size: Int, step: Int): Iterator[NonEmptyString] = theString.sliding(size, step).map(new NonEmptyString(_))

  /**
    * The size of this <code>NonEmptyString</code>.
    *
    * <p>
    * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
    * </p>
    *
    * @return the number of characters in this <code>NonEmptyString</code>.
    */
  final def size: Int = theString.size

  /**
    * Sorts this <code>NonEmptyString</code> according to the <code>Ordering</code> of the result of applying the given function to every character.
    *
    * @tparam U the target type of the transformation <code>f</code>, and the type where the <code>Ordering</code> <code>ord</code> is defined.
    * @param f the transformation function mapping elements to some other domain <code>U</code>.
    * @param ord the ordering assumed on domain <code>U</code>.
    * @return a <code>NonEmptyString</code> consisting of the elements of this <code>NonEmptyString</code> sorted according to the <code>Ordering</code> where
    *    <code>x &lt; y if ord.lt(f(x), f(y))</code>. 
    */
  final def sortBy[U](f: Char => U)(implicit ord: Ordering[U]): NonEmptyString = new NonEmptyString(theString.sortBy(f))

  /**
    * Sorts this <code>NonEmptyString</code> according to a comparison function.
    *
    * <p>
    * The sort is stable. That is, characters that are equal (as determined by <code>lt</code>) appear in the same order in the
    * sorted <code>NonEmptyString</code> as in the original. 
    * </p>
    *
    * @param lt the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
    * @return a <code>NonEmptyString</code> consisting of the elements of this <code>NonEmptyString</code> sorted according to the comparison function <code>lt</code>.
    */
  final def sortWith(lt: (Char, Char) => Boolean): NonEmptyString = new NonEmptyString(theString.sortWith(lt))

  /**
    * Sorts this <code>NonEmptyString</code> according to an <code>Ordering</code>.
    *
    * <p>
    * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
    * sorted <code>NonEmptyString</code> as in the original. 
    * </p>
    *
    * @param ord the <code>Ordering</code> to be used to compare elements.
    * @return a <code>NonEmptyString</code> consisting of the characters of this <code>NonEmptyString</code> sorted according to the ordering defined by <code>ord</code>.
    */
  final def sorted(implicit ord: Ordering[Char]): NonEmptyString = new NonEmptyString(theString.sorted(ord))

  /**
    * Indicates whether this <code>NonEmptyString</code> starts with the given <code>GenSeq</code>. 
    *
    * @param that the <code>GenSeq</code> slice to look for in this <code>NonEmptyString</code>
    * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a prefix, <code>false</code> otherwise.
    */
  final def startsWith(that: GenSeq[Char]): Boolean = theString.startsWith(that)

  /**
    * Indicates whether this <code>NonEmptyString</code> starts with the given <code>GenSeq</code> at the given index. 
    *
    * @param that the <code>GenSeq</code> slice to look for in this <code>NonEmptyString</code>
    * @param offset the index at which this <code>NonEmptyString</code> is searched.
    * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a slice at the index <code>offset</code>, <code>false</code> otherwise.
    */
  final def startsWith(that: GenSeq[Char], offset: Int): Boolean = theString.startsWith(that, offset)

  /**
    * Indicates whether this <code>NonEmptyString</code> starts with the given <code>Every</code>. 
    *
    * @param that the <code>Every</code> to test
    * @return <code>true</code> if this collection has <code>that</code> as a prefix, <code>false</code> otherwise.
    */
  final def startsWith(that: Every[Char]): Boolean = theString.startsWith(that.toVector)

  /**
    * Indicates whether this <code>NonEmptyString</code> starts with the given <code>NonEmptyString</code>. 
    *
    * @param that the <code>NonEmptyString</code> to test
    * @return <code>true</code> if this collection has <code>that</code> as a prefix, <code>false</code> otherwise.
    */
  final def startsWith(that: NonEmptyString): Boolean = theString.startsWith(that.theString)

  /**
    * Indicates whether this <code>NonEmptyString</code> starts with the given <code>Every</code> at the given index. 
    *
    * @param that the <code>Every</code> slice to look for in this <code>NonEmptyString</code>
    * @param offset the index at which this <code>NonEmptyString</code> is searched.
    * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a slice at the index <code>offset</code>, <code>false</code> otherwise.
    */
  final def startsWith(that: Every[Char], offset: Int): Boolean = theString.startsWith(that.toVector, offset)

  /**
    * Indicates whether this <code>NonEmptyString</code> starts with the given <code>NonEmptyString</code> at the given index. 
    *
    * @param that the <code>NonEmptyString</code> slice to look for in this <code>NonEmptyString</code>
    * @param offset the index at which this <code>NonEmptyString</code> is searched.
    * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a slice at the index <code>offset</code>, <code>false</code> otherwise.
    */
  final def startsWith(that: NonEmptyString, offset: Int): Boolean = theString.startsWith(that.theString, offset)

  /**
    * Returns <code>"NonEmptyString"</code>, the prefix of this object's <code>toString</code> representation.
    *
    * @return the string <code>"NonEmptyString"</code>
    */
  def stringPrefix: String = "NonEmptyString"

  /**
    * The result of summing all the characters of this <code>NonEmptyString</code>.
    *
    * <p>
    * This method can be invoked for any <code>NonEmptyString</code> for which an implicit <code>Numeric[Char]</code> exists.
    * </p>
    *
    * @return the sum of all elements
    */
  final def sum(implicit num: Numeric[Char]): Long = theString.sum(num)

  import scala.language.higherKinds

  /**
    * Converts this <code>NonEmptyString</code> into a collection of type <code>Col</code> by copying all elements.
    *
    * @tparam Col the collection type to build.
    * @return a new collection containing all elements of this <code>NonEmptyString</code>. 
    */
  final def to[Col[_]](factory: org.scalactic.ColCompatHelper.Factory[Char, Col[Char @ uV]]): Col[Char @ uV] =
    theString.to(factory)

  /**
    * Converts this <code>NonEmptyString</code> to an array.
    *
    * @return an array containing all characters of this <code>NonEmptyString</code>. A <code>ClassTag</code> must be available for the element type of this <code>NonEmptyString</code>.
    */
  final def toArray(implicit classTag: ClassTag[Char]): Array[Char] = theString.toArray

  /**
    * Converts this <code>NonEmptyString</code> to a <code>Vector</code>.
    *
    * @return a <code>Vector</code> containing all characters of this <code>NonEmptyString</code>.
    */
  final def toVector: Vector[Char] = theString.toVector

  /**
    * Converts this <code>NonEmptyString</code> to a mutable buffer.
    *
    * @return a buffer containing all characters of this <code>NonEmptyString</code>.
    */
  final def toBuffer: Buffer[Char] = theString.toBuffer

  /**
    * Converts this <code>NonEmptyString</code> to an immutable <code>IndexedSeq</code>.
    *
    * @return an immutable <code>IndexedSeq</code> containing all characters of this <code>NonEmptyString</code>.
    */
  final def toIndexedSeq: collection.immutable.IndexedSeq[Char] = theString.toVector

  /**
    * Converts this <code>NonEmptyString</code> to an iterable collection.
    *
    * @return an <code>Iterable</code> containing all characters of this <code>NonEmptyString</code>.
    */
  final def toIterable: scala.collection.Iterable[Char] = theString.toIterable

  /**
    * Returns an <code>Iterator</code> over the elements in this <code>NonEmptyString</code>.
    *
    * @return an <code>Iterator</code> containing all characters of this <code>NonEmptyString</code>.
    */
  final def toIterator: Iterator[Char] = theString.toIterator

  /**
    * Converts this <code>NonEmptyString</code> to a map.
    *
    * <p>
    * This method is unavailable unless the elements are members of <code>Tuple2</code>, each <code>((K, V))</code> becoming a key-value pair
    * in the map. Duplicate keys will be overwritten by later keys.
    * </p>
    *
    * @return a map of type <code>immutable.Map[Int, Char]</code> containing all index/character pairs of type <code>(Int, Char)</code> of this <code>NonEmptyString</code>.
    */
  final def toMap: Map[Int, Char] = Map.empty[Int, Char] ++ theString.zipWithIndex.map(e => e._2 -> e._1)

  /**
    * Converts this <code>NonEmptyString</code> to an immutable <code>IndexedSeq</code>.
    *
    * @return an immutable <code>IndexedSeq</code> containing all characters of this <code>NonEmptyString</code>.
    */
  final def toSeq: collection.immutable.Seq[Char] = theString

  /**
    * Converts this <code>NonEmptyString</code> to a set.
    *
    * @return a set containing all characters of this <code>NonEmptyString</code>.
    */
  final def toSet: Set[Char] = theString.toSet

  /**
    * Converts this <code>NonEmptyString</code> to a stream.
    *
    * @return a stream containing all characters of this <code>NonEmptyString</code>.
    */
  final def toStream: Stream[Char] = theString.toStream

  /**
    * Returns a string representation of this <code>NonEmptyString</code>.
    *
    * @return the string <code>"NonEmptyString"</code> followed by the result of invoking <code>toString</code> on
    *   this <code>NonEmptyString</code>'s elements, surrounded by parentheses.
    */
  override def toString: String = stringPrefix + "(" + theString + ")"

  /**
    * Produces a new <code>NonEmptyString</code> that contains all characters of this <code>NonEmptyString</code> and also all characters of a given <code>Every</code>.
    *
    * <p>
    * <code>nonEmptyStringX</code> <code>union</code> <code>everyY</code> is equivalent to <code>nonEmptyStringX</code> <code>++</code> <code>everyY</code>.
    * </p>
    *
    * <p>
    * Another way to express this is that <code>nonEmptyStringX</code> <code>union</code> <code>everyY</code> computes the order-presevring multi-set union
    * of <code>nonEmptyStringX</code> and <code>everyY</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
    * also work on multi-sets.
    * </p>
    *
    * @param that the <code>Every</code> to add.
    * @return a new <code>NonEmptyString</code> that contains all characters of this <code>NonEmptyString</code> followed by all characters of <code>that</code> <code>Every</code>.
    */
  final def union(that: Every[Char]): NonEmptyString = new NonEmptyString((theString union that.toVector).mkString)

  /**
    * Produces a new <code>NonEmptyString</code> that contains all characters of this <code>NonEmptyString</code> and also all characters of a given <code>NonEmptyString</code>.
    *
    * <p>
    * <code>nonEmptyStringX</code> <code>union</code> <code>nonEmptyStringY</code> is equivalent to <code>nonEmptyStringX</code> <code>++</code> <code>nonEmptyStringY</code>.
    * </p>
    *
    * <p>
    * Another way to express this is that <code>nonEmptyStringX</code> <code>union</code> <code>nonEmptyStringY</code> computes the order-presevring multi-set union
    * of <code>nonEmptyStringX</code> and <code>nonEmptyStringY</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
    * also work on multi-sets.
    * </p>
    *
    * @param that the <code>NonEmptyString</code> to add.
    * @return a new <code>NonEmptyString</code> that contains all elements of this <code>NonEmptyString</code> followed by all characters of <code>that</code>.
    */
  final def union(that: NonEmptyString): NonEmptyString = new NonEmptyString((theString union that.theString).mkString)

  /**
    * Produces a new <code>NonEmptyString</code> that contains all characters of this <code>NonEmptyString</code> and also all characters of a given <code>GenSeq</code>.
    *
    * <p>
    * <code>nonEmptyStringX</code> <code>union</code> <code>ys</code> is equivalent to <code>nonEmptyStringX</code> <code>++</code> <code>ys</code>.
    * </p>
    *
    * <p>
    * Another way to express this is that <code>nonEmptyStringX</code> <code>union</code> <code>ys</code> computes the order-presevring multi-set union
    * of <code>nonEmptyStringX</code> and <code>ys</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
    * also work on multi-sets.
    * </p>
    *
    * @param that the <code>GenSeq</code> to add.
    * @return a new <code>NonEmptyString</code> that contains all elements of this <code>NonEmptyString</code> followed by all elements of <code>that</code> <code>GenSeq</code>.
    */
  final def union(that: GenSeq[Char]): NonEmptyString = new NonEmptyString((theString.union(that)).mkString)

  /**
    * Converts this <code>NonEmptyString</code> of pairs into two <code>NonEmptyString</code>s of the first and second half of each pair. 
    *
    * @tparam L the type of the first half of the character pairs
    * @tparam R the type of the second half of the character pairs
    * @param asPair an implicit conversion that asserts that the character type of this <code>NonEmptyString</code> is a pair.
    * @return a pair of <code>NonEmptyString</code>s, containing the first and second half, respectively, of each character pair of this <code>NonEmptyString</code>.
    */
  final def unzip[L, R](implicit asPair: Char => (L, R)): (GenIterable[L], GenIterable[R]) = {
    val unzipped = theString.unzip
    (unzipped._1, unzipped._2)
  }

  /**
    * Converts this <code>NonEmptyString</code> of triples into three <code>NonEmptyString</code>s of the first, second, and and third character of each triple.
    *
    * @tparam L the type of the first member of the character triples
    * @tparam R the type of the second member of the character triples
    * @tparam R the type of the third member of the character triples
    * @param asTriple an implicit conversion that character that the character type of this <code>NonEmptyString</code> is a triple.
    * @return a triple of <code>NonEmptyString</code>s, containing the first, second, and third member, respectively, of each character triple of this <code>NonEmptyString</code>.
    */
  final def unzip3[L, M, R](implicit asTriple: Char => (L, M, R)): (GenIterable[L], GenIterable[M], GenIterable[R]) = {
    val unzipped = theString.unzip3
    (unzipped._1, unzipped._2, unzipped._3)
  }

  /**
    * A copy of this <code>NonEmptyString</code> with one single replaced character.
    *
    * @param idx the position of the replacement
    * @param c the replacing character
    * @throws IndexOutOfBoundsException if the passed index is greater than or equal to the length of this <code>NonEmptyString</code>
    * @return a copy of this <code>NonEmptyString</code> with the character at position <code>idx</code> replaced by <code>c</code>.
    */
  final def updated(idx: Int, c: Char): NonEmptyString =
    new NonEmptyString(theString.updated(idx, c))

  /**
    * Returns a <code>NonEmptyString</code> formed from this <code>NonEmptyString</code> and an iterable collection by combining corresponding
    * elements in pairs. If one of the two collections is shorter than the other, placeholder elements will be used to extend the
    * shorter collection to the length of the longer.
    *
    * @tparam O the element type of the <code>other</code>
    * @param other the <code>Iterable</code> providing the second half of each result pair
    * @param thisElem the element to be used to fill up the result if this <code>NonEmptyString</code> is shorter than <code>that</code> <code>Iterable</code>.
    * @param otherElem the element to be used to fill up the result if <code>that</code> <code>Iterable</code> is shorter than this <code>NonEmptyString</code>.
    * @return a new <code>NonEmptyString</code> containing pairs consisting of corresponding characters of this <code>NonEmptyString</code> and <code>that</code>. The
    *     length of the returned collection is the maximum of the lengths of this <code>NonEmptyString</code> and <code>that</code>. If this <code>NonEmptyString</code>
    *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
    *     <code>NonEmptyString</code>, <code>thatElem</code> values are used to pad the result. 
    */
  final def zipAll[O](other: collection.Iterable[O], thisElem: Char, otherElem: O): GenIterable[(Char, O)] =
    theString.zipAll(other, thisElem, otherElem)

  /**
    * Returns a <code>NonEmptyString</code> formed from this <code>NonEmptyString</code> and another <code>NonEmptyString</code> combining corresponding
    * elements in pairs. If one of the two <code></code> is shorter than the other, placeholder elements will be used to extend the
    * shorter collection to the length of the longer.
    *
    * @param other the <code>NonEmptyString</code> providing the second half of each result pair
    * @param thisElem the character to be used to fill up the result if this <code>NonEmptyString</code> is shorter than <code>that</code> <code>NonEmptyString</code>.
    * @param otherElem the character to be used to fill up the result if <code>that</code> <code>Iterable</code> is shorter than this <code>NonEmptyString</code>.
    * @return a new <code>NonEmptyString</code> containing pairs consisting of corresponding characters of this <code>NonEmptyString</code> and <code>that</code>. The
    *     length of the returned collection is the maximum of the lengths of this <code>NonEmptyString</code> and <code>that</code>. If this <code>NonEmptyString</code>
    *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
    *     <code>NonEmptyString</code>, <code>thatElem</code> values are used to pad the result.
    */
  final def zipAll(other: NonEmptyString, thisElem: Char, otherElem: Char): GenIterable[(Char, Char)] =
    theString.zipAll(other.theString, thisElem, otherElem)

  /**
    * Zips this <code>NonEmptyString</code>  with its indices.
    *
    * @return A new <code>NonEmptyString</code> containing pairs consisting of all elements of this <code>NonEmptyString</code> paired with their index. Indices start at 0.
    */
  final def zipWithIndex: Iterable[(Char, Int)] = theString.zipWithIndex
}

/**
  * Companion object for class <code>NonEmptyString</code>.
  */
object NonEmptyString {

  /**
    * Constructs a new <code>NonEmptyString</code> given at least one element.
    *
    * @param s the <code>String</code> represented by this <code>NonEmptyString</code>
    */
  def apply(s: String): NonEmptyString = new NonEmptyString(s)

  /**
    * Constructs a new <code>NonEmptyString</code> given at least one character.
    *
    * @param firstChar the first character (with index 0) contained in this <code>NonEmptyString</code>
    * @param otherChars a varargs of zero or more other characters (with index 1, 2, 3, ...) contained in this <code>NonEmptyString</code>
    */
  def apply(firstChar: Char, otherChars: Char*): NonEmptyString = new NonEmptyString(firstChar + otherChars.mkString)

  /**
    * Variable argument extractor for <code>NonEmptyString</code>s.
    *
    * @param nonEmptyString: the <code>NonEmptyString</code> containing the elements to extract
    * @return an <code>Seq</code> containing this <code>NonEmptyString</code>s elements, wrapped in a <code>Some</code> 
    */
  //def unapplySeq(nonEmptyString: NonEmptyString): Option[Seq[Char]] = Some(nonEmptyString.theString)

  def unapplySeq(nonEmptyString: NonEmptyString): Option[Seq[String]] = Some(Seq(nonEmptyString.theString))

  /*
    // TODO: Figure out how to get case NonEmptyString() to not compile
    def unapplySeq[T](nonEmptyString: NonEmptyString[T]): Option[(T, Seq[T])] = Some(nonEmptyString.head, nonEmptyString.tail)
  */

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
      case Some(first) => Some(new NonEmptyString(seq.mkString))
    }

  import scala.language.implicitConversions

  /**
    * Implicit conversion from <code>NonEmptyString</code> to <code>IterableOnce[Char]</code>.
    *
    * <p>
    * One use case for this implicit conversion is to enable <code>GenSeq[NonEmptyString]</code>s to be flattened.
    * Here's an example:
    * </p>
    *
    * <pre class="stREPL">
    * scala&gt; Vector(NonEmptyString("123"), NonEmptyString("34"), NonEmptyString("5678")).flatten
    * res0: scala.collection.immutable.Vector[Char] = Vector('1', '2', '3', '3', '4', '5', '6', '7', '8')
    * </pre>
    *
    * @param nonEmptyString the <code>NonEmptyString</code> to convert to a <code>IterableOnce[Char]</code>
    * @return a <code>IterableOnce[Char]</code> containing the elements, in order, of this <code>NonEmptyString</code>
    */
  implicit def nonEmptyStringToIterableOnceOfChar(nonEmptyString: NonEmptyString): IterableOnce[Char] = nonEmptyString.theString

  implicit def nonEmptyStringToPartialFunction(nonEmptyString: NonEmptyString): PartialFunction[Int, Char] =
    new PartialFunction[Int, Char] {
      def isDefinedAt(idx: Int): Boolean = nonEmptyString.theString.isDefinedAt(idx)
      def apply(idx: Int): Char = nonEmptyString.theString(idx)
    }
}
