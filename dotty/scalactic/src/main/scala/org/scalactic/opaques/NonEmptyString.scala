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
import org.scalactic.Every
import scala.annotation.targetName

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
          def iterator: Iterator[Char] = nonEmptyString.iterator
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
        * Creates and returns a new iterator over all characters contained in this <code>NonEmptyString</code>.
        *
        * @return the new iterator
        */
      def iterator: Iterator[Char] = nonEmptyString.toList.iterator

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
        * Indicates whether this <code>NonEmptyString</code> ends with the given <code>Every</code>.
        *
        * @param that the <code>Every</code> to test
        * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
        */
      def endsWith(that: Every[Char]): Boolean = nonEmptyString.endsWith(that.mkString)
    }  
  }
}