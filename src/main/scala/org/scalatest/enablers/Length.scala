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
package org.scalatest.enablers

/**
 * Supertrait for <code>Length</code> typeclasses.
 *
 * <p>
 * Trait <code>Length</code> is a typeclass trait for objects that can be queried for length.
 * Objects of type T for which an implicit <code>Length[T]</code> is available can be used
 * with the <code>should have length</code> syntax.
 * In other words, this trait enables you to use the length checking
 * syntax with arbitrary objects. As an example, consider
 * <code>java.net.DatagramPacket</code>, which has a <code>getLength</code> method. By default, this
 * can't be used with ScalaTest's <code>have length</code> syntax. 
 * </p>
 *
 * <pre>
 * scala&gt; import java.net.DatagramPacket
 * import java.net.DatagramPacket
 * 
 * scala&gt; import org.scalatest.Matchers._
 * import org.scalatest.Matchers._
 *
 * scala&gt; val dp = new DatagramPacket(Array(0x0, 0x1, 0x2, 0x3), 4)
 * dp: java.net.DatagramPacket = java.net.DatagramPacket@54906181
 * 
 * scala&gt; dp.getLength
 * res0: Int = 4
 *
 * scala&gt; dp should have length 4
 * <console>:13: error: could not find implicit value for parameter ev: org.scalatest.matchers.ShouldMatchers.Extent[java.net.DatagramPacket]
 *          dp should have length 4
 *             ^
 *
 * scala&gt; implicit val lengthOfDatagramPacket =
 *     |   new Length[DatagramPacket] {
 *     |     def lengthOf(dp: DatagramPacket): Long = dp.getLength
 *     |   }
 * lengthOfDatagramPacket: java.lang.Object with org.scalatest.matchers.ShouldMatchers.Length[java.net.DatagramPacket] = $anon$1@550c6b37
 *
 * scala&gt; dp should have length 4
 *
 * scala&gt; dp should have length 3
 * org.scalatest.exceptions.TestFailedException:  java.net.DatagramPacket@54906181 had length 4, not length 3
 * </pre>
 *
 * @author Bill Venners
 */
trait Length[T] {

  /**
   * Returns the length of the passed object.
   *
   * @param obj the object whose length to return
   * @return the length of the passed object
   */
  def lengthOf(obj: T): Long
}

/**
 * Companion object for <code>Length</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>scala.collection.GenSeq</code></li>
 * <li><code>String</code></li>
 * <li><code>Array</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li>arbitary object with a <code>length()</code> method that returns <code>Int</code></li>
 * <li>arbitary object with a parameterless <code>length</code> method that returns <code>Int</code></li>
 * <li>arbitary object with a <code>getLength()</code> method that returns <code>Int</code></li>
 * <li>arbitary object with a parameterless <code>getLength</code> method that returns <code>Int</code></li>
 * <li>arbitary object with a <code>length()</code> method that returns <code>Long</code></li>
 * <li>arbitary object with a parameterless <code>length</code> method that returns <code>Long</code></li>
 * <li>arbitary object with a <code>getLength()</code> method that returns <code>Long</code></li>
 * <li>arbitary object with a parameterless <code>getLength</code> method that returns <code>Long</code></li>
 * </ul>
 */
object Length {

  /**
   * Enable <code>Length</code> implementation for <code>java.util.List</code>
   *
   * @tparam JLIST any subtype of <code>java.util.List</code>
   * @return <code>Length[JLIST]</code> that supports <code>java.util.List</code> in <code>have length</code> syntax
   */
  implicit def lengthOfJavaList[JLIST <: java.util.List[_]]: Length[JLIST] = 
    new Length[JLIST] {
      def lengthOf(javaList: JLIST): Long = javaList.size
    }

  /**
   * Enable <code>Length</code> implementation for <code>scala.collection.GenSeq</code>
   *
   * @tparam SEQ any subtype of <code>scala.collection.GenSeq</code>
   * @return <code>Length[SEQ]</code> that supports <code>scala.collection.GenSeq</code> in <code>have length</code> syntax
   */
  implicit def lengthOfGenSeq[SEQ <: scala.collection.GenSeq[_]]: Length[SEQ] = 
    new Length[SEQ] {
      def lengthOf(seq: SEQ): Long = seq.length
    }

  /**
   * Enable <code>Length</code> implementation for <code>Array</code>
   *
   * @tparam E the type of the element in the <code>Array</code>
   * @return <code>Length[Array[E]]</code> that supports <code>Array</code> in <code>have length</code> syntax
   */
  implicit def lengthOfArray[E]: Length[Array[E]] = 
    new Length[Array[E]] {
      def lengthOf(arr: Array[E]): Long = arr.length
    }

  /**
   * Enable <code>Length</code> implementation for <code>String</code>
   *
   * @return <code>Length[String]</code> that supports <code>String</code> in <code>have length</code> syntax
   */
  implicit val lengthOfString: Length[String] = 
    new Length[String] {
      def lengthOf(str: String): Long = str.length
    }

  import scala.language.reflectiveCalls

  /**
   * Enable <code>Length</code> implementation for arbitary object with <code>length()</code> method that returns <code>Int</code>.
   *
   * @tparam T any type with <code>length()</code> method that returns <code>Int</code>
   * @return <code>Length[T]</code> that supports <code>T</code> in <code>have length</code> syntax
   */
  implicit def lengthOfAnyRefWithLengthMethodForInt[T <: AnyRef { def length(): Int}]: Length[T] = 
    new Length[T] {
      def lengthOf(obj: T): Long = obj.length
    }

  /**
   * Enable <code>Length</code> implementation for arbitary object with parameterless <code>length</code> method that returns <code>Int</code>.
   *
   * @tparam T any type with parameterless <code>length</code> method that returns <code>Int</code>
   * @return <code>Length[T]</code> that supports <code>T</code> in <code>have length</code> syntax
   */
  implicit def lengthOfAnyRefWithParameterlessLengthMethodForInt[T <: AnyRef { def length: Int}]: Length[T] = 
    new Length[T] {
      def lengthOf(obj: T): Long = obj.length
    }

  /**
   * Enable <code>Length</code> implementation for arbitary object with <code>getLength()</code> method that returns <code>Int</code>.
   *
   * @tparam T any type with <code>getLength()</code> method that returns <code>Int</code>
   * @return <code>Length[T]</code> that supports <code>T</code> in <code>have length</code> syntax
   */
  implicit def lengthOfAnyRefWithGetLengthMethodForInt[T <: AnyRef { def getLength(): Int}]: Length[T] = 
    new Length[T] {
      def lengthOf(obj: T): Long = obj.getLength
    }

  /**
   * Enable <code>Length</code> implementation for arbitary object with parameterless <code>getLength</code> method that returns <code>Int</code>.
   *
   * @tparam T any type with parameterless <code>getLength</code> method that returns <code>Int</code>
   * @return <code>Length[T]</code> that supports <code>T</code> in <code>have length</code> syntax
   */
  implicit def lengthOfAnyRefWithParameterlessGetLengthMethodForInt[T <: AnyRef { def getLength: Int}]: Length[T] = 
    new Length[T] {
      def lengthOf(obj: T): Long = obj.getLength
    }

  /**
   * Enable <code>Length</code> implementation for arbitary object with <code>length()</code> method that returns <code>Long</code>.
   *
   * @tparam T any type with <code>length()</code> method that returns <code>Long</code>
   * @return <code>Length[T]</code> that supports <code>T</code> in <code>have length</code> syntax
   */
  implicit def lengthOfAnyRefWithLengthMethodForLong[T <: AnyRef { def length(): Long}]: Length[T] = 
    new Length[T] {
      def lengthOf(obj: T): Long = obj.length
    }

  /**
   * Enable <code>Length</code> implementation for arbitary object with parameterless <code>length</code> method that returns <code>Long</code>.
   *
   * @tparam T any type with parameterless <code>length</code> method that returns <code>Long</code>
   * @return <code>Length[T]</code> that supports <code>T</code> in <code>have length</code> syntax
   */
  implicit def lengthOfAnyRefWithParameterlessLengthMethodForLong[T <: AnyRef { def length: Long}]: Length[T] = 
    new Length[T] {
      def lengthOf(obj: T): Long = obj.length
    }

  /**
   * Enable <code>Length</code> implementation for arbitary object with <code>getLength()</code> method that returns <code>Long</code>.
   *
   * @tparam T any type with <code>getLength()</code> method that returns <code>Long</code>
   * @return <code>Length[T]</code> that supports <code>T</code> in <code>have length</code> syntax
   */
  implicit def lengthOfAnyRefWithGetLengthMethodForLong[T <: AnyRef { def getLength(): Long}]: Length[T] = 
    new Length[T] {
      def lengthOf(obj: T): Long = obj.getLength
    }

  /**
   * Enable <code>Length</code> implementation for arbitary object with parameterless <code>getLength</code> method that returns <code>Long</code>.
   *
   * @tparam T any type with parameterless <code>getLength</code> method that returns <code>Long</code>
   * @return <code>Length[T]</code> that supports <code>T</code> in <code>have length</code> syntax
   */
  implicit def lengthOfAnyRefWithParameterlessGetLengthMethodForLong[T <: AnyRef { def getLength: Long}]: Length[T] = 
    new Length[T] {
      def lengthOf(obj: T): Long = obj.getLength
    }
}

