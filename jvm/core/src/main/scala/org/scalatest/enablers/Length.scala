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
package org.scalatest.enablers

/**
 * Supertrait for <code>Length</code> typeclasses.
 *
 * <p>
 * Trait <code>Length</code> is a typeclass trait for objects that can be queried for length.
 * Objects of type T for which an implicit <code>Length[T]</code> is available can be used
 * with the <code>should have length</code> syntax.
 * In other words, this trait enables you to use the length checking
 * syntax with arbitrary objects. As an example, the following <code>Bridge</code> class:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import enablers.Length
 * import enablers.Length
 *
 * scala&gt; import Matchers._
 * import Matchers._
 *
 * scala&gt; case class Bridge(span: Int)
 * defined class Bridge
 * </pre>
 *
 * <p>
 * Out of the box you can't use the <code>should have length</code> syntax with <code>Bridge</code>,
 * because ScalaTest doesn't know that a bridge's span means its length:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val bridge = new Bridge(2000)
 * bridge: Bridge = Bridge(2000)
 *
 * scala&gt; bridge should have length 2000
 * &lt;console&gt;:34: error: could not find implicit value for
 *     parameter len: org.scalatest.enablers.Length[Bridge]
 *       bridge should have length 2000
 *                          ^
 * </pre>
 *
 * <p>
 * You can teach this to ScalaTest, however, by defining an implicit <code>Length[Bridge]</code>.
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; implicit val lengthOfBridge: Length[Bridge] =
 *      |   new Length[Bridge] {
 *      |     def lengthOf(b: Bridge): Long = b.span
 *      |   }
 * lengthOfBridge: org.scalatest.enablers.Length[Bridge] = $anon$1@3fa27a4a
 * </pre>
 *
 * <p>
 * With the implicit <code>Length[Bridge]</code> in scope, you can now use ScalaTest's <code>should have length</code>
 * syntax with <code>Bridge</code> instances:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; bridge should have length 2000
 * res4: org.scalatest.Assertion = Succeeded
 * 
 * scala&gt; bridge should have length 2001
 * org.scalatest.exceptions.TestFailedException: Bridge(2000) had length 2000 instead of expected length 2001
 *   at org.scalatest.MatchersHelper$.newTestFailedException(MatchersHelper.scala:148)
 *   at org.scalatest.MatchersHelper$.indicateFailure(MatchersHelper.scala:366)
 *   at org.scalatest.Matchers$ResultOfHaveWordForExtent.length(Matchers.scala:2720)
 *   ... 43 elided
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

  //DOTTY-ONLY import scala.reflect.Selectable.reflectiveSelectable

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
      def lengthOf(obj: T): Long = obj.length()
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
      def lengthOf(obj: T): Long = obj.getLength()
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
      def lengthOf(obj: T): Long = obj.length()
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
      def lengthOf(obj: T): Long = obj.getLength()
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

