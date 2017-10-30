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
 * Supertrait for <code>Size</code> typeclasses.
 *
 * <p>
 * Trait <code>Size</code> is a typeclass trait for objects that can be queried for size.
 * Objects of type T for which an implicit <code>Size[T]</code> is available can be used
 * with the <code>should have size</code> syntax.
 * In other words, this trait enables you to use the size checking
 * syntax with arbitrary objects. As an example, the following <code>Bridge</code> class:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import enablers.Size
 * import enablers.Size
 *
 * scala&gt; import Matchers._
 * import Matchers._
 *
 * scala&gt; case class Bridge(span: Int)
 * defined class Bridge
 * </pre>
 *
 * <p>
 * Out of the box you can't use the <code>should have size</code> syntax with <code>Bridge</code>,
 * because ScalaTest doesn't know that a bridge's span means its size:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val bridge = new Bridge(2000)
 * bridge: Bridge = Bridge(2000)
 *
 * scala&gt; bridge should have size 2000
 * &lt;console&gt;:34: error: could not find implicit value for
 *     parameter sz: org.scalatest.enablers.Size[Bridge]
 *       bridge should have size 2000
 *                          ^
 * </pre>
 *
 * <p>
 * You can teach this to ScalaTest, however, by defining an implicit <code>Size[Bridge]</code>.
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; implicit val sizeOfBridge: Size[Bridge] =
 *      |   new Size[Bridge] {
 *      |     def sizeOf(b: Bridge): Long = b.span
 *      |   }
 * sizeOfBridge: org.scalatest.enablers.Size[Bridge] = $anon$1@3fa27a4a
 * </pre>
 *
 * <p>
 * With the implicit <code>Size[Bridge]</code> in scope, you can now use ScalaTest's <code>should have size</code>
 * syntax with <code>Bridge</code> instances:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; bridge should have size 2000
 * res4: org.scalatest.Assertion = Succeeded
 * 
 * scala&gt; bridge should have size 2001
 * org.scalatest.exceptions.TestFailedException: Bridge(2000) had size 2000 instead of expected size 2001
 *   at org.scalatest.MatchersHelper$.newTestFailedException(MatchersHelper.scala:148)
 *   at org.scalatest.MatchersHelper$.indicateFailure(MatchersHelper.scala:366)
 *   at org.scalatest.Matchers$ResultOfHaveWordForExtent.size(Matchers.scala:2720)
 *   ... 43 elided
 * </pre>
 *
 * @author Bill Venners
 */
trait Size[T] {

  /**
   * Returns the size of the passed object.
   *
   * @param obj the object whose size to return
   * @return the size of the passed object
   */
  def sizeOf(obj: T): Long
}

/**
 * Companion object for <code>Size</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>scala.collection.GenTraversable</code></li>
 * <li><code>String</code></li>
 * <li><code>Array</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li><code>java.util.Map</code></li>
 * <li>arbitary object with a <code>size()</code> method that returns <code>Int</code></li>
 * <li>arbitary object with a parameterless <code>size</code> method that returns <code>Int</code></li>
 * <li>arbitary object with a <code>getSize()</code> method that returns <code>Int</code></li>
 * <li>arbitary object with a parameterless <code>getSize</code> method that returns <code>Int</code></li>
 * <li>arbitary object with a <code>size()</code> method that returns <code>Long</code></li>
 * <li>arbitary object with a parameterless <code>size</code> method that returns <code>Long</code></li>
 * <li>arbitary object with a <code>getSize()</code> method that returns <code>Long</code></li>
 * <li>arbitary object with a parameterless <code>getSize</code> method that returns <code>Long</code></li>
 * </ul>
 */
object Size {

  // DOTTY-ONLY import scala.reflect.Selectable.reflectiveSelectable

  /**
   * Enable <code>Size</code> implementation for <code>java.util.Collection</code>
   *
   * @tparam JCOL any subtype of <code>java.util.Collection</code>
   * @return <code>Size[JCOL]</code> that supports <code>java.util.Collection</code> in <code>have size</code> syntax
   */
  implicit def sizeOfJavaCollection[JCOL <: java.util.Collection[_]]: Size[JCOL] = 
    new Size[JCOL] {
      def sizeOf(javaColl: JCOL): Long = javaColl.size
    }

  /**
   * Enable <code>Size</code> implementation for <code>java.util.Map</code>
   *
   * @tparam JMAP any subtype of <code>java.util.Map</code>
   * @return <code>Size[JMAP]</code> that supports <code>java.util.Map</code> in <code>have size</code> syntax
   */
  implicit def sizeOfJavaMap[JMAP <: java.util.Map[_, _]]: Size[JMAP] = 
    new Size[JMAP] {
      def sizeOf(javaMap: JMAP): Long = javaMap.size
    }

  /**
   * Enable <code>Size</code> implementation for <code>scala.collection.GenTraversable</code>
   *
   * @tparam TRAV any subtype of <code>scala.collection.GenTraversable</code>
   * @return <code>Size[TRAV]</code> that supports <code>scala.collection.GenTraversable</code> in <code>have size</code> syntax
   */
  implicit def sizeOfGenTraversable[TRAV <: scala.collection.GenTraversable[_]]: Size[TRAV] = 
    new Size[TRAV] {
      def sizeOf(trav: TRAV): Long = trav.size
    }

  /**
   * Enable <code>Size</code> implementation for <code>Array</code>
   *
   * @tparam E the type of the element in the <code>Array</code>
   * @return <code>Size[Array[E]]</code> that supports <code>Array</code> in <code>have size</code> syntax
   */
  implicit def sizeOfArray[E]: Size[Array[E]] = 
    new Size[Array[E]] {
      def sizeOf(arr: Array[E]): Long = arr.length
    }

  /**
   * Enable <code>Size</code> implementation for <code>String</code>
   *
   * @return <code>Size[String]</code> that supports <code>String</code> in <code>have size</code> syntax
   */
  implicit val sizeOfString: Size[String] = 
    new Size[String] {
      def sizeOf(str: String): Long = str.length
    }

  import scala.language.reflectiveCalls

  /**
   * Enable <code>Size</code> implementation for arbitary object with <code>size()</code> method that returns <code>Int</code>.
   *
   * @tparam T any type with <code>size()</code> method that returns <code>Int</code>
   * @return <code>Size[T]</code> that supports <code>T</code> in <code>have size</code> syntax
   */
  implicit def sizeOfAnyRefWithSizeMethodForInt[T <: AnyRef { def size(): Int}]: Size[T] = 
    new Size[T] {
      def sizeOf(obj: T): Long = obj.size()
    }

  /**
   * Enable <code>Size</code> implementation for arbitary object with parameterless <code>size</code> method that returns <code>Int</code>.
   *
   * @tparam T any type with parameterless <code>size</code> method that returns <code>Int</code>
   * @return <code>Size[T]</code> that supports <code>T</code> in <code>have size</code> syntax
   */
  implicit def sizeOfAnyRefWithParameterlessSizeMethodForInt[T <: AnyRef { def size: Int}]: Size[T] = 
    new Size[T] {
      def sizeOf(obj: T): Long = obj.size
    }

  /**
   * Enable <code>Size</code> implementation for arbitary object with <code>getSize()</code> method that returns <code>Int</code>.
   *
   * @tparam T any type with <code>getSize()</code> method that returns <code>Int</code>
   * @return <code>Size[T]</code> that supports <code>T</code> in <code>have size</code> syntax
   */
  implicit def sizeOfAnyRefWithGetSizeMethodForInt[T <: AnyRef { def getSize(): Int}]: Size[T] = 
    new Size[T] {
      def sizeOf(obj: T): Long = obj.getSize()
    }

  /**
   * Enable <code>Size</code> implementation for arbitary object with parameterless <code>getSize</code> method that returns <code>Int</code>.
   *
   * @tparam T any type with parameterless <code>getSize</code> method that returns <code>Int</code>
   * @return <code>Size[T]</code> that supports <code>T</code> in <code>have size</code> syntax
   */
  implicit def sizeOfAnyRefWithParameterlessGetSizeMethodForInt[T <: AnyRef { def getSize: Int}]: Size[T] = 
    new Size[T] {
      def sizeOf(obj: T): Long = obj.getSize
    }

  /**
   * Enable <code>Size</code> implementation for arbitary object with <code>size()</code> method that returns <code>Long</code>.
   *
   * @tparam T any type with <code>size()</code> method that returns <code>Long</code>
   * @return <code>Size[T]</code> that supports <code>T</code> in <code>have size</code> syntax
   */
  implicit def sizeOfAnyRefWithSizeMethodForLong[T <: AnyRef { def size(): Long}]: Size[T] = 
    new Size[T] {
      def sizeOf(obj: T): Long = obj.size()
    }

  /**
   * Enable <code>Size</code> implementation for arbitary object with parameterless <code>size</code> method that returns <code>Long</code>.
   *
   * @tparam T any type with parameterless <code>size</code> method that returns <code>Long</code>
   * @return <code>Size[T]</code> that supports <code>T</code> in <code>have size</code> syntax
   */
  implicit def sizeOfAnyRefWithParameterlessSizeMethodForLong[T <: AnyRef { def size: Long}]: Size[T] = 
    new Size[T] {
      def sizeOf(obj: T): Long = obj.size
    }

  /**
   * Enable <code>Size</code> implementation for arbitary object with <code>getSize()</code> method that returns <code>Long</code>.
   *
   * @tparam T any type with <code>getSize()</code> method that returns <code>Long</code>
   * @return <code>Size[T]</code> that supports <code>T</code> in <code>have size</code> syntax
   */
  implicit def sizeOfAnyRefWithGetSizeMethodForLong[T <: AnyRef { def getSize(): Long}]: Size[T] = 
    new Size[T] {
      def sizeOf(obj: T): Long = obj.getSize()
    }

  /**
   * Enable <code>Size</code> implementation for arbitary object with <code>getSize</code> method that returns <code>Long</code>.
   *
   * @tparam T any type with <code>getSize</code> method that returns <code>Long</code>
   * @return <code>Size[T]</code> that supports <code>T</code> in <code>have size</code> syntax
   */
  implicit def sizeOfAnyRefWithParameterlessGetSizeMethodForLong[T <: AnyRef { def getSize: Long}]: Size[T] = 
    new Size[T] {
      def sizeOf(obj: T): Long = obj.getSize
    }
}
