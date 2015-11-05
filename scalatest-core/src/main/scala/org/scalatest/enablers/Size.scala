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
 * syntax with arbitrary objects. As an example, consider
 * <code>java.net.DatagramPacket</code>, which has a <code>getSize</code> method. By default, this
 * can't be used with ScalaTest's <code>have size</code> syntax.
 * </p>
 *
 * <pre>
 * scala&gt; import java.awt.image.DataBufferByte
 * import java.awt.image.DataBufferByte
 * 
 * scala&gt; import org.scalatest.matchers.ShouldMatchers._
 * import org.scalatest.matchers.ShouldMatchers._
 *
 * scala&gt; val db = new DataBufferByte(4)
 * db: java.awt.image.DataBufferByte = java.awt.image.DataBufferByte@33d5e94f
 * 
 * scala&gt; db.getSize
 * res0: Int = 4
 *
 * scala&gt; db should have size 4
 * <console>:17: error: could not find implicit value for parameter ev: org.scalatest.matchers.ShouldMatchers.Extent[java.awt.image.DataBufferByte]
 *               db should have size 4
 *                  ^
 * scala&gt; implicit val sizeOfDataBufferByte =
 *      |   new Size[DataBufferByte] {
 *      |     def sizeOf(db: DataBufferByte): Long = db.getSize
 *      |   }
 * sizeOfDataBufferByte: java.lang.Object with org.scalatest.matchers.ShouldMatchers.Size[java.awt.image.DataBufferByte] = $anon$1@4c69bdf8
 *
 * scala&gt; db should have size 4
 *
 * scala&gt; db should have size 3
 * org.scalatest.exceptions.TestFailedException:  java.awt.image.DataBufferByte@33d5e94f had size 4, not size 3
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
 * Companion object for <code>Length</code> that provides implicit implementations for the following types:
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
      def sizeOf(obj: T): Long = obj.size
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
      def sizeOf(obj: T): Long = obj.getSize
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
      def sizeOf(obj: T): Long = obj.size
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
      def sizeOf(obj: T): Long = obj.getSize
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
