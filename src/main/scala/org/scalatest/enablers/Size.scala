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
 * <code>java.net.DatagramPacket</code>, which has a <code>getLength</code> method. By default, this
 * can't be used with ScalaTest's <code>have length</code> syntax. 
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
   * @param the object whose size to return
   * @return the size of the passed object
   */
  def sizeOf(o: T): Long
}

object Size {

  implicit def enablersForJavaList[E, JLIST[_] <: java.util.List[_]]: Size[JLIST[E]] = 
    new Size[JLIST[E]] {
      def sizeOf(javaList: JLIST[E]): Long = javaList.size
    }

  implicit def enablersForGenSeq[E, SEQ[_] <: scala.collection.GenSeq[_]]: Size[SEQ[E]] = 
    new Size[SEQ[E]] {
      def sizeOf(seq: SEQ[E]): Long = seq.length
    }

  implicit def enablersForJavaCollection[E, JCOL[_] <: java.util.Collection[_]]: Size[JCOL[E]] = 
    new Size[JCOL[E]] {
      def sizeOf(javaColl: JCOL[E]): Long = javaColl.size
    }

  implicit def enablersForJavaMap[K, V, JMAP[_, _] <: java.util.Map[_, _]]: Size[JMAP[K, V]] = 
    new Size[JMAP[K, V]] {
      def sizeOf(javaMap: JMAP[K, V]): Long = javaMap.size
    }

  implicit def enablersForGenTraversable[E, TRAV[_] <: scala.collection.GenTraversable[_]]: Size[TRAV[E]] = 
    new Size[TRAV[E]] {
      def sizeOf(trav: TRAV[E]): Long = trav.size
    }

  implicit def enablersForMap[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]]: Size[MAP[K, V]] =
    new Size[MAP[K, V]] {
      def sizeOf(map: MAP[K, V]): Long = map.size
    }

  implicit def enablersForArray[E]: Size[Array[E]] = 
    new Size[Array[E]] {
      def sizeOf(arr: Array[E]): Long = arr.length
    }

  implicit val enablersForString: Size[String] = 
    new Size[String] {
      def sizeOf(str: String): Long = str.length
    }
}
