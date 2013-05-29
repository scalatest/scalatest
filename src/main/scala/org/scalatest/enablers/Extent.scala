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
 * Supertrait for <code>Length</code> and <code>Size</code> type classes.
 *
 * <p>
 * This trait has two subtraits, <code>Length[T]</code> and <code>Size[T]</code>.
 * Objects of type T for which an implicit <code>Length[T]</code> is available can be used
 * with the <code>should have length</code> syntax.
 * Similarly, objects of type T for which an implicit <code>Size[T]</code> is available can be used
 * with the <code>should have size</code> syntax.
 * By creating an appropriate type class, therefore, you can enable the size and length checking syntax with arbitrary objects.
 * As an example, consider <code>java.net.DatagramPacket</code>, which has a <code>getLength</code> method. By default, this
 * can't be used with ScalaTest's <code>have length</code> syntax. 
 * </p>
 *
 * <pre>
 * scala&gt; import java.net.DatagramPacket
 * import java.net.DatagramPacket
 * 
 * scala&gt; import org.scalatest.matchers.ShouldMatchers._
 * import org.scalatest.matchers.ShouldMatchers._
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
 *     |     def extentOf(dp: DatagramPacket): Long = dp.getLength
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
sealed trait Extent[T] {

  /**
   * Returns the extent (<em>i.e.</em>, length or size) of the passed object.
   *
   * @param the object whose extent to return
   * @return the extent of the passed object
   */
  def extentOf(o: T): Long
}

/**
 * Supertrait for <code>Length</code> type classes.
 *
 * <p>
 * Trait <code>Length</code> is a type class trait for objects that can be queried for length.
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
 * scala&gt; import org.scalatest.matchers.ShouldMatchers._
 * import org.scalatest.matchers.ShouldMatchers._
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
 *     |     def extentOf(dp: DatagramPacket): Long = dp.getLength
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
trait Length[T] extends Extent[T]

object Length {

  implicit def enablersForJavaList[E, JLIST[_] <: java.util.List[_]]: Length[JLIST[E]] = 
    new Length[JLIST[E]] {
      def extentOf(javaList: JLIST[E]): Long = javaList.size
    }

  implicit def enablersForGenSeq[E, SEQ[_] <: scala.collection.GenSeq[_]]: Length[SEQ[E]] = 
    new Length[SEQ[E]] {
      def extentOf(seq: SEQ[E]): Long = seq.length
    }

  implicit def enablersForArray[E]: Length[Array[E]] = 
    new Length[Array[E]] {
      def extentOf(arr: Array[E]): Long = arr.length
    }

  implicit val enablersForString: Length[String] = 
    new Length[String] {
      def extentOf(str: String): Long = str.length
    }
}

/**
 * Supertrait for <code>Size</code> type classes.
 *
 * <p>
 * Trait <code>Size</code> is a type class trait for objects that can be queried for size.
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
 *      |     def extentOf(db: DataBufferByte): Long = db.getSize
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
trait Size[T] extends Extent[T]

object Size {

  implicit def enablersForJavaList[E, JLIST[_] <: java.util.List[_]]: Size[JLIST[E]] = 
    new Size[JLIST[E]] {
      def extentOf(javaList: JLIST[E]): Long = javaList.size
    }

  implicit def enablersForGenSeq[E, SEQ[_] <: scala.collection.GenSeq[_]]: Size[SEQ[E]] = 
    new Size[SEQ[E]] {
      def extentOf(seq: SEQ[E]): Long = seq.length
    }

  implicit def enablersForJavaCollection[E, JCOL[_] <: java.util.Collection[_]]: Size[JCOL[E]] = 
    new Size[JCOL[E]] {
      def extentOf(javaColl: JCOL[E]): Long = javaColl.size
    }

  implicit def enablersForJavaMap[K, V, JMAP[_, _] <: java.util.Map[_, _]]: Size[JMAP[K, V]] = 
    new Size[JMAP[K, V]] {
      def extentOf(javaMap: JMAP[K, V]): Long = javaMap.size
    }

  implicit def enablersForGenTraversable[E, TRAV[_] <: scala.collection.GenTraversable[_]]: Size[TRAV[E]] = 
    new Size[TRAV[E]] {
      def extentOf(trav: TRAV[E]): Long = trav.size
    }

  implicit def enablersForMap[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]]: Size[MAP[K, V]] =
    new Size[MAP[K, V]] {
      def extentOf(map: MAP[K, V]): Long = map.size
    }

  implicit def enablersForArray[E]: Size[Array[E]] = 
    new Size[Array[E]] {
      def extentOf(arr: Array[E]): Long = arr.length
    }

  implicit val enablersForString: Size[String] = 
    new Size[String] {
      def extentOf(str: String): Long = str.length
    }
}
