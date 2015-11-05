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

import org.scalactic.Equality
import org.scalatest.words.ArrayWrapper
import scala.collection.GenTraversable
import org.scalatest.FailureMessages
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import Aggregating.tryEquality

/**
 * Supertrait for typeclasses that enable <code>be empty</code> matcher syntax.
 *
 * <p>
 * An <code>Emptiness[T]</code> provides access to the "emptiness" of type <code>T</code> in such
 * a way that <code>be empty</code> matcher syntax can be used with type <code>T</code>. A <code>T</code>
 * can be any type that in some way can be empty. ScalaTest provides implicit implementations for several types. 
 * You can enable the <code>be empty</code> matcher syntax on your own type <code>U</code> by defining an <code>Emptiness[U]</code>
 * for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides implicit <code>Emptiness</code> instances for <code>scala.collection.GenTraversable</code>,
 * <code>java.util.Collection</code>, <code>java.util.Map</code>, <code>String</code>, <code>Array</code>, 
 * and <code>scala.Option</code> in the <code>Emptiness</code> companion object.
 * </p>
 */
trait Emptiness[-T] {

  /**
   * Determines whether the passed thing is readable, <em>i.e.</em>, the passed file is readable.
   *
   * @param thing the thing to check for emptiness
   * @return <code>true</code> if passed thing is empty, <code>false</code> otherwise
   */
  def isEmpty(thing: T): Boolean
}

/**
 * Companion object for <code>Emptiness</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>scala.collection.GenTraversable</code></li>
 * <li><code>String</code></li>
 * <li><code>Array</code></li>
 * <li><code>scala.Option</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li><code>java.util.Map</code></li>
 * <li>arbitary object with a <code>isEmpty()</code> method that returns <code>Boolean</code></li>
 * <li>arbitary object with a parameterless <code>isEmpty</code> method that returns <code>Boolean</code></li>
 * </ul>
 */
object Emptiness {

  import scala.language.higherKinds

  /**
   * Enable <code>Emptiness</code> implementation for <code>scala.collection.GenTraversable</code>
   *
   * @tparam E the type of the element in the <code>scala.collection.GenTraversable</code>
   * @tparam TRAV any subtype of <code>scala.collection.GenTraversable</code>
   * @return <code>Emptiness[TRAV[E]]</code> that supports <code>scala.collection.GenTraversable</code> in <code>be empty</code> syntax
   */
  implicit def emptinessOfGenTraversable[E, TRAV[e] <: scala.collection.GenTraversable[e]]: Emptiness[TRAV[E]] =
    new Emptiness[TRAV[E]] {
      def isEmpty(trav: TRAV[E]): Boolean = trav.isEmpty
    }
  
  /**
   * Enable <code>Emptiness</code> implementation for <code>Array</code>
   *
   * @tparam E the type of the element in the <code>Array</code>
   * @return <code>Emptiness[Array[E]]</code> that supports <code>Array</code> in <code>be empty</code> syntax
   */
  implicit def emptinessOfArray[E]: Emptiness[Array[E]] =
    new Emptiness[Array[E]] {
      def isEmpty(arr: Array[E]): Boolean = arr.length == 0
    }
  
  /**
   * Enable <code>Emptiness</code> implementation for <code>String</code>
   *
   * @return <code>Emptiness[String]</code> that supports <code>String</code> in <code>be empty</code> syntax
   */
  implicit def emptinessOfString: Emptiness[String] =
    new Emptiness[String] {
      def isEmpty(str: String): Boolean = str.isEmpty
    }
  
  /**
   * Enable <code>Emptiness</code> implementation for <code>scala.Option</code>
   *
   * @tparam E the type of the element in the <code>scala.Option</code>
   * @tparam OPT any subtype of <code>scala.Option</code>
   * @return <code>Emptiness[OPT[E]]</code> that supports <code>scala.Option</code> in <code>be empty</code> syntax
   */
  implicit def emptinessOfOption[E, OPT[e] <: Option[e]]: Emptiness[OPT[E]] =
    new Emptiness[OPT[E]] {
      def isEmpty(opt: OPT[E]): Boolean = opt.isEmpty
    }
  
  /**
   * Enable <code>Emptiness</code> implementation for <code>java.util.Collection</code>
   *
   * @tparam E the type of the element in the <code>java.util.Collection</code>
   * @tparam JCOL any subtype of <code>java.util.Collection</code>
   * @return <code>Emptiness[JCOL[E]]</code> that supports <code>java.util.Collection</code> in <code>be empty</code> syntax
   */
  implicit def emptinessOfJavaCollection[E, JCOL[e] <: java.util.Collection[e]]: Emptiness[JCOL[E]] =
    new Emptiness[JCOL[E]] {
      def isEmpty(jcol: JCOL[E]): Boolean = jcol.isEmpty
    }

  /**
   * Enable <code>Emptiness</code> implementation for <code>java.util.Map</code>
   *
   * @tparam K the type of the key in the <code>java.util.Map</code>
   * @tparam V the type of the value in the <code>java.util.Map</code>
   * @tparam JMAP any subtype of <code>java.util.Map</code>
   * @return <code>Emptiness[JMAP[K, V]]</code> that supports <code>java.util.Map</code> in <code>be empty</code> syntax
   */
  implicit def emptinessOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v]]: Emptiness[JMAP[K, V]] =
    new Emptiness[JMAP[K, V]] {
      def isEmpty(jmap: JMAP[K, V]): Boolean = jmap.isEmpty
    }

  import scala.language.reflectiveCalls
  
  /**
   * Enable <code>Emptiness</code> implementation for any arbitrary object with a <code>isEmpty()</code> method that returns <code>Boolean</code>
   *
   * @tparam T any type that has a <code>isEmpty()</code> method that returns <code>Boolean</code>
   * @return <code>Emptiness[T]</code> that supports <code>T</code> in <code>be empty</code> syntax
   */
  implicit def emptinessOfAnyRefWithIsEmptyMethod[T <: AnyRef { def isEmpty(): Boolean}]: Emptiness[T] = 
    new Emptiness[T] {
      def isEmpty(obj: T): Boolean = obj.isEmpty
    }
  
  /**
   * Enable <code>Emptiness</code> implementation for any arbitrary object with a <code>isEmpty</code> method that returns <code>Boolean</code>
   *
   * @tparam T any type that has a parameterless <code>isEmpty</code> method that returns <code>Boolean</code>
   * @return <code>Emptiness[T]</code> that supports <code>T</code> in <code>be empty</code> syntax
   */
  implicit def emptinessOfAnyRefWithParameterlessIsEmptyMethod[T <: AnyRef { def isEmpty: Boolean}]: Emptiness[T] = 
    new Emptiness[T] {
      def isEmpty(obj: T): Boolean = obj.isEmpty
    }
}

