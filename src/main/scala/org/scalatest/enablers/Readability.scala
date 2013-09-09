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

import org.scalautils.Equality
import org.scalatest.words.ArrayWrapper
import scala.collection.GenTraversable
import org.scalatest.FailureMessages
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import Aggregating.tryEquality

/**
 * Supertrait for typeclasses that enable the <code>be readable</code> matcher syntax.
 *
 * <p>
 * A <code>Readability[T]</code> provides access to the "readable nature" of type <code>T</code> in such
 * a way that <code>be readable</code> matcher syntax can be used with type <code>T</code>. A <code>T</code>
 * can be any type for which the concept of being readable makes sense, such as <code>java.io.File</code>.
 * You can enable the <code>be readable</code> matcher syntax on your own type <code>U</code> by defining a
 * <code>Readability[U]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides an implicit <code>Readability</code> instance for <code>java.io.File</code> and arbitary
 * object with <code>isReadable()</code> or <code>isReadable</code> in the <code>Readability</code> companion object.
 * </p>
 */
trait Readability[-T] {

  /**
   * Determines whether the passed thing is readable, <em>i.e.</em>, the passed file is readable.
   *
   * @param thing the thing to check for readability
   * @return <code>true</code> if the passed thing is readable, <code>false</code> otherwise
   *
   */
  def isReadable(thing: T): Boolean
}

/**
 * Companion object for <code>Readability</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>java.io.File</code></li>
 * <li>arbitary object with a <code>isReadable()</code> method that returns <code>Boolean</code></li>
 * <li>arbitary object with a parameterless <code>isReadable</code> method that returns <code>Boolean</code></li>
 * </ul>
 */
object Readability {

  /**
   * Enable <code>Readability</code> implementation for <code>java.io.File</code>.
   *
   * @tparam FILE any subtype of <code>java.io.File</code>
   * @return <code>Readability[FILE]</code> that supports <code>java.io.File</code> in <code>be readable</code> syntax
   */
  implicit def readabilityOfFile[FILE <: java.io.File]: Readability[FILE] =
    new Readability[FILE] {
      def isReadable(file: FILE): Boolean = file.canRead
    }

  /**
   * Enable <code>Readability</code> implementation for any arbitrary object with a <code>isReadable()</code> method that returns <code>Boolean</code>
   *
   * @tparam T any type that has a <code>isReadable()</code> method that returns <code>Boolean</code>
   * @return <code>Readability[T]</code> that supports <code>T</code> in <code>be readable</code> syntax
   */
  implicit def readabilityOfAnyRefWithIsReadableMethod[T <: AnyRef { def isReadable(): Boolean}]: Readability[T] = 
    new Readability[T] {
      def isReadable(obj: T): Boolean = obj.isReadable
    }

  /**
   * Enable <code>Readability</code> implementation for any arbitrary object with a parameterless <code>isReadable</code> method that returns <code>Boolean</code>
   *
   * @tparam T any type that has a parameterless <code>isReadable</code> method that returns <code>Boolean</code>
   * @return <code>Readability[T]</code> that supports <code>T</code> in <code>be readable</code> syntax
   */
  implicit def readabilityOfAnyRefWithParameterlessIsReadableMethod[T <: AnyRef { def isReadable: Boolean}]: Readability[T] = 
    new Readability[T] {
      def isReadable(obj: T): Boolean = obj.isReadable
    }
}

