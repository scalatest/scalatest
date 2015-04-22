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
import org.scalactic.ArrayWrapper
import scala.collection.GenTraversable
import scala.annotation.tailrec
import scala.collection.JavaConverters._

/**
 * Supertrait for typeclasses that enable the <code>be</code> <code>writable</code> matcher syntax.
 *
 * <p>
 * A <code>Writability[T]</code> provides access to the "writable nature" of type <code>T</code> in such
 * a way that <code>be</code> <code>writable</code> matcher syntax can be used with type <code>T</code>. A <code>T</code>
 * can be any type for which the concept of being writable makes sense, such as <code>java.io.File</code>. ScalaTest provides
 * implicit implementation for <code>java.io.File</code>. You can enable the <code>be</code> <code>writable</code> matcher syntax on your own
 * type <code>U</code> by defining a <code>Writability[U]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides an implicit <code>Writability</code> instance for <code>java.io.File</code> and arbitary
 * object with <code>isWritable()</code> or <code>isWritable</code> in the <a href="Writability$.html"><code>Writability</code> companion object</a>.
 * </p>
 */
trait Writability[-T] {

  /**
   * Determines whether the passed thing is writable, <em>i.e.</em>, the passed file is writable.
   *
   * @param thing the thing to check for writability
   * @return <code>true</code> if the passed thing is writable, <code>false</code> otherwise
   */
  def isWritable(thing: T): Boolean
}

/**
 * Companion object for <code>Writability</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>java.io.File</code></li>
 * <li>arbitary object with a <code>isWritable()</code> method that returns <code>Boolean</code></li>
 * <li>arbitary object with a parameterless <code>isWritable</code> method that returns <code>Boolean</code></li>
 * </ul>
 */
object Writability {

  /**
   * Enable <code>Writability</code> implementation for <code>java.io.File</code>.
   *
   * @tparam FILE any subtype of <code>java.io.File</code>
   * @return <code>Writability[FILE]</code> that supports <code>java.io.File</code> in <code>be</code> <code>writable</code> syntax
   */
  implicit def writabilityOfFile[FILE <: java.io.File]: Writability[FILE] =
    new Writability[FILE] {
      def isWritable(file: FILE): Boolean = file.canWrite
    }

  import scala.language.reflectiveCalls

  /**
   * Enable <code>Writability</code> implementation for any arbitrary object with a <code>isWritable()</code> method that returns <code>Boolean</code>
   *
   * @tparam T any type that has a <code>isWritable()</code> method that returns <code>Boolean</code>
   * @return <code>Writability[T]</code> that supports <code>T</code> in <code>be</code> <code>writable</code> syntax
   */
  implicit def writabilityOfAnyRefWithIsWritableMethod[T <: AnyRef { def isWritable(): Boolean}]: Writability[T] = 
    new Writability[T] {
      def isWritable(obj: T): Boolean = obj.isWritable
    }

  /**
   * Enable <code>Writability</code> implementation for any arbitrary object with a parameterless <code>isWritable</code> method that returns <code>Boolean</code>
   *
   * @tparam T any type that has a parameterless <code>isWritable</code> method that returns <code>Boolean</code>
   * @return <code>Writability[T]</code> that supports <code>T</code> in <code>be</code> <code>writable</code> syntax
   */
  implicit def writabilityOfAnyRefWithParameterlessIsWritableMethod[T <: AnyRef { def isWritable: Boolean}]: Writability[T] = 
    new Writability[T] {
      def isWritable(obj: T): Boolean = obj.isWritable
    }
}

