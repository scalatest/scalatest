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
 * Supertrait for typeclasses that enable the <code>exist</code> matcher syntax.
 *
 * <p>
 * An <code>Existence[S]</code> provides access to the "existence nature" of type <code>S</code> in such
 * a way that <code>exist</code> matcher syntax can be used with type <code>S</code>. A <code>S</code>
 * can be any type for which the concept of existence makes sense, such as <code>java.io.File</code>. ScalaTest provides
 * implicit implementations for <code>java.io.File</code>. You can enable the <code>exist</code> matcher syntax on your own
 * type <code>U</code> by defining a <code>Existence[U]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides an implicit <code>Existence</code> instance for <code>java.io.File</code>
 * in the <code>Existence</code> companion object.
 * </p>
 */
trait Existence[-S] {

  /**
   * Determines whether the passed thing exists, <em>i.e.</em>, whether the passed <code>java.io.File</code> exists.
   *
   * @param thing the thing to check for existence
   * @return <code>true</code> if passed thing exists, <code>false</code> otherwise
   */
  def exists(thing: S): Boolean
}

/**
 * Companion object for <code>Existence</code> that provides implicit implementations for <code>java.io.File</code>.
 */
object Existence {

  /**
   * Enable <code>Existence</code> implementation for <code>java.io.File</code>
   *
   * @tparam FILE any subtype of <code>java.io.File</code>
   * @return <code>Existence[FILE]</code> that supports <code>java.io.File</code> in <code>exist</code> syntax
   */
  implicit def existenceOfFile[FILE <: java.io.File]: Existence[FILE] =
    new Existence[FILE] {
      def exists(file: FILE): Boolean = file.exists
    }
  
}