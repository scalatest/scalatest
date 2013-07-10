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
 * Supertrait for typeclasses that enable the <code>should</code> <code>exist</code> matcher syntax.
 *
 * <p>
 * A <code>Existence[S]</code> provides access to the "exist nature" of type <code>S</code> in such
 * a way that <code>be</code> <code>exist</code> matcher syntax can be used with type <code>S</code>. An <code>S</code>
 * can be any type for which the concept of existence makes sense, such as <code>java.io.File</code>. ScalaTest provides
 * implicit implementations for <code>java.io.File</code>. You can enable the <code>be</code> <code>exist</code> matcher syntax on your own
 * type <code>U</code> by defining a <code>Existence[U]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides an implicit <code>Sortable</code> instance for <code>scala.collection.Seq</code>
 * in the <code>Sortable</code> companion object.
 * </p>
 */
trait Existence[-S] {

  /**
   * Determines whether the passed in thing exists, <em>i.e.</em>, whether the passed in <code>java.io.File</code> exists.
   */
  def exists(thing: S): Boolean
}

object Existence {
  
  implicit def existenceOfFile[FILE <: java.io.File]: Existence[FILE] =
    new Existence[FILE] {
      def exists(file: FILE): Boolean = file.exists
    }
  
}