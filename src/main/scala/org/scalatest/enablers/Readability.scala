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
 * Supertrait for typeclasses that enable the <code>be</code> <code>readable</code> matcher syntax.
 *
 * <p>
 * A <code>Readability[T]</code> provides access to the "readable nature" of type <code>T</code> in such
 * a way that <code>be</code> <code>readable</code> matcher syntax can be used with type <code>T</code>. An <code>T</code>
 * can be any type for which the concept of being readable makes sense, such as <code>java.io.File</code>. ScalaTest provides
 * implicit implementation for <code>java.io.File</code>. You can enable the <code>be</code> <code>readable</code> matcher syntax on your own
 * type <code>U</code> by defining a <code>Readability[U]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides an implicit <code>Readability</code> instance for <code>java.io.File</code>
 * in the <code>Readability</code> companion object.
 * </p>
 */
trait Readability[-T] {

  /**
   * Determines whether the passed thing is readable, <em>i.e.</em>, the passed file is readable.
   */
  def isReadable(thing: T): Boolean
}

object Readability {

  /**
   * Enable readability nature for <code>java.io.File</code>
   */
  implicit def readabilityNatureOfFile[FILE <: java.io.File]: Readability[FILE] =
    new Readability[FILE] {
      def isReadable(file: FILE): Boolean = file.canRead
    }

}

