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
 * Supertrait for typeclasses that enable the <code>be defined</code> matcher syntax.
 *
 * <p>
 * A <code>Definition[T]</code> provides access to the "definition nature" of type <code>S</code> in such
 * a way that <code>be defined</code> matcher syntax can be used with type <code>T</code>. A <code>T</code>
 * can be any type for which the concept of being defined makes sense, such as <code>scala.Option</code>. ScalaTest provides
 * implicit implementation for <code>scala.Option</code>. You can enable the <code>be defined</code> matcher syntax on your own
 * type <code>U</code> by defining a <code>Definition[U]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides an implicit <code>Definition</code> instance for <code>scala.Option</code>
 * in the <code>Definition</code> companion object.
 * </p>
 */
trait Definition[-T] {

  /**
   * Determines whether the passed is defined, <em>i.e.</em>, the passed in <code>scala.Option</code> is defined.
   *
   * @param thing the thing to check for definition
   * @return <code>true</code> if passed thing is defined, <code>false</code> otherwise
   */
  def isDefined(thing: T): Boolean
}

/**
 * Companion object for <code>Definition</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>scala.Option</code></li>
 * <li>arbitary object with a <code>isDefined()</code> method that returns <code>Boolean</code></li>
 * <li>arbitary object with a parameterless <code>isDefined</code> method that returns <code>Boolean</code></li>
 * </ul>
 */
object Definition {

  /**
   * Provides <code>Definition</code> implementation for <code>scala.Option</code>
   *
   * @tparam E the type of the element in the <code>Option</code>
   * @tparam OPT any subtype of <code>Option</code>
   * @return <code>Definition[OPT[E]]</code> that supports <code>Option</code> in <code>be defined</code> syntax
   */
  implicit def definitionOfOption[E, OPT[e] <: scala.Option[e]]: Definition[OPT[E]] =
    new Definition[OPT[E]] {
      def isDefined(option: OPT[E]): Boolean = option.isDefined
    }
  
  /**
   * Provides <code>Definition</code> implementation for any arbitrary object with a <code>isDefined()</code> method that returns <code>Boolean</code>
   *
   * @tparam T any type that has a <code>isDefined()</code> method that returns <code>Boolean</code>
   * @return <code>Definition[T]</code> that supports <code>T</code> in <code>be defined</code> syntax
   */
  implicit def definitionOfAnyRefWithIsDefinedMethod[T <: AnyRef { def isDefined(): Boolean}]: Definition[T] = 
    new Definition[T] {
      def isDefined(obj: T): Boolean = obj.isDefined
    }

  /**
   * Provides <code>Definition</code> implementation for any arbitrary object with a <code>isDefined</code> method that returns <code>Boolean</code>
   *
   * @tparam T any type that has a parameterless <code>isDefined</code> method that returns <code>Boolean</code>
   * @return <code>Definition[T]</code> that supports <code>T</code> in <code>be defined</code> syntax
   */
  implicit def definitionOfAnyRefWithParameterlessIsDefinedMethod[T <: AnyRef { def isDefined: Boolean}]: Definition[T] = 
    new Definition[T] {
      def isDefined(obj: T): Boolean = obj.isDefined
    }
}

