/*
 * Copyright 2001-2025 Artima, Inc.
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
 * Supertrait for <code>Messaging</code> typeclasses.
 *
 * <p>
 * Trait <code>Messaging</code> is a typeclass trait for objects that can be queried for message.
 * Objects of type T for which an implicit <code>Messaging[T]</code> is available can be used
 * with the <code>should have message</code> syntax.
 * You can enable the <code>have message</code> matcher syntax on your own
 * type <code>U</code> by defining a <code>Messaging[U]</code> for the type and making it available implicitly.
 * </p>
 *
 * <p>
 * ScalaTest provides an implicit <code>Messaging</code> instance for <code>java.lang.Throwable</code> and
 * arbitary object with <code>message()</code>, <code>message</code>, <code>getMessage()</code> or <code>getMessage</code>
 * method in the <code>Messaging</code> companion object.
 * </p>
 *
 * @author Bill Venners
 * @author Chee Seng
 */
trait Messaging[T] {

  /**
   * Returns the message of the passed object.
   *
   * @param obj object whose message to return
   * @return the message of the passed object
   */
  def messageOf(obj: T): String
}

/**
 * Companion object for <code>Messaging</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>java.lang.Throwable</code></li>
 * <li>arbitary object with a <code>message()</code> method that returns <code>String</code></li>
 * <li>arbitary object with a parameterless <code>message</code> method that returns <code>String</code></li>
 * <li>arbitary object with a <code>getMessage()</code> method that returns <code>String</code></li>
 * <li>arbitary object with a parameterless <code>getMessage</code> method that returns <code>String</code></li>
 * </ul>
 */
object Messaging {

  //DOTTY-ONLY import scala.reflect.Selectable.reflectiveSelectable

  // SKIP-DOTTY-START
  /**
   * Enable <code>Messaging</code> implementation for <code>java.lang.Throwable</code>
   *
   * @tparam EX any subtype of <code>java.lang.Throwable</code>
   * @return <code>Messaging[EX]</code> that supports <code>java.lang.Throwable</code> in <code>have message</code> syntax
   */
  implicit def messagingNatureOfThrowable[EX <: Throwable]: Messaging[EX] = 
    new Messaging[EX] {
      def messageOf(exception: EX): String = exception.getMessage()
    }

  import scala.language.reflectiveCalls
  // SKIP-DOTTY-END

  /**
   * Provides <code>Messaging</code> implementation for any arbitrary object with a <code>message()</code> method that returns <code>String</code>
   *
   * @tparam T any type that has a <code>message()</code> method that returns <code>String</code>
   * @return <code>Messaging[T]</code> that supports <code>T</code> in <code>have message</code> syntax
   */ 
  implicit def messagingNatureOfAnyRefWithMessageMethod[T <: AnyRef { def message(): String}]: Messaging[T] = 
    new Messaging[T] {
      def messageOf(obj: T): String = obj.message()
    }

  /**
   * Provides <code>Messaging</code> implementation for any arbitrary object with a parameterless <code>message</code> method that returns <code>String</code>
   *
   * @tparam T any type that has a parameterless <code>message</code> method that returns <code>String</code>
   * @return <code>Messaging[T]</code> that supports <code>T</code> in <code>have message</code> syntax
   */
  implicit def messagingNatureOfAnyRefWithParameterlessMessageMethod[T <: AnyRef { def message: String}]: Messaging[T] = 
    new Messaging[T] {
      def messageOf(obj: T): String = obj.message
    }

  /**
   * Provides <code>Messaging</code> implementation for any arbitrary object with a <code>getMessage()</code> method that returns <code>String</code>
   *
   * @tparam T any type that has a <code>getMessage()</code> method that returns <code>String</code>
   * @return <code>Messaging[T]</code> that supports <code>T</code> in <code>have message</code> syntax
   */
  implicit def messagingNatureOfAnyRefWithGetMessageMethod[T <: AnyRef { def getMessage(): String}]: Messaging[T] = 
    new Messaging[T] {
      def messageOf(obj: T): String = obj.getMessage()
    }

  /**
   * Provides <code>Messaging</code> implementation for any arbitrary object with a parameterless <code>getMessage</code> method that returns <code>String</code>
   *
   * @tparam T any type that has a parameterless <code>getMessage</code> method that returns <code>String</code>
   * @return <code>Messaging[T]</code> that supports <code>T</code> in <code>have message</code> syntax
   */
  implicit def messagingNatureOfAnyRefWithParameterlessGetMessageMethod[T <: AnyRef { def getMessage: String}]: Messaging[T] = 
    new Messaging[T] {
      def messageOf(obj: T): String = obj.getMessage
    }
}


