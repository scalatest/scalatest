/*
 * Copyright 2001-2009 Artima, Inc.
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
package org.scalatest.mock

import org.jmock.Expectations
import org.hamcrest.Matcher

/**
 * Subclass of <code>org.jmock.Expectations</code> that provides <code>withArg</code>
 * alternatives to the <code>with</code> methods defined in its superclass.
 *
 * <p>
 * <code>JMockCycle</code>'s <code>expecting</code> method of passes an instance of this class
 * to the function passed into <code>expectations</code>. Because <code>JMockExpectations</code>
 * extends <code>org.jmock.Expectations</code>, all of the <code>Expectations</code> methods are
 * available to be invoked on instances of this class, in addition to
 * several overloaded <code>withArg</code> methods defined in this class. These <code>withArg</code> methods simply
 * invoke corresponding <code>with</code> methods on <code>this</code>. Because <code>with</code> is
 * a keyword in Scala, to invoke these directly you must surround them in back ticks, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * oneOf (mockCollaborator).documentAdded(`with`("Document"))
 * </pre>
 *
 * <p>
 * By importing the members of the <code>JMockExpectations</code> object passed to
 * a <code>JMockCycle</code>'s <code>executing</code> method, you can
 * instead call <code>withArg</code> with no back ticks needed:
 * </p>
 *
 * <pre class="stHighlight">
 * oneOf (mockCollaborator).documentAdded(withArg("Document"))
 * </pre>
 *
 * @author Bill Venners
 */
final class JMockExpectations extends Expectations {

  /**
   * Invokes <code>with</code> on this instance, passing in the passed value.
   */
  def withArg[T](value: T): T = `with`(value)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed value.
   */
  def withArg(value: Int): Int = `with`(value)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed value.
   */
  def withArg(value: Short): Short = `with`(value)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed value.
   */
  def withArg(value: Byte): Byte = `with`(value)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed value.
   */
  def withArg(value: Long): Long = `with`(value)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed value.
   */
  def withArg(value: Boolean): Boolean = `with`(value)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed value.
   */
  def withArg(value: Float): Float = `with`(value)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed value.
   */
  def withArg(value: Double): Double = `with`(value)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed value.
   */
  def withArg(value: Char): Char = `with`(value)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed matcher.
   */
  def withArg[T](matcher: Matcher[T]): T = `with`(matcher)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed matcher.
   */
  def withArg(matcher: Matcher[Int]): Int = `with`(matcher)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed matcher.
   */
  def withArg(matcher: Matcher[Short]): Short = `with`(matcher)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed matcher.
   */
  def withArg(matcher: Matcher[Byte]): Byte = `with`(matcher)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed matcher.
   */
  def withArg(matcher: Matcher[Long]): Long = `with`(matcher)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed matcher.
   */
  def withArg(matcher: Matcher[Boolean]): Boolean = `with`(matcher)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed matcher.
   */
  def withArg(matcher: Matcher[Float]): Float = `with`(matcher)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed matcher.
   */
  def withArg(matcher: Matcher[Double]): Double = `with`(matcher)

  /**
   * Invokes <code>with</code> on this instance, passing in the passed matcher.
   */
  def withArg(matcher: Matcher[Char]): Char = `with`(matcher)
}
