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
package org.scalatest.exceptions

import org.scalatest.Resources
import org.scalactic.Requirements._
import org.scalactic.source
import StackDepthExceptionHelper.posOrElseStackDepthFun

/**
 * Exception that indicates an attempt was made to register a test that had the same name as a test
 * already registered in the same suite. The purpose of this exception is to encapsulate information about
 * the stack depth at which the line of code that made this attempt resides, so that information can be presented to
 * the user that makes it quick to find the problem line of code. (In other words, the user need not scan through the
 * stack trace to find the correct filename and line number of the offending code.)
 *
 * @param testName the test name that was attempted to be registered twice
 * @param posOrStackDepthFun either a source position or a function that return the depth in the stack trace of this exception at which the line of code that attempted
 *   to register the test with the duplicate name resides.
 *
 * @throws NullArgumentException if <code>testName</code> is <code>null</code>
 *
 * @author Bill Venners
 */
class DuplicateTestNameException(
  testName: String,
  posOrStackDepthFun: Either[source.Position, StackDepthException => Int]
) extends StackDepthException(
  (_: StackDepthException) => Some(Resources.duplicateTestName(testName)),
  None,
  posOrStackDepthFun
) {
  
  requireNonNull(testName)

  /**
    * Constructs a <code>DuplicateTestNameException</code> with given message and source position
    *
    * @param message the error message
    * @param pos the source position
    */
  def this(
    message: String,
    pos: source.Position
  ) = this(message, Left(pos))

  /**
   * Constructs a <code>DuplicateTestNameException</code> with pre-determined <code>failedCodeStackDepth</code>. (This was
   * the primary constructor form prior to ScalaTest 1.5.)
   *
   * @param testName the test name found to be duplicate
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullArgumentException if <code>testName</code> is <code>null</code>
   */
  def this(testName: String, failedCodeStackDepth: Int) =
    this(testName, Right((e: StackDepthException) => failedCodeStackDepth))

  /**
    * Constructs a <code>DuplicateTestNameException</code> with given test name and stack depth function.
    *
    * @param testName the test name
    * @param failedCodeStackDepthFun the depth in the stack trace of this exception at which the line of test code that failed resides.
    */
  def this(testName: String, failedCodeStackDepthFun: StackDepthException => Int) =
    this(testName, Right(failedCodeStackDepthFun))

  /**
   * Returns an exception of class <code>DuplicateTestNameException</code> with <code>failedExceptionStackDepth</code> set to 0 and 
   * all frames above this stack depth severed off. This can be useful when working with tools (such as IDEs) that do not
   * directly support ScalaTest. (Tools that directly support ScalaTest can use the stack depth information delivered
   * in the StackDepth exceptions.)
   */
  def severedAtStackDepth: DuplicateTestNameException = {
    val truncated = getStackTrace.drop(failedCodeStackDepth)
    val e = new DuplicateTestNameException(testName, posOrStackDepthFun)
    e.setStackTrace(truncated)
    e
  }

  /**
   * Indicates whether this object can be equal to the passed object.
   */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[DuplicateTestNameException]

  /**
   * Indicates whether this object is equal to the passed object. If the passed object is
   * a <code>DuplicateTestNameException</code>, equality requires equal <code>message</code>,
   * <code>cause</code>, and <code>failedCodeStackDepth</code> fields, as well as equal
   * return values of <code>getStackTrace</code>.
   */
  override def equals(other: Any): Boolean =
    other match {
      case that: DuplicateTestNameException => super.equals(that)
      case _ => false
    }

  /**
   * Returns a hash code value for this object.
   */
  // Don't need to change it. Implementing it only so as to not freak out people who know
  // that if you override equals you must override hashCode.
  override def hashCode: Int = super.hashCode
}

