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
package org.scalatest.exceptions

import org.scalactic.Requirements
import org.scalactic.exceptions.NullArgumentException

/**
 * Exception that indicates something was attempted in test code that is not allowed.
 * For example, in a <a href="../FeatureSpec.html"><code>FeatureSpec</code></a>, it is not allowed to nest a <code>feature</code>
 * clause inside another <code>feature</code> clause. If this is attempted, the construction
 * of that suite will fail with a <code>NotAllowedException</code>.
 *
 * @param message a string that explains the problem
 * @param cause an optional cause
 * @param failedCodeStackDepthFun a function that return the depth in the stack trace of this exception at which the line of code that attempted
 *    something not allowed resides.
 *
 * @throws NullArgumentException if either <code>message</code> or <code>failedCodeStackDepthFun</code> is <code>null</code>
 *
 * @author Bill Venners
 */
class NotAllowedException(message: String, cause: Option[Throwable], failedCodeStackDepthFun: StackDepthException => Int)
    extends StackDepthException(Some(message), cause, failedCodeStackDepthFun) with Requirements {

  requireNonNull(message, failedCodeStackDepthFun)

  /**
   * Constructs a <code>NotAllowedException</code> with pre-determined <code>message</code> and
   * <code>failedCodeStackDepth</code>. (This was the primary constructor form prior to ScalaTest 1.5.)
   *
   * @param message the exception's detail message
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of code that attempted
   *    something not allowed resides.
   *
   * @throws NullArgumentException if <code>message</code> is <code>null</code>
   */
  def this(message: String, failedCodeStackDepth: Int) = this(message, None, e => failedCodeStackDepth)

  /**
   * Construct a <code>NotAllowedException</code> with pre-determined <code>message</code> and
   * a function that returns the depth in the stack trace of this exception at which the line of code that attempted.
   *
   * @param message the exception's detail message
   * @param failedCodeStackDepthFun a function that return the depth in the stack trace of this exception at which the line of code that attempted
   *
   * @throws NullArgumentException if <code>message</code> is <code>null</code>
   */
  def this(message: String, failedCodeStackDepthFun: StackDepthException => Int) = this(message, None, failedCodeStackDepthFun)

  /**
   * Returns an exception of class <code>NotAllowedException</code> with <code>failedExceptionStackDepth</code> set to 0 and 
   * all frames above this stack depth severed off. This can be useful when working with tools (such as IDEs) that do not
   * directly support ScalaTest. (Tools that directly support ScalaTest can use the stack depth information delivered
   * in the StackDepth exceptions.)
   */
  def severedAtStackDepth: NotAllowedException = {
    val truncated = getStackTrace.drop(failedCodeStackDepth)
    val e = new NotAllowedException(message, 0)
    e.setStackTrace(truncated)
    e
  }

  /**
   * Indicates whether this object can be equal to the passed object.
   */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[NotAllowedException]

  /**
   * Indicates whether this object is equal to the passed object. If the passed object is
   * a <code>NotAllowedException</code>, equality requires equal <code>message</code>,
   * <code>cause</code>, and <code>failedCodeStackDepth</code> fields, as well as equal
   * return values of <code>getStackTrace</code>.
   */
  override def equals(other: Any): Boolean =
    other match {
      case that: NotAllowedException => super.equals(that)
      case _ => false
    }

  /**
   * Returns a hash code value for this object.
   */
  // Don't need to change it. Implementing it only so as to not freak out people who know
  // that if you override equals you must override hashCode.
  override def hashCode: Int = super.hashCode
}
