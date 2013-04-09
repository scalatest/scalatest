/*
 * Copyright 2001-2008 Artima, Inc.
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

/**
 * Exception that indicates an action that is only allowed during a suite's test registration phase,
 * such as registering a test to run or ignore, was attempted after registration had already closed.
 *
 * <p>
 * In suites that register tests as functions, such as <code>FunSuite</code> and <code>FunSpec</code>, tests
 * are normally registered during construction. Although it is not the usual approach, tests can also
 * be registered after construction by invoking methods that register tests on the already constructed suite so
 * long as <code>run</code> has not been invoked on that suite.
 * As soon as <code>run</code> is invoked for the first time, registration of tests is "closed," meaning
 * that any further attempts to register a test will fail (and result in an instance of this exception class being thrown). This
 * can happen, for example, if an attempt is made to nest tests, such as in a <code>FunSuite</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * test("this test is fine") {
 *   test("but this nested test is not allowed") {
 *   }
 * }
 * </pre>
 *
 * <p>
 * This exception encapsulates information about the stack depth at which the line of code that made this attempt resides,
 * so that information can be presented to the user that makes it quick to find the problem line of code. (In other words,
 * the user need not scan through the stack trace to find the correct filename and line number of the offending code.)
 * </p>
 *
 * @param message the exception's detail message
 * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of code that attempted
 *   to register the test after registration had been closed.
 *
 * @throws NullPointerException if either <code>message</code> or <code>failedCodeStackDepthFun</code> is <code>null</code>
 *
 * @author Bill Venners
 */
class TestRegistrationClosedException(message: String, failedCodeStackDepthFun: StackDepthException => Int)
    extends StackDepthException(Some(message), None, failedCodeStackDepthFun) {

  if (message == null) throw new NullPointerException("message was null")
  if (failedCodeStackDepthFun == null) throw new NullPointerException("failedCodeStackDepthFun was null")

  /**
   * Constructs a <code>TestRegistrationClosedException</code> with a <code>message</code> and a pre-determined 
   * and <code>failedCodeStackDepth</code>. (This was the primary constructor form prior to ScalaTest 1.5.)
   *
   * @param message the exception's detail message
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if <code>message</code> is <code>null</code>
   */
  def this(message: String, failedCodeStackDepth: Int) = this(message, e => failedCodeStackDepth)

  /**
   * Returns an exception of class <code>TestRegistrationClosedException</code> with <code>failedExceptionStackDepth</code> set to 0 and 
   * all frames above this stack depth severed off. This can be useful when working with tools (such as IDEs) that do not
   * directly support ScalaTest. (Tools that directly support ScalaTest can use the stack depth information delivered
   * in the StackDepth exceptions.)
   */
  def severedAtStackDepth: TestRegistrationClosedException = {
    val truncated = getStackTrace.drop(failedCodeStackDepth)
    val e = new TestRegistrationClosedException(message, 0)
    e.setStackTrace(truncated)
    e
  }

  /**
   * Indicates whether this object can be equal to the passed object.
   */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[TestRegistrationClosedException]

  /**
   * Indicates whether this object is equal to the passed object. If the passed object is
   * a <code>TestRegistrationClosedException</code>, equality requires equal <code>message</code>,
   * <code>cause</code>, and <code>failedCodeStackDepth</code> fields, as well as equal
   * return values of <code>getStackTrace</code>.
   */
  override def equals(other: Any): Boolean =
    other match {
      case that: TestRegistrationClosedException => super.equals(that)
      case _ => false
    }

  /**
   * Returns a hash code value for this object.
   */
  // Don't need to change it. Implementing it only so as to not freak out people who know
  // that if you override equals you must override hashCode.
  override def hashCode: Int = super.hashCode
}

// I pass in a message here so different situations can be described better in the
// error message, such as an it inside an it, an ignore inside an it, a describe inside an it, etc.
