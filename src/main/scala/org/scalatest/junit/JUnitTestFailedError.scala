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
package org.scalatest.junit

import _root_.junit.framework.AssertionFailedError
import org.scalatest.exceptions.{PayloadField, ModifiablePayload, StackDepth, ModifiableMessage}

/**
 * Exception that indicates a test failed.
 *
 * <p>
 * The purpose of this exception is to encapsulate the same stack depth information provided by
 * <a href="../TestFailedException.html"><code>TestFailedException</code></a>, which is used
 * when running with ScalaTest, but be reported as
 * a failure not an error when running with JUnit.
 * The stack depth information indicates which line of test code failed, so that when running
 * with ScalaTest information can be presented to
 * the user that makes it quick to find the failing line of test code. (In other words, when
 * running with ScalaTest the user need not scan through the stack trace to find the correct filename
 * and line number of the failing test.)
 * </p>
 *
 * <p>
 * JUnit distinguishes between <em>failures</em> and <em>errors</em>.
 * If a test fails because of a failed assertion, that is considered a <em>failure</em> in JUnit. If a test
 * fails for any other reason, either the test code or the application being tested threw an unexpected
 * exception, that is considered an <em>error</em> in JUnit. This class differs from
 * <a href="../TestFailedException.html"><code>TestFailedException</code></a> in that it extends
 * <code>junit.framework.AssertionFailedError</code>. Instances of this class are thrown by the
 * assertions provided by <a href="AssertionsForJUnit.html"><code>AssertionsForJUnit</code></a>, and matcher
 * expressions provided by <a href="ShouldMatchersForJUnit.html"><code>ShouldMatchersForJUnit</code></a>, and
 * <a href="MustMatchersForJUnit.html"><code>MustMatchersForJUnit</code></a>.
 * </p>
 *
 * <p>
 * The way JUnit 3 (JUnit 3.8 and earlier releases) decided whether an exception represented a failure or error
 * is that only thrown <code>junit.framework.AssertionFailedError</code>s were considered failures. Any other
 * exception type was considered an error. The exception type thrown by the JUnit 3 assertion methods declared
 * in <code>junit.framework.Assert</code> (such as <code>assertEquals</code>, <code>assertTrue</code>,
 * and <code>fail</code>) was, therefore, <code>AssertionFailedError</code>. In JUnit 4, <code>AssertionFailedError</code>
 * was made to extend <code>java.lang.AssertionError</code>, and the distinction between failures and errors
 * was essentially dropped. However, some tools that integrate with JUnit carry on this distinction, so even
 * if you are using JUnit 4 you may want to use the "<code>ForJUnit</code>" of ScalaTest assertions and matchers.
 * </p>
 *
 * @param message an optional detail message for this <code>TestFailedException</code>.
 * @param cause an optional cause, the <code>Throwable</code> that caused this <code>TestFailedException</code> to be thrown.
 * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
 *
 * @throws NullPointerException if either <code>message</code> or <code>cause</code> is <code>null</code>, or <code>Some(null)</code>.
 *
 * @author Bill Venners
 */
class JUnitTestFailedError(val message: Option[String], val cause: Option[Throwable], val failedCodeStackDepth: Int, val payload: Option[Any])
    extends AssertionFailedError(if (message.isDefined) message.get else "") with StackDepth with ModifiableMessage[JUnitTestFailedError]  with PayloadField with ModifiablePayload[JUnitTestFailedError] {

  /**
   * This constructor has been deprecated and will be removed in a future version of ScalaTest. Please
   * use the primary constructor instead.
   */
  @deprecated("Please use the primary constructor instead.")
  def this(message: Option[String], cause: Option[Throwable], failedCodeStackDepth: Int) =
    this(message, cause, failedCodeStackDepth, None)

  // TODO: CHange above to a message.getOrElse(""), and same in other exceptions most likely
  // TODO: Possibly change stack depth to stackDepthFun like in TFE, consider messageFun like in TDE

  if (message == null) throw new NullPointerException("message was null")
  message match {
    case Some(null) => throw new NullPointerException("message was a Some(null)")
    case _ =>
  }

  if (cause == null) throw new NullPointerException("cause was null")
  cause match {
    case Some(null) => throw new NullPointerException("cause was a Some(null)")
    case _ =>
  }

  if (cause.isDefined)
    super.initCause(cause.get)

  /*
  * Throws <code>IllegalStateException</code>, because <code>StackDepthException</code>s are
  * always initialized with a cause passed to the constructor of superclass <code>
  */
  override final def initCause(throwable: Throwable): Throwable = { throw new IllegalStateException }

  /**
   * Create a <code>JUnitTestFailedError</code> with specified stack depth and no detail message or cause.
   *
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   */
  def this(failedCodeStackDepth: Int) = this(None, None, failedCodeStackDepth, None)

  /**
   * Create a <code>JUnitTestFailedError</code> with a specified stack depth and detail message.
   *
   * @param message A detail message for this <code>JUnitTestFailedError</code>.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
  def this(message: String, failedCodeStackDepth: Int) =
    this(
      {
        if (message == null) throw new NullPointerException("message was null")
        Some(message)
      },
      None,
      failedCodeStackDepth,
      None
    )

  /**
   * Create a <code>JUnitTestFailedError</code> with the specified stack depth and cause.  The
   * <code>message</code> field of this exception object will be initialized to
   * <code>if (cause.getMessage == null) "" else cause.getMessage</code>.
   *
   * @param cause the cause, the <code>Throwable</code> that caused this <code>JUnitTestFailedError</code> to be thrown.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if <code>cause</code> is <code>null</code>.
   */
  def this(cause: Throwable, failedCodeStackDepth: Int) =
    this(
      {
        if (cause == null) throw new NullPointerException("cause was null")
        Some(if (cause.getMessage == null) "" else cause.getMessage)
      },
      Some(cause),
      failedCodeStackDepth,
      None
    )

  /**
   * Create a <code>JUnitTestFailedError</code> with the specified stack depth, detail
   * message, and cause.
   *
   * <p>Note that the detail message associated with cause is
   * <em>not</em> automatically incorporated in this throwable's detail
   * message.
   *
   * @param message A detail message for this <code>JUnitTestFailedError</code>.
   * @param cause the cause, the <code>Throwable</code> that caused this <code>JUnitTestFailedError</code> to be thrown.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if either <code>message</code> or <code>cause</code> is <code>null</code>.
   */
  def this(message: String, cause: Throwable, failedCodeStackDepth: Int) =
    this(
      {
        if (message == null) throw new NullPointerException("message was null")
        Some(message)
      },
      {
        if (cause == null) throw new NullPointerException("cause was null")
        Some(cause)
      },
      failedCodeStackDepth,
      None
    )

  /**
   * Returns an exception of class <code>JUnitTestFailedError</code> with <code>failedExceptionStackDepth</code> set to 0 and 
   * all frames above this stack depth severed off. This can be useful when working with tools (such as IDEs) that do not
   * directly support ScalaTest. (Tools that directly support ScalaTest can use the stack depth information delivered
   * in the StackDepth exceptions.)
   */
  def severedAtStackDepth: JUnitTestFailedError = {
    val truncated = getStackTrace.drop(failedCodeStackDepth)
    val e = new JUnitTestFailedError(message, cause, 0, payload)
    e.setStackTrace(truncated)
    e
  }

  /**
   * Returns an instance of this exception's class, identical to this exception,
   * except with the detail message option string replaced with the result of passing
   * the current detail message to the passed function, <code>fun</code>.
   *
   * @param fun A function that, given the current optional detail message, will produce
   * the modified optional detail message for the result instance of <code>TestFailedException</code>.
   */
  def modifyMessage(fun: Option[String] => Option[String]): JUnitTestFailedError = {
    val mod = new JUnitTestFailedError(fun(message), cause, failedCodeStackDepth, payload)
    mod.setStackTrace(getStackTrace)
    mod
  }

  /**
   * Returns an instance of this exception's class, identical to this exception,
   * except with the payload option replaced with the result of passing
   * the current payload option to the passed function, <code>fun</code>.
   *
   * @param fun A function that, given the current optional payload, will produce
   * the modified optional payload for the result instance of <code>TestFailedException</code>.
   */
  def modifyPayload(fun: Option[Any] => Option[Any]): JUnitTestFailedError = {
    val currentPayload = payload
    val mod = new JUnitTestFailedError(message, cause, failedCodeStackDepth, fun(currentPayload))
    mod.setStackTrace(getStackTrace)
    mod
  }

  /**
   * Indicates whether this object can be equal to the passed object.
   */
  def canEqual(other: Any): Boolean = other.isInstanceOf[JUnitTestFailedError]

  /**
   * Indicates whether this object is equal to the passed object. If the passed object is
   * a <code>JUnitTestFailedError</code>, equality requires equal <code>message</code>,
   * <code>cause</code>, and <code>failedCodeStackDepth</code> fields, as well as equal
   * return values of <code>getStackTrace</code>.
   */
  override def equals(other: Any): Boolean =
    other match {
      case that: JUnitTestFailedError => 
        (that canEqual this) &&
        message == that.message &&
        cause == that.cause &&
        failedCodeStackDepth == that.failedCodeStackDepth &&
        getStackTrace.deep == that.getStackTrace.deep
      case _ => false
    }

  /**
   * Returns a hash code value for this object.
   */
  override def hashCode: Int =
    41 * (
      41 * (
        41 * (
          41 + message.hashCode
        ) + cause.hashCode
      ) + failedCodeStackDepth.hashCode
    ) + getStackTrace.hashCode
}
