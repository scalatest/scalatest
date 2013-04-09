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
 * Exception thrown to indicate a test has been canceled.
 *
 * <p>
 * A <em>canceled test</em> is one that is unable to run because a needed dependency, such as
 * an external database, is missing.
 * </p>
 *
 * <p>
 * Canceled tests are ones that complete abruptly with a <code>TestCanceledException</code> after
 * starting.
 * </p>
 *
 * @author Travis Stevens
 */
class TestCanceledException(
  messageFun: StackDepthException => Option[String],
  cause: Option[Throwable],
  failedCodeStackDepthFun: StackDepthException => Int
) extends StackDepthException(messageFun, cause, failedCodeStackDepthFun) with ModifiableMessage[TestCanceledException] {


  /**
   * Constructs a <code>TestCanceledException</code> with pre-determined <code>message</code> and <code>failedCodeStackDepth</code>. (This was
   * the primary constructor form prior to ScalaTest 1.5.)
   *
   * @param message an optional detail message for this <code>StackDepthException</code>.
   * @param cause an optional cause, the <code>Throwable</code> that caused this <code>StackDepthException</code> to be thrown.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if either <code>message</code> of <code>cause</code> is <code>null</code>, or <code>Some(null)</code>.
   */
  def this(message: Option[String], cause: Option[Throwable], failedCodeStackDepth: Int) =
    this(
      StackDepthException.toExceptionFunction(message),
      cause,
      e => failedCodeStackDepth
    )

  /**
   * Create a <code>TestCanceledException</code> with specified stack depth and no detail message or cause.
   *
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   */
  def this(failedCodeStackDepth: Int) = this(None, None, failedCodeStackDepth)

  /**
   * Create a <code>TestCanceledException</code> with a specified stack depth and detail message.
   *
   * @param message A detail message for this <code>TestCanceledException</code>.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
  def this(message: String, failedCodeStackDepth: Int) =
    this(
    {
      Option(message)
    },
    None,
    failedCodeStackDepth
    )

  /**
   * Create a <code>TestCanceledException</code> with the specified stack depth and cause.  The
   * <code>message</code> field of this exception object will be initialized to
   * <code>if (cause.getMessage == null) "" else cause.getMessage</code>.
   *
   * @param cause the cause, the <code>Throwable</code> that caused this <code>TestCanceledException</code> to be thrown.
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
    failedCodeStackDepth
    )

  /**
   * Create a <code>TestCancelledException</code> with the specified stack depth, detail
   * message, and cause.
   *
   * <p>Note that the detail message associated with cause is
   * <em>not</em> automatically incorporated in this throwable's detail
   * message.
   *
   * @param message A detail message for this <code>TestCanceledException</code>.
   * @param cause the cause, the <code>Throwable</code> that caused this <code>TestCanceledException</code> to be thrown.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if either <code>message</code> or <code>cause</code> is <code>null</code>.
   */
  def this(message: String, cause: Throwable, failedCodeStackDepth: Int) =
    this(
    {
      if (message == null) throw new NullPointerException("message was null")
      Some(message)
    }, {
      if (cause == null) throw new NullPointerException("cause was null")
      Some(cause)
    },
    failedCodeStackDepth
    )

  /**
   * Returns an exception of class <code>TestCanceledException</code> with <code>failedExceptionStackDepth</code> set to 0 and
   * all frames above this stack depth severed off. This can be useful when working with tools (such as IDEs) that do not
   * directly support ScalaTest. (Tools that directly support ScalaTest can use the stack depth information delivered
   * in the StackDepth exceptions.)
   */
  def severedAtStackDepth: TestCanceledException = {
    val truncated = getStackTrace.drop(failedCodeStackDepth)
    val e = new TestCanceledException(message, cause, 0)
    e.setStackTrace(truncated)
    e
  }

  /**
   * Returns an instance of this exception's class, identical to this exception,
   * except with the detail message option string replaced with the result of passing
   * the current detail message to the passed function, <code>fun</code>.
   *
   * @param fun A function that, given the current optional detail message, will produce
   *            the modified optional detail message for the result instance of <code>TestCanceledException</code>.
   */
  def modifyMessage(fun: Option[String] => Option[String]): TestCanceledException = {
    val mod = new TestCanceledException(fun(message), cause, failedCodeStackDepth)
    mod.setStackTrace(getStackTrace)
    mod
  }

  /**
   * Indicates whether this object can be equal to the passed object.
   */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[TestCanceledException]

  /**
   * Indicates whether this object is equal to the passed object. If the passed object is
   * a <code>TestCanceledException</code>, equality requires equal <code>message</code>,
   * <code>cause</code>, and <code>failedCodeStackDepth</code> fields, as well as equal
   * return values of <code>getStackTrace</code>.
   */
  override def equals(other: Any): Boolean =
    other match {
      case that: TestCanceledException => super.equals(that)
      case _ => false
    }

  /**
   * Returns a hash code value for this object.
   */
  // Don't need to change it. Implementing it only so as to not freak out people who know
  // that if you override equals you must override hashCode.
  override def hashCode: Int = super.hashCode
}
