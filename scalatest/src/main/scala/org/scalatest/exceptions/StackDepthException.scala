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

import org.scalactic.Requirements._
import org.scalactic.exceptions.NullArgumentException
import org.scalactic.source
import StackDepthExceptionHelper.getStackDepthFun
import org.scalactic.ArrayHelper.deep

/**
 * Exception class that encapsulates information about the stack depth at which the line of code that failed resides,
 * so that information can be presented to the user that makes it quick to find the failing line of code. (In other
 * words, the user need not scan through the stack trace to find the correct filename and line number of the problem code.)
 * Having a stack depth is more useful in a testing environment in which test failures are implemented as
 * thrown exceptions, as is the case in ScalaTest's built-in suite traits.
 *
 * @param messageFun a function that produces an optional detail message for this <code>StackDepthException</code>.
 * @param cause an optional cause, the <code>Throwable</code> that caused this <code>StackDepthException</code> to be thrown.
 * @param posOrStackDepthFun either a source position or a function that produces the depth in the stack trace of this exception at which the line of test code that failed resides.
 *
 * @throws NullArgumentException if either <code>messageFun</code>, <code>cause</code> or <code>failedCodeStackDepthFun</code> is <code>null</code>, or <code>Some(null)</code>.
 *
 * @author Bill Venners
 */
abstract class StackDepthException(
  val messageFun: StackDepthException => Option[String],
  val cause: Option[Throwable],
  posOrStackDepthFun: Either[source.Position, StackDepthException => Int]
) extends RuntimeException(if (cause != null && cause.isDefined) cause.get else null) with StackDepth {

  requireNonNull(messageFun, cause, posOrStackDepthFun)

  cause match {
    case Some(null) => throw new NullArgumentException("cause was a Some(null)")
    case _ =>
  }

  posOrStackDepthFun match {
    case Right(null) => throw new NullArgumentException("posOrStackDepthFun was Right(null)")
    case Left(null) => throw new NullArgumentException("posOrStackDepthFun was Left(null)")
    case _ =>
  }

  val position: Option[source.Position] = posOrStackDepthFun.left.toOption

  /**
    * Constructs a <code>StackDepthException</code> with an optional pre-determined <code>message</code>, optional cause, and
    * a source position.
    *
    * @param message an optional detail message for this <code>StackDepthException</code>.
    * @param cause an optional cause, the <code>Throwable</code> that caused this <code>StackDepthException</code> to be thrown.
    * @param position a source position.
    *.
    */
  def this(message: Option[String], cause: Option[Throwable], position: source.Position) =
    this(
      message match {
        case null => throw new NullArgumentException("message was null")
        case Some(null) => throw new NullArgumentException("message was a Some(null)")
        case _ => (e: StackDepthException) => message
      },
      cause,
      Left(position)
    )

  /**
    * Constructs a <code>StackDepthException</code> with message function, optional cause, and
    * a <code>failedCodeStackDepth</code> function.
    *
    * @param messageFun a function that produces an optional detail message for this <code>StackDepthException</code>.
    * @param cause an optional cause, the <code>Throwable</code> that caused this <code>StackDepthException</code> to be thrown.
    * @param failedCodeStackDepthFun a function that return the depth in the stack trace of this exception at which the line of test code that failed resides.
    *.
    */
  def this(messageFun: StackDepthException => Option[String], cause: Option[Throwable], failedCodeStackDepthFun: StackDepthException => Int) =
    this(
      messageFun,
      cause,
      failedCodeStackDepthFun match {
        case null => throw new NullArgumentException("failedCodeStackDepthFun was null")
        case _ => Right(failedCodeStackDepthFun)
      }
    )

  /**
   * Constructs a <code>StackDepthException</code> with an optional pre-determined <code>message</code>, optional cause, and
   * a <code>failedCodeStackDepth</code> function.
   *
   * @param message an optional detail message for this <code>StackDepthException</code>.
   * @param cause an optional cause, the <code>Throwable</code> that caused this <code>StackDepthException</code> to be thrown.
   * @param failedCodeStackDepthFun a function that return the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullArgumentException if either <code>message</code> or <code>cause</code> is <code>null</code> or <code>Some(null)</code>, or <code>failedCodeStackDepthFun</code> is <code>null</code>.
   */
  def this(message: Option[String], cause: Option[Throwable], failedCodeStackDepthFun: StackDepthException => Int) =
    this(
      message match {
        case null => throw new NullArgumentException("message was null")
        case Some(null) => throw new NullArgumentException("message was a Some(null)")
        case _ => (e: StackDepthException) => message
      },
      cause,
      failedCodeStackDepthFun match {
        case null => throw new NullArgumentException("failedCodeStackDepthFun was null")
        case _ => Right(failedCodeStackDepthFun)
      }
    )

  /**
   * Constructs a <code>StackDepthException</code> with an optional pre-determined <code>message</code>,
   * optional <code>cause</code>, and and <code>failedCodeStackDepth</code>. (This was
   * the primary constructor form prior to ScalaTest 1.5.)
   *
   * @param message an optional detail message for this <code>StackDepthException</code>.
   * @param cause an optional cause, the <code>Throwable</code> that caused this <code>StackDepthException</code> to be thrown.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullArgumentException if either <code>message</code> of <code>cause</code> is <code>null</code>, or <code>Some(null)</code>.
   */
  def this(message: Option[String], cause: Option[Throwable], failedCodeStackDepth: Int) =
    this(
      message match {
        case null => throw new NullArgumentException("message was null")
        case Some(null) => throw new NullArgumentException("message was a Some(null)")
        case _ => (e: StackDepthException) => message
      },
      cause,
      Right((e: StackDepthException) => failedCodeStackDepth)
    )

  /**
   * An optional detail message for this <code>StackDepth</code> exception.
   *
   * <p>
   * One reason this is lazy is to delay any searching of the stack trace until it is actually needed. It will
   * usually be needed, but not always. For example, exceptions thrown during a shrink phase of a failed property
   * will often be <code>StackDepthException</code>s, but whose <code>message</code> will never be used. Another related reason is to remove the need
   * to create a different exception before creating this one just for the purpose of searching through its stack
   * trace for the proper stack depth. Still one more reason is to allow the message to contain information about the
   * stack depth, such as the failed file name and line number.
   * </p>
   */
  lazy val message: Option[String] = messageFun(this)

  lazy val failedCodeFilePathname: Option[String] = position.map(_.filePathname)

  /**
   * The depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * <p>
   * One reason this is lazy is to delay any searching of the stack trace until it is actually needed. It will
   * usually be needed, but not always. For example, exceptions thrown during a shrink phase of a failed property
   * will often be <code>StackDepthException</code>s, but whose <code>failedCodeStackDepth</code> will never be used. Another reason is to remove the need
   * to create a different exception before creating this one just for the purpose of searching through its stack
   * trace for the proper stack depth. Still one more reason is to allow the message to contain information about the
   * stack depth, such as the failed file name and line number.
   * </p>
   */
  lazy val failedCodeStackDepth: Int = {
    val stackDepthFun =
      posOrStackDepthFun match {
        case Left(pos) => getStackDepthFun(pos)
        case Right(sdf) => sdf
      }
    stackDepthFun(this)
  }

  /**
   * Returns the detail message string of this <code>StackDepthException</code>.
   *
   * @return the detail message string of this <code>StackDepthException</code> instance (which may be <code>null</code>).
   */
  override def getMessage: String = message.orNull

  /*
  * Throws <code>IllegalStateException</code>, because <code>StackDepthException</code>s are
  * always initialized with a cause passed to the constructor of superclass <code>RuntimeException</code>.
  */
  override final def initCause(throwable: Throwable): Throwable = { throw new IllegalStateException }

  /**
   * Indicates whether this object can be equal to the passed object.
   */
  def canEqual(other: Any): Boolean = other.isInstanceOf[StackDepthException]

  /**
   * Indicates whether this object is equal to the passed object. If the passed object is
   * a <code>StackDepthException</code>, equality requires equal <code>message</code>,
   * <code>cause</code>, and <code>failedCodeStackDepth</code> fields, as well as equal
   * return values of <code>getStackTrace</code>.
   */
  override def equals(other: Any): Boolean =
    other match {
      case that: StackDepthException =>
        (that canEqual this) &&
        message == that.message &&
        cause == that.cause &&
        failedCodeStackDepth == that.failedCodeStackDepth &&
        deep(getStackTrace) == deep(that.getStackTrace)
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

private[scalatest] object StackDepthException {

  /**
   * If message or message contents are null, throw a null exception, otherwise
   * create a function that returns the option.
   */
  def toExceptionFunction(message: Option[String]): StackDepthException => Option[String] = {
    message match {
        case null => throw new NullArgumentException("message was null")
        case Some(null) => throw new NullArgumentException("message was a Some(null)")
        case _ => { e => message }
    }
  }
}

