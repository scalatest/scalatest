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

import org.scalactic.source

/**
 * Trait that encapsulates the information required of an exception thrown by ScalaTest's assertions
 * and matchers, which includes a stack depth at which the failing line of test code resides.
 *
 * <p>
 * This trait exists so that it can be mixed into two exception superclasses, <a href="StackDepthException.html"><code>StackDepthException</code></a>,
 * from which extend several exceptions that do not depend on JUnit, and <a href="../junit/JUnitTestFailedError.html"><code>JUnitTestFailedError</code></a>, which
 * does depend on JUnit. The latter, which requires JUnit be in the classpath, ensures failed ScalaTest assertions are
 * reported as "failures," not "errors," by JUnit.
 * </p>
 */
trait StackDepth { this: Throwable =>

  /**
   * An optional detail message for this <code>StackDepth</code> exception.
   */
  def message: Option[String]

  /**
   * An optional cause, the <code>Throwable</code> that caused this <code>StackDepth</code> exception to be thrown.
   */
  def cause: Option[Throwable]

  /**
   * The depth in the stack trace of this exception at which the line of test code that failed resides.
   */
  def failedCodeStackDepth: Int

  /**
   * A string that provides the full pathname of the source file containing the line of code that failed, suitable
   * for presenting to a user.
   *
   * @return a string containing the full pathname of the source file containing the line of code that caused this exception
   */
  def failedCodeFilePathname: Option[String]

  /**
   * An optional source position describing the line of test code that caused this exception.
   */
  def position: Option[source.Position]

  /**
   * A string that provides the filename and line number of the line of code that failed, suitable
   * for presenting to a user of the failing line.  It calls <code>failedCodeFileName</code> and
   * <code>failedCodeLineNumber</code> to get the failing filename and line number.
   *
   * <p>
   * <code>failedCodeFileName</code> and <code>failedCodeLineNumber</code> will fall back to exception stack trace
   * when <code>Position</code> is not avaiable, this is the reason it is a <code>def</code> instead of a <code>val</code>,
   * because exceptions are mutable: their stack trace can be changed after the exception is created. This is done, for example,
   * by the <code>SeveredStackTraces</code> trait.
   * </p>
   *
   * @return a user-presentable string containing the filename and line number that caused the failed test
   */
  def failedCodeFileNameAndLineNumberString: Option[String] = {
    for (fileName <- failedCodeFileName; lineNum <- failedCodeLineNumber) yield
      fileName + ":" + lineNum
  }

  /**
    * A string that provides the absolute filename and line number of the line of code that failed, suitable
    * for presenting to a user of the failing line.  It calls <code>failedCodeFilePathname</code> and
    * <code>failedCodeLineNumber</code> to get the failing absolute filename and line number.
    *
    * @return a user-presentable string containing the absolute filename and line number that caused the failed test
    */
  lazy val failedCodeFilePathnameAndLineNumberString: Option[String] = {
    for (fileName <- failedCodeFilePathname; lineNum <- failedCodeLineNumber) yield
      fileName + ":" + lineNum
  }

  private def stackTraceElement: Option[StackTraceElement] = {
    val stackTrace = getStackTrace()
    position match {
      case Some(pos) => stackTrace.find(e => StackDepthExceptionHelper.isMatch(e, pos))
      case None =>
        if (stackTrace.length <= failedCodeStackDepth) None
        else Some(stackTrace(failedCodeStackDepth))
    }
  }

  /**
   * A string that provides the filename of the line of code that failed, suitable
   * for presenting to a user, which is taken from this exception's <code>StackTraceElement</code> at the depth specified
   * by <code>failedCodeStackDepth</code>.
   *
   * <p>
   * This is a <code>def</code> instead of a <code>val</code> because exceptions are mutable: their stack trace can
   * be changed after the exception is created. This is done, for example, by the <code>SeveredStackTraces</code> trait.
   * </p>
   *
   * @return a string containing the filename that caused the failed test
   */
  def failedCodeFileName: Option[String] = {
    position match {
      case Some(pos) => Some(pos.fileName)
      case None =>
        stackTraceElement flatMap { ele =>
          StackDepthExceptionHelper.getFailedCodeFileName(ele)
        }
    }
  }

  /**
   * A string that provides the line number of the line of code that failed, suitable
   * for presenting to a user, which is taken from this exception's <code>StackTraceElement</code> at the depth specified
   * by <code>failedCodeStackDepth</code>.
   *
   * <p>
   * This is a <code>def</code> instead of a <code>val</code> because exceptions are mutable: their stack trace can
   * be changed after the exception is created. This is done, for example, by the <code>SeveredStackTraces</code> trait.
   * </p>
   *
   * @return a string containing the line number that caused the failed test
   */
  def failedCodeLineNumber: Option[Int] = {
    position match {
      case Some(pos) => Some(pos.lineNumber)
      case None =>
        stackTraceElement flatMap { ele =>
          val lineNum = ele.getLineNumber
          if (lineNum > 0) Some(lineNum) else None
        }
    }
  }

  /**
   * Returns an exception of the same class with <code>failedExceptionStackDepth</code> set to 0 and
   * all frames above this stack depth severed off. This can be useful when working with tools (such as IDEs) that do not
   * directly support ScalaTest. (Tools that directly support ScalaTest can use the stack depth information delivered
   * in the StackDepth exceptions.)
   */
  def severedAtStackDepth: Throwable with StackDepth
}
