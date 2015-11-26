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
package org.scalatest

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.scalatest.exceptions.TestFailedException
import scala.reflect.ClassTag

/**
 * @author Bill Venners
 */
trait RecoverMethods {

  // SKIP-SCALATESTJS-START
  private[scalatest] val failStackDepthForRecover = 4
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY private[scalatest] val failStackDepthForRecover = 13

  def recoverToExceptionIf[T <: AnyRef](future: Future[Any])(implicit classTag: ClassTag[T], exCtx: ExecutionContext): Future[T] = {
    val clazz = classTag.runtimeClass
    future.failed.transform(
      ex =>
        if (!clazz.isAssignableFrom(ex.getClass)) {
          val message = Resources.wrongException(clazz.getName, ex.getClass.getName)
          throw newAssertionFailedExceptionForRecover(Some(message), Some(ex), failStackDepthForRecover)
        }
        else ex.asInstanceOf[T]
      ,
      ex => {
        val message = Resources.exceptionExpected(clazz.getName)
        throw newAssertionFailedExceptionForRecover(Some(message), None, failStackDepthForRecover)
      }
    )
  }

  def recoverToSucceededIf[T <: AnyRef](future: Future[Any])(implicit classTag: ClassTag[T], exCtx: ExecutionContext): Future[Assertion] = {
    val clazz = classTag.runtimeClass
    future.failed.transform(
      ex =>
        if (!clazz.isAssignableFrom(ex.getClass)) {
          val message = Resources.wrongException(clazz.getName, ex.getClass.getName)
          throw newAssertionFailedExceptionForRecover(Some(message), Some(ex), failStackDepthForRecover)
        }
        else Succeeded
      ,
      ex => {
        val message = Resources.exceptionExpected(clazz.getName)
        throw newAssertionFailedExceptionForRecover(Some(message), None, failStackDepthForRecover)
      }
    )
  }

  private[scalatest] def newAssertionFailedExceptionForRecover(optionalMessage: Option[Any], optionalCause: Option[Throwable], stackDepth: Int): Throwable =
    (optionalMessage, optionalCause) match {
      case (None, None) => new TestFailedException(stackDepth)
      case (None, Some(cause)) => new TestFailedException(cause, stackDepth)
      case (Some(message), None) => new TestFailedException(message.toString, stackDepth)
      case (Some(message), Some(cause)) => new TestFailedException(message.toString, cause, stackDepth)
    }
}

/**
 * Companion object that facilitates the importing of <code>RecoverMethods</code>'s method as 
 * an alternative to mixing it in. One use case is to import <code>RecoverMethods</code>'s method so you can use
 * it in the Scala interpreter.
 *
 * @author Bill Venners
 */
object RecoverMethods extends RecoverMethods

