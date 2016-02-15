/*
 * Copyright 2001-2016 Artima, Inc.
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

import org.scalatest.Exceptional
import org.scalatest.concurrent.Ender
import org.scalatest.exceptions.StackDepthException
import java.util.TimerTask
import java.util.Timer
import org.scalatest.time.Span
import org.scalatest.concurrent.EnderTimeoutTask

trait Timed[T] {
  def timeoutAfter(
    timeout: Span,
    f: => T,
    interruptor: Ender,
    exceptionFun: Option[Throwable] => StackDepthException
  ): T
}

object Timed {
  implicit def interruptableNatureOf[T]: Timed[T] =
    new Timed[T] {
      def timeoutAfter(
        timeout: Span,
         f: => T,
         interruptor: Ender,
         exceptionFun: Option[Throwable] => StackDepthException
      ): T = {
        val timer = new Timer
        val task = new EnderTimeoutTask(Thread.currentThread(), interruptor)
        timer.schedule(task, timeout.totalNanos / 1000 / 1000) // TODO: Probably use a sleep so I can use nanos
        try {
          val result = f
          timer.cancel()
          result match {
            case Exceptional(ex) => throw ex  // If the result is Exceptional, the exception is already wrapped, just re-throw it to get the old behavior.
            case _ => 
              if (task.timedOut) { 
                if (task.needToResetInterruptedStatus)
                  Thread.interrupted() // To reset the flag probably. He only does this if it was not set before and was set after, I think.
                throw exceptionFun(None)
              }
          }
          result
        }
        catch {
          case t: Throwable => 
            timer.cancel() // Duplicate code could be factored out I think. Maybe into a finally? Oh, not that doesn't work. So a method.
            if(task.timedOut) {
              if (task.needToResetInterruptedStatus)
                Thread.interrupted() // Clear the interrupt status (There's a race condition here, but not sure we an do anything about that.)
              throw exceptionFun(Some(t))
            }
            else
              throw t
        }
      }
    }
}
