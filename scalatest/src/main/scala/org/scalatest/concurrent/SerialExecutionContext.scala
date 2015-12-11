/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalatest.concurrent

import scala.concurrent.{ExecutionContext, Future}
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedBlockingQueue
import org.scalatest.Outcome

private[scalatest] class SerialExecutionContext extends ExecutionContext {

  private val queue = new LinkedBlockingQueue[Runnable]

  def execute(runnable: Runnable): Unit = {
    queue.put(runnable)
    synchronized { notifyAll() }
  }

  def reportFailure(t: Throwable): Unit =
    t.printStackTrace()

  def runNow(future: Future[Outcome]): Unit = {
    while (!future.isCompleted) {
      while (queue.peek != null)
        queue.poll().run() // What to do about exceptions here?
      if (!future.isCompleted)
        synchronized { wait() }
        // SleepHelper.sleep(10)
    }
  }

}
