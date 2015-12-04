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

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

private[scalatest] class DefaultExecutionContext extends ExecutionContext {

  /*val execSvc: ExecutorService = Executors.newFixedThreadPool(1, Executors.defaultThreadFactory)
  private val futureQueue = new LinkedBlockingQueue[Future[T] forSome { type T }]*/

  val executor = Executors.newSingleThreadScheduledExecutor()

  def execute(runnable: Runnable): Unit = {
    executor.schedule(new Runnable {
      def run(): Unit = {
        try {
          runnable.run()
        }
        catch {
          case t: Throwable => reportFailure(t)
        }
      }
    }, 0L, TimeUnit.SECONDS)
  }

  def reportFailure(t: Throwable): Unit =
    t.printStackTrace()

}

object DefaultExecutionContext {

  object Implicits {
    implicit lazy val global: ExecutionContext = new DefaultExecutionContext
  }

}
