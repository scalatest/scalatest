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
import java.util.concurrent.LinkedBlockingQueue

/*
private[scalatest] class DefaultExecutionContext extends ExecutionContext {

  /*val execSvc: ExecutorService = Executors.newFixedThreadPool(1, Executors.defaultThreadFactory)
  private val futureQueue = new LinkedBlockingQueue[Future[T] forSome { type T }]*/

  private val queue = new LinkedBlockingQueue[Runnable]

  //val executor = Executors.newSingleThreadScheduledExecutor()

  def execute(runnable: Runnable): Unit = {
    println("###put into queue!!!")
    queue.put(runnable)
    println("***queue size after put: " + queue.size)
  }

  def reportFailure(t: Throwable): Unit =
    t.printStackTrace()

  def runNow(): Unit = {
    println("###runNow!!! size: " + queue.size)
    val itr = queue.iterator
    while(itr.hasNext) {
      println("---running...")
      val r = itr.next
      r.run()
    }
    println("###completed")
    queue.clear()
  }

}

object DefaultExecutionContext {

  object Implicits {
    implicit lazy val global: ExecutionContext = new DefaultExecutionContext
  }

}
*/
