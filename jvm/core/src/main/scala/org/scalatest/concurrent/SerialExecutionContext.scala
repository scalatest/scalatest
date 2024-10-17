/*
 * Copyright 2001-2024 Artima, Inc.
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

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import org.scalatest.Outcome

/*
This is private[scalatest] so that no one can actually use it except
when we install it as the default. It can't be used generally because
we special case it in the AsyncEngine. Whatever is referenced from
executionContext is what is passed into AsyncEngine, and then we
do a runtime type check to see if it is this one, and if so, we 
call runNow.
*/
private[scalatest] class SerialExecutionContext extends ExecutionContext {

  /*
  A LinkedBlockingQueue is thread-safe, so don't need to synchronize
  its access.
  */
  private final val queue = new org.scalatest.LinkedBlockingQueue[Runnable]

  def execute(runnable: Runnable): Unit = {
    queue.put(runnable)
    // If this is a thread different than the main test thread, we'll
    // notify the main test thread in case it is sitting in the wait
    // in runNow.
    // synchronized { notifyAll() }
  }

  def reportFailure(t: Throwable): Unit =
    t.printStackTrace()

  /*
     runNow will keep executing jobs passed to execute until the
     Future passed to runNow completes. If it runs out of jobs to
     do before the Future completes, it enters the wait set.
     Adding another job to the queue will notify it. We don't
     notify when the future completes, because the last thing an
     AsyncTestSuite currently does is that it transforms a completed
     Future[Assertion] returned from the test into a Future[Outcome].
     That transformation is done using the executionContext, which
     in this case is a SerialExecutionContext. This is always the
     final transformation needed to complete a test, after which
     the Future[Outcome] will be complete. It therefore is enqueued
     onto the LinkedBlockingQueue by calling execute. If from a
     thread other than the main test thread, this will do a notifyAll
     to wake up the main test thread if it is sitting in the wait
     loop in runNow:

     // After being notified, wait() will return
     synchronized { wait() }

     If so, it will loop back and find that as yet the Future[Outcome]
     is not completed, so it will execute the while body again:
  
      while (!future.isCompleted) {
        // It will execute here again
      }
 
     It will find out that the queue does actually contain a job to do:

     // queue.peek will return a job to do, not null
     while (queue.peek != null)

     It will execute this job, which is the transformation from
     Future[Assertion] to Future[Outcome]:

     // This will transform Future[Assertion] => Future[Outcome]
     queue.poll().run()

     That will complete the Future[Outcome], so when it tests to
     see if the Future[Outcome] is completed, it will find that it
     has:

     // This if test fails. !future.isCompleted is now false
     if (!future.isCompleted)

     Thus it will skip the wait() call and loop back to the top
     of the outer while. That will finally return false:
    
      while (!future.isCompleted) {
        // Doesn't get here
      } // Just hops down here because the future is now complete
 
      And runNow will return, as this test has finished running.
   */
  /*def runNow(future: Future[Outcome]): Unit = {
    while (!future.isCompleted || queue.size > 0)
      queue.take().run() // What to do about exceptions here?
  }*/


  def runNow(future: Future[Outcome]): Unit = recRunNow(future)

  @tailrec
  private def recRunNow(future: Future[Outcome]): Unit = // Could take a scala.concurrent.duration.Deadline to be used for poll(timeout, timeUnit)
    if (future.isCompleted && queue.size == 0) ()
    else {
      //SCALATESTJS,NATIVE-ONLY if (!future.isCompleted && queue.size == 0)
      //SCALATESTJS,NATIVE-ONLY throw new IllegalStateException("Queue is empty while future is not completed, this means you're probably using a wrong ExecutionContext for your task, please double check your Future.")
      val task = queue.take() // Note that this will block if queue is empty, alternatively we can use poll(timeout, timeUnit) to deal with deadlines
      task.run()  // TODO: this should abort the suite, let's write a test for that
      recRunNow(future)
    }
}
