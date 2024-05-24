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
package org.scalatest

import collection.JavaConverters._
import java.util.concurrent.atomic.AtomicReference
import org.scalactic.ColCompatHelper.Iterable

private[scalatest] class ConcurrentLinkedQueue[T] extends Serializable {

  private final val queue = new scala.collection.mutable.ListBuffer[T]

  def add(ele: T): Unit = {
    queue += ele
  }

  def iterator: Iterator[T] = queue.iterator

  def isEmpty: Boolean = queue.isEmpty

  def asScala: Iterable[T] = queue

  def poll: T = queue.remove(0)
}

private[scalatest] class LinkedBlockingQueue[T] extends Serializable {

  private final val queue = new scala.collection.mutable.ListBuffer[T]

  def put(ele: T): Unit = queue += ele

  def take(): T = queue.remove(0)

  def size: Int = queue.size
}

private[scalatest] class CountDownLatch(count: Int) {

  private var currentCount: Long = count

  def countDown(): Unit = {
    currentCount =
      if (currentCount > 0)
        currentCount - 1
      else
        0
  }

  def getCount: Long = currentCount

  def await(): Unit =
    if (currentCount == 0)
      return
    else
      throw new UnsupportedOperationException("Scala.js is single-threaded!")
}

private[scalatest] object NameTransformer {

  def decode(encoded: String): String = scala.reflect.NameTransformer.decode(encoded)

}

private[scalatest] trait TimerTask extends Runnable {
  // var timerTaskRef: Option[java.util.TimerTask] = None

  def run(): Unit

  def cancel(): Unit = {
    // timerTaskRef.foreach(_.cancel())
  }
}

private[scalatest] class Timer {

  // val timer = new java.util.Timer

  def schedule(task: TimerTask, delay: Long): Unit = {
    // val javaTimerTask = new java.util.TimerTask {
    //   def run(): Unit = {
    //     task.run()
    //   }
    // }
    //
    // task.timerTaskRef = Some(javaTimerTask)
    // timer.schedule(javaTimerTask, delay)
  }

  def schedule(task: TimerTask, delay: Long, period: Long): Unit = {
    // val javaTimerTask = new java.util.TimerTask {
    //   def run(): Unit = {
    //     task.run()
    //   }
    // }
    //
    // task.timerTaskRef = Some(javaTimerTask)
    // timer.schedule(javaTimerTask, delay, period)
  }

  def cancel(): Unit = {
    // timer.cancel()
  }

}
