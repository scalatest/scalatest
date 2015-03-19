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
import scala.collection.GenTraversable

private[scalatest] class ConcurrentLinkedQueue[T] extends Serializable {

  private final val queue = new java.util.concurrent.ConcurrentLinkedQueue[T]

  def add(ele: T): Unit = {
    queue.add(ele)
  }

  def iterator: Iterator[T] = queue.iterator.asScala

  def isEmpty: Boolean = queue.isEmpty

  def asScala: GenTraversable[T] = queue.asScala
}

private[scalatest] class CountDownLatch(count: Int) {

  @transient private final val latch = new java.util.concurrent.CountDownLatch(count)

  def countDown(): Unit = latch.countDown()

  def getCount: Long = latch.getCount

  def await(): Unit = latch.await()
}

private[scalatest] object NameTransformer {

  def decode(encoded: String): String = scala.reflect.NameTransformer.decode(encoded)

}