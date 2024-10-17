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

private[scalatest] object SleepHelper {

  def sleep(millis: Long, nanos: Int): Unit = {
    sleep(millis)
  }

  def sleep(millis: Long): Unit = {
    val startTime = scala.compat.Platform.currentTime
    val buffer = new StringBuilder
    while ((scala.compat.Platform.currentTime - startTime) < millis) {
      buffer.clear()
      for (i <- 1 to 100)
        buffer.append(i.toString)
    }
  }

}