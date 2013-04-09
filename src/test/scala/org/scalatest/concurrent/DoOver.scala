/*
 * Copyright 2001-2011 Artima, Inc.
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

private[concurrent] object DoOver {
  def tryTryAgain(maxTries: Int = 1, delayBetweenTries: Int = 0)(fun: => Unit) {

    if (maxTries < 1)
      throw new IllegalArgumentException("maxTries cannot be less than 1, but was equal to " + maxTries)

    var successCount = 0
    var failureCount = 0

    val maxCount = (maxTries / 2) + (maxTries % 2)

    while (successCount < maxCount && failureCount < maxCount)
      try {
        fun
        successCount += 1
      }
      catch {
        case e: Exception =>
          failureCount += 1
          if (failureCount == maxCount)
            throw e
      }
  }
}
