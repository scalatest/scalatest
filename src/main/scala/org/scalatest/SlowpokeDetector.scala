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

import time.Span
import time.SpanSugar._
import java.util.concurrent.ConcurrentSkipListSet
import scala.collection.JavaConverters._

private[scalatest] class SlowpokeDetector(timeout: Long = 60000) { // Default timeout is 1 minute

  private final val runningTests = new ConcurrentSkipListSet[RunningTest]

  def testStarting(suiteName: String, suiteId: String, testName: String, timeStamp: Long): Unit = {
    if (suiteName == null || suiteId == null || testName == null) throw new NullPointerException
    require(timeStamp >= 0, "timeStamp must be >= 0")
    runningTests.add(
      new RunningTest(
        suiteName = suiteName,
        suiteId = suiteId,
        testName = testName,
        startTimeStamp = timeStamp
      )
    )
  }
  def testFinished(suiteName: String, suiteId: String, testName: String, timeStamp: Long): Unit = {
    if (suiteName == null || suiteId == null || testName == null) throw new NullPointerException
    require(timeStamp >= 0, "timeStamp must be >= 0")
  }

  def detectSlowpokes(currentTimeStamp: Long): IndexedSeq[Slowpoke] = {
    val rts = runningTests.iterator.asScala.toVector
    val slowTests = rts.filter(currentTimeStamp - _.startTimeStamp > timeout)
    slowTests.map(_.toSlowpoke(currentTimeStamp))
  }
}
