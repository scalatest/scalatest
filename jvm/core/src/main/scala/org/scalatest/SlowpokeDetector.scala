/*
 * Copyright 2001-2025 Artima, Inc.
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
import java.util.concurrent.ConcurrentSkipListSet
import scala.collection.JavaConverters._
import java.io.PrintStream
import org.scalactic.Requirements._

private[scalatest] class SlowpokeDetector(timeout: Long = 60000, out: PrintStream = Console.err) { // Default timeout is 1 minute

  // SKIP-SCALATESTJS-START
  private final val runningTests = new ConcurrentSkipListSet[RunningTest]
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY private final val runningTests = new scala.collection.mutable.TreeSet[RunningTest]
 
  def testStarting(suiteName: String, suiteId: String, testName: String, timeStamp: Long): Unit = {
    requireNonNull(suiteName, suiteId, testName)
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
  def testFinished(suiteName: String, suiteId: String, testName: String): Unit = {
    requireNonNull(suiteName, suiteId, testName)
    val wasRemoved =
      runningTests.remove( // removal uses equality, which is determined only by suite ID and test name
        new RunningTest(
          suiteName = suiteName,
          suiteId = suiteId,
          testName = testName,
          startTimeStamp = 0
        )
      )
    if (!wasRemoved) {
      val stringToPrint = Resources.slowpokeDetectorEventNotFound(suiteName, suiteId, testName)
      out.println(stringToPrint)
    }
  }

  def detectSlowpokes(currentTimeStamp: Long): IndexedSeq[Slowpoke] = {
    // SKIP-SCALATESTJS-START
    val rts = runningTests.iterator.asScala.toVector
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val rts = runningTests.iterator.toVector
    val slowTests = rts.filter(currentTimeStamp - _.startTimeStamp > timeout)
    slowTests.sortBy(_.startTimeStamp).map(_.toSlowpoke(currentTimeStamp))
  }
}
