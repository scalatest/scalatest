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

import time._
import SpanSugar._

private[scalatest] final class RunningTest(
  val suiteName: String,
  val suiteId: String,
  val testName: String,
  val startTimeStamp: Long
) extends Comparable[RunningTest] {

  // Sorts them in start time order
  def compareTo(other: RunningTest): Int = {
    val diff: Long = startTimeStamp - other.startTimeStamp
    if (diff < 0) -1
    else if (diff > 0) 1
    else 0
  }

  override def equals(other: Any): Boolean =
    other match {
      case rt: RunningTest => rt.suiteId == suiteId && rt.testName == testName
      case _ => false
    }

  override def hashCode: Int =
    41 * (
      41 + suiteId.hashCode
    ) + testName.hashCode

  def toSlowpoke(currentTimeStamp: Long): Slowpoke = 
    Slowpoke(
      suiteName = suiteName,
      suiteId = suiteId,
      testName = testName,
      Span(currentTimeStamp - startTimeStamp, Millis)
    )
}

