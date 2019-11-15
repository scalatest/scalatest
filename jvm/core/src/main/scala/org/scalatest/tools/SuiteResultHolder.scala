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
package org.scalatest.tools

import scala.collection.mutable.ListBuffer
import org.scalatest.events.Summary

private[scalatest] class SuiteResultHolder {

  val suiteList = new ListBuffer[SuiteResult]()
  
  def +=(result: SuiteResult): Unit = {
    suiteList += result
  }
  
  def summary: Summary = {
    val (succeeded, failed, ignored, pending, canceled, scopesPending) = suiteList.foldLeft((0, 0, 0, 0, 0, 0)) { case ((succeeded, failed, ignored, pending, canceled, scopesPending), r) =>
      (succeeded + r.testsSucceededCount, failed + r.testsFailedCount, ignored + r.testsIgnoredCount, 
       pending + r.testsPendingCount, canceled + r.testsCanceledCount, scopesPending + r.scopesPendingCount)
    }
    Summary(succeeded, failed, ignored, pending, canceled, suiteList.length, suiteList.filter(!_.isCompleted).length, scopesPending)
  }
  
  def totalDuration: Long = suiteList.map(s => if (s.duration.isDefined) s.duration.get else 0).sum
}
