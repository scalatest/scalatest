/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.events

import org.scalatest._

/**
 * Class each of whose instances hold summary information about one ScalaTest run.
 *
 * @param testsSucceededCount the number of tests that were reported as succeeded during the run
 * @param testsFailedCount the number of tests that were reported as failed during the run
 * @param testsIgnoredCount the number of tests that were were reported as ignored during the run
 * @param testsPendingCount the number of tests that were reported as pending during the run
 *
 * @author Bill Venners
 */
final case class Summary(testsSucceededCount: Int, testsFailedCount: Int, testsIgnoredCount: Int, testsPendingCount: Int, testsCanceledCount: Int,
  suitesCompletedCount: Int, suitesAbortedCount: Int, scopesPendingCount: Int) {

  /**
   * The number of tests completed, which is the sum of the number of tests that succeeded and failed, excluding any
   * tests that were ignored, canceled, or reported as pending.
   */
  val testsCompletedCount = testsSucceededCount + testsFailedCount
  
  /**
   * The total number of tests, which is the sum of the number of tests that succeeded, failed, were ignored, canceled, or
   * reported as pending.
   */
  val totalTestsCount = testsSucceededCount + testsFailedCount + testsIgnoredCount + testsPendingCount + testsCanceledCount
}

/**
 * Companion object for case class <a href="Summary.html"><code>Summary</code></a>.
 */
object Summary
