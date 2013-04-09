/*
 * Copyright 2001-2012 Artima, Inc.
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

/*
 * If distributingTests comes through, then the DSR will not use
 * a SuiteCompleted message to indicate that the suite is done, but instead
 * will wait for a completedTests invocation.
 */
/**
 * A sorter for the events of a run's distributed suites.
 *
 * <p>
 * This trait is used, for example, when <code>-PS</code> is passed to <a href="tools/Runner$.html"><code>Runner</code></a>, to sort the
 * events of distributed suites such that each suite's events are propagated together, with a timeout if an event takes too long.
 * </p>
 */
trait DistributedSuiteSorter {
  def distributingTests(suiteId: String)
  def completedTests(suiteId: String)
}
