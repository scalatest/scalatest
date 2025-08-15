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
package org.scalatest.concurrent

/**
 * Signaling strategy in which nothing is done to try and signal or interrupt an operation.
 *
 * <p>
 * This object can be used for configuration when using traits <a href="TimeLimits.html"><code>TimeLimits</code></a>
 * and <a href="TimeLimitedTests.html"><code>TimeLimitedTests</code></a>.
 * <p>
 */
object DoNotSignal extends Signaler {
  /**
   * Does nothing.
   *
   * @param testThread unused by this strategy
   */
  def apply(testThread: Thread): Unit = {} // Don't do nuthin
}

