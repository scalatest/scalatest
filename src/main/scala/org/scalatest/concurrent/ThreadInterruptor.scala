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
package org.scalatest.concurrent

/**
 * Strategy for interrupting an operation in which <code>interrupt</code> is called on the <code>Thread</code> passed
 * to <code>apply</code>.
 *
 * <p>
 * This object can be used for configuration when using traits <a href="Timeouts.html"><code>Timeouts</code></a>
 * and <a href="TimeLimitedTests.html"><code>TimeLimitedTests</code></a>.
 * <p>
 */
object ThreadInterruptor extends Interruptor {

  /**
   * Invokes <code>interrupt</code> on the passed <code>Thread</code>.
   *
   * @param testThread the <code>Thread</code> to interrupt
   */
  def apply(testThread: Thread) {
    testThread.interrupt()
  }
}
