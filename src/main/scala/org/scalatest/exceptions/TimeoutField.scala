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
package org.scalatest.exceptions

import org.scalatest.time.Span

/**
 * Trait mixed into exceptions thrown due to a timeout, which offers
 * a <code>timeout</code> field providing a <a href="../time/Span.html"><code>Span</code></a> representing the timeout that expired.
 *
 * <p>
 * This trait is used by trait <a href="TimeLimitedTests.html"><code>TimeLimitedTests</code></a> to detect exceptions thrown because of timeouts, and
 * for such exceptions, to modify the message to more clearly indicate a test timed out. (Although in its initial
 * release there is only one subclass of <code>TimeoutField</code> in ScalaTest,
 * <a href="TestFailedDueToTimeoutException.html"><code>TestFailedDueToTimeoutException</code></a>,
 * in a future version of ScalaTest, there will be another....)
 * </p>
 */
trait TimeoutField { this: Throwable => 

  /**
   * The timeout that expired causing this <code>Timeout</code> exception.
   */
  val timeout: Span
}

/*
Will need to add cancelAfter to the doc comment in 2.0.
*/

