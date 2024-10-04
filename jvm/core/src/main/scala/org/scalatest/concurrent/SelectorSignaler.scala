/*
 * Copyright 2001-2024 Artima, Inc.
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
import java.nio.channels.Selector

/**
 * Strategy for signaling an operation in which <code>wakeup</code> is called on the <code>java.nio.channels.Selector</code> passed to
 * the constructor.
 *
 * <p>
 * This class can be used for configuration when using traits <a href="TimeLimits.html"><code>TimeLimits</code></a>
 * and <a href="TimeLimitedTests.html"><code>TimeLimitedTests</code></a>.
 * <p>
 */
class SelectorSignaler(selector: Selector) extends Signaler {
  
  /**
   * Invokes <code>wakeup</code> on the <code>java.nio.channels.Selector</code> passed to this class's constructor.
   *
   * @param testThread unused by this strategy
   */
  def apply(testThread: Thread): Unit = {
    selector.wakeup()
  }
}

/**
 * Companion object that provides a factory method for a <code>SelectorSignaler</code>.
 */
object SelectorSignaler {

  /**
   * Factory method for a <code>SelectorSignaler</code>.
   *
   * @param selector the <code>java.nio.channels.Selector</code> to pass to the <code>SelectorSignaler</code> constructor
   */
  def apply(selector: Selector) = new SelectorSignaler(selector)
}

