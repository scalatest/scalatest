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

import java.net.Socket

/**
 * Strategy for interrupting an operation in which <code>close</code> is called on the <code>Socket</code> passed to
 * the constructor.
 *
 * <p>
 * This class can be used for configuration when using traits <a href="Timeouts.html"><code>Timeouts</code></a>
 * and <a href="TimeLimitedTests.html"><code>TimeLimitedTests</code></a>.
 * <p>
 */
class SocketInterruptor(socket: Socket) extends Interruptor {

  /**
   * Invokes <code>close</code> on the <code>Socket</code> passed to this class's constructor.
   *
   * @param testThread unused by this strategy
   */
  def apply(testThread: Thread) {
    socket.close()
  }
}

/**
 * Companion object that provides a factory method for a <code>SocketInterruptor</code>.
 */
object SocketInterruptor {

  /**
   * Factory method for a <code>SocketInterruptor</code>.
   *
   * @param socket the <code>Socket</code> to pass to the <code>SocketInterruptor</code> constructor
   */
  def apply(socket: Socket) = new SocketInterruptor(socket)
}
