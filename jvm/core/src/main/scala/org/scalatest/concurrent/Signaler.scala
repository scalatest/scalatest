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

/**
 * Strategy for signaling an operation after a timeout expires.
 *
 * <p>
 * An instance of this trait is used for configuration when using traits
 * <a href="TimeLimits.html"><code>TimeLimits</code></a> and <a href="TimeLimitedTests.html"><code>TimeLimitedTests</code></a>.
 * </p>
 */
trait Signaler {

  /**
   * Signals an operation.
   *
   * <p>
   * This method may do anything to attempt to signal or interrupt an operation, or even do nothing.
   * When called by <code>failAfter</code> method of trait <a href="TimeLimits.html"><code>TimeLimits</code></a>, the passed
   * <code>Thread</code> will represent the main test thread. This <code>Thread</code> is
   * passed in case it is useful, but need not be used by implementations of this method.
   * </p>
   */
  def apply(testThread: Thread): Unit
}

/**
 * Companion object that provides a factory method for a <code>Singlaer</code> defined
 * in terms of a function from a function of type <code>Thread</code> to </code>Unit</code>.
 */
object Signaler {

  /**
   * Factory method for a <code>Signaller</code> defined in terms of a function from a function of
   * type <code>Thread</code> to </code>Unit</code>.
   *
   * When this <code>apply</code> method is invoked, it will invoke the passed function's <code>apply</code>
   * method, forwarding along the passed <code>Thread</code>.
   *
   * @param fun the function representing the signaling strategy
   */
  def apply(fun: Thread => Unit) =
    new Signaler {
      def apply(testThread: Thread): Unit = { fun(testThread) }
    }

  /**
   * Implicit <code>Signaler</code> value defining a default signaling strategy for the <code>failAfter</code> and <code>cancelAfter</code> method
   * of trait [[org.scalatest.concurrent.TimeLimits]].
   */
  implicit def default: Signaler = DoNotSignal
}

