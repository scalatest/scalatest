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

/**
 * Trait implemented by <code>PayloadField</code> exception types that can modify their payload.
 *
 * <p>
 * This trait facilitates the <code>withPayload</code> construct provided by trait
 * <code>Payloads</code>. This construct enables a payload object (or modified
 * payload object) to be included as the payload of a thrown exception. The payload
 * can then be included in the ScalaTest event that results from that exception. For
 * example, the payload included in a <code>TestFailedException</code> will be included
 * as the payload of the resulting <code>TestFailed</code> event. Here's an example in
 * which a GUI snapshot is included as a payload when a test fails:
 * </p>
 *
 * <pre class="stHighlight">
 * withPayload(generateGUISnapshot()) {
 *   1 + 1 should be === 3
 * }
 * </pre>
 *
 * <p>
 * Exception types that mix in this trait have a <code>modifyPayload</code> method, which
 * returns an exception identical to itself, except with the payload option replaced with
 * the result of invoking the passed function, supplying the current payload option
 * as the lone <code>Option[Any]</code> parameter.
 * </p>
 */
trait ModifiablePayload[T <: Throwable] { this: Throwable with PayloadField =>
  
  /**
   * Returns an instance of this exception's class, identical to this exception,
   * except with the payload option replaced with
   * the result of invoking the passed function, <code>fun</code>, supplying the current payload option
   * as the lone <code>Option[Any]</code> parameter.
   *
   * <p>
   * Implementations of this method may either mutate this exception or return
   * a new instance with the revised detail message.
   * </p>
   *
   * @param fun A function that returns the new payload option given the old one
   */
  def modifyPayload(fun: Option[Any] => Option[Any]): T
}

