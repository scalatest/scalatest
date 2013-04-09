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

/**
 * Trait facilitating the inclusion of a payload in a thrown ScalaTest exception.
 *
 * <p>
 * This trait includes a <code>withPayload</code> construct 
 * that enables a payload object (or modified
 * payload object) to be included as the payload of a thrown exception.
 *
 * <p>
 * Many ScalaTest events include an optional "payload" field that can be used
 * to pass information to a custom reporter. This trait facilitates such customization, 
 * by making it easy to insert a payload into a thrown exception, such as a <code>TestFailedException</code>.
 * The thrown exception must mix in <code>Payload</code>.
 * ScalaTest looks for trait <code>Payload</code> and fires any payloads it finds in the relevant ScalaTest event
 * stimulated by the exception, such as a <code>TestFailed</code> event stimulated by a <code>TestFailedException</code>.
 * Here's an example in which a GUI snapshot is included as a payload when a test fails:
 * </p>
 *
 * <pre class="stHighlight">
 * withPayload(generateGUISnapshot()) {
 *   1 + 1 should be === 3
 * }
 * </pre>
 *
 * @author Bill Venners
 */
trait Payloads {

  /**
   * Executes the block of code passed as the second parameter, and, if it
   * completes abruptly with a <code>ModifiablePayload</code> exception,
   * replaces the current payload contained in the exception, if any, with the one passed
   * as the first parameter.
   *
   * <p>
   * This method allows you to insert a payload into a thrown <code>Payload</code> exception (such as
   * a <code>TestFailedException</code>), so that payload can be included in events fired to a custom reporter
   * that can make use of the payload.  
   * Here's an example in which a GUI snapshot is included as a payload when a test fails:
   * </p>
   *
   * <pre class="stHighlight">
   * withPayload(generateGUISnapshot()) {
   *   1 + 1 should be === 3
   * }
   * </pre>
   *
  */
  def withPayload(payload: Any)(fun: => Unit) {
    try {
      fun
    }
    catch {
      case e: org.scalatest.exceptions.ModifiablePayload[_] =>
        if (payload != null)
          throw e.modifyPayload((currentPayload: Option[Any]) => Some(payload))
        else
          throw e
    }
  }
}
