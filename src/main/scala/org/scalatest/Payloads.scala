/*
 * Copyright 2001-2013 Artima, Inc.
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
 * stimulated by the exception, such as a <a href="events/TestFailed.html"><code>TestFailed</code></a> event stimulated by a <a href="exceptions/TestFailedException.html"><code>TestFailedException</code></a>.
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
  def withPayload[T](payload: => Any)(fun: => T): T = {
    try {
      val outcome: T = fun
      outcome match {
        case Failed(e: org.scalatest.exceptions.ModifiablePayload[_]) if payload != null =>
          Failed(e.modifyPayload((currentPayload: Option[Any]) => Some(payload))).asInstanceOf[T]
        case Canceled(e: org.scalatest.exceptions.ModifiablePayload[_]) if payload != null =>
          Canceled(e.modifyPayload((currentPayload: Option[Any]) => Some(payload))).asInstanceOf[T]
        case _ => outcome
      }
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

/**
 * Companion object that facilitates the importing of <code>Payloads</code> members as 
 * an alternative to mixing it in. One use case is to import <code>Payloads</code>
 * members so you can use them in the Scala interpreter.
 */
object Payloads extends Payloads
