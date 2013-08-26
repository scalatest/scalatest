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
 * Trait that contains the <code>update</code> method, which can be used to send a status update to the reporter.
 *
 * <p>
 * The difference between <code>update</code> and the <code>info</code> method of <code>Informer</code> is that
 * <code>info</code> messages provided during a test are recorded and sent as part of test completion event, whereas
 * <code>update</code> messages are sent right away as <code>UpdateProvided</code> messages. For long-running tests,
 * <code>update</code> allows you to send "status updates" to the reporter right away, so users can track the
 * progress of the long-running tests. By contrast, <code>info</code> messages will only be seen by the user after the
 * test has completed, and are more geared towards specification (such as Given/When/Then messages) than notification.
 * </p>
 */
trait Updating {

  /**
   * Send a status update via an <code>UpdateProvided</code> event to the reporter.
   */
  protected def update: Updater
}
