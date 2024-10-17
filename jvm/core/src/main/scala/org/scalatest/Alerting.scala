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
package org.scalatest

/**
 * Trait that contains the <code>alert</code> method, which can be used to send an alert to the reporter.
 *
 * <p>
 * One difference between <code>alert</code> and the <code>info</code> method of <code>Informer</code> is that
 * <code>info</code> messages provided during a test are recorded and sent as part of test completion event, whereas
 * <code>alert</code> messages are sent right away as <code>AlertProvided</code> messages. For long-running tests,
 * <code>alert</code> allows you to send "alert notifications" to the reporter right away, so users can be made aware
 * of potential problems being experienced by long-running tests. By contrast, <code>info</code> messages will only be seen by the user after the
 * test has completed, and are more geared towards specification (such as Given/When/Then messages) than notification.
 * </p>
 *
 * <p>
 * The difference between <code>alert</code> and the <code>update</code> method of <a href="Updating.html"><code>Updating</code></a> is
 * that <code>alert</code> is intended to be used
 * for warnings or notifications of potential problems, whereas <code>update</code> is just for status updates.
 * In string reporters for which ANSI color is enabled, <code>update</code> notifications are shown in green and <code>alert</code> notifications
 * in yellow.
 * </p>
 */
trait Alerting {

  /**
   * Returns an <code>Alerter</code> that can send an alert message via an <code>AlertProvided</code> event to the reporter.
   */
  protected def alert: Alerter
}
