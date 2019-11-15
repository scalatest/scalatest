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
 * Trait that contains the <code>note</code> method, which can be used to send a status notification to the reporter.
 *
 * <p>
 * The difference between <code>note</code> and the <code>info</code> method of <a href="Informer.html"><code>Informer</code></a> is that
 * <code>info</code> messages provided during a test are recorded and sent as part of test completion event, whereas
 * <code>note</code> messages are sent right away as <a href="events/NoteProvided.html"><code>NoteProvided</code></a> messages. For long-running tests,
 * <code>note</code> allows you to send "status notifications" to the reporter right away, so users can track the
 * progress of the long-running tests. By contrast, <code>info</code> messages will only be seen by the user after the
 * test has completed, and are more geared towards specification (such as <a href="GivenWhenThen.html">Given/When/Then</a> messages) than notification.
 * </p>
 *
 * <p>
 * The difference between <code>note</code> and the <code>alert</code> method of <a href="Alerting.html"><code>Alerting</code></a> is
 * that <code>alert</code> is intended to be used
 * for warnings or notifications of potential problems, whereas <code>note</code> is just for status notifications.
 * In string reporters for which ANSI color is enabled, <code>note</code> notifications are shown in green and <code>alert</code> notifications
 * in yellow.
 * </p>
 */
trait Notifying {

  /**
   * Returns an <code>Notifier</code> that can send a status notification via an <code>NoteProvided</code> event to the reporter.
   */
  protected def note: Notifier
}
