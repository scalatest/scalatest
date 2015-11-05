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
 * Trait providing an <code>apply</code> method to which status updates about a running suite of tests can be reported.
 * 
 * <p>
 * An <code>Notifier</code> is essentially
 * used to wrap a <code>Reporter</code> and provide easy ways to send status updates
 * to that <code>Reporter</code> via an <code>NoteProvided</code> event.
 * <code>Notifier</code> contains an <code>apply</code> method that takes a string and
 * an optional payload object of type <code>Any</code>.
 * The <code>Notifier</code> will forward the passed alert <code>message</code> string to the
 * <a href="Reporter.html"><code>Reporter</code></a> as the <code>message</code> parameter, and the optional
 * payload object as the <code>payload</code> parameter, of an <a href="NoteProvided.html"><code>NoteProvided</code></a> event.
 * </p>
 *
 * <p>
 * For insight into the differences between <code>Notifier</code>, <code>Alerter</code>, and <code>Informer</code>, see the
 * main documentation for trait <a href="Notifying.html"><code>Notifying</code></a>.
 * </p>
 */
trait Notifier {

  /**
   * Send a status update via an <code>NoteProvided</code> event to the reporter.
   */
  def apply(message: String, payload: Option[Any] = None): Unit
}
