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
 * Trait providing an <code>apply</code> method to which alert messages about a running suite of tests can be reported.
 * 
 * <p>
 * An <code>Alerter</code> is essentially
 * used to wrap a <code>Reporter</code> and provide easy ways to send alert messages
 * to that <code>Reporter</code> via an <code>AlertProvided</code> event.
 * <code>Alerter</code> contains an <code>apply</code> method that takes a string and
 * an optional payload object of type <code>Any</code>.
 * The <code>Alerter</code> will forward the passed alert <code>message</code> string to the
 * <a href="Reporter.html"><code>Reporter</code></a> as the <code>message</code> parameter, and the optional
 * payload object as the <code>payload</code> parameter, of an <a href="AlertProvided.html"><code>AlertProvided</code></a> event.
 * </p>
 *
 * <p>
 * For insight into the differences between <code>Alerter</code>, <code>Notifier</code>, and <code>Informer</code>, see the
 * main documentation for trait <a href="Alerting.html"><code>Alerting</code></a>.
 * </p>
 */
trait Alerter {

  /**
   * Send an alert message via an <code>AlertProvided</code> event to the reporter.
   */
  def apply(message: String, payload: Option[Any] = None): Provided
}
