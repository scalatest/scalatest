/*
 * Copyright 2001-2016 Artima, Inc.
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

import scala.concurrent.Future

/**
 * Supertrait of result objects of methods that fire informational events to the reporter during test runs.
 *
 * The purpose of this result type is to enable <code>info</code>, <code>markup</code>, <code>note</code>, and <code>alert</code>
 * method invocations to appear last in a test body that must end in type <code>Assertion</code>. This is accomplished 
 * by implicitly converting the two possible subobjects of <code>Provided</code>,
 * <a href="Reported$.html"><code>Reported</code></a> and <a href="Recorded$.html"><code>Recorded</code></a>, to <code>Assertion</code>. 
 */
sealed trait Provided

/**
 * Companion object of trait <code>Provided</code>, which provides implicit conversions from <code>Provided</code>
 * to both <code>Assertion</code> and <code>Future[Assertion]</code>.
 */
object Provided {
  import scala.language.implicitConversions

  /**
   * Implicit conversion from <code>Provided</code> to <code>Assertion</code>, which enables ScalaTest's
   * methods that fire informational events to appear last in an traditional (non-async) style test body.
   *
   * @param provided a <code>Provided</code> instance
   * @return the Succeeded singleton
   */
  implicit def convertProvidedToAssertion(provided: Provided): Assertion = Succeeded

  /**
   * Implicit conversion from <code>Provided</code> to <code>Future[Assertion]</code>, which enables ScalaTest's
   * methods that fire informational events to appear last in an async style test body.
   *
   * @param provided a <code>Provided</code> instance
   * @return the Succeeded singleton
   */
  implicit def convertProvidedToFutureAssertion(provided: Provided): Future[Assertion] = Future.successful(Succeeded)
}

/**
 * Object returned by information providing methods that immediately report information
 * to the reporter.
 *
 * <p>
 * This object is returned by <code>note()</code> (see traits <a href="Notifying.html"><code>Notifying</code></a> and
 * <a href="Notifier.html"><code>Notifier</code></a>) and
 * <code>alert()</code> (see traits <a href="Alerting.html"><code>Alerting</code></a> and <a href="Alerter.html"><code>Alerter</code></a>).
 * </p>
 */
object Reported extends Provided

/**
 * Object returned by information providing methods that record information provided during tests, reporting
 * them later in the test completion event.
 *
 * <p>
 * This object is returned by <code>info()</code> (see traits <a href="Informing.html"><code>Informing</code></a> and
 * <a href="Informer.html"><code>Informer</code></a>) and
 * <code>markup()</code> (see traits <a href="Documenting.html"><code>Documenting</code></a> and <a href="Documenter.html"><code>Documenter</code></a>).
 * </p>
 */
object Recorded extends Provided

