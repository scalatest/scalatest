/*
 * Copyright 2001-2011 Artima, Inc.
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

import org.scalatest._
import time.Span

/**
 * Trait providing methods and classes used to configure timeouts and, where relevant, the interval
 * between retries.
 *
 * <p>
 * This trait is called <code>PatienceConfiguration</code> because it allows configuration of two
 * values related to patience: The timeout specifies how much time asynchronous operations will be given
 * to succeed before giving up. The interval specifies how much time to wait between checks to determine
 * success when polling.
 * </p>
 *
 * <p>
 * The default values for timeout and interval provided by trait <code>PatienceConfiguration</code> are tuned for unit testing,
 * where running tests as fast as
 * possible is a high priority and subsystems requiring asynchronous operations are therefore often replaced
 * by mocks. This table shows the default values:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><strong>Configuration Parameter</strong></th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><strong>Default Value</strong></th></tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <code>timeout</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <code>scaled(150 milliseconds)</code>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <code>interval</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <code>scaled(15 milliseconds)</code>
 * </td>
 * </tr>
 * </table>
 *
 * <p>
 * Values more appropriate to integration testing, where asynchronous operations tend to take longer because the tests are run
 * against the actual subsytems (not mocks), can be obtained by mixing in trait <a href="IntegrationPatience.html"><code>IntegrationPatience</code></a>.
 * </p>
 *
 * <p>
 * The default values of both timeout and interval are passed to the <code>scaled</code> method, inherited
 * from <a href="ScaledTimeSpans.html"><code>ScaledTimeSpans</code></a>, so that the defaults can be scaled up
 * or down together with other scaled time spans. See the documentation for trait <a href="ScaledTimeSpans.html"><code>ScaledTimeSpans</code></a>
 * for more information.
 * </p>
 *
 * <p>
 * Timeouts are used by the <code>eventually</code> methods of trait
 * <a href="Eventually.html"><code>Eventually</code></a> and the <code>await</code> method of class
 * <a href="AsyncAssertions/Waiter.html"><code>Waiter</code></a>, a member of trait
 * <a href="AsyncAssertions.html"><code>AsyncAssertions</code></a>. Intervals are used by 
 * the <code>eventually</code> methods.
 * </p>
 *
 * @author Bill Venners
 */
trait PatienceConfiguration extends AbstractPatienceConfiguration {

  /**
   * Abstract class defining a family of configuration parameters for traits <code>Eventually</code> and <code>AsyncAssertions</code>.
   * 
   * <p>
   * The subclasses of this abstract class are used to pass configuration information to
   * the <code>eventually</code> methods of trait <code>Eventually</code> and the <code>await</code> methods of trait <code>AsyncAssertions</code>.
   * </p>
   *
   * @author Bill Venners
   * @author Chua Chee Seng
   */
  sealed abstract class PatienceConfigParam

  /**
   * A <code>PatienceConfigParam</code> that specifies the maximum amount of time to wait for an asynchronous operation to
   * complete. 
   *
   * @param value the maximum amount of time to retry before giving up and throwing
   *   <code>TestFailedException</code>.
   *
   * @author Bill Venners
   */
  final case class Timeout(value: Span) extends PatienceConfigParam
 // TODO: Check for null
  /**
   * A <code>PatienceConfigParam</code> that specifies the amount of time to sleep after
   * a retry.
   *
   * @param value the amount of time to sleep between each attempt
   *
   * @author Bill Venners
   */
  final case class Interval(value: Span) extends PatienceConfigParam
 // TODO: Check for null

  private val defaultPatienceConfig = PatienceConfig()

  /**
   * Implicit <code>PatienceConfig</code> value providing default configuration values.
   *
   * <p>
   * To change the default configuration, override or hide this <code>def</code> with another implicit
   * <code>PatienceConfig</code> containing your desired default configuration values.
   * </p>
   */
  implicit def patienceConfig = defaultPatienceConfig

  /**
   * Returns a <code>Timeout</code> configuration parameter containing the passed value, which
   * specifies the maximum amount to wait for an asynchronous operation to complete.
   */
  def timeout(value: Span) = Timeout(value)

  /**
   * Returns an <code>Interval</code> configuration parameter containing the passed value, which
   * specifies the amount of time to sleep after a retry.
   */
  def interval(value: Span) = Interval(value)    // TODO: Throw NPE
}
