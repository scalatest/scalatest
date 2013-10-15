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
package org.scalatest.concurrent

import org.scalatest.time.{Millis, Seconds, Span}

/**
 * Stackable modification trait for <a href="PatienceConfiguration.html"><code>PatienceConfiguration</code></a> that provides default timeout and interval
 * values appropriate for integration testing. 
 *
 * <p>
 * The default values for the parameters are:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><strong>Configuration Parameter</strong></th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><strong>Default Value</strong></th></tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <code>timeout</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <code>scaled(15 seconds)</code>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <code>interval</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <code>scaled(150 milliseconds)</code>
 * </td>
 * </tr>
 * </table>
 *
 * <p>
 * The default values of both timeout and interval are passed to the <code>scaled</code> method, inherited
 * from <code>ScaledTimeSpans</code>, so that the defaults can be scaled up
 * or down together with other scaled time spans. See the documentation for trait <a href="ScaledTimeSpans.html"><code>ScaledTimeSpans</code></a>
 * for more information.
 * </p>
 *
 * <p>
 * Mix this trait into any class that uses <code>PatienceConfiguration</code> (such as classes that mix in <a href="Eventually.html"><code>Eventually</code></a>
 * or <a href="AsyncAssertions.html"><code>AsyncAssertions</code></a>) to get timeouts tuned towards integration testing, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * class ExampleSpec extends FeatureSpec with Eventually with IntegrationPatience {
 *   // ...
 * }
 * </pre>
 *
 * @author Bill Venners
 * @author Chua Chee Seng
 */
trait IntegrationPatience extends AbstractPatienceConfiguration { this: PatienceConfiguration =>

  private val defaultPatienceConfig: PatienceConfig =
    PatienceConfig(
      timeout = scaled(Span(15, Seconds)),
      interval = scaled(Span(150, Millis))
    )

  /**
   * Implicit <code>PatienceConfig</code> value providing default configuration values suitable for integration testing.
   */
  implicit abstract override val patienceConfig: PatienceConfig = defaultPatienceConfig
}
