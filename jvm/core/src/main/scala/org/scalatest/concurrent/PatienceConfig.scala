/*
 * Copyright 2001-2025 Artima, Inc.
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

import org.scalatest.time.{Span, Millis}

/**
  * Configuration object for asynchronous constructs, such as those provided by traits <a href="Eventually.html"><code>Eventually</code></a> and
  * <a href="Waiters.html"><code>Waiters</code></a>.
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
  * @param timeout the maximum amount of time to wait for an asynchronous operation to complete before giving up and throwing
  *   <code>TestFailedException</code>.
  * @param interval the amount of time to sleep between each check of the status of an asynchronous operation when polling
  *
  * @author Bill Venners
  * @author Chua Chee Seng
  */
final case class PatienceConfig(timeout: Span = PatienceConfiguration.scaled(Span(150, Millis)), interval: Span = PatienceConfiguration.scaled(Span(15, Millis)))