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
package org.scalatest.tools


/**
 * A <code>Reporter</code> that prints test status information to
 * the standard output stream.
 *
 * @author Bill Venners
 */
private[scalatest] class StandardOutReporter(
  presentAllDurations: Boolean,
  presentInColor: Boolean,
  presentShortStackTraces: Boolean,
  presentFullStackTraces: Boolean,
  presentUnformatted: Boolean,
  presentReminder: Boolean,
  presentReminderWithShortStackTraces: Boolean,
  presentReminderWithFullStackTraces: Boolean,
  presentReminderWithoutCanceledTests: Boolean,
  presentFilePathname: Boolean,
  presentJson: Boolean
) extends PrintReporter(
  Console.out,
  presentAllDurations,
  presentInColor,
  presentShortStackTraces,
  presentFullStackTraces,
  presentUnformatted,
  presentReminder,
  presentReminderWithShortStackTraces,
  presentReminderWithFullStackTraces,
  presentReminderWithoutCanceledTests,
  presentFilePathname,
  presentJson
) {
  /**
   * Does nothing, because don't want to dispose the standard output stream.
   */
  override def dispose(): Unit = {
  }
}
