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
package org.scalatest.tools

import org.scalatest.UnitSpec
import StringReporter.fragmentsForEvent
import org.scalatest.events._
import org.scalatest.Resources

class StringReporterAlertSpec extends UnitSpec {

  val AlertText = "a suite name: a test name: some alert"
  val NoteText = "a suite name: a test name: some update"

  object `A StringReporter` {
    object `when presentUnformatted is false but no formatter is supplied` {
      def `should display AlertProvided messages in yellow and unformatted` {
      
        fragmentsForEvent(
          event = AlertProvided(new Ordinal(0), "some alert", Some(NameInfo("a suite name", "a suite Id", Some("a suite class"), Some("a test name")))),
          presentUnformatted = false,
          presentAllDurations = false,
          presentShortStackTraces = false,
          presentFullStackTraces = false,
          presentReminder = false,
          presentReminderWithShortStackTraces = false,
          presentReminderWithFullStackTraces = false,
          presentReminderWithoutCanceledTests = false,
          reminderEvents = Seq.empty
        ) shouldEqual Vector(Fragment(Resources.alertProvided(AlertText), AnsiYellow))
      }
      def `should display NoteProvided messages in green and unformatted` {
      
        fragmentsForEvent(
          event = NoteProvided(new Ordinal(0), "some update", Some(NameInfo("a suite name", "a suite Id", Some("a suite class"), Some("a test name")))),
          presentUnformatted = false,
          presentAllDurations = false,
          presentShortStackTraces = false,
          presentFullStackTraces = false,
          presentReminder = false,
          presentReminderWithShortStackTraces = false,
          presentReminderWithFullStackTraces = false,
          presentReminderWithoutCanceledTests = false,
          reminderEvents = Seq.empty
        ) shouldEqual Vector(Fragment(Resources.noteProvided(NoteText), AnsiGreen))
      }
    }
    object `when a formatter is supplied but presentUnformatted is true` {
      def `should display AlertProvided messages in yellow and unformatted` {
      
        fragmentsForEvent(
          event =
            AlertProvided(
              new Ordinal(0),
              "some alert",
              Some(NameInfo("a suite name", "a suite Id", Some("a suite class"), Some("a test name"))),
              None,
              Some(IndentedText(Resources.alertFormattedText("some alert"), "some alert", 0))
            ),
          presentUnformatted = true,
          presentAllDurations = false,
          presentShortStackTraces = false,
          presentFullStackTraces = false,
          presentReminder = false,
          presentReminderWithShortStackTraces = false,
          presentReminderWithFullStackTraces = false,
          presentReminderWithoutCanceledTests = false,
          reminderEvents = Seq.empty
        ) shouldEqual Vector(Fragment(Resources.alertProvided(AlertText), AnsiYellow))
      }
      def `should display NoteProvided messages in green and unformatted` {
      
        fragmentsForEvent(
          event =
            NoteProvided(
              new Ordinal(0),
              "some update",
              Some(NameInfo("a suite name", "a suite Id", Some("a suite class"), Some("a test name"))),
              None,
              Some(IndentedText(Resources.noteFormattedText("some update"), "some update", 0))
            ),
          presentUnformatted = true,
          presentAllDurations = false,
          presentShortStackTraces = false,
          presentFullStackTraces = false,
          presentReminder = false,
          presentReminderWithShortStackTraces = false,
          presentReminderWithFullStackTraces = false,
          presentReminderWithoutCanceledTests = false,
          reminderEvents = Seq.empty
        ) shouldEqual Vector(Fragment(Resources.noteProvided(NoteText), AnsiGreen))
      }
    }
    object `when a formatter is supplied and presentUnformatted is false` {
      def `should display AlertProvided messages formatted in yellow ` {
      
        fragmentsForEvent(
          event =
            AlertProvided(
              new Ordinal(0),
              "some alert",
              Some(NameInfo("a suite name", "a suite Id", Some("a suite class"), Some("a test name"))),
              None,
              Some(IndentedText(Resources.alertFormattedText("some alert"), "some alert", 0))
            ),
          presentUnformatted = false,
          presentAllDurations = false,
          presentShortStackTraces = false,
          presentFullStackTraces = false,
          presentReminder = false,
          presentReminderWithShortStackTraces = false,
          presentReminderWithFullStackTraces = false,
          presentReminderWithoutCanceledTests = false,
          reminderEvents = Seq.empty
        ) shouldEqual Vector(Fragment(Resources.alertFormattedText("some alert "), AnsiYellow)) // TODO: I'm not sure where the extra space is coming from. Be nice to zap it.
      }
      def `should display NoteProvided messages formatted in green` {

        fragmentsForEvent(
          event =
            NoteProvided(
              new Ordinal(0),
              "some update",
              Some(NameInfo("a suite name", "a suite Id", Some("a suite class"), Some("a test name"))),
              None,
              Some(IndentedText(Resources.noteFormattedText("some update"), "some update", 0))
            ),
          presentUnformatted = false,
          presentAllDurations = false,
          presentShortStackTraces = false,
          presentFullStackTraces = false,
          presentReminder = false,
          presentReminderWithShortStackTraces = false,
          presentReminderWithFullStackTraces = false,
          presentReminderWithoutCanceledTests = false,
          reminderEvents = Seq.empty
        ) shouldEqual Vector(Fragment(Resources.noteFormattedText("some update "), AnsiGreen))
      }
    }
  }
}

