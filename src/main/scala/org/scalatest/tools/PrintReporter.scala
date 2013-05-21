/*
 * Copyright 2001-2008 Artima, Inc.
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

import org.scalatest._
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.util.Iterator
import java.util.Set
import java.io.StringWriter
import org.scalatest.events._
import PrintReporter._
import org.scalatest.junit.JUnitTestFailedError
import StringReporter.colorizeLinesIndividually

/**
 * A <code>Reporter</code> that prints test status information to
 * a <code>Writer</code>, <code>OutputStream</code>, or file.
 *
 * @author Bill Venners
 */
private[scalatest] abstract class PrintReporter(pw: PrintWriter, presentAllDurations: Boolean,
        presentInColor: Boolean, presentShortStackTraces: Boolean, presentFullStackTraces: Boolean,
        presentUnformatted: Boolean) extends StringReporter(
presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces,
presentUnformatted) {

  /**
  * Construct a <code>PrintReporter</code> with passed
  * <code>OutputStream</code>. Information about events reported to instances of this
  * class will be written to the <code>OutputStream</code> using the
  * default character encoding.
  *
  * @param os the <code>OutputStream</code> to which to print reported info
  * @throws NullPointerException if passed <code>os</code> reference is <code>null</code>
  */
  def this(
    os: OutputStream,
    presentAllDurations: Boolean,
    presentInColor: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    presentUnformatted: Boolean
  ) =
    this(
      new PrintWriter(
        new OutputStreamWriter(
          new BufferedOutputStream(os, BufferSize)
        )
      ),
      presentAllDurations,
      presentInColor,
      presentShortStackTraces,
      presentFullStackTraces,
      presentUnformatted
    )

  /**
  * Construct a <code>PrintReporter</code> with passed
  * <code>String</code> file name. Information about events reported to instances of this
  * class will be written to the specified file using the
  * default character encoding.
  *
  * @param filename the <code>String</code> name of the file to which to print reported info
  * @throws NullPointerException if passed <code>filename</code> reference is <code>null</code>
  * @throws IOException if unable to open the specified file for writing
  */
  def this(
    filename: String,
    presentAllDurations: Boolean,
    presentInColor: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    presentUnformatted: Boolean
  ) =
    this(
      new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(filename)), BufferSize)),
      presentAllDurations,
      presentInColor,
      presentShortStackTraces,
      presentFullStackTraces,
      presentUnformatted
    )

  protected def printPossiblyInColor(text: String, ansiColor: String) {
    pw.println(if (presentInColor) colorizeLinesIndividually(text, ansiColor) else text)
  }

  override def apply(event: Event) {

    super.apply(event)

    pw.flush()
  }

  // Closes the print writer. Subclasses StandardOutReporter and StandardErrReporter override dispose to do nothing
  // so that those aren't closed.
  override def dispose() {
    pw.close()
  }

  override def makeFinalReport(resourceName: String, duration: Option[Long], summaryOption: Option[Summary]) {
    super.makeFinalReport(resourceName, duration, summaryOption)
    pw.flush()
  }

  // We subtract one from test reports because we add "- " in front, so if one is actually zero, it will come here as -1
  // private def indent(s: String, times: Int) = if (times <= 0) s else ("  " * times) + s

  // Stupid properties file won't let me put spaces at the beginning of a property
  // "  {0}" comes out as "{0}", so I can't do indenting in a localizable way. For now
  // just indent two space to the left.  //  if (times <= 0) s 
  //  else Resources("indentOnce", indent(s, times - 1))
}

private[tools] object PrintReporter {

  final val BufferSize = 4096

  final val ansiReset = "\033[0m"
  final val ansiGreen = "\033[32m"
  final val ansiCyan = "\033[36m"
  final val ansiYellow = "\033[33m"
  final val ansiRed = "\033[31m"

  def makeDurationString(duration: Long) = {

    val milliseconds = duration % 1000
    val seconds = ((duration - milliseconds) / 1000) % 60
    val minutes = ((duration - milliseconds) / 60000) % 60
    val hours = (duration - milliseconds) / 3600000
    val hoursInSeconds = hours * 3600
    val hoursInMinutes = hours * 60

    val durationInSeconds = duration / 1000
    val durationInMinutes = durationInSeconds / 60

    if (duration == 1)
      Resources("oneMillisecond")
    else if (duration < 1000)
      Resources("milliseconds", duration.toString)
    else if (duration == 1000)
      Resources("oneSecond")
    else if (duration == 1001)
      Resources("oneSecondOneMillisecond")
    else if (duration % 1000 == 0 && duration < 60000) // 2 seconds, 10 seconds, etc.
      Resources("seconds", seconds.toString)
    else if (duration > 1001 && duration < 2000)// 1 second, 45 milliseconds, etc.
      Resources("oneSecondMilliseconds", milliseconds.toString)
    else if (durationInSeconds < 60)// 3 seconds, 45 milliseconds, etc.
      Resources("secondsMilliseconds", seconds.toString, milliseconds.toString)
    else if (durationInSeconds < 61)
      Resources("oneMinute")
    else if (durationInSeconds < 62)
      Resources("oneMinuteOneSecond")
    else if (durationInSeconds < 120)
      Resources("oneMinuteSeconds", seconds.toString)
    else if (durationInSeconds < 121)
      Resources("minutes", minutes.toString) // 
    else if (durationInSeconds < 3600 && (durationInSeconds % 60) == 1)
      Resources("minutesOneSecond", minutes.toString)
    else if (durationInSeconds < 3600)
      Resources("minutesSeconds", minutes.toString, seconds.toString)
    else if (durationInSeconds < hoursInSeconds + 1) {
      if (hours == 1)
        Resources("oneHour")
      else
        Resources("hours", hours.toString)
    }
    else if (durationInSeconds < hoursInSeconds + 2) {
      if (hours == 1)
        Resources("oneHourOneSecond")
      else
        Resources("hoursOneSecond", hours.toString)
    }
    else if (durationInSeconds < hoursInSeconds + 60) {
      if (hours == 1)
        Resources("oneHourSeconds", seconds.toString)
      else
        Resources("hoursSeconds", hours.toString, seconds.toString)
    }
    else if (durationInSeconds == hoursInSeconds + 60) {
      if (hours == 1)
        Resources("oneHourOneMinute")
      else
        Resources("hoursOneMinute", hours.toString)
    }
    else if (durationInSeconds == hoursInSeconds + 61) {
      if (hours == 1)
        Resources("oneHourOneMinuteOneSecond")
      else
        Resources("hoursOneMinuteOneSecond", hours.toString)
    }
    else if (durationInSeconds < hoursInSeconds + 120) {
      if (hours == 1)
        Resources("oneHourOneMinuteSeconds", seconds.toString)
      else
        Resources("hoursOneMinuteSeconds", hours.toString, seconds.toString)
    }
    else if (durationInSeconds % 60 == 0) {
      if (hours == 1)
        Resources("oneHourMinutes", minutes.toString)
      else
        Resources("hoursMinutes", hours.toString, minutes.toString)
    }
    else if (durationInMinutes % 60 != 1 && durationInSeconds % 60 == 1) {
      if (hours == 1)
        Resources("oneHourMinutesOneSecond", minutes.toString)
      else
        Resources("hoursMinutesOneSecond", hours.toString, minutes.toString)
    }
    else {
      if (hours == 1)
        Resources("oneHourMinutesSeconds", minutes.toString, seconds.toString)
      else
        Resources("hoursMinutesSeconds", hours.toString, minutes.toString, seconds.toString)
    }
  }
}
