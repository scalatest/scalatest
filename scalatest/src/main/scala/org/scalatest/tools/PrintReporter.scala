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

import org.scalatest._
// SKIP-SCALATESTJS-START
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
// SKIP-SCALATESTJS-END
import java.io.OutputStreamWriter
import java.io.OutputStream
import java.io.PrintWriter
import org.scalatest.events._
import PrintReporter._

/**
 * A <code>Reporter</code> that prints test status information to
 * a <code>Writer</code>, <code>OutputStream</code>, or file.
 *
 * @author Bill Venners
 */
private[scalatest] abstract class PrintReporter(
  pw: PrintWriter,
  presentAllDurations: Boolean,
  presentInColor: Boolean,
  presentShortStackTraces: Boolean,
  presentFullStackTraces: Boolean,
  presentUnformatted: Boolean,
  presentReminder: Boolean,
  presentReminderWithShortStackTraces: Boolean,
  presentReminderWithFullStackTraces: Boolean,
  presentReminderWithoutCanceledTests: Boolean
) extends StringReporter(
  presentAllDurations,
  presentInColor,
  presentShortStackTraces,
  presentFullStackTraces,
  presentUnformatted,
  presentReminder,
  presentReminderWithShortStackTraces,
  presentReminderWithFullStackTraces,
  presentReminderWithoutCanceledTests
) {

  /**
  * Construct a <code>PrintReporter</code> with passed
  * <code>OutputStream</code>. Information about events reported to instances of this
  * class will be written to the <code>OutputStream</code> using the
  * default character encoding.
  *
  * @param os the <code>OutputStream</code> to which to print reported info
  * @throws NullPointerException if passed <code>os</code> reference is <code>null</code>
  */
  def this( // Used by subclasses StandardOutReporter and StandardErrReporter
    os: OutputStream,
    presentAllDurations: Boolean,
    presentInColor: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    presentUnformatted: Boolean,
    presentReminder: Boolean,
    presentReminderWithShortStackTraces: Boolean,
    presentReminderWithFullStackTraces: Boolean,
    presentReminderWithoutCanceledTests: Boolean
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
      presentUnformatted,
      presentReminder,
      presentReminderWithShortStackTraces,
      presentReminderWithFullStackTraces,
      presentReminderWithoutCanceledTests
    )

  // SKIP-SCALATESTJS-START
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
  def this( // Used by subclass FileReporter
    filename: String,
    presentAllDurations: Boolean,
    presentInColor: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean,
    presentUnformatted: Boolean,
    presentReminder: Boolean,
    presentReminderWithShortStackTraces: Boolean,
    presentReminderWithFullStackTraces: Boolean,
    presentReminderWithoutCanceledTests: Boolean
  ) =
    this(
      new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(filename)), BufferSize)),
      presentAllDurations,
      presentInColor,
      presentShortStackTraces,
      presentFullStackTraces,
      presentUnformatted,
      presentReminder,
      presentReminderWithShortStackTraces,
      presentReminderWithFullStackTraces,
      presentReminderWithoutCanceledTests
    )

  // SKIP-SCALATESTJS-END

  protected def printPossiblyInColor(fragment: Fragment) {
    pw.println(fragment.toPossiblyColoredText(presentInColor))
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

  // We subtract one from test reports because we add "- " in front, so if one is actually zero, it will come here as -1
  // private def indent(s: String, times: Int) = if (times <= 0) s else ("  " * times) + s

  // Stupid properties file won't let me put spaces at the beginning of a property
  // "  {0}" comes out as "{0}", so I can't do indenting in a localizable way. For now
  // just indent two space to the left.  //  if (times <= 0) s 
  //  else Resources.indentOnce(indent(s, times - 1))
}

private[scalatest] object PrintReporter {
  final val BufferSize = 4096
}
