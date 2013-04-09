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
import java.io.IOException

/**
 * A <code>Reporter</code> that prints test status information to a file.
 *
 * The primary constructor creates a <code>FileReporter</code> with passed
 * <code>String</code> file name. Information about events reported to instances of this
 * class will be written to the specified file using the
 * default character encoding.
 *
 * @param filename the <code>String</code> name of the file to which to print reported info
 * @exception NullPointerException if passed <code>file</code> reference is <code>null</code>
 * @exception IOException if unable to open the specified file for writing
 *
 * @author Bill Venners
 */
private[scalatest] class FileReporter(val filename: String, presentAllDurations: Boolean,
    presentInColor: Boolean, presentShortStackTraces: Boolean, presentFullStackTraces: Boolean,
    presentUnformatted: Boolean)
    extends PrintReporter(filename, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces,
    presentUnformatted) {
  def this(filename: String) = this(filename, false, true, false, false, false)
}
