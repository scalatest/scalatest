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
package org.scalatest

import java.io.PrintStream
import org.scalatest.events.Event
import DispatchReporter.propagateDispose

/**
 * This report just catches exceptions thrown by the passed report and
 * prints info about them to the standard error stream. This is because people
 * can pass in custom reports that may have bugs. I want the test run to continue
 * in case one of them throws back an exception.
 *
 * @author Bill Venners
 */
private[scalatest] trait CatchReporter extends ResourcefulReporter {

  val out: PrintStream
  
  def apply(event: Event) {
    try {
      doApply(event)
    }
    catch {
      case e: Exception => 
        val stringToPrint = Resources("reporterThrew", event)
        out.println(stringToPrint)
        e.printStackTrace(out)
    }
  }

  def dispose() {
    try {
      doDispose()
    }
    catch {
      case e: Exception =>
        val stringToPrint = Resources("reporterDisposeThrew")
        out.println(stringToPrint)
        e.printStackTrace(out)
    }
  }
  
  protected def doApply(event: Event)
  protected def doDispose()
}

// Out is not even being used. Can I not just ignore the whole concept?
private[scalatest] class WrapperCatchReporter(reporter: Reporter, val out: PrintStream) extends CatchReporter {
  
  def this(reporter: Reporter) = this(reporter, System.err)
  
  private val report = reporter
  
  def doApply(event: Event) {
    report(event)
  }
  
  def doDispose() {
    propagateDispose(reporter)
  }
}
