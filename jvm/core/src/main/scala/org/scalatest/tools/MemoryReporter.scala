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

import org.scalatest.Reporter
import org.scalatest.events.TestFailed
import org.scalatest.events.TestCanceled
import org.scalatest.events.RunCompleted
import org.scalatest.events.SuiteAborted
import org.scalatest.events.Event

import scala.collection.mutable

/**
 * A <code>Reporter</code> that writes a file listing any tests that
 * failed or canceled during a run, so they can be rerun if desired.
 */
private[scalatest] class MemoryReporter(outputFile: String)
extends Reporter
{
  private val mementos = mutable.Set.empty[Memento]

  //
  // Records TestFailed, TestCanceled, and SuiteAborted events.
  // Generates output file listing the events upon receipt of a
  // RunCompleted event, to enable rerunning of just those tests
  // next time.
  //
  def apply(event: Event): Unit = {
    event match {
      case e: TestFailed   => mementos += Memento(e)
      case e: TestCanceled => mementos += Memento(e)
      case e: SuiteAborted => mementos += Memento(e)
      case _: RunCompleted => Memento.writeToFile(outputFile, mementos.toSet)
      case _ =>
    }
  }
}
