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
package org.scalatest

import org.scalatest.junit.JUnitTestFailedError
import java.util.concurrent.atomic.AtomicReference

/**
 * Trait to define class Checkpoint, which allows multiple failure
 * conditions within a test to be collected prior to failure being
 * reported.
 *
 * <p>
 * Uses the "selfless trait" pattern to let users either mix in the trait or
 * import its members.
 * </p>
 */
trait Checkpoints {

  /**
   * Class to allow multiple failure conditions within a test to be collected
   * prior to failure being reported.
   *
   * <p>
   * E.g.:
   * </p>
   *
   * <pre class="stHighlight">
   * class MySpec extends FunSpec with Checkpoints
   * 
   *   // note no s, trait Checkpoints defines class Checkpoint
   *   val cp = new Checkpoint
   * 
   *   // ...
   *   cp { a should be > 9 } // invoking apply on Checkpoint
   *   // i.e., same as:
   *   // cp apply { a should be > 9 }
   *   // ...
   *   cp { b should === 22 }
   * 
   *   // ...
   *   cp { c should be > 10 }
   * 
   *   cp.reportAll()
   * </pre>
   * 
   * <p>
   * If checkpoints aren't used, as soon as a failure happens, a
   * TestFailedException is thrown and the rest of the test is not
   * executed. So if a equaled 3 in the above example, the TFE would
   * be thrown as of the first assertion, and the user wouldn't know
   * anything about b or c. By using the checkpoint, if c is also 3,
   * the user will find out that both 3 was not greater than 9, with
   * a pointer to the line number of the first assertion, and 3 was
   * not greater than 10, with a pointer to the line number of the
   * third assertion.
   * <p>
   */
  class Checkpoint {
    private val fails: AtomicReference[Vector[Throwable]] =
      new AtomicReference(Vector())

    //
    // Appends specified Throwable to fails Vector.
    //
    private def appendToFails(e: Throwable) {
      val got: Vector[Throwable] = fails.get

      if (!fails.compareAndSet(got, got :+ e))
        appendToFails(e)
    }

    //
    // Returns a string containing the file name and line number where
    // the test failure occurred, e.g. "HelloSuite.scala:18".
    //
    // Accepts a Throwable, but TestFailedExceptions and
    // JUnitTestFailedErrors both mix in the StackDepth trait, so
    // its failedCodeFileNameAndLineNumberString method is used
    // to retrieve the information.
    //
    private def getFailLine(t: Throwable): String = {
      t match {
        case e: StackDepth =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(failLine) => failLine
            case None => "unknown line number"
          }
        case _ => "" // shouldn't happen
      }
    }

    /**
     * Catches TestFailedExceptions or JUnitTestFailedErrors thrown
     * by the test condition and stores them so they can be reported
     * later by a call to reportAll().
     */
    def apply(f: => Unit) {
      try {
        f
      }
      catch {
        case e: TestFailedException  => appendToFails(e)
        case e: JUnitTestFailedError => appendToFails(e)
        case e: Throwable => throw e
      }
    }

    /**
     * If any failures were caught by checkpoints, throws an Exception
     * containing the error messages and line numbers from each of the
     * failed checkpoints.
     */
    def reportAll() {
      if (fails.get.size > 0) {
        val failMessages =
          for (fail <- fails.get)
            yield
              fail.getMessage + " " + Resources("atCheckpointAt") + " " +
              getFailLine(fail)

        fails.get.head match {
          case e: TestFailedException =>
            throw new TestFailedException(failMessages.mkString("\n"), 1)
          case e: JUnitTestFailedError =>
            throw new JUnitTestFailedError(failMessages.mkString("\n"), 1)
        }
      }
    }
  }
}

/**
 * Companion object to Checkpoints.
 */
object Checkpoints extends Checkpoints
