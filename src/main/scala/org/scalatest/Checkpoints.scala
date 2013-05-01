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
    var fails: List[TestFailedException] = List()

    private def getFailLine(e: TestFailedException): String =
      e.failedCodeFileNameAndLineNumberString match {
        case Some(failLine) => failLine
        case None => "line number unknown"
      }
    
    def apply(f: => Unit) {
      try {
        f
      }
      catch {
        case e: TestFailedException => fails ::= e
        case e: Throwable => throw e
      }
    }

    def reportAll() {
      if (fails.size > 0) {
        val failMessages = 
          for (fail <- fails.reverse)
            yield fail.getMessage + " at checkpoint at " + getFailLine(fail)

        throw new TestFailedException(failMessages.mkString("\n"), 1)
      }
    }
  }
}

/**
 * Companion object to Checkpoints.
 */
object Checkpoints extends Checkpoints
