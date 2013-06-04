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

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.JavaConverters._
import exceptions.TestCanceledException

/**
 * Trait defining class <code>Checkpoint</code>, which allows multiple assertions
 * to be performed within a test, with any failures accumulated and reported
 * together at the end of the test.
 */
trait Checkpoints {

  /**
   * Class that allows multiple assertions to be performed within a test, with any
   * failures accumulated and reported together at the end of the test.
   *
   * <p>
   * To use a <code>Checkpoint</code>, you first need to create one:
   * </p>
   *
   * <pre class="stHighlight">
   * class ExampleSpec extends FlatSpec with Checkpoints {
   * 
   *   "This example" should "clarify how to use Checkpoints" in {
   *
   *     // note no s, trait Checkpoints defines class Checkpoint
   *     val cp = new Checkpoint
   * 
   *     val (a, b, c) = (1, 2, 3)
   *     cp { a should be &lt; 9 }
   *     // The above line invokes apply on the Checkpoint.
   *     // I.e., it means the same as:
   *     // cp apply { a should be &gt; 9 }
   *     // Note that a is *not* less than 
   *
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
    private val failures: ConcurrentLinkedQueue[Throwable with StackDepth] =
      new ConcurrentLinkedQueue

    //
    // Returns a string containing the file name and line number where
    // the test failure occurred, e.g. "HelloSuite.scala:18".
    //
    private def getFailLine(t: Throwable with StackDepth): String =
      t.failedCodeFileNameAndLineNumberString match {
        case Some(failLine) => failLine
        case None => "unknown line number"
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
        case e: TestCanceledException => throw e
        case e: StackDepth  => failures.add(e)
        case e: Throwable => throw e
      }
    }

    /**
     * If any failures were caught by checkpoints, throws an Exception
     * containing the error messages and line numbers from each of the
     * failed checkpoints.
     */
    def reportAll() {
      if (!failures.isEmpty) {
        val failMessages =
          failures.asScala.
            map(f =>
              f.getMessage + " " + Resources("atCheckpointAt") + " " +
              getFailLine(f)).
            mkString("\n")

        throw new TestFailedException(failMessages, 1)
      }
    }
  }
}

/**
 * Companion object that facilitates the importing the members of trait <code>Checkpoints</code> as 
 * an alternative to mixing it in. One use case is to import <code>Checkpoints</code> so you can use
 * it in the Scala interpreter.
 *
 * @author Bill Venners
 */
object Checkpoints extends Checkpoints
