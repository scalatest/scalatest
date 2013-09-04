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
import exceptions.TestFailedException
import exceptions.TestRegistrationClosedException
import exceptions.TestRegistrationClosedException
import exceptions.NotAllowedException
import exceptions.DuplicateTestNameException

/**
 * Trait providing class <code>Checkpoint</code>, which enables multiple assertions
 * to be performed within a test, with any failures accumulated and reported
 * together at the end of the test.
 *
 * <p>
 * Because ScalaTest uses exceptions to signal failed assertions, normally execution
 * of a test will stop as soon as the first failed assertion is encountered. Trait
 * <code>Checkpoints</code> provides an option when you want to continue executing
 * the remainder of the test body, or part of it, even if an assertion has already failed in that test.
 * </p>
 * <p>
 * To use a <code>Checkpoint</code> (once you've mixed in or imported the members of trait
 * <code>Checkpoints</code>), you first need to create one, like this:
 * </p>
 *
 * <pre>
 * val cp = new Checkpoint
 * </pre>
 *
 * <p>
 * Then give the <code>Checkpoint</code> assertions to execute by passing them (via a by-name parameter)
 * to its <code>apply</code> method, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val (x, y) = (1, 2)
 * cp { x should be &lt; 0 }
 * cp { y should be &gt; 9 }
 * </pre>
 *
 * <p>
 * Both of the above assertions will fail, but it won't be reported yet. The <code>Checkpoint</code> will execute them
 * right away, each time its <code>apply</code> method is invoked. But it will catch the <code>TestFailedExceptions</code> and
 * save them, only reporting them later when <code>reportAll</code> is invoked. Thus, at the end of the test, you must call
 * <code>reportAll</code>, like this:
 * </p>
 *
 * <pre>
 * cp.reportAll()
 * </pre>
 * 
 * <p>
 * This <code>reportAll</code> invocation will complete abruptly with a <code>TestFailedException</code> whose message
 * includes the message, source file, and line number of each of the checkpointed assertions that previously failed. For example:
 * </p>
 *
 * <pre>
 * 1 was not less than 0 (in Checkpoint) at ExampleSpec.scala:12
 * 2 was not greater than 9 (in Checkpoint) at ExampleSpec.scala:13
 * </pre>
 *
 * <p>
 * Make sure you invoke <code>reportAll</code> before the test completes, otherwise any failures that were detected by the
 * <code>Checkpoint</code> will not be reported.
 * </p>
 *
 * <p>
 * Note that a <code>Checkpoint</code> will catch and record for later reporting (via <code>reportAll</code>) exceptions that mix in <code>StackDepth</code>
 * except for <code>TestCanceledException</code>, <code>TestRegistrationClosedException</code>, <code>NotAllowedException</code>,
 * and <code>DuplicateTestNameException</code>. If a block of code passed to a <code>Checkpoint</code>'s <code>apply</code> method completes
 * abruptly with any of the <code>StackDepth</code> exceptions in the previous list, or any non-<code>StackDepth</code> exception, that invocation
 * of the <code>apply</code> method will complete abruptly with the same exception immediately. Unless you put <code>reportAll</code> in a finally
 * clause and handle this case, such an unexpected exception will cause you to lose any information about assertions that failed earlier in the test and were
 * recorded by the <code>Checkpoint</code>.
 * </p>
 *
 * @author Bill Venners
 * @author George Berger
 */
trait Checkpoints {

  /**
   * Class that allows multiple assertions to be performed within a test, with any
   * failures accumulated and reported together at the end of the test.
   * 
   * <p>
   * See the main documentation for trait <code>Checkpoints</code> for more information and an example.
   * </p>
   */
  class Checkpoint {
    private final val failures: ConcurrentLinkedQueue[Throwable with StackDepth] =
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
     * Executes the passed block of code and catches and records for later reporting (via <code>reportAll</code>) any exceptions that mix in <code>StackDepth</code>
     * except for <code>TestCanceledException</code>, <code>TestRegistrationClosedException </code>, <code>NotAllowedException </code>,
     * and <code>DuplicateTestNameException </code>.
     * 
     * <p>
     * If the block of code completes abruptly with any of the <code>StackDepth</code> exceptions in the
     * previous list, or any non-<code>StackDepth</code> exception, that invocation of this <code>apply</code> method will complete abruptly
     * with the same exception.
     * </p>
     *
     * @param f the block of code, likely containing one or more assertions, to execute
     */
    def apply(f: => Unit) {
      try {
        f
      }
      catch {
        case e: TestCanceledException => throw e
        case e: TestRegistrationClosedException => throw e
        case e: NotAllowedException => throw e
        case e: DuplicateTestNameException => throw e
        case e: StackDepth  => failures.add(e)
        case e: Throwable => throw e
      }
    }

    /**
     * If any failures were caught by checkpoints, throws a <code>TestFailedException</code>
     * whose detail message lists the failure messages and line numbers from each of the
     * failed checkpoints.
     */
    def reportAll() {
      if (!failures.isEmpty) {
        val failMessages =
          for (failure <- failures.asScala)
          yield failure.getMessage + " " + Resources("atCheckpointAt") + " " + getFailLine(failure)
        throw new TestFailedException(failMessages.mkString("\n"), 1)
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
