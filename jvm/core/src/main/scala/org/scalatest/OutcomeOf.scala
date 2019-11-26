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
 * Trait that contains the <code>outcomeOf</code> method, which executes a passed code block and
 * transforms the outcome into an <a href="Outcome.html"><code>Outcome</code></a>, using the
 * same mechanism used by ScalaTest to produce an <code>Outcome</code> when executing
 * a test.
 *
 * <p>
 * For an example of <code>outcomeOf</code> in action, see the documentation for
 * class <a href="prop/TableFor2.html"><code>TableFor2</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
trait OutcomeOf {

  /**
   * Executes the supplied code (a by-name parameter) and returns an <code>Outcome</code>.
   *
   * <p>
   * Because <code>Error</code>s are used to denote serious errors, ScalaTest does not always treat a test that completes abruptly with
   * an <code>Error</code> as a test failure, but sometimes as
   * an indication that serious problems have arisen that should cause the run to abort, and the <code>outcomeOf</code> method exhibits
   * the same behavior. For example, if a test completes abruptly
   * with an <code>OutOfMemoryError</code>, it will not be reported as a test failure, but will instead cause the run to abort.
   * Because not everyone uses <code>Error</code>s only to represent serious problems, however, ScalaTest only behaves this way
   * for the following exception types (and their subclasses):
   * </p>
   *
   * <ul>
   * <li><code>java.lang.annotation.AnnotationFormatError</code></li>
   * <li><code>java.awt.AWTError</code></li>
   * <li><code>java.nio.charset.CoderMalfunctionError</code></li>
   * <li><code>javax.xml.parsers.FactoryConfigurationError</code></li>
   * <li><code>java.lang.LinkageError</code></li>
   * <li><code>java.lang.ThreadDeath</code></li>
   * <li><code>javax.xml.transform.TransformerFactoryConfigurationError</code></li>
   * <li><code>java.lang.VirtualMachineError</code></li>
   * </ul>
   *
   * <p>
   * The previous list includes all <code>Error</code>s that exist as part of Java 1.5 API, excluding <code>java.lang.AssertionError</code>.
   * If the code supplied to <code>outcomeOf</code> completes abruptly in one of the errors in the previous list, <code>outcomeOf</code>
   * will not return an <code>Outcome</code>, but rather will complete abruptly with the same exception.
   * will wrap any other exception thrown by the supplied code in a <code>Some</code> and return it.
   * </p>
   *
   * <p>
   * The <code>outcomeOf</code> method (and ScalaTest in general) does treat a thrown <code>AssertionError</code> as an indication of a test failure and therefore
   * returns a <code>Failed</code> wrapping the <code>AssertionError</code>. In addition, any other <code>Error</code> that is not an instance of a
   * type mentioned in the previous list will be caught by the <code>outcomeOf</code> and transformed as follows:
   *
   * <ul>
   * <li><a href="exceptions/TestPendingException.html"><code>TestPendingException</code></a></li>: <a href="Pending$.html"><code>Pending</code></a>
   * <li><a href="exceptions/TestCanceledException.html"><code>TestCanceledException</code></a></li>: <a href="Canceled.html"><code>Canceled</code></a>
   * <li>otherwise: <a href="Failed.html"><code>Failed</code></a>
   * </ul>
   * </p>
   *
   * <p>
   * If the code block completes normally (<em>i.e.</em>, it doesn't throw any exception), <code>outcomeOf</code> results in <a href="Succeeded$.html"><code>Succeeded</code></a>.
   * </p>
   *
   * @param f a block of code to execute
   * @return an <code>Outcome</code> representing the outcome of executing the block of code
   */
  def outcomeOf(f: => Any): Outcome = {
    try {                                         
      f                                           
      Succeeded
    }                                             
    catch {                                       
      case ex: exceptions.TestCanceledException => Canceled(ex)                           
      case _: exceptions.TestPendingException => Pending
      case tfe: exceptions.TestFailedException => Failed(tfe)
      case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)                           
    }
  }
}

/**
 * Companion object that facilitates the importing of <code>OutcomeOf</code>'s method as 
 * an alternative to mixing it in. One use case is to import <code>OutcomeOf</code>'s method so you can use
 * it in the Scala interpreter.
 *
 * @author Bill Venners
 */
object OutcomeOf extends OutcomeOf

