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

/**
 * <b>Trait <code>FailureOf</code> has been deprecated and will be removed in a future version of ScalaTest.
 * Instead of the <code>failureOf</code> method provided by this trait, please use the <code>outcomeOf</code> method
 * provided by trait <code>OutcomeOf</code> instead.</b>
 *
 * @author Bill Venners
 */
@deprecated("Please use OutcomeOf instead")
trait FailureOf {

  /**
   * Executes the supplied code (a by-name parameter) and returns in an optional throwable indicating whether the passed
   * expression failed (threw an exception that would normally cause a test to fail) or succeeded (did not throw any exception).
   *
   * <p>
   * Because <code>Error</code>s are used to denote serious errors, trait <code>Suite</code> and its subtypes in the
   * ScalaTest API do not always treat a test that completes abruptly with an <code>Error</code> as a test failure, but sometimes as
   * an indication that serious problems have arisen that should cause the run to abort. For example, if a test completes abruptly
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
   * The previous list includes all <code>Error</code>s that exist as part of Java 1.5 API, excluding <code>java.lang.AssertionError</code>. ScalaTest
   * does treat a thrown <code>AssertionError</code> as an indication of a test failure. In addition, any other <code>Error</code> that is not an instance of a
   * type mentioned in the previous list will be caught by the <code>Suite</code> traits in the ScalaTest API and reported as the cause of a test failure. 
   * </p>
   *
   * <p>
   * If the code supplied to <code>failureOf</code> completes abruptly in one of the errors in the previous list, <code>failureOf</code>
   * will not return it wrapped in an option, but rather will complete abruptly with the same exception. The <code>failureOf</code> method
   * will wrap any other exception thrown by the supplied code in a <code>Some</code> and return it.
   * </p>
   */
  def failureOf(f: => Unit): Option[Throwable] = {
    
    try {                                         
      f                                           
      None                                        
    }                                             
    catch {                                       
      case e: Throwable =>
        if (!Suite.anExceptionThatShouldCauseAnAbort(e))
          Some(e)                           
        else
          throw e
    }
  }
}

/**
 * Companion object that facilitates the importing of <code>FailureOf</code>'s method as 
 * an alternative to mixing it in. One use case is to import <code>FailureOf</code>'s method so you can use
 * it in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala version 2.8.0.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_22).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * &nbsp;
 * scala> import org.scalatest.Assertions._
 * import org.scalatest.Assertions._
 * &nbsp;
 * scala> import org.scalatest.FailureOf._
 * import org.scalatest.FailureOf._
 * &nbsp;
 * scala> failureOf { assert(1 + 1 === 2) }
 * res0: Option[Throwable] = None
 * &nbsp;
 * scala> failureOf { assert(1 + 1 === 3) }
 * res1: Option[Throwable] = Some(org.scalatest.TestFailedException: 2 did not equal 3)
 * <pre>
 *
 * @author Bill Venners
 */
object FailureOf extends FailureOf
