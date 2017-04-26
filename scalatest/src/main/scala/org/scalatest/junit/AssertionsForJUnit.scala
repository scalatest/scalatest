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
package org.scalatest.junit

import org.scalatest._
import _root_.junit.framework.AssertionFailedError
import exceptions.StackDepthExceptionHelper.getStackDepth
import org.scalactic._

/**
 * Trait that contains ScalaTest's basic assertion methods, suitable for use with JUnit.
 *
 * <p>
 * The assertion methods provided in this trait look and behave exactly like the ones in
 * <a href="../Assertions.html"><code>Assertions</code></a>, except instead of throwing
 * <a href="../exceptions/TestFailedException.html"><code>TestFailedException</code></a> they throw
 * <a href="JUnitTestFailedError.html"><code>JUnitTestFailedError</code></a>,
 * which extends <code>junit.framework.AssertionFailedError</code>.
 *
 * <p>
 * JUnit 3 (release 3.8 and earlier) distinguishes between <em>failures</em> and <em>errors</em>.
 * If a test fails because of a failed assertion, that is considered a <em>failure</em>. If a test
 * fails for any other reason, either the test code or the application being tested threw an unexpected
 * exception, that is considered an <em>error</em>. The way JUnit 3 decides whether an exception represents
 * a failure or error is that only thrown <code>junit.framework.AssertionFailedError</code>s are considered
 * failures. Any other exception type is considered an error. The exception type thrown by the JUnit 3
 * assertion methods declared in <code>junit.framework.Assert</code> (such as <code>assertEquals</code>,
 * <code>assertTrue</code>, and <code>fail</code>) is, therefore, <code>AssertionFailedError</code>.
 * </p>
 * 
 * <p>
 * In JUnit 4, <code>AssertionFailedError</code> was made to extend <code>java.lang.AssertionError</code>,
 * and the distinction between failures and errors was essentially dropped. However, some tools that integrate
 * with JUnit carry on this distinction, so even if you are using JUnit 4 you may want to use this
 * <code>AssertionsForJUnit</code> trait instead of plain-old ScalaTest
 * <a href="../Assertions.html"><code>Assertions</code></a>.
 * </p>
 *
 * <p>
 * To use this trait in a JUnit 3 <code>TestCase</code>, you can mix it into your <code>TestCase</code> class, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import junit.framework.TestCase
 * import org.scalatest.junit.AssertionsForJUnit
 *
 * class MyTestCase extends TestCase with AssertionsForJUnit {
 *
 *   def testSomething() {
 *     assert("hi".charAt(1) === 'i')
 *   }
 *
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * You can alternatively import the methods defined in this trait.
 * </p>
 *
 * <pre class="stHighlight">
 * import junit.framework.TestCase
 * import org.scalatest.junit.AssertionsForJUnit._
 *
 * class MyTestCase extends TestCase {
 *
 *   def testSomething() {
 *     assert("hi".charAt(1) === 'i')
 *   }
 *
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * For details on the importing approach, see the documentation
 * for the <a href="AssertionsForJUnit$.html"><code>AssertionsForJUnit</code> companion object</a>.
 * For the details on the <code>AssertionsForJUnit</code> syntax, see the Scaladoc documentation for
 * <a href="../Assertions.html"><code>org.scalatest.Assertions</code></a>
 * </p>
 *
 * @author Bill Venners
 */
trait AssertionsForJUnit extends Assertions {

  private[scalatest] override def newAssertionFailedException(optionalMessage: Option[String], optionalCause: Option[Throwable], pos: source.Position): Throwable = {
    new JUnitTestFailedError(optionalMessage, optionalCause, pos, None)
  }

  import scala.language.experimental.macros

  override def assert(condition: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Assertion = macro AssertionsForJUnitMacro.assert

  override def assert(condition: Boolean, clue: Any)(implicit prettifier: Prettifier, pos: source.Position): Assertion = macro AssertionsForJUnitMacro.assertWithClue

  override def assume(condition: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Assertion = macro AssertionsForJUnitMacro.assume

  override def assume(condition: Boolean, clue: Any)(implicit prettifier: Prettifier, pos: source.Position): Assertion = macro AssertionsForJUnitMacro.assumeWithClue
  
 /*
  private[scalatest] override def newAssertionFailedException(optionalMessage: Option[Any], optionalCause: Option[Throwable], stackDepth: Int): Throwable = {

    val assertionFailedError =
      optionalMessage match {
        case None => new AssertionFailedError
        case Some(message) => new AssertionFailedError(message.toString)
      }

    for (cause <- optionalCause)
      assertionFailedError.initCause(cause)
      
    assertionFailedError
  }  */
}

/**
 * Companion object that facilitates the importing of <code>AssertionsForJUnit</code> members as 
 * an alternative to mixing it in. One use case is to import <code>AssertionsForJUnit</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre>
 * $ scala -cp junit3.8.2/junit.jar:../target/jar_contents 
 * Welcome to Scala version 2.7.5.final (Java HotSpot(TM) Client VM, Java 1.5.0_16).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala> import org.scalatest.junit.AssertionsForJUnit._
 * import org.scalatest.junit.AssertionsForJUnit._
 *
 * scala> assert(1 === 2)
 * junit.framework.AssertionFailedError: 1 did not equal 2
 * 	at org.scalatest.junit.AssertionsForJUnit$class.assert(AssertionsForJUnit.scala:353)
 * 	at org.scalatest.junit.AssertionsForJUnit$.assert(AssertionsForJUnit.scala:672)
 * 	at .<init>(<console>:7)
 * 	at .<clinit>(<console>)
 * 	at RequestResult$.<init>(<console>:3)
 * 	at RequestResult$.<clinit>(<console>)
 * 	at RequestResult$result(<consol...
 * scala> expect(3) { 1 + 3 }
 * junit.framework.AssertionFailedError: Expected 3, but got 4
 * 	at org.scalatest.junit.AssertionsForJUnit$class.expect(AssertionsForJUnit.scala:563)
 * 	at org.scalatest.junit.AssertionsForJUnit$.expect(AssertionsForJUnit.scala:672)
 * 	at .<init>(<console>:7)
 * 	at .<clinit>(<console>)
 * 	at RequestResult$.<init>(<console>:3)
 * 	at RequestResult$.<clinit>(<console>)
 * 	at RequestResult$result(<co...
 * scala> val caught = intercept[StringIndexOutOfBoundsException] { "hi".charAt(-1) }
 * caught: StringIndexOutOfBoundsException = java.lang.StringIndexOutOfBoundsException: String index out of range: -1
 * </pre>
 *
 * @author Bill Venners
 */
object AssertionsForJUnit extends AssertionsForJUnit {

  import Requirements._

  /**
    * Helper class used by code generated by the <code>assert</code> macro.
    */
  class AssertionsHelper {

    private def append(currentMessage: Option[String], clue: Any) = {
      val clueStr = clue.toString
      if (clueStr.isEmpty)
        currentMessage
      else {
        currentMessage match {
          case Some(msg) =>
            // clue.toString.head is guaranteed to work, because the previous if check that clue.toString != ""
            val firstChar = clueStr.head
            if (firstChar.isWhitespace || firstChar == '.' || firstChar == ',' || firstChar == ';')
              Some(msg + clueStr)
            else
              Some(msg + " " + clueStr)
          case None => Some(clueStr)
        }
      }
    }

    /**
      * Assert that the passed in <code>Bool</code> is <code>true</code>, else fail with <code>TestFailedException</code>.
      *
      * @param bool the <code>Bool</code> to assert for
      * @param clue optional clue to be included in <code>TestFailedException</code>'s error message when assertion failed
      */
    def macroAssert(bool: Bool, clue: Any, prettifier: Prettifier, pos: source.Position): Assertion = {
      requireNonNull(clue)(prettifier, pos)
      if (!bool.value) {
        val failureMessage = if (Bool.isSimpleWithoutExpressionText(bool)) None else Some(bool.failureMessage)
        throw newAssertionFailedException(append(failureMessage, clue), None, pos)
      }
      Succeeded
    }

    /**
      * Assume that the passed in <code>Bool</code> is <code>true</code>, else throw <code>TestCanceledException</code>.
      *
      * @param bool the <code>Bool</code> to assume for
      * @param clue optional clue to be included in <code>TestCanceledException</code>'s error message when assertion failed
      */
    def macroAssume(bool: Bool, clue: Any, prettifier: Prettifier, pos: source.Position): Assertion = {
      requireNonNull(clue)(prettifier, pos)
      if (!bool.value) {
        val failureMessage = if (Bool.isSimpleWithoutExpressionText(bool)) None else Some(bool.failureMessage)
        throw newTestCanceledException(append(failureMessage, clue), None, pos)
      }
      Succeeded
    }
  }

  /**
    * Helper instance used by code generated by macro assertion.
    */
  val assertionsHelper = new AssertionsHelper

}
