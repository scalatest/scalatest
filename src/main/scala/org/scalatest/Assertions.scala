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

import exceptions.TestCanceledException
import scala.reflect.Manifest
import Assertions.areEqualComparingArraysStructurally
import org.scalactic.TripleEquals
import exceptions.StackDepthExceptionHelper.getStackDepthFun
import exceptions.StackDepthException.toExceptionFunction
import Assertions.NormalResult
import org.scalactic.{Prettifier, Bool}

/**
 * Trait that contains ScalaTest's basic assertion methods.
 *
 * <p>
 * You can use the assertions provided by this trait in any ScalaTest <code>Suite</code>,
 * because <a href="Suite.html"><code>Suite</code></a>
 * mixes in this trait. This trait is designed to be used independently of anything else in ScalaTest, though, so you
 * can mix it into anything. (You can alternatively import the methods defined in this trait. For details, see the documentation
 * for the <a href="Assertions$.html"><code>Assertions</code> companion object</a>.
 * </p>
 *
 * <p>
 * In any Scala program, you can write assertions by invoking <code>assert</code> and passing in a <code>Boolean</code> expression,
 * such as:
 * </p>
 *
 * <pre class="stHighlight">
 * val left = 2
 * val right = 1
 * assert(left == right)
 * </pre>
 *
 * <p>
 * If the passed expression is <code>true</code>, <code>assert</code> will return normally. If <code>false</code>,
 * Scala's <code>assert</code> will complete abruptly with an <code>AssertionError</code>. This behavior is provided by
 * the <code>assert</code> method defined in object <code>Predef</code>, whose members are implicitly imported into every
 * Scala source file. This <code>Assertions</code> trait defines another <code>assert</code> method that hides the
 * one in <code>Predef</code>. It behaves the same, except that if <code>false</code> is passed it throws
 * <a href="exceptions/TestFailedException.html"><code>TestFailedException</code></a> instead of <code>AssertionError</code>. 
 * Why? Because unlike <code>AssertionError</code>, <code>TestFailedException</code> carries information about exactly
 * which item in the stack trace represents
 * the line of test code that failed, which can help users more quickly find an offending line of code in a failing test.
 * <p>
 *
 * <p>
 * If you pass the previous <code>Boolean</code> expression, <code>left == right</code> to <code>assert</code> in a ScalaTest test,
 * a failure will be reported that, because <code>assert</code> is implemented as a macro,
 * includes reporting the left and right values.
 *
 * For example, given the same code as above but using ScalaTest assertions:
 *
 * <pre class="stHighlight">
 * import org.scalatest.Assertions._
 * val left = 2
 * val right = 1
 * assert(left == right)
 * </pre>
 *
 * <p>
 * The detail message in the thrown <code>TestFailedException</code> from this <code>assert</code>
 * will be: "2 did not equal 1".
 * </p>
 *
 * <p>
 * ScalaTest's <code>assert</code> macro works by recognizing patterns in the AST of the expression passed to <code>assert</code> and,
 * for a finite set of common expressions, giving an error message that an equivalent ScalaTest matcher
 * expression would give. Here are some examples, where <code>a</code> is 1, <code>b</code> is 2, <code>c</code> is 3, <code>d</code>
 * is 4, <code>xs</code> is <code>List(a, b, c)</code>, and <code>num</code> is 1.0:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(a == b || c &gt;= d)
 * // Error message: 1 did not equal 2, and 3 was not greater than or equal to 4
 *
 * assert(xs.exists(_ == 4))
 * // Error message: List(1, 2, 3) did not contain 4
 *
 * assert("hello".startsWith("h") &amp;&amp; "goodbye".endsWith("y"))
 * // Error message: "hello" started with "h", but "goodbye" did not end with "y"
 *
 * assert(num.isInstanceOf[Int])
 * // Error message: 1.0 was not instance of scala.Int
 *
 * assert(Some(2).isEmpty)
 * // Error message: Some(2) was not empty
 * </pre>
 * 
 * <p>
 * For expressions that are not recognized, the macro currently prints out a string
 * representation of the (desugared) AST and adds <code>"was false"</code>. Here are some examples of
 * error messages for unrecognized expressions:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(None.isDefined)
 * // Error message: scala.None.isDefined was false
 *
 * assert(xs.exists(i =&gt; i &gt; 10))
 * // Error message: xs.exists(((i: Int) =&gt; i.&gt;(10))) was false
 * </pre>
 * 
 * <p>
 * You can augment the standard error message by providing a <code>String</code> as a second argument
 * to <code>assert</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val attempted = 2
 * assert(attempted == 1, "Execution was attempted " + left + " times instead of 1 time")
 * </pre>
 *
 * <p>
 * Using this form of <code>assert</code>, the failure report will be more specific to your problem domain, thereby
 * helping you debug the problem. This <code>Assertions</code> trait also mixes in the
 * <a href="../scalactic/TripleEquals.html"><code>TripleEquals</code></a>, which gives you a <code>===</code> operator
 * that allows you to customize <a href="../scalactic/Equality.html"><code>Equality</code></a>, perform equality checks with numeric
 * <a href="../scalactic/Tolerance.html"><code>Tolerance</code></a>, and enforce type constraints at compile time with
 * sibling traits <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> and
 * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>.
 * </p>
 *
 * <a name="expectedResults"></a>
 * <h2>Expected results</h2>
 *
 * Although the <code>assert</code> macro provides a natural, readable extension to Scala's <code>assert</code> mechanism that
 * provides good error messages, as the operands become lengthy, the code becomes less readable. In addition, the error messages
 * generated for <code>==</code> and <code>===</code> comparisons
 * don't distinguish between actual and expected values. The operands are just called <code>left</code> and <code>right</code>,
 * because if one were named <code>expected</code> and the other <code>actual</code>, it would be difficult for people to
 * remember which was which. To help with these limitations of assertions, <code>Suite</code> includes a method called <code>assertResult</code> that
 * can be used as an alternative to <code>assert</code>. To use <code>assertResult</code>, you place
 * the expected value in parentheses after <code>assertResult</code>, followed by curly braces containing code
 * that should result in the expected value. For example:
 *
 * <pre class="stHighlight">
 * val a = 5
 * val b = 2
 * assertResult(2) {
 *   a - b
 * }
 * </pre>
 *
 * <p>
 * In this case, the expected value is <code>2</code>, and the code being tested is <code>a - b</code>. This assertion will fail, and
 * the detail message in the <code>TestFailedException</code> will read, "Expected 2, but got 3."
 * </p>
 *
 * <a name="forcingFailures"></a>
 * <h2>Forcing failures</h2>
 *
 * <p>
 * If you just need the test to fail, you can write:
 * </p>
 *
 * <pre class="stHighlight">
 * fail()
 * </pre>
 *
 * <p>
 * Or, if you want the test to fail with a message, write:
 * </p>
 *
 * <pre class="stHighlight">
 * fail("I've got a bad feeling about this")
 * </pre>
 *
 * <a name="interceptedExceptions"></a>
 * <h2>Intercepted exceptions</h2>
 *
 * <p>
 * Sometimes you need to test whether a method throws an expected exception under certain circumstances, such
 * as when invalid arguments are passed to the method. You can do this in the JUnit 3 style, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val s = "hi"
 * try {
 *   s.charAt(-1)
 *   fail()
 * }
 * catch {
 *   case _: IndexOutOfBoundsException =&gt; // Expected, so continue
 * }
 * </pre>
 *
 * <p>
 * If <code>charAt</code> throws <code>IndexOutOfBoundsException</code> as expected, control will transfer
 * to the catch case, which does nothing. If, however, <code>charAt</code> fails to throw an exception,
 * the next statement, <code>fail()</code>, will be run. The <code>fail</code> method always completes abruptly with
 * a <code>TestFailedException</code>, thereby signaling a failed test.
 * </p>
 *
 * <p>
 * To make this common use case easier to express and read, ScalaTest provides an <code>intercept</code>
 * method. You use it like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val s = "hi"
 * intercept[IndexOutOfBoundsException] {
 *   s.charAt(-1)
 * }
 * </pre>
 *
 * <p>
 * This code behaves much like the previous example. If <code>charAt</code> throws an instance of <code>IndexOutOfBoundsException</code>,
 * <code>intercept</code> will return that exception. But if <code>charAt</code> completes normally, or throws a different
 * exception, <code>intercept</code> will complete abruptly with a <code>TestFailedException</code>. <code>intercept</code> returns the
 * caught exception so that you can inspect it further if you wish, for example, to ensure that data contained inside
 * the exception has the expected values.
 * </p>
 *
 * <a name="checkingThatCodeDoesNotCompile"></a>
 * <h2>Checking that a snippet of code does or does not compile</h2>
 * 
 * <p>
 * Often when creating libraries you may wish to ensure that certain arrangements of code that
 * represent potential &ldquo;user errors&rdquo; do not compile, so that your library is more error resistant.
 * ScalaTest's <code>Assertions</code> trait includes the following syntax for that purpose:
 * </p>
 *
 * <pre class="stHighlight">
 * assertDoesNotCompile("val a: String = 1")
 * </pre>
 *
 * <p>
 * If you want to ensure that a snippet of code does not compile because of a type error (as opposed
 * to a syntax error), use:
 * </p>
 *
 * <pre class="stHighlight">
 * assertTypeError("val a: String = 1")
 * </pre>
 *
 * <p>
 * Note that the <code>assertTypeError</code> call will only succeed if the given snippet of code does not
 * compile because of a type error. A syntax error will still result on a thrown <code>TestFailedException</code>.
 * </p>
 *
 * <p>
 * If you want to state that a snippet of code <em>does</em> compile, you can make that
 * more obvious with:
 * </p>
 *
 * <pre class="stHighlight">
 * assertCompiles("val a: Int = 1")
 * </pre>
 *
 * <p>
 * Although the previous three constructs are implemented with macros that determine at compile time whether
 * the snippet of code represented by the string does or does not compile, errors 
 * are reported as test failures at runtime.
 * </p>
 *
 * <a name="assumptions"></a>
 * <h2>Assumptions</h2>
 *
 * <p>
 * Trait <code>Assertions</code> also provides methods that allow you to <em>cancel</em> a test.
 * You would cancel a test if a resource required by the test was unavailable. For example, if a test
 * requires an external database to be online, and it isn't, the test could be canceled to indicate
 * it was unable to run because of the missing database. Such a test <em>assumes</em> a database is
 * available, and you can use the <code>assume</code> method to indicate this at the beginning of
 * the test, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * assume(database.isAvailable)
 * </pre>
 *
 * <p>
 * For each overloaded <code>assert</code> method, trait <code>Assertions</code> provides an
 * overloaded <code>assume</code> method with an identical signature and behavior, except the
 * <code>assume</code> methods throw <a href="exceptions/TestCanceledException.html"><code>TestCanceledException</code></a> whereas the
 * <code>assert</code> methods throw <code>TestFailedException</code>. As with <code>assert</code>,
 * <code>assume</code> hides a Scala method in <code>Predef</code> that performs a similar
 * function, but throws <code>AssertionError</code>. And just as you can with <code>assert</code>,
 * you will get an error message extracted by a macro from the AST passed to <code>assume</code>, and can
 * optionally provide a clue string to augment this error message. Here are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * assume(database.isAvailable, "The database was down again")
 * assume(database.getAllUsers.count === 9)
 * </pre>
 *
 * <a name="forcingCancelations"></a>
 * <h2>Forcing cancelations</h2>
 *
 * <p>
 * For each overloaded <code>fail</code> method, there's a corresponding <code>cancel</code> method
 * with an identical signature and behavior, except the <code>cancel</code> methods throw
 * <code>TestCanceledException</code> whereas the <code>fail</code> methods throw
 * <code>TestFailedException</code>. Thus if you just need to cancel a test, you can write:
 * </p>
 *
 * <pre class="stHighlight">
 * cancel()
 * </pre>
 *
 * <p>
 * If you want to cancel the test with a message, just place the message in the parentheses:
 * </p>
 *
 * <pre class="stHighlight">
 * cancel("Can't run the test because no internet connection was found")
 * </pre>
 *
 * <a name="gettingAClue"></a>
 * <h2>Getting a clue</h2>
 *
 * <p>
 * If you want more information that is provided by default by the methods if this trait,
 * you can supply a "clue" string in one of several ways.
 * The extra information (or "clues") you provide will
 * be included in the detail message of the thrown exception. Both
 * <code>assert</code> and <code>assertResult</code> provide a way for a clue to be
 * included directly, <code>intercept</code> does not.
 * Here's an example of clues provided directly in <code>assert</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(1 + 1 === 3, "this is a clue")
 * </pre>
 *
 * <p>
 * and in <code>assertResult</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * assertResult(3, "this is a clue") { 1 + 1 }
 * </pre>
 *
 * <p>
 * The exceptions thrown by the previous two statements will include the clue
 * string, <code>"this is a clue"</code>, in the exception's detail message.
 * To get the same clue in the detail message of an exception thrown
 * by a failed <code>intercept</code> call requires using <code>withClue</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * withClue("this is a clue") {
 *   intercept[IndexOutOfBoundsException] {
 *     "hi".charAt(-1)
 *   }
 * }
 * </pre>
 *
 * <p>
 * The <code>withClue</code> method will only prepend the clue string to the detail
 * message of exception types that mix in the <code>ModifiableMessage</code> trait.
 * See the documentation for <a href="ModifiableMessage.html"><code>ModifiableMessage</code></a> for more information.
 * If you wish to place a clue string after a block of code, see the documentation for
 * <a href="AppendedClues.html"><code>AppendedClues</code></a>.
 * </p>
 *
 * <p>
 * <em>Note: ScalaTest's <code>assertTypeError</code> construct is in part inspired by the <code>illTyped</code> macro
 * of <a href="https://github.com/milessabin/shapeless" target="_blank">shapeless</a>.</em>
 * </p>
 *
 * @author Bill Venners
 */
trait Assertions extends TripleEquals {

  import language.experimental.macros

  /**
   * Assert that a boolean condition is true.
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>TestFailedException</code>.
   *
   * <p>
   * This method is implemented in terms of a Scala macro that will generate a more helpful error message
   * for expressions of this form:
   * </p>
   *
   * <ul>
   * <li>assert(a == b)</li>
   * <li>assert(a != b)</li>
   * <li>assert(a === b)</li>
   * <li>assert(a !== b)</li>
   * <li>assert(a &gt; b)</li>
   * <li>assert(a &gt;= b)</li>
   * <li>assert(a &lt; b)</li>
   * <li>assert(a &lt;= b)</li>
   * <li>assert(a startsWith "prefix")</li>
   * <li>assert(a endsWith "postfix")</li>
   * <li>assert(a contains "something")</li>
   * <li>assert(a eq b)</li>
   * <li>assert(a ne b)</li>
   * <li>assert(a &gt; 0 &amp;&amp; b &gt; 5)</li>
   * <li>assert(a &gt; 0 || b &gt; 5)</li>
   * <li>assert(a.isEmpty)</li>
   * <li>assert(!a.isEmpty)</li>
   * <li>assert(a.isInstanceOf[String])</li>
   * <li>assert(a.length == 8)</li>
   * <li>assert(a.size == 8)</li>
   * <li>assert(a.exists(_ == 8))</li>
   * </ul>
   *
   * <p>
   * At this time, any other form of expression will get a <code>TestFailedException</code> with message saying the given
   * expression was false.  In the future, we will enhance this macro to give helpful error messages in more situations.
   * In ScalaTest 2.0, however, this behavior was sufficient to allow the <code>===</code> that returns <code>Boolean</code>,
   * not <code>Option[String]</code> to be the default in tests. This makes <code>===</code> consistent between tests and production
   * code. If you have pre-existing code you wrote under ScalaTest 1.x, in which you are expecting<code>===</code> to return an
   * <code>Option[String]</code>, use can get that behavior back by mixing in trait <a href="LegacyTripleEquals.html"><code>LegacyTripleEquals</code></a>.
   * </p>
   *
   * @param condition the boolean condition to assert
   * @throws TestFailedException if the condition is <code>false</code>.
   */
  def assert(condition: Boolean): Unit = macro AssertionsMacro.assert

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
    def macroAssert(bool: Bool, clue: Any) {
      if (clue == null)
        throw new NullPointerException("clue was null")
      if (!bool.value) {
        val failureMessage = if (Bool.isSimpleWithoutExpressionText(bool)) None else Some(bool.failureMessage)
        throw newAssertionFailedException(append(failureMessage, clue), None, "Assertions.scala", "macroAssert", 2)
      }
    }

    /**
     * Assume that the passed in <code>Bool</code> is <code>true</code>, else throw <code>TestCanceledException</code>.
     *
     * @param bool the <code>Bool</code> to assume for
     * @param clue optional clue to be included in <code>TestCanceledException</code>'s error message when assertion failed
     */
    def macroAssume(bool: Bool, clue: Any) {
      if (clue == null)
        throw new NullPointerException("clue was null")
      if (!bool.value) {
        val failureMessage = if (Bool.isSimpleWithoutExpressionText(bool)) None else Some(bool.failureMessage)
        throw newTestCanceledException(append(failureMessage, clue), None, "Assertions.scala", "macroAssume", 2)
      }
    }
  }

  /**
   * Helper instance used by code generated by macro assertion.
   */
  val assertionsHelper = new AssertionsHelper

  private[scalatest] def newAssertionFailedException(optionalMessage: Option[Any], optionalCause: Option[Throwable], stackDepth: Int): Throwable =
    (optionalMessage, optionalCause) match {
      case (None, None) => new TestFailedException(stackDepth)
      case (None, Some(cause)) => new TestFailedException(cause, stackDepth)
      case (Some(message), None) => new TestFailedException(message.toString, stackDepth)
      case (Some(message), Some(cause)) => new TestFailedException(message.toString, cause, stackDepth)
    }
  
  private[scalatest] def newAssertionFailedException(optionalMessage: Option[String], optionalCause: Option[Throwable], fileName: String, methodName: String, stackDepthAdjustment: Int): Throwable =
    new exceptions.TestFailedException(toExceptionFunction(optionalMessage), optionalCause, getStackDepthFun(fileName, methodName, stackDepthAdjustment))

  private def newTestCanceledException(optionalMessage: Option[Any], optionalCause: Option[Throwable], stackDepth: Int): Throwable =
    (optionalMessage, optionalCause) match {
      case (None, None) => new TestCanceledException(stackDepth)
      case (None, Some(cause)) => new TestCanceledException(cause, stackDepth)
      case (Some(message), None) => new TestCanceledException(message.toString, stackDepth)
      case (Some(message), Some(cause)) => new TestCanceledException(message.toString, cause, stackDepth)
    }

  private[scalatest] def newTestCanceledException(optionalMessage: Option[String], optionalCause: Option[Throwable], fileName: String, methodName: String, stackDepthAdjustment: Int): Throwable =
    new TestCanceledException(toExceptionFunction(optionalMessage), optionalCause, getStackDepthFun(fileName, methodName, stackDepthAdjustment), None)

  /**
   * Assert that a boolean condition, described in <code>String</code>
   * <code>message</code>, is true.
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>TestFailedException</code> with a helpful error message
   * appended with the <code>String</code> obtained by invoking <code>toString</code> on the
   * specified <code>clue</code> as the exception's detail message.
   *
   * <p>
   * This method is implemented in terms of a Scala macro that will generate a more helpful error message
   * for expressions of this form:
   * </p>
   *
   * <ul>
   * <li>assert(a == b, "a good clue")</li>
   * <li>assert(a != b, "a good clue")</li>
   * <li>assert(a === b, "a good clue")</li>
   * <li>assert(a !== b, "a good clue")</li>
   * <li>assert(a &gt; b, "a good clue")</li>
   * <li>assert(a &gt;= b, "a good clue")</li>
   * <li>assert(a &lt; b, "a good clue")</li>
   * <li>assert(a &lt;= b, "a good clue")</li>
   * <li>assert(a startsWith "prefix", "a good clue")</li>
   * <li>assert(a endsWith "postfix", "a good clue")</li>
   * <li>assert(a contains "something", "a good clue")</li>
   * <li>assert(a eq b, "a good clue")</li>
   * <li>assert(a ne b, "a good clue")</li>
   * <li>assert(a &gt; 0 &amp;&amp; b &gt; 5, "a good clue")</li>
   * <li>assert(a &gt; 0 || b &gt; 5, "a good clue")</li>
   * <li>assert(a.isEmpty, "a good clue")</li>
   * <li>assert(!a.isEmpty, "a good clue")</li>
   * <li>assert(a.isInstanceOf[String], "a good clue")</li>
   * <li>assert(a.length == 8, "a good clue")</li>
   * <li>assert(a.size == 8, "a good clue")</li>
   * <li>assert(a.exists(_ == 8), "a good clue")</li>
   * </ul>
   *
   * <p>
   * At this time, any other form of expression will just get a <code>TestFailedException</code> with message saying the given
   * expression was false.  In the future, we will enhance this macro to give helpful error messages in more situations.
   * In ScalaTest 2.0, however, this behavior was sufficient to allow the <code>===</code> that returns <code>Boolean</code>,
   * not <code>Option[String]</code> to be the default in tests. This makes <code>===</code> consistent between tests and production
   * code. If you have pre-existing code you wrote under ScalaTest 1.x, in which you are expecting<code>===</code> to return an
   * <code>Option[String]</code>, use can get that behavior back by mixing in trait <a href="LegacyTripleEquals.html"><code>LegacyTripleEquals</code></a>.
   * </p>
   *
   * @param condition the boolean condition to assert
   * @param clue An objects whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestFailedException if the condition is <code>false</code>.
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
  def assert(condition: Boolean, clue: Any): Unit = macro AssertionsMacro.assertWithClue

  /**
   * Assert that an <code>Option[String]</code> is <code>None</code>. 
   * If the condition is <code>None</code>, this method returns normally.
   * Else, it throws <code>TestFailedException</code> with the <code>String</code>
   * value of the <code>Some</code>, as well as the 
   * <code>String</code> obtained by invoking <code>toString</code> on the
   * specified <code>clue</code>,
   * included in the <code>TestFailedException</code>'s detail message.
   *
   * <p>
   * This form of <code>assert</code> is usually called in conjunction with an
   * implicit conversion to <code>Equalizer</code>, using a <code>===</code> comparison, as in:
   * </p>
   *
   * <pre class="stHighlight">
   * assert(a === b, "extra info reported if assertion fails")
   * </pre>
   *
   * <p>
   * For more information on how this mechanism works, see the [[org.scalactic.TripleEqualsSupport.Equalizer documentation for
   * <code>Equalizer</code>]].
   * </p>
   *
   * @param o the <code>Option[String]</code> to assert
   * @param clue An object whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestFailedException if the <code>Option[String]</code> is <code>Some</code>.
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
  @deprecated("This method has been deprecated in favor of macro assertion and will be removed in a future version of ScalaTest. If you need this, please copy the source code into your own trait instead.")
  def assert(o: Option[String], clue: Any) {
    o match {
      case Some(s) => throw newAssertionFailedException(Some(clue + "\n" + s), None, 4)
      case None =>
    }
  }
  
  /**
   * Assert that an <code>Option[String]</code> is <code>None</code>.
   * If the condition is <code>None</code>, this method returns normally.
   * Else, it throws <code>TestFailedException</code> with the <code>String</code>
   * value of the <code>Some</code> included in the <code>TestFailedException</code>'s
   * detail message.
   *
   * <p>
   * This form of <code>assert</code> is usually called in conjunction with an
   * implicit conversion to <code>Equalizer</code>, using a <code>===</code> comparison, as in:
   * </p>
   *
   * <pre class="stHighlight">
   * assert(a === b)
   * </pre>
   *
   * <p>
   * For more information on how this mechanism works, see the [[org.scalactic.TripleEqualsSupport.Equalizer documentation for
   * <code>Equalizer</code>]].
   * </p>
   *
   * @param o the <code>Option[String]</code> to assert
   * @throws TestFailedException if the <code>Option[String]</code> is <code>Some</code>.
   */
  @deprecated("This method has been deprecated in favor of macro assertion and will be removed in a future version of ScalaTest. If you need this, please copy the source code into your own trait instead.")
  def assert(o: Option[String]) {
    o match {
      case Some(s) => throw newAssertionFailedException(Some(s), None, 4)
      case None =>
    }
  }

  /**
   * Assume that a boolean condition is true.
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>TestCanceledException</code>.
   *
   * <p>
   * This method is implemented in terms of a Scala macro that will generate a more helpful error message
   * for expressions of this form:
   * </p>
   *
   * <ul>
   * <li>assume(a == b)</li>
   * <li>assume(a != b)</li>
   * <li>assume(a === b)</li>
   * <li>assume(a !== b)</li>
   * <li>assume(a &gt; b)</li>
   * <li>assume(a &gt;= b)</li>
   * <li>assume(a &lt; b)</li>
   * <li>assume(a &lt;= b)</li>
   * <li>assume(a startsWith "prefix")</li>
   * <li>assume(a endsWith "postfix")</li>
   * <li>assume(a contains "something")</li>
   * <li>assume(a eq b)</li>
   * <li>assume(a ne b)</li>
   * <li>assume(a &gt; 0 &amp;&amp; b &gt; 5)</li>
   * <li>assume(a &gt; 0 || b &gt; 5)</li>
   * <li>assume(a.isEmpty)</li>
   * <li>assume(!a.isEmpty)</li>
   * <li>assume(a.isInstanceOf[String])</li>
   * <li>assume(a.length == 8)</li>
   * <li>assume(a.size == 8)</li>
   * <li>assume(a.exists(_ == 8))</li>
   * </ul>
   *
   * <p>
   * At this time, any other form of expression will just get a <code>TestCanceledException</code> with message saying the given
   * expression was false.  In the future, we will enhance this macro to give helpful error messages in more situations.
   * In ScalaTest 2.0, however, this behavior was sufficient to allow the <code>===</code> that returns <code>Boolean</code>,
   * not <code>Option[String]</code> to be the default in tests. This makes <code>===</code> consistent between tests and production
   * code. If you have pre-existing code you wrote under ScalaTest 1.x, in which you are expecting<code>===</code> to return an
   * <code>Option[String]</code>, use can get that behavior back by mixing in trait <a href="LegacyTripleEquals.html"><code>LegacyTripleEquals</code></a>.
   * </p>
   *
   * @param condition the boolean condition to assume
   * @throws TestCanceledException if the condition is <code>false</code>.
   */
  def assume(condition: Boolean): Unit = macro AssertionsMacro.assume

  /**
   * Assume that a boolean condition, described in <code>String</code>
   * <code>message</code>, is true.
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>TestCanceledException</code> with a helpful error message
   * appended with <code>String</code> obtained by invoking <code>toString</code> on the
   * specified <code>clue</code> as the exception's detail message.
   *
   * <p>
   * This method is implemented in terms of a Scala macro that will generate a more helpful error message
   * for expressions of this form:
   * </p>
   *
   * <ul>
   * <li>assume(a == b, "a good clue")</li>
   * <li>assume(a != b, "a good clue")</li>
   * <li>assume(a === b, "a good clue")</li>
   * <li>assume(a !== b, "a good clue")</li>
   * <li>assume(a &gt; b, "a good clue")</li>
   * <li>assume(a &gt;= b, "a good clue")</li>
   * <li>assume(a &lt; b, "a good clue")</li>
   * <li>assume(a &lt;= b, "a good clue")</li>
   * <li>assume(a startsWith "prefix", "a good clue")</li>
   * <li>assume(a endsWith "postfix", "a good clue")</li>
   * <li>assume(a contains "something", "a good clue")</li>
   * <li>assume(a eq b, "a good clue")</li>
   * <li>assume(a ne b, "a good clue")</li>
   * <li>assume(a &gt; 0 &amp;&amp; b &gt; 5, "a good clue")</li>
   * <li>assume(a &gt; 0 || b &gt; 5, "a good clue")</li>
   * <li>assume(a.isEmpty, "a good clue")</li>
   * <li>assume(!a.isEmpty, "a good clue")</li>
   * <li>assume(a.isInstanceOf[String], "a good clue")</li>
   * <li>assume(a.length == 8, "a good clue")</li>
   * <li>assume(a.size == 8, "a good clue")</li>
   * <li>assume(a.exists(_ == 8), "a good clue")</li>
   * </ul>
   *
   * <p>
   * At this time, any other form of expression will just get a <code>TestCanceledException</code> with message saying the given
   * expression was false.  In the future, we will enhance this macro to give helpful error messages in more situations.
   * In ScalaTest 2.0, however, this behavior was sufficient to allow the <code>===</code> that returns <code>Boolean</code>,
   * not <code>Option[String]</code> to be the default in tests. This makes <code>===</code> consistent between tests and production
   * code. If you have pre-existing code you wrote under ScalaTest 1.x, in which you are expecting<code>===</code> to return an
   * <code>Option[String]</code>, use can get that behavior back by mixing in trait <a href="LegacyTripleEquals.html"><code>LegacyTripleEquals</code></a>.
   * </p>
   *
   * @param condition the boolean condition to assume
   * @param clue An objects whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestCanceledException if the condition is <code>false</code>.
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
  def assume(condition: Boolean, clue: Any): Unit = macro AssertionsMacro.assumeWithClue

  /**
   * Assume that an <code>Option[String]</code> is <code>None</code>. 
   * If the condition is <code>None</code>, this method returns normally.
   * Else, it throws <code>TestCanceledException</code> with the <code>String</code>
   * value of the <code>Some</code>, as well as the 
   * <code>String</code> obtained by invoking <code>toString</code> on the
   * specified <code>clue</code>,
   * included in the <code>TestCanceledException</code>'s detail message.
   *
   * <p>
   * This form of <code>assume</code> is usually called in conjunction with an
   * implicit conversion to <code>Equalizer</code>, using a <code>===</code> comparison, as in:
   * </p>
   *
   * <pre class="stHighlight">
   * assume(a === b, "extra info reported if assertion fails")
   * </pre>
   *
   * <p>
   * For more information on how this mechanism works, see the [[org.scalactic.TripleEqualsSupport.Equalizer documentation for
   * <code>Equalizer</code>]].
   * </p>
   *
   * @param o the <code>Option[String]</code> to assert
   * @param clue An object whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestCanceledException if the <code>Option[String]</code> is <code>Some</code>.
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
  @deprecated("This method has been deprecated in favor of macro assumption and will be removed in a future version of ScalaTest. If you need this, please copy the source code into your own trait instead.")
  def assume(o: Option[String], clue: Any) {
    o match {
      case Some(s) => throw newTestCanceledException(Some(clue + "\n" + s), None, 3)
      case None =>
    }
  }

  /**
   * Assume that an <code>Option[String]</code> is <code>None</code>.
   * If the condition is <code>None</code>, this method returns normally.
   * Else, it throws <code>TestCanceledException</code> with the <code>String</code>
   * value of the <code>Some</code> included in the <code>TestCanceledException</code>'s
   * detail message.
   *
   * <p>
   * This form of <code>assume</code> is usually called in conjunction with an
   * implicit conversion to <code>Equalizer</code>, using a <code>===</code> comparison, as in:
   * </p>
   *
   * <pre class="stHighlight">
   * assume(a === b)
   * </pre>
   *
   * <p>
   * For more information on how this mechanism works, see the [[org.scalactic.TripleEqualsSupport.Equalizer documentation for
   * <code>Equalizer</code>]].
   * </p>
   *
   * @param o the <code>Option[String]</code> to assert
   * @throws TestCanceledException if the <code>Option[String]</code> is <code>Some</code>.
   */
  // def assume(o: Option[String]) = throwIfSome(o, (a: Any) => newTestCanceledException(Some(a.toString), None, 3))
  @deprecated("This method has been deprecated in favor of macro assumption and will be removed in a future version of ScalaTest. If you need this, please copy the source code into your own trait instead.")
  def assume(o: Option[String]) {
    o match {
      case Some(s) => throw newTestCanceledException(Some(s), None, 3)
      case None =>
    }
  }

  /**
   * Asserts that a given string snippet of code does not pass the Scala type checker, failing if the given
   * snippet does not pass the Scala parser.
   *
   * <p>
   * Often when creating libraries you may wish to ensure that certain arrangements of code that
   * represent potential &ldquo;user errors&rdquo; do not compile, so that your library is more error resistant.
   * ScalaTest's <code>Assertions</code> trait includes the following syntax for that purpose:
   * </p>
   *
   * <pre class="stHighlight">
   * assertTypeError("val a: String = 1")
   * </pre>
   *
   * <p>
   * Although <code>assertTypeError</code> is implemented with a macro that determines at compile time whether
   * the snippet of code represented by the passed string type checks, errors (<em>i.e.</em>, 
   * snippets of code that <em>do</em> type check) are reported as test failures at runtime.
   * </p>
   *
   * <p>
   * Note that the difference between <code>assertTypeError</code> and <code>assertDoesNotCompile</code> is
   * that <code>assertDoesNotCompile</code> will succeed if the given code does not compile for any reason,
   * whereas <code>assertTypeError</code> will only succeed if the given code does not compile because of
   * a type error. If the given code does not compile because of a syntax error, for example, <code>assertDoesNotCompile</code>
   * will return normally but <code>assertTypeError</code> will throw a <code>TestFailedException</code>.
   * </p>
   *
   * @param code the snippet of code that should not type check
   */
  def assertTypeError(code: String): Unit = macro CompileMacro.assertTypeErrorImpl

  /**
   * Asserts that a given string snippet of code does not pass either the Scala parser or type checker.
   *
   * <p>
   * Often when creating libraries you may wish to ensure that certain arrangements of code that
   * represent potential &ldquo;user errors&rdquo; do not compile, so that your library is more error resistant.
   * ScalaTest's <code>Assertions</code> trait includes the following syntax for that purpose:
   * </p>
   *
   * <pre class="stHighlight">
   * assertDoesNotCompile("val a: String = \"a string")
   * </pre>
   *
   * <p>
   * Although <code>assertDoesNotCompile</code> is implemented with a macro that determines at compile time whether
   * the snippet of code represented by the passed string doesn't compile, errors (<em>i.e.</em>,
   * snippets of code that <em>do</em> compile) are reported as test failures at runtime.
   * </p>
   *
   * <p>
   * Note that the difference between <code>assertTypeError</code> and <code>assertDoesNotCompile</code> is
   * that <code>assertDoesNotCompile</code> will succeed if the given code does not compile for any reason,
   * whereas <code>assertTypeError</code> will only succeed if the given code does not compile because of
   * a type error. If the given code does not compile because of a syntax error, for example, <code>assertDoesNotCompile</code>
   * will return normally but <code>assertTypeError</code> will throw a <code>TestFailedException</code>.
   * </p>
   *
   * @param code the snippet of code that should not type check
   */
  def assertDoesNotCompile(code: String): Unit = macro CompileMacro.assertDoesNotCompileImpl

  /**
   * Asserts that a given string snippet of code passes both the Scala parser and type checker.
   *
   * <p>
   * You can use this to make sure a snippet of code compiles:
   * </p>
   *
   * <pre class="stHighlight">
   * assertCompiles("val a: Int = 1")
   * </pre>
   *
   * <p>
   * Although <code>assertCompiles</code> is implemented with a macro that determines at compile time whether
   * the snippet of code represented by the passed string compiles, errors (<em>i.e.</em>,
   * snippets of code that <em>do not</em> compile) are reported as test failures at runtime.
   * </p>
   *
   * @param code the snippet of code that should compile
   */
  def assertCompiles(code: String): Unit = macro CompileMacro.assertCompilesImpl

  /* *
   * Implicit conversion from <code>Any</code> to <code>Equalizer</code>, used to enable
   * assertions with <code>===</code> comparisons.
   *
   * <p>
   * For more information
   * on this mechanism, see the <a href="Suite.Equalizer.html">documentation for </code>Equalizer</code></a>.
   * </p>
   *
   * <p>
   * Because trait <code>Suite</code> mixes in <code>Assertions</code>, this implicit conversion will always be
   * available by default in ScalaTest <code>Suite</code>s. This is the only implicit conversion that is in scope by default in every
   * ScalaTest <code>Suite</code>. Other implicit conversions offered by ScalaTest, such as those that support the matchers DSL
   * or <code>invokePrivate</code>, must be explicitly invited into your test code, either by mixing in a trait or importing the
   * members of its companion object. The reason ScalaTest requires you to invite in implicit conversions (with the exception of the
   * implicit conversion for <code>===</code> operator)  is because if one of ScalaTest's implicit conversions clashes with an
   * implicit conversion used in the code you are trying to test, your program won't compile. Thus there is a chance that if you
   * are ever trying to use a library or test some code that also offers an implicit conversion involving a <code>===</code> operator,
   * you could run into the problem of a compiler error due to an ambiguous implicit conversion. If that happens, you can turn off
   * the implicit conversion offered by this <code>convertToEqualizer</code> method simply by overriding the method in your
   * <code>Suite</code> subclass, but not marking it as implicit:
   * </p>
   *
   * <pre class="stHighlight">
   * // In your Suite subclass
   * override def convertToEqualizer(left: Any) = new Equalizer(left)
   * </pre>
   * 
   * @param left the object whose type to convert to <code>Equalizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  // implicit def convertToEqualizer(left: Any) = new Equalizer(left)

  /*
   * Intercept and return an instance of the passed exception class (or an instance of a subclass of the
   * passed class), which is expected to be thrown by the passed function value. This method invokes the passed
   * function. If it throws an exception that's an instance of the passed class or one of its
   * subclasses, this method returns that exception. Else, whether the passed function returns normally
   * or completes abruptly with a different exception, this method throws <code>TestFailedException</code>
   * whose detail message includes the <code>String</code> obtained by invoking <code>toString</code> on the passed <code>clue</code>.
   *
   * <p>
   * Note that the passed <code>Class</code> may represent any type, not just <code>Throwable</code> or one of its subclasses. In
   * Scala, exceptions can be caught based on traits they implement, so it may at times make sense to pass in a class instance for
   * a trait. If a class instance is passed for a type that could not possibly be used to catch an exception (such as <code>String</code>,
   * for example), this method will complete abruptly with a <code>TestFailedException</code>.
   * </p>
   *
   * @param message An object whose <code>toString</code> method returns a message to include in a failure report.
   * @param f the function value that should throw the expected exception
   * @return the intercepted exception, if it is of the expected type
   * @throws TestFailedException if the passed function does not result in a value equal to the
   *     passed <code>expected</code> value.
  def intercept[T <: AnyRef](message: Any)(f: => Any)(implicit manifest: Manifest[T]): T = {
    val clazz = manifest.erasure.asInstanceOf[Class[T]]
    val messagePrefix = if (message.toString.trim.isEmpty) "" else (message +"\n")
    val caught = try {
      f
      None
    }
    catch {
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass)) {
          val s = Resources("wrongException", clazz.getName, u.getClass.getName)
          throw newAssertionFailedException(Some(messagePrefix + s), Some(u), 4)
        }
        else {
          Some(u)
        }
      }
    }
    caught match {
      case None =>
        val message = messagePrefix + Resources("exceptionExpected", clazz.getName)
        throw newAssertionFailedException(Some(message), None, 4)
      case Some(e) => e.asInstanceOf[T] // I know this cast will succeed, becuase iSAssignableFrom succeeded above
    }
  }
THIS DOESN'T OVERLOAD. I THINK I'LL EITHER NEED TO USE interceptWithMessage OR JUST LEAVE IT OUT. FOR NOW I'LL LEAVE IT OUT.
   */

  /**
   * Intercept and return an exception that's expected to
   * be thrown by the passed function value. The thrown exception must be an instance of the
   * type specified by the type parameter of this method. This method invokes the passed
   * function. If the function throws an exception that's an instance of the specified type,
   * this method returns that exception. Else, whether the passed function returns normally
   * or completes abruptly with a different exception, this method throws <code>TestFailedException</code>.
   *
   * <p>
   * Note that the type specified as this method's type parameter may represent any subtype of
   * <code>AnyRef</code>, not just <code>Throwable</code> or one of its subclasses. In
   * Scala, exceptions can be caught based on traits they implement, so it may at times make sense
   * to specify a trait that the intercepted exception's class must mix in. If a class instance is
   * passed for a type that could not possibly be used to catch an exception (such as <code>String</code>,
   * for example), this method will complete abruptly with a <code>TestFailedException</code>.
   * </p>
   *
   * @param f the function value that should throw the expected exception
   * @param manifest an implicit <code>Manifest</code> representing the type of the specified
   * type parameter.
   * @return the intercepted exception, if it is of the expected type
   * @throws TestFailedException if the passed function does not complete abruptly with an exception
   *    that's an instance of the specified type
   *     passed <code>expected</code> value.
   */
  def intercept[T <: AnyRef](f: => Any)(implicit manifest: Manifest[T]): T = {
    val clazz = manifest.erasure.asInstanceOf[Class[T]]
    val caught = try {
      f
      None
    }
    catch {
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass)) {
          val s = Resources("wrongException", clazz.getName, u.getClass.getName)
          throw newAssertionFailedException(Some(s), Some(u), 4)
        }
        else {
          Some(u)
        }
      }
    }
    caught match {
      case None =>
        val message = Resources("exceptionExpected", clazz.getName)
        throw newAssertionFailedException(Some(message), None, 4)
      case Some(e) => e.asInstanceOf[T] // I know this cast will succeed, becuase isAssignableFrom succeeded above
    }
  }

  /*
   * Intercept and return an instance of the passed exception class (or an instance of a subclass of the
   * passed class), which is expected to be thrown by the passed function value. This method invokes the passed
   * function. If it throws an exception that's an instance of the passed class or one of its
   * subclasses, this method returns that exception. Else, whether the passed function returns normally
   * or completes abruptly with a different exception, this method throws <code>TestFailedException</code>.
   *
   * <p>
   * Note that the passed <code>Class</code> may represent any type, not just <code>Throwable</code> or one of its subclasses. In
   * Scala, exceptions can be caught based on traits they implement, so it may at times make sense to pass in a class instance for
   * a trait. If a class instance is passed for a type that could not possibly be used to catch an exception (such as <code>String</code>,
   * for example), this method will complete abruptly with a <code>TestFailedException</code>.
   * </p>
   *
   * @param clazz a type to which the expected exception class is assignable, i.e., the exception should be an instance of the type represented by <code>clazz</code>.
   * @param f the function value that should throw the expected exception
   * @return the intercepted exception, if 
   * @throws TestFailedException if the passed function does not complete abruptly with an exception that is assignable to the 
   *     passed <code>Class</code>.
   * @throws IllegalArgumentException if the passed <code>clazz</code> is not <code>Throwable</code> or
   *     one of its subclasses.
   */

/*
  def intercept[T <: AnyRef](clazz: java.lang.Class[T])(f: => Unit): T = {
    // intercept(clazz)(f)(manifest)
    "hi".asInstanceOf[T]
  }
*/
/*
  def intercept[T <: AnyRef](clazz: java.lang.Class[T])(f: => Unit)(implicit manifest: Manifest[T]): T = {
    intercept(clazz)(f)(manifest)
  }
*/

  /**
   * Trap and return any thrown exception that would normally cause a ScalaTest test to fail, or create and return a new <code>RuntimeException</code>
   * indicating no exception is thrown.
   *
   * <p>
   * This method is intended to be used in the Scala interpreter to eliminate large stack traces when trying out ScalaTest assertions and
   * matcher expressions. It is not intended to be used in regular test code. If you want to ensure that a bit of code throws an expected
   * exception, use <code>intercept</code>, not <code>trap</code>. Here's an example interpreter session without <code>trap</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; import org.scalatest._
   * import org.scalatest._
   *
   * scala&gt; import Matchers._
   * import Matchers._
   *
   * scala&gt; val x = 12
   * a: Int = 12
   *
   * scala&gt; x shouldEqual 13
   * org.scalatest.exceptions.TestFailedException: 12 did not equal 13
   *    at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:449)
   *    at org.scalatest.Assertions$.newAssertionFailedException(Assertions.scala:1203)
   *    at org.scalatest.Assertions$AssertionsHelper.macroAssertTrue(Assertions.scala:417)
   *    at .&lt;init&gt;(&lt;console&gt;:15)
   *    at .&lt;clinit&gt;(&lt;console&gt;)
   *    at .&lt;init&gt;(&lt;console&gt;:7)
   *    at .&lt;clinit&gt;(&lt;console&gt;)
   *    at $print(&lt;console&gt;)
   *    at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
   *    at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)
   *    at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
   *    at java.lang.reflect.Method.invoke(Method.java:597)
   *    at scala.tools.nsc.interpreter.IMain$ReadEvalPrint.call(IMain.scala:731)
   *    at scala.tools.nsc.interpreter.IMain$Request.loadAndRun(IMain.scala:980)
   *    at scala.tools.nsc.interpreter.IMain.loadAndRunReq$1(IMain.scala:570)
   *    at scala.tools.nsc.interpreter.IMain.interpret(IMain.scala:601)
   *    at scala.tools.nsc.interpreter.IMain.interpret(IMain.scala:565)
   *    at scala.tools.nsc.interpreter.ILoop.reallyInterpret$1(ILoop.scala:745)
   *    at scala.tools.nsc.interpreter.ILoop.interpretStartingWith(ILoop.scala:790)
   *    at scala.tools.nsc.interpreter.ILoop.command(ILoop.scala:702)
   *    at scala.tools.nsc.interpreter.ILoop.processLine$1(ILoop.scala:566)
   *    at scala.tools.nsc.interpreter.ILoop.innerLoop$1(ILoop.scala:573)
   *    at scala.tools.nsc.interpreter.ILoop.loop(ILoop.scala:576)
   *    at scala.tools.nsc.interpreter.ILoop$$anonfun$process$1.apply$mcZ$sp(ILoop.scala:867)
   *    at scala.tools.nsc.interpreter.ILoop$$anonfun$process$1.apply(ILoop.scala:822)
   *    at scala.tools.nsc.interpreter.ILoop$$anonfun$process$1.apply(ILoop.scala:822)
   *    at scala.tools.nsc.util.ScalaClassLoader$.savingContextLoader(ScalaClassLoader.scala:135)
   *    at scala.tools.nsc.interpreter.ILoop.process(ILoop.scala:822)
   *    at scala.tools.nsc.MainGenericRunner.runTarget$1(MainGenericRunner.scala:83)
   *    at scala.tools.nsc.MainGenericRunner.process(MainGenericRunner.scala:96)
   *    at scala.tools.nsc.MainGenericRunner$.main(MainGenericRunner.scala:105)
   *    at scala.tools.nsc.MainGenericRunner.main(MainGenericRunner.scala)
   * </pre>
   * 
   * <p>
   * That's a pretty tall stack trace. Here's what it looks like when you use <code>trap</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; trap { x shouldEqual 13 }
   * res1: Throwable = org.scalatest.exceptions.TestFailedException: 12 did not equal 13
   * </pre>
   *
   * <p>
   * Much less clutter. Bear in mind, however, that if <em>no</em> exception is thrown by the
   * passed block of code, the <code>trap</code> method will create a new <a href="Assertions$$NormalResult.html"><code>NormalResult</code></a>
   * (a subclass of <code>Throwable</code> made for this purpose only) and return that. If the result was the <code>Unit</code> value, it
   * will simply say that no exception was thrown:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; trap { x shouldEqual 12 }
   * res2: Throwable = No exception was thrown.
   * </pre>
   *
   * <p>
   * If the passed block of code results in a value other than <code>Unit</code>, the <code>NormalResult</code>'s <code>toString</code> will print the value:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; trap { "Dude!" }
   * res3: Throwable = No exception was thrown. Instead, result was: "Dude!"
   * </pre>
   *
   * <p>
   * Although you can access the result value from the <code>NormalResult</code>, its type is <code>Any</code> and therefore not
   * very convenient to use. It is not intended that <code>trap</code> be used in test code. The sole intended use case for <code>trap</code> is decluttering
   * Scala interpreter sessions by eliminating stack traces when executing assertion and matcher expressions.
   * </p>
   */
  def trap[T](f: => T): Throwable = {
    try { new NormalResult(f) }
    catch {
      case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => ex
    }
  }

  /**
   * Assert that the value passed as <code>expected</code> equals the value passed as <code>actual</code>.
   * If the <code>actual</code> equals the <code>expected</code>
   * (as determined by <code>==</code>), <code>assertResult</code> returns
   * normally. Else, if <code>actual</code> is not equal to <code>expected</code>, <code>assertResult</code> throws a
   * <code>TestFailedException</code> whose detail message includes the expected and actual values, as well as the <code>String</code>
   * obtained by invoking <code>toString</code> on the passed <code>clue</code>.
   *
   * @param expected the expected value
   * @param clue An object whose <code>toString</code> method returns a message to include in a failure report.
   * @param actual the actual value, which should equal the passed <code>expected</code> value
   * @throws TestFailedException if the passed <code>actual</code> value does not equal the passed <code>expected</code> value.
   */
  def assertResult(expected: Any, clue: Any)(actual: Any) {
    if (!areEqualComparingArraysStructurally(actual, expected)) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val s = FailureMessages("expectedButGot", exp, act)
      val fullMsg = AppendedClues.appendClue(s, clue.toString)
      throw newAssertionFailedException(Some(fullMsg), None, 4)
    }
  }

  /**
   * This <code>expectResult</code> method has been deprecated; Please use <code>assertResult</code> instead.
   *
   * <p>
   * To get rid of the deprecation warning, simply replace <code>expectResult</code> with
   * <code>assertResult</code>. The name <code>expectResult</code> will be used for a different purposes in
   * a future version of ScalaTest.
   * </p>
   */
  @deprecated("This expectResult method has been deprecated. Please replace all invocations of expectResult with an identical invocation of assertResult instead.")
  def expectResult(expected: Any, clue: Any)(actual: Any) {
    if (actual != expected) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val s = FailureMessages("expectedButGot", exp, act)
      throw newAssertionFailedException(Some(clue + "\n" + s), None, 4)
    }
  }

  /**
   * This <code>expect</code> method has been deprecated; Please use <code>assertResult</code> instead.
   *
   * <p>
   * To get rid of the deprecation warning, simply replace <code>expect</code> with
   * <code>assertResult</code>. The name <code>expect</code> will be used for a different purposes in
   * a future version of ScalaTest.
   * </p>
   */
  @deprecated("This expect method has been deprecated. Please replace all invocations of expect with an identical invocation of assertResult instead.")
  def expect(expected: Any, clue: Any)(actual: Any) {
    if (actual != expected) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val s = FailureMessages("expectedButGot", exp, act)
      throw newAssertionFailedException(Some(clue + "\n" + s), None, 4)
    }
  }

  /** 
   * Assert that the value passed as <code>expected</code> equals the value passed as <code>actual</code>.
   * If the <code>actual</code> value equals the <code>expected</code> value
   * (as determined by <code>==</code>), <code>assertResult</code> returns
   * normally. Else, <code>assertResult</code> throws a
   * <code>TestFailedException</code> whose detail message includes the expected and actual values.
   *
   * @param expected the expected value
   * @param actual the actual value, which should equal the passed <code>expected</code> value
   * @throws TestFailedException if the passed <code>actual</code> value does not equal the passed <code>expected</code> value.
   */
  def assertResult(expected: Any)(actual: Any) {
    if (!areEqualComparingArraysStructurally(actual, expected)) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val s = FailureMessages("expectedButGot", exp, act)
      throw newAssertionFailedException(Some(s), None, 4)
    }
  }

  /**
   * This <code>expectResult</code> method has been deprecated; Please use <code>assertResult</code> instead.
   *
   * <p>
   * To get rid of the deprecation warning, simply replace <code>expectResult</code> with
   * <code>assertResult</code>. The name <code>expectResult</code> will be used for a different purposes in
   * a future version of ScalaTest.
   * </p>
   */
  @deprecated("This expectResult method has been deprecated. Please replace all invocations of expectResult with an identical invocation of assertResult instead.")
  def expectResult(expected: Any)(actual: Any) {
    if (actual != expected) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val s = FailureMessages("expectedButGot", exp, act)
      throw newAssertionFailedException(Some(s), None, 4)
    }
  }

  /**
   * This <code>expect</code> method has been deprecated; Please use <code>assertResult</code> instead.
   *
   * <p>
   * To get rid of the deprecation warning, simply replace <code>expect</code> with
   * <code>assertResult</code>. The name <code>expect</code> will be used for a different purposes in
   * a future version of ScalaTest.
   * </p>
   */
  @deprecated("This expect method has been deprecated. Please replace all invocations of expect with an identical invocation of assertResult instead.")
  def expect(expected: Any)(actual: Any) {
    if (actual != expected) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val s = FailureMessages("expectedButGot", exp, act)
      throw newAssertionFailedException(Some(s), None, 4)
    }
  }
  
/*
   * TODO: Delete this if sticking with Nothing instead of Unit as result type of fail.
   * <p>
   * The result type of this and the other overloaded <code>fail</code> methods is
   * <code>Unit</code> instead of <code>Nothing</code>, because <code>Nothing</code>
   * is a subtype of all other types. If the result type of <code>fail</code> were
   * <code>Nothing</code>, a block of code that ends in a call to <code>fail()</code> may
   * fail to compile if the block being passed as a by-name parameter or function to an
   * overloaded method. The reason is that the compiler selects which overloaded
   * method to call based on the static types of the parameters passed. Since
   * <code>Nothing</code> is an instance of everything, it can often make the overloaded
   * method selection ambiguous.
   * </p>
   *
   * <p>
   * For a concrete example, the <code>Conductor</code> class
   * in package <code>org.scalatest.concurrent</code> has two overloaded variants of the
   * <code>thread</code> method:
   * </p>
   *
   * <pre class="stHighlight">
   * def thread[T](fun: => T): Thread
   *
   * def thread[T](name: String)(fun: => T): Thread
   * </pre>
   *
   * <p>
   * Given these two overloaded methods, the following code will compile given the result type
   * of <code>fail</code> is <code>Unit</code>, but would not compile if the result type were
   * <code>Nothing</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * thread { fail() }
   * </pre>
   *
   * <p>
   * If the result type of <code>fail</code> were <code>Nothing</code>, the type of the by-name parameter
   * would be inferred to be <code>Nothing</code>, which is a subtype of both <code>T</code> and
   * <code>String</code>. Thus the call is ambiguous, because the type matches the first parameter type
   * of both overloaded <code>thread</code> methods. <code>Unit</code>, by constrast, is <em>not</em>
   * a subtype of <code>String</code>, so it only matches one overloaded variant and compiles just fine.
   * </p>
*/
  /**
   * Throws <code>TestFailedException</code> to indicate a test failed.
   */
  def fail(): Nothing = { throw newAssertionFailedException(None, None, 4) }

  /**
   * Throws <code>TestFailedException</code>, with the passed
   * <code>String</code> <code>message</code> as the exception's detail
   * message, to indicate a test failed.
   *
   * @param message A message describing the failure.
   * @throws NullPointerException if <code>message</code> is <code>null</code>
   */
  def fail(message: String): Nothing = {

    if (message == null)
        throw new NullPointerException("message is null")
     
    throw newAssertionFailedException(Some(message),  None, 4)
  }

  /**
   * Throws <code>TestFailedException</code>, with the passed
   * <code>String</code> <code>message</code> as the exception's detail
   * message and <code>Throwable</code> cause, to indicate a test failed.
   *
   * @param message A message describing the failure.
   * @param cause A <code>Throwable</code> that indicates the cause of the failure.
   * @throws NullPointerException if <code>message</code> or <code>cause</code> is <code>null</code>
   */
  def fail(message: String, cause: Throwable): Nothing = {

    if (message == null)
      throw new NullPointerException("message is null")

    if (cause == null)
      throw new NullPointerException("cause is null")

    throw newAssertionFailedException(Some(message), Some(cause), 4)
  }

  /**
   * Throws <code>TestFailedException</code>, with the passed
   * <code>Throwable</code> cause, to indicate a test failed.
   * The <code>getMessage</code> method of the thrown <code>TestFailedException</code>
   * will return <code>cause.toString</code>.
   *
   * @param cause a <code>Throwable</code> that indicates the cause of the failure.
   * @throws NullPointerException if <code>cause</code> is <code>null</code>
   */
  def fail(cause: Throwable): Nothing = {

    if (cause == null)
      throw new NullPointerException("cause is null")
        
    throw newAssertionFailedException(None, Some(cause), 4)
  }
  
  /**
   * Throws <code>TestCanceledException</code> to indicate a test was canceled.
   */
  def cancel(): Nothing = { throw newTestCanceledException(None, None, 3) }

  /**
   * Throws <code>TestCanceledException</code>, with the passed
   * <code>String</code> <code>message</code> as the exception's detail
   * message, to indicate a test was canceled.
   *
   * @param message A message describing the cancellation.
   * @throws NullPointerException if <code>message</code> is <code>null</code>
   */
  def cancel(message: String): Nothing = {

    if (message == null)
        throw new NullPointerException("message is null")
     
    throw newTestCanceledException(Some(message),  None, 3)
  }

  /**
   * Throws <code>TestCanceledException</code>, with the passed
   * <code>String</code> <code>message</code> as the exception's detail
   * message and <code>Throwable</code> cause, to indicate a test failed.
   *
   * @param message A message describing the failure.
   * @param cause A <code>Throwable</code> that indicates the cause of the failure.
   * @throws NullPointerException if <code>message</code> or <code>cause</code> is <code>null</code>
   */
  def cancel(message: String, cause: Throwable): Nothing = {

    if (message == null)
      throw new NullPointerException("message is null")

    if (cause == null)
      throw new NullPointerException("cause is null")

    throw newTestCanceledException(Some(message), Some(cause), 3)
  }

  /**
   * Throws <code>TestCanceledException</code>, with the passed
   * <code>Throwable</code> cause, to indicate a test failed.
   * The <code>getMessage</code> method of the thrown <code>TestCanceledException</code>
   * will return <code>cause.toString</code>.
   *
   * @param cause a <code>Throwable</code> that indicates the cause of the cancellation.
   * @throws NullPointerException if <code>cause</code> is <code>null</code>
   */
  def cancel(cause: Throwable): Nothing = {

    if (cause == null)
      throw new NullPointerException("cause is null")
        
    throw newTestCanceledException(None, Some(cause), 3)
  }
  
  /**
   * Executes the block of code passed as the second parameter, and, if it
   * completes abruptly with a <code>ModifiableMessage</code> exception,
   * prepends the "clue" string passed as the first parameter to the beginning of the detail message
   * of that thrown exception, then rethrows it. If clue does not end in a white space
   * character, one space will be added
   * between it and the existing detail message (unless the detail message is
   * not defined).
   *
   * <p>
   * This method allows you to add more information about what went wrong that will be
   * reported when a test fails. Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * withClue("(Employee's name was: " + employee.name + ")") {
   *   intercept[IllegalArgumentException] {
   *     employee.getTask(-1)
   *   }
   * }
   * </pre>
   *
   * <p>
   * If an invocation of <code>intercept</code> completed abruptly with an exception, the resulting message would be something like:
   * </p>
   *
   * <pre>
   * (Employee's name was Bob Jones) Expected IllegalArgumentException to be thrown, but no exception was thrown
   * </pre>
   *
   * @throws NullPointerException if the passed <code>clue</code> is <code>null</code>
  */
  def withClue[T](clue: Any)(fun: => T): T = {
    if (clue == null)
      throw new NullPointerException("clue was null")
    def prepend(currentMessage: Option[String]) =
      currentMessage match {
        case Some(msg) =>
          if (clue.toString.last.isWhitespace) // TODO: shouldn't I also check if the head of msg isWhite?
            Some(clue.toString + msg)
          else 
            Some(clue.toString + " " + msg)
        case None => Some(clue.toString)
      }
    try {
      val outcome = fun
      outcome match {
        case Failed(e: org.scalatest.exceptions.ModifiableMessage[_]) if clue.toString != "" =>
          Failed(e.modifyMessage(prepend)).asInstanceOf[T]
        case Canceled(e: org.scalatest.exceptions.ModifiableMessage[_]) if clue.toString != "" =>
          Canceled(e.modifyMessage(prepend)).asInstanceOf[T]
        case _ => outcome
      }
    }
    catch {
      case e: org.scalatest.exceptions.ModifiableMessage[_] =>
        if (clue != "")
          throw e.modifyMessage(prepend)
        else
          throw e
    }
  }

/* Hold off on this for now. See how people do with the simple one that takes an Any.
  def withClueFunction(sfun: Option[String] => Option[String])(fun: => Unit) {
    fun
  }
*/
}

/**
 * Companion object that facilitates the importing of <code>Assertions</code> members as 
 * an alternative to mixing it in. One use case is to import <code>Assertions</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala version 2.7.3.final (Java HotSpot(TM) Client VM, Java 1.5.0_16).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * &nbsp;
 * scala&gt; import org.scalatest.Assertions._
 * import org.scalatest.Assertions._
 * &nbsp;
 * scala&gt; assert(1 === 2)
 * org.scalatest.TestFailedException: 1 did not equal 2
 *      at org.scalatest.Assertions$class.assert(Assertions.scala:211)
 *      at org.scalatest.Assertions$.assert(Assertions.scala:511)
 *      at .&lt;init&gt;(&lt;console&gt;:7)
 *      at .&lt;clinit&gt;(&lt;console&gt;)
 *      at RequestResult$.&lt;init&gt;(&lt;console&gt;:3)
 *      at RequestResult$.&lt;clinit&gt;(&lt;console&gt;)
 *      at RequestResult$result(&lt;console&gt;)
 *      at sun.reflect.NativeMethodAccessorImpl.invoke...
 *&nbsp;
 * scala&gt; assertResult(3) { 1 + 3 }
 * org.scalatest.TestFailedException: Expected 3, but got 4
 *      at org.scalatest.Assertions$class.expect(Assertions.scala:447)
 *      at org.scalatest.Assertions$.expect(Assertions.scala:511)
 *      at .&lt;init&gt;(&lt;console&gt;:7)
 *      at .&lt;clinit&gt;(&lt;console&gt;)
 *      at RequestResult$.&lt;init&gt;(&lt;console&gt;:3)
 *      at RequestResult$.&lt;clinit&gt;(&lt;console&gt;)
 *      at RequestResult$result(&lt;console&gt;)
 *      at sun.reflect.NativeMethodAccessorImpl.in...
 *&nbsp;
 * scala&gt; val caught = intercept[StringIndexOutOfBoundsException] { "hi".charAt(-1) }
 * caught: StringIndexOutOfBoundsException = java.lang.StringIndexOutOfBoundsException: String index out of range: -1
 * </pre>
 *
 * @author Bill Venners
 */
object Assertions extends Assertions {

  case class NormalResult(result: Any) extends Throwable {
    override def toString = if (result == ()) Resources("noExceptionWasThrown") else Resources("resultWas", Prettifier.default(result))
  }

  private[scalatest] def areEqualComparingArraysStructurally(left: Any, right: Any): Boolean = {
    // Prior to 2.0 this only called .deep if both sides were arrays. Loosened it
    // when nearing 2.0.M6 to call .deep if either left or right side is an array.
    // TODO: this is the same algo as in scalactic.DefaultEquality. Put that one in
    // a singleton and use it in both places.
    left match {
      case leftArray: Array[_] =>
        right match {
          case rightArray: Array[_] => leftArray.deep == rightArray.deep
          case _ => leftArray.deep == right
        }
      case _ => {
        right match {
          case rightArray: Array[_] => left == rightArray.deep
          case _ => left == right
        }
      }
    }
  }
  private[scalatest] def checkExpectedException[T](f: => Any, clazz: Class[T], wrongExceptionResourceName: String, exceptionExpectedResourceName: String, stackDepth: Int): T = {
    val caught = try {
      f
      None
    }
    catch {
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass)) {
          val s = Resources(wrongExceptionResourceName, clazz.getName, u.getClass.getName)
          throw newAssertionFailedException(Some(s), Some(u), stackDepth)
        }
        else {
          Some(u)
        }
      }
    }
    caught match {
      case None =>
        val message = Resources(exceptionExpectedResourceName, clazz.getName)
        throw newAssertionFailedException(Some(message), None, stackDepth)
      case Some(e) => e.asInstanceOf[T] // I know this cast will succeed, becuase iSAssignableFrom succeeded above
    }
  }
  private[scalatest] def checkNoException(fun: => Any) {
    val caught = try {
      fun
    }
    catch {
      case u: Throwable => {
        val message = Resources("exceptionNotExpected", u.getClass.getName)
        throw newAssertionFailedException(Some(message), Some(u), 4)
      }
    }
  }
  private[scalatest] def checkNotException[T <: AnyRef](f: => Any, exceptionNotExpectedResourceName: String)(implicit manifest: Manifest[T]) {
    val clazz = manifest.erasure.asInstanceOf[Class[T]]
    try {
      f
    }
    catch {
      case u: Throwable => {
        if (clazz.isAssignableFrom(u.getClass)) {
          val s = Resources(exceptionNotExpectedResourceName, u.getClass.getName)
          throw newAssertionFailedException(Some(s), Some(u), 4)
        }
      }
    }
  }
}
