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

import org.scalactic.{Resources => _, FailureMessages => _, _}
import Requirements._

import scala.reflect.ClassTag
import Assertions.areEqualComparingArraysStructurally
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.StackDepthException.toExceptionFunction
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestPendingException
import org.scalatest.exceptions.TestCanceledException
import ArrayHelper.deep
import org.scalactic.anyvals.NonEmptyArray

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
 * In addition, ScalaTest's <code>assert</code> provides better error messages than Scala's <code>assert</code>.
 * <p>
 *
 * <p>
 * If you pass the previous <code>Boolean</code> expression, <code>left == right</code> to <code>assert</code> in a ScalaTest test,
 * a failure will be reported that, because <code>assert</code> is implemented as a macro,
 * includes reporting the left and right values.
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
 * <a name="achievingSuccess"></a>
 * <h2>Achieving success</h2>
 *
 * <p>
 * In async style tests, you must end your test body with either <code>Future[Assertion]</code> or
 * <code>Assertion</code>. ScalaTest's assertions (including matcher expressions) have result type
 * <code>Assertion</code>, so ending with an assertion will satisfy the compiler.
 * If a test body or function body passed to <code>Future.map</code> does
 * <em>not</em> end with type <code>Assertion</code>, however, you can fix the type error by placing
 * <code>succeed</code> at the end of the
 * test or function body:
 * </p>
 *
 * <pre class="stHighlight">
 * succeed // Has type Assertion
 * </pre>
 *
 * <a name="interceptedExceptions"></a>
 * <a name="expectedExceptions"></a>
 * <h2>Expected exceptions</h2>
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
 * To make this common use case easier to express and read, ScalaTest provides two methods:
 * <code>assertThrows</code> and <code>intercept</code>.
 * Here's how you use <code>assertThrows</code>:
 * </p>
 *
 * <a name="assertThrowsMethod"></a>
 * <pre class="stHighlight">
 * val s = "hi"
 * assertThrows[IndexOutOfBoundsException] { // Result type: Assertion
 *   s.charAt(-1)
 * }
 * </pre>
 *
 * <p>
 * This code behaves much like the previous example. If <code>charAt</code> throws an instance of <code>IndexOutOfBoundsException</code>,
 * <code>assertThrows</code> will return <code>Succeeded</code>. But if <code>charAt</code> completes normally, or throws a different
 * exception, <code>assertThrows</code> will complete abruptly with a <code>TestFailedException</code>.
 * </p>
 *
 * <p>
 * The <code>intercept</code> method behaves the same as <code>assertThrows</code>, except that instead of returning <code>Succeeded</code>,
 * <code>intercept</code> returns the caught exception so that you can inspect it further if you wish. For example, you may need
 * to ensure that data contained inside the exception have expected values. Here's an example:
 * </p>
 *
 * <a name="interceptMethod"></a>
 * <pre class="stHighlight">
 * val s = "hi"
 * val caught =
 *   intercept[IndexOutOfBoundsException] { // Result type: IndexOutOfBoundsException
 *     s.charAt(-1)
 *   }
 * assert(caught.getMessage.indexOf("-1") != -1)
 * </pre>
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
trait Assertions extends TripleEquals  {

  //implicit val prettifier = Prettifier.default

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
   * In ScalaTest 2.0, however, this behavior was sufficient to allow the <code>===</code> that returns <code>Boolean</code>
   * to be the default in tests. This makes <code>===</code> consistent between tests and production
   * code.
   * </p>
   *
   * @param condition the boolean condition to assert
   * @throws TestFailedException if the condition is <code>false</code>.
   */
  def assert(condition: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Assertion = macro AssertionsMacro.assert

  private[scalatest] def newAssertionFailedException(optionalMessage: Option[String], optionalCause: Option[Throwable], pos: source.Position, analysis: scala.collection.immutable.IndexedSeq[String]): Throwable =
    new org.scalatest.exceptions.TestFailedException(toExceptionFunction(optionalMessage), optionalCause, Left(pos), None, analysis)

  private[scalatest] def newTestCanceledException(optionalMessage: Option[String], optionalCause: Option[Throwable], pos: source.Position): Throwable =
    new TestCanceledException(toExceptionFunction(optionalMessage), optionalCause, pos, None)

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
   * In ScalaTest 2.0, however, this behavior was sufficient to allow the <code>===</code> that returns <code>Boolean</code>
   * to be the default in tests. This makes <code>===</code> consistent between tests and production
   * code.
   * </p>
   *
   * @param condition the boolean condition to assert
   * @param clue An objects whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestFailedException if the condition is <code>false</code>.
   * @throws NullArgumentException if <code>message</code> is <code>null</code>.
   */
  def assert(condition: Boolean, clue: Any)(implicit prettifier: Prettifier, pos: source.Position): Assertion = macro AssertionsMacro.assertWithClue

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
   * In ScalaTest 2.0, however, this behavior was sufficient to allow the <code>===</code> that returns <code>Boolean</code>
   * to be the default in tests. This makes <code>===</code> consistent between tests and production
   * code.
   * </p>
   *
   * @param condition the boolean condition to assume
   * @throws TestCanceledException if the condition is <code>false</code>.
   */
  def assume(condition: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Assertion = macro AssertionsMacro.assume

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
   * In ScalaTest 2.0, however, this behavior was sufficient to allow the <code>===</code> that returns <code>Boolean</code>
   * to be the default in tests. This makes <code>===</code> consistent between tests and production
   * code.
   * </p>
   *
   * @param condition the boolean condition to assume
   * @param clue An objects whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestCanceledException if the condition is <code>false</code>.
   * @throws NullArgumentException if <code>message</code> is <code>null</code>.
   */
  def assume(condition: Boolean, clue: Any)(implicit prettifier: Prettifier, pos: source.Position): Assertion = macro AssertionsMacro.assumeWithClue

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
  def assertTypeError(code: String)(implicit pos: source.Position): Assertion = macro CompileMacro.assertTypeErrorImpl

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
  def assertDoesNotCompile(code: String)(implicit pos: source.Position): Assertion = macro CompileMacro.assertDoesNotCompileImpl

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
  def assertCompiles(code: String)(implicit pos: source.Position): Assertion = macro CompileMacro.assertCompilesImpl

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
   * <p>
   * Also note that the difference between this method and <code>assertThrows</code> is that this method
   * returns the expected exception, so it lets you perform further assertions on
   * that exception. By contrast, the <code>assertThrows</code> method returns <code>Succeeded</code>, which means it can
   * serve as the last statement in an async- or safe-style suite. <code>assertThrows</code> also indicates to the reader
   * of the code that nothing further is expected about the thrown exception other than its type.
   * The recommended usage is to use <code>assertThrows</code> by default, <code>intercept</code> only when you
   * need to inspect the caught exception further.
   * </p>
   *
   * @param f the function value that should throw the expected exception
   * @param classTag an implicit <code>ClassTag</code> representing the type of the specified
   * type parameter.
   * @return the intercepted exception, if it is of the expected type
   * @throws TestFailedException if the passed function does not complete abruptly with an exception
   *    that's an instance of the specified type.
   */
  def intercept[T <: AnyRef](f: => Any)(implicit classTag: ClassTag[T], pos: source.Position): T = {
    val clazz = classTag.runtimeClass
    val caught = try {
      f
      None
    }
    catch {
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass)) {
          val s = Resources.wrongException(clazz.getName, u.getClass.getName)
          throw newAssertionFailedException(Some(s), Some(u), pos, Vector.empty)
        }
        else {
          Some(u)
        }
      }
    }
    caught match {
      case None =>
        val message = Resources.exceptionExpected(clazz.getName)
        throw newAssertionFailedException(Some(message), None, pos, Vector.empty)
      case Some(e) => e.asInstanceOf[T] // I know this cast will succeed, becuase isAssignableFrom succeeded above
    }
  }

  /**
   * Ensure that an expected exception is thrown by the passed function value. The thrown exception must be an instance of the
   * type specified by the type parameter of this method. This method invokes the passed
   * function. If the function throws an exception that's an instance of the specified type,
   * this method returns <code>Succeeded</code>. Else, whether the passed function returns normally
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
   * <p>
   * Also note that the difference between this method and <code>intercept</code> is that this method
   * does not return the expected exception, so it does not let you perform further assertions on
   * that exception. Instead, this method returns <code>Succeeded</code>, which means it can
   * serve as the last statement in an async- or safe-style suite. It also indicates to the reader
   * of the code that nothing further is expected about the thrown exception other than its type.
   * The recommended usage is to use <code>assertThrows</code> by default, <code>intercept</code> only when you
   * need to inspect the caught exception further.
   * </p>
   *
   * @param f the function value that should throw the expected exception
   * @param classTag an implicit <code>ClassTag</code> representing the type of the specified
   * type parameter.
   * @return the <code>Succeeded</code> singleton, if an exception of the expected type is thrown
   * @throws TestFailedException if the passed function does not complete abruptly with an exception
   *    that's an instance of the specified type.
   */
  def assertThrows[T <: AnyRef](f: => Any)(implicit classTag: ClassTag[T], pos: source.Position): Assertion = {
    val clazz = classTag.runtimeClass
    val threwExpectedException =
      try {
        f
        false
      }
      catch {
          case u: Throwable => {
          if (!clazz.isAssignableFrom(u.getClass)) {
            val s = Resources.wrongException(clazz.getName, u.getClass.getName)
            throw newAssertionFailedException(Some(s), Some(u), pos, Vector.empty)
          }
          else true
        }
      }
    if (threwExpectedException) {
      Succeeded
    }
    else {
        val message = Resources.exceptionExpected(clazz.getName)
        throw newAssertionFailedException(Some(message), None, pos, Vector.empty)
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
  def assertResult(expected: Any, clue: Any)(actual: Any)(implicit prettifier: Prettifier, pos: source.Position): Assertion = {
    if (!areEqualComparingArraysStructurally(actual, expected)) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val s = FailureMessages.expectedButGot(prettifier, exp, act)
      val fullMsg = AppendedClues.appendClue(s, clue.toString)
      throw newAssertionFailedException(Some(fullMsg), None, pos, Vector.empty)
    }
    Succeeded
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
  def assertResult(expected: Any)(actual: Any)(implicit prettifier: Prettifier, pos: source.Position): Assertion = {
    if (!areEqualComparingArraysStructurally(actual, expected)) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val s = FailureMessages.expectedButGot(prettifier, exp, act)
      throw newAssertionFailedException(Some(s), None, pos, Vector.empty)
    }
    Succeeded
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
  def fail()(implicit pos: source.Position): Nothing = { throw newAssertionFailedException(None, None, pos, Vector.empty) }

  /**
   * Throws <code>TestFailedException</code>, with the passed
   * <code>String</code> <code>message</code> as the exception's detail
   * message, to indicate a test failed.
   *
   * @param message A message describing the failure.
   * @throws NullArgumentException if <code>message</code> is <code>null</code>
   */
  def fail(message: String)(implicit pos: source.Position): Nothing = {

    requireNonNull(message)
     
    throw newAssertionFailedException(Some(message),  None, pos, Vector.empty)
  }

  /**
   * Throws <code>TestFailedException</code>, with the passed
   * <code>String</code> <code>message</code> as the exception's detail
   * message and <code>Throwable</code> cause, to indicate a test failed.
   *
   * @param message A message describing the failure.
   * @param cause A <code>Throwable</code> that indicates the cause of the failure.
   * @throws NullArgumentException if <code>message</code> or <code>cause</code> is <code>null</code>
   */
  def fail(message: String, cause: Throwable)(implicit pos: source.Position): Nothing = {

    requireNonNull(message, cause)

    throw newAssertionFailedException(Some(message), Some(cause), pos, Vector.empty)
  }

  /**
   * Throws <code>TestFailedException</code>, with the passed
   * <code>Throwable</code> cause, to indicate a test failed.
   * The <code>getMessage</code> method of the thrown <code>TestFailedException</code>
   * will return <code>cause.toString</code>.
   *
   * @param cause a <code>Throwable</code> that indicates the cause of the failure.
   * @throws NullArgumentException if <code>cause</code> is <code>null</code>
   */
  def fail(cause: Throwable)(implicit pos: source.Position): Nothing = {

    requireNonNull(cause)
        
    throw newAssertionFailedException(None, Some(cause), pos, Vector.empty)
  }
  
  /**
   * Throws <code>TestCanceledException</code> to indicate a test was canceled.
   */
  def cancel()(implicit pos: source.Position): Nothing = { throw newTestCanceledException(None, None, pos) }

  /**
   * Throws <code>TestCanceledException</code>, with the passed
   * <code>String</code> <code>message</code> as the exception's detail
   * message, to indicate a test was canceled.
   *
   * @param message A message describing the cancellation.
   * @throws NullArgumentException if <code>message</code> is <code>null</code>
   */
  def cancel(message: String)(implicit pos: source.Position): Nothing = {

    requireNonNull(message)
     
    throw newTestCanceledException(Some(message),  None, pos)
  }

  /**
   * Throws <code>TestCanceledException</code>, with the passed
   * <code>String</code> <code>message</code> as the exception's detail
   * message and <code>Throwable</code> cause, to indicate a test failed.
   *
   * @param message A message describing the failure.
   * @param cause A <code>Throwable</code> that indicates the cause of the failure.
   * @throws NullArgumentException if <code>message</code> or <code>cause</code> is <code>null</code>
   */
  def cancel(message: String, cause: Throwable)(implicit pos: source.Position): Nothing = {

    requireNonNull(message, cause)

    throw newTestCanceledException(Some(message), Some(cause), pos)
  }

  /**
   * Throws <code>TestCanceledException</code>, with the passed
   * <code>Throwable</code> cause, to indicate a test failed.
   * The <code>getMessage</code> method of the thrown <code>TestCanceledException</code>
   * will return <code>cause.toString</code>.
   *
   * @param cause a <code>Throwable</code> that indicates the cause of the cancellation.
   * @throws NullArgumentException if <code>cause</code> is <code>null</code>
   */
  def cancel(cause: Throwable)(implicit pos: source.Position): Nothing = {

    requireNonNull(cause)
        
    throw newTestCanceledException(None, Some(cause), pos)
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
   * @throws NullArgumentException if the passed <code>clue</code> is <code>null</code>
  */
  def withClue[T](clue: Any)(fun: => T): T = {
    requireNonNull(clue)
    val prepend = (currentMessage: Option[String]) => {
      currentMessage match {
        case Some(msg) =>
          if (clue.toString.last.isWhitespace) // TODO: shouldn't I also check if the head of msg isWhite?
            Some(clue.toString + msg)
          else
            Some(clue.toString + " " + msg)
        case None => Some(clue.toString)
      }
    }
    try {
      val outcome = fun
      outcome match {
        case Failed(e: org.scalatest.exceptions.ModifiableMessage[_]) if clue.toString != "" =>
          Failed(e.modifyMessage(prepend)).asInstanceOf[T]
        case Canceled(e: org.scalatest.exceptions.ModifiableMessage[_]) if clue.toString != "" =>
          Canceled(e.modifyMessage(prepend)).asInstanceOf[T]
        case fact: Fact =>
          fact.modifyMessage(prepend).asInstanceOf[T]  
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
  /**
   * Throws <code>TestPendingException</code> to indicate a test is pending.
   *
   * <p>
   * A <em>pending test</em> is one that has been given a name but is not yet implemented. The purpose of
   * pending tests is to facilitate a style of testing in which documentation of behavior is sketched
   * out before tests are written to verify that behavior (and often, the before the behavior of
   * the system being tested is itself implemented). Such sketches form a kind of specification of
   * what tests and functionality to implement later.
   * </p>
   *
   * <p>
   * To support this style of testing, a test can be given a name that specifies one
   * bit of behavior required by the system being tested. The test can also include some code that
   * sends more information about the behavior to the reporter when the tests run. At the end of the test,
   * it can call method <code>pending</code>, which will cause it to complete abruptly with <code>TestPendingException</code>.
   * Because tests in ScalaTest can be designated as pending with <code>TestPendingException</code>, both the test name and any information
   * sent to the reporter when running the test can appear in the report of a test run. (In other words,
   * the code of a pending test is executed just like any other test.) However, because the test completes abruptly
   * with <code>TestPendingException</code>, the test will be reported as pending, to indicate
   * the actual test, and possibly the functionality it is intended to test, has not yet been implemented.
   * </p>
   *
   * <p>
   * Note: This method always completes abruptly with a <code>TestPendingException</code>. Thus it always has a side
   * effect. Methods with side effects are usually invoked with parentheses, as in <code>pending()</code>. This
   * method is defined as a parameterless method, in flagrant contradiction to recommended Scala style, because it 
   * forms a kind of DSL for pending tests. It enables tests in suites such as <code>FunSuite</code> or <code>FunSpec</code>
   * to be denoted by placing "<code>(pending)</code>" after the test name, as in:
   * </p>
   *
   * <pre class="stHighlight">
   * test("that style rules are not laws") (pending)
   * </pre>
   *
   * <p>
   * Readers of the code see "pending" in parentheses, which looks like a little note attached to the test name to indicate
   * it is pending. Whereas "<code>(pending())</code> looks more like a method call, "<code>(pending)</code>" lets readers
   * stay at a higher level, forgetting how it is implemented and just focusing on the intent of the programmer who wrote the code.
   * </p>
   */
  def pending: Assertion with PendingStatement = { throw new TestPendingException }

  /**
   * Execute the passed block of code, and if it completes abruptly, throw <code>TestPendingException</code>, else
   * throw <code>TestFailedException</code>.
   *
   * <p>
   * This method can be used to temporarily change a failing test into a pending test in such a way that it will
   * automatically turn back into a failing test once the problem originally causing the test to fail has been fixed.
   * At that point, you need only remove the <code>pendingUntilFixed</code> call. In other words, a
   * <code>pendingUntilFixed</code> surrounding a block of code that isn't broken is treated as a test failure.
   * The motivation for this behavior is to encourage people to remove <code>pendingUntilFixed</code> calls when
   * there are no longer needed.
   * </p>
   *
   * <p>
   * This method facilitates a style of testing in which tests are written before the code they test. Sometimes you may
   * encounter a test failure that requires more functionality than you want to tackle without writing more tests. In this
   * case you can mark the bit of test code causing the failure with <code>pendingUntilFixed</code>. You can then write more
   * tests and functionality that eventually will get your production code to a point where the original test won't fail anymore.
   * At this point the code block marked with <code>pendingUntilFixed</code> will no longer throw an exception (because the
   * problem has been fixed). This will in turn cause <code>pendingUntilFixed</code> to throw <code>TestFailedException</code>
   * with a detail message explaining you need to go back and remove the <code>pendingUntilFixed</code> call as the problem orginally
   * causing your test code to fail has been fixed.
   * </p>
   *
   * @param f a block of code, which if it completes abruptly, should trigger a <code>TestPendingException</code> 
   * @throws TestPendingException if the passed block of code completes abruptly with an <code>Exception</code> or <code>AssertionError</code>
   */
  def pendingUntilFixed(f: => Unit)(implicit pos: source.Position): Assertion with PendingStatement = {
    val isPending =
      try {
        f
        false
      }
      catch {
        case _: Exception => true
        case _: AssertionError => true
      }
      if (isPending)
        throw new TestPendingException
      else
        throw new TestFailedException((sde: StackDepthException) => Some(Resources.pendingUntilFixed), None, pos)
  }

  /**
   * The <code>Succeeded</code> singleton.
   *
   * <p>
   * You can use <code>succeed</code> to solve a type error when an async test 
   * does not end in either <code>Future[Assertion]</code> or <code>Assertion</code>.
   * Because <code>Assertion</code> is a type alias for <code>Succeeded.type</code>,
   * putting <code>succeed</code> at the end of a test body (or at the end of a
   * function being used to map the final future of a test body) will solve
   * the type error.
   * </p>
   */
  final val succeed: Assertion = Succeeded
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

  private[scalatest] def areEqualComparingArraysStructurally(left: Any, right: Any): Boolean = {
    // Prior to 2.0 this only called .deep if both sides were arrays. Loosened it
    // when nearing 2.0.M6 to call .deep if either left or right side is an array.
    // TODO: this is the same algo as in scalactic.DefaultEquality. Put that one in
    // a singleton and use it in both places.
    left match {
      case leftArray: Array[_] =>
        right match {
          case rightArray: Array[_] => deep(leftArray) == deep(rightArray)
          case rightNonEmptyArray: NonEmptyArray[_] => deep(leftArray) == deep(rightNonEmptyArray.toArray)
          case _ => deep(leftArray) == right
        }
      case leftNonEmptyArray: NonEmptyArray[_] =>
        right match {
          case rightArray: Array[_] => deep(leftNonEmptyArray.toArray) == deep(rightArray)
          case rightNonEmptyArray: NonEmptyArray[_] => deep(leftNonEmptyArray.toArray) == deep(rightNonEmptyArray.toArray)
          case _ => deep(leftNonEmptyArray.toArray) == right
        }

      case other => {
        right match {
          case rightArray: Array[_] => left == deep(rightArray)
          case rightNonEmptyArray: NonEmptyArray[_] => left == deep(rightNonEmptyArray.toArray)
          case _ => left == right
        }
      }
    }
  }

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
        throw newAssertionFailedException(append(failureMessage, clue), None, pos, bool.analysis)
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
