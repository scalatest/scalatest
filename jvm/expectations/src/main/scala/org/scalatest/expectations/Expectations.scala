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
package org.scalatest
package expectations

import org.scalactic.{Resources => _, _}
import Fact._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.reflect.ClassTag

import org.scalactic.Requirements.requireNonNull

/**
 * <p>
 * Trait that contains ScalaTest's expectation methods, which result in a <a href="../Fact.html"><code>Fact</code></a> instead of throwing an exception.
 * </p>
 *
 * <p>
 * A <code>Fact</code> can be a <code>Yes</code> (success), a <code>No</code> (failure), or a <code>VacuousYes</code>
 * (vacuously true). You can compose facts using logical operators
 * (such as <code>&amp;&amp;</code>, <code>||</code>, <code>!</code>, <code>implies</code>, and <code>isEqvTo</code>),
 * and convert them to assertions by calling
 * <code>toAssertion</code> or by using them in contexts where an <code>Assertion</code> is expected.
 * </p>
 *
 * <p>
 * You can use the expectations provided by this trait as an alternative to ScalaTest's traditional assertions.
 * Expectations are useful when you want to compose assertions using logical operators.
 * This trait is designed to be mixed into <code>Suite</code> classes that include type <code>Assertion</code>,
 * as part of the expected test result type, such as <code>FunSuite</code>,
 * <code>FixtureFunSuite</code>, or <code>AsyncFunSuite</code>. (You can alternatively
 * import the methods defined in this trait. For details, see the documentation for the
 * <a href="Expectations$.html"><code>Expectations</code> companion object</a>.)
 *
 * When using <code>Expectations</code> in an "<code>Any</code>" style trait, such
 * as <code>AnyFunSuite</code>, you will need to remember to convert <code>Fact</code> to
 * <code>assertion</code> explicitly by calling <code>toAssertion</code>. It is best to use 
 * <code>Expectations</code> in style traits that do not include "<code>Any</code>" in their name,
 * because in those traits, the conversion from <code>Expectation</code> to <code>Assertion</code>
 * is performed automatically by an implicit conversion from <code>Expectation</code> to
 * <code>Assertion</code>.
 * </p>
 *
 * <p>
 * You can write expectations by invoking <code>expect</code> and passing in a <code>Boolean</code> expression,
 * such as:
 * </p>
 *
 * <pre class="stHighlight">
 * val result = 7
 * expect(result == 7) // Yes(7 equaled 7)
 * </pre>
 *
 * <p>
 * If the passed expression is <code>true</code>, <code>expect</code> will return a <code>Yes</code> fact. If <code>false</code>,
 * it will return a <code>No</code> fact. Unlike ScalaTest's <code>assert</code>, which throws <code>TestFailedException</code>
 * immediately, <code>expect</code> returns a value that you can compose with other facts or convert to an assertion later.
 * </p>
 *
 * <p>
 * If you pass the previous <code>Boolean</code> expression, <code>left == right</code>, to <code>expect</code> in a ScalaTest test,
 * the returned fact will contain detailed information. Because <code>expect</code> is implemented as a macro,
 * it includes the left and right values in its message. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * val left = 2
 * val right = 1
 * expect(left == right) // No(2 did not equal 1)
 * </pre>
 *
 * <p>
 * The <code>fact</code> will be a <code>No</code>, and if you convert it to an assertion with <code>fact.toAssertion</code>,
 * the detail message in the thrown <code>TestFailedException</code> will be: "2 did not equal 1".
 * </p>
 *
 * <p>
 * ScalaTest's <code>expect</code> macro works the same way as the <code>assert</code> macro, recognizing patterns in the AST
 * of the expression and giving helpful error messages for common expressions. Here are some examples, where <code>a</code> is 1,
 * <code>b</code> is 2, <code>c</code> is 3, <code>d</code> is 4, <code>xs</code> is <code>List(a, b, c)</code>,
 * and <code>num</code> is 1.0:
 * </p>
 *
 * <pre class="stHighlight">
 * expect(a == b || c &gt;= d)
 * // No(1 did not equal 2, and 3 was not greater than or equal to 4)
 *
 * expect(xs.exists(_ == 4))
 * // No(List(1, 2, 3) did not contain 4)
 *
 * expect("hello".startsWith("h") &amp;&amp; "goodbye".endsWith("y"))
 * // No("hello" started with "h", but "goodbye" did not end with "y")
 *
 * expect(num.isInstanceOf[Int])
 * // No(1.0 was not instance of scala.Int)
 *
 * expect(Some(2).isEmpty)
 * // No(Some(2) was not empty)
 * </pre>
 *
 * <p>
 * For expressions that are not recognized, the macro prints out a string
 * representation of the (desugared) AST and adds <code>"was false"</code>. Here are some examples of
 * error messages for unrecognized expressions:
 * </p>
 *
 * <pre class="stHighlight">
 * expect(None.isDefined)
 * // No(scala.None.isDefined was false)
 *
 * expect(xs.exists(i =&gt; i &gt; 10))
 * // No(xs.exists(((i: Int) =&gt; i.&gt;(10))) was false)
 * </pre>
 *
 * <p>
 * You can augment the standard fact message by providing a <code>String</code> as a second argument
 * to <code>expect</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val attempted = 2
 * expect(attempted == 1, "Execution was attempted " + attempted + " times instead of 1 time")
 * // No(2 did not equal 1 Execution was attempted 2 times instead of 1 time)
 * </pre>
 *
 * <p>
 * Using this form of <code>expect</code>, the fact message will be more specific to your problem domain, thereby
 * helping you debug the problem.
 * </p>
 *
 * <a name="expectedResults"></a>
 * <h2>Expected results</h2>
 *
 * <p>
 * Although the <code>expect</code> macro provides a natural, readable way to express expectations with
 * good error messages, as the operands become lengthy, the code becomes less readable. In addition, the error messages
 * generated for <code>==</code> comparisons don't distinguish between actual and expected values. The operands are just
 * called <code>left</code> and <code>right</code>, because if one were named <code>expected</code> and the other
 * <code>actual</code>, it would be difficult for people to remember which was which. To help with these limitations,
 * this trait includes a method called <code>expectResult</code> that can be used as an alternative to <code>expect</code>.
 * To use <code>expectResult</code>, you place the expected value in parentheses after <code>expectResult</code>, followed
 * by curly braces containing code that should result in the expected value. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * val a = 5
 * val b = 2
 * expectResult(2) {
 *   a - b
 * }
 * // No(Expected 2, but got 3)
 * </pre>
 *
 * <p>
 * In this case, the expected value is <code>2</code>, and the code being tested is <code>a - b</code>. The expectation
 * will fail, and the resulting <code>Fact</code> will be a <code>No</code> with the message: "Expected 2, but got 3".
 * </p>
 *
 * <a name="expectedExceptions"></a>
 * <h2>Expected exceptions</h2>
 *
 * <p>
 * Sometimes you need to test whether a method throws an expected exception under certain circumstances, such
 * as when invalid arguments are passed to the method. With expectations, you can use <code>expectThrows</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * val s = "hi"
 * val fact = expectThrows[IndexOutOfBoundsException] {
 *   s.charAt(-1)
 * }
 * </pre>
 *
 * <p>
 * If <code>charAt</code> throws an instance of <code>IndexOutOfBoundsException</code>,
 * <code>expectThrows</code> will return a <code>Yes</code> fact. But if <code>charAt</code> completes normally, or throws a different
 * exception, <code>expectThrows</code> will return a <code>No</code> fact.
 * </p>
 *
 * <a name="checkingThatCodeDoesNotCompile"></a>
 * <h2>Checking that a snippet of code does or does not compile</h2>
 *
 * <p>
 * Often when creating libraries you may wish to ensure that certain arrangements of code that
 * represent potential &ldquo;user errors&rdquo; do not compile, so that your library is more error resistant.
 * This trait includes the following syntax for that purpose:
 * </p>
 *
 * <pre class="stHighlight">
 * expectDoesNotCompile("val a: String = 1")
 * // Yes(val a: String = 1 did not compile)
 * </pre>
 *
 * <p>
 * If you want to ensure that a snippet of code does not compile because of a type error (as opposed
 * to a syntax error), use:
 * </p>
 *
 * <pre class="stHighlight">
 * expectTypeError("val a: String = 1")
 * // Yes(Got a type error as expected for code: val a: String = 1)
 * </pre>
 *
 * <p>
 * Note that the <code>expectTypeError</code> call will only succeed if the given snippet of code does not
 * compile because of a type error. A syntax error will still result in a <code>No</code> fact.
 * </p>
 *
 * <p>
 * If you want to state that a snippet of code <em>does</em> compile, you can make that
 * more obvious with:
 * </p>
 *
 * <pre class="stHighlight">
 * expectCompiles("val a: Int = 1")
 * // Yes(val a: Int = 1 compiled successfully)
 * </pre>
 *
 * <a name="composingFacts"></a>
 * <h2>Composing facts</h2>
 *
 * <p>
 * One of the key advantages of expectations over assertions is that you can compose facts using logical operators.
 * For example:
 * </p>
 *
 * <pre class="stHighlight">
 * val fact1 = expect(x &gt; 0)
 * val fact2 = expect(x &lt; 10)
 * val fact3 = fact1 &amp;&amp; fact2
 * fact3
 * // No(
 * //   Yes(12 was greater than 0) &&
 * //   No(12 was not less than 10)
 * // )
 * </pre>
 *
 * <p>
 * You can use <code>&amp;&amp;</code>, <code>&amp;</code>, <code>||</code>, <code>|</code>, <code>!</code>,
 * <code>implies</code>, and <code>isEqvTo</code> to combine facts. When you're ready to convert the composed fact
 * to an assertion, call <code>toAssertion</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * fact3.toAssertion
 * // org.scalatest.exceptions.TestFailedException: 12 was greater than 0, but 12 was not less than 10
 * //   at org.scalatest.Fact.toAssertion(Fact.scala:120)
 * //   ... 59 elided
 * </pre>
 *
 * <p>
 * For more information on composing facts, see the documentation for <a href="../Fact.html"><code>Fact</code></a>.
 * </p>
 *
 * @author Bill Venners
 * @author Chua Chee Seng
 */
trait Expectations {
  
  /**
   * Expect that the value passed as <code>expected</code> equals the value passed as <code>actual</code>.
   * If the <code>actual</code> value equals the <code>expected</code> value
   * (as determined by <code>==</code>), <code>expectResult</code> returns a <code>Yes</code> <code>Fact</code>.
   * Else, <code>expectResult</code> returns a <code>No</code> <code>Fact</code> that includes the expected and actual values.
   *
   * @param expected the expected value
   * @param actual the actual value, which should equal the passed <code>expected</code> value
   * @param prettifier an implicit <code>Prettifier</code> used to prettify error messages
   * @param pos an implicit <code>Position</code> that represents the source code location
   * @return a <code>Yes</code> <code>Fact</code> if the passed <code>actual</code> value equals the passed <code>expected</code> value, else a <code>No</code> <code>Fact</code>.
   */
  def expectResult(expected: Any)(actual: Any)(implicit prettifier: Prettifier, pos: source.Position): Fact = {
    if (!DefaultEquality.areEqualComparingArraysStructurally(actual, expected)) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val rawFactMessage = Resources.rawExpectedButGot
      val rawSimplifiedFactMessage = Resources.rawDidNotEqual
      val rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedButGot
      val rawMidSentenceSimplifiedFactMessage = Resources.rawDidNotEqual
      No(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act), 
        prettifier
      )
    }
    else {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val rawFactMessage = Resources.rawExpectedAndGot
      val rawSimplifiedFactMessage = Resources.rawEqualed
      val rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedAndGot
      val rawMidSentenceSimplifiedFactMessage = Resources.rawEqualed
      Yes(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act), 
        prettifier
      )
    }
  }

  /**
   * Expect that the value passed as <code>expected</code> equals the value passed as <code>actual</code>.
   * If the <code>actual</code> equals the <code>expected</code>
   * (as determined by <code>==</code>), <code>expectResult</code> returns a <code>Yes</code> <code>Fact</code>.
   * Else, <code>expectResult</code> returns a <code>No</code> <code>Fact</code> that includes the expected and actual values, as well as the <code>String</code>
   * obtained by invoking <code>toString</code> on the passed <code>clue</code>.
   *
   * @param expected the expected value
   * @param clue An object whose <code>toString</code> method returns a message to include in the <code>Fact</code>'s message.
   * @param actual the actual value, which should equal the passed <code>expected</code> value
   * @param prettifier an implicit <code>Prettifier</code> used to prettify error messages
   * @param pos an implicit <code>Position</code> that represents the source code location
   * @return a <code>Yes</code> <code>Fact</code> if the passed <code>actual</code> value equals the passed <code>expected</code> value, else a <code>No</code> <code>Fact</code>.
   */
  def expectResult(expected: Any, clue: Any)(actual: Any)(implicit prettifier: Prettifier, pos: source.Position): Fact = {
    requireNonNull(clue)
    if (!DefaultEquality.areEqualComparingArraysStructurally(actual, expected)) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val rawFactMessage = Resources.rawExpectedButGot
      val rawSimplifiedFactMessage = Resources.rawDidNotEqual
      val rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedButGot
      val rawMidSentenceSimplifiedFactMessage = Resources.rawDidNotEqual
      No(
        AppendedClues.appendClue(rawFactMessage, clue.toString()),
        AppendedClues.appendClue(rawSimplifiedFactMessage, clue.toString()),
        AppendedClues.appendClue(rawMidSentenceFactMessage, clue.toString()),
        AppendedClues.appendClue(rawMidSentenceSimplifiedFactMessage, clue.toString()),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act), 
        prettifier
      )
    }
    else {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val rawFactMessage = Resources.rawExpectedAndGot
      val rawSimplifiedFactMessage = Resources.rawEqualed
      val rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedAndGot
      val rawMidSentenceSimplifiedFactMessage = Resources.rawEqualed
      Yes(
        AppendedClues.appendClue(rawFactMessage, clue.toString()),
        AppendedClues.appendClue(rawSimplifiedFactMessage, clue.toString()),
        AppendedClues.appendClue(rawMidSentenceFactMessage, clue.toString()),
        AppendedClues.appendClue(rawMidSentenceSimplifiedFactMessage, clue.toString()),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act),
        Vector(exp, act), 
        prettifier
      )
    }
  }

  /**
   * Ensure that an expected exception is thrown by the passed function value. The thrown exception must be an instance of the
   * type specified by the type parameter of this method. This method invokes the passed
   * function. If the function throws an exception that's an instance of the specified type,
   * this method returns a <code>Yes</code> <code>Fact</code>. Else, whether the passed function returns normally
   * or completes abruptly with a different exception, this method returns a <code>No</code> <code>Fact</code>.
   *
   * <p>
   * Note that the type specified as this method's type parameter may represent any subtype of
   * <code>AnyRef</code>, not just <code>Throwable</code> or one of its subclasses. In
   * Scala, exceptions can be caught based on traits they implement, so it may at times make sense
   * to specify a trait that the expected exception's class must mix in. If a class instance is
   * passed for a type that could not possibly be used to catch an exception (such as <code>String</code>,
   * for example), this method will return a <code>No</code> <code>Fact</code>.
   * </p>
   *
   * @param f the function value that should throw the expected exception
   * @param classTag an implicit <code>ClassTag</code> representing the type of the specified
   * type parameter.
   * @param prettifier an implicit <code>Prettifier</code> used to prettify error messages
   * @return a <code>Yes</code> <code>Fact</code> if an exception of the expected type is thrown, else a <code>No</code> <code>Fact</code>.
   */
  def expectThrows[T <: AnyRef](f: => Any)(implicit classTag: ClassTag[T], prettifier: Prettifier): Expectation = {
    val clazz = classTag.runtimeClass
    try {
      f
      No(
        rawFactMessage = Resources.rawExceptionExpected,
        rawSimplifiedFactMessage = Resources.rawFactNoExceptionWasThrown,
        rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedExceptionWasThrown,
        rawMidSentenceSimplifiedFactMessage = Resources.rawMidSentenceFactNoExceptionWasThrown,
        factMessageArgs = Vector(clazz.getName),
        simplifiedFactMessageArgs = Vector.empty,
        midSentenceFactMessageArgs = Vector(clazz.getName),
        midSentenceSimplifiedFactMessageArgs = Vector.empty, 
        prettifier
      )
    }
    catch {
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass))
          No(
            rawFactMessage = Resources.rawWrongException,
            rawSimplifiedFactMessage = Resources.rawFactExceptionWasThrown,
            rawMidSentenceFactMessage = Resources.rawMidSentenceWrongException,
            rawMidSentenceSimplifiedFactMessage = Resources.rawMidSentenceFactExceptionWasThrown,
            factMessageArgs = Vector(clazz.getName, u.getClass.getName),
            simplifiedFactMessageArgs = Vector(u.getClass.getName),
            midSentenceFactMessageArgs = Vector(clazz.getName, u.getClass.getName),
            midSentenceSimplifiedFactMessageArgs = Vector(u.getClass.getName),
            cause = Some(u), 
            prettifier
          )
        else
          Yes(
            rawFactMessage = Resources.rawFactExceptionWasThrown,
            rawSimplifiedFactMessage = Resources.rawFactExceptionWasThrown,
            rawMidSentenceFactMessage = Resources.rawMidSentenceFactExceptionWasThrown,
            rawMidSentenceSimplifiedFactMessage = Resources.rawMidSentenceFactExceptionWasThrown,
            factMessageArgs = Vector(clazz.getName),
            simplifiedFactMessageArgs = Vector(clazz.getName),
            midSentenceFactMessageArgs = Vector(clazz.getName),
            midSentenceSimplifiedFactMessageArgs = Vector(clazz.getName),
            cause = Some(u),
            prettifier
          )
      }
    }
  }

  import language.experimental.macros

  /**
   * Expect that a boolean condition is true.
   * If the condition is <code>true</code>, this method returns a <code>Yes</code> <code>Fact</code>.
   * Else, it returns a <code>No</code> <code>Fact</code>.
   *
   * <p>
   * This method is implemented in terms of a Scala macro that will generate a more helpful error message
   * for expressions of this form:
   * </p>
   *
   * <ul>
   * <li>expect(a == b)</li>
   * <li>expect(a != b)</li>
   * <li>expect(a === b)</li>
   * <li>expect(a !== b)</li>
   * <li>expect(a &gt; b)</li>
   * <li>expect(a &gt;= b)</li>
   * <li>expect(a &lt; b)</li>
   * <li>expect(a &lt;= b)</li>
   * <li>expect(a startsWith "prefix")</li>
   * <li>expect(a endsWith "postfix")</li>
   * <li>expect(a contains "something")</li>
   * <li>expect(a eq b)</li>
   * <li>expect(a ne b)</li>
   * <li>expect(a &gt; 0 &amp;&amp; b &gt; 5)</li>
   * <li>expect(a &gt; 0 || b &gt; 5)</li>
   * <li>expect(a.isEmpty)</li>
   * <li>expect(!a.isEmpty)</li>
   * <li>expect(a.isInstanceOf[String])</li>
   * <li>expect(a.length == 8)</li>
   * <li>expect(a.size == 8)</li>
   * <li>expect(a.exists(_ == 8))</li>
   * </ul>
   *
   * <p>
   * At this time, any other form of expression will just get a <code>Fact</code> with message saying the given
   * expression was false.
   * </p>
   *
   * @param condition the boolean condition to expect
   * @param prettifier an implicit <code>Prettifier</code> used to prettify error messages
   * @param pos an implicit <code>Position</code> that represents the source code location
   * @return a <code>Yes</code> <code>Fact</code> if the condition is <code>true</code>, else a <code>No</code> <code>Fact</code>.
   */
  def expect(expression: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro ExpectationsMacro.expect

  /**
   * Expect that a boolean condition is true.
   * If the condition is <code>true</code>, this method returns a <code>Yes</code> <code>Fact</code>.
   * Else, it returns a <code>No</code> <code>Fact</code> that includes the <code>String</code> obtained
   * by invoking <code>toString</code> on the passed <code>clue</code> as well as information about the expression.
   *
   * <p>
   * This method is implemented in terms of a Scala macro that will generate a more helpful error message.
   * See the documentation for the parameterless <code>expect</code> method for the list of expressions that
   * will generate helpful error messages.
   * </p>
   *
   * @param condition the boolean condition to expect
   * @param clue An object whose <code>toString</code> method returns a message to include in the <code>Fact</code>'s message.
   * @param prettifier an implicit <code>Prettifier</code> used to prettify error messages
   * @param pos an implicit <code>Position</code> that represents the source code location
   * @return a <code>Yes</code> <code>Fact</code> if the condition is <code>true</code>, else a <code>No</code> <code>Fact</code>.
   */
  def expect(expression: Boolean, clue: Any)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro ExpectationsMacro.expectWithClue

  /**
   * Expect that a given string snippet of code does not pass either the Scala parser or type checker.
   *
   * <p>
   * Often when creating libraries you may wish to ensure that certain arrangements of code that
   * represent potential &ldquo;user errors&rdquo; do not compile, so that your library is more error resistant.
   * ScalaTest's <code>Expectations</code> trait includes the following syntax for that purpose:
   * </p>
   *
   * <pre class="stHighlight">
   * expectDoesNotCompile("val a: String = \"a string")
   * </pre>
   *
   * <p>
   * Although <code>expectDoesNotCompile</code> is implemented with a macro that determines at compile time whether
   * the snippet of code represented by the passed string doesn't compile, it returns a <code>Fact</code> at runtime
   * that indicates whether the snippet compiled or not. A <code>Yes</code> <code>Fact</code> is returned if the snippet
   * does not compile (<em>i.e.</em>, the expected behavior), and a <code>No</code> <code>Fact</code> is returned if the snippet
   * <em>does</em> compile (unexpected behavior).
   * </p>
   *
   * <p>
   * Note that the difference between <code>expectTypeError</code> and <code>expectDoesNotCompile</code> is
   * that <code>expectDoesNotCompile</code> will return a <code>Yes</code> <code>Fact</code> if the given code does not compile for any reason,
   * whereas <code>expectTypeError</code> will only return a <code>Yes</code> <code>Fact</code> if the given code does not compile because of
   * a type error. If the given code does not compile because of a syntax error, for example, <code>expectDoesNotCompile</code>
   * will return a <code>Yes</code> <code>Fact</code> but <code>expectTypeError</code> will return a <code>No</code> <code>Fact</code>.
   * </p>
   *
   * @param code the snippet of code that should not compile
   * @param prettifier an implicit <code>Prettifier</code> used to prettify error messages
   * @param pos an implicit <code>Position</code> that represents the source code location
   * @return a <code>Yes</code> <code>Fact</code> if the snippet does not compile, else a <code>No</code> <code>Fact</code>.
   */
  def expectDoesNotCompile(code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro CompileMacro.expectDoesNotCompileImpl

  /**
   * Expect that a given string snippet of code passes both the Scala parser and type checker.
   *
   * <p>
   * You can use this to make sure a snippet of code compiles:
   * </p>
   *
   * <pre class="stHighlight">
   * expectCompiles("val a: Int = 1")
   * </pre>
   *
   * <p>
   * Although <code>expectCompiles</code> is implemented with a macro that determines at compile time whether
   * the snippet of code represented by the passed string compiles, it returns a <code>Fact</code> at runtime
   * that indicates whether the snippet compiled or not. A <code>Yes</code> <code>Fact</code> is returned if the snippet
   * does compile (<em>i.e.</em>, the expected behavior), and a <code>No</code> <code>Fact</code> is returned if the snippet
   * <em>does not</em> compile (unexpected behavior).
   * </p>
   *
   * @param code the snippet of code that should compile
   * @param prettifier an implicit <code>Prettifier</code> used to prettify error messages
   * @param pos an implicit <code>Position</code> that represents the source code location
   * @return a <code>Yes</code> <code>Fact</code> if the snippet compiles, else a <code>No</code> <code>Fact</code>.
   */
  def expectCompiles(code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro CompileMacro.expectCompilesImpl

  /**
   * Expect that a given string snippet of code does not pass the Scala type checker, failing if the given snippet does not pass the Scala parser.
   *
   * <p>
   * Often when creating libraries you may wish to ensure that certain arrangements of code that
   * represent potential &ldquo;user errors&rdquo; do not compile, so that your library is more error resistant.
   * ScalaTest's <code>Expectations</code> trait includes the following syntax for that purpose:
   * </p>
   *
   * <pre class="stHighlight">
   * expectTypeError("val a: String = 1")
   * </pre>
   *
   * <p>
   * Although <code>expectTypeError</code> is implemented with a macro that determines at compile time whether
   * the snippet of code represented by the passed string type checks, it returns a <code>Fact</code> at runtime
   * that indicates whether the snippet type checked or not. A <code>Yes</code> <code>Fact</code> is returned if the snippet
   * does <em>not</em> type check (<em>i.e.</em>, the expected behavior), and a <code>No</code> <code>Fact</code> is returned if the snippet
   * <em>does</em> type check (unexpected behavior).
   * </p>
   *
   * <p>
   * Note that the difference between <code>expectTypeError</code> and <code>expectDoesNotCompile</code> is
   * that <code>expectDoesNotCompile</code> will return a <code>Yes</code> <code>Fact</code> if the given code does not compile for any reason,
   * whereas <code>expectTypeError</code> will only return a <code>Yes</code> <code>Fact</code> if the given code does not compile because of
   * a type error. If the given code does not compile because of a syntax error, for example, <code>expectDoesNotCompile</code>
   * will return a <code>Yes</code> <code>Fact</code> but <code>expectTypeError</code> will return a <code>No</code> <code>Fact</code>.
   * </p>
   *
   * @param code the snippet of code that should not type check
   * @param prettifier an implicit <code>Prettifier</code> used to prettify error messages
   * @param pos an implicit <code>Position</code> that represents the source code location
   * @return a <code>Yes</code> <code>Fact</code> if the snippet does not type check, else a <code>No</code> <code>Fact</code>.
   */
  def expectTypeError(code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro CompileMacro.expectTypeErrorImpl

  import scala.language.implicitConversions

  /**
   * Implicitly converts a <code>Boolean</code> value to a <code>Fact</code> for use in logical expressions.
   *
   * <p>
   * This implicit conversion enables the use of boolean expressions in logical fact compositions,
   * such as <code>(x &gt; 0) implies expect(x &gt; -1)</code>. The boolean value on the left side
   * of <code>implies</code> is implicitly converted to a <code>Fact</code> so it can be combined
   * with the <code>Fact</code> returned by <code>expect</code>.
   * </p>
   *
   * <p>
   * This method is implemented in terms of a Scala macro that will generate helpful error messages
   * similar to the <code>expect</code> macro.
   * </p>
   *
   * @param expression the boolean expression to be converted
   * @param prettifier an implicit <code>Prettifier</code> used to prettify error messages
   * @param pos an implicit <code>Position</code> that represents the source code location
   * @return a <code>Yes</code> <code>Fact</code> if the expression is <code>true</code>, else a <code>No</code> <code>Fact</code>.
   */
  implicit def booleanToFact(expression: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro ExpectationsMacro.expect
  
  /**
   * Implicitly converts an <code>Expectation</code> to an <code>Assertion</code>.
   *
   * <p>
   * This implicit conversion allows <code>Expectation</code> (which is a type alias for <code>Fact</code>)
   * to be used in contexts where an <code>Assertion</code> is expected. The conversion is performed by
   * calling the <code>toAssertion</code> method on the <code>Fact</code>, which returns <code>Succeeded</code>
   * if the fact is <code>Yes</code>, throws <code>TestFailedException</code> if the fact is <code>No</code>,
   * or throws <code>TestCanceledException</code> if the fact is <code>VacuousYes</code>.
   * </p>
   *
   * @param exp the <code>Expectation</code> (which is a <code>Fact</code>) to be converted
   * @param pos an implicit <code>Position</code> that represents the source code location
   * @return <code>Succeeded</code> if the <code>Expectation</code> is a <code>Yes</code> fact
   * @throws TestFailedException if the <code>Expectation</code> is a <code>No</code> fact
   * @throws TestCanceledException if the <code>Expectation</code> is a <code>VacuousYes</code> fact
   */
  implicit def convertExpectationToAssertion(exp: Expectation)(implicit pos: source.Position): Assertion = exp.toAssertion
}


/**
 * Companion object that facilitates the importing of <code>Expectations</code> members as
 * an alternative to mixing in the trait. One use case is to import <code>Expectations</code> members so you
 * can use them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.expectations.Expectations._
 * import org.scalatest.expectations.Expectations._
 * &nbsp;
 * scala&gt; expectResult(2) { 1 + 1 }
 * val res0: org.scalatest.Fact = Yes(Expected 2, and got 2)
 * &nbsp;
 * scala&gt; expectResult(3) { 1 + 3 }
 * val res1: org.scalatest.Fact = No(Expected 3, but got 4)
 * &nbsp;
 * scala&gt; val fact = expectResult(2) { 1 + 3 }
 * val fact: org.scalatest.Fact = No(Expected 2, but got 4)
 * &nbsp;
 * scala&gt; fact.toAssertion
 * org.scalatest.exceptions.TestFailedException: Expected 2, but got 4
 *      at org.scalatest.Fact.toAssertion(Fact.scala:...)
 *      at ... .&lt;init&gt;(&lt;console&gt;:...)
 * &nbsp;
 * scala&gt; val caught = expectThrows[StringIndexOutOfBoundsException] { "hi".charAt(-1) }
 * val caught: org.scalatest.Expectation = Yes(Exception "java.lang.StringIndexOutOfBoundsException" was thrown)
 * </pre>
 *
 * @author Bill Venners
 * @author Chua Chee Seng
 */
object Expectations extends Expectations {

  /**
   * Helper class used by code generated by the <code>expect</code> macro.
   */
  class ExpectationsHelper {

    import org.scalactic.Requirements.requireNonNull

    /**
     * Expect that the passed in <code>Bool</code> is <code>true</code>, else return a <code>No</code> with an error message.
     *
     * @param bool the <code>Bool</code> to check
     * @param clue optional clue to be included in the <code>Fact</code>'s error message when expectation fails
     * @param prettifier the <code>Prettifier</code> used to prettify objects in error messages
     * @param pos the source position
     * @return a <code>Yes</code> <code>Fact</code> if the <code>Bool</code> is <code>true</code>, else a <code>No</code> <code>Fact</code>
     */
    def macroExpect(bool: Bool, clue: Any, prettifier: Prettifier, pos: source.Position): Fact = {
      requireNonNull(clue)
      val result = 
        if (!bool.value)
          No(
            bool.rawFailureMessage,
            bool.rawFailureMessage,
            bool.rawFailureMessage,
            bool.rawFailureMessage,
            bool.failureMessageArgs,
            bool.failureMessageArgs,
            bool.failureMessageArgs,
            bool.failureMessageArgs, 
            prettifier
          )
        else
          Yes(
            bool.rawNegatedFailureMessage,
            bool.rawNegatedFailureMessage,
            bool.rawNegatedFailureMessage,
            bool.rawNegatedFailureMessage,
            bool.negatedFailureMessageArgs,
            bool.negatedFailureMessageArgs,
            bool.negatedFailureMessageArgs,
            bool.negatedFailureMessageArgs, 
            prettifier
          )
      if (clue == "") result else result.modifyMessage(_.map(ori => AppendedClues.appendClue(ori, clue.toString)))    
    }

  }

  /**
   * Helper instance used by code generated by macro expectations.
   */
  val expectationsHelper = new ExpectationsHelper
}
