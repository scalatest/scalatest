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
 */
trait Expectations {
  
  /**
   * Expects that `actual` is equal to `expected` using default equality.
   *
   * @param expected the expected value
   * @param actual the actual value
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
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
   * Expects that `actual` is equal to `expected` using default equality.
   *
   * @param expected the expected value
   * @param clue an object whose <code>toString</code> method returns a message to be appended to the result [[Fact]]'s message
   * @param actual the actual value
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
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
   * Asserts that a block of code throws an exception of type `T`.
   *
   * @param f the block of code to be executed
   * @param classTag the class tag representing the exception type `T`
   * @param prettifier the prettifier used to pretty-print the values
   * @return an [[Expectation]] representing the result of the assertion
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
   * Expects that a boolean expression is `true`.
   *
   * @param expression the boolean expression to be evaluated
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expect(expression: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro ExpectationsMacro.expect

  /**
   * Expects that a boolean expression is `true`, message included in the returned [[Fact]] will be appended with <code>clue</code>'s <code>toString</code>.
   *
   * @param expression the boolean expression to be evaluated
   * @param clue An object whose <code>toString</code> method returns a message to be appended to the result [[Fact]]'s message
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expect(expression: Boolean, clue: Any)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro ExpectationsMacro.expectWithClue

  /**
   * Expects that a given code snippet does not compile.
   *
   * @param code the code snippet to be compiled
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expectDoesNotCompile(code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro CompileMacro.expectDoesNotCompileImpl

  /**
   * Expects that a given code snippet compiles successfully.
   *
   * @param code the code snippet to be compiled
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expectCompiles(code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro CompileMacro.expectCompilesImpl

  /**
   * Expects that a given code snippet results in a type error during compilation.
   *
   * @param code the code snippet to be compiled
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */
  def expectTypeError(code: String)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro CompileMacro.expectTypeErrorImpl

  import scala.language.implicitConversions

  /**
   * Implicitly converts a boolean expression to a [[Fact]] for assertion purposes, which makes (x &gt; 0) implies expect(x &gt; -1) syntax works
   *
   * @param expression the boolean expression to be evaluated
   * @param prettifier the prettifier used to pretty-print the values
   * @param pos the source position
   * @return a [[Fact]] representing the result of the assertion
   */  
  implicit def booleanToFact(expression: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Fact = macro ExpectationsMacro.expect
  
  /**
   * Implicitly converts an [[Expectation]] to an [[Assertion]].
   *
   * @param exp the expectation to be converted
   * @return an [[Assertion]] representing the result of the expectation
   */
  implicit def convertExpectationToAssertion(exp: Expectation): Assertion = exp.toAssertion
}


/**
 * The companion object for the `Expectation` trait.
 */
object Expectations extends Expectations {

  /**
   * A helper used by macro-generated code.
   */
  class ExpectationsHelper {

    import org.scalactic.Requirements.requireNonNull

    /**
     * A helper method for macro-generated assertions.
     *
     * @param bool the [[Bool]] object representing the assertion result
     * @param clue the clue to be used in case of failure
     * @param prettifier the prettifier used to pretty-print the values
     * @param pos the source position
     * @return a [[Fact]] representing the result of the assertion
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

  val expectationsHelper = new ExpectationsHelper
}
