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

import exceptions.TestCanceledException
import scala.reflect.Manifest
import Assertions.areEqualComparingArraysStructurally
import org.scalautils.LegacyTripleEquals

/**
 * Trait that contains ScalaTest's basic assertion methods.
 *
 * <p>
 * You can use the assertions provided by this trait in any ScalaTest <code>Suite</code>, because <code>Suite</code>
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
 * Scala source file. This <code>Assertions</code> traits defines another <code>assert</code> method that hides the
 * one in <code>Predef</code>. It behaves the same, except that if <code>false</code> is passed it throws
 * <code>TestFailedException</code> instead of <code>AssertionError</code>. The reason it throws <code>TestFailedException</code>
 * is because <code>TestFailedException</code> carries information about exactly which item in the stack trace represents
 * the line of test code that failed, which can help users more quickly find an offending line of code in a failing test.
 * <p>
 *
 * <p>
 * If you pass the previous <code>Boolean</code> expression, <code>left == right</code> to <code>assert</code> in a ScalaTest test, a failure
 * will be reported, but without reporting the left and right values. You can alternatively encode these values in a <code>String</code> passed as
 * a second argument to <code>assert</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * val left = 2
 * val right = 1
 * assert(left == right, left + " did not equal " + right)
 * </pre>
 *
 * <p>
 * Using this form of <code>assert</code>, the failure report will include the left and right values, thereby
 * helping you debug the problem. However, ScalaTest provides the <code>===</code> operator to make this easier.
 * You use it like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val left = 2
 * val right = 1
 * assert(left === right)
 * </pre>
 *
 * <p>
 * Because you use <code>===</code> here instead of <code>==</code>, the failure report will include the left
 * and right values. For example, the detail message in the thrown <code>TestFailedException</code> from the <code>assert</code>
 * shown previously will include, "2 did not equal 1".
 * From this message you will know that the operand on the left had the value 2, and the operand on the right had the value 1.
 * </p>
 *
 * <p>
 * If you're familiar with JUnit, you would use <code>===</code>
 * in a ScalaTest <code>Suite</code> where you'd use <code>assertEquals</code> in a JUnit <code>TestCase</code>.
 * The <code>===</code> operator is made possible by an implicit conversion from <code>Any</code>
 * to <code>Equalizer</code>. If you're curious to understand the mechanics, see the <a href="Assertions$Equalizer.html">documentation for
 * <code>Equalizer</code></a> and the <code>convertToEqualizer</code> method.
 * </p>
 *
 * <h2>Expected results</h2>
 *
 * Although <code>===</code> provides a natural, readable extension to Scala's <code>assert</code> mechanism,
 * as the operands become lengthy, the code becomes less readable. In addition, the <code>===</code> comparison
 * doesn't distinguish between actual and expected values. The operands are just called <code>left</code> and <code>right</code>,
 * because if one were named <code>expected</code> and the other <code>actual</code>, it would be difficult for people to
 * remember which was which. To help with these limitations of assertions, <code>Suite</code> includes a method called <code>expectResult</code> that
 * can be used as an alternative to <code>assert</code> with <code>===</code>. To use <code>expectResult</code>, you place
 * the expected value in parentheses after <code>expectResult</code>, followed by curly braces containing code
 * that should result in the expected value. For example:
 *
 * <pre class="stHighlight">
 * val a = 5
 * val b = 2
 * expectResult(2) {
 *   a - b
 * }
 * </pre>
 *
 * <p>
 * In this case, the expected value is <code>2</code>, and the code being tested is <code>a - b</code>. This expectation will fail, and
 * the detail message in the <code>TestFailedException</code> will read, "Expected 2, but got 3."
 * </p>
 *
 * <h2>Forcing failures</h2>
 *
 * <p>
 * If you just need the test to fail, you can write:
 * </p>
 *
 * <pre class="stHiglight">
 * fail()
 * </pre>
 *
 * <p>
 * Or, if you want the test to fail with a message, write:
 * </p>
 *
 * <pre class="stHiglight">
 * fail("I've got a bad feeling about this")
 * </pre>
 *
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
 *   case _: IndexOutOfBoundsException => // Expected, so continue
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
 * <code>assume</code> methods throw <code>TestCanceledException</code> whereas the
 * <code>assert</code> methods throw <code>TestFailedException</code>. As with <code>assert</code>,
 * <code>assume</code> hides a Scala method in <code>Predef</code> that performs a similar
 * function, but throws <code>AssertionError</code>. And just as you can with <code>assert</code>,
 * you can optionally provide a clue string, or use <code>===</code> to get a more detailed
 * error message. Here are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * assume(database.isAvailable, "The database was down again")
 * assume(database.getAllUsers.count === 9)
 * </pre>
 *
 * <h2>Forcing cancelations</h2>
 *
 * <p>
 * For each overloaded <code>fail</code> method, there's a corresponding <code>cancel</code> method
 * with an identical signature and behavior, except the <code>cancel</code> methods throw
 * <code>TestCanceledException</code> whereas the <code>fail</code> methods throw
 * <code>TestFailedException</code>. Thus if you just need to cancel a test, you can write:
 * </p>
 *
 * <pre class="stHiglight">
 * cancel()
 * </pre>
 *
 * <p>
 * If you want to cancel the test with a message, just place the message in the parentheses:
 * </p>
 *
 * <pre class="stHiglight">
 * cancel("Can't run the test because no internet connection was found")
 * </pre>
 *
 * <h2>Getting a clue</h2>
 *
 * <p>
 * If you want more information that is provided by default by the methods if this trait,
 * you can supply a "clue" string in one of several ways.
 * The extra information (or "clues") you provide will
 * be included in the detail message of the thrown exception. Both
 * <code>assert</code> and <code>expect</code> provide a way for a clue to be
 * included directly, <code>intercept</code> does not.
 * Here's an example of clues provided directly in <code>assert</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(1 + 1 === 3, "this is a clue")
 * </pre>
 *
 * <p>
 * and in <code>expect</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * expect(3, "this is a clue") { 1 + 1 }
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
 * @author Bill Venners
 */
trait Assertions extends LegacyTripleEquals {

  /* *
   * Class used via an implicit conversion to enable any two objects to be compared with
   * <code>===</code> in assertions in tests. For example:
   *
   * <pre class="stHighlight">
   * assert(a === b)
   * </pre>
   *
   * <p>
   * The benefit of using <code>assert(a === b)</code> rather than <code>assert(a == b)</code> is
   * that a <code>TestFailedException</code> produced by the former will include the values of <code>a</code> and <code>b</code>
   * in its detail message.
   * The implicit method that performs the conversion from <code>Any</code> to <code>Equalizer</code> is
   * <code>convertToEqualizer</code> in trait <code>Assertions</code>.
   * </p>
   *
   * <p>
   * In case you're not familiar with how implicit conversions work in Scala, here's a quick explanation.
   * The <code>convertToEqualizer</code> method in <code>Assertions</code> is defined as an "implicit" method that takes an
   * <code>Any</code>, which means you can pass in any object, and it will convert it to an <code>Equalizer</code>.
   * The <code>Equalizer</code> has <code>===</code> defined. Most objects don't have <code>===</code> defined as a method
   * on them. Take two Strings, for example:
   * </p>
   *
   * <pre class="stHighlight">
   * assert("hello" === "world")
   * </pre>
   *
   * <p>
   * Given this code, the Scala compiler looks for an <code>===</code> method on class <code>String</code>, because that's the class of
   * <code>"hello"</code>. <code>String</code> doesn't define <code>===</code>, so the compiler looks for an implicit conversion from
   * <code>String</code> to something that does have an <code>===</code> method, and it finds the <code>convertToEqualizer</code> method. It
   * then rewrites the code to this:
   * </p>
   *
   * <pre class="stHighlight">
   * assert(convertToEqualizer("hello").===("world"))
   * </pre>
   *
   * <p>
   * So inside a <code>Suite</code> (which mixes in <code>Assertions</code>, <code>===</code> will work on anything. The only
   * situation in which the implicit conversion wouldn't 
   * happen is on types that have an <code>===</code> method already defined.
   * </p>
   * 
   * <p>
   * The primary constructor takes one object, <code>left</code>, whose type is being converted to <code>Equalizer</code>. The <code>left</code>
   * value may be a <code>null</code> reference, because this is allowed by Scala's <code>==</code> operator.
   * </p>
   *
   * @param left An object to convert to <code>Equalizer</code>, which represents the <code>left</code> value
   *     of an assertion.
   *
   * @author Bill Venners
   */
/*
  final class Equalizer(left: Any) {

    /**
     * The <code>===</code> operation compares this <code>Equalizer</code>'s <code>left</code> value (passed
     * to the constructor, usually via an implicit conversion) with the passed <code>right</code> value 
     * for equality as determined by the expression <code>left == right</code>.
     * If <code>true</code>, <code>===</code> returns <code>None</code>. Else, <code>===</code> returns
     * a <code>Some</code> whose <code>String</code> value indicates the <code>left</code> and <code>right</code> values.
     *
     * <p>
     * In its typical usage, the <code>Option[String]</code> returned by <code>===</code> will be passed to one of two
     * of trait <code>Assertion</code>' overloaded <code>assert</code> methods. If <code>None</code>,
     * which indicates the assertion succeeded, <code>assert</code> will return normally. But if <code>Some</code> is passed,
     * which indicates the assertion failed, <code>assert</code> will throw a <code>TestFailedException</code> whose detail
     * message will include the <code>String</code> contained inside the <code>Some</code>, which in turn includes the
     * <code>left</code> and <code>right</code> values. This <code>TestFailedException</code> is typically embedded in a 
     * <code>Report</code> and passed to a <code>Reporter</code>, which can present the <code>left</code> and <code>right</code>
     * values to the user.
     * </p>
     */
    def ===(right: Any) =
      if (areEqualComparingArraysStructurally(left, right))
        None
      else {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        Some(FailureMessages("didNotEqual", leftee, rightee))
      }
/*
    def !==(right: Any) =
      if (left != right)
        None
      else {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        Some(FailureMessages("equaled", leftee, rightee))
      }
*/
  }
*/

  /**
   * Assert that a boolean condition is true.
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>TestFailedException</code>.
   *
   * @param condition the boolean condition to assert
   * @throws TestFailedException if the condition is <code>false</code>.
   */
  def assert(condition: Boolean) {
    if (!condition)
      throw newAssertionFailedException(None, None, 4)
  }

  private[scalatest] def newAssertionFailedException(optionalMessage: Option[Any], optionalCause: Option[Throwable], stackDepth: Int): Throwable =
    (optionalMessage, optionalCause) match {
      case (None, None) => new TestFailedException(stackDepth)
      case (None, Some(cause)) => new TestFailedException(cause, stackDepth)
      case (Some(message), None) => new TestFailedException(message.toString, stackDepth)
      case (Some(message), Some(cause)) => new TestFailedException(message.toString, cause, stackDepth)
    }

  private def newTestCanceledException(optionalMessage: Option[Any], optionalCause: Option[Throwable], stackDepth: Int): Throwable =
    (optionalMessage, optionalCause) match {
      case (None, None) => new TestCanceledException(stackDepth)
      case (None, Some(cause)) => new TestCanceledException(cause, stackDepth)
      case (Some(message), None) => new TestCanceledException(message.toString, stackDepth)
      case (Some(message), Some(cause)) => new TestCanceledException(message.toString, cause, stackDepth)
    }

  /**
   * Assert that a boolean condition, described in <code>String</code>
   * <code>message</code>, is true.
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>TestFailedException</code> with the
   * <code>String</code> obtained by invoking <code>toString</code> on the
   * specified <code>clue</code> as the exception's detail message.
   *
   * @param condition the boolean condition to assert
   * @param clue An objects whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestFailedException if the condition is <code>false</code>.
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
  def assert(condition: Boolean, clue: Any) {
    if (!condition)
      throw newAssertionFailedException(Some(clue), None, 4)
  }

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
   * For more information on how this mechanism works, see the <a href="Assertions$Equalizer.html">documentation for
   * <code>Equalizer</code></a>.
   * </p>
   *
   * @param o the <code>Option[String]</code> to assert
   * @param clue An objects whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestFailedException if the <code>Option[String]</code> is <code>Some</code>.
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
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
   * For more information on how this mechanism works, see the <a href="Assertions$Equalizer.html">documentation for
   * <code>Equalizer</code></a>.
   * </p>
   *
   * @param o the <code>Option[String]</code> to assert
   * @throws TestFailedException if the <code>Option[String]</code> is <code>Some</code>.
   */
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
   * @param condition the boolean condition to assert
   * @throws TestCanceledException if the condition is <code>false</code>.
   */
  def assume(condition: Boolean) {
    if (!condition)
      throw newTestCanceledException(None, None, 3)
  }

  
  /**
   * Assume that a boolean condition, described in <code>String</code>
   * <code>message</code>, is true.
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>TestCanceledException</code> with the
   * <code>String</code> obtained by invoking <code>toString</code> on the
   * specified <code>clue</code> as the exception's detail message.
   *
   * @param condition the boolean condition to assume
   * @param clue An objects whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestFailedException if the condition is <code>false</code>.
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
  def assume(condition: Boolean, clue: Any) {
    if (!condition)
      throw newTestCanceledException(Some(clue.toString), None, 3)
  }

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
   * For more information on how this mechanism works, see the <a href="Assertions$Equalizer.html">documentation for
   * <code>Equalizer</code></a>.
   * </p>
   *
   * @param o the <code>Option[String]</code> to assert
   * @param clue An objects whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestCanceledException if the <code>Option[String]</code> is <code>Some</code>.
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
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
   * assert(a === b)
   * </pre>
   *
   * <p>
   * For more information on how this mechanism works, see the <a href="Assertions$Equalizer.html">documentation for
   * <code>Equalizer</code></a>.
   * </p>
   *
   * @param o the <code>Option[String]</code> to assert
   * @throws TestFailedException if the <code>Option[String]</code> is <code>Some</code>.
   */
  // def assume(o: Option[String]) = throwIfSome(o, (a: Any) => newTestCanceledException(Some(a.toString), None, 3))
  def assume(o: Option[String]) {
    o match {
      case Some(s) => throw newTestCanceledException(Some(s), None, 3)
      case None =>
    }
  }

/*
  def throwIfSome(o: Option[String], exception: (Any) => Throwable) {
    o match {
      case Some(s) => throw exception(s)
      case None =>
    }
  }
*/
  

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
      case Some(e) => e.asInstanceOf[T] // I know this cast will succeed, becuase iSAssignableFrom succeeded above
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
    if (actual != expected) {
      val (act, exp) = Suite.getObjectsForFailureMessage(actual, expected)
      val s = FailureMessages("expectedButGot", exp, act)
      throw newAssertionFailedException(Some(clue + "\n" + s), None, 4)
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
   * This <code>expect</code> method has been deprecated; Please use <code>expectResult</code> instead.
   *
   * <p>
   * To get rid of the deprecation warning, simply replace <code>expect</code> with
   * <code>expectResult</code>. The name <code>expect</code> will be used for a different purposes in
   * a future version of ScalaTest.
   * </p>
   */
  @deprecated("This expect method has been deprecated. Please replace all invocations of expect with an identical invocation of expectResult instead.")
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
    if (actual != expected) {
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
   * This <code>expect</code> method has been deprecated; Please use <code>expectResult</code> instead.
   *
   * <p>
   * To get rid of the deprecation warning, simply replace <code>expect</code> with
   * <code>expectResult</code>. The name <code>expect</code> will be used for a different purposes in
   * a future version of ScalaTest.
   * </p>
   */
  @deprecated("This expect method has been deprecated. Please replace all invocations of expect with an identical invocation of expectResult instead.")
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
      fun
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
 * scala> import org.scalatest.Assertions._
 * import org.scalatest.Assertions._
 * &nbsp;
 * scala> assert(1 === 2)
 * org.scalatest.TestFailedException: 1 did not equal 2
 * 	at org.scalatest.Assertions$class.assert(Assertions.scala:211)
 * 	at org.scalatest.Assertions$.assert(Assertions.scala:511)
 * 	at .<init>(<console>:7)
 * 	at .<clinit>(<console>)
 * 	at RequestResult$.<init>(<console>:3)
 * 	at RequestResult$.<clinit>(<console>)
 * 	at RequestResult$result(<console>)
 * 	at sun.reflect.NativeMethodAccessorImpl.invoke...
 *&nbsp;
 * scala> expect(3) { 1 + 3 }
 * org.scalatest.TestFailedException: Expected 3, but got 4
 * 	at org.scalatest.Assertions$class.expect(Assertions.scala:447)
 * 	at org.scalatest.Assertions$.expect(Assertions.scala:511)
 * 	at .<init>(<console>:7)
 * 	at .<clinit>(<console>)
 * 	at RequestResult$.<init>(<console>:3)
 * 	at RequestResult$.<clinit>(<console>)
 * 	at RequestResult$result(<console>)
 * 	at sun.reflect.NativeMethodAccessorImpl.in...
 *&nbsp;
 * scala> val caught = intercept[StringIndexOutOfBoundsException] { "hi".charAt(-1) }
 * caught: StringIndexOutOfBoundsException = java.lang.StringIndexOutOfBoundsException: String index out of range: -1
 * </pre>
 *
 * @author Bill Venners
 */
object Assertions extends Assertions {
  private[scalatest] def areEqualComparingArraysStructurally(left: Any, right: Any) = {
      left match {
        case leftArray: Array[_] =>
          right match {
            case rightArray: Array[_] => leftArray.deep.equals(rightArray.deep)
            case _ => left == right
        }
        case _ => left == right
    }
  }
}
