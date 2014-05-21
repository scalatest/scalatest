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

import java.lang.annotation._
import java.io.Serializable
import java.lang.reflect.Constructor
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import Suite.simpleNameForTest
import Suite.parseSimpleName
import Suite.stripDollars
import Suite.formatterForSuiteStarting
import Suite.formatterForSuiteCompleted
import Suite.checkForPublicNoArgConstructor
import Suite.checkChosenStyles
import Suite.formatterForSuiteAborted
import Suite.anExceptionThatShouldCauseAnAbort
import Suite.getSimpleNameOfAnObjectsClass
import Suite.takesInformer
import Suite.handleFailedTest
import Suite.isTestMethodGoodies
import Suite.testMethodTakesAnInformer
import scala.collection.immutable.TreeSet
import Suite.getIndentedTextForTest
import Suite.getEscapedIndentedTextForTest
import Suite.getTopOfClass
import Suite.getTopOfMethod
import Suite.getTopOfMethod
import Suite.getSuiteRunTestGoodies
import Suite.autoTagClassAnnotations
import org.scalatest.events._
import org.scalatest.tools.StandardOutReporter
import Suite.getMessageForException
import Suite.reportTestStarting
import Suite.reportTestIgnored
import Suite.reportTestSucceeded
import Suite.reportTestPending
import Suite.reportTestCanceled
import Suite.reportInfoProvided
import Suite.createInfoProvided
import Suite.createMarkupProvided
import Suite.wrapReporterIfNecessary
import Suite.getMethodForTestName
import scala.reflect.NameTransformer
import tools.SuiteDiscoveryHelper
import tools.Runner
import exceptions.StackDepthExceptionHelper.getStackDepthFun
import exceptions._
import exceptions._
import collection.mutable.ListBuffer
import collection.GenTraversable
import annotation.tailrec
import collection.immutable
import OutcomeOf.outcomeOf
import org.scalactic.Prettifier

/*
 * <h2>Using <code>info</code> and <code>markup</code></h2>
 *
 * <p>
 * One of the parameters to <code>Suite</code>'s <code>run</code> method is a <code>Reporter</code>, which
 * will collect and report information about the running suite of tests.
 * Information about suites and tests that were run, whether tests succeeded or failed, 
 * and tests that were ignored will be passed to the <code>Reporter</code> as the suite runs.
 * Most often the reporting done by default will be sufficient, but
 * occasionally you may wish to provide custom information to the <code>Reporter</code> from a test.
 * For this purpose, an <a href="Informer.html"><code>Informer</code></a> that will forward information
 * to the current <code>Reporter</code> is provided via the <code>info</code> parameterless method.
 * You can pass the extra information to the <code>Informer</code> via its <code>apply</code> method.
 * The <code>Informer</code> will then pass the information to the <code>Reporter</code> via an <code>InfoProvided</code> event.
 * Here's an example that shows both a direct use as well as an indirect use through the methods
 * of <a href="GivenWhenThen.html"><code>GivenWhenThen</code></a>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.info
 *
 * import collection.mutable
 * import org.scalatest._
 * 
 * class SetSuite extends Suite with GivenWhenThen {
 *
 *   def &#96;test: an element can be added to an empty mutable Set&#96; {
 *
 *     given("an empty mutable Set")
 *     val set = mutable.Set.empty[String]
 *
 *     when("an element is added")
 *     set += "clarity"
 *
 *     then("the Set should have size 1")
 *     assert(set.size === 1)
 *
 *     and("the Set should contain the added element")
 *     assert(set.contains("clarity"))
 *
 *     info("That's all folks!")
 *   }
 * }
 * </pre>
 *
 * If you run this <code>Suite</code> from the interpreter, you will see the messages
 * included in the output:
 *
 * <pre class="stREPL">
 * scala&gt; new SetSuite execute
 * <span class="stGreen">SetSuite:
 * - an element can be added to an empty mutable Set
 *   + Given an empty mutable Set
 *   + When an element is added
 *   + Then the Set should have size 1
 *   + And the Set should contain the added element
 *   + That's all folks!</span>
 * </pre>
 *
 * <p>
 * Trait <code>Suite</code> also carries a <a href="Documenter.html"><code>Documenter</code></a> named <code>markup</code>, which
 * you can use to transmit markup text to the <code>Reporter</code>.
 * </p>
 * --------------
 * <h2>Assertions and <code>=</code><code>=</code><code>=</code></h2>
 *
 * <p>
 * Inside test methods in a <code>Suite</code>, you can write assertions by invoking <code>assert</code> and passing in a <code>Boolean</code> expression,
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
 * <code>assert</code> will complete abruptly with a <code>TestFailedException</code>. This exception is usually not caught
 * by the test method, which means the test method itself will complete abruptly by throwing the <code>TestFailedException</code>. Any
 * test method that completes abruptly with an exception is considered a failed
 * test. A test method that returns normally is considered a successful test.
 * </p>
 *
 * <p>
 * If you pass a <code>Boolean</code> expression to <code>assert</code>, a failed assertion will be reported, but without
 * reporting the left and right values. You can alternatively encode these values in a <code>String</code> passed as
 * a second argument to <code>assert</code>, as in:
 * </p>
 * 
 * <pre class="stHighlight">
 * val left = 2
 * val right = 1
 * assert(left == right, left + " did not equal " + right)
 * </pre>
 *
 * <p>
 * Using this form of <code>assert</code>, the failure report will include the left and right values, 
 * helping you debug the problem. However, ScalaTest provides the <code>===</code> operator to make this easier.
 * (The <code>===</code> operator is defined in trait <a href="Assertions.html"><code>Assertions</code></a> which trait <code>Suite</code> extends.)
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
 * remember which was which. To help with these limitations of assertions, <code>Suite</code> includes a method called <code>assertResult</code> that
 * can be used as an alternative to <code>assert</code> with <code>===</code>. To use <code>assertResult</code>, you place
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
 * In this case, the expected value is <code>2</code>, and the code being tested is <code>a - b</code>. This expectation will fail, and
 * the detail message in the <code>TestFailedException</code> will read, "Expected 2, but got 3."
 * </p>
 *
 * <h2>Intercepted exceptions</h2>
 *
 * <p>
 * Sometimes you need to test whether a method throws an expected exception under certain circumstances, such
 * as when invalid arguments are passed to the method. You can do this in the JUnit style, like this:
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
 * the next statement, <code>fail()</code>, will be executed. The <code>fail</code> method always completes abruptly with
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
 * exception, <code>intercept</code> will complete abruptly with a <code>TestFailedException</code>. The <code>intercept</code> method returns the
 * caught exception so that you can inspect it further if you wish, for example, to ensure that data contained inside
 * the exception has the expected values. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val s = "hi"
 * val caught =
 *   intercept[IndexOutOfBoundsException] {
 *     s.charAt(-1)
 *   }
 * assert(caught.getMessage === "String index out of range: -1")
 * </pre>
 *
 * <h2>Using matchers and other assertions</h2>
 *
 * <p>
 * ScalaTest also supports another style of assertions via its matchers DSL. By mixing in
 * trait <a href="matchers/ShouldMatchers.html"><code>ShouldMatchers</code></a>, you can 
 * write suites that look like:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.matchers
 *
 * import org.scalatest._
 *
 * class SetSuite extends Suite with ShouldMatchers {
 *
 *   def &#96;test: an empty Set should have size 0&#96; {
 *     Set.empty.size should equal (0)
 *   }
 *
 *   def &#96;test: invoking head on an empty Set should produce NoSuchElementException&#96; {
 *     evaluating { Set.empty.head } should produce [NoSuchElementException]
 *   }
 * }
 * </pre>
 * 
 * <p>If you prefer the word "<code>must</code>" to the word "<code>should</code>," you can alternatively mix in
 * trait <a href="matchers/MustMatchers.html"><code>MustMatchers</code></a>.
 * </p>
 *
 * <p>
 * If you are comfortable with assertion mechanisms from other test frameworks, chances
 * are you can use them with ScalaTest. Any assertion mechanism that indicates a failure with an exception
 * can be used as is with ScalaTest. For example, to use the <code>assertEquals</code>
 * methods provided by JUnit or TestNG, simply import them and use them. (You will of course need
 * to include the relevant JAR file for the framework whose assertions you want to use on either the
 * classpath or runpath when you run your tests.) 
 * </p>
 */

/**
 * A suite of tests. A <code>Suite</code> instance encapsulates a conceptual
 * suite (<em>i.e.</em>, a collection) of tests.
 *
 * <p>
 * This trait provides an interface composed of "lifecycle methods" that allow suites of tests to be run.
 * Its implementation enables a default way of writing and executing tests.  Subtraits and subclasses can
 * override <code>Suite</code>'s lifecycle methods to enable other ways of writing and executing tests.
 * </p>
 *
 * <p>
 * <strong>Prior to ScalaTest 2.0.M4, trait <code>Suite</code> served two purposes: 1) It served as the base
 * class of ScalaTest's family of style traits, and 2) It was itself a style trait in which tests are methods.
 * Although it will continue to serve its first purpose, it has been deprecated as a style trait. Pre-existing code
 * that used <code>Suite</code> as a style trait to define tests as methods will continue to work during the
 * deprecation period, but will generate a deprecation warning. Please change all such uses of <code>Suite</code>
 * to use trait <a href="Spec.html"><code>Spec</code></a> instead.</strong>
 * </p>
 *
 * <h2>Nested suites</h2>
 *
 * <p>
 * A <code>Suite</code> can refer to a collection of other <code>Suite</code>s,
 * which are called <em>nested</em> <code>Suite</code>s. Those nested  <code>Suite</code>s can in turn have
 * their own nested  <code>Suite</code>s, and so on. Large test suites can be organized, therefore, as a tree of
 * nested <code>Suite</code>s.
 * This trait's <code>run</code> method, in addition to invoking its
 * test methods, invokes <code>run</code> on each of its nested <code>Suite</code>s.
 * </p>
 *
 * <p>
 * A <code>List</code> of a <code>Suite</code>'s nested <code>Suite</code>s can be obtained by invoking its
 * <code>nestedSuites</code> method. If you wish to create a <code>Suite</code> that serves as a
 * container for nested <code>Suite</code>s, whether or not it has test methods of its own, simply override <code>nestedSuites</code>
 * to return a <code>List</code> of the nested <code>Suite</code>s. Because this is a common use case, ScalaTest provides
 * a convenience <code>Suites</code> class, which takes a variable number of nested <code>Suite</code>s as constructor
 * parameters. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.nested
 *
 * import org.scalatest._
 *
 * class ASuite extends FunSuite {
 *   test("A should have ASCII value 41 hex") {
 *     assert('A' === 0x41)
 *   }
 *   test("a should have ASCII value 61 hex") {
 *     assert('a' === 0x61)
 *   }
 * }
 * class BSuite extends FunSuite {
 *   test("B should have ASCII value 42 hex") {
 *     assert('B' === 0x42)
 *   }
 *   test("b should have ASCII value 62 hex") {
 *     assert('b' === 0x62)
 *   }
 * }
 * class CSuite extends FunSuite {
 *   test("C should have ASCII value 43 hex") {
 *     assert('C' === 0x43)
 *   }
 *   test("c should have ASCII value 63 hex") {
 *     assert('c' === 0x63)
 *   }
 * }
 *
 * class ASCIISuite extends Suites(
 *   new ASuite,
 *   new BSuite,
 *   new CSuite
 * )
 * </pre>
 *
 * <p>
 * If you now run <code>ASCIISuite</code>:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ASCIISuite execute
 * </pre>
 *
 * <p>
 * You will see reports printed to the standard output that indicate the nested
 * suites&#8212;<code>ASuite</code>, <code>BSuite</code>, and
 * <code>CSuite</code>&#8212;were run:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">ASCIISuite:
 * ASuite:
 * - A should have ASCII value 41 hex
 * - a should have ASCII value 61 hex
 * BSuite:
 * - B should have ASCII value 42 hex
 * - b should have ASCII value 62 hex
 * CSuite:
 * - C should have ASCII value 43 hex
 * - c should have ASCII value 63 hex</span>
 * </pre>
 *
 * <p>
 * Note that <code>Runner</code> can discover <code>Suite</code>s automatically, so you need not
 * necessarily define nested <code>Suites</code> explicitly. See the <a href="tools/Runner$.html#membersOnlyWildcard">documentation
 * for <code>Runner</code></a> for more information.
 * </p>
 *
 * <a name="configMapSection"></a><h2>The config map</h2>
 *
 * <p>
 * In some cases you may need to pass information to a suite of tests.
 * For example, perhaps a suite of tests needs to grab information from a file, and you want
 * to be able to specify a different filename during different runs.  You can accomplish this in ScalaTest by passing
 * the filename in a <em>config map</em> of key-value pairs, which is passed to <code>run</code> as a <a href="ConfigMap.html"><code>ConfigMap</code></a>.
 * The values in the config map are called "config objects," because they can be used to <em>configure</em>
 * suites, reporters, and tests.
 * </p>
 *
 * <p>
 * You can specify a string config object is via the ScalaTest <code>Runner</code>, either via the command line
 * or ScalaTest's ant task.
 * (See the <a href="tools/Runner$.html#configMapSection">documentation for Runner</a> for information on how to specify 
 * config objects on the command line.)
 * The config map is passed to <code>run</code>, <code>runNestedSuites</code>, <code>runTests</code>, and <code>runTest</code>,
 * so one way to access it in your suite is to override one of those methods. If you need to use the config map inside your tests, you
 * can access it from the <code>NoArgTest</code> passed to <code>withFixture</code>, or the <code>OneArgTest</code> passed to
 * <code>withFixture</code> in the traits in the <code>org.scalatest.fixture</code> package. (See the
 * <a href="fixture/Suite.html">documentation for <code>fixture.Suite</code></a>
 * for instructions on how to access the config map in tests.)
 * </p>
 *
 * <h2>Executing suites in parallel</h2>
 *
 * <p>
 * The <code>run</code> method takes as one of its parameters an optional <a href="Distributor.html"><code>Distributor</code></a>. If 
 * a <code>Distributor</code> is passed in, this trait's implementation of <code>run</code> puts its nested
 * <code>Suite</code>s into the distributor rather than executing them directly. The caller of <code>run</code>
 * is responsible for ensuring that some entity runs the <code>Suite</code>s placed into the 
 * distributor. The <code>-P</code> command line parameter to <code>Runner</code>, for example, will cause
 * <code>Suite</code>s put into the <code>Distributor</code> to be run in parallel via a pool of threads.
 * If you wish to execute the tests themselves in parallel, mix in <a href="ParallelTestExecution.html"><code>ParallelTestExecution</code></a>.
 * </p>
 *
 * <a name="errorHandling"></a>
 * <h2>Treatment of <code>java.lang.Error</code>s</h2>
 *
 * <p>
 * The Javadoc documentation for <code>java.lang.Error</code> states:
 * </p>
 *
 * <blockquote>
 * An <code>Error</code> is a subclass of <code>Throwable</code> that indicates serious problems that a reasonable application should not try to catch. Most
 * such errors are abnormal conditions.
 * </blockquote>
 *
 * <p>
 * Because <code>Error</code>s are used to denote serious errors, trait <code>Suite</code> and its subtypes in the ScalaTest API do not always treat a test
 * that completes abruptly with an <code>Error</code> as a test failure, but sometimes as an indication that serious problems
 * have arisen that should cause the run to abort. For example, if a test completes abruptly with an <code>OutOfMemoryError</code>, 
 * it will not be reported as a test failure, but will instead cause the run to abort. Because not everyone uses <code>Error</code>s only to represent serious
 * problems, however, ScalaTest only behaves this way for the following exception types (and their subclasses):
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
 * Although trait <code>Suite</code> and all its subtypes in the ScalaTest API consistently behave this way with regard to <code>Error</code>s,
 * this behavior is not required by the contract of <code>Suite</code>. Subclasses and subtraits that you define, for example, may treat all
 * <code>Error</code>s as test failures, or indicate errors in some other way that has nothing to do with exceptions.
 * </p>
 *
 * <a name="lifecyle-methods"></a>
 * <h2>Extensibility</h2>
 *
 * <p>
 * Trait <code>Suite</code> provides default implementations of its methods that should
 * be sufficient for most applications, but many methods can be overridden when desired. Here's
 * a summary of the methods that are intended to be overridden:
 * </p>
 *
 * <ul>
 * <li><code>run</code> - override this method to define custom ways to run suites of
 *   tests.</li>
 * <li><code>runNestedSuites</code> - override this method to define custom ways to run nested suites.</li>
 * <li><code>runTests</code> - override this method to define custom ways to run a suite's tests.</li>
 * <li><code>runTest</code> - override this method to define custom ways to run a single named test.</li>
 * <li><code>testNames</code> - override this method to specify the <code>Suite</code>'s test names in a custom way.</li>
 * <li><code>tags</code> - override this method to specify the <code>Suite</code>'s test tags in a custom way.</li>
 * <li><code>nestedSuites</code> - override this method to specify the <code>Suite</code>'s nested <code>Suite</code>s in a custom way.</li>
 * <li><code>suiteName</code> - override this method to specify the <code>Suite</code>'s name in a custom way.</li>
 * <li><code>expectedTestCount</code> - override this method to count this <code>Suite</code>'s expected tests in a custom way.</li>
 * </ul>
 *
 * <p>
 * For example, this trait's implementation of <code>testNames</code> performs reflection to discover methods starting with <code>test</code>,
 * and places these in a <code>Set</code> whose iterator returns the names in alphabetical order. If you wish to run tests in a different
 * order in a particular <code>Suite</code>, perhaps because a test named <code>testAlpha</code> can only succeed after a test named
 * <code>testBeta</code> has run, you can override <code>testNames</code> so that it returns a <code>Set</code> whose iterator returns
 * <code>testBeta</code> <em>before</em> <code>testAlpha</code>. (This trait's implementation of <code>run</code> will invoke tests
 * in the order they come out of the <code>testNames</code> <code>Set</code> iterator.)
 * </p>
 *
 * <p>
 * Alternatively, you may not like starting your test methods with <code>test</code>, and prefer using <code>@Test</code> annotations in
 * the style of Java's JUnit 4 or TestNG. If so, you can override <code>testNames</code> to discover tests using either of these two APIs
 * <code>@Test</code> annotations, or one of your own invention. (This is in fact
 * how <code>org.scalatest.junit.JUnitSuite</code> and <code>org.scalatest.testng.TestNGSuite</code> work.)
 * </p>
 *
 * <p>
 * Moreover, <em>test</em> in ScalaTest does not necessarily mean <em>test method</em>. A test can be anything that can be given a name,
 * that starts and either succeeds or fails, and can be ignored. In <code>org.scalatest.FunSuite</code>, for example, tests are represented
 * as function values. This
 * approach might look foreign to JUnit users, but may feel more natural to programmers with a functional programming background.
 * To facilitate this style of writing tests, <code>FunSuite</code> overrides <code>testNames</code>, <code>runTest</code>, and <code>run</code> such that you can 
 * define tests as function values.
 * </p>
 *
 * <p>
 * You can also model existing JUnit 3, JUnit 4, or TestNG tests as suites of tests, thereby incorporating tests written in Java into a ScalaTest suite.
 * The "wrapper" classes in packages <code>org.scalatest.junit</code> and <code>org.scalatest.testng</code> exist to make this easy.
 * No matter what legacy tests you may have, it is likely you can create or use an existing <code>Suite</code> subclass that allows you to model those tests
 * as ScalaTest suites and tests and incorporate them into a ScalaTest suite. You can then write new tests in Scala and continue supporting
 * older tests in Java.
 * </p>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.MethodFinder"))
trait Suite extends Assertions with AbstractSuite with Serializable { thisSuite =>

  import Suite.TestMethodPrefix, Suite.InformerInParens, Suite.IgnoreAnnotation

  /**
   * A test function taking no arguments and returning an <code>Outcome</code>.
   *
   * <p>
   * For more detail and examples, see the relevant section in the 
   * <a href="FlatSpec.html#withFixtureNoArgTest">documentation for trait <code>fixture.FlatSpec</code></a>.
   * </p>
   */
  protected trait NoArgTest extends (() => Outcome) with TestData {

    /**
     * Runs the body of the test, returning an <code>Outcome</code>.
     */
    def apply(): Outcome
  }

  // Keep this out of the public until there's a use case demonstrating its need
  private[scalatest] object NoArgTest {
    def apply(test: NoArgTest)(f: => Outcome): NoArgTest = {
      new NoArgTest {
        def apply(): Outcome = { f }
        val text: String = test.text
        val configMap: ConfigMap = test.configMap
        val scopes: collection.immutable.IndexedSeq[String] = test.scopes
        val name: String = test.name
        val tags: Set[String] = test.tags
      }
    }
  }

  /**
  * An immutable <code>IndexedSeq</code> of this <code>Suite</code> object's nested <code>Suite</code>s. If this <code>Suite</code> contains no nested <code>Suite</code>s,
  * this method returns an empty <code>IndexedSeq</code>. This trait's implementation of this method returns an empty <code>List</code>.
  */
  def nestedSuites: collection.immutable.IndexedSeq[Suite] = Vector.empty

  /**
   * Executes one or more tests in this <code>Suite</code>, printing results to the standard output.
   *
   * <p>
   * This method invokes <code>run</code> on itself, passing in values that can be configured via the parameters to this
   * method, all of which have default values. This behavior is convenient when working with ScalaTest in the Scala interpreter.
   * Here's a summary of this method's parameters and how you can use them:
   * </p>
   *
   * <p>
   * <strong>The <code>testName</code> parameter</strong>
   * </p>
   *
   * <p>
   * If you leave <code>testName</code> at its default value (of <code>null</code>), this method will pass <code>None</code> to
   * the <code>testName</code> parameter of <code>run</code>, and as a result all the tests in this suite will be executed. If you
   * specify a <code>testName</code>, this method will pass <code>Some(testName)</code> to <code>run</code>, and only that test
   * will be run. Thus to run all tests in a suite from the Scala interpreter, you can write:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; new ExampleSuite execute
   * </pre>
   *
   * <p>
   * (The above syntax actually invokes the overloaded parameterless form of <code>execute</code>, which calls this form with its default parameter values.)
   * To run just the test named <code>"my favorite test"</code> in a suite from the Scala interpreter, you would write:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; new ExampleSuite execute ("my favorite test")
   * </pre>
   *
   * <p>
   * Or:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; new ExampleSuite execute (testName = "my favorite test")
   * </pre>
   *
   * <p>
   * <strong>The <code>configMap</code> parameter</strong>
   * </p>
   *
   * <p>
   * If you provide a value for the <code>configMap</code> parameter, this method will pass it to <code>run</code>. If not, the default value
   * of an empty <code>Map</code> will be passed. For more information on how to use a config map to configure your test suites, see
   * the <a href="#configMapSection">config map section</a> in the main documentation for this trait. Here's an example in which you configure
   * a run with the name of an input file:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; new ExampleSuite execute (configMap = Map("inputFileName" -> "in.txt")
   * </pre>
   *
   * <p>
   * <strong>The <code>color</code> parameter</strong>
   * </p>
   *
   * <p>
   * If you leave the <code>color</code> parameter unspecified, this method will configure the reporter it passes to <code>run</code> to print
   * to the standard output in color (via ansi escape characters). If you don't want color output, specify false for <code>color</code>, like this:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; new ExampleSuite execute (color = false)
   * </pre>
   *
   * <p>
   * <strong>The <code>durations</code> parameter</strong>
   * </p>
   *
   * <p>
   * If you leave the <code>durations</code> parameter unspecified, this method will configure the reporter it passes to <code>run</code> to
   * <em>not</em> print durations for tests and suites to the standard output. If you want durations printed, specify true for <code>durations</code>,
   * like this:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; new ExampleSuite execute (durations = true)
   * </pre>
   *
   * <p>
   * <strong>The <code>shortstacks</code> and <code>fullstacks</code> parameters</strong>
   * </p>
   *
   * <p>
   * If you leave both the <code>shortstacks</code> and <code>fullstacks</code> parameters unspecified, this method will configure the reporter
   * it passes to <code>run</code> to <em>not</em> print stack traces for failed tests if it has a stack depth that identifies the offending
   * line of test code. If you prefer a short stack trace (10 to 15 stack frames) to be printed with any test failure, specify true for
   * <code>shortstacks</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; new ExampleSuite execute (shortstacks = true)
   * </pre>
   *
   * <p>
   * For full stack traces, set <code>fullstacks</code> to true:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; new ExampleSuite execute (fullstacks = true)
   * </pre>
   *
   * <p>
   * If you specify true for both <code>shortstacks</code> and <code>fullstacks</code>, you'll get full stack traces.
   * </p>
   *
   * <p>
   * <strong>The <code>stats</code> parameter</strong>
   * </p>
   *
   * <p>
   * If you leave the <code>stats</code> parameter unspecified, this method will <em>not</em> fire <code>RunStarting</code> and either <code>RunCompleted</code>
   * or <code>RunAborted</code> events to the reporter it passes to <code>run</code>.
   * If you specify true for <code>stats</code>, this method will fire the run events to the reporter, and the reporter will print the
   * expected test count before the run, and various statistics after, including the number of suites completed and number of tests that
   * succeeded, failed, were ignored or marked pending. Here's how you get the stats:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; new ExampleSuite execute (stats = true)
   * </pre>
   *
   *
   * <p>
   * To summarize, this method will pass to <code>run</code>:
   * </p>
   * <ul>
   * <li><code>testName</code> - <code>None</code> if this method's <code>testName</code> parameter is left at its default value of <code>null</code>, else <code>Some(testName)</code>.
   * <li><code>reporter</code> - a reporter that prints to the standard output</li>
   * <li><code>stopper</code> - a <code>Stopper</code> whose <code>apply</code> method always returns <code>false</code></li>
   * <li><code>filter</code> - a <code>Filter</code> constructed with <code>None</code> for <code>tagsToInclude</code> and <code>Set()</code>
   *   for <code>tagsToExclude</code></li>
   * <li><code>configMap</code> - the <code>configMap</code> passed to this method</li>
   * <li><code>distributor</code> - <code>None</code></li>
   * <li><code>tracker</code> - a new <code>Tracker</code></li>
   * </ul>
   *
   * <p>
   * Note:  In ScalaTest, the terms "execute" and "run" basically mean the same thing and
   * can be used interchangably. The reason this method isn't named <code>run</code> is that it takes advantage of
   * default arguments, and you can't mix overloaded methods and default arguments in Scala. (If named <code>run</code>,
   * this method would have the same name but different arguments than the main <a href="#run"><code>run</code> method</a> that
   * takes seven arguments. Thus it would overload and couldn't be used with default argument values.)
   * </p>
   *
   * <p>
   * Design note: This method has two "features" that may seem unidiomatic. First, the default value of <code>testName</code> is <code>null</code>.
   * Normally in Scala the type of <code>testName</code> would be <code>Option[String]</code> and the default value would
   * be <code>None</code>, as it is in this trait's <code>run</code> method. The <code>null</code> value is used here for two reasons. First, in
   * ScalaTest 1.5, <code>execute</code> was changed from four overloaded methods to one method with default values, taking advantage of
   * the default and named parameters feature introduced in Scala 2.8.
   * To not break existing source code, <code>testName</code> needed to have type <code>String</code>, as it did in two of the overloaded
   * <code>execute</code> methods prior to 1.5. The other reason is that <code>execute</code> has always been designed to be called primarily
   * from an interpeter environment, such as the Scala REPL (Read-Evaluate-Print-Loop). In an interpreter environment, minimizing keystrokes is king.
   * A <code>String</code> type with a <code>null</code> default value lets users type <code>suite.execute("my test name")</code> rather than
   * <code>suite.execute(Some("my test name"))</code>, saving several keystrokes.
   * </p>
   *
   * <p>
   * The second non-idiomatic feature is that <code>shortstacks</code> and <code>fullstacks</code> are all lower case rather than
   * camel case. This is done to be consistent with the <a href="Shell.html"><code>Shell</code></a>, which also uses those forms. The reason 
   * lower case is used in the <code>Shell</code> is to save keystrokes in an interpreter environment.  Most Unix commands, for
   * example, are all lower case, making them easier and quicker to type.  In the ScalaTest
   * <code>Shell</code>, methods like <code>shortstacks</code>, <code>fullstacks</code>, and <code>nostats</code>, <em>etc.</em>, are 
   * designed to be all lower case so they feel more like shell commands than methods.
   * </p>
   *
   * @param testName the name of one test to run.
   * @param configMap a <code>Map</code> of key-value pairs that can be used by the executing <code>Suite</code> of tests.
   * @param color a boolean that configures whether output is printed in color
   * @param durations a boolean that configures whether test and suite durations are printed to the standard output
   * @param shortstacks a boolean that configures whether short stack traces should be printed for test failures
   * @param fullstacks a boolean that configures whether full stack traces should be printed for test failures
   * @param stats a boolean that configures whether test and suite statistics are printed to the standard output
   *
   * @throws NullPointerException if the passed <code>configMap</code> parameter is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  final def execute(
    testName: String = null,
    configMap: ConfigMap = ConfigMap.empty,
    color: Boolean = true,
    durations: Boolean = false,
    shortstacks: Boolean = false,
    fullstacks: Boolean = false,
    stats: Boolean = false
  ) {
    if (configMap == null)
      throw new NullPointerException("configMap was null")
    val SelectedTag = "Selected"
    val SelectedSet = Set(SelectedTag)
    val desiredTests: Set[String] =
      if (testName == null) Set.empty
      else {
        testNames.filter { s =>
          s.indexOf(testName) >= 0 || NameTransformer.decode(s).indexOf(testName) >= 0
        }
      }
    if (testName != null && desiredTests.isEmpty)
      throw new IllegalArgumentException(Resources("testNotFound", testName))

    val dispatch = new DispatchReporter(List(new StandardOutReporter(durations, color, shortstacks, fullstacks, false, false, false, false, false)))
    val tracker = new Tracker
    val filter =
      if (testName == null) Filter()
      else {
        val taggedTests: Map[String, Set[String]] = desiredTests.map(_ -> SelectedSet).toMap
        Filter(
          tagsToInclude = Some(SelectedSet),
          excludeNestedSuites = true,
          dynaTags = DynaTags(Map.empty, Map(suiteId -> taggedTests))
        )
      }
    val runStartTime = System.currentTimeMillis
    if (stats)
      dispatch(RunStarting(tracker.nextOrdinal(), expectedTestCount(filter), configMap))

    val suiteStartTime = System.currentTimeMillis
    def dispatchSuiteAborted(e: Throwable) {
      val eMessage = e.getMessage
      val rawString = 
        if (eMessage != null && eMessage.length > 0)
          Resources("runOnSuiteException")
        else
          Resources("runOnSuiteExceptionWithMessage", eMessage)
      val formatter = formatterForSuiteAborted(thisSuite, rawString)
      val duration = System.currentTimeMillis - suiteStartTime
      dispatch(SuiteAborted(tracker.nextOrdinal(), rawString, thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), Some(e), Some(duration), formatter, Some(SeeStackDepthException)))
    }

    try {

      val formatter = formatterForSuiteStarting(thisSuite)
      dispatch(SuiteStarting(tracker.nextOrdinal(), thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), formatter, Some(getTopOfClass(thisSuite))))

      run(
        None,
        Args(dispatch,
        Stopper.default,
        filter,
        configMap,
        None,
        tracker,
        Set.empty)
      )
      val suiteCompletedFormatter = formatterForSuiteCompleted(thisSuite)
      val duration = System.currentTimeMillis - suiteStartTime
      dispatch(SuiteCompleted(tracker.nextOrdinal(), thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), Some(duration), suiteCompletedFormatter, Some(getTopOfClass(thisSuite))))
      if (stats) {
        val duration = System.currentTimeMillis - runStartTime
        dispatch(RunCompleted(tracker.nextOrdinal(), Some(duration)))
      }
    }
    catch {
      case e: InstantiationException =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources("cannotInstantiateSuite", e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
      case e: IllegalAccessException =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources("cannotInstantiateSuite", e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
      case e: NoClassDefFoundError =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources("cannotLoadClass", e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
      case e: Throwable =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources.bigProblems(e), Some(e), Some(System.currentTimeMillis - runStartTime)))
    }
    finally {
      dispatch.dispatchDisposeAndWaitUntilDone()
    }
  }

  /**
   * Executes this <code>Suite</code>, printing results to the standard output.
   *
   * <p>
   * This method, which simply invokes the other overloaded form of <code>execute</code> with default parameter values,
   * is intended for use only as a mini-DSL for the Scala interpreter. It allows you to execute a <code>Suite</code> in the
   * interpreter with a minimum of finger typing:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; new SetSpec execute
   * <span class="stGreen">An empty Set</span>
   * <span class="stGreen">- should have size 0</span>
   * <span class="stYellow">- should produce NoSuchElementException when head is invoked !!! IGNORED !!!</span>
   * </pre>
   *
   * <p>
   * If you do ever want to invoke <code>execute</code> outside the Scala interpreter, it is best style to invoke it with
   * empty parens to indicate it has a side effect, like this:
   * </p>
   *
   * <pre class="stREPL">
   * // Use empty parens form in regular code (outside the Scala interpreter)
   * (new ExampleSuite).execute()
   * </pre>
   */
  final def execute { execute() }

  /**
   * A <code>Map</code> whose keys are <code>String</code> names of tests that are tagged and
   * whose associated values are the <code>Set</code> of tag names for the test.  If a test has no associated tags, its name
   * does not appear as a key in the returned <code>Map</code>. If this <code>Suite</code> contains no tests with tags, this
   * method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation of this method uses Java reflection to discover any Java annotations attached to its test methods. The
   * fully qualified name of each unique annotation that extends <code>TagAnnotation</code> is considered a tag. This trait's
   * implementation of this method, therefore, places one key/value pair into to the
   * <code>Map</code> for each test for which a tag annotation is discovered through reflection.
   * </p>
   * 
   * <p>
   * In addition to test methods annotations, this trait's implementation will also auto-tag test methods with class level annotations.  
   * For example, if you annotate @Ignore at the class level, all test methods in the class will be auto-annotated with @Ignore.
   * </p>
   *
   * <p>
   * Subclasses may override this method to define and/or discover tags in a custom manner, but overriding method implementations
   * should never return an empty <code>Set</code> as a value. If a test has no tags, its name should not appear as a key in the
   * returned <code>Map</code>.
   * </p>
   */
  def tags: Map[String, Set[String]] = {
    val testNameSet = testNames
      
    val testTags = Map() ++ 
      (for (testName <- testNameSet; if !getTags(testName).isEmpty)
        yield testName -> (Set() ++ getTags(testName)))

    autoTagClassAnnotations(testTags, this)
  }
  
  private def getTags(testName: String) =
    for {
      a <- getMethodForTestName(thisSuite, testName).getDeclaredAnnotations
      annotationClass = a.annotationType
      if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
    } yield annotationClass.getName

  /**
   * A <code>Set</code> of test names. If this <code>Suite</code> contains no tests, this method returns an empty <code>Set</code>.
   *
   * <p>
   * <strong><code>Suite</code> has been deprecated as a style trait. During the deprecation period, the following behavior will continue
   * to work as before, but will go away at the conclusion of the deprecation period:</strong>
   * This trait's implementation of this method uses Java reflection to discover all public methods whose name starts with <code>"test"</code>,
   * which take either nothing or a single <code>Informer</code> as parameters. For each discovered test method, it assigns a test name
   * comprised of just the method name if the method takes no parameters, or the method name plus <code>(Informer)</code> if the
   * method takes a <code>Informer</code>. Here are a few method signatures and the names that this trait's implementation assigns them:
   * </p>
   *
   * <pre class="stHighlight">
   * def testCat() {}         // test name: "testCat"
   * def testCat(Informer) {} // test name: "testCat(Informer)"
   * def testDog() {}         // test name: "testDog"
   * def testDog(Informer) {} // test name: "testDog(Informer)"
   * def test() {}            // test name: "test"
   * def test(Informer) {}    // test name: "test(Informer)"
   * </pre>
   *
   * <p>
   * This trait's implementation of this method returns an immutable <code>Set</code> of all such names, excluding the name
   * <code>testNames</code>. The iterator obtained by invoking <code>elements</code> on this
   * returned <code>Set</code> will produce the test names in their <em>natural order</em>, as determined by <code>String</code>'s
   * <code>compareTo</code> method.
   * </p>
   *
   * <p>
   * This trait's implementation of <code>runTests</code> invokes this method
   * and calls <code>runTest</code> for each test name in the order they appear in the returned <code>Set</code>'s iterator.
   * Although this trait's implementation of this method returns a <code>Set</code> whose iterator produces <code>String</code>
   * test names in a well-defined order, the contract of this method does not required a defined order. Subclasses are free to
   * override this method and return test names in an undefined order, or in a defined order that's different from <code>String</code>'s
   * natural order.
   * </p>
   *
   * <p>
   * Subclasses may override this method to produce test names in a custom manner. One potential reason to override <code>testNames</code> is
   * to run tests in a different order, for example, to ensure that tests that depend on other tests are run after those other tests.
   * Another potential reason to override is allow tests to be defined in a different manner, such as methods annotated <code>@Test</code> annotations
   * (as is done in <code>JUnitSuite</code> and <code>TestNGSuite</code>) or test functions registered during construction (as is
   * done in <code>FunSuite</code> and <code>FunSpec</code>).
   * </p>
   *
   * <p>
   * In ScalaTest's event model, a test may be surrounded by &ldquo;scopes.&rdquo; Each test and scope is associated with string of text.
   * A test's name is concatenation of the text of any surrounding scopes followed by the text provided with the test
   * itself, after each text element has been trimmed and one space inserted between each component. Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * package org.scalatest.examples.freespec
   *
   * import org.scalatest.FreeSpec
   *
   * class SetSpec extends FreeSpec {
   *
   *   "A Set" - {
   *     "when empty" - {
   *       "should have size 0" in {
   *         assert(Set.empty.size === 0)
   *       }
   *
   *       "should produce NoSuchElementException when head is invoked" in {
   *         intercept[NoSuchElementException] {
   *           Set.empty.head
   *         }
   *       }
   *     }
   *   }
   * }
   * </pre>
   *
   * <p>
   * The above <code>FreeSpec</code> contains two tests, both nested inside the same two scopes. The outermost scope names
   * the subject, <code>A Set</code>. The nested scope qualifies the subject with <code>when empty</code>. Inside that
   * scope are the two tests. The text of the tests are:
   * <p>
   *
   * <ul>
   * <li><code>should have size 0</code></li>
   * <li><code>should produce NoSuchElementException when head is invoked</code></li>
   * </ul>
   *
   * <p>
   * Therefore, the names of these two tests are:
   * </p>
   *
   * <ul>
   * <li><code>A Stack when empty should have size 0</code></li>
   * <li><code>A Stack when empty should produce NoSuchElementException when head is invoked</code></li>
   * </ul>
   *
   * <p>
   * Note that because the component scope and test text strings are trimmed, any leading or trailing space will be dropped
   * before they are strung together to form the test name, with each trimmed component separated by a space. If the scopes
   * in the above example had text <code>" A Set "</code> and <code>" when empty "</code>, and the first test had text
   * <code>" should have size 0 "</code>, its test name would still be the same, "A Set when empty should have size 0"</code>.
   * </p>
   */
  def testNames: Set[String] = {

    def isTestMethod(m: Method) = {

      // Factored out to share code with fixture.Suite.testNames
      val (isInstanceMethod, simpleName, firstFour, paramTypes, hasNoParams, isTestNames, isTestTags, isTestDataFor) = isTestMethodGoodies(m)

      isInstanceMethod && (firstFour == "test") && !isTestDataFor && ((hasNoParams && !isTestNames && !isTestTags) || takesInformer(m))
    }

    val testNameArray =
      for (m <- getClass.getMethods; if isTestMethod(m)) 
        yield if (takesInformer(m)) m.getName + InformerInParens else m.getName

    val result = TreeSet.empty[String](EncodedOrdering) ++ testNameArray
    if (result.size != testNameArray.length) {
      throw new NotAllowedException("Howdy", 0)
    }
    result
  }

  /*
  Old style method names will have (Informer) at the end still, but new ones will
  not. This method will find the one without a Rep if the same name is used
  with and without a Rep.
  private[scalatest] def getMethodForTestName(theSuite: Suite, testName: String): Method =
    try {
      theSuite.getClass.getMethod(
        simpleNameForTest(testName),
        (if (testMethodTakesAnInformer(testName)) Array(classOf[Informer]) else new Array[Class[_]](0)): _*
      )
    }
    catch {
      case e: NoSuchMethodException =>
        // Try (Rep) on the end
        try {
          theSuite.getClass.getMethod(simpleNameForTest(testName), classOf[Rep])
        }
        catch {
          case e: NoSuchMethodException =>
            throw new IllegalArgumentException(Resources("testNotFound", testName))
        }
      case e: Throwable =>
        throw e
    }
   */

  /**
   *  Run the passed test function in the context of a fixture established by this method.
   *
   * <p>
   * This method should set up the fixture needed by the tests of the
   * current suite, invoke the test function, and if needed, perform any clean
   * up needed after the test completes. Because the <code>NoArgTest</code> function
   * passed to this method takes no parameters, preparing the fixture will require
   * side effects, such as reassigning instance <code>var</code>s in this <code>Suite</code> or initializing
   * a globally accessible external database. If you want to avoid reassigning instance <code>var</code>s
   * you can use <a href="fixture/Suite.html">fixture.Suite</a>.
   * </p>
   *
   * <p>
   * This trait's implementation of <code>runTest</code> invokes this method for each test, passing
   * in a <code>NoArgTest</code> whose <code>apply</code> method will execute the code of the test.
   * </p>
   *
   * <p>
   * This trait's implementation of this method simply invokes the passed <code>NoArgTest</code> function.
   * </p>
   *
   * @param test the no-arg test function to run with a fixture
   */
  protected def withFixture(test: NoArgTest): Outcome = {
    test()
  }

  /**
   * Run a test.
   *
   * <p>
   * This trait's implementation uses Java reflection to invoke on this object the test method identified by the passed <code>testName</code>.
   * </p>
   *
   * <p>
   * Implementations of this method are responsible for ensuring a <code>TestStarting</code> event
   * is fired to the <code>Reporter</code> before executing any test, and either <code>TestSucceeded</code>,
   * <code>TestFailed</code>, <code>TestPending</code> or <code>TestCanceled</code> after executing any nested
   * <code>Suite</code>. (If a test is marked with the <code>org.scalatest.Ignore</code> tag, the
   * <code>runTests</code> method is responsible for ensuring a <code>TestIgnored</code> event is fired and that
   * this <code>runTest</code> method is not invoked for that ignored test.)
   * </p>
   *
   * @param testName the name of one test to run.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   *
   * @throws NullPointerException if any of <code>testName</code> or <code>args</code> is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected def runTest(testName: String, args: Args): Status = {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")
    
    import args._

    val (stopRequested, report, method, testStartTime) =
      getSuiteRunTestGoodies(thisSuite, stopper, reporter, testName)

    reportTestStarting(this, report, tracker, testName, testName, rerunner, Some(getTopOfMethod(thisSuite, testName)))

    val formatter = getEscapedIndentedTextForTest(testName, 1, true)

    val messageRecorderForThisTest = new MessageRecorder(report)
    val informerForThisTest =
      MessageRecordingInformer(
        messageRecorderForThisTest, 
        (message, payload, isConstructingThread, testWasPending, testWasCanceled, location) => createInfoProvided(thisSuite, report, tracker, Some(testName), message, payload, 2, location, isConstructingThread, true)
      )

    // TODO: Was using reportInfoProvided here before, to double check with Bill for changing to markup provided.
    val documenterForThisTest =
      MessageRecordingDocumenter(
        messageRecorderForThisTest, 
        (message, _, isConstructingThread, testWasPending, testWasCanceled, location) => createMarkupProvided(thisSuite, report, tracker, Some(testName), message, 2, location, isConstructingThread) // TODO: Need a test that fails because testWasCanceleed isn't being passed
      )

    val argsArray: Array[Object] =
      if (testMethodTakesAnInformer(testName)) {
        Array(informerForThisTest)  
      }
      else Array()

    try {
      val theConfigMap = configMap
      val testData = testDataFor(testName, theConfigMap)
      withFixture(
        new NoArgTest {
          val name = testData.name
          def apply(): Outcome = { outcomeOf { method.invoke(thisSuite, argsArray: _*) } }
          val configMap = testData.configMap
          val scopes = testData.scopes
          val text = testData.text
          val tags = testData.tags
        }
      ).toUnit
      val duration = System.currentTimeMillis - testStartTime
      reportTestSucceeded(this, report, tracker, testName, testName, messageRecorderForThisTest.recordedEvents(false, false), duration, formatter, rerunner, Some(getTopOfMethod(thisSuite, method)))
      SucceededStatus
    }
    catch { 
      case ite: InvocationTargetException =>
        val t = ite.getTargetException
        t match {
          case _: TestPendingException =>
            val duration = System.currentTimeMillis - testStartTime
            // testWasPending = true so info's printed out in the finally clause show up yellow
            reportTestPending(this, report, tracker, testName, testName, messageRecorderForThisTest.recordedEvents(true, false), duration, formatter, Some(getTopOfMethod(thisSuite, method)))
            SucceededStatus
          case e: TestCanceledException =>
            val duration = System.currentTimeMillis - testStartTime
            val message = getMessageForException(e)
            val formatter = getEscapedIndentedTextForTest(testName, 1, true)
            // testWasCanceled = true so info's printed out in the finally clause show up yellow
            reportTestCanceled(this, report, t, testName, testName, messageRecorderForThisTest.recordedEvents(false, true), rerunner, tracker, duration, formatter, Some(TopOfMethod(thisSuite.getClass.getName, method.toGenericString())))
            SucceededStatus                 
          case e if !anExceptionThatShouldCauseAnAbort(e) =>
            val duration = System.currentTimeMillis - testStartTime
            handleFailedTest(thisSuite, t, testName, messageRecorderForThisTest.recordedEvents(false, false), report, tracker, getEscapedIndentedTextForTest(testName, 1, true), duration)
            FailedStatus
          case e => throw e
        }
      case e if !anExceptionThatShouldCauseAnAbort(e) =>
        val duration = System.currentTimeMillis - testStartTime
        handleFailedTest(thisSuite, e, testName, messageRecorderForThisTest.recordedEvents(false, false), report, tracker, getEscapedIndentedTextForTest(testName, 1, true), duration)
        FailedStatus
      case e: Throwable => throw e  
    }
  }
  
  /**
   * Run zero to many of this <code>Suite</code>'s tests.
   *
   * <p>
   * This method takes a <code>testName</code> parameter that optionally specifies a test to invoke.
   * If <code>testName</code> is defined, this trait's implementation of this method 
   * invokes <code>runTest</code> on this object, passing in:
   * </p>
   *
   * <ul>
   * <li><code>testName</code> - the <code>String</code> value of the <code>testName</code> <code>Option</code> passed
   *   to this method</li>
   * <li><code>reporter</code> - the <code>Reporter</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>stopper</code> - the <code>Stopper</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>configMap</code> - the <code>configMap</code> <code>Map</code> passed to this method, or one that wraps and delegates to it</li>
   * </ul>
   *
   * <p>
   * This method takes a <code>Filter</code>, which encapsulates an optional <code>Set</code> of tag names that should be included
   * (<code>tagsToInclude</code>) and a <code>Set</code> that should be excluded (<code>tagsToExclude</code>), when deciding which
   * of this <code>Suite</code>'s tests to run.
   * If <code>tagsToInclude</code> is <code>None</code>, all tests will be run
   * except those those belonging to tags listed in the <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is defined, only tests
   * belonging to tags mentioned in the <code>tagsToInclude</code> <code>Set</code>, and not mentioned in the <code>tagsToExclude</code <code>Set</code>
   * will be run. However, if <code>testName</code> is defined, <code>tagsToInclude</code> and <code>tagsToExclude</code> are essentially ignored.
   * Only if <code>testName</code> is <code>None</code> will <code>tagsToInclude</code> and <code>tagsToExclude</code> be consulted to
   * determine which of the tests named in the <code>testNames</code> <code>Set</code> should be run. This trait's implementation
   * behaves this way, and it is part of the general contract of this method, so all overridden forms of this method should behave
   * this way as well.  For more information on test tags, see the main documentation for this trait and for class <a href="Filter"><code>Filter</code></a>.
   * Note that this means that even if a test is marked as ignored, for example a test method in a <code>Suite</code> annotated with
   * <code>org.scalatest.Ignore</code>, if that test name is passed as <code>testName</code> to <code>runTest</code>, it will be invoked
   * despite the <code>Ignore</code> annotation.
   * </p>
   *
   * <p>
   * If <code>testName</code> is <code>None</code>, this trait's implementation of this method
   * invokes <code>testNames</code> on this <code>Suite</code> to get a <code>Set</code> of names of tests to potentially run.
   * (A <code>testNames</code> value of <code>None</code> essentially acts as a wildcard that means all tests in
   * this <code>Suite</code> that are selected by <code>tagsToInclude</code> and <code>tagsToExclude</code> should be run.)
   * For each test in the <code>testName</code> <code>Set</code>, in the order
   * they appear in the iterator obtained by invoking the <code>elements</code> method on the <code>Set</code>, this trait's implementation
   * of this method checks whether the test should be run based on the <code>Filter</code>.
   * If so, this implementation invokes <code>runTest</code>, passing in:
   * </p>
   *
   * <ul>
   * <li><code>testName</code> - the <code>String</code> name of the test to run (which will be one of the names in the <code>testNames</code> <code>Set</code>)</li>
   * <li><code>reporter</code> - the <code>Reporter</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>stopper</code> - the <code>Stopper</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>configMap</code> - the <code>configMap</code> passed to this method, or one that wraps and delegates to it</li>
   * </ul>
   *
   * <p>
   * If a test is marked with the <code>org.scalatest.Ignore</code> tag, implementations
   * of this method are responsible for ensuring a <code>TestIgnored</code> event is fired for that test
   * and that <code>runTest</code> is not called for that test.
   * </p>
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests started by this method have completed, and whether or not a failure occurred.
   *
   * @throws NullPointerException if any of the passed parameters is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected def runTests(testName: Option[String], args: Args): Status = {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")
    
    if (!this.isInstanceOf[Spec] && !this.isInstanceOf[Suites] && !this.isInstanceOf[Specs] && !this.isInstanceOf[Sequential] && !this.isInstanceOf[Stepwise])
      println("Unfortunately Suite has been deprecated as a style trait. Please use trait Spec instead.")

    import args._

    val theTestNames = testNames
    if (theTestNames.size > 0)
      checkChosenStyles(configMap, styleName)

    val stopRequested = stopper

    // Wrap any non-DispatchReporter, non-CatchReporter in a CatchReporter,
    // so that exceptions are caught and transformed
    // into error messages on the standard error stream.
    val report = wrapReporterIfNecessary(thisSuite, reporter)
    val newArgs = args.copy(reporter = report)
    
    val statusBuffer = new ListBuffer[Status]()

    // If a testName is passed to run, just run that, else run the tests returned
    // by testNames.
    testName match {

      case Some(tn) =>
        val (filterTest, ignoreTest) = filter(tn, tags, suiteId)
        if (!filterTest) {
          if (ignoreTest)
            reportTestIgnored(thisSuite, report, tracker, tn, tn, getEscapedIndentedTextForTest(tn, 1, true), Some(getTopOfMethod(thisSuite, tn)))
          else
            statusBuffer += runTest(tn, newArgs)
        }

      case None =>
        for ((tn, ignoreTest) <- filter(theTestNames, tags, suiteId)) {
          if (!stopRequested()) {
            if (ignoreTest)
              reportTestIgnored(thisSuite, report, tracker, tn, tn, getEscapedIndentedTextForTest(tn, 1, true), Some(getTopOfMethod(thisSuite, tn)))
            else
              statusBuffer += runTest(tn, newArgs)
          }
      }
    }
    new CompositeStatus(Set.empty ++ statusBuffer)
  }

  /**
   * Runs this suite of tests.
   *
   * <p>If <code>testName</code> is <code>None</code>, this trait's implementation of this method
   * calls these two methods on this object in this order:</p>
   *
   * <ol>
   * <li><code>runNestedSuites</code></li>
   * <li><code>runTests</code></li>
   * </ol>
   *
   * <p>
   * If <code>testName</code> is defined, then this trait's implementation of this method
   * calls <code>runTests</code>, but does not call <code>runNestedSuites</code>. This behavior
   * is part of the contract of this method. Subclasses that override <code>run</code> must take
   * care not to call <code>runNestedSuites</code> if <code>testName</code> is defined. (The
   * <code>OneInstancePerTest</code> trait depends on this behavior, for example.)
   * </p>
   *
   * <p>
   * Subclasses and subtraits that override this <code>run</code> method can implement them without
   * invoking either the <code>runTests</code> or <code>runNestedSuites</code> methods, which
   * are invoked by this trait's implementation of this method. It is recommended, but not required,
   * that subclasses and subtraits that override <code>run</code> in a way that does not
   * invoke <code>runNestedSuites</code> also override <code>runNestedSuites</code> and make it
   * final. Similarly it is recommended, but not required,
   * that subclasses and subtraits that override <code>run</code> in a way that does not
   * invoke <code>runTests</code> also override <code>runTests</code> (and <code>runTest</code>,
   * which this trait's implementation of <code>runTests</code> calls) and make it
   * final. The implementation of these final methods can either invoke the superclass implementation
   * of the method, or throw an <code>UnsupportedOperationException</code> if appropriate. The
   * reason for this recommendation is that ScalaTest includes several traits that override
   * these methods to allow behavior to be mixed into a <code>Suite</code>. For example, trait
   * <code>BeforeAndAfterEach</code> overrides <code>runTests</code>s. In a <code>Suite</code>
   * subclass that no longer invokes <code>runTests</code> from <code>run</code>, the
   * <code>BeforeAndAfterEach</code> trait is not applicable. Mixing it in would have no effect.
   * By making <code>runTests</code> final in such a <code>Suite</code> subtrait, you make
   * the attempt to mix <code>BeforeAndAfterEach</code> into a subclass of your subtrait
   * a compiler error. (It would fail to compile with a complaint that <code>BeforeAndAfterEach</code>
   * is trying to override <code>runTests</code>, which is a final method in your trait.) 
   * </p>
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests and nested suites started by this method have completed, and whether or not a failure occurred.
   *         
   * @throws NullPointerException if any passed parameter is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  def run(testName: Option[String], args: Args): Status = {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    val originalThreadName = Thread.currentThread.getName
    try {
      Thread.currentThread.setName(SuiteHelpers.augmentedThreadName(originalThreadName, suiteName))

      val stopRequested = stopper
      val report = wrapReporterIfNecessary(thisSuite, reporter)
      val newArgs = args.copy(reporter = report)

      val nestedSuitesStatus = 
        testName match {
          case None => runNestedSuites(newArgs)
          case Some(_) => SucceededStatus
        }
      val testsStatus = runTests(testName, newArgs)

      if (stopRequested()) {
        val rawString = Resources("executeStopping")
        report(InfoProvided(tracker.nextOrdinal(), rawString, Some(NameInfo(thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), testName))))
      }
      new CompositeStatus(Set(nestedSuitesStatus, testsStatus))
    }
    finally Thread.currentThread.setName(originalThreadName)
  }

  /**
   *
   * Run zero to many of this <code>Suite</code>'s nested <code>Suite</code>s.
   *
   * <p>
   * If the passed <code>distributor</code> is <code>None</code>, this trait's
   * implementation of this method invokes <code>run</code> on each
   * nested <code>Suite</code> in the <code>List</code> obtained by invoking <code>nestedSuites</code>.
   * If a nested <code>Suite</code>'s <code>run</code>
   * method completes abruptly with an exception, this trait's implementation of this
   * method reports that the <code>Suite</code> aborted and attempts to run the
   * next nested <code>Suite</code>.
   * If the passed <code>distributor</code> is defined, this trait's implementation
   * puts each nested <code>Suite</code> 
   * into the <code>Distributor</code> contained in the <code>Some</code>, in the order in which the
   * <code>Suite</code>s appear in the <code>List</code> returned by <code>nestedSuites</code>, passing
   * in a new <code>Tracker</code> obtained by invoking <code>nextTracker</code> on the <code>Tracker</code>
   * passed to this method.
   * </p>
   *
   * <p>
   * Implementations of this method are responsible for ensuring <code>SuiteStarting</code> events
   * are fired to the <code>Reporter</code> before executing any nested <code>Suite</code>, and either <code>SuiteCompleted</code>
   * or <code>SuiteAborted</code> after executing any nested <code>Suite</code>.
   * </p>
   *
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all nested suites started by this method have completed, and whether or not a failure occurred.
   *
   * @throws NullPointerException if any passed parameter is <code>null</code>.
   */
  protected def runNestedSuites(args: Args): Status = {

    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    val stopRequested = stopper
    val report = wrapReporterIfNecessary(thisSuite, reporter)

    def callExecuteOnSuite(nestedSuite: Suite): Status = {

      if (!stopRequested()) {

        // Create a Rerunner if the Suite has a no-arg constructor 
        val hasPublicNoArgConstructor = Suite.checkForPublicNoArgConstructor(nestedSuite.getClass)

        val rawString = Resources("suiteExecutionStarting")
        val formatter = formatterForSuiteStarting(nestedSuite)

        val suiteStartTime = System.currentTimeMillis

        report(SuiteStarting(tracker.nextOrdinal(), nestedSuite.suiteName, nestedSuite.suiteId, Some(nestedSuite.getClass.getName), formatter, Some(TopOfClass(nestedSuite.getClass.getName)), nestedSuite.rerunner))

        try { // TODO: pass runArgs down and that will get the chosenStyles passed down
          // Same thread, so OK to send same tracker
          val status = nestedSuite.run(None, Args(report, stopRequested, filter, configMap, distributor, tracker, Set.empty))

          val rawString = Resources("suiteCompletedNormally")
          val formatter = formatterForSuiteCompleted(nestedSuite)

          val duration = System.currentTimeMillis - suiteStartTime
          report(SuiteCompleted(tracker.nextOrdinal(), nestedSuite.suiteName, nestedSuite.suiteId, Some(nestedSuite.getClass.getName), Some(duration), formatter, Some(TopOfClass(nestedSuite.getClass.getName)), nestedSuite.rerunner))
          SucceededStatus
        }
        catch {       
          case e: RuntimeException => {
            val eMessage = e.getMessage
            val rawString = 
              if (eMessage != null && eMessage.length > 0)
                Resources("executeExceptionWithMessage", eMessage)
              else
                Resources("executeException")
            val formatter = formatterForSuiteAborted(nestedSuite, rawString)

            val duration = System.currentTimeMillis - suiteStartTime
            report(SuiteAborted(tracker.nextOrdinal(), rawString, nestedSuite.suiteName, nestedSuite.suiteId, Some(nestedSuite.getClass.getName), Some(e), Some(duration), formatter, Some(SeeStackDepthException), nestedSuite.rerunner))
            FailedStatus
          }
        }
      }
      else
        FailedStatus
    }

    val statusBuffer = new ListBuffer[Status]()
    if (!filter.excludeNestedSuites) {
      val nestedSuitesArray = nestedSuites.toArray
      distributor match {
        case None =>
          for (nestedSuite <- nestedSuitesArray) {
            if (!stopRequested()) 
              statusBuffer += callExecuteOnSuite(nestedSuite)
          }
        case Some(distribute) =>
          for (nestedSuite <- nestedSuitesArray) 
            statusBuffer += distribute(nestedSuite, args.copy(tracker = tracker.nextTracker))
      }
    }
    new CompositeStatus(Set.empty ++ statusBuffer)
  }

  /**
   * A user-friendly suite name for this <code>Suite</code>.
   *
   * <p>
   * This trait's
   * implementation of this method returns the simple name of this object's class. This
   * trait's implementation of <code>runNestedSuites</code> calls this method to obtain a
   * name for <code>Report</code>s to pass to the <code>suiteStarting</code>, <code>suiteCompleted</code>,
   * and <code>suiteAborted</code> methods of the <code>Reporter</code>.
   * </p>
   *
   * @return this <code>Suite</code> object's suite name.
   */
  def suiteName: String = getSimpleNameOfAnObjectsClass(thisSuite)

  /**
   * A string ID for this <code>Suite</code> that is intended to be unique among all suites reported during a run.
   *
   * <p>
   * This trait's
   * implementation of this method returns the fully qualified name of this object's class. 
   * Each suite reported during a run will commonly be an instance of a different <code>Suite</code> class,
   * and in such cases, this default implementation of this method will suffice. However, in special cases
   * you may need to override this method to ensure it is unique for each reported suite. For example, if you write
   * a <code>Suite</code> subclass that reads in a file whose name is passed to its constructor and dynamically
   * creates a suite of tests based on the information in that file, you will likely need to override this method
   * in your <code>Suite</code> subclass, perhaps by appending the pathname of the file to the fully qualified class name. 
   * That way if you run a suite of tests based on a directory full of these files, you'll have unique suite IDs for
   * each reported suite.
   * </p>
   *
   * <p>
   * The suite ID is <em>intended</em> to be unique, because ScalaTest does not enforce that it is unique. If it is not
   * unique, then you may not be able to uniquely identify a particular test of a particular suite. This ability is used,
   * for example, to dynamically tag tests as having failed in the previous run when rerunning only failed tests.
   * </p>
   *
   * @return this <code>Suite</code> object's ID.
   */
  def suiteId: String = thisSuite.getClass.getName

// XXX

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
  def pending: PendingNothing = { throw new TestPendingException }

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
  def pendingUntilFixed(f: => Unit) {
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
        throw new TestFailedException(Resources("pendingUntilFixed"), 2)
  }

  /**
   * The total number of tests that are expected to run when this <code>Suite</code>'s <code>run</code> method is invoked.
   *
   * <p>
   * This trait's implementation of this method returns the sum of:
   * </p>
   *
   * <ul>
   * <li>the size of the <code>testNames</code> <code>List</code>, minus the number of tests marked as ignored and
   * any tests that are exluded by the passed <code>Filter</code></li>
   * <li>the sum of the values obtained by invoking
   *     <code>expectedTestCount</code> on every nested <code>Suite</code> contained in
   *     <code>nestedSuites</code></li>
   * </ul>
   *
   * @param filter a <code>Filter</code> with which to filter tests to count based on their tags
   */
  def expectedTestCount(filter: Filter): Int = {

    @tailrec
    def countNestedSuiteTests(acc: Int, nestedSuitesToCount: List[Suite], filter: Filter): Int =
      nestedSuitesToCount match {
        case List() => acc
        case head :: tail => 
          countNestedSuiteTests(acc + head.expectedTestCount(filter), tail, filter)
      }

     countNestedSuiteTests(filter.runnableTestCount(testNames, tags, suiteId), nestedSuites.toList, filter)
  }

  // MOVE IT
  private[scalatest] def createCatchReporter(reporter: Reporter): Reporter = new WrapperCatchReporter(reporter)
  
  /**
   * The fully qualified class name of the rerunner to rerun this suite.  This implementation will look at this.getClass and see if it is
   * either an accessible Suite, or it has a WrapWith annotation. If so, it returns the fully qualified class name wrapped in a Some, 
   * or else it returns None.
   */
  def rerunner: Option[String] = {
    val suiteClass = getClass
    val isAccessible = SuiteDiscoveryHelper.isAccessibleSuite(suiteClass)
    val hasWrapWithAnnotation = suiteClass.getAnnotation(classOf[WrapWith]) != null
    if (isAccessible || hasWrapWithAnnotation)
      Some(suiteClass.getName)
    else
      None
  }
  
  /**
   * Suite style name.
   */
  val styleName: String = "org.scalatest.Suite"
  
  /**
   * Provides a <code>TestData</code> instance for the passed test name, given the passed config map.
   *
   * <p>
   * This method is used to obtain a <code>TestData</code> instance to pass to <code>withFixture(NoArgTest)</code>
   * and <code>withFixture(OneArgTest)</code> and the <code>beforeEach</code> and <code>afterEach</code> methods
   * of trait <code>BeforeAndAfterEach</code>.
   * </p>
   *
   * @param testName the name of the test for which to return a <code>TestData</code> instance
   * @param theConfigMap the config map to include in the returned <code>TestData</code>
   * @return a <code>TestData</code> instance for the specified test, which includes the specified config map
   */
  def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = {
    val suiteTags = for { 
      a <- this.getClass.getAnnotations
      annotationClass = a.annotationType
      if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
    } yield annotationClass.getName
    val testTags: Set[String] = 
      try {
        getTags(testName).toSet
      }
      catch {
        case e: IllegalArgumentException => Set.empty[String]
      }
    new TestData {
      val configMap = theConfigMap 
      val name = testName
      val scopes = Vector.empty
      val text = testName
      val tags = Set.empty ++ suiteTags ++ testTags
    }
  }
}

private[scalatest] object Suite {

  val TestMethodPrefix = "test"
  val InformerInParens = "(Informer)"
  val IgnoreAnnotation = "org.scalatest.Ignore"

  def getSimpleNameOfAnObjectsClass(o: AnyRef) = stripDollars(parseSimpleName(o.getClass.getName))

  // [bv: this is a good example of the expression type refactor. I moved this from SuiteClassNameListCellRenderer]
  // this will be needed by the GUI classes, etc.
  def parseSimpleName(fullyQualifiedName: String) = {

    val dotPos = fullyQualifiedName.lastIndexOf('.')

    // [bv: need to check the dotPos != fullyQualifiedName.length]
    if (dotPos != -1 && dotPos != fullyQualifiedName.length)
      fullyQualifiedName.substring(dotPos + 1)
    else
      fullyQualifiedName
  }
  
  def checkForPublicNoArgConstructor(clazz: java.lang.Class[_]) = {
    
    try {
      val constructor = clazz.getConstructor(new Array[java.lang.Class[T] forSome { type T }](0): _*)

      Modifier.isPublic(constructor.getModifiers)
    }
    catch {
      case nsme: NoSuchMethodException => false
    }
  }

  // This attempts to strip dollar signs that happen when using the interpreter. It is quite fragile
  // and already broke once. In the early days, all funky dollar sign encrusted names coming out of
  // the interpreter started with "line". Now they don't, but in both cases they seemed to have at
  // least one "$iw$" in them. So now I leave the string alone unless I see a "$iw$" in it. Worst case
  // is sometimes people will get ugly strings coming out of the interpreter. -bv April 3, 2012
  def stripDollars(s: String): String = {
    val lastDollarIndex = s.lastIndexOf('$')
    if (lastDollarIndex < s.length - 1)
      if (lastDollarIndex == -1 || !s.contains("$iw$")) s else s.substring(lastDollarIndex + 1)
    else {
      // The last char is a dollar sign
      val lastNonDollarChar = s.reverse.find(_ != '$')
      lastNonDollarChar match {
        case None => s
        case Some(c) => {
          val lastNonDollarIndex = s.lastIndexOf(c)
          if (lastNonDollarIndex == -1) s
          else stripDollars(s.substring(0, lastNonDollarIndex + 1))
        }
      }
    }
  }

  def diffStrings(s: String, t: String): Tuple2[String, String] = Prettifier.diffStrings(s, t)
  
  // If the objects are two strings, replace them with whatever is returned by diffStrings.
  // Otherwise, use the same objects.
  def getObjectsForFailureMessage(a: Any, b: Any) = Prettifier.getObjectsForFailureMessage(a, b)

  //
  // Use MotionToSuppress on aggregator Suites, i.e. Suites that
  // contain other Suites but don't contain any tests themselves
  // (e.g. DiscoverySuite).
  //
  def formatterForSuiteStarting(suite: Suite): Option[Formatter] =
    if ((suite.testNames.size == 0) && (suite.nestedSuites.size > 0))
      Some(MotionToSuppress)
    else
      Some(IndentedText(suite.suiteName + ":", suite.suiteName, 0))

  def formatterForSuiteCompleted(suite: Suite): Option[Formatter] =
      Some(MotionToSuppress)

  def formatterForSuiteAborted(suite: Suite, message: String): Option[Formatter] =
      Some(IndentedText(message, message, 0))

/*
  def simpleNameForTest(testName: String) =
    if (testName.endsWith(InformerInParens))
      testName.substring(0, testName.length - InformerInParens.length)
    else
      testName
*/

  def anExceptionThatShouldCauseAnAbort(throwable: Throwable): Boolean =
    throwable match {
      case _: AnnotationFormatError | 
/*
           _: org.scalatest.TestRegistrationClosedException |
           _: org.scalatest.DuplicateTestNameException |
           _: org.scalatest.NotAllowedException |
*/
           _: CoderMalfunctionError |
           _: FactoryConfigurationError | 
           _: LinkageError | 
           _: ThreadDeath | 
           _: TransformerFactoryConfigurationError | 
           _: VirtualMachineError => true
      // Don't use AWTError directly because it doesn't exist on Android, and a user
      // got ScalaTest to compile under Android.
      case e if e.getClass.getName == "java.awt.AWTError" => true
      case _ => false
    }

  def takesInformer(m: Method) = {
    val paramTypes = m.getParameterTypes
    paramTypes.length == 1 && classOf[Informer].isAssignableFrom(paramTypes(0))
  }

  def isTestMethodGoodies(m: Method) = {

    val isInstanceMethod = !Modifier.isStatic(m.getModifiers())

    // name must have at least 4 chars (minimum is "test")
    val simpleName = m.getName
    val firstFour = if (simpleName.length >= 4) simpleName.substring(0, 4) else "" 

    val paramTypes = m.getParameterTypes
    val hasNoParams = paramTypes.length == 0

    // Discover testNames(Informer) because if we didn't it might be confusing when someone
    // actually wrote a testNames(Informer) method and it was silently ignored.
    val isTestNames = simpleName == "testNames"
    val isTestTags = simpleName == "testTags"
    val isTestDataFor = (simpleName == "testDataFor" && paramTypes.length == 2 && classOf[String].isAssignableFrom(paramTypes(0)) && classOf[ConfigMap].isAssignableFrom(paramTypes(1))) || 
                        (simpleName == "testDataFor$default$2" && paramTypes.length == 0)

    (isInstanceMethod, simpleName, firstFour, paramTypes, hasNoParams, isTestNames, isTestTags, isTestDataFor)
  }

  def testMethodTakesAnInformer(testName: String): Boolean = testName.endsWith(InformerInParens)

  /*
   For info and test names, the formatted text should have one level shaved off so that the text will
   line up correctly, and the icon is over to the left of that even with the enclosing level.

   If a test is at the top level (not nested inside a describe), it's level is 0. So no need to subtract 1
   to make room for the icon in that case. An info inside such a test will have level 1. And agin, in that
   case no need to subtract 1. Such a test is "outermost test" and the info inside is "in outermost test" in:

class ArghSpec extends Spec with GivenWhenThen {
  info("in ArghSpec")
  it("outermost test") {
    info("in outermost test")
  }
  describe("Apple") {
    info("in Apple")
    describe("Boat") {
      info("in Boat")
      describe("Cat") {
        info("in Cat")
        describe("Dog") {
          info("in Dog")
          describe("Elephant") {
            info("in Elephant")
            it("Factory") {
              info("in Factory (test)")
              given("an empty Stack")
              when("push is invoked")
              then("it should have size 1")
              and("pop should return the pushed value")
            }
          }
        }
      }
    }
  }
}

It should (and at this point does) output this:

[scalatest] ArghSpec:
[scalatest] + in ArghSpec 
[scalatest] - outermost test (5 milliseconds)
[scalatest]   + in outermost test 
[scalatest] Apple 
[scalatest] + in Apple 
[scalatest]   Boat 
[scalatest]   + in Boat 
[scalatest]     Cat 
[scalatest]     + in Cat 
[scalatest]       Dog 
[scalatest]       + in Dog 
[scalatest]         Elephant 
[scalatest]         + in Elephant 
[scalatest]         - Factory (1 millisecond)
[scalatest]           + in Factory (test) 
[scalatest]           + Given an empty Stack 
[scalatest]           + When push is invoked 
[scalatest]           + Then it should have size 1 
[scalatest]           + And pop should return the pushed value 

FeatureSpec doesn't want any icons printed out. So adding includeIcon here. It
was already in getIndentedTextForInfo because of descriptions being printed out
without icons.

This should really be named getIndentedTextForTest maybe, because I think it is just
used for test events like succeeded/failed, etc.
  */
  def getIndentedTextForTest(testText: String, level: Int, includeIcon: Boolean) = {
    val decodedTestText = NameTransformer.decode(testText)
    val formattedText =
      if (includeIcon) {
        val testSucceededIcon = Resources("testSucceededIconChar")
        ("  " * (if (level == 0) 0 else (level - 1))) + Resources("iconPlusShortName", testSucceededIcon, decodedTestText)
      }
      else {
        ("  " * level) + decodedTestText
      }
    IndentedText(formattedText, decodedTestText, level)
  }
  
  def getEscapedIndentedTextForTest(testText: String, level: Int, includeIcon: Boolean) = {
    val decodedTestText = NameTransformer.decode(testText)
    val escapedTestText = 
      if (decodedTestText.startsWith("test: "))
        decodedTestText.drop(6)
      else
        decodedTestText
    val formattedText =
      if (includeIcon) {
        val testSucceededIcon = Resources("testSucceededIconChar")
        ("  " * (if (level == 0) 0 else (level - 1))) + Resources("iconPlusShortName", testSucceededIcon, escapedTestText)
      }
      else {
        ("  " * level) + escapedTestText
      }
    IndentedText(formattedText, decodedTestText, level)
  }

  // The icon is not included for branch description text, but is included for things sent via info(), given(),
  // when(), then(), etc. When it is included, reduce the level by 1, unless it is already 1 or 0.
  def getIndentedTextForInfo(message: String, level: Int, includeIcon: Boolean, infoIsInsideATest: Boolean) = {
    val formattedText =
      if (includeIcon) {
        val infoProvidedIcon = Resources("infoProvidedIconChar")
        //
        // Inside a test, you want level 1 to stay 1
        // [scalatest] - outermost test (5 milliseconds)
        // [scalatest]   + in outermost test
        //
        // But outside a test, level 1 should be transformed to 0
        // [scalatest] Apple
        // [scalatest] + in Apple
        //
        val indentationLevel =
          level match {
            case 0 => 0
            case 1 if infoIsInsideATest => 1
            case _ => level - 1
          }
        ("  " * indentationLevel) + Resources("iconPlusShortName", infoProvidedIcon, message)
        // ("  " * (if (level <= 1) level else (level - 1))) + Resources("iconPlusShortName", infoProvidedIcon, message)
      }
      else {
        ("  " * level) + message
      }
    IndentedText(formattedText, message, level)
  }

  def getMessageForException(e: Throwable): String =
    if (e.getMessage != null)
      e.getMessage
    else
      Resources("exceptionThrown", e.getClass.getName) // Say something like, "java.lang.Exception was thrown."

  def indentation(level: Int) = "  " * level
  
  def indentLines(level: Int, lines: GenTraversable[String]) = 
    lines.map(line => line.split("\n").map(indentation(level) + _).mkString("\n"))
    
  def substituteHtmlSpace(value: String) = value.replaceAll(" ", "&nbsp;")
    
  def unparsedXml(value: String) = scala.xml.Unparsed(value)
  
  def xmlContent(value: String) = unparsedXml(substituteHtmlSpace(value))

  def reportTestFailed(theSuite: Suite, report: Reporter, throwable: Throwable, testName: String, testText: String,
                       recordedEvents: collection.immutable.IndexedSeq[RecordableEvent], rerunnable: Option[String], tracker: Tracker, duration: Long, formatter: Formatter, location: Option[Location]) {

    val message = getMessageForException(throwable)
    //val formatter = getEscapedIndentedTextForTest(testText, level, includeIcon)
    val payload = 
      throwable match {
        case optPayload: PayloadField => 
          optPayload.payload
        case _ => 
          None
      }
    report(TestFailed(tracker.nextOrdinal(), message, theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, recordedEvents, Some(throwable), Some(duration), Some(formatter), location, theSuite.rerunner, payload))
  }

  // TODO: Possibly separate these out from method tests and function tests, because locations are different
  // Update: Doesn't seems to need separation, to be confirmed with Bill.
  def reportTestStarting(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, rerunnable: Option[String], location: Option[Location]) {
    report(TestStarting(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, Some(MotionToSuppress),
      location, rerunnable))
  }

  def reportTestPending(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, recordedEvents: collection.immutable.IndexedSeq[RecordableEvent], duration: Long, formatter: Formatter, location: Option[Location]) {
    report(TestPending(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, recordedEvents, Some(duration), Some(formatter),
      location))
  }

/*
  def reportTestCanceled(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, duration: Long, formatter: Formatter, location: Option[Location]) {
    val message = getMessageForException(throwable)
    report(TestCanceled(tracker.nextOrdinal(), message, theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, Some(duration), Some(formatter),
      location))
  }
*/

  def reportTestCanceled(theSuite: Suite, report: Reporter, throwable: Throwable, testName: String, testText: String,
      recordedEvents: collection.immutable.IndexedSeq[RecordableEvent], rerunnable: Option[String], tracker: Tracker, duration: Long, formatter: Formatter, location: Option[Location]) {

    val message = getMessageForException(throwable)
    val payload = 
      throwable match {
        case optPayload: PayloadField => 
          optPayload.payload
        case _ => 
          None
      }
    //val formatter = getEscapedIndentedTextForTest(testText, level, includeIcon)
    report(TestCanceled(tracker.nextOrdinal(), message, theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, recordedEvents, Some(throwable), Some(duration), Some(formatter), location, rerunnable, payload))
  }

  def reportTestSucceeded(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, recordedEvents: collection.immutable.IndexedSeq[RecordableEvent], duration: Long, formatter: Formatter, rerunnable: Option[String], location: Option[Location]) {
    report(TestSucceeded(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, recordedEvents, Some(duration), Some(formatter),
      location, rerunnable))
  }

  def reportTestIgnored(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, formatter: Formatter, location: Option[Location]) {
    val testSucceededIcon = Resources("testSucceededIconChar")
    report(TestIgnored(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, Some(formatter),
      location))
  }
  
  def createInfoProvided(theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    payload: Option[Any], 
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    includeIcon: Boolean = true) = {
    InfoProvided(
        tracker.nextOrdinal(),
        message,
        if (includeNameInfo)
          Some(NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName))
        else
          None,
        None,
        Some(getIndentedTextForInfo(message, level, includeIcon, testName.isDefined)),
        location,
        payload
      )
  }

  def createNoteProvided(theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    payload: Option[Any], 
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    includeIcon: Boolean = true) = {
    NoteProvided(
        tracker.nextOrdinal(),
        message,
        if (includeNameInfo)
          Some(NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName))
        else
          None,
        None,
        Some(getIndentedTextForInfo(message, level, includeIcon, testName.isDefined)),
        location,
        payload
      )
  }

  def createAlertProvided(theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    payload: Option[Any], 
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    includeIcon: Boolean = true) = {
    AlertProvided(
        tracker.nextOrdinal(),
        message,
        if (includeNameInfo)
          Some(NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName))
        else
          None,
        None,
        Some(getIndentedTextForInfo(message, level, includeIcon, testName.isDefined)),
        location,
        payload
      )
  }

  // If not fired in the context of a test, then testName will be None
  def reportInfoProvided(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    payload: Option[Any], 
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    includeIcon: Boolean = true
  ) {
    report(
      createInfoProvided(
        theSuite,
        report,
        tracker,
        testName,
        message,
        payload, 
        level,
        location,
        includeNameInfo,
        includeIcon
      )
    )
  }
  
  def reportNoteProvided(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    payload: Option[Any], 
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    includeIcon: Boolean = true
  ) {
    report(
      createNoteProvided(
        theSuite,
        report,
        tracker,
        testName,
        message,
        payload, 
        level,
        location,
        includeNameInfo,
        includeIcon
      )
    )
  }
  
  def reportAlertProvided(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    payload: Option[Any], 
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    includeIcon: Boolean = true
  ) {
    report(
      createAlertProvided(
        theSuite,
        report,
        tracker,
        testName,
        message,
        payload, 
        level,
        location,
        includeNameInfo,
        includeIcon
      )
    )
  }
  
  def createMarkupProvided(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    includeIcon: Boolean = true
  ) = {
    MarkupProvided(
      tracker.nextOrdinal(),
      message,
      if (includeNameInfo)
        Some(NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName))
      else
        None,
      Some(getIndentedTextForInfo(message, level, includeIcon, testName.isDefined)),
      location
    )
  }
  

  // If not fired in the context of a test, then testName will be None
  def reportMarkupProvided(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    includeIcon: Boolean = true
  ) {
    report(
      createMarkupProvided(
        theSuite,
        report,
        tracker,
        testName,
        message,
        level,
        location,
        includeNameInfo
      )
    )
  }

  // If not fired in the context of a test, then testName will be None
  def reportScopeOpened(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    message: String,
    level: Int,
    includeIcon: Boolean = true,
    location: Option[Location]
  ) {
    report(
      ScopeOpened(
        tracker.nextOrdinal(),
        message,
        NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), None),
        Some(getIndentedTextForInfo(message, level, includeIcon, false)),
        location
      )
    )
  }

  // If not fired in the context of a test, then testName will be None
  def reportScopeClosed(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    message: String,
    level: Int,
    includeIcon: Boolean = true,
    location: Option[Location]
  ) {
    report(
      ScopeClosed(
        tracker.nextOrdinal(),
        message,
        NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), None),
        Some(MotionToSuppress),
        location
      )
    )
  }
  
  def reportScopePending(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    message: String,
    level: Int,
    includeIcon: Boolean = true,
    location: Option[Location]
  ) {
    report(
      ScopePending(
        tracker.nextOrdinal(),
        message,
        NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), None),
        Some(getIndentedTextForInfo(message, level, includeIcon, false)),
        location
      )
    )
  }
  
  /*def getLineInFile(stackTraceList:List[StackTraceElement], sourceFileName:String, methodName: String):Option[LineInFile] = {
    val baseStackDepth = stackTraceList.takeWhile(stackTraceElement => sourceFileName != stackTraceElement.getFileName || stackTraceElement.getMethodName != methodName).length
    val stackTraceOpt = stackTraceList.drop(baseStackDepth).find(stackTraceElement => stackTraceElement.getMethodName() == "<init>")
    stackTraceOpt match {
      case Some(stackTrace) => Some(LineInFile(stackTrace.getLineNumber, stackTrace.getFileName))
      case None => None
    }
  }*/
  
  def getLineInFile(stackTraceList: Array[StackTraceElement], stackDepth: Int) = {
    if(stackDepth >= 0 && stackDepth < stackTraceList.length) {
      val stackTrace = stackTraceList(stackDepth)
      if(stackTrace.getLineNumber >= 0 && stackTrace.getFileName != null)
        Some(LineInFile(stackTrace.getLineNumber, stackTrace.getFileName))
      else
        None
    }
    else
      None
  }

  def checkChosenStyles(configMap: ConfigMap, styleName: String) {
    val chosenStyleSet = 
        if (configMap.isDefinedAt(Runner.CHOSEN_STYLES))
          configMap(Runner.CHOSEN_STYLES).asInstanceOf[Set[String]]
        else
          Set.empty[String]
    if (chosenStyleSet.size > 0 && !chosenStyleSet.contains(styleName)) {
      val e =
        if (chosenStyleSet.size == 1)
          new NotAllowedException(Resources("notTheChosenStyle", styleName, chosenStyleSet.head), getStackDepthFun("Suite.scala", "checkChosenStyles"))
        else
          new NotAllowedException(Resources("notOneOfTheChosenStyles", styleName, Suite.makeListForHumans(Vector.empty ++ chosenStyleSet.iterator)), getStackDepthFun("Scala.scala", "checkChosenStyles"))
      throw e
    }
  }

  // If it contains a space, or is an empty string, put quotes around it. OTherwise people might
  // get confused by the chosenStyles error message.
  def makeListForHumans(words: Vector[String]): String = {
    val quotedWords = words map { w =>
      if (w.length == 0 || w.indexOf(' ') >= 0) "\"" + w + "\""
      else w
    }
    quotedWords.length match {
      case 0 => "<empty list>"
      //case 1 if quotedWords(0).isEmpty => "\"\""
      case 1 => quotedWords(0)
      case 2 => Resources("leftAndRight", quotedWords(0), quotedWords(1))
      case _ =>
        val (leading, trailing) = quotedWords.splitAt(quotedWords.length - 2)
        leading.mkString(", ") + ", " + Resources("leftCommaAndRight", trailing(0), trailing(1))
    }
  }
  
  def autoTagClassAnnotations(tags: Map[String, Set[String]], theSuite: Suite) = {
    val suiteTags = for { 
      a <- theSuite.getClass.getAnnotations
      annotationClass = a.annotationType
      if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
    } yield annotationClass.getName
    
    val autoTestTags = 
      if (suiteTags.size > 0)
        Map() ++ theSuite.testNames.map(tn => (tn, suiteTags.toSet))
      else
        Map.empty[String, Set[String]]
    
    Runner.mergeMap[String, Set[String]](List(tags, autoTestTags)) ( _ ++ _ )
  }

  def handleFailedTest(
    theSuite: Suite,
    throwable: Throwable,
    testName: String,
    recordedEvents: collection.immutable.IndexedSeq[RecordableEvent],
    report: Reporter,
    tracker: Tracker,
    formatter: Formatter,
    duration: Long
  ) {
    val message = getMessageForException(throwable)
    //val formatter = getEscapedIndentedTextForTest(testName, 1, true)
    val payload = 
      throwable match {
        case optPayload: PayloadField => 
          optPayload.payload
        case _ => 
          None
      }
    report(TestFailed(tracker.nextOrdinal(), message, theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testName, recordedEvents, Some(throwable), Some(duration), Some(formatter), Some(SeeStackDepthException), theSuite.rerunner, payload))
  }

  def getTopOfClass(theSuite: Suite) = TopOfClass(theSuite.getClass.getName)
  def getTopOfMethod(theSuite: Suite, method: Method) = TopOfMethod(theSuite.getClass.getName, method.toGenericString())
  def getTopOfMethod(theSuite: Suite, testName: String) = TopOfMethod(theSuite.getClass.getName, getMethodForTestName(theSuite, testName).toGenericString())

  // Factored out to share this with fixture.Suite.runTest
  def getSuiteRunTestGoodies(theSuite: Suite, stopper: Stopper, reporter: Reporter, testName: String): (Stopper, Reporter, Method, Long) = {
    val (stopRequested, report, testStartTime) = getRunTestGoodies(theSuite, stopper, reporter, testName)
    val method = getMethodForTestName(theSuite, testName)
    (stopRequested, report, method, testStartTime)
  }

  // Sharing this with FunSuite and fixture.FunSuite as well as Suite and fixture.Suite
  def getRunTestGoodies(theSuite: Suite, stopper: Stopper, reporter: Reporter, testName: String): (Stopper, Reporter, Long) = {

    val stopRequested = stopper
    val report = wrapReporterIfNecessary(theSuite, reporter)

    val testStartTime = System.currentTimeMillis

    (stopRequested, report, testStartTime)
  }

  // Wrap any non-DispatchReporter, non-CatchReporter in a CatchReporter,
  // so that exceptions are caught and transformed
  // into error messages on the standard error stream.
  def wrapReporterIfNecessary(theSuite: Suite, reporter: Reporter): Reporter = reporter match {
    case cr: CatchReporter => cr
    case _ => theSuite.createCatchReporter(reporter)
  }

  val FixtureAndInformerInParens = "(FixtureParam, Informer)"
  val FixtureInParens = "(FixtureParam)"

  def testMethodTakesAFixtureAndInformer(testName: String) = testName.endsWith(FixtureAndInformerInParens)
  def testMethodTakesAFixture(testName: String) = testName.endsWith(FixtureInParens)

  def simpleNameForTest(testName: String) =
    if (testName.endsWith(FixtureAndInformerInParens))
      testName.substring(0, testName.length - FixtureAndInformerInParens.length)
    else if (testName.endsWith(FixtureInParens))
      testName.substring(0, testName.length - FixtureInParens.length)
    else if (testName.endsWith(InformerInParens))
      testName.substring(0, testName.length - InformerInParens.length)
    else
      testName

  def getMethodForTestName(theSuite: org.scalatest.Suite, testName: String): Method = {
    val candidateMethods = theSuite.getClass.getMethods.filter(_.getName == Suite.simpleNameForTest(testName))
    val found =
      if (testMethodTakesAFixtureAndInformer(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 2 && paramTypes(1) == classOf[Informer]
          }
        )
      else if (testMethodTakesAnInformer(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 1 && paramTypes(0) == classOf[Informer]
          }
        )
      else if (testMethodTakesAFixture(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 1
          }
        )
      else
        candidateMethods.find(_.getParameterTypes.length == 0)

     found match {
       case Some(method) => method
       case None =>
         throw new IllegalArgumentException(Resources("testNotFound", testName))
     }
  }

  // The substitution, if defined, indicates that the _1 string should be replace
  // by _2. This may transform "FunSpec" into "path.FunSpec", for example.
  def suiteToString(substitution: Option[(String, String)], theSuite: Suite): String = {
    val candidate = getSimpleNameOfAnObjectsClass(theSuite)
    val simpleName =
      substitution match {
        case Some((from, to)) if (candidate == from) => to
        case None => candidate
      }
   if (theSuite.nestedSuites.isEmpty) simpleName
   else simpleName + theSuite.nestedSuites.mkString("(", ", ", ")")
  }
}


