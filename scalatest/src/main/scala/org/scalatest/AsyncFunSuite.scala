/*
 * Copyright 2001-2014 Artima, Inc.
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
 * Enables testing of asynchronous code without blocking,
 * using a style consistent with traditional <code>FunSuite</code> tests.
 *
 * <p>
 * Given a <code>Future</code> returned by the code you are testing,
 * you need not block until the <code>Future</code> completes before
 * performing assertions against its value. You can instead map those
 * assertions onto the <code>Future</code> and return the resulting
 * <code>Future[Assertion]</code> to ScalaTest. The test will complete
 * asynchronously, when the <code>Future[Assertion]</code> completes.
 *
 * Here's an example <code>AsyncFunSuite</code>:
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite

 * import org.scalatest.AsyncFunSuite
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 *
 * class AddSuite extends AsyncFunSuite {
 *
 *   implicit val executionContext = ExecutionContext.Implicits.global
 *
 *   def addSoon(addends: Int*): Future[Int] = Future { addends.sum }
 *
 *   test("addSoon will eventually compute a sum of passed Ints") {
 *     val futureSum: Future[Int] = addSoon(1, 2)
 *     // You can map assertions onto a Future, then return
 *     // the resulting Future[Assertion] to ScalaTest:
 *     futureSum map { sum =&gt; assert(sum == 3) }
 *   } 
 *
 *   def addNow(addends: Int*): Int = addends.sum
 *
 *   test("addNow will immediately compute a sum of passed Ints") {
 *     val sum: Int = addNow(1, 2)
 *     // You can also write synchronous tests, which
 *     // must result in type Assertion:
 *     assert(sum == 3)
 *   }
 * }
 * </pre>
 *
 * <p>
 * &ldquo;<code>test</code>&rdquo; is a method, defined in <code>AsyncFunSuite</code>, which will be invoked
 * by the primary constructor of <code>AddSuite</code>. You specify the name of the test as
 * a string between the parentheses, and the test code itself between curly braces.
 * The test code is a function passed as a by-name parameter to <code>test</code>, which registers
 * it for later execution. The result type of the by-name in an <code>AsyncFunSuite</code> must
 * be <code>Future[Assertion]</code>. 
 * </p>
 *
 * <p>
 * <code>AsyncFunSuite</code> allows you to test asynchronous code without blocking. Instead of using
 * <code>scala.concurrent.Await</code> or <code>org.scalatest.concurrent.ScalaFutures</code> to 
 * block until a <code>Future</code> completes, then performing assertions on the result of the
 * <code>Future</code>, you map the assertions directly onto the <code>Future</code>. ScalaTest
 * assertions and matchers have result type <code>Assertion</code>. Thus the result type of the
 * first test in the example above is <code>Future[Assertion]</code>. For clarity, here's the relevant code
 * in a REPL session:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import Assertions._
 * import Assertions._
 *
 * scala&gt; import scala.concurrent.Future
 * import scala.concurrent.Future
 *
 * scala&gt; import scala.concurrent.ExecutionContext
 * import scala.concurrent.ExecutionContext
 *
 * scala&gt; implicit val executionContext = ExecutionContext.Implicits.global
 * executionContext: scala.concurrent.ExecutionContextExecutor = scala.concurrent.impl.ExecutionContextImpl@26141c5b
 *
 * scala&gt; def addSoon(addends: Int*): Future[Int] = Future { addends.sum }
 * addSoon: (addends: Int*)scala.concurrent.Future[Int]
 *
 * scala&gt; val futureSum: Future[Int] = addSoon(1, 2)
 * futureSum: scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPromise@721f47b2
 *
 * scala&gt; futureSum map { sum =&gt; assert(sum == 3) }
 * res0: scala.concurrent.Future[org.scalatest.Assertion] = scala.concurrent.impl.Promise$DefaultPromise@3955cfcb
 * </pre>
 *
 * <p>
 * The second test has result type <code>Assertion</code>:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; def addNow(addends: Int*): Int = addends.sum
 * addNow: (addends: Int*)Int
 *
 * scala&gt; val sum: Int = addNow(1, 2)
 * sum: Int = 3
 *
 * scala&gt; assert(sum == 3)
 * res1: org.scalatest.Assertion = Succeeded
 * </pre>
 * 
 * <p>
 * The second test will be implicitly converted to <code>Future[Assertion]</code> and registered.
 * The implicit conversion is from <code>Assertion</code> to <code>Future[Assertion]</code>, so
 * you must end synchronous tests in some ScalaTest assertion or matcher expression. If you need to,
 * you can put <code>succeed</code> at the end of the test body. <code>succeed</code> is a field in
 * trait <code>Assertions</code> that returns the <code>Succeeded</code> singleton:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.Assertions._
 * import org.scalatest.Assertions._
 *
 * scala&gt; succeed
 * res2: org.scalatest.Assertion = Succeeded
 * </pre>
 *
 * <p>
 * Thus placing <code>succeed</code> at the end of a test body will solve
 * the type error:
 * </p>
 *
 * <pre class="stHighlight">
 *   test("addNow will immediately compute a sum of passed Ints") {
 *     val sum: Int = addNow(1, 2)
 *     assert(sum == 3)
 *     println("hi") // println has result type Unit
 *     succeed       // succeed has result type Assertion
 *   }
 * </pre>
 *
 * <p>
 * An <code>AsyncFunSuite</code>'s lifecycle has two phases: the <em>registration</em> phase and the
 * <em>ready</em> phase. It starts in registration phase and enters ready phase the first time
 * <code>run</code> is called on it. It then remains in ready phase for the remainder of its lifetime.
 * </p>
 *
 * <p>
 * Tests can only be registered with the <code>test</code> method while the <code>AsyncFunSuite</code> is
 * in its registration phase. Any attempt to register a test after the <code>AsyncFunSuite</code> has
 * entered its ready phase, <em>i.e.</em>, after <code>run</code> has been invoked on the <code>AsyncFunSuite</code>,
 * will be met with a thrown <code>TestRegistrationClosedException</code>. The recommended style
 * of using <code>AsyncFunSuite</code> is to register tests during object construction as is done in all
 * the examples shown here. If you keep to the recommended style, you should never see a
 * <code>TestRegistrationClosedException</code>.
 * </p>
 *
 * <a name="executionContext"></a><h2>Execution context</h2>
 *
 * <p>
 * Here I'll describe that you must define an executionContext, and the test bodies will
 * be executed on those threads. Show an example in JVM and JS. 
 * </p>
 *
 * <p>
 * Then I'll describe one-at-a time default, and that if you want parallel execution
 * you mix in PTE. But if the execution context includes multiple threads, then
 * you'll need to synchronize access to shared mutable state.
 * </p>
 *
 * <a name="ignoredTests"></a><h2>Ignored tests</h2>
 *
 * <p>
 * To support the common use case of temporarily disabling a test, with the
 * good intention of resurrecting the test at a later time, <code>AsyncFunSuite</code> provides registration
 * methods that start with <code>ignore</code> instead of <code>test</code>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.ignore
 *
 * import org.scalatest.AsyncFunSuite
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 *
 * class AddSuite extends AsyncFunSuite {
 *
 *   implicit val executionContext = ExecutionContext.Implicits.global
 *
 *   def addSoon(addends: Int*): Future[Int] = Future { addends.sum }
 *
 *   ignore("addSoon will eventually compute a sum of passed Ints") {
 *     val futureSum: Future[Int] = addSoon(1, 2)
 *     // You can map assertions onto a Future, then return
 *     // the resulting Future[Assertion] to ScalaTest:
 *     futureSum map { sum =&gt; assert(sum == 3) }
 *   }
 *
 *   def addNow(addends: Int*): Int = addends.sum
 *
 *   test("addNow will immediately compute a sum of passed Ints") {
 *     val sum: Int = addNow(1, 2)
 *     // You can also write synchronous tests. The body
 *     // must have result type Assertion:
 *     assert(sum == 3)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>AddSuite</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new AddSuite execute
 * </pre>
 *
 * <p>
 * It will run only the second test and report that the first test was ignored:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">AddSuite:</span>
 * <span class="stYellow">- addSoon will eventually compute a sum of passed Ints !!! IGNORED !!!</span>
 * <span class="stGreen">- addNow will immediately compute a sum of passed Ints</span>
 * </pre>
 *
 * <p>
 * If you wish to temporarily ignore an entire suite of tests, you can (on the JVM, not Scala.js) annotate the test class with <code>@Ignore</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.ignoreall
 *
 * import org.scalatest.AsyncFunSuite
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 * import org.scalatest.Ignore
 *
 * @Ignore
 * class AddSuite extends AsyncFunSuite {
 *
 *   implicit val executionContext = ExecutionContext.Implicits.global
 *
 *   def addSoon(addends: Int*): Future[Int] = Future { addends.sum }
 *
 *   test("addSoon will eventually compute a sum of passed Ints") {
 *     val futureSum: Future[Int] = addSoon(1, 2)
 *     // You can map assertions onto a Future, then return
 *     // the resulting Future[Assertion] to ScalaTest:
 *     futureSum map { sum =&gt; assert(sum == 3) }
 *   }
 *
 *   def addNow(addends: Int*): Int = addends.sum
 *
 *   test("addNow will immediately compute a sum of passed Ints") {
 *     val sum: Int = addNow(1, 2)
 *     // You can also write synchronous tests. The body
 *     // must have result type Assertion:
 *     assert(sum == 3)
 *   }
 * }
 * </pre>
 *
 * <p>
 * When you mark a test class with a tag annotation, ScalaTest will mark each test defined in that class with that tag.
 * Thus, marking the <code>AddSuite</code> in the above example with the <code>@Ignore</code> tag annotation means that both tests
 * in the class will be ignored. If you run the above <code>AddSuite</code> in the Scala interpreter, you'll see:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new AddSuite execute
 * <span class="stGreen">AddSuite:</span>
 * <span class="stYellow">- addSoon will eventually compute a sum of passed Ints !!! IGNORED !!!
 * - addNow will immediately compute a sum of passed Ints !!! IGNORED !!!</span>
 * </pre>
 *
 * <p>
 * Note that marking a test class as ignored won't prevent it from being discovered by ScalaTest. Ignored classes
 * will be discovered and run, and all their tests will be reported as ignored. This is intended to keep the ignored
 * class visible, to encourage the developers to eventually fix and &ldquo;un-ignore&rdquo; it. If you want to
 * prevent a class from being discovered at all (on the JVM, not Scala.js), use the <a href="DoNotDiscover.html"><code>DoNotDiscover</code></a>
 * annotation instead.
 * </p>
 *
 * <a name="pendingTests"></a><h2>Pending tests</h2>
 *
 * <p>
 * A <em>pending test</em> is one that has been given a name but is not yet implemented. The purpose of
 * pending tests is to facilitate a style of testing in which documentation of behavior is sketched
 * out before tests are written to verify that behavior (and often, before the behavior of
 * the system being tested is itself implemented). Such sketches form a kind of specification of
 * what tests and functionality to implement later.
 * </p>
 *
 * <p>
 * To support this style of testing, a test can be given a name that specifies one
 * bit of behavior required by the system being tested. The test can also include some code that
 * sends more information about the behavior to the reporter when the tests run. At the end of the test,
 * it can call method <code>pending</code>, which will cause it to complete abruptly with <code>TestPendingException</code>.
 * </p>
 *
 * <p>
 * Because tests in ScalaTest can be designated as pending with <code>TestPendingException</code>, both the test name and any information
 * sent to the reporter when running the test can appear in the report of a test run. (In other words,
 * the code of a pending test is executed just like any other test.) However, because the test completes abruptly
 * with <code>TestPendingException</code>, the test will be reported as pending, to indicate
 * the actual test, and possibly the functionality, has not yet been implemented. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.pending
 *
 * import org.scalatest.AsyncFunSuite
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 *
 * class AddSuite extends AsyncFunSuite {
 *
 *   implicit val executionContext = ExecutionContext.Implicits.global
 *
 *   def addSoon(addends: Int*): Future[Int] = Future { addends.sum }
 *
 *   test("addSoon will eventually compute a sum of passed Ints") (pending)
 *
 *   def addNow(addends: Int*): Int = addends.sum
 *
 *   test("addNow will immediately compute a sum of passed Ints") {
 *     val sum: Int = addNow(1, 2)
 *     // You can also write synchronous tests. The body
 *     // must have result type Assertion:
 *     assert(sum == 3)
 *   }
 * }
 * </pre>
 *
 * <p>
 * (Note: "<code>(pending)</code>" is the body of the test. Thus the test contains just one statement, an invocation
 * of the <code>pending</code> method, which throws <code>TestPendingException</code>.)
 * If you run this version of <code>AddSuite</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new AddSuite execute
 * </pre>
 *
 * <p>
 * It will run both tests, but report that first test is pending. You'll see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">AddSuite:</span>
 * <span class="stYellow">- addSoon will eventually compute a sum of passed Ints (pending)</span>
 * <span class="stGreen">- addNow will immediately compute a sum of passed Ints</span>
 * </pre>
 * 
 * <p>
 * One difference between an ignored test and a pending one is that an ignored test is intended to be used during a
 * significant refactorings of the code under test, when tests break and you don't want to spend the time to fix
 * all of them immediately. You can mark some of those broken tests as ignored temporarily, so that you can focus the red
 * bar on just failing tests you actually want to fix immediately. Later you can go back and fix the ignored tests.
 * In other words, by ignoring some failing tests temporarily, you can more easily notice failed tests that you actually
 * want to fix. By contrast, a pending test is intended to be used before a test and/or the code under test is written.
 * Pending indicates you've decided to write a test for a bit of behavior, but either you haven't written the test yet, or
 * have only written part of it, or perhaps you've written the test but don't want to implement the behavior it tests
 * until after you've implemented a different bit of behavior you realized you need first. Thus ignored tests are designed
 * to facilitate refactoring of existing code whereas pending tests are designed to facilitate the creation of new code.
 * </p>
 *
 * <p>
 * One other difference between ignored and pending tests is that ignored tests are implemented as a test tag that is
 * excluded by default. Thus an ignored test is never executed. By contrast, a pending test is implemented as a
 * test that throws <code>TestPendingException</code> (which is what calling the <code>pending</code> method does). Thus
 * the body of pending tests are executed up until they throw <code>TestPendingException</code>.
 * </p>
 *
 */
abstract class AsyncFunSuite extends AsyncFunSuiteLike {

  /**
   * Returns a user friendly string for this suite, composed of the
   * simple name of the class (possibly simplified further by removing dollar signs if added by the Scala interpeter) and, if this suite
   * contains nested suites, the result of invoking <code>toString</code> on each
   * of the nested suites, separated by commas and surrounded by parentheses.
   *
   * @return a user-friendly string for this suite
   */
  override def toString: String = Suite.suiteToString(None, this)
}
