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
 * <a name="executionContext"></a><h2>The execution context and parallel execution</h2>
 *
 * <p>
 * In an <code>AsyncFunSuite</code> you will need to define an implicit
 * <code>ExecutionContext</code> named <code>executionContext</code>. This
 * execution context will be used by <code>AsyncFunSuite</code> to 
 * transform the <code>Future[Assertion]</code>s returned by tests
 * into the <code>Future[Outcome]</code> returned by the test function
 * passed to <code>withAsyncFixture</code>.
 * It is also intended to be used in the tests when an <code>ExecutionContext</code>
 * is needed, including when you map assertions onto a future.
 * </p>
 * 
 * <p>
 * By default, tests in an <code>AsyncFunSuite</code> will be executed one after
 * another, <em>i.e.</em>, serially. This is true whether those tests are synchronous
 * or asynchronous, no matter what threads are involved. This default behavior allows
 * you to re-use a shared fixture, such as an external database that needs to be cleaned
 * after each test, in multiple tests.
 * </p>
 *
 * <p>
 * If you want the tests of an <code>AsyncFunSuite</code> to be executed in parallel, you
 * must mix in <code>ParallelTestExecution</code>.
 * If <code>ParallelTestExecution</code> is mixed in but no distributor is passed, 
 * tests will be started sequentially, by the single thread that invoked <code>run</code>,
 * without waiting for tests to complete before the next test is started. Nevertheless,
 * asynchronous tests will be allowed to <em>complete</em> in parallel, using threads
 * from the <code>executionContext</code>. If <code>ParallelTestExecution</code> is mixed
 * in and a distributor is passed, tests will be started in parallel, using threads from
 * the distributor and allowed to complete in parallel, using threads from the
 * <code>executionContext</code>.
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
 * <a name="taggingTests"></a><h2>Tagging tests</h2>
 *
 * <p>
 * An <code>AsyncFunSuite</code>'s tests may be classified into groups by <em>tagging</em> them with string names.
 * As with any suite, when executing an <code>AsyncFunSuite</code>, groups of tests can
 * optionally be included and/or excluded. To tag an <code>AsyncFunSuite</code>'s tests,
 * you pass objects that extend class <code>org.scalatest.Tag</code> to methods
 * that register tests. Class <code>Tag</code> takes one parameter, a string name.  If you have
 * created tag annotation interfaces as described in the <a href="Tag.html"><code>Tag</code> documentation</a>, then you
 * will probably want to use tag names on your test functions that match. To do so, simply 
 * pass the fully qualified names of the tag interfaces to the <code>Tag</code> constructor. For example, if you've
 * defined a tag annotation interface with fully qualified name,
 * <code>com.mycompany.tags.DbTest</code>, then you could
 * create a matching tag for <code>AsyncFunSuite</code>s like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.tagging
 *
 * import org.scalatest.Tag
 *
 * object DbTest extends Tag("com.mycompany.tags.DbTest")
 * </pre>
 *
 * <p>
 * Given these definitions, you could place <code>AsyncFunSuite</code> tests into groups like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.AsyncFunSuite
 * import org.scalatest.tagobjects.Slow
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 *
 * class AddSuite extends AsyncFunSuite {
 *
 *   implicit val executionContext = ExecutionContext.Implicits.global
 *
 *   def addSoon(addends: Int*): Future[Int] = Future { addends.sum }
 *
 *   test("addSoon will eventually compute a sum of passed Ints", Slow) {
 *     val futureSum: Future[Int] = addSoon(1, 2)
 *     // You can map assertions onto a Future, then return
 *     // the resulting Future[Assertion] to ScalaTest:
 *     futureSum map { sum =&gt; assert(sum == 3) }
 *   }
 *
 *   def addNow(addends: Int*): Int = addends.sum
 *
 *   test("addNow will immediately compute a sum of passed Ints",
 *       Slow, DbTest) {
 *
 *     val sum: Int = addNow(1, 2)
 *     // You can also write synchronous tests. The body
 *     // must have result type Assertion:
 *     assert(sum == 3)
 *   }
 * }
 * </pre>
 *
 * <p>
 * This code marks both tests with the <code>org.scalatest.tags.Slow</code> tag, 
 * and the second test with the <code>com.mycompany.tags.DbTest</code> tag.
 * </p>
 *
 * <p>
 * The <code>run</code> method takes a <code>Filter</code>, whose constructor takes an optional
 * <code>Set[String]</code> called <code>tagsToInclude</code> and a <code>Set[String]</code> called
 * <code>tagsToExclude</code>. If <code>tagsToInclude</code> is <code>None</code>, all tests will be run
 * except those those belonging to tags listed in the
 * <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is defined, only tests
 * belonging to tags mentioned in the <code>tagsToInclude</code> set, and not mentioned in <code>tagsToExclude</code>,
 * will be run.
 * </p>
 *
 * <p>
 * It is recommended, though not required, that you create a corresponding tag annotation when you
 * create a <code>Tag</code> object. A tag annotation allows you to tag all the tests of an <code>AsyncFunSuite</code> in
 * one stroke by annotating the class. For more information and examples, see the
 * <a href="Tag.html">documentation for class <code>Tag</code></a>.
 * </p>
 *
 * <a name="sharedFixtures"></a>
 * <h2>Shared fixtures</h2>
 *
 * <p>
 * A test <em>fixture</em> is composed of the objects and other artifacts (files, sockets, database
 * connections, <em>etc.</em>) tests use to do their work.
 * When multiple tests need to work with the same fixtures, it is important to try and avoid
 * duplicating the fixture code across those tests. The more code duplication you have in your
 * tests, the greater drag the tests will have on refactoring the actual production code.
 * </p>
 *
 * <p>
 * ScalaTest recommends three techniques to eliminate such code duplication:
 * </p>
 *
 * <ul>
 * <li>Refactor using Scala</li>
 * <li>Override <code>withAsyncFixture</code></li>
 * <li>Mix in a <em>before-and-after</em> trait</li>
 * </ul>
 *
 * <p>Each technique is geared towards helping you reduce code duplication without introducing
 * instance <code>var</code>s, shared mutable objects, or other dependencies between tests. Eliminating shared
 * mutable state across tests will make your test code easier to reason about and more amenable for parallel
 * test execution.</p><p>The following sections
 * describe these techniques, including explaining the recommended usage
 * for each. But first, here's a table summarizing the options:</p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 *
 * <tr>
 *   <td colspan="2" style="background-color: #CCCCCC; border-width: 1px; padding: 3px; padding-top: 7px; border: 1px solid black; text-align: left">
 *     <strong>Refactor using Scala when different tests need different fixtures.</strong>
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 *     <a href="#getFixtureMethods">get-fixture methods</a>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     The <em>extract method</em> refactor helps you create a fresh instances of mutable fixture objects in each test
 *     that needs them, but doesn't help you clean them up when you're done.
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 *     <a href="#loanFixtureMethods">loan-fixture methods</a>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Factor out dupicate code with the <em>loan pattern</em> when different tests need different fixtures <em>that must be cleaned up afterwards</em>.
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td colspan="2" style="background-color: #CCCCCC; border-width: 1px; padding: 3px; padding-top: 7px; border: 1px solid black; text-align: left">
 *     <strong>Override <code>withAsyncFixture</code> when most or all tests need the same fixture.</strong>
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 *     <a href="#withAsyncFixtureNoArgAsyncTest">
 *       <code>withAsyncFixture(NoArgAsyncTest)</code></a>
 *     </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     <p>
 *     The recommended default approach when most or all tests need the same fixture treatment. This general technique
 *     allows you, for example, to perform side effects at the beginning and end of all or most tests, 
 *     transform the outcome of tests, retry tests, make decisions based on test names, tags, or other test data.
 *     Use this technique unless:
 *     </p>
 *  <ul>
 *  <li>Different tests need different fixtures (refactor using Scala instead)</li>
 *  <li>An exception in fixture code should abort the suite, not fail the test (use a <em>before-and-after</em> trait instead)</li>
 *  <li>You have objects to pass into tests (override <code>withAsyncFixture(<em>One</em>ArgAsyncTest)</code> instead)</li>
 *  </ul>
 *  </td>
 * </tr>
 *
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 *     <a href="#withAsyncFixtureOneArgAsyncTest">
 *       <code>withAsyncFixture(OneArgAsyncTest)</code>
 *     </a>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Use when you want to pass the same fixture object or objects as a parameter into all or most tests.
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td colspan="2" style="background-color: #CCCCCC; border-width: 1px; padding: 3px; padding-top: 7px; border: 1px solid black; text-align: left">
 *     <strong>Mix in a before-and-after trait when you want an aborted suite, not a failed test, if the fixture code fails.</strong>
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 *     <a href="#beforeAndAfter"><code>BeforeAndAfter</code></a>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Use this boilerplate-buster when you need to perform the same side-effects before and/or after tests, rather than at the beginning or end of tests.
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 *     <a href="#composingFixtures"><code>BeforeAndAfterEach</code></a>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Use when you want to <em>stack traits</em> that perform the same side-effects before and/or after tests, rather than at the beginning or end of tests.
 *   </td>
 * </tr>
 *
 * </table>
 *
 * <a name="getFixtureMethods"></a>
 * <h4>Calling get-fixture methods</h4>
 *
 * <p>
 * If you need to create the same mutable fixture objects in multiple tests, and don't need to clean them up after using them, the simplest approach is to write one or
 * more <em>get-fixture</em> methods. A get-fixture method returns a new instance of a needed fixture object (or an holder object containing
 * multiple fixture objects) each time it is called. You can call a get-fixture method at the beginning of each
 * test that needs the fixture, storing the returned object or objects in local variables. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.getfixture

 * import org.scalatest.AsyncFunSuite
 * import collection.mutable.ListBuffer
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 *
 * class ExampleSuite extends AsyncFunSuite {
 *
 *   implicit val executionContext = ExecutionContext.Implicits.global
 *
 *   def fixture: Future[String] = Future { "ScalaTest is " }
 *
 *   test("Testing should be easy") {
 *     val future = fixture
 *     val result = future map { s =&gt; s + "easy!" }
 *     result map { s =&gt;
 *       assert(s === "ScalaTest is easy!")
 *     }
 *   }
 *
 *   test("Testing should be fun") {
 *     val future = fixture
 *     val result = future map { s =&gt; s + "fun!" }
 *     result map { s =&gt;
 *       assert(s === "ScalaTest is fun!")
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you need to configure fixture objects differently in different tests, you can pass configuration into the get-fixture method.
 * For example, if you could pass in an initial value for a fixture object as a parameter to the get-fixture method.
 * </p>
 *
 * <a name="withAsyncFixtureNoArgAsyncTest"></a>
 * <h4>Overriding <code>withAsyncFixture(NoArgAsyncTest)</code></h4>
 *
 * <p>
 * Although the get-fixture method approach takes care of setting up a fixture at the beginning of each
 * test, it doesn't address the problem of cleaning up a fixture at the end of the test. If you just need to perform a side-effect at the beginning or end of
 * a test, and don't need to actually pass any fixture objects into the test, you can override <code>withAsyncFixture(NoArgAsyncTest)</code>, a
 * method defined in trait <a href="AsyncSuite.html"><code>AsyncSuite</code></a>, a supertrait of <code>AsyncFunSuite</code>.
 * </p>
 *
 * <p>
 * Trait <code>AsyncFunSuite</code>'s <code>runTest</code> method passes a no-arg async test function to
 * <code>withAsyncFixture(NoArgAsyncTest)</code>. It is <code>withAsyncFixture</code>'s
 * responsibility to invoke that test function. The default implementation of <code>withAsyncFixture</code> simply
 * invokes the function and returns the result, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Default implementation in trait AsyncSuite
 * protected def withAsyncFixture(test: NoArgAsyncTest): Future[Outcome] = {
 *   test()
 * }
 * </pre>
 *
 * <p>
 * You can, therefore, override <code>withAsyncFixture</code> to perform setup before invoking the test function,
 * and/or perform cleanup after the test completes. The recommended way to ensure cleanup is performed after a test completes is
 * to use the <code>withCleanup</code> helper method, defined in supertrait <a href="AsyncSuite.html"><code>AsyncSuite</code></a>,
 * which will ensure that
 * cleanup will occur whether future-producing code completes abruptly by throwing an exception, or returns
 * normally yielding a future. In the latter case, <code>withCleanup</code> will register the cleanup code
 * to execute asynchronously when the future completes.
 * </p>
 *
 * <p>
 * The <code>withAsyncFixture</code> method is designed to be stacked, and to enable this, you should always call the <code>super</code> implementation
 * of <code>withAsyncFixture</code>, and let it invoke the test function rather than invoking the test function directly. In other words, instead of writing
 * &ldquo;<code>test()</code>&rdquo;, you should write &ldquo;<code>super.withAsyncFixture(test)</code>&rdquo;, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * override def withAsyncFixture(test: NoArgTest) = {
 *
 *   // Perform setup here
 * 
 *   withCleanup {
 *     super.withAsyncFixture(test) // Invoke the test function
 *   } {
 *     // Perform cleanup here
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you have no cleanup to perform, you can write <code>withAsyncFixture</code> like this instead:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * override def withAsyncFixture(test: NoArgTest) = {
 *
 *   // Perform setup here
 *
 *   super.withAsyncFixture(test) // Invoke the test function
 * }
 * </pre>
 *
 * <p>
 * If you want to perform an action only for certain outcomes, you'll need to 
 * register code performing that action as a callback on the <code>Future</code> using
 * one of <code>Future</code> registration methods: <code>onComplete</code>, <code>onSuccess</code>,
 * or <code>onFailure</code>. Note that if a test fails, that will be treated as a
 * <code>scala.util.Success(org.scalatest.Failure)</code>. So if you want to perform an 
 * action if a test fails, for example, you'd register the callaback using <code>onSuccess</code>.
 * </p>
 *
 * <p>
 * Here's an example in which <code>withAsyncFixture(NoArgAsyncTest)</code> is used to take a
 * snapshot of the working directory if a test fails, and 
 * send that information to the standard output stream:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.noargasynctest
 *
 * import java.io.File
 * import org.scalatest._
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 *
 * class ExampleSuite extends AsyncFunSuite {
 *
 *   implicit val executionContext = ExecutionContext.Implicits.global
 *
 *   override def withAsyncFixture(test: NoArgAsyncTest) = {
 *
 *     val futureOutcome = super.withAsyncFixture(test)
 *
 *     futureOutcome onSuccess {
 *       case _: Failed =&gt;
 *         val currDir = new File(".")
 *         val fileNames = currDir.list()
 *         println("Dir snapshot: " + fileNames.mkString(", "))
 *     }
 *
 *     futureOutcome
 *   }
 *
 *   def addSoon(addends: Int*): Future[Int] = Future { addends.sum }
 *
 *   test("This test should succeed") {
 *     addSoon(1, 1) map { sum =&gt; assert(sum === 2) }
 *   }
 *
 *   test("This test should fail") {
 *     addSoon(1, 1) map { sum =&gt; assert(sum === 3) }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Running this version of <code>ExampleSuite</code> in the interpreter in a directory with two files, <code>hello.txt</code> and <code>world.txt</code>
 * would give the following output:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ExampleSuite execute
 * <span class="stGreen">ExampleSuite:
 * - this test should succeed</span>
 * Dir snapshot: hello.txt, world.txt
 * <span class="stRed">- this test should fail *** FAILED ***
 *   2 did not equal 3 (<console>:33)</span>
 * </pre>
 *
 * <p>
 * Note that the <a href="Suite$NoArgTest.html"><code>NoArgAsyncTest</code></a> passed to <code>withAsyncFixture</code>, in addition to
 * an <code>apply</code> method that executes the test, also includes the test name and the <a href="#configMapSection">config
 * map</a> passed to <code>runTest</code>. Thus you can also use the test name and configuration objects in your <code>withAsyncFixture</code>
 * implementation.
 * </p>
 *
 * <p>
 * Lastly, if you want to transform the outcome in some way in <code>withAsyncFixture</code>, you'll need to use either the
 * <code>map</code> or <code>transform</code> methods of <code>Future</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * // Your implementation
 * override def withAsyncFixture(test: NoArgTest) = {
 *
 *   // Perform setup here
 *
 *   val futureOutcome = super.withAsyncFixture(test) // Invoke the test function
 *
 *   futureOutcome map { outcome =&gt;
 *     // transform the outcome into a new outcome here
 *   }
 * }
 * </pre>
 * 
 * <p>
 * Note that a <code>NoArgAsyncTest</code>'s <code>apply</code> method will only return a <code>Failure</code> if
 * the test completes abruptly with an exception (such as <code>OutOfMemoryError</code>) that should
 * cause the suite to abort rather than the test to fail. Thus usually you would use <code>map</code>
 * to transform future outcomes, not <code>transform</code>, so that such suite-aborting exceptions pass through
 * unchanged.  The suite will abort asynchronously with any exception returned in a <code>Failure</code>.
 * </p>
 * 
 * <a name="loanFixtureMethods"></a>
 * <h4>Calling loan-fixture methods</h4>
 *
 * <p>
 * If you need to both pass a fixture object into a test <em>and</em> perform cleanup at the end of the test, you'll need to use the <em>loan pattern</em>.
 * If different tests need different fixtures that require cleanup, you can implement the loan pattern directly by writing <em>loan-fixture</em> methods.
 * A loan-fixture method takes a function whose body forms part or all of a test's code. It creates a fixture, passes it to the test code by invoking the
 * function, then cleans up the fixture after the function returns.
 * </p>
 *
 * <p>
 * The following example shows three tests that use two fixtures, a database and a file. Both require cleanup after, so each is provided via a
 * loan-fixture method. (In this example, the database is simulated with a <code>StringBuffer</code>.)
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.loanfixture
 *
 * import java.util.concurrent.ConcurrentHashMap
 *
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 *
 * object DbServer { // Simulating a database server
 *   type Db = StringBuffer
 *   private val databases = new ConcurrentHashMap[String, Db]
 *   def createDb(name: String): Db = {
 *     val db = new StringBuffer
 *     databases.put(name, db)
 *     db
 *   }
 *   def removeDb(name: String): Unit = {
 *     databases.remove(name)
 *   }
 * }
 *
 * // Defining actor messages
 * sealed abstract class StringOp
 * case object Clear extends StringOp
 * case class Append(value: String) extends StringOp
 * case object GetValue
 *
 * class StringActor { // Simulating an actor
 *   private final val sb = new StringBuilder
 *   def !(op: StringOp): Unit =
 *     synchronized {
 *       op match {
 *         case Append(value) =&gt; sb.append(value)
 *         case Clear =&gt; sb.clear()
 *       }
 *     }
 *   def ?(get: GetValue.type)(implicit c: ExecutionContext): Future[String] =
 *     Future {
 *       synchronized { sb.toString }
 *     }
 * }
 *
 * import org.scalatest._
 * import DbServer._
 * import java.util.UUID.randomUUID
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 *
 * class ExampleSuite extends AsyncFunSuite {
 *
 *   implicit val executionContext = ExecutionContext.Implicits.global
 *
 *   def withDatabase(testCode: Future[Db] =&gt; Future[Assertion]) = {
 *     val dbName = randomUUID.toString // generate a unique db name
 *     val futureDb = Future { createDb(dbName) } // create the fixture
 *     withCleanup {
 *       val futurePopulatedDb =
 *         futureDb map { db =&gt;
 *           db.append("ScalaTest is ") // perform setup 
 *         }
 *       testCode(futurePopulatedDb) // "loan" the fixture to the test code
 *     } {
 *       removeDb(dbName) // ensure the fixture will be cleaned up
 *     }
 *   }
 *
 *   def withActor(testCode: StringActor =&gt; Future[Assertion]) = {
 *     val actor = new StringActor
 *     withCleanup {
 *       actor ! Append("ScalaTest is ") // set up the fixture
 *       testCode(actor) // "loan" the fixture to the test code
 *     } {
 *       actor ! Clear // ensure the fixture will be cleaned up
 *     }
 *   }
 *
 *   // This test needs the actor fixture
 *   test("Testing should be productive") {
 *     withActor { actor =&gt;
 *       actor ! Append("productive!")
 *       val futureString = actor ? GetValue
 *       futureString map { s =&gt;
 *         assert(s === "ScalaTest is productive!")
 *       }
 *     }
 *   }
 *
 *   // This test needs the database fixture
 *   test("Test code should be readable") {
 *     withDatabase { futureDb =&gt;
 *       futureDb map { db =&gt;
 *         db.append("readable!")
 *         assert(db.toString === "ScalaTest is readable!")
 *       }
 *     }
 *   }
 *
 *   // This test needs both the file and the database
 *   test("Test code should be clear and concise") {
 *     withDatabase { futureDb =&gt;
 *       withActor { actor =&gt; // loan-fixture methods compose
 *         actor ! Append("concise!")
 *         val futureString = actor ? GetValue
 *         val futurePair: Future[(Db, String)] =
 *           futureDb zip futureString
 *         futurePair map { case (db, s) =&gt;
 *           db.append("clear!")
 *           assert(db.toString === "ScalaTest is clear!")
 *           assert(s === "ScalaTest is concise!")
 *         }
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * As demonstrated by the last test, loan-fixture methods compose. Not only do loan-fixture methods allow you to
 * give each test the fixture it needs, they allow you to give a test multiple fixtures and clean everything up afterwards.
 * </p>
 *
 * <p>
 * Also demonstrated in this example is the technique of giving each test its own "fixture sandbox" to play in. When your fixtures
 * involve external side-effects, like creating databases, it is a good idea to give each database a unique name as is
 * done in this example. This keeps tests completely isolated, allowing you to run them in parallel if desired.
 * </p>
 *
 * <a name="withAsyncFixtureOneArgAsyncTest"></a>
 * <h4>Overriding <code>withFixture(OneArgTest)</code></h4>
 *
 * <p>
 * If all or most tests need the same fixture, you can avoid some of the boilerplate of the loan-fixture method approach by using a
 * <code>fixture.AsyncSuite</code> and overriding <code>withAsyncFixture(OneArgAsyncTest)</code>.
 * Each test in a <code>fixture.AsyncSuite</code> takes a fixture as a parameter, allowing you to pass the fixture into
 * the test. You must indicate the type of the fixture parameter by specifying <code>FixtureParam</code>, and implement a
 * <code>withAsyncFixture</code> method that takes a <code>OneArgAsyncTest</code>. This <code>withAsyncFixture</code> method is responsible for
 * invoking the one-arg async test function, so you can perform fixture set up before, invoking and passing
 * the fixture into the test function, and perform clean up after the test completes (by registering the cleanup code as a
 * callback on the Future[Outcome] returned by the test function).
 * </p>
 *
 * <p>
 * To enable the stacking of traits that define <code>withAsyncFixture(NoArgAsyncTest)</code>, it is a good idea to let
 * <code>withAsyncFixture(NoArgAsyncTest)</code> invoke the test function instead of invoking the test
 * function directly. To do so, you'll need to convert the <code>OneArgAsyncTest</code> to a <code>NoArgAsyncTest</code>. You can do that by passing
 * the fixture object to the <code>toNoArgAsyncTest</code> method of <code>OneArgAsyncTest</code>. In other words, instead of
 * writing &ldquo;<code>test(theFixture)</code>&rdquo;, you'd delegate responsibility for
 * invoking the test function to the <code>withAsyncFixture(NoArgAsyncTest)</code> method of the same instance by writing:
 * </p>
 *
 * <pre>
 * withAsyncFixture(test.toNoArgAsyncTest(theFixture))
 * </pre>
 *
 * <p>
 * Here's a complete example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.oneargasynctest
 * 
 * import org.scalatest._
 * import java.io._
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 * 
 * // Defining actor messages
 * sealed abstract class StringOp
 * case object Clear extends StringOp
 * case class Append(value: String) extends StringOp
 * case object GetValue
 * 
 * class StringActor { // Simulating an actor
 *   private final val sb = new StringBuilder
 *   def !(op: StringOp): Unit =
 *     synchronized {
 *       op match {
 *         case Append(value) =&gt; sb.append(value)
 *         case Clear =&gt; sb.clear()
 *       }
 *     }
 *   def ?(get: GetValue.type)(implicit c: ExecutionContext): Future[String] =
 *     Future {
 *       synchronized { sb.toString }
 *     }
 * }
 * 
 * class ExampleSuite extends fixture.AsyncFunSuite {
 * 
 *   implicit val executionContext = ExecutionContext.Implicits.global
 * 
 *   type FixtureParam = StringActor
 * 
 *   def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] = {
 * 
 *     val actor = new StringActor
 *     withCleanup {
 *       actor ! Append("ScalaTest is ") // set up the fixture
 *       withAsyncFixture(test.toNoArgAsyncTest(actor))
 *     } {
 *       actor ! Clear // ensure the fixture will be cleaned up
 *     }
 *   }
 * 
 *   test("Testing should be easy") { actor =&gt;
 *     actor ! Append("easy!")
 *     val futureString = actor ? GetValue
 *     futureString map { s =&gt;
 *       assert(s === "ScalaTest is easy!")
 *     }
 *   }
 * 
 *   test("Testing should be fun") { actor =&gt;
 *     actor ! Append("fun!")
 *     val futureString = actor ? GetValue
 *     futureString map { s =&gt;
 *       assert(s === "ScalaTest is fun!")
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * In this example, the tests actually required two fixture objects, a <code>File</code> and a <code>FileWriter</code>. In such situations you can
 * simply define the <code>FixtureParam</code> type to be a tuple containing the objects, or as is done in this example, a case class containing
 * the objects.  For more information on the <code>withAsyncFixture(OneArgAsyncTest)</code> technique, see the <a href="fixture/AsyncFunSuite.html">documentation for <code>fixture.AsyncFunSuite</code></a>.
 * </p>
 *
 * <a name="beforeAndAfter"></a>
 * <h4>Mixing in <code>BeforeAndAfter</code></h4>
 *
 * <p>
 * In all the shared fixture examples shown so far, the activities of creating, setting up, and cleaning up the fixture objects have been
 * performed <em>during</em> the test.  This means that if an exception occurs during any of these activities, it will be reported as a test failure.
 * Sometimes, however, you may want setup to happen <em>before</em> the test starts, and cleanup <em>after</em> the test has completed, so that if an
 * exception occurs during setup or cleanup, the entire suite aborts and no more tests are attempted. The simplest way to accomplish this in ScalaTest is
 * to mix in trait <a href="BeforeAndAfter.html"><code>BeforeAndAfter</code></a>.  With this trait you can denote a bit of code to run before each test
 * with <code>before</code> and/or after each test each test with <code>after</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.beforeandafter
 * 
 * import org.scalatest.AsyncFunSuite
 * import org.scalatest.BeforeAndAfter
 * import collection.mutable.ListBuffer
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 * 
 * // Defining actor messages
 * sealed abstract class StringOp
 * case object Clear extends StringOp
 * case class Append(value: String) extends StringOp
 * case object GetValue
 * 
 * class StringActor { // Simulating an actor
 *   private final val sb = new StringBuilder
 *   def !(op: StringOp): Unit =
 *     synchronized {
 *       op match {
 *         case Append(value) =&gt; sb.append(value)
 *         case Clear =&gt; sb.clear()
 *       }
 *     }
 *   def ?(get: GetValue.type)(implicit c: ExecutionContext): Future[String] =
 *     Future {
 *       synchronized { sb.toString }
 *     }
 * }
 * 
 * class ExampleSuite extends AsyncFunSuite with BeforeAndAfter {
 * 
 *   implicit val executionContext = ExecutionContext.Implicits.global
 * 
 *   final val actor = new StringActor
 * 
 *   before {
 *     actor ! Append("ScalaTest is ") // set up the fixture
 *   }
 * 
 *   after {
 *     actor ! Clear // clean up the fixture
 *   }
 * 
 *   test("testing should be easy") {
 *     actor ! Append("easy!")
 *     val futureString = actor ? GetValue
 *     futureString map { s =&gt;
 *       assert(s === "ScalaTest is easy!")
 *     }
 *   }
 * 
 *   test("testing should be fun") {
 *     actor ! Append("fun!")
 *     val futureString = actor ? GetValue
 *     futureString map { s =&gt;
 *       assert(s === "ScalaTest is fun!")
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Note that the only way <code>before</code> and <code>after</code> code can communicate with test code is via some side-effecting mechanism, commonly by
 * reassigning instance <code>var</code>s or by changing the state of mutable objects held from instance <code>val</code>s (as in this example). If using
 * instance <code>var</code>s or mutable objects held from instance <code>val</code>s you wouldn't be able to run tests in parallel in the same instance
 * of the test class unless you synchronized access to the shared, mutable state. This is why ScalaTest's <code>ParallelTestExecution</code> trait extends
 * <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a>. By running each test in its own instance of the class, each test has its own copy of the instance variables, so you
 * don't need to synchronize. If you mixed <code>ParallelTestExecution</code> into the <code>ExampleSuite</code> above, the tests would run in parallel just fine
 * without any synchronization needed on the mutable <code>StringBuilder</code> and <code>ListBuffer[String]</code> objects.
 * </p>
 *
 * <p>
 * Although <code>BeforeAndAfter</code> provides a minimal-boilerplate way to execute code before and after tests, it isn't designed to enable stackable
 * traits, because the order of execution would be non-obvious.  If you want to factor out before and after code that is common to multiple test suites, you 
 * should use trait <code>BeforeAndAfterEach</code> instead, as shown later in the next section,
 * <a href="#composingFixtures.html">composing fixtures by stacking traits</a>.
 * </p>
 *
 * <a name="composingFixtures"></a><h2>Composing fixtures by stacking traits</h2>
 *
 * <p>
 * In larger projects, teams often end up with several different fixtures that test classes need in different combinations,
 * and possibly initialized (and cleaned up) in different orders. A good way to accomplish this in ScalaTest is to factor the individual
 * fixtures into traits that can be composed using the <em>stackable trait</em> pattern. This can be done, for example, by placing
 * <code>withFixture</code> methods in several traits, each of which call <code>super.withFixture</code>. Here's an example in
 * which the <code>StringBuilder</code> and <code>ListBuffer[String]</code> fixtures used in the previous examples have been
 * factored out into two <em>stackable fixture traits</em> named <code>Builder</code> and <code>Buffer</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.composingwithfixture
 * 
 * import org.scalatest._
 * import org.scalatest.SuiteMixin
 * import collection.mutable.ListBuffer
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 * 
 * class ThreadSafeListBufferOfString {
 *   private final val buf = ListBuffer.empty[String]
 *   def += (s: String): Unit = synchronized { buf += s }
 *   def toList: List[String] = synchronized { buf.toList }
 *   def clear(): Unit = synchronized { buf.clear() }
 *   def isEmpty: Boolean = synchronized { buf.isEmpty }
 * }
 * 
 * class ThreadSafeStringBuilder {
 *   private final val bldr = new StringBuilder
 *   def append(s: String): Unit =
 *     synchronized {
 *       bldr.append(s)
 *     }
 *   def clear(): Unit = synchronized { bldr.clear() }
 *   override def toString = synchronized { bldr.toString }
 * }
 * 
 * trait Builder extends AsyncSuiteMixin { this: AsyncSuite =&gt;
 * 
 *   final val builder = new ThreadSafeStringBuilder
 * 
 *   abstract override def withAsyncFixture(test: NoArgAsyncTest) = {
 *     builder.append("ScalaTest is ")
 *     withCleanup {
 *       super.withAsyncFixture(test) // To be stackable, must call super.withAsyncFixture
 *     } {
 *       builder.clear()
 *     }
 *   }
 * }
 * 
 * trait Buffer extends AsyncSuiteMixin { this: AsyncSuite =&gt;
 * 
 *   final val buffer = new ThreadSafeListBufferOfString
 * 
 *   abstract override def withAsyncFixture(test: NoArgAsyncTest) = {
 *     withCleanup {
 *       super.withAsyncFixture(test) // To be stackable, must call super.withAsyncFixture
 *     } {
 *       buffer.clear()
 *     }
 *   }
 * }
 * 
 * class ExampleSuite extends AsyncFunSuite with Builder with Buffer {
 * 
 *   implicit val executionContext = ExecutionContext.Implicits.global
 * 
 *   test("Testing should be easy") {
 *     Future {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *       succeed
 *     }
 *   }
 * 
 *   test("Testing should be fun") {
 *     Future {
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(buffer.isEmpty)
 *       buffer += "clear"
 *       succeed
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * By mixing in both the <code>Builder</code> and <code>Buffer</code> traits, <code>ExampleSuite</code> gets both fixtures, which will be
 * initialized before each test and cleaned up after. The order the traits are mixed together determines the order of execution.
 * In this case, <code>Builder</code> is &ldquo;super&rdquo; to <code>Buffer</code>. If you wanted <code>Buffer</code> to be &ldquo;super&rdquo;
 * to <code>Builder</code>, you need only switch the order you mix them together, like this: 
 * </p>
 *
 * <pre class="stHighlight">
 * class Example2Suite extends Suite with Buffer with Builder
 * </pre>
 *
 * <p>
 * And if you only need one fixture you mix in only that trait:
 * </p>
 *
 * <pre class="stHighlight">
 * class Example3Suite extends Suite with Builder
 * </pre>
 *
 * <p>
 * Another way to create stackable fixture traits is by extending the <a href="BeforeAndAfterEach.html"><code>BeforeAndAfterEach</code></a>
 * and/or <a href="BeforeAndAfterAll.html"><code>BeforeAndAfterAll</code></a> traits.
 * <code>BeforeAndAfterEach</code> has a <code>beforeEach</code> method that will be run before each test (like JUnit's <code>setUp</code>),
 * and an <code>afterEach</code> method that will be run after (like JUnit's <code>tearDown</code>).
 * Similarly, <code>BeforeAndAfterAll</code> has a <code>beforeAll</code> method that will be run before all tests,
 * and an <code>afterAll</code> method that will be run after all tests. Here's what the previously shown example would look like if it
 * were rewritten to use the <code>BeforeAndAfterEach</code> methods instead of <code>withFixture</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.composingbeforeandaftereach
 * 
 * import org.scalatest._
 * import org.scalatest.BeforeAndAfterEach
 * import collection.mutable.ListBuffer
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 * 
 * class ThreadSafeListBufferOfString {
 *   private final val buf = ListBuffer.empty[String]
 *   def += (s: String): Unit = synchronized { buf += s }
 *   def toList: List[String] = synchronized { buf.toList }
 *   def clear(): Unit = synchronized { buf.clear() }
 *   def isEmpty: Boolean = synchronized { buf.isEmpty }
 * }
 * 
 * class ThreadSafeStringBuilder {
 *   private final val bldr = new StringBuilder
 *   def append(s: String): Unit =
 *     synchronized {
 *       bldr.append(s)
 *     }
 *   def clear(): Unit = synchronized { bldr.clear() }
 *   override def toString = synchronized { bldr.toString }
 * }
 * 
 * trait Builder extends BeforeAndAfterEach { this: Suite =&gt;
 * 
 *   val builder = new ThreadSafeStringBuilder
 * 
 *   override def beforeEach() {
 *     builder.append("ScalaTest is ")
 *     super.beforeEach() // To be stackable, must call super.beforeEach
 *   }
 * 
 *   override def afterEach() {
 *     try super.afterEach() // To be stackable, must call super.afterEach
 *     finally builder.clear()
 *   }
 * }
 * 
 * trait Buffer extends BeforeAndAfterEach { this: Suite =&gt;
 * 
 *   val buffer = new ThreadSafeListBufferOfString
 * 
 *   override def afterEach() {
 *     try super.afterEach() // To be stackable, must call super.afterEach
 *     finally buffer.clear()
 *   }
 * }
 * 
 * class ExampleSuite extends AsyncFunSuite with Builder with Buffer {
 * 
 *   implicit val executionContext = ExecutionContext.Implicits.global
 * 
 *   test("Testing should be easy") {
 *     Future {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *       succeed
 *     }
 *   }
 * 
 *   test("Testing should be fun") {
 *     Future {
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(buffer.isEmpty)
 *       buffer += "clear"
 *       succeed
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * To get the same ordering as <code>withFixture</code>, place your <code>super.beforeEach</code> call at the end of each
 * <code>beforeEach</code> method, and the <code>super.afterEach</code> call at the beginning of each <code>afterEach</code>
 * method, as shown in the previous example. It is a good idea to invoke <code>super.afterEach</code> in a <code>try</code>
 * block and perform cleanup in a <code>finally</code> clause, as shown in the previous example, because this ensures the
 * cleanup code is performed even if <code>super.afterEach</code> throws an exception.
 * </p>
 *
 * <p>
 * The difference between stacking traits that extend <code>BeforeAndAfterEach</code> versus traits that implement <code>withFixture</code> is
 * that setup and cleanup code happens before and after the test in <code>BeforeAndAfterEach</code>, but at the beginning and
 * end of the test in <code>withFixture</code>. Thus if a <code>withFixture</code> method completes abruptly with an exception, it is
 * considered a failed test. By contrast, if any of the <code>beforeEach</code> or <code>afterEach</code> methods of <code>BeforeAndAfterEach</code> 
 * complete abruptly, it is considered an aborted suite, which will result in a <a href="events/SuiteAborted.html"><code>SuiteAborted</code></a> event.
 * </p>
 * 
 * <a name="sharedTests"></a><h2>Shared tests</h2>
 *
 * <p>
 * Sometimes you may want to run the same test code on different fixture objects. In other words, you may want to write tests that are "shared"
 * by different fixture objects.
 * To accomplish this in a <code>FunSuite</code>, you first place shared tests in
 * <em>behavior functions</em>. These behavior functions will be
 * invoked during the construction phase of any <code>FunSuite</code> that uses them, so that the tests they contain will
 * be registered as tests in that <code>FunSuite</code>.
 * For example, given this stack class:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunsuite.sharedtests
 * 
 * import scala.collection.mutable.ListBuffer
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 * 
 * class ThreadSafeStack[T] {
 * 
 *   final val MAX = 10
 *   private final val buf = new ListBuffer[T]
 * 
 *   def push(o: T): Unit =
 *     synchronized {
 *       if (buf.size != MAX)
 *         buf.prepend(o)
 *       else
 *         throw new IllegalStateException("can't push onto a full stack")
 *     }
 * 
 *   def pop(): T =
 *     synchronized {
 *       if (buf.size != 0)
 *         buf.remove(0)
 *       else
 *         throw new IllegalStateException("can't pop an empty stack")
 *     }
 * 
 *   def peek: T =
 *     synchronized {
 *       if (buf.size != 0)
 *         buf(0)
 *       else
 *         throw new IllegalStateException("can't pop an empty stack")
 *     }
 * 
 *   def full: Boolean = synchronized { buf.size == MAX }
 *   def empty: Boolean = synchronized { buf.size == 0 }
 *   def size = synchronized { buf.size }
 * 
 *   override def toString =
 *     synchronized { buf.mkString("ThreadSafeStack(", ", ", ")") }
 * }
 * </pre>
 *
 * <p>
 * You may want to test the <code>Stack</code> class in different states: empty, full, with one item, with one item less than capacity,
 * <em>etc</em>. You may find you have several tests that make sense any time the stack is non-empty. Thus you'd ideally want to run
 * those same tests for three stack fixture objects: a full stack, a stack with a one item, and a stack with one item less than
 * capacity. With shared tests, you can factor these tests out into a behavior function, into which you pass the
 * stack fixture to use when running the tests. So in your <code>FunSuite</code> for stack, you'd invoke the
 * behavior function three times, passing in each of the three stack fixtures so that the shared tests are run for all three fixtures.
 * </p>
 *
 * <p>
 * You can define a behavior function that encapsulates these shared tests inside the <code>FunSuite</code> that uses them. If they are shared
 * between different <code>FunSuite</code>s, however, you could also define them in a separate trait that is mixed into
 * each <code>FunSuite</code> that uses them.
 * <a name="StackBehaviors">For</a> example, here the <code>nonEmptyStack</code> behavior function (in this case, a
 * behavior <em>method</em>) is defined in a trait along with another
 * method containing shared tests for non-full stacks:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest.AsyncFunSuite
 * 
 * trait FunSuiteStackBehaviors { this: AsyncFunSuite =&gt;
 * 
 *   def nonEmptyStack( createNonEmptyStack: =&gt; Future[ThreadSafeStack[Int]],
 *       lastItemAdded: Int, name: String): Unit = {
 * 
 *     test("empty is invoked on this non-empty stack: " + name) {
 *       val futureStack = createNonEmptyStack
 *       futureStack map { stack =&gt; assert(!stack.empty) }
 *     }
 * 
 *     test("peek is invoked on this non-empty stack: " + name) {
 *       val futureStack = createNonEmptyStack
 *       futureStack map { stack =&gt;
 *         val size = stack.size
 *         assert(stack.peek === lastItemAdded)
 *         assert(stack.size === size)
 *       }
 *     }
 * 
 *     test("pop is invoked on this non-empty stack: " + name) {
 *       val futureStack = createNonEmptyStack
 *       futureStack map { stack =&gt;
 *         val size = stack.size
 *         assert(stack.pop === lastItemAdded)
 *         assert(stack.size === size - 1)
 *       }
 *     }
 *   }
 * 
 *   def nonFullStack(createNonFullStack: =&gt; Future[ThreadSafeStack[Int]],
 *       name: String): Unit = {
 * 
 *     test("full is invoked on this non-full stack: " + name) {
 *       val futureStack = createNonFullStack
 *       futureStack map { stack =&gt; assert(!stack.full) }
 *     }
 * 
 *     test("push is invoked on this non-full stack: " + name) {
 *       val futureStack = createNonFullStack
 *       futureStack map { stack =&gt;
 *         val size = stack.size
 *         stack.push(7)
 *         assert(stack.size === size + 1)
 *         assert(stack.peek === 7)
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Given these behavior functions, you could invoke them directly, but <code>FunSuite</code> offers a DSL for the purpose,
 * which looks like this:
 * </p>
 *
 * <pre class="stHighlight">
 * testsFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 * testsFor(nonFullStack(stackWithOneItem))
 * </pre>
 *
 * <p>
 * If you prefer to use an imperative style to change fixtures, for example by mixing in <code>BeforeAndAfterEach</code> and
 * reassigning a <code>stack</code> <code>var</code> in <code>beforeEach</code>, you could write your behavior functions
 * in the context of that <code>var</code>, which means you wouldn't need to pass in the stack fixture because it would be
 * in scope already inside the behavior function. In that case, your code would look like this:
 * </p>
 *
 * <pre class="stHighlight">
 * testsFor(nonEmptyStack) // assuming lastValuePushed is also in scope inside nonEmptyStack
 * testsFor(nonFullStack)
 * </pre>
 *
 * <p>
 * The recommended style, however, is the functional, pass-all-the-needed-values-in style. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * class StackFunSuite extends AsyncFunSuite with FunSuiteStackBehaviors {
 * 
 *   implicit val executionContext = ExecutionContext.Implicits.global
 * 
 *   // Stack fixture creation methods
 *   val emptyStackName = "empty stack"
 *   def emptyStack = Future { new ThreadSafeStack[Int] }
 * 
 *   val fullStackName = "full stack"
 *   def fullStack =
 *     Future {
 *       val stack = new ThreadSafeStack[Int]
 *       for (i <- 0 until stack.MAX)
 *         stack.push(i)
 *       stack
 *     }
 * 
 *   val almostEmptyStackName = "almost empty stack"
 *   def almostEmptyStack =
 *     Future {
 *       val stack = new ThreadSafeStack[Int]
 *       stack.push(9)
 *       stack
 *     }
 * 
 *   val almostFullStackName = "almost full stack"
 *   def almostFullStack =
 *     Future {
 *       val stack = new ThreadSafeStack[Int]
 *       for (i &lt;- 1 to 9)
 *         stack.push(i)
 *       stack
 *     }
 * 
 *   val lastValuePushed = 9
 * 
 *   test("empty is invoked on an empty stack") {
 *     val futureStack = emptyStack
 *     futureStack map { stack => assert(stack.empty) }
 *   }
 * 
 *   test("peek is invoked on an empty stack") {
 *     val futureStack = emptyStack
 *     futureStack map { stack =&gt;
 *       assertThrows[IllegalStateException] {
 *         stack.peek
 *       }
 *     }
 *   }
 * 
 *   test("pop is invoked on an empty stack") {
 *     val futureStack = emptyStack
 *     futureStack map { stack =&gt;
 *       assertThrows[IllegalStateException] {
 *         stack.pop
 *       }
 *     }
 *   }
 * 
 *   testsFor(nonEmptyStack(almostEmptyStack, lastValuePushed,
 *       almostEmptyStackName))
 *   testsFor(nonFullStack(almostEmptyStack, almostEmptyStackName))
 * 
 *   testsFor(nonEmptyStack(almostFullStack, lastValuePushed,
 *       almostFullStackName))
 *   testsFor(nonFullStack(almostFullStack, almostFullStackName))
 * 
 *   test("full is invoked on a full stack") {
 *     val futureStack = fullStack
 *     futureStack map { stack =&gt; assert(stack.full) }
 *   }
 * 
 *   testsFor(nonEmptyStack(fullStack, lastValuePushed, fullStackName))
 * 
 *   test("push is invoked on a full stack") {
 *     val futureStack = fullStack
 *     futureStack map { stack =&gt;
 *       assertThrows[IllegalStateException] {
 *         stack.push(10)
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you load these classes into the Scala interpreter (with scalatest's JAR file on the class path), and execute it,
 * you'll see:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new StackFunSuite execute
 * <span class="stGreen">StackFunSuite:
 * - empty is invoked on an empty stack
 * - peek is invoked on an empty stack
 * - pop is invoked on an empty stack
 * - empty is invoked on this non-empty stack: Stack(9)
 * - peek is invoked on this non-empty stack: Stack(9)
 * - pop is invoked on this non-empty stack: Stack(9)
 * - full is invoked on this non-full stack: Stack(9)
 * - push is invoked on this non-full stack: Stack(9)
 * - empty is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 * - peek is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 * - pop is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 * - full is invoked on this non-full stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 * - push is invoked on this non-full stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 * - full is invoked on a full stack
 * - empty is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
 * - peek is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
 * - pop is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
 * - push is invoked on a full stack</span>
 * </pre>
 * 
 * <p>
 * One thing to keep in mind when using shared tests is that in ScalaTest, each test in a suite must have a unique name.
 * If you register the same tests repeatedly in the same suite, one problem you may encounter is an exception at runtime
 * complaining that multiple tests are being registered with the same test name.
 * In a <code>FunSuite</code> there is no nesting construct analogous to <code>FunSpec</code>'s <code>describe</code> clause.
 * Therefore, you need to do a bit of
 * extra work to ensure that the test names are unique. If a duplicate test name problem shows up in a
 * <code>FunSuite</code>, you'll need to pass in a prefix or suffix string to add to each test name. You can pass this string
 * the same way you pass any other data needed by the shared tests, or just call <code>toString</code> on the shared fixture object.
 * This is the approach taken by the previous <code>FunSuiteStackBehaviors</code> example.
 * </p>
 *
 * <p>
 * Given this <code>FunSuiteStackBehaviors</code> trait, calling it with the <code>stackWithOneItem</code> fixture, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * testsFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 * </pre>
 *
 * <p>
 * yields test names:
 * </p>
 *
 * <ul>
 * <li><code>empty is invoked on this non-empty stack: Stack(9)</code></li>
 * <li><code>peek is invoked on this non-empty stack: Stack(9)</code></li>
 * <li><code>pop is invoked on this non-empty stack: Stack(9)</code></li>
 * </ul>
 *
 * <p>
 * Whereas calling it with the <code>stackWithOneItemLessThanCapacity</code> fixture, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * testsFor(nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed))
 * </pre>
 *
 * <p>
 * yields different test names:
 * </p>
 *
 * <ul>
 * <li><code>empty is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)</code></li>
 * <li><code>peek is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)</code></li>
 * <li><code>pop is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)</code></li>
 * </ul>
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
