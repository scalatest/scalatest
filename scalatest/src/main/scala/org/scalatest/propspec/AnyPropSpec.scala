/*
 * Copyright 2001-2019 Artima, Inc.
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
package org.scalatest.propspec

import org.scalatest.{Finders, Suite}

/**
 * A suite of property-based tests.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Class <code>AnyPropSpec</code> is a good fit for teams that want to write tests exclusively in terms of property checks, and is also a good choice
 * for writing the occasional <a href="#testMatrix">test matrix</a> when a different style trait is chosen as the main unit testing style.
 * </td></tr></table>
 * 
 * Here's an example <code>AnyPropSpec</code>:
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.propspec
 * 
 * import org.scalatest._
 * import prop._
 * import scala.collection.immutable._
 * 
 * class SetSpec extends propspec.AnyPropSpec with TableDrivenPropertyChecks with Matchers {
 * 
 *   val examples =
 *     Table(
 *       "set",
 *       BitSet.empty,
 *       HashSet.empty[Int],
 *       TreeSet.empty[Int]
 *     )
 *   
 *   property("an empty Set should have size 0") {
 *     forAll(examples) { set =&gt;
 *       set.size should be (0)
 *     }
 *   }
 * 
 *   property("invoking head on an empty set should produce NoSuchElementException") {
 *     forAll(examples) { set =&gt;
 *       a [NoSuchElementException] should be thrownBy { set.head }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * You can run a <code>AnyPropSpec</code> by invoking <code>execute</code> on it.
 * This method, which prints test results to the standard output, is intended to serve as a
 * convenient way to run tests from within the Scala interpreter. For example,
 * to run <code>SetSpec</code> from within the Scala interpreter, you could write:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; org.scalatest.run(new SetSpec)
 * </pre>
 *
 * <p>
 * And you would see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">SetSpec:
 * - an empty Set should have size 0
 * - invoking head on an empty Set should produce NoSuchElementException</span>
 * </pre>
 *
 * <p>
 * Or, to run just the &ldquo;<code>an empty Set should have size 0</code>&rdquo; method, you could pass that test's name, or any unique substring of the
 * name, such as <code>"size 0"</code> or even just <code>"0"</code>. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; org.scalatest.run(new SetSpec, "size 0")
 * <span class="stGreen">SetSpec:
 * - an empty Set should have size 0</span>
 * </pre>
 *
 * <p>
 * You can also pass to <code>execute</code> a <a href="ConfigMap.html"><em>config map</em></a> of key-value
 * pairs, which will be passed down into suites and tests, as well as other parameters that configure the run itself.
 * For more information on running in the Scala interpreter, see the documentation for <code>execute</code> (below) and the
 * <a href="Shell.html">ScalaTest shell</a>.
 * </p>
 *
 * <p>
 * The <code>execute</code> method invokes a <code>run</code> method that takes two
 * parameters. This <code>run</code> method, which actually executes the suite, will usually be invoked by a test runner, such
 * as <a href="run$.html"><code>run</code></a>, <a href="tools/Runner$.html"><code>tools.Runner</code></a>, a build tool, or an IDE.
 * </p>
 *
 * <p>
 * &ldquo;<code>property</code>&rdquo; is a method, defined in <code>AnyPropSpec</code>, which will be invoked
 * by the primary constructor of <code>SetSpec</code>. You specify the name of the test as
 * a string between the parentheses, and the test code itself between curly braces.
 * The test code is a function passed as a by-name parameter to <code>property</code>, which registers
 * it for later execution.
 * </p>
 *
 * <p>
 * A <code>AnyPropSpec</code>'s lifecycle has two phases: the <em>registration</em> phase and the
 * <em>ready</em> phase. It starts in registration phase and enters ready phase the first time
 * <code>run</code> is called on it. It then remains in ready phase for the remainder of its lifetime.
 * </p>
 *
 * <p>
 * Tests can only be registered with the <code>property</code> method while the <code>AnyPropSpec</code> is
 * in its registration phase. Any attempt to register a test after the <code>AnyPropSpec</code> has
 * entered its ready phase, <em>i.e.</em>, after <code>run</code> has been invoked on the <code>AnyPropSpec</code>,
 * will be met with a thrown <a href="exceptions/TestRegistrationClosedException.html"><code>TestRegistrationClosedException</code></a>. The recommended style
 * of using <code>AnyPropSpec</code> is to register tests during object construction as is done in all
 * the examples shown here. If you keep to the recommended style, you should never see a
 * <code>TestRegistrationClosedException</code>.
 * </p>
 *
 * <h2>Ignored tests</h2>
 *
 * <p>
 * To support the common use case of temporarily disabling a test, with the
 * good intention of resurrecting the test at a later time, <code>AnyPropSpec</code> provides registration
 * methods that start with <code>ignore</code> instead of <code>property</code>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.ignore
 * 
 * import org.scalatest._
 * import prop._
 * import scala.collection.immutable._
 * 
 * class SetSpec extends propspec.AnyPropSpec with TableDrivenPropertyChecks with Matchers {
 * 
 *   val examples =
 *     Table(
 *       "set",
 *       BitSet.empty,
 *       HashSet.empty[Int],
 *       TreeSet.empty[Int]
 *     )
 * 
 *   ignore("an empty Set should have size 0") {
 *     forAll(examples) { set =>
 *       set.size should be (0)
 *     }
 *   }
 * 
 *   property("invoking head on an empty set should produce NoSuchElementException") {
 *     forAll(examples) { set =>
 *       a [NoSuchElementException] should be thrownBy { set.head }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>SetSuite</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; org.scalatest.run(new SetSpec)
 * </pre>
 *
 * <p>
 * It will run only the second test and report that the first test was ignored:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">SetSuite:</span>
 * <span class="stYellow">- an empty Set should have size 0 !!! IGNORED !!!</span>
 * <span class="stGreen">- invoking head on an empty Set should produce NoSuchElementException</span>
 * </pre>
 *
 * <a name="informers"></a><h2>Informers</h2>
 *
 * <p>
 * One of the parameters to <code>AnyPropSpec</code>'s <code>run</code> method is a <a href="Reporter.html"><code>Reporter</code></a>, which
 * will collect and report information about the running suite of tests.
 * Information about suites and tests that were run, whether tests succeeded or failed, 
 * and tests that were ignored will be passed to the <code>Reporter</code> as the suite runs.
 * Most often the reporting done by default by <code>AnyPropSpec</code>'s methods will be sufficient, but
 * occasionally you may wish to provide custom information to the <code>Reporter</code> from a test.
 * For this purpose, an <a href="Informer.html"><code>Informer</code></a> that will forward information
 * to the current <code>Reporter</code> is provided via the <code>info</code> parameterless method.
 * You can pass the extra information to the <code>Informer</code> via its <code>apply</code> method.
 * The <code>Informer</code> will then pass the information to the <code>Reporter</code> via an <a href="events/InfoProvided.html"><code>InfoProvided</code></a> event.
 * Here's an example that shows both a direct use as well as an indirect use through the methods
 * of <a href="GivenWhenThen.html"><code>GivenWhenThen</code></a>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.propspec.info
 * 
 * import org.scalatest._
 * import prop._
 * import collection.mutable
 * 
 * class SetSpec extends propspec.AnyPropSpec with TableDrivenPropertyChecks with GivenWhenThen {
 * 
 *   val examples =
 *     Table(
 *       "set",
 *       mutable.BitSet.empty,
 *       mutable.HashSet.empty[Int],
 *       mutable.LinkedHashSet.empty[Int]
 *     )
 * 
 *   property("an element can be added to an empty mutable Set") {
 * 
 *     forAll(examples) { set =&gt;
 * 
 *       info("----------------")
 * 
 *       Given("an empty mutable " + set.getClass.getSimpleName)
 *       assert(set.isEmpty)
 * 
 *       When("an element is added")
 *       set += 99
 * 
 *       Then("the Set should have size 1")
 *       assert(set.size === 1)
 * 
 *       And("the Set should contain the added element")
 *       assert(set.contains(99))
 *     }
 *   }
 * }
 * </pre>
 *
 *
 * If you run this <code>AnyPropSpec</code> from the interpreter, you will see the following output:
 *
 * <pre class="stREPL">
 * scala&gt; org.scalatest.run(new SetSpec)
 * <span class="stGreen">SetSpec:
 * - an element can be added to an empty mutable Set
 *   + ---------------- 
 *   + Given an empty mutable BitSet 
 *   + When an element is added 
 *   + Then the Set should have size 1 
 *   + And the Set should contain the added element 
 *   + ---------------- 
 *   + Given an empty mutable HashSet 
 *   + When an element is added 
 *   + Then the Set should have size 1 
 *   + And the Set should contain the added element 
 *   + ---------------- 
 *   + Given an empty mutable LinkedHashSet 
 *   + When an element is added 
 *   + Then the Set should have size 1 
 *   + And the Set should contain the added element</span>
 * </pre>
 *
 * <a name="documenters"></a><h2>Documenters</h2>
 *
 * <p>
 * <code>AnyPropSpec</code> also provides a <code>markup</code> method that returns a <a href="Documenter.html"><code>Documenter</code></a>, which allows you to send
 * to the <code>Reporter</code> text formatted in <a href="http://daringfireball.net/projects/markdown/" target="_blank">Markdown syntax</a>.
 * You can pass the extra information to the <code>Documenter</code> via its <code>apply</code> method.
 * The <code>Documenter</code> will then pass the information to the <code>Reporter</code> via an <a href="events/MarkupProvided.html"><code>MarkupProvided</code></a> event.
 * </p>
 *
 * <p>
 * Here's an example <code>AnyPropSpec</code> that uses <code>markup</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.propspec.markup
 *
 * import org.scalatest._
 * import prop._
 * import collection.mutable
 *
 * class SetSpec extends propspec.AnyPropSpec with TableDrivenPropertyChecks with GivenWhenThen {
 *
 *   markup { """
 *
 * Mutable Set
 * -----------
 *
 * A set is a collection that contains no duplicate elements.
 *
 * To implement a concrete mutable set, you need to provide implementations
 * of the following methods:
 *
 *     def contains(elem: A): Boolean
 *     def iterator: Iterator[A]
 *     def += (elem: A): this.type
 *     def -= (elem: A): this.type
 *
 * If you wish that methods like `take`,
 * `drop`, `filter` return the same kind of set,
 * you should also override:
 *
 *     def empty: This
 *
 * It is also good idea to override methods `foreach` and
 * `size` for efficiency.
 *
 *   """ }
 *
 *   val examples =
 *     Table(
 *       "set",
 *       mutable.BitSet.empty,
 *       mutable.HashSet.empty[Int],
 *       mutable.LinkedHashSet.empty[Int]
 *     )
 *
 *   property("an element can be added to an empty mutable Set") {
 *
 *     forAll(examples) { set =>
 *
 *       info("----------------")
 *
 *       Given("an empty mutable " + set.getClass.getSimpleName)
 *       assert(set.isEmpty)
 *
 *       When("an element is added")
 *       set += 99
 *
 *       Then("the Set should have size 1")
 *       assert(set.size === 1)
 *
 *       And("the Set should contain the added element")
 *       assert(set.contains(99))
 *     }
 *
 *     markup("This test finished with a **bold** statement!")
 *   }
 * }
 * </pre>
 *
 * <p>
 * Although all of ScalaTest's built-in reporters will display the markup text in some form,
 * the HTML reporter will format the markup information into HTML. Thus, the main purpose of <code>markup</code> is to
 * add nicely formatted text to HTML reports. Here's what the above <code>SetSpec</code> would look like in the HTML reporter:
 * </p>
 *
 * <img class="stScreenShot" src="../../../lib/propSpec.gif">
 *
 * <a name="notifiersAlerters"></a><h2>Notifiers and alerters</h2>
 *
 * <p>
 * ScalaTest records text passed to <code>info</code> and <code>markup</code> during tests, and sends the recorded text in the <code>recordedEvents</code> field of
 * test completion events like <code>TestSucceeded</code> and <code>TestFailed</code>. This allows string reporters (like the standard out reporter) to show
 * <code>info</code> and <code>markup</code> text <em>after</em> the test name in a color determined by the outcome of the test. For example, if the test fails, string
 * reporters will show the <code>info</code> and <code>markup</code> text in red. If a test succeeds, string reporters will show the <code>info</code>
 * and <code>markup</code> text in green. While this approach helps the readability of reports, it means that you can't use <code>info</code> to get status
 * updates from long running tests.
 * </p>
 *
 * <p>
 * To get immediate (<em>i.e.</em>, non-recorded) notifications from tests, you can use <code>note</code> (a <a href="Notifier.html"><code>Notifier</code></a>) and <code>alert</code>
 * (an <a href="Alerter.html"><code>Alerter</code></a>). Here's an example showing the differences:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.propspec.note
 *
 * import org.scalatest._
 * import prop._
 * import collection.mutable
 *
 * class SetSpec extends propspec.AnyPropSpec with TableDrivenPropertyChecks {
 *
 *   val examples =
 *     Table(
 *       "set",
 *       mutable.BitSet.empty,
 *       mutable.HashSet.empty[Int],
 *       mutable.LinkedHashSet.empty[Int]
 *     )
 *
 *   property("an element can be added to an empty mutable Set") {
 *
 *     info("info is recorded")
 *     markup("markup is *also* recorded")
 *     note("notes are sent immediately")
 *     alert("alerts are also sent immediately")
 *
 *     forAll(examples) { set =>
 *
 *       assert(set.isEmpty)
 *       set += 99
 *       assert(set.size === 1)
 *       assert(set.contains(99))
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Because <code>note</code> and <code>alert</code> information is sent immediately, it will appear <em>before</em> the test name in string reporters, and its color will
 * be unrelated to the ultimate outcome of the test: <code>note</code> text will always appear in green, <code>alert</code> text will always appear in yellow.
 * Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; org.scalatest.run(new SetSpec)
 * <span class="stGreen">SetSpec:
 *   + notes are sent immediately</span>
 *   <span class="stYellow">+ alerts are also sent immediately</span>
 * <span class="stGreen">- an element can be added to an empty mutable Set
 *   + info is recorded
 *   + markup is *also* recorded</span>
 * </pre>
 *
 * <p>
 * Another example is <a href="tools/Runner$.html#slowpokeNotifications">slowpoke notifications</a>.
 * If you find a test is taking a long time to complete, but you're not sure which test, you can enable 
 * slowpoke notifications. ScalaTest will use an <code>Alerter</code> to fire an event whenever a test has been running
 * longer than a specified amount of time.
 * </p>
 *
 * <p>
 * In summary, use <code>info</code> and <code>markup</code> for text that should form part of the specification output. Use
 * <code>note</code> and <code>alert</code> to send status notifications. (Because the HTML reporter is intended to produce a
 * readable, printable specification, <code>info</code> and <code>markup</code> text will appear in the HTML report, but
 * <code>note</code> and <code>alert</code> text will not.)
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
 * it can call method <code>pending</code>, which will cause it to complete abruptly with <a href="exceptions/TestPendingException.html"><code>TestPendingException</code></a>.
 * </p>
 *
 * <p>
 * Because tests in ScalaTest can be designated as pending with <code>TestPendingException</code>, both the test name and any information
 * sent to the reporter when running the test can appear in the report of a test run. 
 * (The code of a pending test is executed just like any other test.) However, because the test completes abruptly
 * with <code>TestPendingException</code>, the test will be reported as pending, to indicate
 * the actual test, and possibly the functionality, has not yet been implemented.
 * </p>
 *
 * <p>
 * You can mark tests pending in <code>AnyPropSpec</code> like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest._
 * import prop._
 * import scala.collection.immutable._
 * 
 * class SetSpec extends propspec.AnyPropSpec with TableDrivenPropertyChecks with Matchers {
 * 
 *   val examples =
 *     Table(
 *       "set",
 *       BitSet.empty,
 *       HashSet.empty[Int],
 *       TreeSet.empty[Int]
 *     )
 * 
 *   property("an empty Set should have size 0") (pending)
 * 
 *   property("invoking head on an empty set should produce NoSuchElementException") {
 *     forAll(examples) { set =&gt;
 *       a [NoSuchElementException] should be thrownBy { set.head }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * (Note: "<code>(pending)</code>" is the body of the test. Thus the test contains just one statement, an invocation
 * of the <code>pending</code> method, which throws <code>TestPendingException</code>.)
 * If you run this version of <code>SetSuite</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; org.scalatest.run(new SetSuite)
 * </pre>
 *
 * <p>
 * It will run both tests, but report that first test is pending. You'll see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">SetSuite:</span>
 * <span class="stYellow">- An empty Set should have size 0 (pending)</span>
 * <span class="stGreen">- Invoking head on an empty Set should produce NoSuchElementException</span>
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
 * the body of pending tests are executed up until they throw <code>TestPendingException</code>. The reason for this difference
 * is that it enables your unfinished test to send <code>InfoProvided</code> messages to the reporter before it completes
 * abruptly with <code>TestPendingException</code>, as shown in the previous example on <code>Informer</code>s
 * that used the <code>GivenWhenThen</code> trait.
 * </p>
 *
 * <a name="taggingTests"></a><h2>Tagging tests</h2>
 *
 * <p>
 * A <code>AnyPropSpec</code>'s tests may be classified into groups by <em>tagging</em> them with string names.
 * As with any suite, when executing a <code>AnyPropSpec</code>, groups of tests can
 * optionally be included and/or excluded. To tag a <code>AnyPropSpec</code>'s tests,
 * you pass objects that extend class <code>org.scalatest.Tag</code> to methods
 * that register tests. Class <code>Tag</code> takes one parameter, a string name.  If you have
 * created tag annotation interfaces as described in the <a href="Tag.html"><code>Tag</code> documentation</a>, then you
 * will probably want to use tag names on your test functions that match. To do so, simply 
 * pass the fully qualified names of the tag interfaces to the <code>Tag</code> constructor. For example, if you've
 * defined a tag annotation interface with fully qualified names,
 * <code>com.mycompany.tags.DbTest</code>, then you could
 * create a matching tag for <code>AnyPropSpec</code>s like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.propspec.tagging
 *
 * import org.scalatest.Tag
 *
 * object DbTest extends Tag("com.mycompany.tags.DbTest")
 * </pre>
 *
 * <p>
 * Given these definitions, you could place <code>AnyPropSpec</code> tests into groups with tags like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest._
 * import prop._
 * import tagobjects.Slow
 * import scala.collection.immutable._
 * 
 * class SetSpec extends propspec.AnyPropSpec with TableDrivenPropertyChecks with Matchers {
 * 
 *   val examples =
 *     Table(
 *       "set",
 *       BitSet.empty,
 *       HashSet.empty[Int],
 *       TreeSet.empty[Int]
 *     )
 * 
 *   property("an empty Set should have size 0", Slow) {
 *     forAll(examples) { set =&gt;
 *       set.size should be (0)
 *     }
 *   }
 * 
 *   property("invoking head on an empty set should produce NoSuchElementException",
 *       Slow, DbTest) {
 * 
 *     forAll(examples) { set =&gt;
 *       a [NoSuchElementException] should be thrownBy { set.head }
 *     }
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
 * <li>Override <code>withFixture</code></li>
 * <li>Mix in a <em>before-and-after</em> trait</li>
 * </ul>
 *
 * <p>Each technique is geared towards helping you reduce code duplication without introducing
 * instance <code>var</code>s, shared mutable objects, or other dependencies between tests. Eliminating shared
 * mutable state across tests will make your test code easier to reason about and more amenable for parallel
 * test execution.</p>
 *
 * <p>
 * The techniques in <code>AnyPropSpec</code> are identical to those in <code>FunSuite</code>, but with &ldquo;<code>test</code>&rdquo;
 * replaced by &ldquo;<code>property</code>&rdquo;. The following table summarizes the options with a link to the relevant
 * documentation for trait <code>FunSuite</code>:
 * </p>
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
 *     <a href="FunSuite.html#getFixtureMethods">get-fixture methods</a>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     The <em>extract method</em> refactor helps you create a fresh instances of mutable fixture objects in each test
 *     that needs them, but doesn't help you clean them up when you're done.
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 *     <a href="FunSuite.html#fixtureContextObjects">fixture-context objects</a>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     By placing fixture methods and fields into traits, you can easily give each test just the newly created
 *     fixtures it needs by mixing together traits.  Use this technique when you need <em>different combinations
 *     of mutable fixture objects in different tests</em>, and don't need to clean up after.
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 *     <a href="FunSuite.html#loanFixtureMethods">loan-fixture methods</a>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Factor out dupicate code with the <em>loan pattern</em> when different tests need different fixtures <em>that must be cleaned up afterwards</em>.
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td colspan="2" style="background-color: #CCCCCC; border-width: 1px; padding: 3px; padding-top: 7px; border: 1px solid black; text-align: left">
 *     <strong>Override <code>withFixture</code> when most or all tests need the same fixture.</strong>
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 *     <a href="FunSuite.html#withFixtureNoArgTest">
 *       <code>withFixture(NoArgTest)</code></a>
 *     </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     <p>
 *     The recommended default approach when most or all tests need the same fixture treatment. This general technique
 *     allows you, for example, to perform side effects at the beginning and end of all or most tests, 
 *     transform the outcome of tests, retry tests, make decisions based on test names, tags, or other test data.
 *     Use this technique unless:
 *     </p>
 *  <dl>
 *  <dd style="display: list-item; list-style-type: disc; margin-left: 1.2em;">Different tests need different fixtures (refactor using Scala instead)</dd>
 *  <dd style="display: list-item; list-style-type: disc; margin-left: 1.2em;">An exception in fixture code should abort the suite, not fail the test (use a <em>before-and-after</em> trait instead)</dd>
 *  <dd style="display: list-item; list-style-type: disc; margin-left: 1.2em;">You have objects to pass into tests (override <code>withFixture(<em>One</em>ArgTest)</code> instead)</dd>
 *  </dl>
 *  </td>
 * </tr>
 *
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 *     <a href="FunSuite.html#withFixtureOneArgTest">
 *       <code>withFixture(OneArgTest)</code>
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
 *     <a href="FunSuite.html#beforeAndAfter"><code>BeforeAndAfter</code></a>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Use this boilerplate-buster when you need to perform the same side-effects before and/or after tests, rather than at the beginning or end of tests.
 *   </td>
 * </tr>
 *
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">
 *     <a href="FunSuite.html#composingFixtures"><code>BeforeAndAfterEach</code></a>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Use when you want to <em>stack traits</em> that perform the same side-effects before and/or after tests, rather than at the beginning or end of tests.
 *   </td>
 * </tr>
 *
 * </table>
 *
 * <a name="testMatrix"></a>
 * <h4>Using <code>AnyPropSpec</code> to implement a test matrix</h4>
 *
 * <p>
 * Using fixture-context objects in a <code>AnyPropSpec</code> is a good way to implement a test matrix.
 * What is the matrix? A test matrix is a series of tests that you need to run on a series of subjects. For example, The Scala API contains
 * many implementations of trait <code>Set</code>. Every implementation must obey the contract of <code>Set</code>. 
 * One property of any <code>Set</code> is that an empty <code>Set</code> should have size 0, another is that
 * invoking head on an empty <code>Set</code> should give you a <code>NoSuchElementException</code>, and so on. Already you have a matrix,
 * where rows are the properties and the columns are the set implementations:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">&nbsp;</th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><code>BitSet</code></th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><code>HashSet</code></th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><code>TreeSet</code></th></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">An empty Set should have size 0</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><span class="stGreen">pass</span></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><span class="stGreen">pass</span></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><span class="stGreen">pass</span></td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right">Invoking head on an empty set should produce NoSuchElementException</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><span class="stGreen">pass</span></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><span class="stGreen">pass</span></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><span class="stGreen">pass</span></td></td></tr>
 * </table>
 *
 * <p>
 * One way to implement this test matrix is to define a trait to represent the columns (in this case, <code>BitSet</code>, <code>HashSet</code>,
 * and <code>TreeSet</code>) as elements in a single-dimensional <code>Table</code>. Each element in the <code>Table</code> represents
 * one <code>Set</code> implementation. Because different properties may require different fixture instances for those implementations, you
 * can define a trait to hold the examples, like this:
 *
 * <pre class="stHighlight">
 * trait SetExamples extends Tables {
 *
 *   def examples = Table("set", bitSet, hashSet, treeSet)
 * 
 *   def bitSet: BitSet
 *   def hashSet: HashSet[Int]
 *   def treeSet: TreeSet[Int]
 * }
 * </pre>
 *
 * <p>
 * Given this trait, you could provide empty sets in one implementation of <code>SetExamples</code>, and non-empty sets in another.
 * Here's how you might provide empty set examples:
 * </p>
 *
 * <pre class="stHighlight">
 * class EmptySetExamples extends SetExamples {
 *   def bitSet = BitSet.empty
 *   def hashSet = HashSet.empty[Int]
 *   def treeSet = TreeSet.empty[Int]
 * }
 * </pre>
 * 
 * <p>
 * And here's how you might provide set examples with one item each:
 * </p>
 *
 * <pre class="stHighlight">
 * class SetWithOneItemExamples extends SetExamples {
 *   def bitSet = BitSet(1)
 *   def hashSet = HashSet(1)
 *   def treeSet = TreeSet(1)
 * }
 * </pre>
 * 
 * <p>
 * Armed with these example classes, you can define checks of properties that require
 * empty or non-empty set fixtures by using instances of these classes as fixture-context
 * objects. In other words, the columns of the test matrix are implemented as elements of
 * a one-dimensional table of fixtures, the rows are implemented as <code>property</code>
 * clauses of a <code>AnyPropSpec</code>.
 * </p>
 *
 * <p>
 * Here's a complete example that checks the two properties mentioned previously:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.propspec.matrix
 * 
 * import org.scalatest._
 * import org.scalatest.prop._
 * import scala.collection.immutable._
 * 
 * trait SetExamples extends Tables {
 *
 *   def examples = Table("set", bitSet, hashSet, treeSet)
 * 
 *   def bitSet: BitSet
 *   def hashSet: HashSet[Int]
 *   def treeSet: TreeSet[Int]
 * }
 * 
 * class EmptySetExamples extends SetExamples {
 *   def bitSet = BitSet.empty
 *   def hashSet = HashSet.empty[Int]
 *   def treeSet = TreeSet.empty[Int]
 * }
 * 
 * class SetSpec extends propspec.AnyPropSpec with TableDrivenPropertyChecks with Matchers {
 * 
 *   property("an empty Set should have size 0") {
 *     new EmptySetExamples {
 *       forAll(examples) { set =&gt;
 *         set.size should be (0)
 *       }
 *     }
 *   }
 * 
 *   property("invoking head on an empty set should produce NoSuchElementException") {
 *     new EmptySetExamples {
 *       forAll(examples) { set =&gt;
 *         a [NoSuchElementException] should be thrownBy { set.head }
 *       }
 *     }
 *   }
 * }
 * </pre>
 * 
 * <p>
 * One benefit of this approach is that the compiler will help you when you need to add either a new row
 * or column to the matrix. In either case, you'll need to ensure all cells are checked to get your code to compile.
 * </p>
 *
 * <a name="sharedTests"></a><h2>Shared tests</h2>
 *
 * <p>
 * Sometimes you may want to run the same test code on different fixture objects. That is to say, you may want to write tests that are "shared"
 * by different fixture objects.
 * You accomplish this in a <code>AnyPropSpec</code> in the same way you would do it in a <code>FunSuite</code>, except instead of <code>test</code>
 * you say <code>property</code>, and instead of <code>testsFor</code> you say <code>propertiesFor</code>. 
 * For more information, see the <a href="FunSuite.html#sharedTests">Shared tests</a> section of <code>FunSuite</code>'s
 * documentation.
 * </p>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.PropSpecFinder"))
class AnyPropSpec extends AnyPropSpecLike {

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
