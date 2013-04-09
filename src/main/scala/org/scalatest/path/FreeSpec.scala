package org.scalatest.path

import org.scalatest.words.BehaveWord
import scala.collection.immutable.ListSet
import org.scalatest._
import org.scalatest.Suite.autoTagClassAnnotations

/**
 * A sister class to <code>org.scalatest.FreeSpec</code> that isolates tests by running each test in its own
 * instance of the test class, and for each test, only executing the <em>path</em> leading to that test.
 *
 * <p>
 * Class <code>path.FreeSpec</code> behaves similarly to class <code>org.scalatest.FreeSpec</code>, except that tests
 * are isolated based on their path. The purpose of <code>path.FreeSpec</code> is to facilitate writing
 * specification-style tests for mutable objects in a clear, boilerpate-free way. To test mutable objects, you need to
 * mutate them. Using a path class, you can make a statement in text, then implement that statement in code (including
 * mutating state), and nest and combine these test/code pairs in any way you wish. Each test will only see
 * the side effects of code that is in blocks that enclose the test. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.path
 * import org.scalatest.matchers.ShouldMatchers
 * import scala.collection.mutable.ListBuffer
 *
 * class ExampleSpec extends path.FreeSpec with ShouldMatchers {
 *
 *   "A ListBuffer" - {
 *
 *     val buf = ListBuffer.empty[Int] // This implements "A ListBuffer"
 *
 *     "should be empty when created" in {
 *
 *       // This test sees:
 *       //   val buf = ListBuffer.empty[Int]
 *       // So buf is: ListBuffer()
 *
 *       buf should be ('empty)
 *     }
 *
 *     "when 1 is appended" - {
 *
 *       buf += 1 // This implements "when 1 is appended", etc...
 *
 *       "should contain 1" in {
 *
 *         // This test sees:
 *         //   val buf = ListBuffer.empty[Int]
 *         //   buf += 1
 *         // So buf is: ListBuffer(1)
 *
 *         buf.remove(0) should equal (1)
 *         buf should be ('empty)
 *       }
 *
 *       "when 2 is appended" - {
 *
 *         buf += 2
 *
 *         "should contain 1 and 2" in {
 *
 *           // This test sees:
 *           //   val buf = ListBuffer.empty[Int]
 *           //   buf += 1
 *           //   buf += 2
 *           // So buf is: ListBuffer(1, 2)
 *
 *           buf.remove(0) should equal (1)
 *           buf.remove(0) should equal (2)
 *           buf should be ('empty)
 *         }
 *
 *         "when 2 is removed" - {
 *
 *           buf -= 2
 *
 *           "should contain only 1 again" in {
 *
 *             // This test sees:
 *             //   val buf = ListBuffer.empty[Int]
 *             //   buf += 1
 *             //   buf += 2
 *             //   buf -= 2
 *             // So buf is: ListBuffer(1)
 *
 *             buf.remove(0) should equal (1)
 *             buf should be ('empty)
 *           }
 *         }
 *
 *         "when 3 is appended" - {
 *
 *           buf += 3
 *
 *           "should contain 1, 2, and 3" in {
 *
 *             // This test sees:
 *             //   val buf = ListBuffer.empty[Int]
 *             //   buf += 1
 *             //   buf += 2
 *             //   buf += 3
 *             // So buf is: ListBuffer(1, 2, 3)
 *
 *             buf.remove(0) should equal (1)
 *             buf.remove(0) should equal (2)
 *             buf.remove(0) should equal (3)
 *             buf should be ('empty)
 *           }
 *         }
 *       }
 *
 *       "when 88 is appended" - {
 *
 *         buf += 88
 *
 *         "should contain 1 and 88" in {
 *
 *           // This test sees:
 *           //   val buf = ListBuffer.empty[Int]
 *           //   buf += 1
 *           //   buf += 88
 *           // So buf is: ListBuffer(1, 88)
 *
 *           buf.remove(0) should equal (1)
 *           buf.remove(0) should equal (88)
 *           buf should be ('empty)
 *         }
 *       }
 *     }
 *
 *     "should have size 0 when created" in {
 *
 *       // This test sees:
 *       //   val buf = ListBuffer.empty[Int]
 *       // So buf is: ListBuffer()
 *
 *       buf should have size 0
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Note that the above class is organized by writing a bit of specification text that opens a new block followed
 * by, at the top of the new block, some code that "implements" or "performs" what is described in the text. This is repeated as
 * the mutable object (here, a <code>ListBuffer</code>), is prepared for the enclosed tests. For example:
 * <p>
 *
 * <pre class="stHighlight">
 * "A ListBuffer" - {
 *   val buf = ListBuffer.empty[Int]
 * </pre>
 *
 * <p>
 * Or:
 * </p>
 *
 * <pre class="stHighlight">
 * "when 2 is appended" - {
 *   buf += 2
 * </pre>
 *
 * <p>
 * Note also that although each test mutates the <code>ListBuffer</code>, none of the other tests observe those
 * side effects:
 * <p>
 *
 * <pre class="stHighlight">
 * "should contain 1" in {
 *
 *   buf.remove(0) should equal (1)
 *   // ...
 * }
 *
 * "when 2 is appended" - {
 *
 *   buf += 2
 *
 *   "should contain 1 and 2" in {
 *
 *     // This test does not see the buf.remove(0) from the previous test,
 *     // so the first element in the ListBuffer is again 1
 *     buf.remove(0) should equal (1)
 *     buf.remove(0) should equal (2)
 * </pre>
 *
 * <p>
 * This kind of isolation of tests from each other is a consequence of running each test in its own instance of the test
 * class, and can also be achieved by simply mixing <code>OneInstancePerTest</code> into a regular
 * <code>org.scalatest.FreeSpec</code>. However, <code>path.FreeSpec</code> takes isolation one step further: a test
 * in a <code>path.FreeSpec</code> does not observe side effects performed outside tests in earlier blocks that do not
 * enclose it. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * "when 2 is removed" - {
 *
 *   buf -= 2
 *
 *   // ...
 * }
 *
 * "when 3 is appended" - {
 *
 *   buf += 3
 *
 *   "should contain 1, 2, and 3" in {
 *
 *     // This test does not see the buf -= 2 from the earlier "when 2 is removed" block,
 *     // because that block does not enclose this test, so the second element in the
 *     // ListBuffer is still 2
 *     buf.remove(0) should equal (1)
 *     buf.remove(0) should equal (2)
 *     buf.remove(0) should equal (3)
 * </pre>
 *
 * <p>
 * Running the full <code>ExampleSpec</code>, shown above, in the Scala interpeter would give you:
 * </p>
 *
 * <pre class="stREPL">
 * scala> import org.scalatest._
 * import org.scalatest._
 *
 * scala> run(new ExampleSpec)
 * <span class="stGreen">ExampleSpec:
 * A ListBuffer
 * - should be empty when created
 * &nbsp; when 1 is appended
 * &nbsp; - should contain 1
 * &nbsp;   when 2 is appended
 * &nbsp;   - should contain 1 and 2
 * &nbsp;     when 2 is removed
 * &nbsp;     - should contain only 1 again
 * &nbsp;     when 3 is appended
 * &nbsp;     - should contain 1, 2, and 3
 * &nbsp;   when 88 is appended
 * &nbsp;   - should contain 1 and 88
 * - should have size 0 when created</span>
 * </pre>
 *
 * <p>
 * <em>Note: class <code>path.FreeSpec</code>'s approach to isolation was inspired in part by the
 * <a href="https://github.com/orfjackal/specsy">specsy</a> framework, written by Esko Luontola.</em>
 * </p>
 *
 * <a name="sharedFixtures"></a><h2>Shared fixtures</h2>
 *
 * <p>
 * A test <em>fixture</em> is objects or other artifacts (such as files, sockets, database
 * connections, <em>etc.</em>) used by tests to do their work.
 * If a fixture is used by only one test, then the definitions of the fixture objects can
 * be local to the method. If multiple tests need to share an immutable fixture, you can simply
 * assign them to instance variables. If multiple tests need to share mutable fixture objects or <code>var</code>s,
 * there's one and only one way to do it in a <code>path.FreeSpec</code>: place the mutable objects lexically before
 * the test. Any mutations needed by the test must be placed lexically before and/or after the test.
 * As used here, "Lexically before" means that the code needs to be executed during construction of that test's
 * instance of the test class to <em>reach</em> the test (or put another way, the
 * code is along the "path to the test.") "Lexically after" means that the code needs to be executed to exit the
 * constructor after the test has been executed.
 * </p>
 *
 * <p>
 * The reason lexical placement is the one and only one way to share fixtures in a <code>path.FreeSpec</code> is because
 * all of its lifecycle methods are overridden and declared <code>final</code>. Thus you can't override
 * <code>withFixture</code>, because it is <code>final</code>, or mix in <code>BeforeAndAfter</code> or
 * <code>BeforeAndAfterEach</code>, because both override <code>runTest</code>, which is <code>final</code> in
 * a <code>path.FreeSpec</code>. In short:
 * </p>
 *
 * <p>
 * <table style="border-collapse: collapse; border: 1px solid black; width: 70%; margin: auto">
 * <tr>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 15px; text-align: left; border: 1px solid black; font-size: 125%; font-weight: bold">
 * In a <code>path.FreeSpec</code>, if you need some code to execute before a test, place that code lexically before
 * the test. If you need some code to execute after a test, place that code lexically after the test.
 * </th>
 * </tr>
 * </table>
 * </p>
 *
 * <p>
 * The reason the life cycle methods are final, by the way, is to prevent users from attempting to combine
 * a <code>path.FreeSpec</code>'s approach to isolation with other ways ScalaTest provides to share fixtures or
 * execute tests, because doing so could make the resulting test code hard to reason about. A
 * <code>path.FreeSpec</code>'s execution model is a bit magical, but because it executes in one and only one
 * way, users should be able to reason about the code.
 * To help you visualize how a <code>path.FreeSpec</code> is executed, consider the following variant of
 * <code>ExampleSpec</code> that includes print statements:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.path
 * import org.scalatest.matchers.ShouldMatchers
 * import scala.collection.mutable.ListBuffer
 *
 * class ExampleSpec extends path.FreeSpec with ShouldMatchers {
 *
 *   println("Start of: ExampleSpec")
 *   "A ListBuffer" - {
 *
 *     println("Start of: A ListBuffer")
 *     val buf = ListBuffer.empty[Int]
 *
 *     "should be empty when created" in {
 *
 *       println("In test: should be empty when created; buf is: " + buf)
 *       buf should be ('empty)
 *     }
 *
 *     "when 1 is appended" - {
 *
 *       println("Start of: when 1 is appended")
 *       buf += 1
 *
 *       "should contain 1" in {
 *
 *         println("In test: should contain 1; buf is: " + buf)
 *         buf.remove(0) should equal (1)
 *         buf should be ('empty)
 *       }
 *
 *       "when 2 is appended" - {
 *
 *         println("Start of: when 2 is appended")
 *         buf += 2
 *
 *         "should contain 1 and 2" in {
 *
 *           println("In test: should contain 1 and 2; buf is: " + buf)
 *           buf.remove(0) should equal (1)
 *           buf.remove(0) should equal (2)
 *           buf should be ('empty)
 *         }
 *
 *         "when 2 is removed" - {
 *
 *           println("Start of: when 2 is removed")
 *           buf -= 2
 *
 *           "should contain only 1 again" in {
 *
 *             println("In test: should contain only 1 again; buf is: " + buf)
 *             buf.remove(0) should equal (1)
 *             buf should be ('empty)
 *           }
 *
 *           println("End of: when 2 is removed")
 *         }
 *
 *         "when 3 is appended" - {
 *
 *           println("Start of: when 3 is appended")
 *           buf += 3
 *
 *           "should contain 1, 2, and 3" in {
 *
 *             println("In test: should contain 1, 2, and 3; buf is: " + buf)
 *             buf.remove(0) should equal (1)
 *             buf.remove(0) should equal (2)
 *             buf.remove(0) should equal (3)
 *             buf should be ('empty)
 *           }
 *           println("End of: when 3 is appended")
 *         }
 *
 *         println("End of: when 2 is appended")
 *       }
 *
 *       "when 88 is appended" - {
 *
 *         println("Start of: when 88 is appended")
 *         buf += 88
 *
 *         "should contain 1 and 88" in {
 *
 *           println("In test: should contain 1 and 88; buf is: " + buf)
 *           buf.remove(0) should equal (1)
 *           buf.remove(0) should equal (88)
 *           buf should be ('empty)
 *         }
 *
 *         println("End of: when 88 is appended")
 *       }
 *
 *       println("End of: when 1 is appended")
 *     }
 *
 *     "should have size 0 when created" in {
 *
 *       println("In test: should have size 0 when created; buf is: " + buf)
 *       buf should have size 0
 *     }
 *
 *     println("End of: A ListBuffer")
 *   }
 *   println("End of: ExampleSpec")
 *   println()
 * }
 * </pre>
 *
 * <p>
 * Running the above version of <code>ExampleSpec</code> in the Scala interpreter will give you output similar to:
 * </p>
 *
 * <pre class="stREPL">
 * scala> import org.scalatest._
 * import org.scalatest._
 *
 * scala> run(new ExampleSpec)
 * <span class="stGreen">ExampleSpec:</span>
 * Start of: ExampleSpec
 * Start of: A ListBuffer
 * In test: should be empty when created; buf is: ListBuffer()
 * End of: A ListBuffer
 * End of: ExampleSpec
 *
 * Start of: ExampleSpec
 * Start of: A ListBuffer
 * Start of: when 1 is appended
 * In test: should contain 1; buf is: ListBuffer(1)
 * ExampleSpec:
 * End of: when 1 is appended
 * End of: A ListBuffer
 * End of: ExampleSpec
 *
 * Start of: ExampleSpec
 * Start of: A ListBuffer
 * Start of: when 1 is appended
 * Start of: when 2 is appended
 * In test: should contain 1 and 2; buf is: ListBuffer(1, 2)
 * End of: when 2 is appended
 * End of: when 1 is appended
 * End of: A ListBuffer
 * End of: ExampleSpec
 *
 * Start of: ExampleSpec
 * Start of: A ListBuffer
 * Start of: when 1 is appended
 * Start of: when 2 is appended
 * Start of: when 2 is removed
 * In test: should contain only 1 again; buf is: ListBuffer(1)
 * End of: when 2 is removed
 * End of: when 2 is appended
 * End of: when 1 is appended
 * End of: A ListBuffer
 * End of: ExampleSpec
 *
 * Start of: ExampleSpec
 * Start of: A ListBuffer
 * Start of: when 1 is appended
 * Start of: when 2 is appended
 * Start of: when 3 is appended
 * In test: should contain 1, 2, and 3; buf is: ListBuffer(1, 2, 3)
 * End of: when 3 is appended
 * End of: when 2 is appended
 * End of: when 1 is appended
 * End of: A ListBuffer
 * End of: ExampleSpec
 *
 * Start of: ExampleSpec
 * Start of: A ListBuffer
 * Start of: when 1 is appended
 * Start of: when 88 is appended
 * In test: should contain 1 and 88; buf is: ListBuffer(1, 88)
 * End of: when 88 is appended
 * End of: when 1 is appended
 * End of: A ListBuffer
 * End of: ExampleSpec
 *
 * Start of: ExampleSpec
 * Start of: A ListBuffer
 * In test: should have size 0 when created; buf is: ListBuffer()
 * End of: A ListBuffer
 * End of: ExampleSpec
 *
 * <span class="stGreen">A ListBuffer
 * - should be empty when created
 *   when 1 is appended
 * &nbsp; - should contain 1
 * &nbsp;   when 2 is appended
 * &nbsp;   - should contain 1 and 2
 * &nbsp;     when 2 is removed
 * &nbsp;     - should contain only 1 again
 * &nbsp;     when 3 is appended
 * &nbsp;     - should contain 1, 2, and 3
 * &nbsp;   when 88 is appended
 * &nbsp;   - should contain 1 and 88
 * - should have size 0 when created</span>
 * </pre>
 *
 * <p>
 * Note that each test is executed in order of appearance in the <code>path.FreeSpec</code>, and that only
 * those <code>println</code> statements residing in blocks that enclose the test being run are executed. Any
 * <code>println</code> statements in blocks that do not form the "path" to a test are not executed in the
 * instance of the class that executes that test.
 * </p>
 *
 * <a name="howItExecutes" />
 * <h2>How it executes</h2>
 *
 * <p>
 * To provide its special brand of test isolation, <code>path.FreeSpec</code> executes quite differently from its
 * sister class in <code>org.scalatest</code>. An <code>org.scalatest.FreeSpec</code>
 * registers tests during construction and executes them when <code>run</code> is invoked. An
 * <code>org.scalatest.path.FreeSpec</code>, by contrast, runs each test in its own instance <em>while that
 * instance is being constructed</em>. During construction, it registers not the tests to run, but the results of
 * running those tests. When <code>run</code> is invoked on a <code>path.FreeSpec</code>, it reports the registered
 * results and does not run the tests again. If <code>run</code> is invoked a second or third time, in fact,
 * a <code>path.FreeSpec</code> will each time report the same results registered during construction. If you want
 * to run the tests of a <code>path.FreeSpec</code> anew, you'll need to create a new instance and invoke
 * <code>run</code> on that.
 * <p>
 *
 * <p>
 * A <code>path.FreeSpec</code> will create one instance for each "leaf" node it contains. The main kind of leaf node is
 * a test, such as:
 * </p>
 *
 * <pre class="stHighlight">
 * // One instance will be created for each test
 * "should be empty when created" in {
 *   buf should be ('empty)
 * }
 * </pre>
 *
 * <p>
 * However, an empty scope (a scope that contains no tests or nested scopes) is also a leaf node:
 * </p>
 *
 * <pre class="stHighlight">
 *  // One instance will be created for each empty scope
 * "when 99 is added" - {
 *   // A scope is "empty" and therefore a leaf node if it has no
 *   // tests or nested scopes, though it may have other code (which
 *   // will be executed in the instance created for that leaf node)
 *   buf += 99
 * }
 * </pre>
 *
 * <p>
 * The tests will be executed sequentially, in the order of appearance. The first test (or empty scope,
 * if that is first) will be executed when a class that mixes in <code>path.FreeSpec</code> is
 * instantiated. Only the first test will be executed during this initial instance, and of course, only
 * the path to that test. Then, the first time the client uses the initial instance (by invoking one of <code>run</code>,
 * <code>expectedTestsCount</code>, <code>tags</code>, or <code>testNames</code> on the instance), the initial instance will,
 * before doing anything else, ensure that any remaining tests are executed, each in its own instance.
 * </p>
 *
 * <p>
 * To ensure that the correct path is taken in each instance, and to register its test results, the initial
 * <code>path.FreeSpec</code> instance must communicate with the other instances it creates for running any subsequent
 * leaf nodes. It does so by setting a thread-local variable prior to creating each instance (a technique
 * suggested by Esko Luontola). Each instance
 * of <code>path.FreeSpec</code> checks the thread-local variable. If the thread-local is not set, it knows it
 * is an initial instance and therefore executes every block it encounters until it discovers, and executes the
 * first test (or empty scope, if that's the first leaf node). It then discovers, but does not execute the next
 * leaf node, or discovers there are no other leaf nodes remaining to execute. It communicates the path to the next
 * leaf node, if any, and the result of running the test it did execute, if any, back to the initial instance. The
 * initial instance repeats this process until all leaf nodes have been executed and all test results registered.
 * </p>
 *
 * <a name="ignoredTests" />
 * <h2>Ignored tests</h2>
 *
 * <p>
 * You mark a test as ignored in an <code>org.scalatest.path.FreeSpec</code> in the same manner as in
 * an <code>org.scalatest.FreeSpec</code>. Please see the <a href="../FreeSpec.html#ignoredTests">Ignored tests</a> section
 * in its documentation for more information.
 * </p>
 *
 * <p>
 * Note that a separate instance will be created for an ignored test,
 * and the path to the ignored test will be executed in that instance, but the test function itself will not
 * be executed. Instead, a <code>TestIgnored</code> event will be fired.
 * </p>
 *
 * <a name="informers" />
 * <h2>Informers</h2>
 *
 * <p>
 * You output information using <code>Informer</code>s in an <code>org.scalatest.path.FreeSpec</code> in the same manner
 * as in an <code>org.scalatest.FreeSpec</code>. Please see the <a href="../FreeSpec.html#informers">Informers</a>
 * section in its documentation for more information.
 * </p>
 *
 * <a name="pendingTests" />
 * <h2>Pending tests</h2>
 *
 * <p>
 * You mark a test as pending in an <code>org.scalatest.path.FreeSpec</code> in the same manner as in
 * an <code>org.scalatest.FreeSpec</code>. Please see the <a href="../FreeSpec.html#pendingTests">Pending tests</a>
 * section in its documentation for more information.
 * </p>
 * 
 * <p>
 * Note that a separate instance will be created for a pending test,
 * and the path to the ignored test will be executed in that instance, as well as the test function (up until it
 * completes abruptly with a <code>TestPendingException</code>).
 * </p>
 *
 * <a name="taggingTests" />
 * <h2>Tagging tests</h2>
 *
 * <p>
 * You can place tests into groups by tagging them in an <code>org.scalatest.path.FreeSpec</code> in the same manner
 * as in an <code>org.scalatest.FreeSpec</code>. Please see the <a href="../FreeSpec.html#taggingTests">Tagging tests</a>
 * section in its documentation for more information.
 * </p>
 *
 * <p>
 * Note that one difference between this class and its sister class
 * <code>org.scalatest.FreeSpec</code> is that because tests are executed at construction time, rather than each
 * time run is invoked, an <code>org.scalatest.path.FreeSpec</code> will always execute all non-ignored tests. When
 * <code>run</code> is invoked on a <code>path.FreeSpec</code>, if some tests are excluded based on tags, the registered
 * results of running those tests will not be reported. (But those tests will have already run and the results
 * registered.) By contrast, because an <code>org.scalatest.FreeSpec</code> only executes tests after <code>run</code>
 * has been called, and at that time the tags to include and exclude are known, only tests selected by the tags
 * will be executed.
 * </p>
 * 
 * <p>
 * In short, in an <code>org.scalatest.FreeSpec</code>, tests not selected by the tags to include
 * and exclude specified for the run (via the <code>Filter</code> passed to <code>run</code>) will not be executed.
 * In an <code>org.scalatest.path.FreeSpec</code>, by contrast, all non-ignored tests will be executed, each
 * during the construction of its own instance, and tests not selected by the tags to include and exclude specified
 * for a run will not be reported. (One upshot of this is that if you have tests that you want to tag as being slow so
 * you can sometimes exclude them during a run, you probably don't want to put them in a <code>path.FreeSpec</code>. Because
 * in a <code>path.Freespec</code> the slow tests will be run regardless, with only their registered results not being <em>reported</em>
 * if you exclude slow tests during a run.)
 * </p>
 *
 * <a name="SharedTests"></a><h2>Shared tests</h2>
 * <p>
 * You can factor out shared tests in an <code>org.scalatest.path.FreeSpec</code> in the same manner as in
 * an <code>org.scalatest.FreeSpec</code>. Please see the <a href="../FreeSpec.html#SharedTests">Shared tests</a>
 * section in its documentation for more information.
 * </p>
 *
 * <a name="nestedSuites"></a><h2>Nested suites</h2>
 *
 * <p>
 * Nested suites are not allowed in a <code>path.FreeSpec</code>. Because
 * a <code>path.FreeSpec</code> executes tests eagerly at construction time, registering the results of those test runs
 * and reporting them later when <code>run</code> is invoked, the order of nested suites versus test runs would be
 * different in a <code>org.scalatest.path.FreeSpec</code> than in an <code>org.scalatest.FreeSpec</code>. In
 * <code>org.scalatest.FreeSpec</code>'s implementation of <code>run</code>, nested suites are executed then tests
 * are executed. A <code>org.scalatest.path.FreeSpec</code> with nested suites would execute these in the opposite
 * order: first tests then nested suites. To help make <code>path.FreeSpec</code> code easier to
 * reason about by giving readers of one less difference to think about, nested suites are not allowed. If you want
 * to add nested suites to a <code>path.FreeSpec</code>, you can instead wrap them all in a
 * <a href="../Suites.html"><code>Suites</code></a> or <a href="../Specs.html"><code>Specs</code></a> object. They will
 * be executed in the order of appearance (unless a <a href="../Distributor">Distributor</a> is passed, in which case
 * they will execute in parallel).
 * </p>

 * </p>
 *
 * <a name="durations"></a><h2>Durations</h2>
 * <p>
 * Many ScalaTest events include a duration that indicates how long the event being reported took to execute. For
 * example, a <code>TestSucceeded</code> event provides a duration indicating how long it took for that test
 * to execute. A <code>SuiteCompleted</code> event provides a duration indicating how long it took for that entire
 * suite of tests to execute.
 * </p>
 *
 * <p>
 * In the test completion events fired by a <code>path.FreeSpec</code> (<code>TestSucceeded</code>,
 * <code>TestFailed</code>, or <code>TestPending</code>), the durations reported refer
 * to the time it took for the tests to run. This time is registered with the test results and reported along
 * with the test results each time <code>run</code> is invoked.
 * By contrast, the suite completion events fired for a <code>path.FreeSpec</code> represent the amount of time
 * it took to report the registered results. (These events are not fired by <code>path.FreeSpec</code>, but instead
 * by the entity that invokes <code>run</code> on the <code>path.FreeSpec</code>.) As a result, the total time
 * for running the tests of a <code>path.FreeSpec</code>, calculated by summing the durations of all the individual
 * test completion events, may be greater than the duration reported for executing the entire suite.
 * </p>
 *
 * @author Bill Venners
 * @author Chua Chee Seng
 */
@Finders(Array("org.scalatest.finders.FreeSpecFinder"))
class FreeSpec extends FreeSpecLike

