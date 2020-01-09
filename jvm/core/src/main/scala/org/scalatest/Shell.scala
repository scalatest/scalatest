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

/*
Delete this later:
class ArithmeticSuite extends FunSuite with matchers.Matchers {
  test("addition works") {
    1 + 1 should equal (2)
  }
  ignore("subtraction works") {
    1 - 1 should equal (0)
  }
  test("multiplication works") {
    1 * 1 should equal (2)
  }
  test("division works") (pending)
}
*/

/**
 * Trait whose instances provide a <a href="run$.html"><code>run</code></a> method and configuration fields that implement
 * the <em>ScalaTest shell</em>: its DSL for the Scala interpreter.
 *
 * <p>
 * The main command of the ScalaTest shell is <code>run</code>, which you can use to run a suite of tests.
 * The shell also provides several commands for configuring a call to <code>run</code>:
 * </p>
 *
 * <ul>
 * <li><code>color</code> (the default) - display results in color (green for success; red for failure; yellow for warning; blue for statistics)</li>
 * <li><code>nocolor</code> - display results without color</li>
 * <li><code>durations</code> - display durations of (<em>i.e.</em>, how long it took to run) tests and suites</li>
 * <li><code>nodurations</code> (the default) - do not display durations of tests and suites</li>
 * <li><code>shortstacks</code> - display short (<em>i.e.</em>, truncated to show just the most useful portion) stack traces for all exceptions</li>
 * <li><code>fullstacks</code> - display full stack trackes for all exceptions</li>
 * <li><code>nostacks</code> (the default) - display no stack trace for <code>StackDepth</code> exceptions and a short stack trace for non-<code>StackDepth</code>
 *   exceptions</li>
 * <li><code>stats</code> - display statistics before and after the run, such as expected test count before the run and tests succeeded, failed, pending,
 * <em>etc.</em>, counts after the run</li>
 * <li><code>nostats</code> (the default) not display statistics before or after the run</li>
 * </ul>
 *
 * <p>
 * The default configuration is <code>color</code>, <code>nodurations</code>, <code>nostacks</code>, and <code>nostats</code>.
 * </p>
 *
 * <p>
 * All of these commands are fields of trait <code>org.scalatest.Shell</code>. Each configuration command is a field that refers to
 * another <code>Shell</code> instance with every configuration parameter
 * the same except for the one you've asked to change. For example, <code>durations</code> provides a
 * <code>Shell</code> instance that has every parameter configured the same way, except with durations enabled. When you invoke
 * <code>run</code> on that, you will get a run with durations enabled and every other configuration parameter at its default value.
 * <p>
 *
 * <p>
 * The other useful "command"
 * to know about, though not technically part of the shell, is the <code>apply</code> factory method in the <a href="Suites$.html"><code>Suites</code></a>
 * singleton object. This allows you to easily create composite suites out of nested suites, which you can then pass to <code>run</code>. This
 * will be demonstrated later in this documentation.
 * </p>
 *
 * <h2>Using the ScalaTest shell</h2>
 *
 * <p>
 * The package object of the <code>org.scalatest</code> package, although it does not extend <code>Shell</code>, declares all the
 * same members as <code>Shell</code>. Its <code>run</code> method runs with all the <code>Shell</code> configuration parameters set
 * to their default values. A good way to use the ScalaTest shell, therefore, is to import the members of package <code>org.scalatest</code>:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> import org.scalatest._
 * import org.scalatest._</span>
 * </pre>
 *
 * <p>
 * One thing importing <code>org.scalatest._</code> allows you to do is access any of ScalaTest's classes and traits by shorter
 * names, for example:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> class ArithmeticSuite extends FunSuite with matchers.Matchers {
 *      |   test("addition works") {
 *      |     1 + 1 should equal (2)
 *      |   }
 *      |   ignore("subtraction works") {
 *      |     1 - 1 should equal (0)
 *      |   }
 *      |   test("multiplication works") {
 *      |     1 * 1 should equal (2)
 *      |   }
 *      |   test("division works") (pending)
 *      | }
 * defined class ArithmeticSuite</span>
 * </pre>
 *
 * <p>
 * But importing <code>org.scalatest._</code> also brings into scope the commands of the <code>Shell</code>, so you can, for
 * example, invoke <code>run</code> without qualification:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> run(new ArithmeticSuite)</span>
 * <span style="color: #00cc00">ArithmeticSuite:
 * - addition works</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED ***
 *   1 did not equal 2 (<console>:16)</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * </pre>
 *
 * <h2>Configuring a single run</h2>
 *
 * <p>
 * To configure a single run, you can prefix run by one or more configuration commands, separated by dots. For example, to enable
 * durations during a single run, you would write:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> durations.run(new ArithmeticSuite)</span>
 * <span style="color: #00cc00">ArithmeticSuite:
 * - addition works (102 milliseconds)</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED *** (36 milliseconds)
 *   1 did not equal 2 (<console>:16)</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * </pre>
 *
 * <p>
 * To enable statistics during a single run, you would write:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> stats.run(new ArithmeticSuite)</span>
 * <span style="color: #00dddd">Run starting. Expected test count is: 3</span>
 * <span style="color: #00cc00">ArithmeticSuite:
 * - addition works</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED ***
 *   1 did not equal 2 (<console>:16)</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * <span style="color: #00dddd">Run completed in 386 milliseconds.
 * Total number of tests run: 2
 * Suites: completed 1, aborted 0
 * Tests: succeeded 1, failed 1, ignored 1, pending 1</span>
 * <span style="color: #dd2233">*** 1 TEST FAILED ***</span>
 * </pre>
 *
 * <p>
 * And to enable both durations and statistics during a single run, you could write:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> durations.stats.run(new ArithmeticSuite)</span>
 * <span style="color: #00dddd">Run starting. Expected test count is: 3</span>
 * <span style="color: #00cc00">ArithmeticSuite:
 * - addition works (102 milliseconds)</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED (36 milliseconds)***
 *   1 did not equal 2 (<console>:16)</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * <span style="color: #00dddd">Run completed in 386 milliseconds.
 * Total number of tests run: 2
 * Suites: completed 1, aborted 0
 * Tests: succeeded 1, failed 1, ignored 1, pending 1</span>
 * <span style="color: #dd2233">*** 1 TEST FAILED ***</span>
 * </pre>
 *
 * <p>
 * The order doesn't matter when you are chaining multiple configuration commands. You'll get the same
 * result whether you write <code>durations.stats.run</code> or <code>stats.durations.run</code>.
 * </p>
 *
 * <p>
 * To disable color, use <code>nocolor</code>:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> nocolor.run(new ArithmeticSuite)
 * ArithmeticSuite:
 * - addition works
 * - subtraction works !!! IGNORED !!!
 * - multiplication works *** FAILED ***
 *   1 did not equal 2 (<console>:16)
 * - division works (pending)</span>
 * </pre>
 *
 * <p>
 * To enable short stack traces during a single run, use <code>shortstacks</code>:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> shortstacks.run(new ArithmeticSuite)</span>
 * <span style="color: #00cc00">ArithmeticSuite:
 * - addition works (101 milliseconds)</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED *** (33 milliseconds)
 *   1 did not equal 2 (<console>:16)
 *   org.scalatest.exceptions.TestFailedException:
 *   ...
 *   at line2$object$$iw$$iw$$iw$$iw$ArithmeticSuite$$anonfun$3.apply$mcV$sp(<console>:16)
 *   at line2$object$$iw$$iw$$iw$$iw$ArithmeticSuite$$anonfun$3.apply(<console>:16)
 *   at line2$object$$iw$$iw$$iw$$iw$ArithmeticSuite$$anonfun$3.apply(<console>:16)
 *   at org.scalatest.FunSuite$$anon$1.apply(FunSuite.scala:992)
 *   at org.scalatest.Suite$class.withFixture(Suite.scala:1661)
 *   at line2$object$$iw$$iw$$iw$$iw$ArithmeticSuite.withFixture(<console>:8)
 *   at org.scalatest.FunSuite$class.invokeWithFixture$1(FunSuite.scala:989)
 *   ...</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * </pre>
 *
 * <h2>Changing the default configuration</h2>
 *
 * <p>
 * If you want to change the default for multiple runs, you can import the members of your favorite <code>Shell</code> configuration. For example,
 * if you <em>always</em> like to run with durations and statistics enabled, you could write:
 * <p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> import stats.durations._
 * import stats.durations._</span>
 * </pre>
 *
 * <p>
 * Now anytime you run statistics and durations will, by default, be enabled:
 * <p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> run(new ArithmeticSuite)</span>
 * <span style="color: #00dddd">Run starting. Expected test count is: 3</span>
 * <span style="color: #00cc00">ArithmeticSuite:
 * - addition works (9 milliseconds)</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED *** (10 milliseconds)
 *   1 did not equal 2 (<console>:18)</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * <span style="color: #00dddd">Run completed in 56 milliseconds.
 * Total number of tests run: 2
 * Suites: completed 1, aborted 0
 * Tests: succeeded 1, failed 1, ignored 1, pending 1</span>
 * <span style="color: #dd2233">*** 1 TEST FAILED ***</span>
 * </pre>
 *
 * <h2>Running multiple suites</h2>
 *
 * <p>
 * If you want to run multiple suites, you can use the factory method in the <a href="Suites$.html"><code>Suites</code></a>
 * singleton object. If you wrap a comma-separated list of suite instances inside <code>Suites(...)</code>, for example,
 * you'll get a suite instance that contains no tests, but whose nested suites includes the suite instances you placed between
 * the parentheses. You can place <code>Suites</code> inside <code>Suites</code> to any level of depth, creating a tree of
 * suites to pass to <code>run</code>. Here's a (contrived) example in which <code>ArithmeticSuite</code> is executed four times:
 * <p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> run(Suites(new ArithmeticSuite, new ArithmeticSuite, Suites(new ArithmeticSuite, new ArithmeticSuite)))</span>
 * <span style="color: #00dddd">Run starting. Expected test count is: 12</span>
 * <span style="color: #00cc00">Suites:
 * ArithmeticSuite:
 * - addition works (0 milliseconds)</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED *** (1 millisecond)
 *   1 did not equal 2 (<console>:16)</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * <span style="color: #00cc00">ArithmeticSuite:
 * - addition works (1 millisecond)</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED *** (0 milliseconds)
 *   1 did not equal 2 (<console>:16)</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * <span style="color: #00cc00">Suites:
 * ArithmeticSuite:
 * - addition works (0 milliseconds)</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED *** (0 milliseconds)
 *   1 did not equal 2 (<console>:16)</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * <span style="color: #00cc00">ArithmeticSuite:
 * - addition works (0 milliseconds)</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED *** (0 milliseconds)
 *   1 did not equal 2 (<console>:16)</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * <span style="color: #00dddd">Run completed in 144 milliseconds.
 * Total number of tests run: 8
 * Suites: completed 6, aborted 0
 * Tests: succeeded 4, failed 4, ignored 4, pending 4</span>
 * <span style="color: #dd2233">*** 4 TESTS FAILED ***</span>
 * </pre>
 *
 * <h2>Running a single test</h2>
 *
 * <p>
 * The <code>run</code> command also allows you to specify the name of a test to run and/or a config map. You can run
 * a particular test in a suite, for example, by specifying the test name after the suite instance in your call to <code>run</code>, like this:
 * <p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala> run(new ArithmeticSuite, "addition works")</span>
 * <span style="color: #00cc00">ArithmeticSuite:
 * - addition works</span>
 * </pre>
 */
sealed trait Shell {

  /**
   * A <code>Shell</code> whose <code>run</code> method will pass <code>true</code> for <code>execute</code>'s <code>color</code>
   * parameter, and pass for all other parameters the same values as this <code>Shell</code>.
   */
  def color: Shell

  /**
   * A <code>Shell</code> whose <code>run</code> method will pass <code>true</code> for <code>execute</code>'s <code>durations</code>
   * parameter, and pass for all other parameters the same values as this <code>Shell</code>.
   */
  def durations: Shell

  /**
   * A <code>Shell</code> whose <code>run</code> method will pass <code>true</code> for <code>execute</code>'s <code>shortstacks</code>
   * parameter and <code>false</code> for its <code>fullstacks</code> parameter, and pass for all other parameters the same values as
   * this <code>Shell</code>.
   */
  def shortstacks: Shell

  /**
   * A <code>Shell</code> whose <code>run</code> method will pass <code>false</code> for <code>execute</code>'s <code>shortstacks</code>
   * parameter and <code>true</code> for its <code>fullstacks</code> parameter, and pass for all other parameters the same values as this <code>Shell</code>.
   */
  def fullstacks: Shell

  /**
   * A <code>Shell</code> whose <code>run</code> method will pass <code>true</code> for <code>execute</code>'s <code>stats</code>
   * parameter, and pass for all other parameters the same values as this <code>Shell</code>.
   */
  def stats: Shell

  /**
   * Returns a copy of this <code>Shell</code> with <code>colorPassed</code> configuration parameter set to <code>false</code>.
   */
  def nocolor: Shell

  /**
   * Returns a copy of this <code>Shell</code> with <code>durationsPassed</code> configuration parameter set to <code>false</code>.
   */
  def nodurations: Shell

  /**
   * Returns a copy of this <code>Shell</code> with <code>shortStacksPassed</code> configuration parameter set to <code>false</code>.
   */
  def nostacks: Shell

  /**
   * Returns a copy of this <code>Shell</code> with <code>statsPassed</code> configuration parameter set to <code>false</code>.
   */
  def nostats: Shell

  /**
   * Run the passed suite, optionally passing in a test name and config map.
   *
   * <p>
   * This method will invoke <code>execute</code> on the passed <code>suite</code>, passing in
   * the specified (or default) <code>testName</code> and <code>configMap</code> and a set of configuration values. A
   * particular <code>Shell</code> instance will always pass the same configuration values (<code>color</code>,
   * <code>durations</code>, <code>shortstacks</code>, <code>fullstacks</code>, and <code>stats</code>) to <code>execute</code> each time
   * this method is invoked.
   * </p>
   */
  def run(suite: Suite, testName: String = null, configMap: ConfigMap = ConfigMap.empty): Unit
}

// parameters were private, but after pulling out the trait so I don't import copy() as part
// of the package object, I made the whole case class private[scalatest], so I made these normal
// so that I could write some tests against it.
private[scalatest] final case class ShellImpl(
  colorPassed: Boolean = true,
  durationsPassed: Boolean = false,
  shortstacksPassed: Boolean = false,
  fullstacksPassed: Boolean = false,
  statsPassed: Boolean = false
) extends Shell {

  lazy val color: Shell = copy(colorPassed = true)
  lazy val durations: Shell = copy(durationsPassed = true)
  lazy val shortstacks: Shell = copy(shortstacksPassed = true, fullstacksPassed = false)
  lazy val fullstacks: Shell = copy(fullstacksPassed = true, shortstacksPassed = false)
  lazy val stats: Shell = copy(statsPassed = true)
  lazy val nocolor: Shell = copy(colorPassed = false)
  lazy val nodurations: Shell = copy(durationsPassed = false)
  lazy val nostacks: Shell = copy(shortstacksPassed = false, fullstacksPassed = false)
  lazy val nostats: Shell = copy(statsPassed = false)

  def run(suite: Suite, testName: String = null, configMap: ConfigMap = ConfigMap.empty): Unit = {
    suite.execute(testName, configMap, colorPassed, durationsPassed, shortstacksPassed, fullstacksPassed, statsPassed)
  }
}
