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

/**
 * Wrapper <code>Suite</code> that passes an instance of the config map to the constructor of the
 * wrapped <code>Suite</code> when <code>run</code> is invoked.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Trait <code>ConfigMapWrapperSuite</code> is primarily intended to be used with the <a href="path/package.html">"path" traits</a>, which can't
 * use the usual approaches to accessing the config map because of the eager manner in which they run tests.</em>
 * </td></tr></table>
 * 
 * <p>
 * Each time <code>run</code> is invoked on an instance of <code>ConfigMapWrapperSuite</code>, this
 * suite will create a new instance of the suite to wrap, passing to the constructor the config map passed to
 * <code>run</code>. This way, if the same <code>ConfigMapWrapperSuite</code> instance is run multiple
 * times, each time with a different config map, an instance of the wrapped suite will be created
 * for each config map. In addition to being passed to the wrapped suite's constructor, the config map passed
 * to the <code>ConfigMapWrapperSuite</code>'s <code>run</code> method will also be passed to the <code>run</code>
 * method of the newly created wrapped suite instance.
 * </p>
 *
 * <p>
 * The config map is accessible inside a <code>Suite</code> in many ways. It is passed to <code>run</code>,
 * <code>runNestedSuites</code>, <code>runTests</code>, and <code>runTest</code>. It is also passed to
 * <code>withFixture</code>, accessible via a method on <a href="Suite$NoArgTest.html"><code>NoArgTest</code></a> and
 * <a href="fixture/Suite$OneArgTest.html"><code>OneArgTest</code></a>.
 * It is passed to an overloaded forms of the <code>beforeEach</code> and <code>afterEach</code> methods of trait
 * <a href="BeforeAndAfterEach.html"><code>BeforeAndAfterEach</code></a>, as well as overloaded forms of the <code>beforeAll</code> and
 * <code>afterAll</code> methods of trait <a href="BeforeAndAfterAll.html"><code>BeforeAndAfterAll</code></a>. Tests themselves can have information
 * taken from the config map, or the entire config map, through various means. The config map may be passed into
 * the test via a <a href="fixture/ConfigMapFixture.html"><code>ConfigMapFixture</code></a>, for example. Class <code>ConfigMapWrapperSuite</code>
 * represents one more way to get at the config map inside a suite of test: <code>ConfigMapWrapperSuite</code> will
 * pass the config map to the constructor of your suite class, bringing it easily into scope for tests and
 * helper methods alike.
 * </p>
 *
 * <p>
 * Having the config map passed to the suite constructor might be more convenient in some cases, but in the case
 * of the <a href="path/package.html"><code>org.scalatest.path</code></a> traits, it is necessary if a test needs
 * information from a config map. The reason is that in a path trait, the test code is executed eagerly,
 * <em>before <code>run</code> is invoked</em>. The results of the tests are registered when the tests are executed, and those
 * results are merely <em>reported</em> once <code>run</code> is invoked. Thus by the time <code>run</code> has been invoked, it
 * is too late to get the config map to the tests, which have already been executed. Using a <code>ConfigMapWrapperSuite</code> solves that problem.
 * By passing the config map to the constructor, it is available early enough for the running tests to use it.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest._
 *
 * @WrapWith(classOf[ConfigMapWrapperSuite])
 * class ExampleSpec(configMap: ConfigMap) extends path.FunSpec {
 *
 *   describe("A widget database") {
 *     it("should contain consistent values") {
 *       val dbName = configMap("WidgetDbName") // Can access config map
 *       // ...
 *     }
 *   }
 * }
 * </pre>
 *
 * @author Bill Venners
 */
final class ConfigMapWrapperSuite(clazz: Class[_ <: Suite]) extends Suite {

  private lazy val wrappedSuite = {
    val constructor = clazz.getConstructor(classOf[Map[_, _]])
    constructor.newInstance(Map.empty)
  }

  override def suiteId = clazz.getName

  /**
   * Returns the result obtained from invoking <code>expectedTestCount</code> on an instance of the wrapped
   * suite, constructed by passing an empty config map to its constructor, passing into the wrapped suite's
   * <code>expectedTestCount</code> method the specified <code>Filter</code>.
   *
   * @param filter the <code>Filter</code> to pass to the wrapped suite's <code>expectedTestCount</code> method
   * @return the result of invoking <code>expectedTestCount</code> on an instance of wrapped suite
   */
  override def expectedTestCount(filter: Filter): Int = wrappedSuite.expectedTestCount(filter)

  /**
   * Returns the result obtained from invoking <code>testNames</code> on an instance of the wrapped
   * suite, constructed by passing an empty config map to its constructor.
   *
   * @return the result of invoking <code>testNames</code> on an instance of wrapped suite
   */
  override def testNames: Set[String] = wrappedSuite.testNames

  /**
   * Returns the result obtained from invoking <code>nestedSuites</code> on an instance of the wrapped
   * suite, constructed by passing an empty config map to its constructor.
   *
   * @return the result of invoking <code>nestedSuites</code> on an instance of wrapped suite
   */
  override def nestedSuites: collection.immutable.IndexedSeq[Suite] = wrappedSuite.nestedSuites

  /**
   * Returns the result obtained from invoking <code>tags</code> on an instance of the wrapped
   * suite, constructed by passing an empty config map to its constructor.
   *
   * @return the result of invoking <code>testNames</code> on an instance of wrapped suite
   */
  override def tags: Map[String, Set[String]] = wrappedSuite.tags

  /**
   * Constructs a new instance of the suite to wrap, whose <code>Class</code> is passed to this
   * suite's constructor, passing in the specified config map, and invokes <code>run</code> on
   * that new instance, passing in the same arguments passed to this method.
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests and nested suites started by this method have completed, and whether or not a failure occurred.
   *
   * @throws NullPointerException if any passed parameter is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in the <code>Suite</code>
   */
  override def run(testName: Option[String], args: Args): Status = {
    val constructor = clazz.getConstructor(classOf[Map[_, _]])
    val suite = constructor.newInstance(args.configMap)
    suite.run(testName, args)
  }
}
  // TODO: Check the testName throwing an IAE behavior
