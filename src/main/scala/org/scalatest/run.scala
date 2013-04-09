/*
 * Copyright 2001-2011 Artima, Inc.
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
 * Singleton object providing an <code>apply</code> method for the ScalaTest shell and a
 * <code>main</code> method for ScalaTest's simple runner.
 *
 * <p>
 * The <code>apply</code> method can be used in the ScalaTest Shell (its DSL for the Scala
 * interpreter) in this way:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; class ArithmeticSuite extends FunSuite with ShouldMatchers {
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
 * defined class ArithmeticSuite
 *
 * scala&gt; run(new ArithmeticSuite)</span>
 * <span style="color: #00cc00">ArithmeticSuite:
 * - addition works</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED ***
 *   1 did not equal 2 (<console>:16)</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * </pre>
 *
 * <p>
 * The last command is calling the <code>apply</code> method on the <code>run</code> singleton object. In other
 * words, you could alternatively call it this way:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">scala&gt; run.apply(new ArithmeticSuite)</span>
 * <span style="color: #00cc00">ArithmeticSuite:
 * - addition works</span>
 * <span style="color: #cfc923">- subtraction works !!! IGNORED !!!</span>
 * <span style="color: #dd2233">- multiplication works *** FAILED ***
 *   1 did not equal 2 (<console>:16)</span>
 * <span style="color: #cfc923">- division works (pending)</span>
 * </pre>
 *
 * <p>
 * The <code>run</code> singleton object also serves a different purpose. Its <code>main</code> method
 * allows users to "run" <code>run</code> as a Scala application. ScalaTest's <code>Runner</code> application is very
 * powerful, but doesn't provide the simplest out-of-box experience for people trying ScalaTest for the first time. For example,
 * to run an <code>ExampleSpec</code> in the unnamed package from the directory where it is compiled with
 * <code>Runner</code>'s standard out reporter requires this command:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">$ scala -cp scalatest-RELEASE.jar org.scalatest.tools.Runner -R . -o -s ExampleSpec</span>
 * </pre>
 *
 * <p>
 * Running it with the <code>run</code> application is simpler:
 * </p>
 *
 * <pre style="background-color: #2c415c; padding: 10px">
 * <span style="color: white">$ scala -cp scalatest-RELEASE.jar org.scalatest.run ExampleSpec</span>
 * </pre>
 *
 *
 */
object run {

  private val defaultShell = ShellImpl()

  /**
   * Run the suites whose fully qualified names are passed as arguments.
   *
   * <p>
   * This method will invoke the main method of <code>org.scalatest.tools.Runner</code>, passing
   * in <code>"-R ."</code> to set the runpath to the current directory, <code>"-o"</code> to select the
   * standard out reporter, and each argument preceded by <code>-s</code>. For example, this <code>run</code>
   * command:
   * </p>
   *
   * <pre style="background-color: #2c415c; padding: 10px">
   * <span style="color: white">$ scala -cp scalatest-RELEASE.jar org.scalatest.run ExampleSpec</span>
   * </pre>
   *
   * <p>
   * Has the same effect as this <code>Runner</code> command:
   * </p>
   *
   * <pre style="background-color: #2c415c; padding: 10px">
   * <span style="color: white">$ scala -cp scalatest-RELEASE.jar org.scalatest.tools.Runner -R . -o -s ExampleSpec</span>
   * </pre>
   *
   * @param args
   */
  def main(args: Array[String]) {
    tools.Runner.main(Array("-R", ".", "-o") ++ args.flatMap(s => Array("-s", s)))
  }

  /**
   * Run the passed suite, optionally passing in a test name and config map. 
   *
   * <p>
   * This method will invoke <code>execute</code> on the passed <code>suite</code>, passing in
   * the specified (or default) <code>testName</code> and <code>configMap</code> and the configuration values
   * passed to this <code>Shell</code>'s constructor (<code>colorPassed</code>, <code>durationsPassed</code>, <code>shortStacksPassed</code>,
   * <code>fullStacksPassed</code>, and <code>statsPassed</code>).
   * </p>
   */
  def apply(suite: Suite, testName: String = null, configMap: ConfigMap = ConfigMap.empty) {
    defaultShell.run(suite, testName, configMap)
  }
}
