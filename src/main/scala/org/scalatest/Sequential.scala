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
 * A <code>Suite</code> class mixing in <a href="SequentialNestedSuiteExecution.html"><code>SequentialNestedSuiteExecution</code></a> that takes
 * zero to many <code>Suite</code>s, which will be returned from its <code>nestedSuites</code> method.
 *
 * <p>
 * For example, you can define a suite that always executes a list of
 * nested suites like this:
 * </p>
 *
 * <pre class="stHighlight">
 * class StepsSuite extends Sequential(
 *   new Step1Suite,
 *   new Step2Suite,
 *   new Step3Suite,
 *   new Step4Suite,
 *   new Step5Suite
 * )
 * </pre>
 *
 * <p>
 * When <code>StepsSuite</code> is executed, it will execute its
 * nested suites in the passed order: <code>Step1Suite</code>, <code>Step2Suite</code>,
 * <code>Step3Suite</code>, <code>Step4Suite</code>, and <code>Step5Suite</code>.
 * Because <code>Sequential</code> extends <code>SequentialNestedSuiteExecution</code>,
 * the distributor passed to <code>runNestedSuites</code> will always be <code>None</code>.
 * So not only will the suites passed to the constructor be executed sequentially, any
 * tests and nested suites of the passed suites will also be executed sequentually.
 * </p>
 *
 * <p>
 * The difference between <code>Sequential</code> and <a href="Stepwise.html"><code>Stepwise</code></a>
 * is that although <code>Stepwise</code> executes its own nested suites sequentially, it passes
 * whatever distributor was passed to it to those nested suites. Thus the nested suites could run their own nested
 * suites and tests in parallel if that distributor is defined. By contrast, <code>Sequential</code> always
 * passes <code>None</code> for the distributor to the nested suites, so any and every test and nested suite 
 * contained within the nested suites passed to the <code>Sequential</code> construtor will be executed sequentially.
 * </p>
 * 
 * @param suitesToNest a sequence of <code>Suite</code>s to nest.
 *
 * @throws NullPointerException if <code>suitesToNest</code>, or any suite
 * it contains, is <code>null</code>.
 *
 * @author Bill Venners
 */
class Sequential(suitesToNest: Suite*) extends Suite with SequentialNestedSuiteExecution { thisSuite => 

  for (s <- suitesToNest) {
    if (s == null)
      throw new NullPointerException("A passed suite was null")
  }

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the suites passed to the constructor in
   * the order they were passed.
   */
  override val nestedSuites: collection.immutable.IndexedSeq[Suite] = Vector.empty ++ suitesToNest

  /**
   * Returns a user friendly string for this suite, composed of the
   * simple name of the class (possibly simplified further by removing dollar signs if added by the Scala interpeter) and, if this suite
   * contains nested suites, the result of invoking <code>toString</code> on each
   * of the nested suites, separated by commas and surrounded by parentheses.
   *
   * @return a user-friendly string for this suite
   */
  override def toString: String = Suite.suiteToString(None, thisSuite)
}

/**
 * Companion object to class <code>Sequential</code> that offers an <code>apply</code> factory method
 * for creating a <code>Sequential</code> instance.
 *
 * <p>
 * One use case for this object is to run multiple specification-style suites in the Scala interpreter, like this:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; Sequential(new MyFirstSuite, new MyNextSuite).execute()
 * </pre>
 */
object Sequential {

  /**
   * Factory method for creating a <code>Sequential</code> instance.
   */
  def apply(suitesToNest: Suite*): Sequential = new Sequential(suitesToNest: _*)
}

