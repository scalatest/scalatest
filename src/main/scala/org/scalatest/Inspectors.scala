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

import scala.collection.GenTraversable
import scala.collection.GenSeq

/**
 * Provides nestable <em>inspector methods</em> (or just <em>inspectors</em>) that enable assertions to be made about collections.
 *
 * <p>
 * For example, the <code>forAll</code> method enables you to state that something should be true about all elements of a collection, such
 * as that all elements should be positive:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import Assertions._
 * import Assertions._
 *
 * scala&gt; import Inspectors._
 * import Inspectors._
 *
 * scala&gt; val xs = List(1, 2, 3, 4, 5)
 * xs: List[Int] = List(1, 2, 3, 4, 5)
 *
 * scala&gt; forAll (xs) { x =&gt; assert(x &gt; 0) }
 * </pre>
 *
 * <p>
 * Or, with matchers:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import Matchers._
 * import Matchers._
 *
 * scala&gt; forAll (xs) { x =&gt; x should be &gt; 0 }
 * </pre>
 *
 * <p>
 * To make assertions about nested collections, you can nest the inspector method invocations.
 * For example, given the following list of lists of <code>Int</code>:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val yss =
 *      |   List(
 *      |     List(1, 2, 3),
 *      |     List(1, 2, 3),
 *      |     List(1, 2, 3)
 *      |   )
 * yss: List[List[Int]] = List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))
 * </pre>
 *
 * <p>
 * You can assert that all <code>Int</code> elements in all nested lists are positive by nesting two <code>forAll</code> method invocations, like this:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; forAll (yss) { ys =&gt;
 *      |   forAll (ys) { y =&gt; y should be &gt; 0 }
 *      | }
 * </pre>
 *
 * <p>
 * The full list of inspector methods are:
 * </p>
 *
 * <ul>
 * <li><code>forAll</code> - succeeds if the assertion holds true for every element</li>
 * <li><code>forAtLeast</code> - succeeds if the assertion holds true for at least the specified number of elements</li>
 * <li><code>forAtMost</code> - succeeds if the assertion holds true for at most the specified number of elements</li>
 * <li><code>forBetween</code> - succeeds if the assertion holds true for between the specified minimum and maximum number of elements, inclusive</li>
 * <li><code>forEvery</code> - same as <code>forAll</code>, but lists all failing elements if it fails (whereas <code>forAll</code> just reports the first failing element)</li>
 * <li><code>forExactly</code> - succeeds if the assertion holds true for exactly the specified number of elements</li>
 * </ul>
 *
 * <p>
 * The error messages produced by inspector methods are designed to make sense no matter how deeply you nest the method invocations. 
 * Here's an example of a nested inspection that fails and the resulting error message:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; forAll (yss) { ys =&gt;
 *      |   forAll (ys) { y =&gt; y should be &lt; 2 }
 *      | }
 * org.scalatest.exceptions.TestFailedException: forAll failed, because: 
 *   at index 0, forAll failed, because: 
 *     at index 1, 2 was not less than 2 (&lt;console&gt;:20) 
 *   in List(1, 2, 3) (&lt;console&gt;:20) 
 * in List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))
 *      at org.scalatest.InspectorsHelper$.doForAll(Inspectors.scala:146)
 *      ...
 * </pre>
 *
 * <p>
 * One way the error message is designed to help you understand the error is by using indentation that mimics the indentation of the
 * source code (optimistically assuming the source will be nicely indented). The error message above indicates the outer <code>forAll</code> failed
 * because its initial <code>List</code> (<em>i.e.</em>, at index 0) failed
 * the assertion, which was that all elements of that initial <code>List[Int]</code> at index 0 should be less than 2. This assertion failed because index 1 of
 * that inner list contained the value 2, which was indeed &ldquo;not less than 2.&rdquo; The error message for the inner list is an indented line inside the error message
 * for the outer list. The actual contents of each list are displayed at the end in inspector error messages, also indented appropriately. The actual contents
 * are placed at the end so that for very large collections, the contents will not drown out and make it difficult to find the messages that describe
 * actual causes of the failure.
 * </p>
 *
 * <p>
 * The <code>forAll</code> and <code>forEvery</code> methods are similar in that both succeed only if the assertion holds for all elements of the collection.
 * They differ in that <code>forAll</code> will only report the first element encountered that failed the assertion, but <code>forEvery</code> will report <em>all</em>
 * elements that fail the assertion. The tradeoff is that while <code>forEvery</code> gives more information, it may take longer to run because it must inspect every element
 * of the collection. The <code>forAll</code> method can simply stop inspecting once it encounters the first failing element. Here's an example that
 * shows the difference in the <code>forAll</code> and <code>forEvery</code> error messages:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; forAll (xs) { x =&gt; x should be &lt; 3 }
 * org.scalatest.exceptions.TestFailedException: forAll failed, because: 
 *   at index 2, 3 was not less than 3 (&lt;console&gt;:18) 
 * in List(1, 2, 3, 4, 5)
 *      at org.scalatest.InspectorsHelper$.doForAll(Inspectors.scala:146)
 *      ...
 *
 * scala&gt; forEvery (xs) { x =&gt; x should be &lt; 3 }
 * org.scalatest.exceptions.TestFailedException: forEvery failed, because: 
 *   at index 2, 3 was not less than 3 (&lt;console&gt;:18), 
 *   at index 3, 4 was not less than 3 (&lt;console&gt;:18), 
 *   at index 4, 5 was not less than 3 (&lt;console&gt;:18) 
 * in List(1, 2, 3, 4, 5)
 *      at org.scalatest.InspectorsHelper$.doForEvery(Inspectors.scala:226)
 *      ...
 * </pre>
 *
 * <p>
 * Note that if you're using matchers, you can alternatively use <em>inspector shorthands</em> for writing non-nested
 * inspections. Here's an example:
 * </p>
 * 
 * <pre>
 * 
 * scala&gt; all (xs) should be &gt; 3
 * org.scalatest.exceptions.TestFailedException: 'all' inspection failed, because: 
 *   at index 0, 1 was not greater than 3 
 * in List(1, 2, 3, 4, 5)
 *      at org.scalatest.InspectorsHelper$.doForAll(Inspectors.scala:146)
 * </pre>
 */
trait Inspectors {

  import InspectorsHelper._

  def forAll[T](xs: GenTraversable[T])(fun: T => Unit) {
    doForAll(xs, "forAllFailed", "Inspectors.scala", "forAll", 2)(fun)
  }

  def forAtLeast[T](min: Int, xs: GenTraversable[T])(fun: T => Unit) {
    doForAtLeast(min, xs, "forAtLeastFailed", "Inspectors.scala", "forAtLeast", 2)(fun)
  }

  private def shouldIncludeIndex[T, R](xs: GenTraversable[T]) = xs.isInstanceOf[GenSeq[T]]

  private def createElementsMessage[T](elements: IndexedSeq[(Int, T)], includeIndex: Boolean): String = elements.map { case (index, element) => 
    if (includeIndex) 
      Resources("forAssertionsMessageWithIndex", index.toString, element.toString) 
    else 
      Resources("forAssertionsMessageWithoutIndex", element.toString) 
  }.mkString(", ")

  def forAtMost[T](max: Int, xs: GenTraversable[T])(fun: T => Unit) {
    doForAtMost(max, xs, "forAtMostFailed", "Inspectors.scala", "forAtMost", 2)(fun)
  }

  def forExactly[T](succeededCount: Int, xs: GenTraversable[T])(fun: T => Unit) {
    doForExactly(succeededCount, xs, "forExactlyFailed", "Inspectors.scala", "forExactly", 2)(fun)
  }
  
  private[scalatest] def forNo[T](xs: GenTraversable[T])(fun: T => Unit) {
    doForNo(xs, "forNoFailed", "Inspectors.scala", "forNo", 2)(fun)
  }
  
  def forBetween[T](from: Int, upTo: Int, xs: GenTraversable[T])(fun: T => Unit) {
    doForBetween(from, upTo, xs, "forBetweenFailed", "Inspectors.scala", "forBetween", 2)(fun)
  }
  
  def forEvery[T](xs: GenTraversable[T])(fun: T => Unit) {
    doForEvery(xs, "forEveryFailed", "Inspectors.scala", "forEvery", 2)(fun)
  }
}

object Inspectors extends Inspectors