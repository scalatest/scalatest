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
import Suite.indentLines
import enablers.Collecting
import scala.language.higherKinds
import enablers.InspectorAsserting
import org.scalactic._
import org.scalatest.exceptions._

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
 *      at org.scalatest.InspectorsHelper$.forAll(Inspectors.scala:146)
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
 *      at org.scalatest.InspectorsHelper$.forAll(Inspectors.scala:146)
 *      ...
 *
 * scala&gt; forEvery (xs) { x =&gt; x should be &lt; 3 }
 * org.scalatest.exceptions.TestFailedException: forEvery failed, because: 
 *   at index 2, 3 was not less than 3 (&lt;console&gt;:18), 
 *   at index 3, 4 was not less than 3 (&lt;console&gt;:18), 
 *   at index 4, 5 was not less than 3 (&lt;console&gt;:18) 
 * in List(1, 2, 3, 4, 5)
 *      at org.scalatest.InspectorsHelper$.forEvery(Inspectors.scala:226)
 *      ...
 * </pre>
 *
 * <p>
 * Note that if you're using matchers, you can alternatively use <em>inspector shorthands</em> for writing non-nested
 * inspections. Here's an example:
 * </p>
 * 
 * <pre>
 * scala&gt; all (xs) should be &gt; 3
 * org.scalatest.exceptions.TestFailedException: 'all' inspection failed, because: 
 *   at index 0, 1 was not greater than 3 
 * in List(1, 2, 3, 4, 5)
 *      at org.scalatest.InspectorsHelper$.forAll(Inspectors.scala:146)
 * </pre>
 *
 * <p>
 * You can use <code>Inspectors</code> on any <code>scala.collection.GenTraversable</code>, <code>java.util.Collection</code>,
 * <code>java.util.Map</code> (with <a href="Entry.html"><code>Entry</code></a>), <code>Array</code>, or <code>String</code>. 
 * Here are some examples:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 * 
 * scala&gt; import Inspectors._
 * import Inspectors._
 * 
 * scala&gt; import Matchers._
 * import Matchers._
 * 
 * scala&gt; forAll (Array(1, 2, 3)) { e =&gt; e should be &lt; 5 }
 * 
 * scala&gt; import collection.JavaConverters._
 * import collection.JavaConverters._
 * 
 * scala&gt; val js = List(1, 2, 3).asJava
 * js: java.util.List[Int] = [1, 2, 3]
 * 
 * scala&gt; forAll (js) { j =&gt; j should be &lt; 5 }
 * 
 * scala&gt; val jmap = Map("a" -&gt; 1, "b" -&gt; 2).asJava 
 * jmap: java.util.Map[String,Int] = {a=1, b=2}
 * 
 * scala&gt; forAtLeast(1, jmap) { e =&gt; e shouldBe Entry("b", 2) }
 * 
 * scala&gt; forAtLeast(2, "hello, world!") { c =&gt; c shouldBe 'o' }
 * </pre>
 */
trait Inspectors {

  
  /**
   * Ensure that all elements in a given collection pass the given inspection function, where "pass" means returning normally from the function (<em>i.e.</em>,
   * without throwing an exception).
   *
   * <p>
   *  The difference between <code>forAll</code> and <code>forEvery</code> is that
   * <code>forAll</code> will stop on the first failure, while <code>forEvery</code> will continue to inspect all elements after the
   * first failure (and report all failures).
   * </p>
   *
   * @param xs the collection of elements
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam E the type of element in the collection
   * @tparam C the type of collection
   *
   */
  def forAll[E, C[_], ASSERTION](xs: C[E])(fun: E => ASSERTION)(implicit collecting: Collecting[E, C[E]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAll(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  /**
    * Ensure that all elements in a given <code>scala.collection.GenMap</code> pass the given inspection function, where "pass" means returning normally from the function (<em>i.e.</em>,
    * without throwing an exception).
    *
    * <p>
    * The difference between <code>forAll</code> and <code>forEvery</code> is that
    * <code>forAll</code> will stop on the first failure, while <code>forEvery</code> will continue to inspect all <code>scala.collection.GenMap</code> entries after the
    * first failure (and report all failures).
    * </p>
    *
    * @param xs the <code>java.util.Map</code>
    * @param fun the inspection function
    * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
    * @tparam K the type of key in the Map
    * @tparam V the type of value in the Map
    * @tparam MAP subtype of <code>java.util.Map</code>
    *
    */
  def forAll[K, V, MAP[k, v] <: scala.collection.GenMap[k, v], ASSERTION](xs: MAP[K, V])(fun: ((K, V)) => ASSERTION)(implicit collecting: Collecting[(K, V), scala.collection.GenTraversable[(K, V)]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAll(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Ensure that all elements in a given <code>java.util.Map</code> pass the given inspection function, where "pass" means returning normally from the function (<em>i.e.</em>,
   * without throwing an exception).
   *
   * <p>
   * The difference between <code>forAll</code> and <code>forEvery</code> is that
   * <code>forAll</code> will stop on the first failure, while <code>forEvery</code> will continue to inspect all <code>java.util.Map</code> elements after the
   * first failure (and report all failures).
   * </p>
   *
   * @param xs the <code>java.util.Map</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam K the type of key in the Java Map
   * @tparam V the type of value in the Java Map
   * @tparam JMAP subtype of <code>java.util.Map</code>
   *
   */
  def forAll[K, V, JMAP[k, v] <: java.util.Map[k, v], ASSERTION](xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => ASSERTION)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAll(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Ensure that all characters in a given <code>String</code> pass the given inspection function, where "pass" means returning normally from the function (<em>i.e.</em>,
   * without throwing an exception).
   *
   * <p>
   * The difference between <code>forAll</code> and <code>forEvery</code> is that
   * <code>forAll</code> will stop on the first failure, while <code>forEvery</code> will continue to inspect all characters in the <code>String</code> after the
   * first failure (and report all failures).
   * </p>
   *
   * @param xs the <code>String</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   *
   */
  def forAll[ASSERTION](xs: String)(fun: Char => ASSERTION)(implicit collecting: Collecting[Char, String], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAll(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  /**
   * Ensure that at least <code>min</code> number of elements of a given collection pass the given inspection function.
   *
   * @param min the minimum number of elements that must pass the inspection function
   * @param xs the collection of elements
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam E the type of element in the collection
   * @tparam C the type of collection
   *
   */
  def forAtLeast[E, C[_], ASSERTION](min: Int, xs: C[E])(fun: E => ASSERTION)(implicit collecting: Collecting[E, C[E]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAtLeast(min, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  /**
    * Ensure that at least <code>min</code> number of elements in a given <code>scala.collection.GenMap</code> pass the given inspection function.
    *
    * @param min the minimum number of elements that must pass the inspection function
    * @param xs the <code>scala.collection.GenMap</code>
    * @param fun the inspection function
    * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
    * @tparam K the type of key in the <code>scala.collection.GenMap</code>
    * @tparam V the type of value in the <code>scala.collection.GenMap</code>
    * @tparam MAP subtype of <code>scala.collection.GenMap</code>
    *
    */
  def forAtLeast[K, V, MAP[k, v] <: scala.collection.GenMap[k, v], ASSERTION](min: Int, xs: MAP[K, V])(fun: ((K, V)) => ASSERTION)(implicit collecting: Collecting[(K, V), scala.collection.GenTraversable[(K, V)]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAtLeast(min, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Ensure that at least <code>min</code> number of elements in a given <code>java.util.Map</code> pass the given inspection function.
   *
   * @param min the minimum number of elements that must pass the inspection function
   * @param xs the <code>java.util.Map</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam K the type of key in the <code>java.util.Map</code>
   * @tparam V the type of value in the <code>java.util.Map</code>
   * @tparam JMAP subtype of <code>java.util.Map</code>
   *
   */
  def forAtLeast[K, V, JMAP[k, v] <: java.util.Map[k, v], ASSERTION](min: Int, xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => ASSERTION)(implicit collecting: Collecting[org.scalatest.Entry[K, V],JMAP[K, V]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAtLeast(min, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Ensure that at least <code>min</code> number of characters in a given <code>String</code> pass the given inspection function.
   *
   * @param min the minimum number of characters in <code>String</code> that must pass the inspection function
   * @param xs the <code>String</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   *
   */
  def forAtLeast[ASSERTION](min: Int, xs: String)(fun: Char => ASSERTION)(implicit collecting: Collecting[Char, String], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAtLeast(min, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  private def shouldIncludeIndex[T, R](xs: GenTraversable[T]) = xs.isInstanceOf[GenSeq[T]]

  /**
   * Ensure that at most <code>max</code> number of elements of a given collection pass the given inspection function.
   *
   * @param max the maximum number of elements that must pass the inspection function
   * @param xs the collection of elements
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam E the type of element in the collection
   * @tparam C the type of collection
   */
  def forAtMost[E, C[_], ASSERTION](max: Int, xs: C[E])(fun: E => ASSERTION)(implicit collecting: Collecting[E, C[E]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAtMost(max, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  /**
    * Ensure that at most <code>max</code> number of elements in a given <code>scala.collection.GenMap</code> pass the given inspection function.
    *
    * @param max the maximum number of elements in the <code>scala.collection.GenMap</code> that must pass the inspection function
    * @param xs the <code>scala.collection.GenMap</code>
    * @param fun the inspection function
    * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
    * @tparam K the type of key in the <code>scala.collection.GenMap</code>
    * @tparam V the type of value in the <code>scala.collection.GenMap</code>
    * @tparam MAP subtype of <code>scala.collection.GenMap</code>
    */
  def forAtMost[K, V, MAP[k, v] <: scala.collection.GenMap[k, v], ASSERTION](max: Int, xs: MAP[K, V])(fun: ((K, V)) => ASSERTION)(implicit collecting: Collecting[(K, V), scala.collection.GenTraversable[(K, V)]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAtMost(max, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Ensure that at most <code>max</code> number of elements in a given <code>java.util.Map</code> pass the given inspection function.
   *
   * @param max the maximum number of elements in the <code>java.util.Map</code> that must pass the inspection function
   * @param xs the <code>java.util.Map</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam K the type of key in the <code>java.util.Map</code>
   * @tparam V the type of value in the <code>java.util.Map</code>
   * @tparam JMAP subtype of <code>java.util.Map</code>
   */
  def forAtMost[K, V, JMAP[k, v] <: java.util.Map[k, v], ASSERTION](max: Int, xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => ASSERTION)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAtMost(max, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Ensure that at most <code>max</code> number of characters in a given <code>String</code> pass the given inspection function.
   *
   * @param max the maximum number of characters in <code>String</code> that must pass the inspection function
   * @param xs the <code>String</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   */
  def forAtMost[ASSERTION](max: Int, xs: String)(fun: Char => ASSERTION)(implicit collecting: Collecting[Char, String], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAtMost(max, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  /**
   * Ensure that exactly <code>succeededCount</code> number of elements of a given collection pass the given inspection function.
   *
   * @param succeededCount the number of elements that must pass the inspection function
   * @param xs the collection of elements
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam E the type of element in the collection
   * @tparam C the type of collection
   */
  def forExactly[E, C[_], ASSERTION](succeededCount: Int, xs: C[E])(fun: E => ASSERTION)(implicit collecting: Collecting[E, C[E]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forExactly(succeededCount, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  /**
    * Ensure that exactly <code>succeededCount</code> number of elements in a given <code>scala.collection.GenMap</code> pass the given inspection function.
    *
    * @param succeededCount the number of entries in the <code>scala.collection.GenMap</code> that must pass the inspection function
    * @param xs the <code>scala.collection.GenMap</code>
    * @param fun the inspection function
    * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
    * @tparam K the type of key in the <code>scala.collection.GenMap</code>
    * @tparam V the type of value in the <code>scala.collection.GenMap</code>
    * @tparam MAP subtype of <code>scala.collection.GenMap</code>
    */
  def forExactly[K, V, MAP[k, v] <: scala.collection.GenMap[k, v], ASSERTION](succeededCount: Int, xs: MAP[K, V])(fun: ((K, V)) => ASSERTION)(implicit collecting: Collecting[(K, V), scala.collection.GenTraversable[(K, V)]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forExactly(succeededCount, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Ensure that exactly <code>succeededCount</code> number of elements in a given <code>java.util.Map</code> pass the given inspection function.
   *
   * @param succeededCount the number of elements in the <code>java.util.Map</code> that must pass the inspection function
   * @param xs the <code>java.util.Map</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam K the type of key in the <code>java.util.Map</code>
   * @tparam V the type of value in the <code>java.util.Map</code>
   * @tparam JMAP subtype of <code>java.util.Map</code>
   */
  def forExactly[K, V, JMAP[k, v] <: java.util.Map[k, v], ASSERTION](succeededCount: Int, xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => ASSERTION)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forExactly(succeededCount, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Ensure that exactly <code>succeededCount</code> number of characters in a given <code>String</code> pass the given inspection function.
   *
   * @param succeededCount the number of characters in the <code>String</code> that must pass the inspection function
   * @param xs the <code>String</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   */
  def forExactly[ASSERTION](succeededCount: Int, xs: String)(fun: Char => ASSERTION)(implicit collecting: Collecting[Char, String], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forExactly(succeededCount, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }
  
  private[scalatest] def forNo[E, C[_], ASSERTION](xs: C[E])(fun: E => ASSERTION)(implicit collecting: Collecting[E, C[E]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forNo(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  private[scalatest] def forNo[K, V, MAP[k, v] <: scala.collection.GenMap[k, v], ASSERTION](xs: MAP[K, V])(fun: ((K, V)) => ASSERTION)(implicit collecting: Collecting[(K, V), scala.collection.GenTraversable[(K, V)]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forNo(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  // SKIP-SCALATESTJS,NATIVE-START
  private[scalatest] def forNo[K, V, JMAP[k, v] <: java.util.Map[k, v], ASSERTION](xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => ASSERTION)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forNo(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }
  // SKIP-SCALATESTJS,NATIVE-END

  private[scalatest] def forNo[ASSERTION](xs: String)(fun: Char => ASSERTION)(implicit collecting: Collecting[Char, String], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forNo(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  /**
   * Ensure the number of elements of a given collection that pass the given inspection function is between <code>from</code> and <code>upTo</code>.
   *
   * @param from the minimum number of elements that must pass the inspection number
   * @param upTo the maximum number of elements that must pass the inspection number
   * @param xs the collection of elements
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam E the type of element in the collection
   * @tparam C the type of collection
   */
  def forBetween[E, C[_], ASSERTION](from: Int, upTo: Int, xs: C[E])(fun: E => ASSERTION)(implicit collecting: Collecting[E, C[E]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forBetween(from, upTo, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  /**
    * Ensure the number of elements in a given <code>scala.collection.GenMap</code> that pass the given inspection function is between <code>from</code> and <code>upTo</code>.
    *
    * @param from the minimum number of elements in the <code>scala.collection.GenMap</code> that must pass the inspection number
    * @param upTo the maximum number of elements in the <code>scala.collection.GenMap</code> that must pass the inspection number
    * @param xs the <code>scala.collection.GenMap</code>
    * @param fun the inspection function
    * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
    * @tparam K the type of key in the <code>scala.collection.GenMap</code>
    * @tparam V the type of value in the <code>scala.collection.GenMap</code>
    * @tparam MAP subtype of <code>scala.collection.GenMap</code>
    */
  def forBetween[K, V, MAP[k, v] <: scala.collection.GenMap[k, v], ASSERTION](from: Int, upTo: Int, xs: MAP[K, V])(fun: ((K, V)) => ASSERTION)(implicit collecting: Collecting[(K, V), scala.collection.GenTraversable[(K, V)]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forBetween(from, upTo, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Ensure the number of elements in a given <code>java.util.Map</code> that pass the given inspection function is between <code>from</code> and <code>upTo</code>.
   *
   * @param from the minimum number of elements in the <code>java.util.Map</code> that must pass the inspection number
   * @param upTo the maximum number of elements in the <code>java.util.Map</code> that must pass the inspection number
   * @param xs the <code>java.util.Map</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam K the type of key in the <code>java.util.Map</code>
   * @tparam V the type of value in the <code>java.util.Map</code>
   * @tparam JMAP subtype of <code>java.util.Map</code>
   */
  def forBetween[K, V, JMAP[k, v] <: java.util.Map[k, v], ASSERTION](from: Int, upTo: Int, xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => ASSERTION)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forBetween(from, upTo, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Ensure the number of characters of a given <code>String</code> that pass the given inspection function is between <code>from</code> and <code>upTo</code>.
   *
   * @param from the minimum number of characters in the <code>String</code> that must pass the inspection number
   * @param upTo the maximum number of characters in the <code>String</code> that must pass the inspection number
   * @param xs the <code>String</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   */
  def forBetween[ASSERTION](from: Int, upTo: Int, xs: String)(fun: Char => ASSERTION)(implicit collecting: Collecting[Char, String], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forBetween(from, upTo, collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  /**
   * Ensure that every element in a given collection passes the given inspection function, where "pass" means returning normally from the function (<em>i.e.</em>,
   * without throwing an exception).
   *
   * <p>
   * The difference between <code>forEvery</code> and <code>forAll</code> is that
   * <code>forEvery</code> will continue to inspect all elements after first failure, and report all failures,
   * whereas <code>forAll</code> will stop on (and only report) the first failure.
   * </p>
   *
   * @param xs the collection of elements
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam E the type of element in the collection
   * @tparam C the type of collection
   */
  def forEvery[E, C[_], ASSERTION](xs: C[E])(fun: E => ASSERTION)(implicit collecting: Collecting[E, C[E]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forEvery(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  /**
    * Ensure that every element in a given <code>scala.collection.GenMap</code> passes the given inspection function, where "pass" means returning normally
    * from the function (<em>i.e.</em>, without throwing an exception).
    *
    * <p>
    * The difference between <code>forEvery</code> and <code>forAll</code> is that
    * <code>forEvery</code> will continue to inspect all entries in the <code>scala.collection.GenMap</code> after first failure, and report all failures,
    * whereas <code>forAll</code> will stop on (and only report) the first failure.
    * </p>
    *
    * @param xs the <code>scala.collection.GenMap</code>
    * @param fun the inspection function
    * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
    * @tparam K the type of key in the <code>scala.collection.GenMap</code>
    * @tparam V the type of value in the <code>scala.collection.GenMap</code>
    * @tparam MAP subtype of <code>scala.collection.GenMap</code>
    */
  def forEvery[K, V, MAP[k, v] <: scala.collection.GenMap[k, v], ASSERTION](xs: MAP[K, V])(fun: ((K, V)) => ASSERTION)(implicit collecting: Collecting[(K, V), scala.collection.GenTraversable[(K, V)]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forEvery(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Ensure that every element in a given <code>java.util.Map</code> passes the given inspection function, where "pass" means returning normally
   * from the function (<em>i.e.</em>, without throwing an exception).
   *
   * <p>
   * The difference between <code>forEvery</code> and <code>forAll</code> is that
   * <code>forEvery</code> will continue to inspect all elements in the <code>java.util.Map</code> after first failure, and report all failures,
   * whereas <code>forAll</code> will stop on (and only report) the first failure.
   * </p>
   *
   * @param xs the <code>java.util.Map</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   * @tparam K the type of key in the <code>java.util.Map</code>
   * @tparam V the type of value in the <code>java.util.Map</code>
   * @tparam JMAP subtype of <code>java.util.Map</code>
   */
  def forEvery[K, V, JMAP[k, v] <: java.util.Map[k, v], ASSERTION](xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => ASSERTION)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forEvery(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Ensure that every character in a given <code>String</code> passes the given inspection function, where "pass" means returning normally from the function (<em>i.e.</em>,
   * without throwing an exception).
   *
   * <p>
   * The difference between <code>forEvery</code> and <code>forAll</code> is that
   * <code>forEvery</code> will continue to inspect all characters in the <code>String</code> after first failure, and report all failures,
   * whereas <code>forAll</code> will stop on (and only report) the first failure.
   * </p>
   *
   * @param xs the <code>String</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   */
  def forEvery[ASSERTION](xs: String)(fun: Char => ASSERTION)(implicit collecting: Collecting[Char, String], asserting: InspectorAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forEvery(collecting.iterableFrom(xs), xs, false, prettifier, pos)(fun)
  }
}

/**
 * Companion object that facilitates the importing of <code>Inspectors</code> members as
 * an alternative to mixing it in. One use case is to import <code>Inspectors</code>'s members so you can use
 * them in the Scala interpreter.
 */
object Inspectors extends Inspectors

private[scalatest] object InspectorsHelper {

  def indentErrorMessages(messages: IndexedSeq[String]) = indentLines(1, messages)

  def shouldPropagate(throwable: Throwable): Boolean = 
    throwable match {
      case _: NotAllowedException |
           _: TestPendingException |
           _: TestCanceledException => true
      case _ if Suite.anExceptionThatShouldCauseAnAbort(throwable) => true
      case _ => false
    }

}
