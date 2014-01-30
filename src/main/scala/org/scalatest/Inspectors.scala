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
import scala.annotation.tailrec
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import scala.collection.GenSeq
import Suite.indentLines
import FailureMessages.decorateToStringValue
import enablers.Collecting
import scala.language.higherKinds

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
 * scala&gt; all (xs) should be &gt; 3
 * org.scalatest.exceptions.TestFailedException: 'all' inspection failed, because: 
 *   at index 0, 1 was not greater than 3 
 * in List(1, 2, 3, 4, 5)
 *      at org.scalatest.InspectorsHelper$.doForAll(Inspectors.scala:146)
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

  import InspectorsHelper._

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
  def forAll[E, C[_]](xs: C[E])(fun: E => Unit)(implicit collecting: Collecting[E, C[E]]) {
    doForAll(collecting.genTraversableFrom(xs), xs, "forAllFailed", "Inspectors.scala", "forAll", 0)(fun)
  }

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
  def forAll[K, V, JMAP[k, v] <: java.util.Map[k, v]](xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => Unit)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]) {
    doForAll(collecting.genTraversableFrom(xs), xs, "forAllFailed", "Inspectors.scala", "forAll", 0)(fun)
  }

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
  def forAll(xs: String)(fun: Char => Unit)(implicit collecting: Collecting[Char, String]) {
    doForAll(collecting.genTraversableFrom(xs), xs, "forAllFailed", "Inspectors.scala", "forAll", 0)(fun)
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
  def forAtLeast[E, C[_]](min: Int, xs: C[E])(fun: E => Unit)(implicit collecting: Collecting[E, C[E]]) {
    doForAtLeast(min, collecting.genTraversableFrom(xs), xs, "forAtLeastFailed", "Inspectors.scala", "forAtLeast", 0)(fun)
  }

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
  def forAtLeast[K, V, JMAP[k, v] <: java.util.Map[k, v]](min: Int, xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => Unit)(implicit collecting: Collecting[org.scalatest.Entry[K, V],JMAP[K, V]]) {
    doForAtLeast(min, collecting.genTraversableFrom(xs), xs, "forAtLeastFailed", "Inspectors.scala", "forAtLeast", 0)(fun)
  }

  /**
   * Ensure that at least <code>min</code> number of characters in a given <code>String</code> pass the given inspection function.
   *
   * @param min the minimum number of characters in <code>String</code> that must pass the inspection function
   * @param xs the <code>String</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   *
   */
  def forAtLeast(min: Int, xs: String)(fun: Char => Unit)(implicit collecting: Collecting[Char, String]) {
    doForAtLeast(min, collecting.genTraversableFrom(xs), xs, "forAtLeastFailed", "Inspectors.scala", "forAtLeast", 0)(fun)
  }

  private def shouldIncludeIndex[T, R](xs: GenTraversable[T]) = xs.isInstanceOf[GenSeq[T]]

  private def createElementsMessage[T](elements: IndexedSeq[(Int, T)], includeIndex: Boolean): String = elements.map { case (index, element) => 
    if (includeIndex) 
      Resources("forAssertionsMessageWithIndex", index.toString, element.toString) 
    else 
      Resources("forAssertionsMessageWithoutIndex", element.toString) 
  }.mkString(", ")

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
  def forAtMost[E, C[_]](max: Int, xs: C[E])(fun: E => Unit)(implicit collecting: Collecting[E, C[E]]) {
    doForAtMost(max, collecting.genTraversableFrom(xs), xs, "forAtMostFailed", "Inspectors.scala", "forAtMost", 0)(fun)
  }

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
  def forAtMost[K, V, JMAP[k, v] <: java.util.Map[k, v]](max: Int, xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => Unit)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]) {
    doForAtMost(max, collecting.genTraversableFrom(xs), xs, "forAtMostFailed", "Inspectors.scala", "forAtMost", 0)(fun)
  }

  /**
   * Ensure that at most <code>max</code> number of characters in a given <code>String</code> pass the given inspection function.
   *
   * @param max the maximum number of characters in <code>String</code> that must pass the inspection function
   * @param xs the <code>String</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   */
  def forAtMost(max: Int, xs: String)(fun: Char => Unit)(implicit collecting: Collecting[Char, String]) {
    doForAtMost(max, collecting.genTraversableFrom(xs), xs, "forAtMostFailed", "Inspectors.scala", "forAtMost", 0)(fun)
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
  def forExactly[E, C[_]](succeededCount: Int, xs: C[E])(fun: E => Unit)(implicit collecting: Collecting[E, C[E]]) {
    doForExactly(succeededCount, collecting.genTraversableFrom(xs), xs, "forExactlyFailed", "Inspectors.scala", "forExactly", 0)(fun)
  }

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
  def forExactly[K, V, JMAP[k, v] <: java.util.Map[k, v]](succeededCount: Int, xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => Unit)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]) {
    doForExactly(succeededCount, collecting.genTraversableFrom(xs), xs, "forExactlyFailed", "Inspectors.scala", "forExactly", 0)(fun)
  }

  /**
   * Ensure that exactly <code>succeededCount</code> number of characters in a given <code>String</code> pass the given inspection function.
   *
   * @param succeededCount the number of characters in the <code>String</code> that must pass the inspection function
   * @param xs the <code>String</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   */
  def forExactly(succeededCount: Int, xs: String)(fun: Char => Unit)(implicit collecting: Collecting[Char, String]) {
    doForExactly(succeededCount, collecting.genTraversableFrom(xs), xs, "forExactlyFailed", "Inspectors.scala", "forExactly", 0)(fun)
  }
  
  private[scalatest] def forNo[E, C[_]](xs: C[E])(fun: E => Unit)(implicit collecting: Collecting[E, C[E]]) {
    doForNo(collecting.genTraversableFrom(xs), xs, "forNoFailed", "Inspectors.scala", "forNo", 0)(fun)
  }

  private[scalatest] def forNo[K, V, JMAP[k, v] <: java.util.Map[k, v]](xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => Unit)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]) {
    doForNo(collecting.genTraversableFrom(xs), xs, "forNoFailed", "Inspectors.scala", "forNo", 0)(fun)
  }

  private[scalatest] def forNo(xs: String)(fun: Char => Unit)(implicit collecting: Collecting[Char, String]) {
    doForNo(collecting.genTraversableFrom(xs), xs, "forNoFailed", "Inspectors.scala", "forNo", 0)(fun)
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
  def forBetween[E, C[_]](from: Int, upTo: Int, xs: C[E])(fun: E => Unit)(implicit collecting: Collecting[E, C[E]]) {
    doForBetween(from, upTo, collecting.genTraversableFrom(xs), xs, "forBetweenFailed", "Inspectors.scala", "forBetween", 0)(fun)
  }

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
  def forBetween[K, V, JMAP[k, v] <: java.util.Map[k, v]](from: Int, upTo: Int, xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => Unit)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]) {
    doForBetween(from, upTo, collecting.genTraversableFrom(xs), xs, "forBetweenFailed", "Inspectors.scala", "forBetween", 0)(fun)
  }

  /**
   * Ensure the number of characters of a given <code>String</code> that pass the given inspection function is between <code>from</code> and <code>upTo</code>.
   *
   * @param from the minimum number of characters in the <code>String</code> that must pass the inspection number
   * @param upTo the maximum number of characters in the <code>String</code> that must pass the inspection number
   * @param xs the <code>String</code>
   * @param fun the inspection function
   * @param collecting the implicit <code>Collecting</code> that can transform <code>xs</code> into a <code>scala.collection.GenTraversable</code>
   */
  def forBetween(from: Int, upTo: Int, xs: String)(fun: Char => Unit)(implicit collecting: Collecting[Char, String]) {
    doForBetween(from, upTo, collecting.genTraversableFrom(xs), xs, "forBetweenFailed", "Inspectors.scala", "forBetween", 0)(fun)
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
  def forEvery[E, C[_]](xs: C[E])(fun: E => Unit)(implicit collecting: Collecting[E, C[E]]) {
    doForEvery(collecting.genTraversableFrom(xs), xs, "forEveryFailed", "Inspectors.scala", "forEvery", 0)(fun)
  }

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
  def forEvery[K, V, JMAP[k, v] <: java.util.Map[k, v]](xs: JMAP[K, V])(fun: org.scalatest.Entry[K, V] => Unit)(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]) {
    doForEvery(collecting.genTraversableFrom(xs), xs, "forEveryFailed", "Inspectors.scala", "forEvery", 0)(fun)
  }

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
  def forEvery(xs: String)(fun: Char => Unit)(implicit collecting: Collecting[Char, String]) {
    doForEvery(collecting.genTraversableFrom(xs), xs, "forEveryFailed", "Inspectors.scala", "forEvery", 0)(fun)
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
  
  def getResourceNamePrefix(xs: Any): String =
    xs match {
      case _: collection.GenMap[_, _] => "forAssertionsGenMapMessage"
      case _: java.util.Map[_, _] => "forAssertionsGenMapMessage"
      case _ => "forAssertionsGenTraversableMessage"
    }
  
  def shouldPropagate(throwable: Throwable): Boolean = 
    throwable match {
      case _: exceptions.TestPendingException |
           _: exceptions.TestCanceledException => true
      case _ if Suite.anExceptionThatShouldCauseAnAbort(throwable) => true
      case _ => false
    }
  
  def createMessage(messageKey: String, t: Throwable, resourceNamePrefix: String): String = 
    t match {
      case sde: exceptions.StackDepthException => 
        sde.failedCodeFileNameAndLineNumberString match {
          case Some(failedCodeFileNameAndLineNumber) => 
            Resources(resourceNamePrefix + "WithStackDepth", messageKey, sde.getMessage, failedCodeFileNameAndLineNumber)
          case None => 
            Resources(resourceNamePrefix + "WithoutStackDepth", messageKey, sde.getMessage)
        }
      case _ =>
        Resources(resourceNamePrefix + "WithoutStackDepth", messageKey, if (t.getMessage != null) t.getMessage else "null")
    }
  
  def elementLabel(count: Int): String = 
    if (count > 1) Resources("forAssertionsElements", count.toString) else Resources("forAssertionsElement", count.toString)
  
  case class ForResult[T](passedCount: Int = 0, messageAcc: IndexedSeq[String] = IndexedSeq.empty, 
                                 passedElements: IndexedSeq[(Int, T)] = IndexedSeq.empty, failedElements: IndexedSeq[(Int, T, Throwable)] = IndexedSeq.empty)
  
  @tailrec
  def runFor[T](itr: Iterator[T], resourceNamePrefix: String, index:Int, result: ForResult[T], fun: T => Unit, stopFun: ForResult[_] => Boolean): ForResult[T] = {
    if (itr.hasNext) {
      val head = itr.next
      val newResult = 
        try {
          fun(head)
          result.copy(passedCount = result.passedCount + 1, passedElements = result.passedElements :+ (index, head))
        }
        catch {
          case e if !shouldPropagate(e) => 
            val messageKey = head match {
              case tuple: Tuple2[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => tuple._1.toString
              case entry: Entry[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => entry.getKey.toString
              case _ => index.toString
            }
            result.copy(messageAcc = result.messageAcc :+ createMessage(messageKey, e, resourceNamePrefix), failedElements = result.failedElements :+ (index, head, e))
        }
      if (stopFun(newResult))
        newResult
      else
        runFor(itr, resourceNamePrefix, index + 1, newResult, fun, stopFun)
    }
    else
      result
  }
  
  def keyOrIndexLabel(xs: Any, passedElements: IndexedSeq[(Int, _)]): String = {
    def makeAndLabel(indexes: IndexedSeq[Int]): String = 
      if (indexes.length > 1)
        indexes.dropRight(1).mkString(", ") + " and " + indexes.last
      else
        indexes.mkString(", ")
      
    val (prefixResourceName, elements) = xs match {
      case _: collection.GenMap[_, _] | _: java.util.Map[_, _] =>
        val elements = passedElements.map{ case (index, e) =>
          e match {
            case tuple2: Tuple2[_, _] => tuple2._1
            case entry: java.util.Map.Entry[_, _] => entry.getKey
            case _ => index
          }
        }
        ("forAssertionsKey", elements)
      case _ => 
        ("forAssertionsIndex", passedElements.map(_._1))
    }
    if (elements.length > 1)
      Resources(prefixResourceName + "AndLabel", elements.dropRight(1).mkString(", "), elements.last.toString) 
    else
      Resources(prefixResourceName + "Label", elements.mkString(", "))
  }
  
  def doForAll[E](xs: GenTraversable[E], original: Any, resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => Unit) {
    val resourceNamePrefix = getResourceNamePrefix(original)
    val result = 
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[E], fun, _.failedElements.length > 0)
    if (result.failedElements.length > 0) 
      throw new exceptions.TestFailedException(
        sde => Some(Resources(resourceName, indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))),
        Some(result.failedElements(0)._3),
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }
  
  def doForAtLeast[T](min: Int, xs: GenTraversable[T], original: Any, resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    @tailrec
    def forAtLeastAcc(itr: Iterator[T], includeIndex: Boolean, index: Int, passedCount: Int, messageAcc: IndexedSeq[String]): (Int, IndexedSeq[String]) = {
      if (itr.hasNext) {
        val head = itr.next
        val (newPassedCount, newMessageAcc) = 
          try {
            fun(head)
            (passedCount + 1, messageAcc)
          }
          catch {
            case e if !shouldPropagate(e) => 
              val resourceNamePrefix = getResourceNamePrefix(original)
              val messageKey = head match {
                case tuple: Tuple2[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => tuple._1.toString
                case entry: Entry[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => entry.getKey.toString
                case _ => index.toString
              }
              (passedCount, messageAcc :+ createMessage(messageKey, e, resourceNamePrefix))
          }
        if (newPassedCount < min)
          forAtLeastAcc(itr, includeIndex, index + 1, newPassedCount, newMessageAcc)
        else
          (newPassedCount, newMessageAcc)
      }
      else
        (passedCount, messageAcc)
    }
    
    if (min <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'min'"))
    
    val (passedCount, messageAcc) = forAtLeastAcc(xs.toIterator, xs.isInstanceOf[Seq[T]], 0, 0, IndexedSeq.empty)
    if (passedCount < min)
      throw new exceptions.TestFailedException(
        sde => 
          Some(
            if (passedCount > 0)
              Resources(resourceName, min.toString, elementLabel(passedCount), indentErrorMessages(messageAcc).mkString(", \n"), decorateToStringValue(original))
            else
              Resources(resourceName + "NoElement", min.toString, indentErrorMessages(messageAcc).mkString(", \n"), decorateToStringValue(original))
          ),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }
  
  def doForEvery[T](xs: GenTraversable[T], original: Any, resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    @tailrec
    def runAndCollectErrorMessage[T](itr: Iterator[T], messageList: IndexedSeq[String], index: Int)(fun: T => Unit): IndexedSeq[String] = {
      if (itr.hasNext) {
        val head = itr.next
        val newMessageList = 
          try {
            fun(head)
            messageList
          }
          catch {
            case e if !shouldPropagate(e) => 
              val resourceNamePrefix = getResourceNamePrefix(original)
              val messageKey = head match {
                case tuple: Tuple2[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => tuple._1.toString
                case entry: Entry[_, _] if resourceNamePrefix == "forAssertionsGenMapMessage" => entry.getKey.toString
                case _ => index.toString
              }
              messageList :+ createMessage(messageKey, e, resourceNamePrefix)
          }
        
        runAndCollectErrorMessage(itr, newMessageList, index + 1)(fun)
      }
      else
        messageList
    }
    val messageList = runAndCollectErrorMessage(xs.toIterator, IndexedSeq.empty, 0)(fun)
    if (messageList.size > 0)
      throw new exceptions.TestFailedException(
          sde => Some(Resources(resourceName, indentErrorMessages(messageList).mkString(", \n"), decorateToStringValue(original))),
          None,
          getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
        )
  }
  
  def doForExactly[T](succeededCount: Int, xs: GenTraversable[T], original: Any, resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    if (succeededCount <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'succeededCount'"))
    
    val resourceNamePrefix = getResourceNamePrefix(original)
    val result = 
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount > succeededCount)
    if (result.passedCount != succeededCount)
      throw new exceptions.TestFailedException(
        sde => 
          Some(
            if (result.passedCount == 0)
              Resources(resourceName + "NoElement", succeededCount.toString, indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
            else {
              if (result.passedCount < succeededCount)
                Resources(resourceName + "Less", succeededCount.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
              else
                Resources(resourceName + "More", succeededCount.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original))
            }
          ),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }

  def doForNo[T](xs: GenTraversable[T], original: Any, resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    val resourceNamePrefix = getResourceNamePrefix(original)
    val result =
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount != 0)
    if (result.passedCount != 0)
      throw new exceptions.TestFailedException(
        sde => Some(Resources(resourceName, keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original))),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }

  def doForBetween[T](from: Int, upTo: Int, xs: GenTraversable[T], original: Any, resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    if (from < 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanEqualZero", "'from'"))
    if (upTo <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'upTo'"))
    if (upTo <= from)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThan", "'upTo'", "'from'"))

    val resourceNamePrefix = getResourceNamePrefix(original)
    val result =
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount > upTo)
    if (result.passedCount < from || result.passedCount > upTo)
      throw new exceptions.TestFailedException(
        sde =>
          Some(
            if (result.passedCount == 0)
              Resources(resourceName + "NoElement", from.toString, upTo.toString, indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
            else {
              if (result.passedCount < from)
                Resources(resourceName + "Less", from.toString, upTo.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
              else
                Resources(resourceName + "More", from.toString, upTo.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original))
            }
          ),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }

  def doForAtMost[T](max: Int, xs: GenTraversable[T], original: Any, resourceName: String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: T => Unit) {
    if (max <= 0)
      throw new IllegalArgumentException(Resources("forAssertionsMoreThanZero", "'max'"))

    val resourceNamePrefix = getResourceNamePrefix(original)
    val result =
      runFor(xs.toIterator, resourceNamePrefix, 0, new ForResult[T], fun, _.passedCount > max)
    if (result.passedCount > max)
      throw new exceptions.TestFailedException(
        sde => Some(Resources(resourceName, max.toString, result.passedCount.toString, keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original))),
        None,
        getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
      )
  }
}
