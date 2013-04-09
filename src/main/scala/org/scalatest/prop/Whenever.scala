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
package org.scalatest.prop

/* Uncomment this when remove the deprecated type aliases in the org.scalatest.prop package object.
import org.scalatest.exceptions.DiscardedEvaluationException
*/

/**
 * Trait that contains the <code>whenever</code> clause that can be used in table- or generator-driven property checks.
 *
 * @author Bill Venners
 */
trait Whenever {

  /**
   * Evaluates the passed code block if the passed boolean condition is true, else throws <code>DiscardedEvaluationException</code>.
   *
   * <p>
   * The <code>whenever</code> method can be used inside property check functions to discard invocations of the function with
   * data for which it is known the property would fail. For example, given the following <code>Fraction</code> class:
   * </p>
   *
   * <pre class="stHighlight">
   * class Fraction(n: Int, d: Int) {
   *
   *   require(d != 0)
   *   require(d != Integer.MIN_VALUE)
   *   require(n != Integer.MIN_VALUE)
   *
   *   val numer = if (d < 0) -1 * n else n
   *   val denom = d.abs
   *
   *   override def toString = numer + " / " + denom
   * }
   * </pre>
   *
   * <pre class="stHighlight">
   * import org.scalatest.prop.TableDrivenPropertyChecks._
   *
   * val fractions =
   *   Table(
   *     ("n", "d"),
   *     (  1,   2),
   *     ( -1,   2),
   *     (  1,  -2),
   *     ( -1,  -2),
   *     (  3,   1),
   *     ( -3,   1),
   *     ( -3,   0),
   *     (  3,  -1),
   *     (  3,  Integer.MIN_VALUE),
   *     (Integer.MIN_VALUE, 3),
   *     ( -3,  -1)
   *   )
   * </pre>
   *
   * <p>
   * Imagine you wanted to check a property against this class with data that includes some
   * value that are rejected by the constructor, such as a denominator of zero, which should
   * result in an <code>IllegalArgumentException</code>. You could use <code>whenever</code>
   * to discard any rows in the <code>fraction</code> that represent illegal arguments, like this:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.matchers.ShouldMatchers._
   *
   * forAll (fractions) { (n: Int, d: Int) =>
   *
   *   whenever (d != 0 && d != Integer.MIN_VALUE
   *       && n != Integer.MIN_VALUE) {
   *
   *     val f = new Fraction(n, d)
   *
   *     if (n < 0 && d < 0 || n > 0 && d > 0)
   *       f.numer should be > 0
   *     else if (n != 0)
   *       f.numer should be < 0
   *     else
   *       f.numer should be === 0
   *
   *     f.denom should be > 0
   *   }
   * }
   * </pre>
   *
   * <p>
   * In this example, rows 6, 8, and 9 have values that would cause a false to be passed
   * to <code>whenever</code>. (For example, in row 6, <code>d</code> is 0, which means <code>d</code> <code>!=</code> <code>0</code>
   * will be false.) For those rows, <code>whenever</code> will throw <code>DiscardedEvaluationException</code>,
   * which will cause the <code>forAll</code> method to discard that row.
   * </p>
   *
   * @param condition the boolean condition that determines whether <code>whenever</code> will evaluate the
   *    <code>fun</code> function (<code>condition</code> is true) or throws <code>DiscardedEvaluationException</code> (<code>condition</code> is false)
   * @param fun the function to evaluate if the specified <code>condition</code> is true
   */
  def whenever(condition: Boolean)(fun: => Unit) {
    if (!condition)
      throw new DiscardedEvaluationException
    else
     fun
  }
}
