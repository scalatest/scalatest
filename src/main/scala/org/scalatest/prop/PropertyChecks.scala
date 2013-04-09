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
package prop

/**
 * Trait that facilitates property checks on data supplied by tables and generators.
 *
 * <p>
 * This trait extends both <a href="TableDrivenPropertyChecks.html"><code>TableDrivenPropertyChecks</code></a> and
 * <a href="GeneratorDrivenPropertyChecks.html"><code>GeneratorDrivenPropertyChecks</code></a>. Thus by mixing in
 * this trait you can perform property checks on data supplied either by tables or generators. For the details of
 * table- and generator-driven property checks, see the documentation for each by following the links above.
 * </p>
 *
 * <p>
 * For a quick example of using both table and generator-driven property checks in the same suite of tests, however,
 * imagine you want to test this <code>Fraction</code> class:
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
 * <p>
 * If you mix in <code>PropertyChecks</code>, you could use a generator-driven property check to test that the passed values for numerator and
 * denominator are properly normalized, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * forAll { (n: Int, d: Int) =>
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
 * And you could use a table-driven property check to test that all combinations of invalid values passed to the <code>Fraction</code> constructor
 * produce the expected <code>IllegalArgumentException</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val invalidCombos =
 *   Table(
 *     ("n",               "d"),
 *     (Integer.MIN_VALUE, Integer.MIN_VALUE),
 *     (1,                 Integer.MIN_VALUE),
 *     (Integer.MIN_VALUE, 1),
 *     (Integer.MIN_VALUE, 0),
 *     (1,                 0)
 *   )
 *
 * forAll (invalidCombos) { (n: Int, d: Int) =>
 *   evaluating {
 *     new Fraction(n, d)
 *   } should produce [IllegalArgumentException]
 * }
 * </pre>
 *
 * @author Bill Venners
 */
trait PropertyChecks extends TableDrivenPropertyChecks with GeneratorDrivenPropertyChecks

/**
 * Companion object that facilitates the importing of <code>PropertyChecks</code> members as 
 * an alternative to mixing it in. One use case is to import <code>PropertyChecks</code> members so you can use
 * them in the Scala interpreter.
 *
 * @author Bill Venners
 */
object PropertyChecks extends PropertyChecks

