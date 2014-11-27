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
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.util.Calendar
import scala.collection.JavaConversions._

object GenGen {

val scaladocForTableFor1VerbatimString = """
/**
 * A table with 1 column.
 *
 * <p>
 * For an overview of using tables, see the documentation for trait
 * <a href="TableDrivenPropertyChecks.html">TableDrivenPropertyChecks</a>.
 * </p>
 *
 * <p>
 * This table is a sequence of objects, where each object represents one row of the (one-column) table.
 * This table also carries with it a <em>heading</em> tuple that gives a string name to the
 * lone column of the table.
 * </p>
 *
 * <p>
 * A handy way to create a <code>TableFor1</code> is via an <code>apply</code> factory method in the <code>Table</code>
 * singleton object provided by the <code>Tables</code> trait. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val examples =
 *   Table(
 *     "a",
 *       0,
 *       1,
 *       2,
 *       3,
 *       4,
 *       5,
 *       6,
 *       7,
 *       8,
 *       9
 *   )
 * </pre>
 *
 * <p>
 * Because you supplied a list of non-tuple objects, the type you'll get back will be a <code>TableFor1</code>.
 * </p>
 *
 * <p>
 * The table provides an <code>apply</code> method that takes a function with a parameter list that matches
 * the type of the objects contained in this table. The <code>apply</code> method will invoke the
 * function with the object in each row passed as the lone argument, in ascending order by index. (<em>I.e.</em>,
 * the zeroth object is checked first, then the object with index 1, then index 2, and so on until all the rows
 * have been checked (or until a failure occurs). The function represents a property of the code under test
 * that should succeed for every row of the table. If the function returns normally, that indicates the property
 * check succeeded for that row. If the function completes abruptly with an exception, that indicates the
 * property check failed and the <code>apply</code> method will complete abruptly with a
 * <code>TableDrivenPropertyCheckFailedException</code> that wraps the exception thrown by the supplied property function.
 * </p>
 * 
 * <p>
 * The usual way you'd invoke the <code>apply</code> method that checks a property is via a <code>forAll</code> method
 * provided by trait <code>TableDrivenPropertyChecks</code>. The <code>forAll</code> method takes a <code>TableFor1</code> as its
 * first argument, then in a curried argument list takes the property check function. It invokes <code>apply</code> on
 * the <code>TableFor1</code>, passing in the property check function. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * forAll (examples) { (a) =>
 *   a should equal (a * 1)
 * }
 * </pre>
 *
 * <p>
 * Because <code>TableFor1</code> is a <code>Seq[(A)]</code>, you can use it as a <code>Seq</code>. For example, here's how
 * you could get a sequence of <a href="../Outcome.html"><code>Outcome</code></a>s for each row of the table, indicating whether a property check succeeded or failed
 * on each row of the table:
 * </p>
 *
 * <pre class="stHighlight">
 * for (row <- examples) yield {
 *   outcomeOf { row._1 should not equal (7) }
 * }
 * </pre>
 *
 * <p>
 * Note: the <code>outcomeOf</code> method, contained in the <code>OutcomeOf</code> trait, will execute the supplied code (a by-name parameter) and
 * transform it to an <code>Outcome</code>. If no exception is thrown by the code, <code>outcomeOf</code> will result in a
 * <a href="../Succeeded\$.html"><code>Succeeded</code></a>, indicating the "property check"
 * succeeded. If the supplied code completes abruptly in an exception that would normally cause a test to fail, <code>outcomeOf</code> will result in
 * in a <a href="../Failed.html"><code>Failed</code></a> instance containing that exception. For example, the previous for expression would give you:
 * </p>
 *
 * <pre class="stHighlight">
 * Vector(Succeeded, Succeeded, Succeeded, Succeeded, Succeeded, Succeeded, Succeeded,
 *     Failed(org.scalatest.TestFailedException: 7 equaled 7), Succeeded, Succeeded)
 * </pre>
 *
 * <p>
 * This shows that all the property checks succeeded, except for the one at index 7.
 * </p>
 *
 * <p>
 * One other way to use a <code>TableFor1</code> is to test subsequent return values
 * of a stateful function. Imagine, for example, you had an object named <code>FiboGen</code>
 * whose <code>next</code> method returned the <em>next</em> fibonacci number, where next
 * means the next number in the series following the number previously returned by <code>next</code>.
 * So the first time <code>next</code> was called, it would return 0. The next time it was called
 * it would return 1. Then 1. Then 2. Then 3, and so on. <code>FiboGen</code> would need to
 * be stateful, because it has to remember where it is in the series. In such a situation,
 * you could create a <code>TableFor1</code> (a table with one column, which you could alternatively
 * think of as one row), in which each row represents
 * the next value you expect.
 * </p>
 *
 * <pre class="stHighlight">
 * val first14FiboNums =
 *   Table("n", 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233)
 * </pre>
 *
 * <p>
 * Then in your <code>forAll</code> simply call the function and compare it with the
 * expected return value, like this:
 * </p>
 *
 * <pre class="stHighlight">
 *  forAll (first14FiboNums) { n =>
 *    FiboGen.next should equal (n)
 *  }
 * </pre>
 *
 * @param heading a string name for the lone column of this table
 * @param rows a variable length parameter list of objects containing the data of this table
 *
 * @author Bill Venners 
 */
"""

val copyrightTemplate = """/*
 * Copyright 2001-$year$ Artima, Inc.
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
"""

val propertyCheckPreamble = """
import org.scalacheck.Arbitrary
import org.scalacheck.Shrink
import org.scalacheck.Prop
import org.scalacheck.Gen
import org.scalacheck.Prop._

/**
 * Trait containing methods that faciliate property checks against generated data using ScalaCheck.
 *
 * <p>
 * This trait contains <code>forAll</code> methods that provide various ways to check properties using
 * generated data. Use of this trait requires that ScalaCheck be on the class path when you compile and run your tests.
 * It also contains a <code>wherever</code> method that can be used to indicate a property need only hold whenever
 * some condition is true.
 * </p>
 *
 * <p>
 * For an example of trait <code>GeneratorDrivenPropertyChecks</code> in action, imagine you want to test this <code>Fraction</code> class:
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
 * To test the behavior of <code>Fraction</code>, you could mix in or import the members of <code>GeneratorDrivenPropertyChecks</code>
 * (and <code>Matchers</code>) and check a property using a <code>forAll</code> method, like this:
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
 * Trait <code>GeneratorDrivenPropertyChecks</code> provides overloaded <code>forAll</code> methods
 * that allow you to check properties using the data provided by a ScalaCheck generator. The simplest form
 * of <code>forAll</code> method takes two parameter lists, the second of which is implicit. The first parameter list
 * is a "property" function with one to six parameters. An implicit <code>Arbitrary</code> generator and <code>Shrink</code> object needs to be supplied for
 * The <code>forAll</code> method will pass each row of data to
 * each parameter type. ScalaCheck provides many implicit <code>Arbitrary</code> generators for common types such as
 * <code>Int</code>, <code>String</code>, <code>List[Float]</code>, <em>etc.</em>, in its <code>org.scalacheck.Arbitrary</code> companion
 * object. So long as you use types for which ScalaCheck already provides implicit <code>Arbitrary</code> generators, you needn't
 * worry about them. Same for <code>Shrink</code> objects, which are provided by ScalaCheck's <code>org.scalacheck.Shrink</code> companion
 * object. Most often you can simply pass a property function to <code>forAll</code>, and the compiler will grab the implicit
 * values provided by ScalaCheck.
 * </p>
 *
 * <p>
 * The <code>forAll</code> methods use the supplied <code>Arbitrary</code> generators to generate example
 * arguments and pass them to the property function, and
 * generate a <code>GeneratorDrivenPropertyCheckFailedException</code> if the function
 * completes abruptly for any exception that would <a href="../Suite.html#errorHandling">normally cause</a> a test to
 * fail in ScalaTest other than <code>DiscardedEvaluationException</code>. An
 * <code>DiscardedEvaluationException</code>,
 * which is thrown by the <code>whenever</code> method (defined in trait <code>Whenever</code>, which this trait extends) to indicate
 * a condition required by the property function is not met by a row
 * of passed data, will simply cause <code>forAll</code> to discard that row of data.
 * </p>
 *
 * <a name="supplyingArgumentNames"></a><h2>Supplying argument names</h2>
 *
 * <p>
 * You can optionally specify string names for the arguments passed to a property function, which will be used
 * in any error message when describing the argument values that caused the failure. To supply the names, place them in a comma separated list
 * in parentheses after <code>forAll</code> before the property function (a curried form of <code>forAll</code>). Here's
 * an example:
 * </p>
 *
 * <pre class="stHighlight">
 * forAll ("a", "b") { (a: String, b: String) =>
 *   a.length + b.length should equal ((a + b).length + 1) // Should fail
 * }
 * </pre>
 *
 * <p>
 * When this fails, you'll see an error message that includes this:
 * </p>
 *
 * <pre>
 * Occurred when passed generated values (
 *   a = "",
 *   b = ""
 * )
 * </pre>
 *
 * <p>
 * When you don't supply argument names, the error message will say <code>arg0</code>, <code>arg1</code>, <em>etc.</em>.
 * For example, this property check:
 * </p>
 *
 * <pre class="stHighlight">
 * forAll { (a: String, b: String) =>
 *   a.length + b.length should equal ((a + b).length + 1) // Should fail
 * }
 * </pre>
 *
 * <p>
 * Will fail with an error message that includes:
 * </p>
 *
 * <pre>
 * Occurred when passed generated values (
 *   arg0 = "",
 *   arg1 = ""
 * )
 * </pre>
 *
 * <a name="supplyingGenerators"></a><h2>Supplying generators</h2>
 *
 * <p>
 * ScalaCheck provides a nice library of compositors that makes it easy to create your own custom generators. If you
 * want to supply custom generators to a property check, place them in parentheses after <code>forAll</code>, before
 * the property check function (a curried form of <code>forAll</code>).
 * </p>
 *
 * <p>
 * For example, to create a generator of even integers between (and including) -2000 and 2000, you could write this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalacheck.Gen
 *
 * val evenInts = for (n <- Gen.choose(-1000, 1000)) yield 2 * n
 * </pre>
 *
 * <p>
 * Given this generator, you could use it on a property check like this:
 * </p>
 *
 * <pre class="stHighlight">
 * forAll (evenInts) { (n) => n % 2 should equal (0) }
 * </pre>
 *
 * <p>
 * Custom generators are necessary when you want to pass data types not supported by ScalaCheck's arbitrary generators,
 * but are also useful when some of the values in the full range for the passed types are not valid. For such values you
 * would use a <code>whenever</code> clause. In the <code>Fraction</code> class shown above, neither the passed numerator or
 * denominator can be <code>Integer.MIN_VALUE</code>, and the passed denominator cannot be zero. This shows up in the
 * <code>whenever</code> clause like this:
 * </p>
 *
 * <pre class="stHighlight">
 *   whenever (d != 0 && d != Integer.MIN_VALUE
 *       && n != Integer.MIN_VALUE) { ...
 * </pre>
 *
 * <p>
 * You could in addition define generators for the numerator and denominator that only produce valid values, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val validNumers =
 *   for (n <- Gen.choose(Integer.MIN_VALUE + 1, Integer.MAX_VALUE)) yield n
 * val validDenoms =
 *   for (d <- validNumers if d != 0) yield d
 * </pre>
 *
 * <p>
 * You could then use them in the property check like this:
 * </p>
 *
 * <pre class="stHighlight">
 * forAll (validNumers, validDenoms) { (n: Int, d: Int) =>
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
 * Note that even if you use generators that don't produce the invalid values, you still need the
 * <code>whenever</code> clause. The reason is that once a property fails, ScalaCheck will try and shrink
 * the values to the smallest values that still cause the property to fail. During this shrinking process ScalaCheck
 * may pass invalid values. The <code>whenever</code> clause is still needed to guard against those values. (The
 * <code>whenever</code> clause also clarifies to readers of the code exactly what the property is in a succinct
 * way, without requiring that they find and understand the generator definitions.)
 * </p>
 *
 * <a name="supplyingGeneratorsAndArgNames"></a><h2>Supplying both generators and argument names</h2>
 *
 * <p>
 * If you want to supply both generators and named arguments, you can do so by providing a list of <code>(&lt;generator&gt;, &lt;name&gt;)</code> pairs
 * in parentheses after <code>forAll</code>, before the property function. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * forAll ((validNumers, "n"), (validDenoms, "d")) { (n: Int, d: Int) =>
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
 * Were this property check to fail, it would mention the names n and d in the error message, like this:
 * </p>
 *
 * <pre>
 * Occurred when passed generated values (
 *   n = 17,
 *   d = 21
 * )
 * </pre>
 *
 * <a name="propCheckConfig"></a><h2>Property check configuration</h2>
 *
 * <p>
 * The property checks performed by the <code>forAll</code> methods of this trait can be flexibly configured via the services
 * provided by supertrait <code>Configuration</code>.  The five configuration parameters for property checks along with their 
 * default values and meanings are described in the following table:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong>Configuration Parameter</strong>
 * </th>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong>Default Value</strong>
 * </th>
 * <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">
 * <strong>Meaning</strong>
 * </th>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * minSuccessful
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * 100
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * the minimum number of successful property evaluations required for the property to pass
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * maxDiscarded
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * 500
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * the maximum number of discarded property evaluations allowed during a property check
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * minSize
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * 0
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * the minimum size parameter to provide to ScalaCheck, which it will use when generating objects for which size matters (such as strings or lists)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * maxSize
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * 100
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * the maximum size parameter to provide to ScalaCheck, which it will use when generating objects for which size matters (such as strings or lists)
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * workers
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * 1
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * specifies the number of worker threads to use during property evaluation
 * </td>
 * </tr>
 * </table>
 *
 * <p>
 * The <code>forAll</code> methods of trait <code>GeneratorDrivenPropertyChecks</code> each take a <code>PropertyCheckConfiguration</code>
 * object as an implicit parameter. This object provides values for each of the five configuration parameters. Trait <code>Configuration</code>
 * provides an implicit <code>val</code> named <code>generatorDrivenConfig</code> with each configuration parameter set to its default value. 
 * If you want to set one or more configuration parameters to a different value for all property checks in a suite you can override this
 * val (or hide it, for example, if you are importing the members of the <code>GeneratorDrivenPropertyChecks</code> companion object rather
 * than mixing in the trait.) For example, if
 * you want all parameters at their defaults except for <code>minSize</code> and <code>maxSize</code>, you can override
 * <code>generatorDrivenConfig</code>, like this:
 *
 * <pre class="stHighlight">
 * implicit override val generatorDrivenConfig =
 *   PropertyCheckConfiguration(minSize = 10, sizeRange = 10)
 * </pre>
 *
 * <p>
 * Or, hide it by declaring a variable of the same name in whatever scope you want the changed values to be in effect:
 * </p>
 *
 * <pre class="stHighlight">
 * implicit val generatorDrivenConfig =
 *   PropertyCheckConfiguration(minSize = 10, sizeRange = 10)
 * </pre>
 *
 * <p>
 * In addition to taking a <code>PropertyCheckConfiguration</code> object as an implicit parameter, the <code>forAll</code> methods of trait
 * <code>GeneratorDrivenPropertyChecks</code> also take a variable length argument list of <code>PropertyCheckConfigParam</code>
 * objects that you can use to override the values provided by the implicit <code>PropertyCheckConfiguration</code> for a single <code>forAll</code>
 * invocation. For example, if you want to set <code>minSuccessful</code> to 500 for just one particular <code>forAll</code> invocation,
 * you can do so like this:
 * </p>
 *
 * <pre class="stHighlight">
 * forAll (minSuccessful(500)) { (n: Int, d: Int) => ...
 * </pre>
 *
 * <p>
 * This invocation of <code>forAll</code> will use 500 for <code>minSuccessful</code> and whatever values are specified by the 
 * implicitly passed <code>PropertyCheckConfiguration</code> object for the other configuration parameters.
 * If you want to set multiple configuration parameters in this way, just list them separated by commas:
 * </p>
 * 
 * <pre class="stHighlight">
 * forAll (minSuccessful(500), maxDiscardedFactor(0.6)) { (n: Int, d: Int) => ...
 * </pre>
 *
 * <p>
 * If you are using an overloaded form of <code>forAll</code> that already takes an initial parameter list, just
 * add the configuration parameters after the list of generators, names, or generator/name pairs, as in:
 * </p>
 * 
 * <pre class="stHighlight">
 * // If providing argument names
 * forAll ("n", "d", minSuccessful(500), maxDiscarded(300)) {
 *   (n: Int, d: Int) => ...
 *
 * // If providing generators
 * forAll (validNumers, validDenoms, minSuccessful(500), maxDiscarded(300)) {
 *   (n: Int, d: Int) => ...
 *
 * // If providing (&lt;generators&gt;, &lt;name&gt;) pairs
 * forAll ((validNumers, "n"), (validDenoms, "d"), minSuccessful(500), maxDiscarded(300)) {
 *   (n: Int, d: Int) => ...
 * </pre>
 *
 * <p>
 * For more information, see the documentation for supertrait <a href="Configuration.html"><code>Configuration</code></a>.
 * </p>
 * 
 * @author Bill Venners
 */
trait GeneratorDrivenPropertyChecks extends Whenever with Configuration {

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed 
   * <code>PropertyGenConfig</code> object with explicitly passed parameter values.
   *
   * <p>
   * This method creates a <code>ConfiguredPropertyCheck</code> object that has six overloaded apply methods
   * that take a function. Thus it is used with functions of all six arities.
   * Here are some examples:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), maxSize(10)) { (a: String) =>
   *   a.length should equal ((a).length)
   * }
   *
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String) =>
   *   a.length + b.length should equal ((a + b).length)
   * }
   *
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String) =>
   *   a.length + b.length + c.length should equal ((a + b + c).length)
   * }
   *
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String, d: String) =>
   *   a.length + b.length + c.length + d.length should equal ((a + b + c + d).length)
   * }
   *
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String, d: String, e: String) =>
   *   a.length + b.length + c.length + d.length + e.length should equal ((a + b + c + d + e).length)
   * }
   *
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String, d: String, e: String, f: String) =>
   *   a.length + b.length + c.length + d.length + e.length + f.length should equal ((a + b + c + d + e + f).length)
   * }
   * </pre>
   *
   * @param configParams a variable length list of <code>PropertyCheckConfigParam</code> objects that should override corresponding
   *   values in the <code>PropertyCheckConfiguration</code> implicitly passed to the <code>apply</code> methods of the <code>ConfiguredPropertyCheck</code>
   *   object returned by this method.
   */
  def forAll(configParams: PropertyCheckConfigParam*): ConfiguredPropertyCheck = new ConfiguredPropertyCheck(configParams)

  /**
   * Performs a configured property checks by applying property check functions passed to its <code>apply</code> methods to arguments
   * supplied by implicitly passed generators, modifying the values in the 
   * <code>PropertyGenConfig</code> object passed implicitly to its <code>apply</code> methods with parameter values passed to its constructor.
   *
   * <p>
   * Instances of this class are returned by trait <code>GeneratorDrivenPropertyChecks</code> <code>forAll</code> method that accepts a variable length
   * argument list of <code>PropertyCheckConfigParam</code> objects. Thus it is used with functions of all six arities.
   * Here are some examples:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), maxSize(10)) { (a: String) =>
   *   a.length should equal ((a).length)
   * }
   *
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String) =>
   *   a.length + b.length should equal ((a + b).length)
   * }
   *
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String) =>
   *   a.length + b.length + c.length should equal ((a + b + c).length)
   * }
   *
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String, d: String) =>
   *   a.length + b.length + c.length + d.length should equal ((a + b + c + d).length)
   * }
   *
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String, d: String, e: String) =>
   *   a.length + b.length + c.length + d.length + e.length should equal ((a + b + c + d + e).length)
   * }
   *
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String, d: String, e: String, f: String) =>
   *   a.length + b.length + c.length + d.length + e.length + f.length should equal ((a + b + c + d + e + f).length)
   * }
   * </pre>
   *
   * <p>
   * In the first example above, the <code>ConfiguredPropertyCheck</code> object is returned by:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), maxSize(10))
   * </pre>
   *
   * <p>
   * The code that follows is an invocation of one of the <code>ConfiguredPropertyCheck</code> <code>apply</code> methods:
   * </p>
   *
   * <pre class="stHighlight">
   * { (a: String) =>
   *   a.length should equal ((a).length)
   * }
   * </pre>
   *
   * @param configParams a variable length list of <code>PropertyCheckConfigParam</code> objects that should override corresponding
   *   values in the <code>PropertyCheckConfiguration</code> implicitly passed to the <code>apply</code> methods of instances of this class.
   *
   * @author Bill Venners
  */
  class ConfiguredPropertyCheck(configParams: Seq[PropertyCheckConfigParam]) {

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed 
   * <code>PropertyGenConfig</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), maxSize(10)) { (a: String) =>
   *   a.length should equal ((a).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A](fun: (A) => Unit)
      (implicit
        config: PropertyCheckConfiguration,
      arbA: Arbitrary[A], shrA: Shrink[A]
      ) {
        val propF = { (a: A) =>
          val (unmetCondition, exception) =
            try {
              fun(a)
              (false, None)
            }
            catch {
              case e: DiscardedEvaluationException => (true, None)
              case e: Throwable => (false, Some(e))
            }
          !unmetCondition ==> (
            if (exception.isEmpty) Prop.passed else Prop.exception(exception.get)
          )
        }
        val prop = Prop.forAll(propF)
        val params = getParams(configParams, config)
        Checkers.doCheck(prop, params, "GeneratorDrivenPropertyChecks.scala", "apply")
    }

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed 
   * <code>PropertyGenConfig</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String) =>
   *   a.length + b.length should equal ((a + b).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A, B](fun: (A, B) => Unit)
      (implicit
        config: PropertyCheckConfiguration,
      arbA: Arbitrary[A], shrA: Shrink[A],
      arbB: Arbitrary[B], shrB: Shrink[B]
      ) {
        val propF = { (a: A, b: B) =>
          val (unmetCondition, exception) =
            try {
              fun(a, b)
              (false, None)
            }
            catch {
              case e: DiscardedEvaluationException => (true, None)
              case e: Throwable => (false, Some(e))
            }
          !unmetCondition ==> (
            if (exception.isEmpty) Prop.passed else Prop.exception(exception.get)
          )
        }
        val prop = Prop.forAll(propF)
        val params = getParams(configParams, config)
        Checkers.doCheck(prop, params, "GeneratorDrivenPropertyChecks.scala", "apply")
    }

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed 
   * <code>PropertyGenConfig</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String) =>
   *   a.length + b.length + c.length should equal ((a + b + c).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A, B, C](fun: (A, B, C) => Unit)
      (implicit
        config: PropertyCheckConfiguration,
      arbA: Arbitrary[A], shrA: Shrink[A],
      arbB: Arbitrary[B], shrB: Shrink[B],
      arbC: Arbitrary[C], shrC: Shrink[C]
      ) {
        val propF = { (a: A, b: B, c: C) =>
          val (unmetCondition, exception) =
            try {
              fun(a, b, c)
              (false, None)
            }
            catch {
              case e: DiscardedEvaluationException => (true, None)
              case e: Throwable => (false, Some(e))
            }
          !unmetCondition ==> (
            if (exception.isEmpty) Prop.passed else Prop.exception(exception.get)
          )
        }
        val prop = Prop.forAll(propF)
        val params = getParams(configParams, config)
        Checkers.doCheck(prop, params, "GeneratorDrivenPropertyChecks.scala", "apply")
    }

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed 
   * <code>PropertyGenConfig</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String, d: String) =>
   *   a.length + b.length + c.length + d.length should equal ((a + b + c + d).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A, B, C, D](fun: (A, B, C, D) => Unit)
      (implicit
        config: PropertyCheckConfiguration,
      arbA: Arbitrary[A], shrA: Shrink[A],
      arbB: Arbitrary[B], shrB: Shrink[B],
      arbC: Arbitrary[C], shrC: Shrink[C],
      arbD: Arbitrary[D], shrD: Shrink[D]
      ) {
        val propF = { (a: A, b: B, c: C, d: D) =>
          val (unmetCondition, exception) =
            try {
              fun(a, b, c, d)
              (false, None)
            }
            catch {
              case e: DiscardedEvaluationException => (true, None)
              case e: Throwable => (false, Some(e))
            }
          !unmetCondition ==> (
            if (exception.isEmpty) Prop.passed else Prop.exception(exception.get)
          )
        }
        val prop = Prop.forAll(propF)
        val params = getParams(configParams, config)
        Checkers.doCheck(prop, params, "GeneratorDrivenPropertyChecks.scala", "apply")
    }

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed 
   * <code>PropertyGenConfig</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String, d: String, e: String) =>
   *   a.length + b.length + c.length + d.length + e.length should equal ((a + b + c + d + e).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A, B, C, D, E](fun: (A, B, C, D, E) => Unit)
      (implicit
        config: PropertyCheckConfiguration,
      arbA: Arbitrary[A], shrA: Shrink[A],
      arbB: Arbitrary[B], shrB: Shrink[B],
      arbC: Arbitrary[C], shrC: Shrink[C],
      arbD: Arbitrary[D], shrD: Shrink[D],
      arbE: Arbitrary[E], shrE: Shrink[E]
      ) {
        val propF = { (a: A, b: B, c: C, d: D, e: E) =>
          val (unmetCondition, exception) =
            try {
              fun(a, b, c, d, e)
              (false, None)
            }
            catch {
              case e: DiscardedEvaluationException => (true, None)
              case e: Throwable => (false, Some(e))
            }
          !unmetCondition ==> (
            if (exception.isEmpty) Prop.passed else Prop.exception(exception.get)
          )
        }
        val prop = Prop.forAll(propF)
        val params = getParams(configParams, config)
        Checkers.doCheck(prop, params, "GeneratorDrivenPropertyChecks.scala", "apply")
    }

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed 
   * <code>PropertyGenConfig</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), maxSize(10)) { (a: String, b: String, c: String, d: String, e: String, f: String) =>
   *   a.length + b.length + c.length + d.length + e.length + f.length should equal ((a + b + c + d + e + f).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A, B, C, D, E, F](fun: (A, B, C, D, E, F) => Unit)
      (implicit
        config: PropertyCheckConfiguration,
      arbA: Arbitrary[A], shrA: Shrink[A],
      arbB: Arbitrary[B], shrB: Shrink[B],
      arbC: Arbitrary[C], shrC: Shrink[C],
      arbD: Arbitrary[D], shrD: Shrink[D],
      arbE: Arbitrary[E], shrE: Shrink[E],
      arbF: Arbitrary[F], shrF: Shrink[F]
      ) {
        val propF = { (a: A, b: B, c: C, d: D, e: E, f: F) =>
          val (unmetCondition, exception) =
            try {
              fun(a, b, c, d, e, f)
              (false, None)
            }
            catch {
              case e: DiscardedEvaluationException => (true, None)
              case e: Throwable => (false, Some(e))
            }
          !unmetCondition ==> (
            if (exception.isEmpty) Prop.passed else Prop.exception(exception.get)
          )
        }
        val prop = Prop.forAll(propF)
        val params = getParams(configParams, config)
        Checkers.doCheck(prop, params, "GeneratorDrivenPropertyChecks.scala", "apply")
    }
  }
"""

val propertyCheckForAllTemplate = """
  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll { ($namesAndTypes$) =>
   *   $sumOfArgLengths$ should equal (($sumOfArgs$).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
  def forAll[$alphaUpper$](fun: ($alphaUpper$) => Unit)
    (implicit
      config: PropertyCheckConfiguration,
$arbShrinks$
    ) {
      val propF = { ($argType$) =>
        val (unmetCondition, exception) =
          try {
            fun($alphaLower$)
            (false, None)
          }
          catch {
            case e: DiscardedEvaluationException => (true, None)
            case e: Throwable => (false, Some(e))
          }
        !unmetCondition ==> (
          if (exception.isEmpty) Prop.passed else Prop.exception(exception.get)
        )
      }
      val prop = Prop.forAll(propF)
      val params = getParams(Seq(), config)
      Checkers.doCheck(prop, params, "GeneratorDrivenPropertyChecks.scala", "forAll")
  }

  /**
   * Performs a property check by applying the specified property check function with the specified
   * argument names to arguments supplied by implicitly passed generators.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll ($argNames$) { ($namesAndTypes$) =>
   *   $sumOfArgLengths$ should equal (($sumOfArgs$).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
  def forAll[$alphaUpper$]($argNameNamesAndTypes$, configParams: PropertyCheckConfigParam*)(fun: ($alphaUpper$) => Unit)
    (implicit
      config: PropertyCheckConfiguration,
$arbShrinks$
    ) {
      val propF = { ($argType$) =>
        val (unmetCondition, exception) =
          try {
            fun($alphaLower$)
            (false, None)
          }
          catch {
            case e: DiscardedEvaluationException => (true, None)
            case e: Throwable => (false, Some(e))
          }
        !unmetCondition ==> (
          if (exception.isEmpty) Prop.passed else Prop.exception(exception.get)
        )
      }
      val prop = Prop.forAll(propF)
      val params = getParams(configParams, config)
      Checkers.doCheck(prop, params, "GeneratorDrivenPropertyChecks.scala", "forAll", Some(List($argNameNames$)))
  }

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by the specified generators.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalacheck.Gen
   *
   * // Define your own string generator:
   * val famousLastWords = for {
   *   s <- Gen.oneOf("the", "program", "compiles", "therefore", "it", "should", "work")
   * } yield s
   * 
   * forAll ($famousArgs$) { ($namesAndTypes$) =>
   *   $sumOfArgLengths$ should equal (($sumOfArgs$).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
  def forAll[$alphaUpper$]($genArgsAndTypes$, configParams: PropertyCheckConfigParam*)(fun: ($alphaUpper$) => Unit)
    (implicit
      config: PropertyCheckConfiguration,
$shrinks$
    ) {
      val propF = { ($argType$) =>
        val (unmetCondition, exception) =
          try {
            fun($alphaLower$)
            (false, None)
          }
          catch {
            case e: DiscardedEvaluationException => (true, None)
            case e: Throwable => (false, Some(e))
          }
        !unmetCondition ==> (
          if (exception.isEmpty) Prop.passed else Prop.exception(exception.get)
        )
      }
      val prop = Prop.forAll($genArgs$)(propF)
      val params = getParams(configParams, config)
      Checkers.doCheck(prop, params, "GeneratorDrivenPropertyChecks.scala", "forAll")
  }

  /**
   * Performs a property check by applying the specified property check function to named arguments
   * supplied by the specified generators.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalacheck.Gen
   *
   * // Define your own string generator:
   * val famousLastWords = for {
   *   s <- Gen.oneOf("the", "program", "compiles", "therefore", "it", "should", "work")
   * } yield s
   * 
   * forAll ($nameGenTuples$) { ($namesAndTypes$) =>
   *   $sumOfArgLengths$ should equal (($sumOfArgs$).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
  def forAll[$alphaUpper$]($nameAndGenArgsAndTypes$, configParams: PropertyCheckConfigParam*)(fun: ($alphaUpper$) => Unit)
    (implicit
      config: PropertyCheckConfiguration,
$shrinks$
    ) {

$tupleBusters$

      val propF = { ($argType$) =>
        val (unmetCondition, exception) =
          try {
            fun($alphaLower$)
            (false, None)
          }
          catch {
            case e: DiscardedEvaluationException => (true, None)
            case e: Throwable => (false, Some(e))
          }
        !unmetCondition ==> (
          if (exception.isEmpty) Prop.passed else Prop.exception(exception.get)
        )
      }
      val prop = Prop.forAll($genArgs$)(propF)
      val params = getParams(configParams, config)
      Checkers.doCheck(prop, params, "GeneratorDrivenPropertyChecks.scala", "forAll", Some(List($argNameNames$)))
  }
"""

val generatorDrivenPropertyChecksCompanionObjectVerbatimString = """

object GeneratorDrivenPropertyChecks extends GeneratorDrivenPropertyChecks
"""

val generatorSuitePreamble = """

import org.scalacheck.Gen
"""

val generatorSuitePostamble = """
  val famousLastWords = for {
    s <- Gen.oneOf("the", "program", "compiles", "therefore", "it", "should", "work")
  } yield s

  val sevenEleven: Gen[String] =
    Gen.sized { (size: Int) =>
      if (size >= 7 && size <= 11)
        Gen.value("OKAY")
      else
        throw new Exception("expected 7 <= size <= 11 but got " + size)
    }

  val fiveFive: Gen[String] =
    Gen.sized { (size: Int) =>
      if (size == 5)
        Gen.value("OKAY")
      else
        throw new Exception("expected size 5 but got " + size)
    }
"""

val generatorSuiteTemplate = """

  def `generator-driven property that takes $n$ args, which succeeds` {

    forAll { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  def `generator-driven property that takes $n$ args, which fails` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  def `generator-driven property that takes $n$ named args, which succeeds` {

    forAll ($argNames$) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  def `generator-driven property that takes $n$ named args, which fails` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($argNames$) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  def `generator-driven property that takes $n$ args and generators, which succeeds` {

    forAll ($famousArgs$) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  def `generator-driven property that takes $n$ args and generators, which fails` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which succeeds` {

    forAll ($nameGenTuples$) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which fails` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  // Same thing, but with config params
  def `generator-driven property that takes $n$ args, which succeeds, with config params` {

    forAll (minSize(10), maxSize(20)) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  def `generator-driven property that takes $n$ args, which fails, with config params` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll (minSize(10), maxSize(20)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  def `generator-driven property that takes $n$ named args, which succeeds, with config params` {

    forAll ($argNames$, minSize(10), maxSize(20)) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  def `generator-driven property that takes $n$ named args, which fails, with config params` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($argNames$, minSize(10), maxSize(20)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  def `generator-driven property that takes $n$ args and generators, which succeeds, with config params` {

    forAll ($famousArgs$, minSize(10), maxSize(20)) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  def `generator-driven property that takes $n$ args and generators, which fails, with config params` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($famousArgs$, minSize(10), maxSize(20)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which succeeds, with config params` {

    forAll ($nameGenTuples$, minSize(10), maxSize(20)) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which fails, with config params` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($nameGenTuples$, minSize(10), maxSize(20)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  // Same thing, but set minSuccessful to 5 with param, prop fails after 5
  def `generator-driven property that takes $n$ args, which succeeds, with minSuccessful param set to 5` {

    var i = 0
    forAll (minSuccessful(5)) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  def `generator-driven property that takes $n$ args, which fails, with minSuccessful param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll (minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  def `generator-driven property that takes $n$ named args, which succeeds, with minSuccessful param set to 5` {

    var i = 0
    forAll ($argNames$, minSuccessful(5)) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  def `generator-driven property that takes $n$ named args, which fails, with minSuccessful param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  def `generator-driven property that takes $n$ args and generators, which succeeds, with minSuccessful param set to 5` {

    var i = 0
    forAll ($famousArgs$, minSuccessful(5)) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  def `generator-driven property that takes $n$ args and generators, which fails, with minSuccessful param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which succeeds, with minSuccessful param set to 5` {

    var i = 0
    forAll ($nameGenTuples$, minSuccessful(5)) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which fails, with minSuccessful param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  // Same thing, but set default minSuccessful to 5, prop fails after 5
  def `generator-driven property that takes $n$ args, which succeeds, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  def `generator-driven property that takes $n$ args, which fails, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  def `generator-driven property that takes $n$ named args, which succeeds, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll ($argNames$) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  def `generator-driven property that takes $n$ named args, which fails, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  def `generator-driven property that takes $n$ args and generators, which succeeds, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll ($famousArgs$) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  def `generator-driven property that takes $n$ args and generators, which fails, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which succeeds, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll ($nameGenTuples$) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which fails, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  // Same thing, but set maxDiscarded to 5 with param, prop fails after 5
  def `generator-driven property that takes $n$ args, which succeeds, with maxDiscarded param set to 5` {

    var i = 0
    forAll (maxDiscarded(5)) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  def `generator-driven property that takes $n$ args, which fails, with maxDiscarded param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll (maxDiscarded(5)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 6) { assert(1 + 1 === (2)) }
      }
    }
  }

  def `generator-driven property that takes $n$ named args, which succeeds, with maxDiscarded param set to 5` {

    var i = 0
    forAll ($argNames$, maxDiscarded(5)) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  def `generator-driven property that takes $n$ named args, which fails, with maxDiscarded param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$, maxDiscarded(5)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 6) { assert(1 + 1 === (2)) }
      }
    }
  }

  def `generator-driven property that takes $n$ args and generators, which succeeds, with maxDiscarded param set to 5` {

    var i = 0
    forAll ($famousArgs$, maxDiscarded(5)) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  def `generator-driven property that takes $n$ args and generators, which fails, with maxDiscarded param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$, maxDiscarded(5)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 6) { assert(1 + 1 === (2)) }
      }
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which succeeds, with maxDiscarded param set to 5` {

    var i = 0
    forAll ($nameGenTuples$, maxDiscarded(5)) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which fails, with maxDiscarded param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$, maxDiscarded(5)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 6) { assert(1 + 1 === (2)) }
      }
    }
  }

  // Same thing, but set default maxDiscarded to 5, prop fails after 5
  def `generator-driven property that takes $n$ args, which succeeds, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    var i = 0
    forAll { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  def `generator-driven property that takes $n$ args, which fails, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll { ($namesAndTypes$) =>
        i += 1
        whenever (i > 6) { assert(1 + 1 === (2)) }
      }
    }
  }

  def `generator-driven property that takes $n$ named args, which succeeds, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    var i = 0
    forAll ($argNames$) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  def `generator-driven property that takes $n$ named args, which fails, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 6) { assert(1 + 1 === (2)) }
      }
    }
  }

  def `generator-driven property that takes $n$ args and generators, which succeeds, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    var i = 0
    forAll ($famousArgs$) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  def `generator-driven property that takes $n$ args and generators, which fails, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 6) { assert(1 + 1 === (2)) }
      }
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which succeeds, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    var i = 0
    forAll ($nameGenTuples$) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which fails, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 6) { assert(1 + 1 === (2)) }
      }
    }
  }

  // set minSize > maxSize with (param, param) (intercept IAE)
  def `generator-driven property that takes $n$ args, which should throw IAE because maxSize > maxSize, specified as (param, param)` {

    intercept[IllegalArgumentException] {
      forAll (minSize(5), maxSize(4)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  def `generator-driven property that takes $n$ named args, which should throw IAE because maxSize > maxSize, specified as (param, param)` {

    intercept[IllegalArgumentException] {
      forAll ($argNames$, minSize(5), maxSize(4)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  def `generator-driven property that takes $n$ args and generators, which should throw IAE because maxSize > maxSize, specified as (param, param)` {

    intercept[IllegalArgumentException] {
      forAll ($famousArgs$, minSize(5), maxSize(4)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which should throw IAE because maxSize > maxSize, specified as (param, param)` {

    intercept[IllegalArgumentException] {
      forAll ($nameGenTuples$, minSize(5), maxSize(4)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  // set minSize > maxSize with (param, default) (intercept IAE)
  def `generator-driven property that takes $n$ args, which should throw IAE because maxSize > maxSize, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 4)

    intercept[IllegalArgumentException] {
      forAll (minSize(5)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  def `generator-driven property that takes $n$ named args, which should throw IAE because maxSize > maxSize, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 4)

    intercept[IllegalArgumentException] {
      forAll ($argNames$, minSize(5)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  def `generator-driven property that takes $n$ args and generators, which should throw IAE because maxSize > maxSize, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 4)

    intercept[IllegalArgumentException] {
      forAll ($famousArgs$, minSize(5)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which should throw IAE because maxSize > maxSize, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 4)

    intercept[IllegalArgumentException] {
      forAll ($nameGenTuples$, minSize(5)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  // set minSize > maxSize with (default, param) (intercept IAE)
  def `generator-driven property that takes $n$ args, which should throw IAE because maxSize > maxSize, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    intercept[IllegalArgumentException] {
      forAll (maxSize(4)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  def `generator-driven property that takes $n$ named args, which should throw IAE because maxSize > maxSize, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    intercept[IllegalArgumentException] {
      forAll ($argNames$, maxSize(4)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  def `generator-driven property that takes $n$ args and generators, which should throw IAE because maxSize > maxSize, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    intercept[IllegalArgumentException] {
      forAll ($famousArgs$, maxSize(4)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  def `generator-driven property that takes $n$ named args and generators, which should throw IAE because maxSize > maxSize, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    intercept[IllegalArgumentException] {
      forAll ($nameGenTuples$, maxSize(4)) { ($namesAndTypes$) =>
        assert(1 + 1 === (2))
      }
    }
  }

  // set maxSize with param (ensure always passed with a size less than maxSize)
  def `generator-driven property that takes $n$ args, with maxSize specified as param` {

    forAll (maxSize(5)) { ($namesAndTypes$) =>
$lengthAssertions$
    }
  }

  def `generator-driven property that takes $n$ named args, with maxSize specified as param` {

    forAll ($argNames$, maxSize(5)) { ($namesAndTypes$) =>
$lengthAssertions$
    }
  }

  // set maxSize with default (ensure always passed with a size less than maxSize)
  def `generator-driven property that takes $n$ args, with maxSize specified as default` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 5)

    forAll { ($namesAndTypes$) =>
$lengthAssertions$
    }
  }

  def `generator-driven property that takes $n$ named args, with maxSize specified as default` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 5)

    forAll ($argNames$) { ($namesAndTypes$) =>
$lengthAssertions$
    }
  }
 
  // set minSize == maxSize with (param, param) (ensure always passed with that size)
  def `generator-driven property that takes $n$ args and generators, with minSize == maxSize, specified as (param, param)` {

    forAll ($fiveFiveArgs$, minSize(5), maxSize(5)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  def `generator-driven property that takes $n$ named args and generators, with minSize == maxSize, specified as (param, param)` {

    forAll ($fiveFiveNameGenTuples$, minSize(5), maxSize(5)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set minSize == maxSize with (param, default) (ensure always passed with that size)
  def `generator-driven property that takes $n$ args and generators, with minSize == maxSize, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 5)

    forAll ($fiveFiveArgs$, minSize(5)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  def `generator-driven property that takes $n$ named args and generators, with minSize == maxSize, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 5)

    forAll ($fiveFiveNameGenTuples$, minSize(5)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set minSize == maxSize with (default, param) (ensure always passed with that size)
  def `generator-driven property that takes $n$ args and generators, with minSize == maxSize, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    forAll ($fiveFiveArgs$, maxSize(5)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  def `generator-driven property that takes $n$ named args and generators, with minSize == maxSize, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    forAll ($fiveFiveNameGenTuples$, maxSize(5)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set minSize == maxSize with (default, default) (ensure always passed with that size)
  def `generator-driven property that takes $n$ args and generators, with minSize == maxSize, specified as (default, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(minSize = 5, maxSize = 5)

    forAll ($fiveFiveArgs$) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  def `generator-driven property that takes $n$ named args and generators, with minSize == maxSize, specified as (default, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(minSize = 5, maxSize = 5)

    forAll ($fiveFiveNameGenTuples$) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set minSize to 7 and maxSize to 11 with (param, param) (ensure always passed with that size)
  def `generator-driven property that takes $n$ args and generators, with minSize to 7 and maxSize to 11, specified as (param, param)` {

    forAll ($sevenElevenArgs$, minSize(7), maxSize(11)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  def `generator-driven property that takes $n$ named args and generators, with minSize to 7 and maxSize to 11, specified as (param, param)` {

    forAll ($sevenElevenNameGenTuples$, minSize(7), maxSize(11)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set minSize to 7 and maxSize to 11 with (param, default) (ensure always passed with that size)
  def `generator-driven property that takes $n$ args and generators, with minSize to 7 and maxSize to 11, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 11)

    forAll ($sevenElevenArgs$, minSize(7)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  def `generator-driven property that takes $n$ named args and generators, with minSize to 7 and maxSize to 11, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 11)

    forAll ($sevenElevenNameGenTuples$, minSize(7)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set minSize to 7 and maxSize to 11 with (default, param) (ensure always passed with that size)
  def `generator-driven property that takes $n$ args and generators, with minSize to 7 and maxSize to 11, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 7)

    forAll ($sevenElevenArgs$, maxSize(11)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  def `generator-driven property that takes $n$ named args and generators, with minSize to 7 and maxSize to 11, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 7)

    forAll ($sevenElevenNameGenTuples$, maxSize(11)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set minSize to 7 and maxSize to 11 with (default, default) (ensure always passed with that size)
  def `generator-driven property that takes $n$ args and generators, with minSize to 7 and maxSize to 11, specified as (default, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(minSize = 7, maxSize = 11)

    forAll ($sevenElevenArgs$) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  def `generator-driven property that takes $n$ named args and generators, with minSize to 7 and maxSize to 11, specified as (default, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(minSize = 7, maxSize = 11)

    forAll ($sevenElevenNameGenTuples$) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }
"""

val checkersSuiteTemplate = """

  def `ScalaCheck property that takes $n$ args, which succeeds` {

    check { ($namesAndTypes$) =>
      $sumOfArgLengths$ == (($sumOfArgs$).length)
    }
  }

  def `ScalaCheck property that takes $n$ args, which fails` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      check { ($namesAndTypes$) =>
        $sumOfArgLengths$ < 0
      }
    }
  }

  def `ScalaCheck property that takes $n$ args and generators, which succeeds` {

    val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
      $sumOfArgLengths$ == (($sumOfArgs$).length)
    }
    check(prop)
  }

  def `ScalaCheck property that takes $n$ args and generators, which fails` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
        $sumOfArgLengths$ < 0
      }
      check(prop)
    }
  }

  // Same thing, but with config params
  def `ScalaCheck property that takes $n$ args, which succeeds, with config params` {

    check(
      ($namesAndTypes$) => $sumOfArgLengths$ == (($sumOfArgs$).length),
      minSize(10),
      maxSize(20)
    )
  }

  def `ScalaCheck property that takes $n$ args, which fails, with config params` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      check(
        ($namesAndTypes$) => $sumOfArgLengths$ < 0,
        minSize(10),
        maxSize(20)
      )
    }
  }

  def `ScalaCheck property that takes $n$ args and generators, which succeeds, with config params` {

    val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
      $sumOfArgLengths$ == (($sumOfArgs$).length)
    }
    check(prop, minSize(10), maxSize(20))
  }

  def `ScalaCheck property that takes $n$ args and generators, which fails, with config params` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
        $sumOfArgLengths$ < 0
      }
      check(prop, minSize(10), maxSize(20))
    }
  }

  // Same thing, but set minSuccessful to 5 with param, prop fails after 5
  def `ScalaCheck property that takes $n$ args, which succeeds, with minSuccessful param set to 5` {

    var i = 0
    check(
      ($namesAndTypes$) => {
        val res = i != 5
        i += 1
        res
      },
      minSuccessful(5)
    )
  }

  def `ScalaCheck property that takes $n$ args, which fails, with minSuccessful param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      check(
        ($namesAndTypes$) => {
          val res = i != 4
          i += 1
        res
        },
        minSuccessful(5)
      ) 
    }
  }

  def `ScalaCheck property that takes $n$ args and generators, which succeeds, with minSuccessful param set to 5` {

    var i = 0
    val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
      val res = i != 5
      i += 1
      res
    }
    check(prop, minSuccessful(5))
  }

  def `ScalaCheck property that takes $n$ args and generators, which fails, with minSuccessful param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
        val res = i != 4
        i += 1
        res
      }
      check(prop, minSuccessful(5))
    }
  }

  // Same thing, but set default minSuccessful to 5, prop fails after 5
  def `ScalaCheck property that takes $n$ args, which succeeds, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    check { ($namesAndTypes$) =>
      val res = i != 5
      i += 1
      res
    }
  }

  def `ScalaCheck property that takes $n$ args, which fails, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      check { ($namesAndTypes$) =>
        val res = i != 4
        i += 1
        res
      }
    }
  }

  def `ScalaCheck property that takes $n$ args and generators, which succeeds, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
      val res = i != 5
      i += 1
      res
    }
    check(prop)
  }

  def `ScalaCheck property that takes $n$ args and generators, which fails, with default minSuccessful param set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
        val res = i != 4
        i += 1
        res
      }
      check(prop)
    }
  }

  // Same thing, but set maxDiscarded to 5 with param, prop fails after 5
  def `ScalaCheck property that takes $n$ args, which succeeds, with maxDiscarded param set to 5` {

    var i = 0
    check(
      ($namesAndTypes$) => {
        i += 1
        (i > 5) ==> { 1 + 1 == (2) }
      },
      maxDiscarded(5)
    )
  }

  def `ScalaCheck property that takes $n$ args, which fails, with maxDiscarded param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      check(
        ($namesAndTypes$) => {
          i += 1
          (i > 6) ==> { 1 + 1 == (2) }
        },
        maxDiscarded(5)
      ) 
    }
  }

  def `ScalaCheck property that takes $n$ args and generators, which succeeds, with maxDiscarded param set to 5` {

    var i = 0
    val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
      i += 1
      (i > 5) ==> { 1 + 1 == (2) }
    }
    check(prop, maxDiscarded(5))
  }

  def `ScalaCheck property that takes $n$ args and generators, which fails, with maxDiscarded param set to 5` {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
        i += 1
        (i > 6) ==> { 1 + 1 == (2) }
      }
      check(prop, maxDiscarded(5))
    }
  }

  // Same thing, but set default maxDiscarded to 5, prop fails after 5
  def `ScalaCheck property that takes $n$ args, which succeeds, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    var i = 0
    check { ($namesAndTypes$) =>
      i += 1
      (i > 5) ==> { 1 + 1 == (2) }
    }
  }

  def `ScalaCheck property that takes $n$ args, which fails, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      check { ($namesAndTypes$) =>
        i += 1
        (i > 6) ==> { 1 + 1 == (2) }
      }
    }
  }

  def `ScalaCheck property that takes $n$ args and generators, which succeeds, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    var i = 0
    val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
      i += 1
      (i > 5) ==> { 1 + 1 == (2) }
    }
    check(prop)
  }

  def `ScalaCheck property that takes $n$ args and generators, which fails, with default maxDiscarded set to 5` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
        i += 1
        (i > 6) ==> { 1 + 1 == (2) }
      }
      check(prop)
    }
  }

  // set minSize > maxSize with (param, param) (intercept IAE)
  def `ScalaCheck property that takes $n$ args, which should throw IAE because maxSize > maxSize, specified as (param, param)` {

    intercept[IllegalArgumentException] {
      check(
        ($namesAndTypes$) => {
          1 + 1 == (2)
        },
        minSize(5),
        maxSize(4)
      ) 
    }
  }

  def `ScalaCheck property that takes $n$ args and generators, which should throw IAE because maxSize > maxSize, specified as (param, param)` {

    intercept[IllegalArgumentException] {
      val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
        1 + 1 == (2)
      }
      check(prop, minSize(5), maxSize(4))
    }
  }

  // set minSize > maxSize with (param, default) (intercept IAE)
  def `ScalaCheck property that takes $n$ args, which should throw IAE because maxSize > maxSize, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 4)

    intercept[IllegalArgumentException] {
      check(
        ($namesAndTypes$) => {
          1 + 1 == (2)
        },
        minSize(5)
      )
    }
  }

  def `ScalaCheck property that takes $n$ args and generators, which should throw IAE because maxSize > maxSize, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 4)

    intercept[IllegalArgumentException] {
      val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
        1 + 1 == (2)
      }
      check(prop, minSize(5))
    }
  }

  // set minSize > maxSize with (default, param) (intercept IAE)
  def `ScalaCheck property that takes $n$ args, which should throw IAE because maxSize > maxSize, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    intercept[IllegalArgumentException] {
      check(
        ($namesAndTypes$) => {
          1 + 1 == (2)
        },
        maxSize(4)
      )
    }
  }

  def `ScalaCheck property that takes $n$ args and generators, which should throw IAE because maxSize > maxSize, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    intercept[IllegalArgumentException] {
      val prop = forAll ($famousArgs$) { ($namesAndTypes$) =>
        1 + 1 == (2)
      }
      check(prop, maxSize(4))
    }
  }

  // set maxSize with param (ensure always passed with a size less than maxSize)
  def `ScalaCheck property that takes $n$ args, with maxSize specified as param` {

    check(
      ($namesAndTypes$) => {
$lengthExpressions$
      },
      maxSize(5)
    ) 
  }

  // set maxSize with default (ensure always passed with a size less than maxSize)
  def `ScalaCheck property that takes $n$ args, with maxSize specified as default` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 5)

    check { ($namesAndTypes$) =>
$lengthExpressions$
    }
  }

  // set minSize == maxSize with (param, param) (ensure always passed with that size)
  def `ScalaCheck property that takes $n$ args and generators, with minSize == maxSize, specified as (param, param)` {

    val prop = forAll ($fiveFiveArgs$) { ($namesAndTypes$) =>
$okayExpressions$
    }
    check(prop, minSize(5), maxSize(5))
  }

  // set minSize == maxSize with (param, default) (ensure always passed with that size)
  def `ScalaCheck property that takes $n$ args and generators, with minSize == maxSize, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 5)

    val prop = forAll ($fiveFiveArgs$) { ($namesAndTypes$) =>
$okayExpressions$
    }
    check(prop, minSize(5))
  }

  // set minSize == maxSize with (default, param) (ensure always passed with that size)
  def `ScalaCheck property that takes $n$ args and generators, with minSize == maxSize, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    val prop = forAll ($fiveFiveArgs$) { ($namesAndTypes$) =>
$okayExpressions$
    }
    check(prop, maxSize(5))
  }

  // set minSize == maxSize with (default, default) (ensure always passed with that size)
  def `ScalaCheck property that takes $n$ args and generators, with minSize == maxSize, specified as (default, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(minSize = 5, maxSize = 5)

    val prop = forAll ($fiveFiveArgs$) { ($namesAndTypes$) =>
$okayExpressions$
    }
    check(prop)
  }

  // set minSize to 7 and maxSize to 11 with (param, param) (ensure always passed with that size)
  def `ScalaCheck property that takes $n$ args and generators, with minSize to 7 and maxSize to 11, specified as (param, param)` {

    val prop = forAll ($sevenElevenArgs$) { ($namesAndTypes$) =>
$okayExpressions$
    }
    check(prop, minSize(7), maxSize(11))
  }

  // set minSize to 7 and maxSize to 11 with (param, default) (ensure always passed with that size)
  def `ScalaCheck property that takes $n$ args and generators, with minSize to 7 and maxSize to 11, specified as (param, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 11)

    val prop = forAll ($sevenElevenArgs$) { ($namesAndTypes$) =>
$okayExpressions$
    }
    check(prop, minSize(7))
  }

  // set minSize to 7 and maxSize to 11 with (default, param) (ensure always passed with that size)
  def `ScalaCheck property that takes $n$ args and generators, with minSize to 7 and maxSize to 11, specified as (default, param)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 7)

    val prop = forAll ($sevenElevenArgs$) { ($namesAndTypes$) =>
$okayExpressions$
    }
    check(prop, maxSize(11))
  }

  // set minSize to 7 and maxSize to 11 with (default, default) (ensure always passed with that size)
  def `ScalaCheck property that takes $n$ args and generators, with minSize to 7 and maxSize to 11, specified as (default, default)` {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfig(minSize = 7, maxSize = 11)

    val prop = forAll ($sevenElevenArgs$) { ($namesAndTypes$) =>
$okayExpressions$
    }
    check(prop)
  }
"""
// 1712  2205

// For some reason that I don't understand, I need to leave off the stars before the <pre> when 
// they are next to ST commands. So I say  "   <pre>" sometimes instead of " * <pre>".

  val thisYear = Calendar.getInstance.get(Calendar.YEAR)

  def genPropertyChecks(targetDir: File) {
    targetDir.mkdirs()
    val bw = new BufferedWriter(new FileWriter(new File(targetDir, "GeneratorDrivenPropertyChecks.scala")))
 
    try {
      val st = new org.antlr.stringtemplate.StringTemplate(copyrightTemplate)
      st.setAttribute("year", thisYear);
      bw.write(st.toString)
      bw.write(propertyCheckPreamble)
      val alpha = "abcdefghijklmnopqrstuv"
      for (i <- 1 to 6) {
        val st = new org.antlr.stringtemplate.StringTemplate(propertyCheckForAllTemplate)
        val alphaLower = alpha.take(i).mkString(", ")
        val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
        val argType = alpha.take(i).map(c => c + ": " + c.toUpper).mkString(", ")
        val strings = List.fill(i)("String").mkString(", ")
        val arbShrinks = alpha.take(i).toUpperCase.map(
          c => "      arb" + c + ": Arbitrary[" + c + "], shr" + c + ": Shrink[" + c + "]"
        ).mkString(",\n")
        val shrinks = alpha.take(i).toUpperCase.map(
          c => "      shr" + c + ": Shrink[" + c + "]"
        ).mkString(",\n")
        val sumOfArgLengths = alpha.take(i).map(_ + ".length").mkString(" + ")
        val namesAndTypes = alpha.take(i).map(_ + ": String").mkString(", ")
        val sumOfArgs = alpha.take(i).mkString(" + ")
        val genArgsAndTypes = alpha.take(i).toUpperCase.map(c => "gen" + c + ": Gen[" + c + "]").mkString(", ")
        val genArgs = alpha.take(i).toUpperCase.map(c => "gen" + c).mkString(", ")
        val famousArgs = List.fill(i)("famousLastWords").mkString(", ")
        val argNames = alpha.take(i).map("\"" + _ + "\"").mkString(", ")
        val argNameNames = alpha.take(i).toUpperCase.map("name" + _).mkString(", ")
        val argNameNamesAndTypes = alpha.take(i).toUpperCase.map("name" + _ + ": String").mkString(", ")
        val nameGenTuples = alpha.take(i).map("(famousLastWords, \"" + _ + "\")").mkString(", ")
        val nameAndGenArgsAndTypes = alpha.take(i).toUpperCase.map(c => "genAndName" + c + ": (Gen[" + c + "], String)").mkString(", ")
        val tupleBusters = alpha.take(i).toUpperCase.map(c => "      val (gen" + c + ", name" + c + ") = genAndName" + c).mkString("\n")
        st.setAttribute("n", i)
        st.setAttribute("argType", argType)
        st.setAttribute("arbShrinks", arbShrinks)
        st.setAttribute("shrinks", shrinks)
        st.setAttribute("alphaLower", alphaLower)
        st.setAttribute("alphaUpper", alphaUpper)
        st.setAttribute("strings", strings)
        st.setAttribute("sumOfArgLengths", sumOfArgLengths)
        st.setAttribute("namesAndTypes", namesAndTypes)
        st.setAttribute("sumOfArgs", sumOfArgs)
        st.setAttribute("genArgs", genArgs)
        st.setAttribute("genArgsAndTypes", genArgsAndTypes)
        st.setAttribute("famousArgs", famousArgs)
        st.setAttribute("argNames", argNames)
        st.setAttribute("tupleBusters", tupleBusters)
        st.setAttribute("nameGenTuples", nameGenTuples)
        st.setAttribute("nameAndGenArgsAndTypes", nameAndGenArgsAndTypes)
        st.setAttribute("argNameNames", argNameNames)
        st.setAttribute("argNameNamesAndTypes", argNameNamesAndTypes)
        bw.write(st.toString)
      }
      bw.write("}\n")
      bw.write(generatorDrivenPropertyChecksCompanionObjectVerbatimString)
    }
    finally {
      bw.close()
    }
  }

  // Invitation style indicates how GeneratorDrivenPropertyChecks is imported
  def genGeneratorDrivenSuite(targetDir: File, mixinInvitationStyle: Boolean, withTables: Boolean, doItForCheckers: Boolean) {

    targetDir.mkdirs()
    
    val traitOrObjectName =
      if (doItForCheckers)
        "Checkers"
      else {
        if (withTables) "PropertyChecks" else "GeneratorDrivenPropertyChecks"
      }
    val suiteClassName = traitOrObjectName + (if (mixinInvitationStyle) "Mixin" else "Import") + "Suite" 
    val fileName = suiteClassName + ".scala" 

    val bw = new BufferedWriter(new FileWriter(new File(targetDir, fileName)))
 
    try {
      val st = new org.antlr.stringtemplate.StringTemplate(copyrightTemplate)
      st.setAttribute("year", thisYear);
      bw.write(st.toString)
      bw.write(generatorSuitePreamble)
      if (doItForCheckers) {
        bw.write("import org.scalacheck.Prop.{Exception => _, _}\n")
      }
      if (!mixinInvitationStyle)
        bw.write("import " + traitOrObjectName + "._\n")
      bw.write("\n")
      bw.write(
        "class " + suiteClassName + " extends Spec " +
        (if (mixinInvitationStyle) "with " + traitOrObjectName else "") + " {\n")
      bw.write(generatorSuitePostamble)
      val alpha = "abcdefghijklmnopqrstuv"
      for (i <- 1 to 6) {
        val st =
          if (doItForCheckers)
            new org.antlr.stringtemplate.StringTemplate(checkersSuiteTemplate)
          else
            new org.antlr.stringtemplate.StringTemplate(generatorSuiteTemplate)
        val rowOfOnes = List.fill(i)("  1").mkString(", ")
        val rowOfTwos = List.fill(i)("  2").mkString(", ")
        val listOfIs = List.fill(i)("i").mkString(", ")
        val columnsOfOnes = List.fill(i)("        (" + rowOfOnes + ")").mkString(",\n")
        val columnsOfTwos = List.fill(i)("        (" + rowOfTwos + ")").mkString(",\n")
        val rawRows =                              
          for (idx <- 0 to 9) yield                
            List.fill(i)("  " + idx).mkString("        (", ", ", ")")
        val columnsOfIndexes = rawRows.mkString(",\n")
        val argNames = alpha.take(i).map("\"" + _ + "\"").mkString(", ")
        //val argNames = alpha.map("\"" + _ + "\"").take(i).mkString(", ")
        val names = alpha.take(i).mkString(", ")
        val namesAndTypes = alpha.take(i).map(_ + ": String").mkString(", ")
        val sumOfArgs = alpha.take(i).mkString(" + ")
        val sumOfArgLengths = alpha.take(i).map(_ + ".length").mkString(" + ")
        val famousArgs = List.fill(i)("famousLastWords").mkString(", ")
        val sevenElevenArgs = List.fill(i)("sevenEleven").mkString(", ")
        val fiveFiveArgs = List.fill(i)("fiveFive").mkString(", ")
        val nameGenTuples = alpha.take(i).map("(famousLastWords, \"" + _ + "\")").mkString(", ")
        val fiveFiveNameGenTuples = alpha.take(i).map("(fiveFive, \"" + _ + "\")").mkString(", ")
        val sevenElevenNameGenTuples = alpha.take(i).map("(sevenEleven, \"" + _ + "\")").mkString(", ")
        val lengthAssertions = alpha.take(i).map("      assert(" + _ + ".length <= 5)").mkString("\n")
        val okayAssertions = alpha.take(i).map("        assert(" + _ + " === (\"OKAY\"))").mkString("\n")
        val lengthExpressions = alpha.take(i).map("      " + _ + ".length <= 5").mkString("\n")
        val okayExpressions = alpha.take(i).map("        " + _ + " == (\"OKAY\")").mkString("\n")
        st.setAttribute("n", i)
        st.setAttribute("columnsOfOnes", columnsOfOnes)
        st.setAttribute("columnsOfTwos", columnsOfTwos)
        st.setAttribute("columnsOfIndexes", columnsOfIndexes)
        st.setAttribute("argNames", argNames)
        st.setAttribute("names", names)
        st.setAttribute("namesAndTypes", namesAndTypes)
        st.setAttribute("sumOfArgs", sumOfArgs)
        st.setAttribute("sumOfArgLengths", sumOfArgLengths)
        st.setAttribute("listOfIs", listOfIs)
        st.setAttribute("famousArgs", famousArgs)
        st.setAttribute("sevenElevenArgs", sevenElevenArgs)
        st.setAttribute("fiveFiveArgs", fiveFiveArgs)
        st.setAttribute("nameGenTuples", nameGenTuples)
        st.setAttribute("fiveFiveNameGenTuples", fiveFiveNameGenTuples)
        st.setAttribute("sevenElevenNameGenTuples", sevenElevenNameGenTuples)
        st.setAttribute("lengthAssertions", lengthAssertions)
        st.setAttribute("okayAssertions", okayAssertions)
        st.setAttribute("lengthExpressions", lengthExpressions)
        st.setAttribute("okayExpressions", okayExpressions)
        bw.write(st.toString)
      }

      bw.write("}\n")
    }
    finally {
      bw.close()
    }
  }
  
  def main(args: Array[String]) {
    val targetDir = args(0)
    val version = args(1)
    val scalaVersion = args(2)
    val mainDir = new File(targetDir + "/main/scala/org/scalatest/prop")
    mainDir.mkdirs()
    genMain(mainDir, version, scalaVersion)
    
    val testDir = new File("gentests/" + targetDir + "/test/scala/org/scalatest/prop")
    testDir.mkdirs()
    genTest(testDir, version, scalaVersion)
  }
  
  def genMain(dir: File, version: String, scalaVersion: String) {
    genPropertyChecks(dir)
  }
  
  def genTest(dir: File, version: String, scalaVersion: String) {
    genGeneratorDrivenSuite(dir, true, false, false)
    genGeneratorDrivenSuite(dir, false, false, false)
    genGeneratorDrivenSuite(dir, true, true, false)
    genGeneratorDrivenSuite(dir, false, true, false)
    genGeneratorDrivenSuite(dir, true, true, true)
    genGeneratorDrivenSuite(dir, false, true, true)
  }
}

