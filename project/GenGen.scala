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

  val generatorSource = new File("GenGen.scala")

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
package org.scalatest.prop

import org.scalactic.anyvals.PosZInt
import org.scalactic.ColCompatHelper.LazyListOrStream

"""

  val propertyCheckPreamble = """
import org.scalactic._
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.exceptions.StackDepthException
import scala.annotation.tailrec
import scala.util.{Try, Failure, Success}
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException
import org.scalatest.exceptions.StackDepth
import org.scalatest.exceptions.TestFailedException
import org.scalatest.enablers.PropCheckerAsserting

/**
 * Trait containing methods that faciliate property checks against generated data using [[Generator]].
 *
 * <p>
 * This trait contains <code>forAll</code> methods that provide various ways to check properties using
 * generated data.  It also contains a <code>wherever</code> method that can be used to indicate a property
 * need only hold whenever some condition is true.
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
 * that allow you to check properties using the data provided by [[Generator]]. The simplest form
 * of <code>forAll</code> method takes two parameter lists, the second of which is implicit. The first parameter list
 * is a "property" function with one to six parameters. An implicit [[Generator]] generator object needs to be supplied for.
 * The <code>forAll</code> method will pass each row of data to each parameter type. ScalaTest provides many implicit [[Generator]]s for
 * common types such as <code>Int</code>, <code>String</code>, <code>List[Float]</code>, <em>etc.</em>, in its [[Generator]] companion
 * object. So long as you use types for which ScalaTest already provides implicit [[Generator]]s, you needn't
 * worry about them. Most often you can simply pass a property function to <code>forAll</code>, and the compiler will grab the implicit
 * values provided by ScalaTest.
 * </p>
 *
 * <p>
 * The <code>forAll</code> methods use the supplied [[Generator]]s to generate example
 * arguments and pass them to the property function, and
 * generate a <code>GeneratorDrivenPropertyCheckFailedException</code> if the function
 * completes abruptly for any exception that would <a href="../Suite.html#errorHandling">normally cause</a> a test to
 * fail in ScalaTest other than <code>DiscardedEvaluationException</code>. An
 * <code>DiscardedEvaluationException</code>,
 * which is thrown by the <code>whenever</code> method (defined in trait [[Whenever]], which this trait extends) to indicate
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
 * ScalaTest provides a nice library of compositors that makes it easy to create your own custom generators. If you
 * want to supply custom generators to a property check, place them in parentheses after <code>forAll</code>, before
 * the property check function (a curried form of <code>forAll</code>).
 * </p>
 *
 * <p>
 * For example, to create a generator of even integers between (and including) -2000 and 2000, you could write this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.prop.Generator
 *
 * val evenInts = for (n <- Generator.chooseInt(-1000, 1000)) yield 2 * n
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
 * Custom generators are necessary when you want to pass data types not supported by ScalaTest's [[Generator]]s,
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
 *   for (n <- Generator.chooseInt(Integer.MIN_VALUE + 1, Integer.MAX_VALUE)) yield n
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
 * sizeRange
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * 100
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * the size range parameter to provide to ScalaCheck, which it will use when generating objects for which size matters (such as strings or lists)
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
 * you want all parameters at their defaults except for <code>minSize</code> and <code>sizeRange</code>, you can override
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
 * forAll ("n", "d", minSuccessful(500), maxDiscardedFactor(0.6)) {
 *   (n: Int, d: Int) => ...
 *
 * // If providing generators
 * forAll (validNumers, validDenoms, minSuccessful(500), maxDiscardedFactor(0.6)) {
 *   (n: Int, d: Int) => ...
 *
 * // If providing (&lt;generators&gt;, &lt;name&gt;) pairs
 * forAll ((validNumers, "n"), (validDenoms, "d"), minSuccessful(500), maxDiscardedFactor(0.6)) {
 *   (n: Int, d: Int) => ...
 * </pre>
 *
 * <p>
 * For more information, see the documentation for supertrait <a href="Configuration.html"><code>Configuration</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
trait GeneratorDrivenPropertyChecks extends CommonGenerators with Whenever with Configuration {

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
   * forAll (minSize(1), sizeRange(9)) { (a: String) =>
   *   a.length should equal ((a).length)
   * }
   *
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String) =>
   *   a.length + b.length should equal ((a + b).length)
   * }
   *
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String) =>
   *   a.length + b.length + c.length should equal ((a + b + c).length)
   * }
   *
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String, d: String) =>
   *   a.length + b.length + c.length + d.length should equal ((a + b + c + d).length)
   * }
   *
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String, d: String, e: String) =>
   *   a.length + b.length + c.length + d.length + e.length should equal ((a + b + c + d + e).length)
   * }
   *
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String, d: String, e: String, f: String) =>
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
   * forAll (minSize(1), sizeRange(9)) { (a: String) =>
   *   a.length should equal ((a).length)
   * }
   *
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String) =>
   *   a.length + b.length should equal ((a + b).length)
   * }
   *
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String) =>
   *   a.length + b.length + c.length should equal ((a + b + c).length)
   * }
   *
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String, d: String) =>
   *   a.length + b.length + c.length + d.length should equal ((a + b + c + d).length)
   * }
   *
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String, d: String, e: String) =>
   *   a.length + b.length + c.length + d.length + e.length should equal ((a + b + c + d + e).length)
   * }
   *
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String, d: String, e: String, f: String) =>
   *   a.length + b.length + c.length + d.length + e.length + f.length should equal ((a + b + c + d + e + f).length)
   * }
   * </pre>
   *
   * <p>
   * In the first example above, the <code>ConfiguredPropertyCheck</code> object is returned by:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), sizeRange(9))
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
   * <code>PropertyCheckConfiguration</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), sizeRange(9)) { (a: String) =>
   *   a.length should equal ((a).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A, ASSERTION](fun: (A) => ASSERTION)
      (implicit
        config: PropertyCheckConfiguration,
        genA: org.scalatest.prop.Generator[A],
        asserting: PropCheckerAsserting[ASSERTION],
        prettifier: Prettifier,
        pos: source.Position
      ): asserting.Result = {
      val param = getParameter(configParams, config)
      asserting.check1(fun, genA, param, prettifier, pos, List.empty)
    }

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed
   * <code>PropertyCheckConfiguration</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String) =>
   *   a.length + b.length should equal ((a + b).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A, B, ASSERTION](fun: (A, B) => ASSERTION)
      (implicit
        config: PropertyCheckConfiguration,
        genA: org.scalatest.prop.Generator[A],
        genB: org.scalatest.prop.Generator[B],
        asserting: PropCheckerAsserting[ASSERTION],
        prettifier: Prettifier,
        pos: source.Position
      ): asserting.Result = {
      val param = getParameter(configParams, config)
      asserting.check2(fun, genA, genB, param, prettifier, pos, List.empty)
    }

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed
   * <code>PropertyCheckConfiguration</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String) =>
   *   a.length + b.length + c.length should equal ((a + b + c).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A, B, C, ASSERTION](fun: (A, B, C) => ASSERTION)
      (implicit
        config: PropertyCheckConfiguration,
        genA: org.scalatest.prop.Generator[A],
        genB: org.scalatest.prop.Generator[B],
        genC: org.scalatest.prop.Generator[C],
        asserting: PropCheckerAsserting[ASSERTION],
        prettifier: Prettifier,
        pos: source.Position
      ): asserting.Result = {
      val param = getParameter(configParams, config)
      asserting.check3(fun, genA, genB, genC, param, prettifier, pos, List.empty)
    }

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed
   * <code>PropertyCheckConfiguration</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String, d: String) =>
   *   a.length + b.length + c.length + d.length should equal ((a + b + c + d).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A, B, C, D, ASSERTION](fun: (A, B, C, D) => ASSERTION)
      (implicit
        config: PropertyCheckConfiguration,
        genA: org.scalatest.prop.Generator[A],
        genB: org.scalatest.prop.Generator[B],
        genC: org.scalatest.prop.Generator[C],
        genD: org.scalatest.prop.Generator[D],
        asserting: PropCheckerAsserting[ASSERTION],
        prettifier: Prettifier,
        pos: source.Position
      ): asserting.Result = {
      val param = getParameter(configParams, config)
      asserting.check4(fun, genA, genB, genC, genD, param, prettifier, pos, List.empty)
    }

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed
   * <code>PropertyCheckConfiguration</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String, d: String, e: String) =>
   *   a.length + b.length + c.length + d.length + e.length should equal ((a + b + c + d + e).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A, B, C, D, E, ASSERTION](fun: (A, B, C, D, E) => ASSERTION)
      (implicit
        config: PropertyCheckConfiguration,
        genA: org.scalatest.prop.Generator[A],
        genB: org.scalatest.prop.Generator[B],
        genC: org.scalatest.prop.Generator[C],
        genD: org.scalatest.prop.Generator[D],
        genE: org.scalatest.prop.Generator[E],
        asserting: PropCheckerAsserting[ASSERTION],
        prettifier: Prettifier,
        pos: source.Position
      ): asserting.Result = {
      val param = getParameter(configParams, config)
      asserting.check5(fun, genA, genB, genC, genD, genE, param, prettifier, pos, List.empty)
    }

  /**
   * Performs a property check by applying the specified property check function to arguments
   * supplied by implicitly passed generators, modifying the values in the implicitly passed
   * <code>PropertyCheckConfiguration</code> object with parameter values passed to this object's constructor.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll (minSize(1), sizeRange(9)) { (a: String, b: String, c: String, d: String, e: String, f: String) =>
   *   a.length + b.length + c.length + d.length + e.length + f.length should equal ((a + b + c + d + e + f).length)
   * }
   * </pre>
   *
   * @param fun the property check function to apply to the generated arguments
   */
    def apply[A, B, C, D, E, F, ASSERTION](fun: (A, B, C, D, E, F) => ASSERTION)
      (implicit
        config: PropertyCheckConfiguration,
        genA: org.scalatest.prop.Generator[A],
        genB: org.scalatest.prop.Generator[B],
        genC: org.scalatest.prop.Generator[C],
        genD: org.scalatest.prop.Generator[D],
        genE: org.scalatest.prop.Generator[E],
        genF: org.scalatest.prop.Generator[F],
        asserting: PropCheckerAsserting[ASSERTION],
        prettifier: Prettifier,
        pos: source.Position
      ): asserting.Result = {
      val param = getParameter(configParams, config)
      asserting.check6(fun, genA, genB, genC, genD, genE, genF, param, prettifier, pos, List.empty)
    }
  }

"""

  val propertyCheckForAllTemplate = """
  def forAll[$alphaUpper$, ASSERTION](fun: ($alphaUpper$) => ASSERTION)
  (implicit
    config: PropertyCheckConfiguration,
    $gens$,
    prettifier: Prettifier,
    pos: source.Position,
    asserting: PropCheckerAsserting[ASSERTION]
  ): asserting.Result =
    asserting.check$n$(fun, $genRefs$, getParameter(List.empty, config), prettifier, pos, List.empty)

  def forAll[$alphaUpper$, ASSERTION]($namesAndTypes$)(fun: ($alphaUpper$) => ASSERTION)
    (implicit
      config: PropertyCheckConfiguration,
$gens$,
      prettifier: Prettifier,
      pos: source.Position,
      asserting: PropCheckerAsserting[ASSERTION]
    ): asserting.Result =
      asserting.check$n$(fun, $genRefs$, getParameter(List.empty, config), prettifier, pos, List($alphaLower$))

  def forAll[$alphaUpper$, ASSERTION]($namesAndTypes$, configParams: PropertyCheckConfigParam*)(fun: ($alphaUpper$) => ASSERTION)
    (implicit
      config: PropertyCheckConfiguration,
$gens$,
      prettifier: Prettifier,
      pos: source.Position,
      asserting: PropCheckerAsserting[ASSERTION]
    ): asserting.Result =
      asserting.check$n$(fun, $genRefs$, getParameter(configParams, config), prettifier, pos, List($alphaLower$))

  def forAll[$alphaUpper$, ASSERTION]($gens$)(fun: ($alphaUpper$) => ASSERTION)
    (implicit
      config: PropertyCheckConfiguration,
      prettifier: Prettifier,
      pos: source.Position,
      asserting: PropCheckerAsserting[ASSERTION]
    ): asserting.Result =
    asserting.check$n$(fun, $genRefs$, getParameter(List.empty, config), prettifier, pos, List.empty)

  def forAll[$alphaUpper$, ASSERTION]($gens$, configParams: PropertyCheckConfigParam*)(fun: ($alphaUpper$) => ASSERTION)
    (implicit
      config: PropertyCheckConfiguration,
      prettifier: Prettifier,
      pos: source.Position,
      asserting: PropCheckerAsserting[ASSERTION]
    ): asserting.Result =
    asserting.check$n$(fun, $genRefs$, getParameter(configParams, config), prettifier, pos, List.empty)

  def forAll[$alphaUpper$, ASSERTION]($gensAndNames$)(fun: ($alphaUpper$) => ASSERTION)
    (implicit
      config: PropertyCheckConfiguration,
      prettifier: Prettifier,
      pos: source.Position,
      asserting: PropCheckerAsserting[ASSERTION]
    ): asserting.Result = {
    $tupleBusters$
    asserting.check$n$(fun, $genRefs$, getParameter(List.empty, config), prettifier, pos, List($argNameNames$))
  }

  def forAll[$alphaUpper$, ASSERTION]($gensAndNames$, configParams: PropertyCheckConfigParam*)(fun: ($alphaUpper$) => ASSERTION)
    (implicit
      config: PropertyCheckConfiguration,
      prettifier: Prettifier,
      pos: source.Position,
      asserting: PropCheckerAsserting[ASSERTION]
    ): asserting.Result = {
      $tupleBusters$
      asserting.check$n$(fun, $genRefs$, getParameter(configParams, config), prettifier, pos, List($argNameNames$))
    }

"""

  val generatorDrivenPropertyChecksCompanionObjectVerbatimString = """

object GeneratorDrivenPropertyChecks extends GeneratorDrivenPropertyChecks
"""

  val generatorSuitePreamble = """

import org.scalatest._
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException
"""

  val generatorSuitePostamble = """
  val famousLastWords = for {
    s <- org.scalatest.prop.CommonGenerators.specificValues("the", "program", "compiles", "therefore", "it", "should", "work")
  } yield s

  val sevenEleven: Generator[String] =
    new Generator[String] {
      def next(szp: SizeParam, edges: List[String], rnd: Randomizer): (RoseTree[String], List[String], Randomizer) = {
        if (szp.size.value >= 7 && szp.size.value <= 11)
          (Rose("OKAY"), edges, rnd)
        else
          throw new Exception("expected 7 <= size <= 11 but got " + szp.size)
      }
    }


  val fiveFive: Generator[String] =
    new Generator[String] {
      def next(szp: SizeParam, edges: List[String], rnd: Randomizer): (RoseTree[String], List[String], Randomizer) = {
        if (szp.size.value == 5)
          (Rose("OKAY"), edges, rnd)
        else
          throw new Exception("expected size 5 but got " + szp.size)
      }
    }
"""

  val generatorSuiteAssertTemplate = """

  it("generator-driven property that takes $n$ args, which succeeds") {

    forAll { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  it("generator-driven property that takes $n$ args, which fails") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which succeeds") {

    forAll ($argNames$) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  it("generator-driven property that takes $n$ named args, which fails") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($argNames$) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds") {

    forAll ($famousArgs$) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds") {

    forAll ($nameGenTuples$) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  // Same thing, but with config params
  it("generator-driven property that takes $n$ args, which succeeds, with config params") {

    forAll (minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  it("generator-driven property that takes $n$ args, which fails, with config params") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll (minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with config params") {

    forAll ($argNames$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  it("generator-driven property that takes $n$ named args, which fails, with config params") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($argNames$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with config params") {

    forAll ($famousArgs$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails, with config params") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($famousArgs$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with config params") {

    forAll ($nameGenTuples$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
      assert($sumOfArgLengths$ === (($sumOfArgs$).length))
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails, with config params") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($nameGenTuples$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
      }
    }
  }

  // Same thing, but set minSuccessful to 5 with param, prop fails after 5
  it("generator-driven property that takes $n$ args, which succeeds, with minSuccessful param set to 5") {

    var i = 0
    forAll (minSuccessful(5)) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  it("generator-driven property that takes $n$ args, which fails, with minSuccessful param set to 5") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll (minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with minSuccessful param set to 5") {

    var i = 0
    forAll ($argNames$, minSuccessful(5)) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  it("generator-driven property that takes $n$ named args, which fails, with minSuccessful param set to 5") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with minSuccessful param set to 5") {

    var i = 0
    forAll ($famousArgs$, minSuccessful(5)) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails, with minSuccessful param set to 5") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with minSuccessful param set to 5") {

    var i = 0
    forAll ($nameGenTuples$, minSuccessful(5)) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails, with minSuccessful param set to 5") {

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
      }
    }
  }

  // Same thing, but set default minSuccessful to 5, prop fails after 5
  it("generator-driven property that takes $n$ args, which succeeds, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  it("generator-driven property that takes $n$ args, which fails, with default minSuccessful param set to 5") {

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

  it("generator-driven property that takes $n$ named args, which succeeds, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll ($argNames$) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  it("generator-driven property that takes $n$ named args, which fails, with default minSuccessful param set to 5") {

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

  it("generator-driven property that takes $n$ args and generators, which succeeds, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll ($famousArgs$) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails, with default minSuccessful param set to 5") {

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

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll ($nameGenTuples$) { ($namesAndTypes$) =>
      i += 1
      assert(i != 6)
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails, with default minSuccessful param set to 5") {

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

  // Same thing, but set maxDiscardedFactor to 0.5 with param, prop fails after 5
  it("generator-driven property that takes $n$ args, which succeeds, with maxDiscardedFactor param set to 0.6") {

    var i = 0
    forAll (maxDiscardedFactor(0.6)) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  it("generator-driven property that takes $n$ args, which fails, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll (maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with maxDiscardedFactor param set to 0.6") {

    var i = 0
    forAll ($argNames$, maxDiscardedFactor(0.6)) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with maxDiscardedFactor param set to 0.6") {

    var i = 0
    forAll ($famousArgs$, maxDiscardedFactor(0.6)) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with maxDiscardedFactor param set to 0.6") {

    var i = 0
    forAll ($nameGenTuples$, maxDiscardedFactor(0.6)) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
      }
    }
  }

  // Same thing, but set default maxDiscardedFactor to 0.5, prop fails after 5
  it("generator-driven property that takes $n$ args, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    forAll { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  it("generator-driven property that takes $n$ args, which fails, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    forAll ($argNames$) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    forAll ($famousArgs$) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    forAll ($nameGenTuples$) { ($namesAndTypes$) =>
      i += 1
      whenever (i > 5) { assert(1 + 1 === (2)) }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    intercept[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
      }
    }
  }

  // set sizeRange with param (ensure always passed with a size less within sizeRange)
  it("generator-driven property that takes $n$ args, with sizeRange specified as param") {

    forAll (sizeRange(5)) { ($namesAndTypes$) =>
$lengthAssertions$
    }
  }

  it("generator-driven property that takes $n$ named args, with sizeRange specified as param") {

    forAll ($argNames$, sizeRange(5)) { ($namesAndTypes$) =>
$lengthAssertions$
    }
  }

  // set sizeRange with default (ensure always passed with a size less than sizeRange)
  it("generator-driven property that takes $n$ args, with sizeRange specified as default") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 5)

    forAll { ($namesAndTypes$) =>
$lengthAssertions$
    }
  }

  it("generator-driven property that takes $n$ named args, with sizeRange specified as default") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 5)

    forAll ($argNames$) { ($namesAndTypes$) =>
$lengthAssertions$
    }
  }

  // set sizeRange = 0 with (param, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange = 0, specified as (param, param)") {

    forAll ($fiveFiveArgs$, minSize(5), sizeRange(0)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (param, param)") {

    forAll ($fiveFiveNameGenTuples$, minSize(5), sizeRange(0)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set sizeRange = 0 with (param, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange = 0, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 0)

    forAll ($fiveFiveArgs$, minSize(5)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 0)

    forAll ($fiveFiveNameGenTuples$, minSize(5)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set sizeRange = 0 with (default, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange = 0, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    forAll ($fiveFiveArgs$, sizeRange(0)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    forAll ($fiveFiveNameGenTuples$, sizeRange(0)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set sizeRange = 0 with (default, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange = 0, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 5, sizeRange = 0)

    forAll ($fiveFiveArgs$) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 5, sizeRange = 0)

    forAll ($fiveFiveNameGenTuples$) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set minSize to 7 and sizeRange to 4 with (param, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (param, param)") {

    forAll ($sevenElevenArgs$, minSize(7), sizeRange(4)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (param, param)") {

    forAll ($sevenElevenNameGenTuples$, minSize(7), sizeRange(4)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set minSize to 7 and sizeRange to 4 with (param, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 4)

    forAll ($sevenElevenArgs$, minSize(7)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 4)

    forAll ($sevenElevenNameGenTuples$, minSize(7)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set minSize to 7 and sizeRange to 4 with (default, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 7)

    forAll ($sevenElevenArgs$, sizeRange(4)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 7)

    forAll ($sevenElevenNameGenTuples$, sizeRange(4)) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  // set minSize to 7 and sizeRange to 4 with (default, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 7, sizeRange = 4)

    forAll ($sevenElevenArgs$) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 7, sizeRange = 4)

    forAll ($sevenElevenNameGenTuples$) { ($namesAndTypes$) =>
$okayAssertions$
    }
  }
"""

  val generatorSuiteFutureAssertTemplate = """

  it("generator-driven property that takes $n$ args, which succeeds") {
    forAll { ($namesAndTypes$) =>
      Future {
        assert($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails in future block") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll { ($namesAndTypes$) =>
        Future {
          assert($sumOfArgLengths$ < 0)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails before future block") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which succeeds") {
    forAll ($argNames$) { ($namesAndTypes$) =>
      Future {
        assert($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails in future block") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($argNames$) { ($namesAndTypes$) =>
        Future {
          assert($sumOfArgLengths$ < 0)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails before future block") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($argNames$) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds") {
    forAll ($famousArgs$) { ($namesAndTypes$) =>
      Future {
        assert($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails in future block") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        Future {
          assert($sumOfArgLengths$ < 0)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails before future block") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds") {
    forAll ($nameGenTuples$) { ($namesAndTypes$) =>
      Future {
        assert($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails in future block") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        Future {
          assert($sumOfArgLengths$ < 0)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails before future block") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
        Future {
          assert(true)
        }
      }
    }
  }

  // Same thing, but with config params
  it("generator-driven property that takes $n$ args, which succeeds, with config params") {
    forAll (minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
      Future {
        assert($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails in future block, with config params") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll (minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        Future {
          assert($sumOfArgLengths$ < 0)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails before future block, with config params") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll (minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with config params") {
    forAll ($argNames$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
      Future {
        assert($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails in future block, with config params") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($argNames$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        Future {
          assert($sumOfArgLengths$ < 0)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails before future block, with config params") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($argNames$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with config params") {
    forAll ($famousArgs$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
      Future {
        assert($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails in future block, with config params") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($famousArgs$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        Future {
          assert($sumOfArgLengths$ < 0)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails before future block, with config params") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($famousArgs$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with config params") {
    forAll ($nameGenTuples$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
      Future {
        assert($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails in future block, with config params") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($nameGenTuples$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        Future {
          assert($sumOfArgLengths$ < 0)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails before future block, with config params") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      forAll ($nameGenTuples$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        assert($sumOfArgLengths$ < 0)
        Future {
          assert(true)
        }
      }
    }
  }

  // Same thing, but set minSuccessful to 5 with param, prop fails after 5
  it("generator-driven property that takes $n$ args, which succeeds, with minSuccessful param set to 5") {
    var i = 0
    forAll (minSuccessful(5)) { ($namesAndTypes$) =>
      Future {
        i += 1
        assert(i != 6)
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails in future block, with minSuccessful param set to 5") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll (minSuccessful(5)) { ($namesAndTypes$) =>
        Future {
          i += 1
          assert(i != 5)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails before future block, with minSuccessful param set to 5") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll (minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with minSuccessful param set to 5") {
    var i = 0
    forAll ($argNames$, minSuccessful(5)) { ($namesAndTypes$) =>
      Future {
        i += 1
        assert(i != 6)
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails in future block, with minSuccessful param set to 5") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$, minSuccessful(5)) { ($namesAndTypes$) =>
        Future {
          i += 1
          assert(i != 5)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails before future block, with minSuccessful param set to 5") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with minSuccessful param set to 5") {
    var i = 0
    forAll ($famousArgs$, minSuccessful(5)) { ($namesAndTypes$) =>
      Future {
        i += 1
        assert(i != 6)
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails in future block, with minSuccessful param set to 5") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$, minSuccessful(5)) { ($namesAndTypes$) =>
        Future {
          i += 1
          assert(i != 5)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails before future block, with minSuccessful param set to 5") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with minSuccessful param set to 5") {
    var i = 0
    forAll ($nameGenTuples$, minSuccessful(5)) { ($namesAndTypes$) =>
      Future {
        i += 1
        assert(i != 6)
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails in future block, with minSuccessful param set to 5") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$, minSuccessful(5)) { ($namesAndTypes$) =>
        Future {
          i += 1
          assert(i != 5)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails before future block, with minSuccessful param set to 5") {
    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
        Future {
          assert(true)
        }
      }
    }
  }

  // Same thing, but set default minSuccessful to 5, prop fails after 5
  it("generator-driven property that takes $n$ args, which succeeds, with default minSuccessful param set to 5") {
    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll { ($namesAndTypes$) =>
      Future {
        i += 1
        assert(i != 6)
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails in future block, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll { ($namesAndTypes$) =>
        Future {
          i += 1
          assert(i != 5)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails before future block, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll ($argNames$) { ($namesAndTypes$) =>
      Future {
        i += 1
        assert(i != 6)
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails in future block, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll ($famousArgs$) { ($namesAndTypes$) =>
      Future {
        i += 1
        assert(i != 6)
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails in future block, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        Future {
          i += 1
          assert(i != 5)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails before future block, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
        Future {
          assert(true)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    forAll ($nameGenTuples$) { ($namesAndTypes$) =>
      Future {
        i += 1
        assert(i != 6)
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails in future block, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        Future {
          i += 1
          assert(i != 5)
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails before future block, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        i += 1
        assert(i != 5)
        Future {
          assert(true)
        }
      }
    }
  }

  // Same thing, but set maxDiscarded to 5 with param, prop fails after 5
  it("generator-driven property that takes $n$ args, which succeeds, with maxDiscardedFactor param set to 0.6") {

    var i = 0
    forAll (maxDiscardedFactor(0.6)) { ($namesAndTypes$) =>
      Future {
        i += 1
        whenever (i > 5) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails in future block, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll (maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        Future {
          i += 1
          whenever (i > 7) { assert(1 + 1 === (2)) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails before future block, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll (maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
        Future {
          whenever (i > 7) { assert(true) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with maxDiscardedFactor param set to 0.6") {
    var i = 0
    forAll ($argNames$, maxDiscardedFactor(0.6)) { ($namesAndTypes$) =>
      Future {
        i += 1
        whenever (i > 5) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails in future block, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        Future {
          i += 1
          whenever (i > 7) { assert(1 + 1 === (2)) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails before future block, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
        Future {
          whenever (i > 7) { assert(true) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with maxDiscardedFactor param set to 0.6") {

    var i = 0
    forAll ($famousArgs$, maxDiscardedFactor(0.6)) { ($namesAndTypes$) =>
      Future {
        i += 1
        whenever (i > 5) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails in future block, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        Future {
          i += 1
          whenever (i > 7) { assert(1 + 1 === (2)) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails before future block, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
        Future {
          whenever (i > 7) { assert(true) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with maxDiscardedFactor param set to 0.6") {

    var i = 0
    forAll ($nameGenTuples$, maxDiscardedFactor(0.6)) { ($namesAndTypes$) =>
      Future {
        i += 1
        whenever (i > 5) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails in future block, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        Future {
          i += 1
          whenever (i > 7) { assert(1 + 1 === (2)) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails before future block, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
        Future {
          whenever (i > 7) { assert(true) }
        }
      }
    }
  }

  // Same thing, but set default maxDiscardedFactor to 0.6, prop fails after 5
  it("generator-driven property that takes $n$ args, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    forAll { ($namesAndTypes$) =>
      Future {
        i += 1
        whenever (i > 5) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails in future block, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll { ($namesAndTypes$) =>
        Future {
          i += 1
          whenever (i > 7) { assert(1 + 1 === (2)) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args, which fails before future block, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
        Future {
          whenever (i > 7) { assert(true) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    forAll ($argNames$) { ($namesAndTypes$) =>
      Future {
        i += 1
        whenever (i > 5) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails in future block, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$) { ($namesAndTypes$) =>
        Future {
          i += 1
          whenever (i > 7) { assert(1 + 1 === (2)) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args, which fails before future block, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($argNames$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
        Future {
          whenever (i > 7) { assert(true) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    forAll ($famousArgs$) { ($namesAndTypes$) =>
      Future {
        i += 1
        whenever (i > 5) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails in future block, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        Future {
          i += 1
          whenever (i > 7) { assert(1 + 1 === (2)) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ args and generators, which fails before future block, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
        Future {
          whenever (i > 7) { assert(true) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    forAll ($nameGenTuples$) { ($namesAndTypes$) =>
      Future {
        i += 1
        whenever (i > 5) { assert(1 + 1 === (2)) }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails in future block, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        Future {
          i += 1
          whenever (i > 7) { assert(1 + 1 === (2)) }
        }
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, which fails before future block, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    recoverToSucceededIf[GeneratorDrivenPropertyCheckFailedException] {
      var i = 0
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { assert(1 + 1 === (2)) }
        Future {
          whenever (i > 7) { assert(true) }
        }
      }
    }
  }

  // set sizeRange with param (ensure always passed within a size range)
  it("generator-driven property that takes $n$ args, with sizeRange specified as param") {

    forAll (sizeRange(5)) { ($namesAndTypes$) =>
      Future {
$lengthAssertions$
      }
    }
  }

  it("generator-driven property that takes $n$ named args, with sizeRange specified as param") {

    forAll ($argNames$, sizeRange(5)) { ($namesAndTypes$) =>
      Future {
$lengthAssertions$
      }
    }
  }

  // set sizeRange with default (ensure always passed with a size less than sizeRange)
  it("generator-driven property that takes $n$ args, with sizeRange specified as default") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 5)

    forAll { ($namesAndTypes$) =>
      Future {
$lengthAssertions$
      }
    }
  }

  it("generator-driven property that takes $n$ named args, with sizeRange specified as default") {
    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 5)

    forAll ($argNames$) { ($namesAndTypes$) =>
      Future {
$lengthAssertions$
      }
    }
  }

  // set sizeRange = 0 with (param, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange = 0, specified as (param, param)") {
    forAll ($fiveFiveArgs$, minSize(5), sizeRange(0)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (param, param)") {
    forAll ($fiveFiveNameGenTuples$, minSize(5), sizeRange(0)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  // set sizeRange = 0 with (param, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange = 0, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 0)

    forAll ($fiveFiveArgs$, minSize(5)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 0)

    forAll ($fiveFiveNameGenTuples$, minSize(5)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  // set sizeRange = 0 with (default, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange = 0, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    forAll ($fiveFiveArgs$, sizeRange(0)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    forAll ($fiveFiveNameGenTuples$, sizeRange(0)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  // set sizeRange = 0 with (default, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange = 0, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 5, sizeRange = 0)

    forAll ($fiveFiveArgs$) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 5, sizeRange = 0)

    forAll ($fiveFiveNameGenTuples$) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  // set minSize to 7 and sizeRange to 4 with (param, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (param, param)") {

    forAll ($sevenElevenArgs$, minSize(7), sizeRange(4)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (param, param)") {

    forAll ($sevenElevenNameGenTuples$, minSize(7), sizeRange(4)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  // set minSize to 7 and sizeRange to 4 with (param, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 4)

    forAll ($sevenElevenArgs$, minSize(7)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 4)

    forAll ($sevenElevenNameGenTuples$, minSize(7)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  // set minSize to 7 and sizeRange to 4 with (default, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 7)

    forAll ($sevenElevenArgs$, sizeRange(4)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 7)

    forAll ($sevenElevenNameGenTuples$, sizeRange(4)) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  // set minSize to 7 and sizeRange to 4 with (default, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 7, sizeRange = 4)

    forAll ($sevenElevenArgs$) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 7, sizeRange = 4)

    forAll ($sevenElevenNameGenTuples$) { ($namesAndTypes$) =>
      Future {
$okayAssertions$
      }
    }
  }
"""

  val generatorSuiteExpectTemplate = """

  it("generator-driven property that takes $n$ args, which succeeds") {
    val result =
      forAll { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args, which fails") {
    val result =
      forAll { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ < 0)
      }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args, which succeeds") {
    val result =
      forAll ($argNames$) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args, which fails") {
    val result =
      forAll ($argNames$) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ < 0)
      }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds") {
    val result =
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args and generators, which fails") {
    val result =
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ < 0)
      }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds") {
    val result =
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, which fails") {
    val result =
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ < 0)
      }
    assert(result.isNo)
  }

  // Same thing, but with config params
  it("generator-driven property that takes $n$ args, which succeeds, with config params") {
    val result =
      forAll (minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args, which fails, with config params") {
    val result =
      forAll (minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ < 0)
      }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with config params") {
    val result =
      forAll ($argNames$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args, which fails, with config params") {
    val result =
      forAll ($argNames$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ < 0)
      }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with config params") {
    val result =
      forAll ($famousArgs$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args and generators, which fails, with config params") {
    val result =
      forAll ($famousArgs$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ < 0)
      }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with config params") {
    val result =
      forAll ($nameGenTuples$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ === (($sumOfArgs$).length))
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, which fails, with config params") {
    val result =
      forAll ($nameGenTuples$, minSize(10), sizeRange(10)) { ($namesAndTypes$) =>
        expect($sumOfArgLengths$ < 0)
      }
    assert(result.isNo)
  }

  // Same thing, but set minSuccessful to 5 with param, prop fails after 5
  it("generator-driven property that takes $n$ args, which succeeds, with minSuccessful param set to 5") {
    var i = 0
    val result =
      forAll (minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        expect(i != 6)
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args, which fails, with minSuccessful param set to 5") {
    var i = 0
    val result =
      forAll (minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        expect(i != 5)
      }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with minSuccessful param set to 5") {
    var i = 0
    val result =
      forAll ($argNames$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        expect(i != 6)
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args, which fails, with minSuccessful param set to 5") {
    var i = 0
    val result =
      forAll ($argNames$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        expect(i != 5)
      }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with minSuccessful param set to 5") {
    var i = 0
    val result =
      forAll ($famousArgs$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        expect(i != 6)
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args and generators, which fails, with minSuccessful param set to 5") {
    var i = 0
    val result =
      forAll ($famousArgs$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        expect(i != 5)
      }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with minSuccessful param set to 5") {
    var i = 0
    val result =
      forAll ($nameGenTuples$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        expect(i != 6)
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, which fails, with minSuccessful param set to 5") {
    var i = 0
    val result =
      forAll ($nameGenTuples$, minSuccessful(5)) { ($namesAndTypes$) =>
        i += 1
        expect(i != 5)
      }
    assert(result.isNo)
  }

  // Same thing, but set default minSuccessful to 5, prop fails after 5
  it("generator-driven property that takes $n$ args, which succeeds, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    val result =
      forAll { ($namesAndTypes$) =>
        i += 1
        expect(i != 6)
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args, which fails, with default minSuccessful param set to 5") {
    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)
    var i = 0
    val result =
      forAll { ($namesAndTypes$) =>
        i += 1
        expect(i != 5)
      }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    val result =
      forAll ($argNames$) { ($namesAndTypes$) =>
        i += 1
        expect(i != 6)
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args, which fails, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    val result = {
      var i = 0
      forAll ($argNames$) { ($namesAndTypes$) =>
        i += 1
        expect(i != 5)
      }
    }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    val result =
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        i += 1
        expect(i != 6)
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args and generators, which fails, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    val result = {
      var i = 0
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        i += 1
        expect(i != 5)
      }
    }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    var i = 0
    val result =
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        i += 1
        expect(i != 6)
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, which fails, with default minSuccessful param set to 5") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    val result = {
      var i = 0
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        i += 1
        expect(i != 5)
      }
    }
    assert(result.isNo)
  }

  // Same thing, but set maxDiscarded to 5 with param, prop fails after 5
  it("generator-driven property that takes $n$ args, which succeeds, with maxDiscarded param set to 5") {

    var i = 0
    val result =
      forAll (maxDiscardedFactor(5)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 5) { expect(1 + 1 === (2)) }
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args, which fails, with maxDiscarded param set to 5") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    val result = {
      var i = 0
      forAll (maxDiscardedFactor(5)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { expect(1 + 1 === (2)) }
      }
    }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with maxDiscardedFactor param set to 0.6") {

    var i = 0
    val result =
      forAll ($argNames$, maxDiscardedFactor(0.6)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 5) { expect(1 + 1 === (2)) }
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args, which fails, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    val result = {
      var i = 0
      forAll ($argNames$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { expect(1 + 1 === (2)) }
      }
    }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with maxDiscarded param set to 0.6") {

    var i = 0
    val result =
      forAll ($famousArgs$, maxDiscardedFactor(0.6)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 5) { expect(1 + 1 === (2)) }
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args and generators, which fails, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

    val result = {
      var i = 0
      forAll ($famousArgs$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { expect(1 + 1 === (2)) }
      }
    }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with maxDiscardedFactor param set to 0.6") {

    var i = 0
    val result =
      forAll ($nameGenTuples$, maxDiscarded(0.6)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 5) { expect(1 + 1 === (2)) }
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, which fails, with maxDiscardedFactor param set to 1.2") {

    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessfulFactor = 1.2)

    val result = {
      var i = 0
      forAll ($nameGenTuples$, maxDiscardedFactor(1.2)) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { expect(1 + 1 === (2)) }
      }
    }
    assert(result.isNo)
  }

  // Same thing, but set default maxDiscardedFactor to 0.6, prop fails after 0.6
  it("generator-driven property that takes $n$ args, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    val result =
      forAll { ($namesAndTypes$) =>
        i += 1
        whenever (i > 5) { expect(1 + 1 === (2)) }
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args, which fails, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    val result = {
      var i = 0
      forAll { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { expect(1 + 1 === (2)) }
      }
    }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    val result =
      forAll ($argNames$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 5) { expect(1 + 1 === (2)) }
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args, which fails, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    val result = {
      var i = 0
      forAll ($argNames$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { expect(1 + 1 === (2)) }
      }
    }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ args and generators, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    val result =
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 5) { expect(1 + 1 === (2)) }
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ args and generators, which fails, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    val result = {
      var i = 0
      forAll ($famousArgs$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { expect(1 + 1 === (2)) }
      }
    }
    assert(result.isNo)
  }

  it("generator-driven property that takes $n$ named args and generators, which succeeds, with default maxDiscardedFactor set to 0.6") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 0.6)

    var i = 0
    val result =
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 5) { expect(1 + 1 === (2)) }
      }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, which fails, with default maxDiscardedFactor set to 1.2") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(maxDiscardedFactor = 1.2, minSuccessful = 5)

    val result = {
      var i = 0
      forAll ($nameGenTuples$) { ($namesAndTypes$) =>
        i += 1
        whenever (i > 7) { expect(1 + 1 === (2)) }
      }
    }
    assert(result.isNo)
  }

  // set sizeRange with param (ensure always passed within a size range)
  it("generator-driven property that takes $n$ args, with sizeRange specified as param") {
    val result =
    forAll (sizeRange(5)) { ($namesAndTypes$) =>
$lengthAssertions$
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args, with sizeRange specified as param") {
    val result =
    forAll ($argNames$, sizeRange(5)) { ($namesAndTypes$) =>
$lengthAssertions$
    }
    assert(result.isYes)
  }

  // set sizeRange with default (ensure always passed with a size less than sizeRange)
  it("generator-driven property that takes $n$ args, with sizeRange specified as default") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 5)

    val result =
    forAll { ($namesAndTypes$) =>
$lengthAssertions$
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args, with sizeRange specified as default") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 5)

    val result =
    forAll ($argNames$) { ($namesAndTypes$) =>
$lengthAssertions$
    }
    assert(result.isYes)
  }

  // set sizeRange = 0 with (param, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange 0, specified as (param, param)") {

    val result =
    forAll ($fiveFiveArgs$, minSize(5), sizeRange(0)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (param, param)") {

    val result =
    forAll ($fiveFiveNameGenTuples$, minSize(5), sizeRange(0)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  // set sizeRange = 0 with (param, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange = 0, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 0)

    val result =
    forAll ($fiveFiveArgs$, minSize(5)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 0)

    val result =
    forAll ($fiveFiveNameGenTuples$, minSize(5)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  // set sizeRange = 0 with (default, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange = 0, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    val result =
    forAll ($fiveFiveArgs$, sizeRange(0)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 5)

    val result =
    forAll ($fiveFiveNameGenTuples$, sizeRange(0)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  // set sizeRange = 0 with (default, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with sizeRange = 0, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 5, sizeRange = 0)

    val result =
    forAll ($fiveFiveArgs$) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, with sizeRange = 0, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 5, sizeRange = 0)

    val result =
    forAll ($fiveFiveNameGenTuples$) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  // set minSize to 7 and sizeRange to 4 with (param, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (param, param)") {

    val result =
    forAll ($sevenElevenArgs$, minSize(7), sizeRange(4)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (param, param)") {

    val result =
    forAll ($sevenElevenNameGenTuples$, minSize(7), sizeRange(4)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  // set minSize to 7 and sizeRange to 4 with (param, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 4)

    val result =
    forAll ($sevenElevenArgs$, minSize(7)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (param, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(sizeRange = 4)

    val result =
    forAll ($sevenElevenNameGenTuples$, minSize(7)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  // set minSize to 7 and sizeRange to 4 with (default, param) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 7)

    val result =
    forAll ($sevenElevenArgs$, sizeRange(4)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (default, param)") {

    // Hides the member
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSize = 7)

    val result =
    forAll ($sevenElevenNameGenTuples$, sizeRange(4)) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  // set minSize to 7 and sizeRange to 4 with (default, default) (ensure always passed with that size)
  it("generator-driven property that takes $n$ args and generators, with minSize to 7 and sizeRange to 4, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 7, sizeRange = 4)

    val result =
    forAll ($sevenElevenArgs$) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, with minSize to 7 and sizeRange to 4, specified as (default, default)") {

    // Hides the member
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSize = 7, sizeRange = 4)

    val result =
    forAll ($sevenElevenNameGenTuples$) { ($namesAndTypes$) =>
$okayAssertions$
    }
    assert(result.isYes)
  }

  it("generator-driven property that takes $n$ named args and generators, returns VacuousYes should be considered discarded evaluation") {
    val result =
      forAll { ($namesAndTypes$) =>
        val x = -1
        expect(x > 0) implies expect(x > -1)
      }
    assert(result.isNo)
  }
"""

  // 1712  2205

  // For some reason that I don't understand, I need to leave off the stars before the <pre> when
  // they are next to ST commands. So I say  "   <pre>" sometimes instead of " * <pre>".

  val thisYear = Calendar.getInstance.get(Calendar.YEAR)

  def genPropertyChecks(targetDir: File): Seq[File] = {
    targetDir.mkdirs()

    val targetFile = new File(targetDir, "GeneratorDrivenPropertyChecks.scala")

    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
      val bw = new BufferedWriter(new FileWriter(targetFile))

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

          val gens = alpha.take(i).toUpperCase.map(
            c => "      gen" + c + ": org.scalatest.prop.Generator[" + c + "]"
          ).mkString(",\n")

          val genRefs = alpha.take(i).toUpperCase.map(
            c => "gen" + c
          ).mkString(", ")

          val gensAndNames = alpha.take(i).toUpperCase.map(
            c => "      genAndName" + c + ": (org.scalatest.prop.Generator[" + c + "], String)"
          ).mkString(",\n")

          val stepToStepToResult =
            "        val (a, ar) = genA.next(10, nextRandomizer)" + "\n" +
              (if (i > 1)
                alpha.take(i).sliding(2).map {
                  ab =>
                    val a = ab.charAt(0)
                    val b = ab.charAt(1)
                    "      val (" + b + ", " + b + "r) = gen" + b.toString.toUpperCase + ".next(10, " + a + "r)"
                }.mkString("\n")
              else
                "")

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
          st.setAttribute("gens", gens)
          st.setAttribute("genRefs", genRefs)
          st.setAttribute("gensAndNames", gensAndNames)
          st.setAttribute("stepToStepToResult", stepToStepToResult)
          st.setAttribute("alphaLower", alphaLower)
          st.setAttribute("alphaUpper", alphaUpper)
          st.setAttribute("alphaLast", alpha.take(i).last.toString)
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

    Seq(targetFile)
  }

  val generatorTemplate =
    """private[prop] class GeneratorFor$arity$[$alphaUpper$](
      |  $initToLastName$: $initType$ => $lastType$,
      |  $lastToInitName$: $lastType$ => $initType$
      |)(
      |  $initGensDecls$
      |) extends Generator[$lastType$] { thisGenerator =>
      |
      |  private val underlying: Generator[$lastType$] = {
      |    for {
      |      $initGenArrows$
      |    } yield $initToLastName$($initLower$)
      |  }
      |
      |  def next(szp: SizeParam, edges: List[$lastType$], rnd: Randomizer): (RoseTree[$lastType$], List[$lastType$], Randomizer) = underlying.next(szp, edges, rnd)
      |  override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[$lastType$], Randomizer) = underlying.initEdges(maxLength, rnd)
      |  override def map[Z](f: ($lastType$) => Z): Generator[Z] = underlying.map(f)
      |  override def flatMap[Z](f: ($lastType$) => Generator[Z]): Generator[Z] = underlying.flatMap(f)
      |  override def canonicals: LazyListOrStream[RoseTree[$lastType$]] = underlying.canonicals
      |}
    """.stripMargin

  def genGenerators(targetDir: File): Seq[File] = {
    targetDir.mkdirs()

    val alphaAll = "abcdefghijklmnopqrstuvw"

    for (i <- 1 to 22) yield {

      val alpha = alphaAll.take(i + 1)
      val alphaLower = alpha.mkString(", ")
      val initLower = alpha.init.mkString(", ")
      val alphaUpper = alpha.toUpperCase.mkString(", ")
      val initToLastName = alpha.init.head + alpha.init.tail.toUpperCase + "To" + alpha.last.toString.toUpperCase
      val lastToInitName = alpha.last.toString + "To" + alpha.init.toUpperCase
      val initType = if (i > 1) "(" + alpha.init.mkString(", ").toUpperCase + ")" else alpha.init.mkString(", ").toUpperCase
      val lastType = alphaUpper.last.toString
      val initGensDecls = alpha.init.map(a => "genOf" + a.toString.toUpperCase + ": Generator[" + a.toString.toUpperCase + "]").mkString(", \n")
      val initGenArrows = alpha.init.map(a => a + " <- genOf" + a.toString.toUpperCase).mkString("\n")
      val initStreams = alpha.init.map(a => "val streamOf" + a.toString.toUpperCase + ": Stream[" + a.toString.toUpperCase + "] = itOf" + a.toString.toUpperCase + ".toStream").mkString("\n")
      
      val targetFile = new File(targetDir, "GeneratorFor" + i + ".scala")

      if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
        val bw = new BufferedWriter(new FileWriter(targetFile))
        val stCopyRight = new org.antlr.stringtemplate.StringTemplate(copyrightTemplate)
        stCopyRight.setAttribute("year", thisYear)
        bw.write(stCopyRight.toString)

        val st = new org.antlr.stringtemplate.StringTemplate(generatorTemplate)
        st.setAttribute("arity", i)
        st.setAttribute("alphaUpper", alphaUpper)
        st.setAttribute("initLower", initLower)
        st.setAttribute("initToLastName", initToLastName)
        st.setAttribute("lastToInitName", lastToInitName)
        st.setAttribute("initType", initType)
        st.setAttribute("lastType", lastType)
        st.setAttribute("initGensDecls", initGensDecls)
        st.setAttribute("initGenArrows", initGenArrows)
        st.setAttribute("initStreams", initStreams)
        bw.write(st.toString)

        bw.flush()
        bw.close()
      }

      targetFile
    }
  }

  // Invitation style indicates how GeneratorDrivenPropertyChecks is imported
  def genGeneratorDrivenSuite(targetDir: File, mixinInvitationStyle: Boolean, withTables: Boolean, generatorSuiteTemplate: String, checkMethod: String, async: Boolean): Seq[File] = {

    targetDir.mkdirs()

    val traitOrObjectName = if (withTables) "PropertyChecks" else "GeneratorDrivenPropertyChecks"

    val asyncPrefix = if (async) "Async" else "Any"
    val suiteClassName = asyncPrefix + traitOrObjectName + (if (mixinInvitationStyle) "Mixin" else "Import") + "Suite"
    val fileName = checkMethod.capitalize + suiteClassName + ".scala"

    val targetFile = new File(targetDir, fileName)

    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
      val bw = new BufferedWriter(new FileWriter(targetFile))

      try {
        val st = new org.antlr.stringtemplate.StringTemplate(copyrightTemplate)
        st.setAttribute("year", thisYear);
        bw.write(st.toString)
        bw.write(generatorSuitePreamble)
        if (!mixinInvitationStyle)
          bw.write("import " + traitOrObjectName + "._\n")
        bw.write("import org.scalatest.expectations.Expectations._\n")
        if (async)
          bw.write("import scala.concurrent.Future\n")
        bw.write("\n")
        bw.write(
          "class " + checkMethod.capitalize + suiteClassName + " extends org.scalatest.funspec." + asyncPrefix + "FunSpec " +
            (if (mixinInvitationStyle) "with " + traitOrObjectName else "") + " {\n")
        bw.write(generatorSuitePostamble)
        val alpha = "abcdefghijklmnopqrstuv"
        for (i <- 1 to 6) {
          val st = new org.antlr.stringtemplate.StringTemplate(generatorSuiteTemplate)
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
          val lengthAssertions = alpha.take(i).map("      " + checkMethod + "(" + _ + ".length <= 5)").mkString("\n")
          val okayAssertions = alpha.take(i).map("        " + checkMethod + "(" + _ + " === (\"OKAY\"))").mkString("\n")
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

    Seq(targetFile)
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

  def genMain(dir: File, version: String, scalaVersion: String): Seq[File] = {
    genPropertyChecks(dir) ++
    genGenerators(dir)
  }

  def genTest(dir: File, version: String, scalaVersion: String): Seq[File] = {
    genGeneratorDrivenSuite(dir, true, false, generatorSuiteAssertTemplate, "assert", false) ++
    genGeneratorDrivenSuite(dir, false, false, generatorSuiteAssertTemplate, "assert", false) ++
    genGeneratorDrivenSuite(dir, true, true, generatorSuiteAssertTemplate, "assert", false) ++
    genGeneratorDrivenSuite(dir, false, true, generatorSuiteAssertTemplate, "assert", false) ++
    genGeneratorDrivenSuite(dir, true, false, generatorSuiteFutureAssertTemplate, "assert", true) ++
    genGeneratorDrivenSuite(dir, false, false, generatorSuiteFutureAssertTemplate, "assert", true) ++
    genGeneratorDrivenSuite(dir, true, true, generatorSuiteFutureAssertTemplate, "assert", true) ++
    genGeneratorDrivenSuite(dir, false, true, generatorSuiteFutureAssertTemplate, "assert", true) /*++
    genGeneratorDrivenSuite(dir, true, false, generatorSuiteExpectTemplate, "expect", false) ++
    genGeneratorDrivenSuite(dir, false, false, generatorSuiteExpectTemplate, "expect", false) ++
    genGeneratorDrivenSuite(dir, true, true, generatorSuiteExpectTemplate, "expect", false) ++
    genGeneratorDrivenSuite(dir, false, true, generatorSuiteExpectTemplate, "expect", false)*/

  }

  def genTestForJS(dir: File, version: String, scalaVersion: String): Seq[File] = {
    genGeneratorDrivenSuite(dir, true, false, generatorSuiteAssertTemplate, "assert", false) ++
    genGeneratorDrivenSuite(dir, false, false, generatorSuiteAssertTemplate, "assert", false) ++
    genGeneratorDrivenSuite(dir, true, true, generatorSuiteAssertTemplate, "assert", false) ++
    genGeneratorDrivenSuite(dir, false, true, generatorSuiteAssertTemplate, "assert", false) /*++
    genGeneratorDrivenSuite(dir, true, false, generatorSuiteFutureAssertTemplate, "assert", true) ++
    genGeneratorDrivenSuite(dir, false, false, generatorSuiteFutureAssertTemplate, "assert", true) ++
    genGeneratorDrivenSuite(dir, true, true, generatorSuiteFutureAssertTemplate, "assert", true) ++
    genGeneratorDrivenSuite(dir, false, true, generatorSuiteFutureAssertTemplate, "assert", true) ++
    genGeneratorDrivenSuite(dir, true, false, generatorSuiteExpectTemplate, "expect", false) ++
    genGeneratorDrivenSuite(dir, false, false, generatorSuiteExpectTemplate, "expect", false) ++
    genGeneratorDrivenSuite(dir, true, true, generatorSuiteExpectTemplate, "expect", false) ++
    genGeneratorDrivenSuite(dir, false, true, generatorSuiteExpectTemplate, "expect", false)*/

  }

  def genTestForNative(dir: File, version: String, scalaVersion: String): Seq[File] = {
    genGeneratorDrivenSuite(dir, true, false, generatorSuiteAssertTemplate, "assert", false) ++
    genGeneratorDrivenSuite(dir, false, false, generatorSuiteAssertTemplate, "assert", false) ++
    genGeneratorDrivenSuite(dir, true, true, generatorSuiteAssertTemplate, "assert", false) ++
    genGeneratorDrivenSuite(dir, false, true, generatorSuiteAssertTemplate, "assert", false) /*++
    genGeneratorDrivenSuite(dir, true, false, generatorSuiteFutureAssertTemplate, "assert", true) ++
    genGeneratorDrivenSuite(dir, false, false, generatorSuiteFutureAssertTemplate, "assert", true) ++
    genGeneratorDrivenSuite(dir, true, true, generatorSuiteFutureAssertTemplate, "assert", true) ++
    genGeneratorDrivenSuite(dir, false, true, generatorSuiteFutureAssertTemplate, "assert", true) ++
    genGeneratorDrivenSuite(dir, true, false, generatorSuiteExpectTemplate, "expect", false) ++
    genGeneratorDrivenSuite(dir, false, false, generatorSuiteExpectTemplate, "expect", false) ++
    genGeneratorDrivenSuite(dir, true, true, generatorSuiteExpectTemplate, "expect", false) ++
    genGeneratorDrivenSuite(dir, false, true, generatorSuiteExpectTemplate, "expect", false)*/
  }
}
