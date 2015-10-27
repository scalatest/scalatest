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
package org.scalatest.prop

import org.scalatest._
import org.scalatest.Suite
import org.scalatest.enablers.CheckerAsserting
import org.scalacheck.Arbitrary
import org.scalacheck.Shrink
import org.scalacheck.util.Pretty
import org.scalacheck.Prop.Arg
import org.scalacheck.Prop
import org.scalacheck.Test
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import org.scalatest.exceptions.StackDepth
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException
import org.scalacheck.Test.Parameters
import org.scalacheck.Test.TestCallback

/**
 * Trait that contains several &ldquo;check&rdquo; methods that perform ScalaCheck property checks.
 * If ScalaCheck finds a test case for which a property doesn't hold, the problem will be reported as a ScalaTest test failure.
 * 
 * <p>
 * To use ScalaCheck, you specify properties and, in some cases, generators that generate test data. You need not always
 * create generators, because ScalaCheck provides many default generators for you that can be used in many situations.
 * ScalaCheck will use the generators to generate test data and with that data run tests that check that the property holds.
 * Property-based tests can, therefore, give you a lot more testing for a lot less code than assertion-based tests.
 * Here's an example of using ScalaCheck from a <code>JUnitSuite</code>:
 * </p>
 * <pre class="stHighlight">
 * import org.scalatest.junit.JUnitSuite
 * import org.scalatest.prop.Checkers
 * import org.scalacheck.Arbitrary._
 * import org.scalacheck.Prop._
 *
 * class MySuite extends JUnitSuite with Checkers {
 *   @Test
 *   def testConcat() {
 *     check((a: List[Int], b: List[Int]) => a.size + b.size == (a ::: b).size)
 *   }
 * }
 * </pre>
 * <p>
 * The <code>check</code> method, defined in <code>Checkers</code>, makes it easy to write property-based tests inside
 * ScalaTest, JUnit, and TestNG test suites. This example specifies a property that <code>List</code>'s <code>:::</code> method
 * should obey. ScalaCheck properties are expressed as function values that take the required
 * test data as parameters. ScalaCheck will generate test data using generators and 
repeatedly pass generated data to the function. In this case, the test data is composed of integer lists named <code>a</code> and <code>b</code>.
 * Inside the body of the function, you see:
 * </p>
 * <pre class="stHighlight">
 * a.size + b.size == (a ::: b).size
 * </pre>
 * <p>
 * The property in this case is a <code>Boolean</code> expression that will yield true if the size of the concatenated list is equal
 * to the size of each individual list added together. With this small amount
 * of code, ScalaCheck will generate possibly hundreds of value pairs for <code>a</code> and <code>b</code> and test each pair, looking for
 * a pair of integers for which the property doesn't hold. If the property holds true for every value ScalaCheck tries,
 * <code>check</code> returns normally. Otherwise, <code>check</code> will complete abruptly with a <code>TestFailedException</code> that
 * contains information about the failure, including the values that cause the property to be false.
 * </p>
 *
 * <p>
 * For more information on using ScalaCheck properties, see the documentation for ScalaCheck, which is available
 * from <a href="http://code.google.com/p/scalacheck/">http://code.google.com/p/scalacheck/</a>.
 * </p>
 *
 * <p>
 * To execute a suite that mixes in <code>Checkers</code> with ScalaTest's <code>Runner</code>, you must include ScalaCheck's jar file on the class path or runpath.
 * </p>
 *
 * <a name="propCheckConfig"></a><h2>Property check configuration</h2>
 *
 * <p>
 * The property checks performed by the <code>check</code> methods of this trait can be flexibly configured via the services
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
 * The <code>check</code> methods of trait <code>Checkers</code> each take a <code>PropertyCheckConfiguration</code>
 * object as an implicit parameter. This object provides values for each of the five configuration parameters. Trait <code>Configuration</code>
 * provides an implicit <code>val</code> named <code>generatorDrivenConfig</code> with each configuration parameter set to its default value.
 * If you want to set one or more configuration parameters to a different value for all property checks in a suite you can override this
 * val (or hide it, for example, if you are importing the members of the <code>Checkers</code> companion object rather
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
 * Or, if hide it by declaring a variable of the same name in whatever scope you want the changed values to be in effect:
 * </p>
 *
 * <pre class="stHighlight">
 * implicit val generatorDrivenConfig =
 *   PropertyCheckConfiguration(minSize = 10, sizeRange = 10)
 * </pre>
 *
 * <p>
 * In addition to taking a <code>PropertyCheckConfiguration</code> object as an implicit parameter, the <code>check</code> methods of trait
 * <code>Checkers</code> also take a variable length argument list of <code>PropertyCheckConfigParam</code>
 * objects that you can use to override the values provided by the implicit <code>PropertyCheckConfiguration</code> for a single <code>check</code>
 * invocation. You place these configuration settings after the property or property function, For example, if you want to
 * set <code>minSuccessful</code> to 500 for just one particular <code>check</code> invocation,
 * you can do so like this:
 * </p>
 *
 * <pre class="stHighlight">
 * check((n: Int) => n + 0 == n, minSuccessful(500))
 * </pre>
 *
 * <p>
 * This invocation of <code>check</code> will use 500 for <code>minSuccessful</code> and whatever values are specified by the
 * implicitly passed <code>PropertyCheckConfiguration</code> object for the other configuration parameters.
 * If you want to set multiple configuration parameters in this way, just list them separated by commas:
 * </p>
 *
 * <pre class="stHighlight">
 * check((n: Int) => n + 0 == n, minSuccessful(500), maxDiscardedFactor(0.6))
 * </pre>
 *
 * <p>
 * The previous configuration approach works the same in <code>Checkers</code> as it does in <code>GeneratorDrivenPropertyChecks</code>.
 * Trait <code>Checkers</code> also provides one <code>check</code> method that takes an <code>org.scalacheck.Test.Parameters</code> object,
 * in case you want to configure ScalaCheck that way.
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalacheck.Prop
 * import org.scalacheck.Test.Parameters
 * import org.scalatest.prop.Checkers._
 *
 * check(Prop.forAll((n: Int) => n + 0 == n), Parameters.Default { override val minSuccessfulTests = 5 })
 * </pre>
 *
 * <p>
 * For more information, see the documentation
 * for supertrait <a href="Configuration.html"><code>Configuration</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
trait Checkers extends Configuration {

  /**
   * Convert the passed 1-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1, P, ASSERTION](f: A1 => P, configParams: PropertyCheckConfigParam*)
    (implicit
      config: PropertyCheckConfigurable,
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
      asserting: CheckerAsserting[ASSERTION]
    ): asserting.Result = {
    val params = getParams(configParams, config)
    asserting.doCheck(Prop.forAll(f)(p, a1, s1, pp1), params, "Checkers.scala", "check")
  }

  /**
   * Convert the passed 2-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1, A2, P, ASSERTION](f: (A1,A2) => P, configParams: PropertyCheckConfigParam*)
    (implicit
      config: PropertyCheckConfigurable,
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
      a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
      asserting: CheckerAsserting[ASSERTION]
    ): asserting.Result = {
    val params = getParams(configParams, config)
    asserting.doCheck(Prop.forAll(f)(p, a1, s1, pp1, a2, s2, pp2), params, "Checkers.scala", "check")
  }

  /**
   * Convert the passed 3-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1, A2, A3, P, ASSERTION](f: (A1,A2,A3) => P, configParams: PropertyCheckConfigParam*)
    (implicit
      config: PropertyCheckConfigurable,
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
      a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
      a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
      asserting: CheckerAsserting[ASSERTION]
    ): asserting.Result = {
    val params = getParams(configParams, config)
    asserting.doCheck(Prop.forAll(f)(p, a1, s1, pp1, a2, s2, pp2, a3, s3, pp3), params, "Checkers.scala", "check")
  }

  /**
   * Convert the passed 4-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1, A2, A3, A4, P, ASSERTION](f: (A1,A2,A3,A4) => P, configParams: PropertyCheckConfigParam*)
    (implicit
      config: PropertyCheckConfigurable,
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
      a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
      a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
      a4: Arbitrary[A4], s4: Shrink[A4], pp4: A4 => Pretty,
      asserting: CheckerAsserting[ASSERTION]
    ): asserting.Result = {
    val params = getParams(configParams, config)
    asserting.doCheck(Prop.forAll(f)(p, a1, s1, pp1, a2, s2, pp2, a3, s3, pp3, a4, s4, pp4), params, "Checkers.scala", "check")
  }

  /**
   * Convert the passed 5-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1, A2, A3, A4, A5, P, ASSERTION](f: (A1,A2,A3,A4,A5) => P, configParams: PropertyCheckConfigParam*)
    (implicit
      config: PropertyCheckConfigurable,
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
      a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
      a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
      a4: Arbitrary[A4], s4: Shrink[A4], pp4: A4 => Pretty,
      a5: Arbitrary[A5], s5: Shrink[A5], pp5: A5 => Pretty,
      asserting: CheckerAsserting[ASSERTION]
    ): asserting.Result = {
    val params = getParams(configParams, config)
    asserting.doCheck(Prop.forAll(f)(p, a1, s1, pp1, a2, s2, pp2, a3, s3, pp3, a4, s4, pp4, a5, s5, pp5), params, "Checkers.scala", "check")
  }

  /**
   * Convert the passed 6-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1, A2, A3, A4, A5, A6, P, ASSERTION](f: (A1,A2,A3,A4,A5,A6) => P, configParams: PropertyCheckConfigParam*)
    (implicit
      config: PropertyCheckConfigurable,
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
      a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
      a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
      a4: Arbitrary[A4], s4: Shrink[A4], pp4: A4 => Pretty,
      a5: Arbitrary[A5], s5: Shrink[A5], pp5: A5 => Pretty,
      a6: Arbitrary[A6], s6: Shrink[A6], pp6: A6 => Pretty,
      asserting: CheckerAsserting[ASSERTION]
    ): asserting.Result = {
    val params = getParams(configParams, config)
    asserting.doCheck(Prop.forAll(f)(p, a1, s1, pp1, a2, s2, pp2, a3, s3, pp3, a4, s4, pp4, a5, s5, pp5, a6, s6, pp6), params, "Checkers.scala", "check")
  }

  /**
   * Check a property with the given testing parameters.
   *
   * @param p the property to check
   * @param prms the test parameters
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[ASSERTION](p: Prop, prms: Test.Parameters)(implicit asserting: CheckerAsserting[ASSERTION]): asserting.Result = {
    asserting.doCheck(p, prms, "Checkers.scala", "check")
  }

  /**
   * Check a property.
   *
   * @param p the property to check
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[ASSERTION](p: Prop, configParams: PropertyCheckConfigParam*)(implicit config: PropertyCheckConfigurable, asserting: CheckerAsserting[ASSERTION]): asserting.Result = {
    val params = getParams(configParams, config)
    asserting.doCheck(p, params, "Checkers.scala", "check")
  }
}

  /*
   * Returns a ScalaCheck <code>Prop</code> that succeeds if the passed by-name
   * parameter, <code>fun</code>, returns normally; fails if it throws
   * an exception.
   *
   * <p>
   * This method enables ScalaTest assertions and matcher expressions to be used 
   * in property checks. Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * check((s: String, t: String) => successOf(s + t should endWith (s)))
   * </pre>
   *
   * <p>
   * The detail message of the <code>TestFailedException</code> that will likely
   * be thrown by the matcher expression will be added as a label to the ScalaCheck
   * <code>Prop</code> returned by <code>successOf</code>. This, this property
   * check might fail with an exception like:
   * </p>
   *
   * <pre class="stHighlight">
   * org.scalatest.prop.GeneratorDrivenPropertyCheckFailedException: TestFailedException (included as this exception's cause) was thrown during property evaluation.
   * Label of failing property: "ab" did not end with substring "a" (script.scala:24)
   * > arg0 = "?" (1 shrinks)
   * > arg1 = "?" (1 shrinks)
   * 	at org.scalatest.prop.Checkers$class.check(Checkers.scala:252)
   * 	at org.scalatest.prop.Checkers$.check(Checkers.scala:354)
   *    ...
   * </pre>
   *
   * <p>
   * One use case for using matcher expressions in your properties is to 
   * get helpful error messages without using ScalaCheck labels. For example,
   * instead of:
   * </p>
   *
   * <pre class="stHighlight">
   * val complexProp = forAll { (m: Int, n: Int) =>
   *   val res = n * m
   *   (res >= m)    :| "result > #1" &&
   *   (res >= n)    :| "result > #2" &&
   *   (res < m + n) :| "result not sum"
   * }
   * </pre>
   * 
   * <p>
   * You could write:
   * </p>
   *
   * <pre class="stHighlight">
   * val complexProp = forAll { (m: Int, n: Int) =>
   *   successOf {
   *     val res = n * m
   *     res should be >= m
   *     res should be >= n
   *     res should be < (m + n)
   *   }
   * </pre>
   *
   * @param fun the expression to evaluate to determine what <code>Prop</code>
   *            to return
   * @return a ScalaCheck property that passes if the passed by-name parameter,
   *         <code>fun</code>, returns normally, fails if it throws an exception
  private def successOf(fun: => Unit): Prop =
    try {
      fun
      Prop.passed
    }
    catch {
      case e: StackDepth =>
        val msgPart = if (e.message.isDefined) e.message.get + " " else ""
        val fileLinePart =
          if (e.failedCodeFileNameAndLineNumberString.isDefined)
            "(" + e.failedCodeFileNameAndLineNumberString.get + ")"
          else
            ""
        val lbl = msgPart + fileLinePart
        Prop.exception(e).label(lbl)
      case e => Prop.exception(e) // Not sure what to do here
    }
   */
