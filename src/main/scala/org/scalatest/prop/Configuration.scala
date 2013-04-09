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

import org.scalacheck.Test.Params

/**
 * Trait providing methods and classes used to configure property checks provided by the
 * the <code>forAll</code> methods of trait <code>GeneratorDrivenPropertyChecks</code> (for ScalaTest-style
 * property checks) and the <code>check</code> methods of trait <code>Checkers</code> (for ScalaCheck-style property checks).
 *
 * @author Bill Venners
 */
trait Configuration {

  /**
   * Configuration object for property checks.
   *
   * <p>
   * The default values for the parameters are:
   * </p>
   *
   * <table style="border-collapse: collapse; border: 1px solid black">
   * <tr>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * minSuccessful
   * </td>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * 100
   * </td>
   * </tr>
   * <tr>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * maxDiscarded
   * </td>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * 500
   * </td>
   * </tr>
   * <tr>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * minSize
   * </td>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * 0
   * </td>
   * </tr>
   * <tr>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * maxSize
   * </td>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * 100
   * </td>
   * </tr>
   * <tr>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * workers
   * </td>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * 1
   * </td>
   * </tr>
   * </table>
   *
   * @param minSuccessful the minimum number of successful property evaluations required for the property to pass.
   * @param maxDiscarded the maximum number of discarded property evaluations allowed during a property check
   * @param minSize the minimum size parameter to provide to ScalaCheck, which it will use when generating objects for which size matters (such as strings or lists).
   * @param maxSize the maximum size parameter to provide to ScalaCheck, which it will use when generating objects for which size matters (such as strings or lists).
   * @param workers specifies the number of worker threads to use during property evaluation
   * @throws IllegalArgumentException if the specified <code>minSuccessful</code> value is less than or equal to zero,
   *   the specified <code>maxDiscarded</code> value is less than zero,
   *   the specified <code>minSize</code> value is less than zero,
   *   the specified <code>maxSize</code> value is less than zero,
   *   the specified <code>minSize</code> is greater than the specified or default value of <code>maxSize</code>, or
   *   the specified <code>workers</code> value is less than or equal to zero.
   *
   * @author Bill Venners
   */
  case class PropertyCheckConfig(
    minSuccessful: Int = 100,
    maxDiscarded: Int = 500,
    minSize: Int = 0,
    maxSize: Int = 100,
    workers: Int = 1
  ) {
    require(minSuccessful > 0, "minSuccessful had value " + minSuccessful + ", but must be greater than zero")
    require(maxDiscarded >= 0, "maxDiscarded had value " + maxDiscarded + ", but must be greater than or equal to zero")
    require(minSize >= 0, "minSize had value " + minSize + ", but must be greater than or equal to zero")
    require(maxSize >= 0, "maxSize had value " + maxSize + ", but must be greater than or equal to zero")
    require(minSize <= maxSize, "minSize had value " + minSize + ", which must be less than or equal to maxSize, which had value " + maxSize)
    require(workers > 0, "workers had value " + workers + ", but must be greater than zero")
  }

  /**
   * Abstract class defining a family of configuration parameters for property checks.
   * 
   * <p>
   * The subclasses of this abstract class are used to pass configuration information to
   * the <code>forAll</code> methods of traits <code>PropertyChecks</code> (for ScalaTest-style
   * property checks) and <code>Checkers</code>(for ScalaCheck-style property checks).
   * </p>
   *
   * @author Bill Venners
   */
  sealed abstract class PropertyCheckConfigParam
  
  /**
   * A <code>PropertyCheckConfigParam</code> that specifies the minimum number of successful
   * property evaluations required for the property to pass.
   *
   * @throws IllegalArgumentException if specified <code>value</code> is less than or equal to zero.
   *
   * @author Bill Venners
   */
  case class MinSuccessful(value: Int) extends PropertyCheckConfigParam {
    require(value > 0)
  }
  
  /**
   * A <code>PropertyCheckConfigParam</code> that specifies the maximum number of discarded
   * property evaluations allowed during property evaluation.
   *
   * <p>
   * In <code>GeneratorDrivenPropertyChecks</code>, a property evaluation is discarded if it throws
   * <code>DiscardedEvaluationException</code>, which is produce by <code>whenever</code> clause that
   * evaluates to false. For example, consider this ScalaTest property check:
   * </p>
   *
   * <pre class="stHighlight">
   * // forAll defined in <code>GeneratorDrivenPropertyChecks</code>
   * forAll { (n: Int) => 
   *   whenever (n > 0) {
   *     doubleIt(n) should equal (n * 2)
   *   }
   * }
   *
   * </pre>
   *
   * <p>
   * In the above code, whenever a non-positive <code>n</code> is passed, the property function will complete abruptly
   * with <code>DiscardedEvaluationException</code>.
   * </p>
   *
   * <p>
   * Similarly, in <code>Checkers</code>, a property evaluation is discarded if the expression to the left
   * of ScalaCheck's <code>==></code> operator is false. Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * // forAll defined in <code>Checkers</code>
   * forAll { (n: Int) => 
   *   (n > 0) ==> doubleIt(n) == (n * 2)
   * }
   *
   * </pre>
   *
   * <p>
   * For either kind of property check, <code>MaxDiscarded</code> indicates the maximum number of discarded 
   * evaluations that will be allowed. As soon as one past this number of evaluations indicates it needs to be discarded,
   * the property check will fail.
   * </p>
   *
   * @throws IllegalArgumentException if specified <code>value</code> is less than zero.
   *
   * @author Bill Venners
   */
  case class MaxDiscarded(value: Int) extends PropertyCheckConfigParam {
    require(value >= 0)
  }
  
  /**
   * A <code>PropertyCheckConfigParam</code> that specifies the minimum size parameter to
   * provide to ScalaCheck, which it will use when generating objects for which size matters (such as
   * strings or lists).
   *
   * @throws IllegalArgumentException if specified <code>value</code> is less than zero.
   *
   * @author Bill Venners
   */
  case class MinSize(value: Int) extends PropertyCheckConfigParam {
    require(value >= 0)
  }
  
  /**
   * A <code>PropertyCheckConfigParam</code> that specifies the maximum size parameter to
   * provide to ScalaCheck, which it will use when generating objects for which size matters (such as
   * strings or lists).
   *
   * <p>
   * Note that the maximum size should be greater than or equal to the minimum size. This requirement is
   * enforced by the <code>PropertyCheckConfig</code> constructor and the <code>forAll</code> methods of
   * traits <code>PropertyChecks</code> and <code>Checkers</code>. In other words, it is enforced at the point
   * both a maximum and minimum size are provided together.
   * </p>
   * 
   * @throws IllegalArgumentException if specified <code>value</code> is less than zero.
   *
   * @author Bill Venners
   */
  case class MaxSize(value: Int) extends PropertyCheckConfigParam {
    require(value >= 0)
  }
  
  /**
   * A <code>PropertyCheckConfigParam</code> that specifies the number of worker threads
   * to use when evaluating a property.
   *
   * @throws IllegalArgumentException if specified <code>value</code> is less than or equal to zero.
   *
   * @author Bill Venners
   */
  case class Workers(value: Int) extends PropertyCheckConfigParam {
    require(value > 0)
  }
  
  /**
   * Returns a <code>MinSuccessful</code> property check configuration parameter containing the passed value, which specifies the minimum number of successful
   * property evaluations required for the property to pass.
   *
   * @throws IllegalArgumentException if specified <code>value</code> is less than or equal to zero.
   */
  def minSuccessful(value: Int): MinSuccessful = new MinSuccessful(value)

  /**
   * Returns a <code>MaxDiscarded</code> property check configuration parameter containing the passed value, which specifies the maximum number of discarded
   * property evaluations allowed during property evaluation.
   *
   * @throws IllegalArgumentException if specified <code>value</code> is less than zero.
   */
  def maxDiscarded(value: Int): MaxDiscarded = new MaxDiscarded(value)

  /**
   * Returns a <code>MinSize</code> property check configuration parameter containing the passed value, which specifies the minimum size parameter to
   * provide to ScalaCheck, which it will use when generating objects for which size matters (such as
   * strings or lists).
   *
   * @throws IllegalArgumentException if specified <code>value</code> is less than zero.
   */
  def minSize(value: Int): MinSize = new MinSize(value)

  /**
   * Returns a <code>MaxSize</code> property check configuration parameter containing the passed value, which specifies the maximum size parameter to
   * provide to ScalaCheck, which it will use when generating objects for which size matters (such as
   * strings or lists).
   *
   * <p>
   * Note that the maximum size should be greater than or equal to the minimum size. This requirement is
   * enforced by the <code>PropertyCheckConfig</code> constructor and the <code>forAll</code> methods of
   * traits <code>PropertyChecks</code> and <code>Checkers</code>. In other words, it is enforced at the point
   * both a maximum and minimum size are provided together.
   * </p>
   * 
   * @throws IllegalArgumentException if specified <code>value</code> is less than zero.
   */
  def maxSize(value: Int): MaxSize = new MaxSize(value)

  /**
   * Returns a <code>Workers</code> property check configuration parameter containing the passed value, which specifies the number of worker threads
   * to use when evaluating a property.
   *
   * @throws IllegalArgumentException if specified <code>value</code> is less than or equal to zero.
   */
  def workers(value: Int): Workers = new Workers(value)

  private[prop] def getParams(
    configParams: Seq[PropertyCheckConfigParam],
    config: PropertyCheckConfig
  ): Params = {

    var minSuccessful = -1
    var maxDiscarded = -1
    var minSize = -1
    var maxSize = -1
    var workers = -1

    var minSuccessfulTotalFound = 0
    var maxDiscardedTotalFound = 0
    var minSizeTotalFound = 0
    var maxSizeTotalFound = 0
    var workersTotalFound = 0

    for (configParam <- configParams) {
      configParam match {
        case param: MinSuccessful =>
          minSuccessful = param.value
          minSuccessfulTotalFound += 1
        case param: MaxDiscarded =>
          maxDiscarded = param.value
          maxDiscardedTotalFound += 1
        case param: MinSize =>
          minSize = param.value
          minSizeTotalFound += 1
        case param: MaxSize =>
          maxSize = param.value
          maxSizeTotalFound += 1
        case param: Workers =>
          workers = param.value
          workersTotalFound += 1
      }
    }
  
    if (minSuccessfulTotalFound > 1)
      throw new IllegalArgumentException("can pass at most MinSuccessful config parameters, but " + minSuccessfulTotalFound + " were passed")
    if (maxDiscardedTotalFound > 1)
      throw new IllegalArgumentException("can pass at most MaxDiscarded config parameters, but " + maxDiscardedTotalFound + " were passed")
    if (minSizeTotalFound > 1)
      throw new IllegalArgumentException("can pass at most MinSize config parameters, but " + minSizeTotalFound + " were passed")
    if (maxSizeTotalFound > 1)
      throw new IllegalArgumentException("can pass at most MaxSize config parameters, but " + maxSizeTotalFound + " were passed")
    if (workersTotalFound > 1)
      throw new IllegalArgumentException("can pass at most Workers config parameters, but " + workersTotalFound + " were passed")

    // Adding one to maxDiscarded, because I think it is easier to understand that maxDiscarded means the maximum number of times
    // allowed that discarding will occur and the property can still pass. One more discarded evaluation than this number and the property will fail
    // because of it. ScalaCheck fails at exactly maxDiscardedTests.
    Params(
      if (minSuccessful != -1) minSuccessful else config.minSuccessful,
      (if (maxDiscarded != -1) maxDiscarded else config.maxDiscarded) + 1,
      if (minSize != -1) minSize else config.minSize,
      if (maxSize != -1) maxSize else config.maxSize,
      Params().rng,
      if (workers != -1) workers else config.workers,
      Params().testCallback
    )
  }

  /**
   * Implicit <code>PropertyCheckConfig</code> value providing default configuration values.
   */
  implicit val generatorDrivenConfig = PropertyCheckConfig()
}

/**
 * Companion object that facilitates the importing of <code>Configuration</code> members as
 * an alternative to mixing it in. One use case is to import <code>Configuration</code> members so you can use
 * them in the Scala interpreter.
 */
object Configuration extends Configuration
