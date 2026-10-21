/*
 * Copyright 2001-2026 Artima, Inc.
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

import org.scalactic.anyvals.{PosInt, PosZDouble, PosZInt}
import java.util.concurrent.atomic.AtomicReference

/**
 * Trait providing methods and classes used to configure property checks provided by the
 * the <code>forAll</code> methods of trait <code>GeneratorDrivenPropertyChecks</code> (for ScalaTest-style
 * property checks) and the <code>check</code> methods of trait <code>Checkers</code> (for ScalaCheck-style property checks).
 *
 * @author Bill Venners
 */
trait Configuration {

  import Configuration.{PropertyCheckConfiguration, PropertyCheckConfigParam, 
                        MinSuccessful, MaxDiscardedFactor, MinSize, SizeRange, Workers, Seed}
  
  /**
   * Returns a <code>MinSuccessful</code> property check configuration parameter containing the passed value, which specifies the minimum number of successful
   * property evaluations required for the property to pass.
   *
   */
  def minSuccessful(value: PosInt): MinSuccessful = new MinSuccessful(value)

  /**
   * Returns a <code>MaxDiscardedFactor</code> property check configuration parameter containing the passed value, which specifies the factor of discarded
   * property evaluations allowed during property evaluation.
   *
   */
  def maxDiscardedFactor(value: PosZDouble): MaxDiscardedFactor = MaxDiscardedFactor(value)

  /**
   * Returns a <code>MinSize</code> property check configuration parameter containing the passed value, which specifies the minimum size parameter to
   * provide to ScalaCheck, which it will use when generating objects for which size matters (such as
   * strings or lists).
   *
   */
  def minSize(value: PosZInt): MinSize = new MinSize(value)

  /**
   * Returns a <code>SizeRange</code> property check configuration parameter containing the passed value, that (with minSize) specifies the maximum size parameter to
   * provide to ScalaCheck, which it will use when generating objects for which size matters (such as
   * strings or lists).
   *
   * <p>
   * Note that the size range is added to minSize in order to calculate the maximum size passed to ScalaCheck.
   * Using a range allows compile-time checking of a non-negative number being specified.
   * </p>
   *
   * @author Bill Venners
   */
  def sizeRange(value: PosZInt): SizeRange = SizeRange(value)

  /**
   * Returns a <code>Workers</code> property check configuration parameter containing the passed value, which specifies the number of worker threads
   * to use when evaluating a property.
   *
   */
  def workers(value: PosInt): Workers = new Workers(value)

  /**
   * Returns a <code>Seed</code> property check configuration parameter containing the passed value, which specifies the initial seed
   * to use for the random data generated during property evaluation.
   *
   * <p>
   * When a seed is specified, the property check will be deterministic: re-running the same property check
   * with the same seed will generate exactly the same "random" data. This is useful for reproducing a failure
   * that was reported with an Init Seed value, without needing to pass the <code>-S</code> flag to the entire
   * test run.
   * </p>
   *
   */
  def seed(value: Long): Seed = Seed(value)

  /**
    * Given some optional [[PropertyCheckConfigParam]]s and a [[PropertyCheckConfiguration]], compute the resulting
    * [[Configuration.Parameter]].
    *
    * This function deals with resolving the various forms of these configuration values, into a consistent form
    * suitable for using in properties.
    *
    * Duplicate [[PropertyCheckConfigParam]] entries are not permitted in the `configParams` list.
    *
    * @param configParams optionally, some parameters that differ from the provided `c`
    * @param c a configuration object, describing how to run property evaluations
    * @return a fully-set-up [[Configuration.Parameter]] object, ready to evaluate properties with.
    */
  private[scalatest] def getParameter(configParams: Seq[PropertyCheckConfigParam], config: PropertyCheckConfiguration): Configuration.Parameter = {

    var minSuccessful: Option[Int] = None
    var maxDiscardedFactor: Option[Double] = None
    var pminSize: Option[Int] = None
    var psizeRange: Option[Int] = None
    var pworkers: Option[Int] = None
    var pinitSeed: Option[Long] = None

    var minSuccessfulTotalFound = 0
    var maxDiscardedTotalFound = 0
    var maxDiscardedFactorTotalFound = 0
    var minSizeTotalFound = 0
    var sizeRangeTotalFound = 0
    var workersTotalFound = 0
    var initSeedTotalFound = 0

    for (configParam <- configParams) {
      configParam match {
        case MinSuccessful(value) =>
          minSuccessful = Some(value)
          minSuccessfulTotalFound += 1 
        case MaxDiscardedFactor(value) =>
          maxDiscardedFactor = Some(value)
          maxDiscardedFactorTotalFound += 1
        case MinSize(value) =>
          pminSize = Some(value)
          minSizeTotalFound += 1
        case SizeRange(value) =>
          psizeRange = Some(value)
          sizeRangeTotalFound += 1
        case Workers(value) =>
          pworkers = Some(value)
          workersTotalFound += 1
        case Seed(value) =>
          pinitSeed = Some(value)
          initSeedTotalFound += 1
      }
    }

    if (minSuccessfulTotalFound > 1)
      throw new IllegalArgumentException("can pass at most one MinSuccessful config parameters, but " + minSuccessfulTotalFound + " were passed")
    val maxDiscardedAndFactorTotalFound = maxDiscardedTotalFound + maxDiscardedFactorTotalFound
    if (maxDiscardedAndFactorTotalFound > 1)
      throw new IllegalArgumentException("can pass at most one MaxDiscarded or MaxDiscardedFactor config parameters, but " + maxDiscardedAndFactorTotalFound + " were passed")
    if (minSizeTotalFound > 1)
      throw new IllegalArgumentException("can pass at most one MinSize config parameters, but " + minSizeTotalFound + " were passed")
    if (sizeRangeTotalFound > 1)
      throw new IllegalArgumentException("can pass at most one SizeRange config parameters, but " + sizeRangeTotalFound + " were passed")
    if (workersTotalFound > 1)
      throw new IllegalArgumentException("can pass at most one Workers config parameters, but " + workersTotalFound + " were passed")
    if (initSeedTotalFound > 1)
      throw new IllegalArgumentException("can pass at most one Seed config parameters, but " + initSeedTotalFound + " were passed")

    val minSuccessfulTests: Int = minSuccessful.getOrElse(config.minSuccessful)

    val minSize: Int = pminSize.getOrElse(config.minSize)

    val maxSize = psizeRange.getOrElse(config.sizeRange.value) + minSize

    val maxDiscardRatio: Float = maxDiscardedFactor.getOrElse(config.maxDiscardedFactor.value).toFloat

    val param =
      Configuration.Parameter(
        PosInt.from(minSuccessfulTests).getOrElse(config.minSuccessful),
        PosZDouble.from(maxDiscardRatio).getOrElse(config.maxDiscardedFactor),
        PosZInt.from(minSize).getOrElse(config.minSize),
        PosZInt.from(maxSize - minSize).getOrElse(config.sizeRange),
        PosInt.from(pworkers.getOrElse(config.workers)).getOrElse(config.workers),
        pinitSeed.orElse(config.initSeed)
      )

    if(
      param.minSuccessful.value <= 0 ||
      param.maxDiscardedFactor.value <= 0 ||
      param.minSize.value < 0 ||
      maxSize < param.minSize.value ||
      param.workers.value <= 0
    ) throw new IllegalArgumentException("Invalid test parameters")

    param
  }

  // SKIP-DOTTY-START
  import scala.language.implicitConversions

  /**
   * Implicit <code>PropertyCheckConfig</code> value providing default configuration values.
   */
  implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration()
  // SKIP-DOTTY-END

  //DOTTY-ONLY val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration()
  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Given <code>PropertyCheckConfig</code> value providing default configuration values.
  //DOTTY-ONLY  */
  //DOTTY-ONLY given PropertyCheckConfiguration = generatorDrivenConfig
}

/**
 * Companion object that facilitates the importing of <code>Configuration</code> members as
 * an alternative to mixing it in. One use case is to import <code>Configuration</code> members so you can use
 * them in the Scala interpreter.
 */
object Configuration extends Configuration {

  /**
    * Internal utility functions for converting between old-style discard-count
    * configuration and the new-style [[MaxDiscardedFactor]] ratio.
    */
  object PropertyCheckConfiguration {
    /**
      * Compute the new-style MaxDiscardedFactor based on the old-style MaxDiscarded.
      *
      * @param minSuccessful the provided minimum number of successful evaluations
      * @param maxDiscarded the old-style provided maximum number of discards to allow
      * @return the new-style ratio of maximum discards to successful evaluations
      */
    private[scalatest] def calculateMaxDiscardedFactor(minSuccessful: Int, maxDiscarded: Int): Double =
      (maxDiscarded.toDouble + 1.0) / minSuccessful.toDouble

    /**
      * Compute the actual number of discards allowed for this run.
      *
      * @param maxDiscardedRatio the maximum number of discards, as a multiplier of the minimum number of
      *                          successful evaluations
      * @param minSuccessful the number of successful evaluations to require
      * @return the maximum number of discards to allow for this run
      */
    private[scalatest] def calculateMaxDiscarded(maxDiscardedRatio: Double, minSuccessful: Int): Double =
      (maxDiscardedRatio * minSuccessful.toDouble) - 1.0
  }

  /**
    * Describes the configuration to use when evaluating a property.
    *
    * @param minSuccessful      the minimum number of successful property evaluations required for
    *                           the property to pass; see [[MinSuccessful]]
    * @param maxDiscardedFactor how many generated values may be discarded,
    *                           as a multiple of the successful attempts, before the property check is considered to be
    *                           [[org.scalatest.prop.PropertyCheckResult.Exhausted]]; see [[MaxDiscardedFactor]]
    * @param minSize      the minimum size parameter to provide to ScalaCheck, which it will use when
    *                     generating objects for which size matters (such as strings or lists); see [[MinSize]]
    * @param sizeRange    the maximum size parameter to provide to ScalaCheck, which it will use when
    *                     generating objects for which size matters (such as strings or lists); see [[SizeRange]]
    * @param workers      number of worker threads to use when evaluating a property; see [[Workers]]
   * @param initSeed     an optional seed value to use for the random data generated during property
   *                     evaluation; when set, property checks become deterministic, generating the same
   *                     "random" data on every run; see [[Seed]]
    */
  case class PropertyCheckConfiguration(minSuccessful: PosInt = PosInt(10),
                                        maxDiscardedFactor: PosZDouble = PosZDouble(5.0),
                                        minSize: PosZInt = PosZInt(0),
                                        sizeRange: PosZInt = PosZInt(100),
                                        workers: PosInt = PosInt(1),
                                        initSeed: Option[Long] = None)

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
  sealed abstract class PropertyCheckConfigParam extends Product with Serializable

  /**
   * A <code>PropertyCheckConfigParam</code> that specifies the minimum number of successful
   * property evaluations required for the property to pass.
    *
    * Once this many evaluations have passed, the property will return
    * [[PropertyCheckResult.Success]].
   *
   * @author Bill Venners
   */
  case class MinSuccessful(value: PosInt) extends PropertyCheckConfigParam

  /**
    * A [[PropertyCheckConfigParam]] that specifies how many generated values may be discarded,
    * as a multiple of the successful attempts, before the property check is considered to be
    * [[org.scalatest.prop.PropertyCheckResult.Exhausted]].
    *
    *
    * <p>
    * In <code>GeneratorDrivenPropertyChecks</code>, a property evaluation is discarded if it throws
    * <code>DiscardedEvaluationException</code>, which is produced by a <code>whenever</code> clause that
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
    * For either kind of property check, <code>MaxDiscardedFactor</code> indicates the maximum fraction of
    * total tests that may be discarded, relative to the number of successful tests. For example, if this
    * is set to 4.0, and you are running 100 tests, it may discard up to 400 tries before considering the
    * test to be [[org.scalatest.prop.PropertyCheckResult.Exhausted]].
    * </p>
    *
    * @param value the permitted number of discarded tests, as a multiple of successful ones.
    */
  case class MaxDiscardedFactor(value: PosZDouble) extends PropertyCheckConfigParam
  
  /**
   * A <code>PropertyCheckConfigParam</code> that specifies the minimum size parameter to
   * provide to ScalaCheck, which it will use when generating objects for which size matters (such as
   * strings or lists).
   *
   * @author Bill Venners
   */
  case class MinSize(value: PosZInt) extends PropertyCheckConfigParam

  /**
   * A <code>PropertyCheckConfigParam</code> that (with minSize) specifies the maximum size parameter to
   * provide to ScalaCheck, which it will use when generating objects for which size matters (such as
   * strings or lists).
   *
   * <p>
   * Note that the size range is added to minSize in order to calculate the maximum size passed to ScalaCheck.
   * Using a range allows compile-time checking of a non-negative number being specified.
   * </p>
   *
   * @author Bill Venners
   */
  case class SizeRange(value: PosZInt) extends PropertyCheckConfigParam
  
  /**
   * A <code>PropertyCheckConfigParam</code> that specifies the number of worker threads
   * to use when evaluating a property.
    *
    * Property evaluation runs on a single thread by default, but may run multiple
    * threads if desired. If so, the evaluation will generally run faster. However,
    * be careful not to use this if there is any risk of deadlocks, race conditions,
    * or other hazards of multi-threaded code in evaluating this property or the
    * code under test.
   *
   * @author Bill Venners
   */
  case class Workers(value: PosInt) extends PropertyCheckConfigParam

  /**
   * A [[PropertyCheckConfigParam]] that specifies a seed for generating random values for property checks.
   *
   * <p>
   * When a seed is specified, the property check will be deterministic: re-running the same property check
   * with the same seed will generate exactly the same "random" data. This is useful for reproducing a failure
   * that was reported with an Init Seed value, without needing to pass the <code>-S</code> flag to the entire
   * test run.
   * </p>
   *
   * @param value the seed value to use for randomization
   */
  case class Seed(value: Long) extends PropertyCheckConfigParam

  /**
   * Companion object for the `Seed` class.
   */
  object Seed {

    import java.util.concurrent.atomic.AtomicReference

    /**
      * This seed is empty under ordinary circumstances. It is here so that the test
      * Runner can poke in a seed value to be used during a test run. If set, it will be used
      * as the seed for all calls to [[Seed.default]].
      */
    private[scalatest] val configuredRef: AtomicReference[Option[Long]] = new AtomicReference(None)

    /**
     * Creates a new Seed using default approach, which is initialized based on the current time.
     *
     * This should not be considered a strong source of seed for randomness -- in cases where high entropy really
     * matters, it's a bit mediocre -- but for general purposes it's typically good enough.
     */
    def default: Seed = 
      Seed(
        configuredRef.get() match {
          case Some(value) => value
          case None => System.currentTimeMillis()
        }
      )

    /**
     * Retrieves an optional `Seed` object based on the currently configured seed value.
     *
     * If a seed value has been configured (i.e., set in `configuredRef`), this method will return a `Some` 
     * containing a `Seed` instance initialized with that value. Otherwise, it returns `None`, indicating that
     * no seed value has been explicitly configured.
     *
     * @return an optional `Seed` containing the configured seed value, or `None` if no seed has been set
     */
    def configured: Option[Seed] = configuredRef.get().map(Seed(_))  
  }

  /**
    * The parameters that define how a property evaluation should be executed.
    *
    * You typically will not build this directly. Instead, you should define a
    * [[PropertyCheckConfiguration]] and/or some [[PropertyCheckConfigParam]]s, pass
    * those in, and the system will resolve those to a coherent [[Parameter]].
    *
    * @param minSuccessful      the minimum number of successful property evaluations required for
    *                           the property to pass; see [[MinSuccessful]]
    * @param maxDiscardedFactor how many generated values may be discarded,
    *                           as a multiple of the successful attempts, before the property check is considered to be
    *                           [[org.scalatest.prop.PropertyCheckResult.Exhausted]]; see [[MaxDiscardedFactor]]
    * @param minSize      the minimum size parameter to provide to ScalaCheck, which it will use when
    *                     generating objects for which size matters (such as strings or lists); see [[MinSize]]
    * @param sizeRange    the maximum size parameter to provide to ScalaCheck, which it will use when
    *                     generating objects for which size matters (such as strings or lists); see [[SizeRange]]
    * @param workers      number of worker threads to use when evaluating a property; see [[Workers]]
   * @param initSeed     an optional seed value to use for the random data generated during property
   *                     evaluation; see [[Seed]]
    */
  case class Parameter(minSuccessful: PosInt = PosInt(10),
                       maxDiscardedFactor: PosZDouble = PosZDouble(5.0),
                       minSize: PosZInt = PosZInt(0),
                       sizeRange: PosZInt = PosZInt(100),
                       workers: PosInt = PosInt(1),
                       initSeed: Option[Long] = None) {

    import org.scalactic.Requirements._

    require(minSize.value + sizeRange.value >= 0)

    lazy val maxSize: PosZInt = {
      PosZInt.ensuringValid(minSize.value + sizeRange.value)
    }
  }

  /**
    * Compute the number of discarded evaluation attempts that will be considered to be exhausted. This is a
    * multiple of two config parameters.
    *
    * @param maxDiscardedRatio the multiplier of acceptable discarded-to-successful evaluations
    * @param minSuccessful the minimum number of successful evaluations
    * @return how many discarded evaluations to allow in this run
    */
  private[scalatest] def calculateMaxDiscarded(maxDiscardedRatio: Double, minSuccessful: Int): Double =
    PropertyCheckConfiguration.calculateMaxDiscarded(maxDiscardedRatio, minSuccessful)

}
