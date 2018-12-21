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
package org.scalatest.check

import org.scalacheck.Test.Parameters
import org.scalactic.anyvals.{PosZInt, PosZDouble, PosInt}
import org.scalacheck.Test.TestCallback
import org.scalatest.prop.Configuration

/**
 * Trait providing methods and classes used to configure property checks provided by the
 * the <code>forAll</code> methods of trait <code>GeneratorDrivenPropertyChecks</code> (for ScalaTest-style
 * property checks) and the <code>check</code> methods of trait <code>Checkers</code> (for ScalaCheck-style property checks).
 *
 * @author Bill Venners
 */
private[scalatest] trait ScalaCheckConfiguration extends Configuration {

  private[scalatest] def getParams(
                               configParams: Seq[Configuration#PropertyCheckConfigParam],
                               c: PropertyCheckConfigurable
                               ): Parameters = {

    val config: PropertyCheckConfiguration = c.asPropertyCheckConfiguration
    var minSuccessful: Option[Int] = None
    var maxDiscarded: Option[Int] = None
    var maxDiscardedFactor: Option[Double] = None
    var pminSize: Option[Int] = None
    var psizeRange: Option[Int] = None
    var pmaxSize: Option[Int] = None
    var pworkers: Option[Int] = None

    var minSuccessfulTotalFound = 0
    var maxDiscardedTotalFound = 0
    var maxDiscardedFactorTotalFound = 0
    var minSizeTotalFound = 0
    var sizeRangeTotalFound = 0
    var maxSizeTotalFound = 0
    var workersTotalFound = 0

    for (configParam <- configParams) {
      configParam match {
        case param: MinSuccessful =>
          minSuccessful = Some(param.value)
          minSuccessfulTotalFound += 1
        case param: MaxDiscarded =>
          maxDiscarded = Some(param.value)
          maxDiscardedTotalFound += 1
        case param: MaxDiscardedFactor =>
          maxDiscardedFactor = Some(param.value)
          maxDiscardedFactorTotalFound += 1
        case param: MinSize =>
          pminSize = Some(param.value)
          minSizeTotalFound += 1
        case param: SizeRange =>
          psizeRange = Some(param.value)
          sizeRangeTotalFound += 1
        case param: MaxSize =>
          pmaxSize = Some(param.value)
          maxSizeTotalFound += 1
        case param: Workers =>
          pworkers = Some(param.value)
          workersTotalFound += 1
      }
    }

    if (minSuccessfulTotalFound > 1)
      throw new IllegalArgumentException("can pass at most one MinSuccessful config parameters, but " + minSuccessfulTotalFound + " were passed")
    val maxDiscardedAndFactorTotalFound = maxDiscardedTotalFound + maxDiscardedFactorTotalFound
    if (maxDiscardedAndFactorTotalFound > 1)
      throw new IllegalArgumentException("can pass at most one MaxDiscarded or MaxDiscardedFactor config parameters, but " + maxDiscardedAndFactorTotalFound + " were passed")
    if (minSizeTotalFound > 1)
      throw new IllegalArgumentException("can pass at most one MinSize config parameters, but " + minSizeTotalFound + " were passed")
    val maxSizeAndSizeRangeTotalFound = maxSizeTotalFound + sizeRangeTotalFound
    if (maxSizeAndSizeRangeTotalFound > 1)
      throw new IllegalArgumentException("can pass at most one SizeRange or MaxSize config parameters, but " + maxSizeAndSizeRangeTotalFound + " were passed")
    if (workersTotalFound > 1)
      throw new IllegalArgumentException("can pass at most one Workers config parameters, but " + workersTotalFound + " were passed")

    val minSuccessfulTests: Int = minSuccessful.getOrElse(config.minSuccessful)

    val minSize: Int = pminSize.getOrElse(config.minSize)

    val maxSize = {
      (psizeRange, pmaxSize, config.legacyMaxSize) match {
        case (None, None, Some(legacyMaxSize)) =>
          legacyMaxSize
        case (None, Some(maxSize), _) =>
          maxSize
        case _ =>
          psizeRange.getOrElse(config.sizeRange.value) + minSize
      }
    }

    val maxDiscardRatio: Float = {
      (maxDiscardedFactor, maxDiscarded, config.legacyMaxDiscarded, minSuccessful) match {
        case (None, None, Some(legacyMaxDiscarded), Some(specifiedMinSuccessful)) =>
          PropertyCheckConfiguration.calculateMaxDiscardedFactor(specifiedMinSuccessful, legacyMaxDiscarded).toFloat
        case (None, Some(md), _, _) =>
          if (md < 0) Parameters.default.maxDiscardRatio
          else PropertyCheckConfiguration.calculateMaxDiscardedFactor(minSuccessfulTests, md).toFloat
        case _ =>
          maxDiscardedFactor.getOrElse(config.maxDiscardedFactor.value).toFloat
      }
    }

    Parameters.default
      .withMinSuccessfulTests(minSuccessfulTests)
      .withMinSize(minSize)
      .withMaxSize(maxSize)
      .withWorkers(pworkers.getOrElse(config.workers))
      .withTestCallback(new TestCallback {})
      .withMaxDiscardRatio(maxDiscardRatio)
      .withCustomClassLoader(None)
  }
}

/**
 * Companion object that facilitates the importing of <code>Configuration</code> members as
 * an alternative to mixing it in. One use case is to import <code>Configuration</code> members so you can use
 * them in the Scala interpreter.
 */
private[scalatest] object ScalaCheckConfiguration extends ScalaCheckConfiguration
