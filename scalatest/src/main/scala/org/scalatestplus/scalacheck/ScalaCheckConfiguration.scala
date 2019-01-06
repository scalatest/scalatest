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
package org.scalatestplus.scalacheck

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
private[scalacheck] trait ScalaCheckConfiguration extends Configuration {

  private def calculateMaxDiscardedFactor(minSuccessful: Int, maxDiscarded: Int): Double =
    ((maxDiscarded + 1): Double) / (minSuccessful: Double)

  // Note: We should remove this class once we're fully moved on to PropertyCheckConfiguration
  private class InternalPropertyCheckConfiguration(minSuccessful: PosInt = PosInt(10),
                                                maxDiscardedFactor: PosZDouble = PosZDouble(5.0),
                                                minSize: PosZInt = PosZInt(0),
                                                sizeRange: PosZInt = PosZInt(100),
                                                workers: PosInt = PosInt(1),
                                                val maxDiscarded: Option[Int] = None,
                                                val maxSize: Option[Int] = None) extends PropertyCheckConfiguration(minSuccessful, maxDiscardedFactor, minSize, sizeRange, workers)

  override implicit def PropertyCheckConfig2PropertyCheckConfiguration(p: PropertyCheckConfig): PropertyCheckConfiguration = {
    val maxDiscardedFactor = calculateMaxDiscardedFactor(p.minSuccessful, p.maxDiscarded)
    new InternalPropertyCheckConfiguration(
      minSuccessful = PosInt.from(p.minSuccessful).get,
      maxDiscardedFactor = PosZDouble.from(maxDiscardedFactor).get,
      minSize = PosZInt.from(p.minSize).get,
      sizeRange = PosZInt.from(p.maxSize - p.minSize).get,
      workers = PosInt.from(p.workers).get,
      Some(p.maxDiscarded),
      Some(p.maxSize))
  }

  private[scalacheck] def getParams(
                               configParams: Seq[Configuration#PropertyCheckConfigParam],
                               c: PropertyCheckConfigurable
                               ): Parameters = {

    val config: InternalPropertyCheckConfiguration =
      c match {
        case legacyConfig: PropertyCheckConfig =>
          PropertyCheckConfig2PropertyCheckConfiguration(legacyConfig).asInstanceOf[InternalPropertyCheckConfiguration]  // safe cast
        case legacyConfig: InternalPropertyCheckConfiguration => legacyConfig
        case _ =>
          val config = new PropertyCheckConfiguration(c)
          new InternalPropertyCheckConfiguration(config.minSuccessful, config.maxDiscardedFactor, config.minSize, config.sizeRange, config.workers)
          /*println("###c: " + c.getClass.getName)
          if (c.getClass.getName == "org.scalatest.prop.Configuration$PropertyCheckConfiguration")
            (new PropertyCheckConfiguration(c), None, None)
          else {
            val legacyMaxDiscardedField = c.getClass.getDeclaredField("legacyMaxDiscarded")
            legacyMaxDiscardedField.setAccessible(true)
            val legacyMaxDiscardedValue = legacyMaxDiscardedField.get(c).asInstanceOf[Option[Int]]
            legacyMaxDiscardedField.setAccessible(false)
            val legacyMaxSizeField = c.getClass.getDeclaredField("legacyMaxSize")
            legacyMaxSizeField.setAccessible(true)
            val legacyMaxSizeValue = legacyMaxSizeField.get(c).asInstanceOf[Option[Int]]
            legacyMaxSizeField.setAccessible(false)
            (new PropertyCheckConfiguration(c), legacyMaxDiscardedValue, legacyMaxSizeValue)
          }*/
      }

    /*val (config: PropertyCheckConfiguration, legacyMaxDiscarded: Option[Int], legacyMaxSize: Option[Int]) = {
      val config = c.asPropertyCheckConfiguration
      (config, config.legacyMaxDiscarded, config.legacyMaxSize)
    }*/

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
      (psizeRange, pmaxSize, config.maxSize) match {
        case (None, None, Some(legacyMaxSize)) =>
          legacyMaxSize
        case (None, Some(maxSize), _) =>
          maxSize
        case _ =>
          psizeRange.getOrElse(config.sizeRange.value) + minSize
      }
    }

    val maxDiscardRatio: Float = {
      (maxDiscardedFactor, maxDiscarded, config.maxDiscarded, minSuccessful) match {
        case (None, None, Some(legacyMaxDiscarded), Some(specifiedMinSuccessful)) =>
          calculateMaxDiscardedFactor(specifiedMinSuccessful, legacyMaxDiscarded).toFloat
        case (None, Some(md), _, _) =>
          if (md < 0) Parameters.default.maxDiscardRatio
          else calculateMaxDiscardedFactor(minSuccessfulTests, md).toFloat
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
private[scalacheck] object ScalaCheckConfiguration extends ScalaCheckConfiguration
