/*
 * Copyright 2001-2025 Artima, Inc.
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

import org.scalactic.anyvals.{PosInt,PosZInt}

/**
  * Represents the result of [[CommonGenerators.classify]], showing how generated values are distributed into buckets.
  *
  * The `classify` function organizes values from a [[Generator]] using a [[PartialFunction]], counting how many fall into each bucket.
  * If some values are not covered by the PartialFunction, [[totals]] will not include them, and the sum of [[totals]] may be less than [[totalGenerated]].
  *
  * @param totalGenerated Total number of values generated.
  * @param totals Map of bucket name to count of values in that bucket.
  */
case class Classification(val totalGenerated: PosInt, val totals: Map[String, PosZInt]) {

  /**
    * Returns the fraction of generated values in each bucket.
    *
    * @return Map of bucket name to proportion (0.0 to 1.0).
    */
  def portions: Map[String, Double] =
    totals.mapValues(count => count.toDouble / totalGenerated.toDouble).toMap

  /**
    * Returns the percentage of generated values in each bucket (rounded to integer).
    *
    * Easier to interpret than [[portions]].
    *
    * @return Map of bucket name to percentage (0 to 100).
    */
  def percentages: Map[String, PosZInt] =
    (totals mapValues { count =>
      PosZInt.ensuringValid((count * 100.0 / totalGenerated).round.toInt)
    }).toMap

  override def toString = {
    val lines = percentages map { case (classification, percentage) => s"${percentage.value}% $classification" }
    lines.mkString("\n")
  }
}

