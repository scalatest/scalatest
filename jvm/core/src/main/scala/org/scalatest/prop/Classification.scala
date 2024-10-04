/*
 * Copyright 2001-2024 Artima, Inc.
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
  * The results of a call to [[CommonGenerators.classify]].
  *
  * The `classify` function takes a [[PartialFunction]] and a [[Generator]], and organizes the values created
  * by the Generator based on the PartialFunction. It returns this data structure, which describes
  * how many of the values went into each bucket.
  *
  * If the PartialFunction did not cover all the possible generated values, then the [[totals]] field will
  * not include the others, and the numbers in totals will add up to less than [[totalGenerated]].
  *
  * @param totalGenerated How many values were actually created by the Generator overall.
  * @param totals For each of the buckets defined in the PartialFunction, how many values belonged in each one.
  */
case class Classification(val totalGenerated: PosInt, val totals: Map[String, PosZInt]) {

  /**
    * For each bucket, what fraction of the generated values fell into it?
    *
    * @return Exactly what proportion of the values fell into each bucket.
    */
  def portions: Map[String, Double] =
    totals.mapValues(count => count.toDouble / totalGenerated.toDouble).toMap

  /**
    * For each bucket, what percentage of the generated values fell into it?
    *
    * This is essentially a lower-precision but easier-to-understand variant of [[portions]].
    *
    * @return Approximately what proportion of the values fell into each bucket.
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

