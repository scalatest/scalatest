/*
 * Copyright 2001-2016 Artima, Inc.
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

case class Classification(val totalGenerated: PosInt, val totals: Map[String, PosZInt]) {

  def proportions: Map[String, Double] =
    totals.mapValues(count => count.toDouble / totalGenerated.toDouble)

  def percentages: Map[String, PosZInt] =
    totals mapValues { count =>
      PosZInt.from((count * 100.0 / totalGenerated).round.toInt).get // Need unsafeFrom
    }

  override def toString = {
    val lines = percentages map { case (classification, percentage) => s"${percentage.value}% $classification" }
    lines.mkString("\n")
  }
}

