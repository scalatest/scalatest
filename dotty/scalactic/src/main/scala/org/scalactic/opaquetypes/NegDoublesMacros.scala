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
package org.scalactic.opaquetypes

import scala.quoted.*

private[opaquetypes] object NegFiniteDoubleMacros {
  def intLiteralToNegFiniteDouble(x: Expr[Int])(using Quotes): Expr[NegDoubles.NegFiniteDouble] = {
    import quotes.reflect.*
    x.value match {
      case Some(v) if v < 0 => Expr(v.toDouble).asExprOf[NegDoubles.NegFiniteDouble]
      case Some(_) => report.errorAndAbort("NegFiniteDouble cannot be instantiated with a non-negative integer literal")
      case None => report.errorAndAbort("NegFiniteDouble conversion requires an integer literal")
    }
  }

  def floatLiteralToNegFiniteDouble(x: Expr[Float])(using Quotes): Expr[NegDoubles.NegFiniteDouble] = {
    import quotes.reflect.*
    x.value match {
      case Some(v) if v < 0.0f => Expr(v.toDouble).asExprOf[NegDoubles.NegFiniteDouble]
      case Some(_) => report.errorAndAbort("NegFiniteDouble cannot be instantiated with a non-negative float literal")
      case None => report.errorAndAbort("NegFiniteDouble conversion requires a float literal")
    }
  }

  def doubleLiteralToNegFiniteDouble(x: Expr[Double])(using Quotes): Expr[NegDoubles.NegFiniteDouble] = {
    import quotes.reflect.*
    x.value match {
      case Some(v) if v < 0.0 && v != Double.PositiveInfinity && v != Double.NegativeInfinity =>
        Expr(v).asExprOf[NegDoubles.NegFiniteDouble]
      case Some(_) => report.errorAndAbort("NegFiniteDouble cannot be instantiated with a non-negative double literal or infinity")
      case None => report.errorAndAbort("NegFiniteDouble conversion requires a double literal")
    }
  }
}
