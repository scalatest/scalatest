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

object FiniteDoubleMacros {

  def intLiteralToFiniteDouble(x: Expr[Int])(using Quotes): Expr[Finites.FiniteDouble] =
    x.value match {
      case Some(value) => '{ ${Expr(value.toDouble)}.asInstanceOf[Finites.FiniteDouble] }
      case None => quotes.reflect.report.errorAndAbort("FiniteDouble.apply requires an integer literal")
    }

  def floatLiteralToFiniteDouble(x: Expr[Float])(using Quotes): Expr[Finites.FiniteDouble] =
    x.value match {
      case Some(value) if java.lang.Float.isFinite(value) => '{ ${Expr(value.toDouble)}.asInstanceOf[Finites.FiniteDouble] }
      case Some(_) => quotes.reflect.report.errorAndAbort("FiniteDouble cannot be instantiated with infinity or NaN")
      case None => quotes.reflect.report.errorAndAbort("FiniteDouble.apply requires a float literal")
    }

  def doubleLiteralToFiniteDouble(x: Expr[Double])(using Quotes): Expr[Finites.FiniteDouble] =
    x.value match {
      case Some(value) if java.lang.Double.isFinite(value) => '{ ${Expr(value)}.asInstanceOf[Finites.FiniteDouble] }
      case Some(_) => quotes.reflect.report.errorAndAbort("FiniteDouble cannot be instantiated with infinity or NaN")
      case None => quotes.reflect.report.errorAndAbort("FiniteDouble.apply requires a double literal")
    }
}