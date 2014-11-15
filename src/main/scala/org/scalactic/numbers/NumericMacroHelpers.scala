/*
* Copyright 2001-2014 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalactic.numbers

import reflect.macros.Context
import org.scalactic.Resources

trait NumericMacroHelpers {

  def ensureValidIntLiteral(c: Context)(value: c.Expr[Int], notValidMsg: String, notLiteralMsg: String)(isValid: Int => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(intConst) =>
        val literalValue = intConst.value.toString.toInt
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    } 
  } 

  def ensureValidLongLiteral(c: Context)(value: c.Expr[Long], notValidMsg: String, notLiteralMsg: String)(isValid: Long => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(longConst) =>
        val literalValue = longConst.value.toString.toLong
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    } 
  } 

  def ensureValidFloatLiteral(c: Context)(value: c.Expr[Float], notValidMsg: String, notLiteralMsg: String)(isValid: Float => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(floatConst) =>
        val literalValue = floatConst.value.toString.toFloat
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    } 
  } 

  def ensureValidDoubleLiteral(c: Context)(value: c.Expr[Double], notValidMsg: String, notLiteralMsg: String)(isValid: Double => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(doubleConst) =>
        val literalValue = doubleConst.value.toString.toDouble
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    } 
  } 
}

object NumericMacroHelpers extends NumericMacroHelpers
