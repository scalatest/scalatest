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

object BoundedNumberMacros {

  def checkBounds[T](c: Context)(value: c.Expr[Int])(isValid: Int => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(intConst) =>
        val literalValue = intConst.value.toString.toInt
        if (isValid(literalValue))
          println("RETURNING ()")
        else
          c.abort(c.enclosingPosition, "nonValiHELPEDTYPE")
      case _ =>
        c.abort(c.enclosingPosition, "nonValidHELPEDTYPENotALiteral")
    } 
  } 
}

private[scalactic] object GuessANumberMacro {

  def apply(c: Context)(value: c.Expr[Int]): c.Expr[GuessANumber] = {
    BoundedNumberMacros.checkBounds(c)(value) { i =>
      i >= 1 && i <= 10
    }
    c.universe.reify { GuessANumber.from(value.splice).get }
  } 
}

private[scalactic] object PercentMacro {

  def apply(c: Context)(value: c.Expr[Int]): c.Expr[Percent] = {

    BoundedNumberMacros.checkBounds(c)(value) { i => i >= 0 && i <= 100 }
    c.universe.reify { Percent.from(value.splice).get }
  } 
}

