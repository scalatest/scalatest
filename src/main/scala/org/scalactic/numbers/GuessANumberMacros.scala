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

private[scalactic] object GuessANumberMacro extends CompileTimeAssertions {
  def apply(c: Context)(value: c.Expr[Int]): c.Expr[GuessANumber] = {
    val notValidMsg =
      "GuessANumber.apply can only be invoked on Int literals between 1 and "+
      "10, inclusive, like GuessANumber(8)."
    val notLiteralMsg =
      "GuessANumber.apply can only be invoked on Int literals, like "+
      "GuessANumber(8). Please use GuessANumber.from instead."
    ensureValidIntLiteral(c)(value, notValidMsg, notLiteralMsg) { i =>
      i >= 1 && i <= 10
    }
    c.universe.reify { GuessANumber.from(value.splice).get }
  } 
}

import CompileTimeAssertions._
private[scalactic] object PercentMacro {
  def apply(c: Context)(value: c.Expr[Int]): c.Expr[Percent] = {
    val notValidMsg =
      "Percent.apply can only be invoked on Int literals between 0 and 100, "+
      "inclusive, like Percent(8)."
    val notLiteralMsg =
      "Percent.apply can only be invoked on Int literals, like Percent(8)."+
      " Please use Percent.from instead."
    ensureValidIntLiteral(c)(value, notValidMsg, notLiteralMsg) { i =>
      i >= 0 && i <= 100
    }
    c.universe.reify { Percent.from(value.splice).get }
  } 
}

