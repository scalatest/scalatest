/*
 * Copyright 2001-2015 Artima, Inc.
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

trait PrettyFunction1[A, B] extends (A => B) {
  val paramName: String
  val paramTypeName: String
}

object PrettyFunction1 {
  def chain[A, B, C](aToBPretty: PrettyFunction1[A, B], bToCPretty: PrettyFunction1[B, C]): PrettyFunction1[A, C] =
    new PrettyFunction1[A, C] {
      def apply(a: A): C = {
        val f = aToBPretty
        val g = bToCPretty
        g(f(a))
      }
      val paramName = aToBPretty.paramName
      val paramTypeName = aToBPretty.paramTypeName
      override def toString = {
        s"(${aToBPretty.paramName}: ${aToBPretty.paramTypeName}) => { " + 
        s"val f = ${aToBPretty.toString}; " +
        s"val g = ${bToCPretty.toString}; " +
        s"g(f(${aToBPretty.paramName})) }"
      }
    }
}
