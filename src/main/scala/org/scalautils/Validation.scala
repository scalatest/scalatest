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
package org.scalautils

sealed trait Validation[+B] {
  def &&[C >: B](other: => Validation[C]): Validation[C]
}
case object Pass extends Validation[Nothing] {
  def &&[C](other: => Validation[C]): Validation[C] = other
}
case class Fail[B](b: B) extends Validation[B] {
  def &&[C >: B](other: => Validation[C]): Validation[C] = this
}

/*
OK, I can keep Validation simple like this, and it will only support &&, which
will short-circuit like && does, at least in the error message. And when I do Expectation, I
can put it in ScalaUtils, and have an implicit coversion in the Expectation or Validation
companion object that goes from Expectation to Validation *if* the error type is String.
*/
