/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalactic.algebra

import org.scalactic.Every
import org.scalatest.{No, Yes, Fact}
import org.scalatest.Matchers._

import scala.language.higherKinds

trait Law {
  val name: String
  val test: () => Fact

  def assert(): Unit = test() shouldBe a [Yes]
  val no = No("does not satisfy the $name", "satisfies the $name")
  val yes = Yes("does not satisfy the $name", "satisfies the $name")
}

trait Laws[Context[_]] {
  val name: String
  def laws: Every[Law]
  def test: Fact = laws.map(_.test()).reduceLeft(_ && _)
  def assert(): Unit = laws foreach (_.assert())
}

object Law {
  def apply(lawName: String)(code: () => Fact) = new Law {
    override val name = lawName
    override val test = code
  }
}
