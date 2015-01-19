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

trait Laws {
  val lawsName: String
  // in order to allow access to the law name, a law takes the law name as a parameter
  type Law = String => Fact
  //val laws: Seq[Law]
  def test(): Fact
  //def test: Fact = laws.map(_.test()).reduceLeft(_ && _)
  //def assert(): Unit = test() shouldBe a [Yes]
  def assert(): Unit = test().isInstanceOf[Yes] shouldBe true
}

object Laws {
  // note: should probably be a method on object Fact that takes pos, neg, and boolean, e.g.
  //   Fact(expr, "is larger than the max size", "is not larger than the max size")
  def yes(lawsName: String, lawName: String) = Yes("does not satisfy the $lawsName $lawName", "satisfies the $lawsName $lawName")
  def no(lawsName: String, lawName: String) = No("does not satisfy the $lawsName $lawName", "satisfies the $lawsName $lawName")
}

/**
 * A law is a requirement expressed in the form of a boolean expression
 * augmented with explanatory information.  As generalized, laws take no
 * parameters, but are rather designed to be used where required parameters
 * are in scope.  Commonly, this will be a class that takes implicit generators
 * for any required parameters.
 */
trait Law {
  val name: String
  val test: String => Fact
  def assert(): Unit = test(name).isInstanceOf[Yes] shouldBe true
}


object Law {
  def apply(lawName: String)(code: String => Fact): Law = new Law {
    override val name = lawName
    override val test = code
  }
}
