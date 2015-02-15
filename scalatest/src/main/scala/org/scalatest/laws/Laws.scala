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
package org.scalatest.laws

import org.scalatest._
import org.scalactic._
import Matchers._

import scala.language.higherKinds

/**
 * A law is a requirement expressed in the form of an assertion function
 * augmented with explanatory information.  As generalized, laws take no
 * parameters, but are rather designed to be used where required parameters
 * are in scope.  Commonly, this will be a class that takes implicit generators
 * for any required parameters.
 */
case class Law(lawsName: String, lawName: String, fun: () => Unit)

/**
 * A Laws class represents a list of Laws, together with a name
 * for the group and methods to assert all of the laws.
 * @param lawsName  The name of the group of laws.
 */
abstract class Laws(val lawsName: String) {
  def laws: Every[Law]

  def law(name: String)(code: () => Unit): Law = Law(lawsName, name, code)

  def assert() = laws.foreach { law =>
    withClue(s"The ${law.lawsName} ${law.lawName} law could not be verified: ")(law.fun())
  }
}
