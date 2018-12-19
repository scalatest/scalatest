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
package org.scalatest

/**
 * ScalaTest's main traits, classes, and other members, including members supporting ScalaTest's DSL for the Scala interpreter.
 */
package object prop {

  @deprecated("Please use org.scalatest.check.Checkers instead.")
  type Checkers = org.scalatest.check.Checkers

  @deprecated("Please use org.scalatest.check.Checkers instead.")
  lazy val Checkers = org.scalatest.check.Checkers

  @deprecated("Please use org.scalatest.check.ScalaCheckDrivenPropertyChecks instead.")
  type GeneratorDrivenPropertyChecks = org.scalatest.check.ScalaCheckDrivenPropertyChecks

  @deprecated("Please use org.scalatest.check.ScalaCheckDrivenPropertyChecks instead.")
  lazy val GeneratorDrivenPropertyChecks = org.scalatest.check.ScalaCheckDrivenPropertyChecks

  @deprecated("Please use org.scalatest.check.ScalaCheckPropertyChecks instead.")
  type PropertyChecks = org.scalatest.check.ScalaCheckPropertyChecks

  @deprecated("Please use org.scalatest.check.ScalaCheckPropertyChecks instead.")
  lazy val PropertyChecks = org.scalatest.check.ScalaCheckPropertyChecks

}
