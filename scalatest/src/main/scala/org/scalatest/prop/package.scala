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

  @deprecated("Checkers has been moved from org.scalatest.prop to org.scalatestplus.scalacheck. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type Checkers = org.scalatestplus.scalacheck.Checkers

  @deprecated("Checkers has been moved from org.scalatest.prop to org.scalatestplus.scalacheck. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  lazy val Checkers = org.scalatestplus.scalacheck.Checkers

  @deprecated("GeneratorDrivenPropertyChecks has been moved from org.scalatest.prop to org.scalatestplus.scalacheck and renamed as ScalaCheckDrivenPropertyChecks. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type GeneratorDrivenPropertyChecks = org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

  @deprecated("GeneratorDrivenPropertyChecks has been moved from org.scalatest.prop to org.scalatestplus.scalacheck and renamed as ScalaCheckDrivenPropertyChecks. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  lazy val GeneratorDrivenPropertyChecks = org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

  @deprecated("PropertyChecks has been moved from org.scalatest.prop to org.scalatestplus.scalacheck and renamed as ScalaCheckPropertyChecks. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type PropertyChecks = org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

  @deprecated("PropertyChecks has been moved from org.scalatest.prop to org.scalatestplus.scalacheck and renamed as ScalaCheckPropertyChecks. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  lazy val PropertyChecks = org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

}
