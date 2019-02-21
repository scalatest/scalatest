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
  * Package object of jmock that contains deprecated type alias that will be removed in the future version of ScalaTest.
  */
package object junit {

  @deprecated("AssertionsForJUnit has been moved from org.scalatest.junit to org.scalatestplus.junit. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type AssertionsForJUnit = org.scalatestplus.junit.AssertionsForJUnit

  @deprecated("AssertionsForJUnit has been moved from org.scalatest.junit to org.scalatestplus.junit. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  lazy val AssertionsForJUnit = org.scalatestplus.junit.AssertionsForJUnit

  @deprecated("JUnit3Suite has been moved from org.scalatest.junit to org.scalatestplus.junit. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type JUnit3Suite = org.scalatestplus.junit.JUnit3Suite

  @deprecated("JUnitRunner has been moved from org.scalatest.junit to org.scalatestplus.junit. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type JUnitRunner = org.scalatestplus.junit.JUnitRunner

  @deprecated("JUnitSuite has been moved from org.scalatest.junit to org.scalatestplus.junit. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type JUnitSuite = org.scalatestplus.junit.JUnitSuite

  @deprecated("JUnitSuiteLike has been moved from org.scalatest.junit to org.scalatestplus.junit. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type JUnitSuiteLike = org.scalatestplus.junit.JUnitSuiteLike

  @deprecated("JUnitTestFailedError has been moved from org.scalatest.junit to org.scalatestplus.junit. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type JUnitTestFailedError = org.scalatestplus.junit.JUnitTestFailedError

  @deprecated("JUnitWrapperSuite has been moved from org.scalatest.junit to org.scalatestplus.junit. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type JUnitWrapperSuite = org.scalatestplus.junit.JUnitWrapperSuite

}
