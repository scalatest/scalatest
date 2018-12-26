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
 * Package object of mockito that contains deprecated type alias that will be removed in the future version of ScalaTest.
 */
package object selenium {

  @deprecated("Page has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type Page = org.scalatestplus.selenium.Page

  @deprecated("WebBrowser has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type WebBrowser = org.scalatestplus.selenium.WebBrowser

  @deprecated("WebBrowser has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  lazy val WebBrowser = org.scalatestplus.selenium.WebBrowser

  @deprecated("Driver has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type Driver = org.scalatestplus.selenium.Driver

  @deprecated("HtmlUnit has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type HtmlUnit = org.scalatestplus.selenium.HtmlUnit

  @deprecated("HtmlUnit has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  lazy val HtmlUnit = org.scalatestplus.selenium.HtmlUnit

  @deprecated("Firefox has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type Firefox = org.scalatestplus.selenium.Firefox

  @deprecated("Firefox has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  lazy val Firefox = org.scalatestplus.selenium.Firefox

  @deprecated("Safari has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type Safari = org.scalatestplus.selenium.Safari

  @deprecated("Safari has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  lazy val Safari = org.scalatestplus.selenium.Safari

  @deprecated("Chrome has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type Chrome = org.scalatestplus.selenium.Chrome

  @deprecated("Chrome has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  lazy val Chrome = org.scalatestplus.selenium.Chrome

  @deprecated("InternetExplorer has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type InternetExplorer = org.scalatestplus.selenium.InternetExplorer

  @deprecated("InternetExplorer has been moved from org.scalatest.selenium to org.scalatestplus.selenium. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  lazy val InternetExplorer = org.scalatestplus.selenium.InternetExplorer

}
