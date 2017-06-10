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
package org.scalatest.selenium

/**
 * Trait that facilitates using the <em>page object pattern</em> with the ScalaTest Selenium DSL.
 *
 * <p>
 * If you use the page object pattern, mixing trait <code>Page</code> into your page classes will allow you to use the <code>go to</code>
 * syntax with your page objects. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * class HomePage extends Page {
 *   val url = "localhost:9000/index.html"
 * }
 *
 * val homePage = new HomePage
 * go to homePage
 * </pre>
 */
trait Page {
  /**
   * The URL of the page represented by this page object.
   */
  val url: String
}