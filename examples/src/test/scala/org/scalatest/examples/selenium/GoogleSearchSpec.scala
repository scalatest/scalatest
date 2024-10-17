/*
 * Copyright 2001-2024 Artima, Inc.
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

package org.scalatest.examples.selenium

import org.scalatest._
import selenium._

trait GoogleSearchSpec extends FunSpecLike with concurrent.Eventually { this: WebBrowser with Driver =>

  describe("google.com") {

    it("should change its title based on the term searched") {
      // Cancel test when cannot access google.com
      try goTo("http://www.google.com") catch { case e: Throwable => cancel(e) }
        clickOn("q")
        textField("q").value = "Cheese!"
        submit()
        // Google's search is rendered dynamically with JavaScript.
        eventually(assert(pageTitle === "Cheese! - Google Search"))
        close()
    }

  }

}

class GoogleSearchSpecWithChrome extends GoogleSearchSpec with Chrome
class GoogleSearchSpecWithSafari extends GoogleSearchSpec with Safari
class GoogleSearchSpecWithInternetExplorer extends GoogleSearchSpec with InternetExplorer
class GoogleSearchSpecWithFirefox extends GoogleSearchSpec with Firefox
