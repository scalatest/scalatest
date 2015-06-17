/*
 * Copyright 2001-2015 Artima, Inc.
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

import java.io.File

import org.openqa.selenium.WebDriver.{Options, TargetLocator, Navigation}
import org.openqa.selenium._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.Matchers

class WithScreenshotSpec extends JettySpec with Matchers with WebBrowser {

  implicit val webDriver: WebDriver = new MockWebDriver

  val systemTmpDir = new File(System.getProperty("java.io.tmpdir"))

  def tmpFiles(tmpDir: File): Seq[String] =
    tmpDir.listFiles() map (_.getName) filter { nm => nm.startsWith("screenshot") && nm.endsWith("png") }

  describe("withScreenshot") {

    it("should save a screenshot file when a ModifiableMessage-based exception is thrown in the given function") {
      def fun: Int = throw new TestFailedException("foo", 1)

      val beforeFiles = tmpFiles(systemTmpDir)

      go to (host + "index.html")
      a [TestFailedException] should be thrownBy withScreenshot { fun }

      val afterFiles = tmpFiles(systemTmpDir)
      assert(beforeFiles.size === afterFiles.size - 1)
    }

    it("should return the result if the given function when ModifiableMessageException is not thrown") {
      def fun: Int = 3
      go to (host + "index.html")
      withScreenshot { fun } should be (3)
    }
  }

  class MockWebDriver extends WebDriver with TakesScreenshot {

    override def get(url: String): Unit = ()
    override def getScreenshotAs[X](target: OutputType[X]): X = {
      target.convertFromBase64Png("")
    }

    override def getPageSource: String = ???
    override def findElements(by: By): java.util.List[WebElement] = ???
    override def getWindowHandle: String = ???
    override def manage(): Options = ???
    override def getWindowHandles: java.util.Set[String] = ???
    override def switchTo(): TargetLocator = ???
    override def close(): Unit = ???
    override def quit(): Unit = ???
    override def getCurrentUrl: String = ???
    override def navigate(): Navigation = ???
    override def getTitle: String = ???
    override def findElement(by: By): WebElement = ???
  }
}
