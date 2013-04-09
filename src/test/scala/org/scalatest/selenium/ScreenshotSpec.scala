/*
 * Copyright 2001-2012 Artima, Inc.
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

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.concurrent.TimeUnit
import org.scalatest.time.SpanSugar
import org.scalatest.ParallelTestExecution
import org.openqa.selenium.WebDriver
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.scalatest.time.Span
import org.scalatest.time.Seconds
import org.scalatest.exceptions.TestFailedException
import java.io.File
import org.scalatest.Suite
import org.scalatest.Args
import org.scalatest.ScreenshotOnFailure
import org.scalatest.SharedHelpers.SilentReporter
import org.scalatest.Ignore

@Ignore
class ScreenshotSpec extends JettySpec with ShouldMatchers with SpanSugar with WebBrowser with HtmlUnit {

  describe("ScreenshotFixture") {
    
    val systemTmpDir = System.getProperty("java.io.tmpdir")
      
    def tmpFiles(tmpDir: String): Seq[String] = for {
      f <- new File(tmpDir).listFiles()
        fName = f.toString
        if fName.startsWith("Scala") && fName.endsWith("Test")
      } yield fName
      
    it("should by default save the file in the system's default temp dir") {
      class MySuite extends Suite with Firefox with ScreenshotOnFailure {
        def `test: screenshot should be saved` {
          go to "http://www.artima.com"
          assert(1 + 1 === 3)
        }    
      }
      val beforeFiles = tmpFiles(systemTmpDir)
      (new MySuite).run(None, Args(SilentReporter))
      val afterFiles = tmpFiles(systemTmpDir)
      assert(beforeFiles.size === afterFiles.size - 1)
    }
    
    it("should not create a temp file if a test succeeds") {
      class MySuite extends Suite with Firefox with ScreenshotOnFailure {
        def `test: no screenshot needed for this one` {
          assert(1 + 1 === 2)
        }    
      }
      val beforeFiles = tmpFiles(systemTmpDir)
      (new MySuite).run(None, Args(SilentReporter))
      val afterFiles = tmpFiles(systemTmpDir)
      assert(beforeFiles === afterFiles)
    }
    
    it("should create a temp file in a user-chosen directory") {
      class MySuite extends Suite with Firefox with ScreenshotOnFailure {
        override val screenshotDir = "myTmpDir"
        def `test: screenshot should be saved` {
          go to "http://www.artima.com"
          assert(1 + 1 === 3)
        }    
      }
      val theTmpDir = new File("myTmpDir")
      theTmpDir.mkdir()
      try {
        val beforeFiles = tmpFiles("myTmpDir")
        (new MySuite).run(None, Args(SilentReporter))
        val afterFiles = tmpFiles("myTmpDir")
        assert(beforeFiles.size === afterFiles.size - 1)
      }
      finally theTmpDir.delete()
    }
  }
}

