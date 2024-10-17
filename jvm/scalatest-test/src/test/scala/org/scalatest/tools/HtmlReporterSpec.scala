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
package org.scalatest.tools

import java.io.File

import org.scalatest._
import SharedHelpers._
import org.scalatest.events._

import scala.io.Source
import org.scalatest.funspec.AnyFunSpec

class HtmlReporterSpec extends AnyFunSpec {

  describe("HtmlReporter ") {
    
    it("should throw IllegalStateException when SuiteCompleted is received without any suite events") {
      val tempDir = createTempDirectory()
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, None, None)
      val suiteCompleted = SuiteCompleted(new Ordinal(99), "TestSuite", "TestSuite", Some("TestSuite"))
      val e = intercept[IllegalStateException] {
        htmlRep(suiteCompleted)
      }
      assert(e.getMessage === "Expected SuiteStarting for completion event: " + suiteCompleted + " in the head of suite events, but we got no suite event at all") 
    }
    
    it("should throw IllegalStateException when SuiteCompleted is received without a SuiteStarting") {
      val tempDir = createTempDirectory()
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, None, None)
      val testStarting = TestStarting(new Ordinal(99), "TestSuite", "TestSuite", Some("TestSuite"), "A Test", "A Test")
      htmlRep(testStarting)
      val suiteCompleted = SuiteCompleted(new Ordinal(99), "TestSuite", "TestSuite", Some("TestSuite"))
      val e = intercept[IllegalStateException] {
        htmlRep(suiteCompleted)
      }
      assert(e.getMessage === "Expected SuiteStarting for completion event: " + suiteCompleted + " in the head of suite events, but we got: " + testStarting) 
    }
    
    it("should throw IllegalStateException when SuiteAborted is received without any suite events") {
      val tempDir = createTempDirectory()
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, None, None)
      val suiteAborted = SuiteAborted(new Ordinal(99), "Error", "TestSuite", "TestSuite", Some("TestSuite"))
      val e = intercept[IllegalStateException] {
        htmlRep(suiteAborted)
      }
      assert(e.getMessage === "Expected SuiteStarting for completion event: " + suiteAborted + " in the head of suite events, but we got no suite event at all") 
    }
    
    it("should throw IllegalStateException when SuiteAborted is received without a SuiteStarting") {
      val tempDir = createTempDirectory()
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, None, None)
      val testStarting = TestStarting(new Ordinal(99), "TestSuite", "TestSuite", Some("TestSuite"), "A Test", "A Test")
      htmlRep(testStarting)
      val suiteAborted = SuiteAborted(new Ordinal(99), "Error", "TestSuite", "TestSuite", Some("TestSuite"))
      val e = intercept[IllegalStateException] {
        htmlRep(suiteAborted)
      }
      assert(e.getMessage === "Expected SuiteStarting for completion event: " + suiteAborted + " in the head of suite events, but we got: " + testStarting) 
    }

    it("should take MarkupProvided with '&' in it without problem") {
      val tempDir = createTempDirectory()
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, None, None)
      val suiteStarting =
        SuiteStarting(
          new Ordinal(99),
          "TestSuite",
          "TestSuite",
          Some("TestSuite")
        )
      val markupProvided =
        MarkupProvided(
          new Ordinal(99),
          "<a href=\"http://XXX.xxx.com/stc?registerRandom=true&add_random_sku_to_wait_list_sku_available=true\">a link</a>",
          Some(
            NameInfo("TestSuite", "TestSuite", Some("TestSuite"), Some("test 1"))
          )
        )
      val suiteCompleted =
        SuiteCompleted(
          new Ordinal(99),
          "TestSuite",
          "TestSuite",
          Some("TestSuite")
        )
      htmlRep(suiteStarting)
      htmlRep(markupProvided)
      htmlRep(suiteCompleted)
      htmlRep.dispose()
    }

    it("should produce UTF-8 encoded output") {
      val tempDir = createTempDirectory()
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, None, None)
      val suiteStarting =
        SuiteStarting(
          new Ordinal(99),
          "TestSuit\u20ac",
          "TestSuite",
          Some("TestSuite")
        )
      val suiteCompleted =
        SuiteCompleted(
          new Ordinal(99),
          "TestSuit\u20ac",
          "TestSuite",
          Some("TestSuite")
        )
      htmlRep(suiteStarting)
      htmlRep(suiteCompleted)
      htmlRep.dispose()

      val indexFile = new File(tempDir, "index.html")
      assert(indexFile.exists())
      val fromIndexFile = Source.fromFile(indexFile, "UTF-8").mkString
      assert(fromIndexFile.contains("TestSuit\u20ac"))

      val suiteFile = new File(tempDir, "TestSuite.html")
      assert(suiteFile.exists())
      val fromSuiteFile = Source.fromFile(suiteFile, "UTF-8").mkString
      assert(fromSuiteFile.contains("TestSuit\u20ac"))
    }
  }

  describe("HtmlReporter's convertSingleParaToDefinition method") {
    it("should leave strings that contain no <p> alone") {
      assert(HtmlReporter.convertSingleParaToDefinition("") === "")
      assert(HtmlReporter.convertSingleParaToDefinition("hello") === "hello")
      assert(HtmlReporter.convertSingleParaToDefinition("  hello") === "  hello")
    }
    it("should transform something that is a single HTML paragraph to a definition") {
      val actual = HtmlReporter.convertSingleParaToDefinition("<p>This test finished with a <strong>bold</strong> statement!</p>")
      val expected = "<dl>\n<dt>This test finished with a <strong>bold</strong> statement!</dt>\n</dl>"
      assert(actual === expected)
      val actual2 = HtmlReporter.convertSingleParaToDefinition("<p>Whereas this test finished with an <em>emphatic</em> statement!</p>")
      val expected2 = "<dl>\n<dt>Whereas this test finished with an <em>emphatic</em> statement!</dt>\n</dl>"
      assert(actual2 === expected2)
    }
    it("should return unchanged strings with more than one <p>") {
      val original = "<p>This test finished with a <strong>bold</strong> statement!</p><p>second</p>"
      val actual = HtmlReporter.convertSingleParaToDefinition(original)
      assert(actual === original)
    }
    it("should return unchanged strings that have just one <p>, but which is not first") {
      val original = "other stuff<p>This test finished with a <strong>bold</strong> statement!</p>"
      val actual = HtmlReporter.convertSingleParaToDefinition(original)
      assert(actual === original)
    }
    it("should return unchanged strings that start with <p>, but don't end with </p>") {
      val original = "<p>This test finished with a <strong>bold</strong> statement!</p>extra"
      val actual = HtmlReporter.convertSingleParaToDefinition(original)
      assert(actual === original)
    }
  }
  
}
