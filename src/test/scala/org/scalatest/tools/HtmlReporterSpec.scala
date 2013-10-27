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
package org.scalatest.tools

import org.scalatest._
import SharedHelpers._
import org.scalatest.events.{SuiteCompleted, SuiteAborted, Ordinal, TestStarting}

class HtmlReporterSpec extends Spec {

  object `HtmlReporter ` {
    
    def `should throw IllegalStateException when SuiteCompleted is received without any suite events` {
      val tempDir = createTempDirectory()
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, None, None)
      val suiteCompleted = SuiteCompleted(new Ordinal(99), "TestSuite", "TestSuite", Some("TestSuite"))
      val e = intercept[IllegalStateException] {
        htmlRep(suiteCompleted)
      }
      assert(e.getMessage === "Expected SuiteStarting for completion event: " + suiteCompleted + " in the head of suite events, but we got no suite event at all") 
    }
    
    def `should throw IllegalStateException when SuiteCompleted is received without a SuiteStarting` {
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
    
    def `should throw IllegalStateException when SuiteAborted is received without any suite events` {
      val tempDir = createTempDirectory()
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, None, None)
      val suiteAborted = SuiteAborted(new Ordinal(99), "Error", "TestSuite", "TestSuite", Some("TestSuite"))
      val e = intercept[IllegalStateException] {
        htmlRep(suiteAborted)
      }
      assert(e.getMessage === "Expected SuiteStarting for completion event: " + suiteAborted + " in the head of suite events, but we got no suite event at all") 
    }
    
    def `should throw IllegalStateException when SuiteAborted is received without a SuiteStarting` {
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
  }
  object `HtmlReporter's convertSingleParaToDefinition method` {
    def `should leave strings that contain no <p> alone` {
      assert(HtmlReporter.convertSingleParaToDefinition("") === "")
      assert(HtmlReporter.convertSingleParaToDefinition("hello") === "hello")
      assert(HtmlReporter.convertSingleParaToDefinition("  hello") === "  hello")
    }
    def `should transform something that is a single HTML paragraph to a definition` {
      val actual = HtmlReporter.convertSingleParaToDefinition("<p>This test finished with a <strong>bold</strong> statement!</p>")
      val expected = "<dl>\n<dt>This test finished with a <strong>bold</strong> statement!</dt>\n</dl>"
      assert(actual === expected)
      val actual2 = HtmlReporter.convertSingleParaToDefinition("<p>Whereas this test finished with an <em>emphatic</em> statement!</p>")
      val expected2 = "<dl>\n<dt>Whereas this test finished with an <em>emphatic</em> statement!</dt>\n</dl>"
      assert(actual2 === expected2)
    }
    def `should return unchanged strings with more than one <p>` {
      val original = "<p>This test finished with a <strong>bold</strong> statement!</p><p>second</p>"
      val actual = HtmlReporter.convertSingleParaToDefinition(original)
      assert(actual === original)
    }
    def `should return unchanged strings that have just one <p>, but which is not first` {
      val original = "other stuff<p>This test finished with a <strong>bold</strong> statement!</p>"
      val actual = HtmlReporter.convertSingleParaToDefinition(original)
      assert(actual === original)
    }
    def `should return unchanged strings that start with <p>, but don't end with </p>` {
      val original = "<p>This test finished with a <strong>bold</strong> statement!</p>extra"
      val actual = HtmlReporter.convertSingleParaToDefinition(original)
      assert(actual === original)
    }
  }
  
}
