package org.scalatest.tools

import org.scalatest._
import org.scalatest.events.{SuiteCompleted, SuiteAborted, Ordinal, TestStarting}

class HtmlReporterSpec extends Spec with SharedHelpers {

  object `HtmlReporter ` {
    
    def `should throw IllegalStateException when SuiteCompleted is received without any suite events` {
      val tempDir = createTempDirectory()
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, false, false, false, None, None)
      val suiteCompleted = SuiteCompleted(new Ordinal(99), "TestSuite", "TestSuite", Some("TestSuite"))
      val e = intercept[IllegalStateException] {
        htmlRep(suiteCompleted)
      }
      assert(e.getMessage === "Expected SuiteStarting for completion event: " + suiteCompleted + " in the head of suite events, but we got no suite event at all") 
    }
    
    def `should throw IllegalStateException when SuiteCompleted is received without a SuiteStarting` {
      val tempDir = createTempDirectory()
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, false, false, false, None, None)
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
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, false, false, false, None, None)
      val suiteAborted = SuiteAborted(new Ordinal(99), "Error", "TestSuite", "TestSuite", Some("TestSuite"))
      val e = intercept[IllegalStateException] {
        htmlRep(suiteAborted)
      }
      assert(e.getMessage === "Expected SuiteStarting for completion event: " + suiteAborted + " in the head of suite events, but we got no suite event at all") 
    }
    
    def `should throw IllegalStateException when SuiteAborted is received without a SuiteStarting` {
      val tempDir = createTempDirectory()
      val htmlRep = new HtmlReporter(tempDir.getAbsolutePath, false, false, false, false, None, None)
      val testStarting = TestStarting(new Ordinal(99), "TestSuite", "TestSuite", Some("TestSuite"), "A Test", "A Test")
      htmlRep(testStarting)
      val suiteAborted = SuiteAborted(new Ordinal(99), "Error", "TestSuite", "TestSuite", Some("TestSuite"))
      val e = intercept[IllegalStateException] {
        htmlRep(suiteAborted)
      }
      assert(e.getMessage === "Expected SuiteStarting for completion event: " + suiteAborted + " in the head of suite events, but we got: " + testStarting) 
    }
  }
  
}