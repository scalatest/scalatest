package org.scalatest.suiteprop

import org.scalatest._
import prop.TableDrivenPropertyChecks
import matchers.ShouldMatchers

class PathSuiteMatrix extends PropSpec with ShouldMatchers with TableDrivenPropertyChecks with SharedHelpers {
  
  property("A path trait should execute the first test, and only the first test, on initial instance creation") {

    new OnlyFirstTestExecutedOnCreationExamples {
      forAll (examples) { suite =>
        val expectedFirstTestCount = if (suite.expectedTotalTestsCount >= 1 && suite.expectFirstTestToRunInInitialInstance) 1 else 0
        suite.counts.firstTestCount should be (expectedFirstTestCount)
        suite.counts.secondTestCount should be (0)
        suite.counts.instanceCount should be (1)
      }
    }
  }

  property("A path trait should run each test once, in its own instance") {
    new OnlyFirstTestExecutedOnCreationExamples {
      forAll (examples) { suite =>
        suite.run(None, Args(SilentReporter))
        val expectedFirstTestCount = if (suite.expectedTotalTestsCount >= 1) 1 else 0
        val expectedSecondTestCount = if (suite.expectedTotalTestsCount == 2) 1 else 0
        suite.counts.firstTestCount should be (expectedFirstTestCount)
        suite.counts.secondTestCount should be (expectedSecondTestCount)
        suite.counts.instanceCount should be (suite.expectedInstanceCount)
      }
    }
  }

  property("A path trait should run only the path to and from each test") {
    new PathBeforeAndAfterExamples {
      forAll (examples) { suite =>
        suite.run(None, Args(SilentReporter))
        suite.firstTestCounts should be (suite.expectedFirstTestCounts)
        suite.secondTestCounts should be (suite.expectedSecondTestCounts)
        suite.counts should be (suite.expectedCounts)
      }
    }
  }

  property("A path trait properly written to test ListBuffer should execute without any test failures") {
    new PathListBufferExamples {
      forAll (examples) { suite =>
        val rec = new EventRecordingReporter
        suite.run(None, Args(rec))
        rec.testFailedEventsReceived should have size 0
        suite.counts.instanceCount should be (suite.expectedInstanceCount)
      }
    }
  }
}
