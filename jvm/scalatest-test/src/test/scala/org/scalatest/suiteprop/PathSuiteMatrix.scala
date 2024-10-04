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
package org.scalatest.suiteprop

import org.scalatest._
import SharedHelpers._
import prop.TableDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class PathSuiteMatrix extends AnyPropSpec with Matchers with TableDrivenPropertyChecks {
  
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
