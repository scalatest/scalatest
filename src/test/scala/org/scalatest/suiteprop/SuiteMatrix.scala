/*
 * Copyright 2001-2011 Artima, Inc.
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
import prop.TableDrivenPropertyChecks
import matchers.ShouldMatchers

class SuiteMatrix extends PropSpec with ShouldMatchers with TableDrivenPropertyChecks with SharedHelpers {

  property("When info appears in the code of a successful test, it should be reported in the TestSucceeded.") {
    new InfoInsideTestFiredAfterTestExamples {
      forAll (examples) { suite =>
        val (testStartingIndex, testSucceededIndex) =
          getIndexesForTestInformerEventOrderTests(suite, suite.theTestName, suite.msg)
        testStartingIndex should be < testSucceededIndex
      }
    }
  }
  
  // Add a property for completely empty suites and their empty tags
  property("should, if no test is marked as ignored and there are no tests tagged, return an empty tags map") {
    new InfoInsideTestFiredAfterTestExamples {
      forAll (examples) { suite =>
        suite.tags should be ('empty)
      }
    }
  }
  
  property("should, if the first test is marked as ignored, return a tags map from the tags method that says the first test is ignored") {
    new FirstTestIgnoredExamples {
      forAll (examples) { suite =>
        val firstTestName = suite.theTestNames(0)
        suite.tags should be (Map(firstTestName -> Set("org.scalatest.Ignore")))
      }
    }
  }

  property("should, if the second test is marked as ignored, return a tags map from the tags method that says the second test is ignored") {
    new SecondTestIgnoredExamples {
      forAll (examples) { suite =>
        val secondTestName = suite.theTestNames(1)
        suite.tags should be (Map(secondTestName -> Set("org.scalatest.Ignore")))
      }
    }
  }

  property("should, if two tests is marked as ignored, return a tags map from the tags method that says that both tests are ignored") {
    new TwoTestsIgnoredExamples {
      forAll (examples) { suite =>
        val firstTestName = suite.theTestNames(0)
        val secondTestName = suite.theTestNames(1)
        suite.tags should be (Map(firstTestName -> Set("org.scalatest.Ignore"), secondTestName -> Set("org.scalatest.Ignore")))
      }
    }
  }
  
  property("should, if both the second test is marked as ignored and both are marked Slow, return a tags map from the tags method that says the second test is ignored and both are Slow") {

    new TwoSlowTestsExample {
      forAll (examples) { suite =>
        val firstTestName = suite.theTestNames(0)
        val secondTestName = suite.theTestNames(1)
        suite.tags should be (
          Map(
            firstTestName -> Set("org.scalatest.SlowAsMolasses"),
            secondTestName -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses")
          )
        )
      }
    }
  }
  
  property("should, if both tests are marked Slow and the first test Weak, return a tags map from the tags method that says both are Slow and the first also Weak") {

    new TwoSlowAndOneWeakTestExamples {
      forAll (examples) { suite =>
        val firstTestName = suite.theTestNames(0)
        val secondTestName = suite.theTestNames(1)
        suite.tags should be (
          Map(
            firstTestName -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"),
            secondTestName -> Set("org.scalatest.SlowAsMolasses")
          )
        )
      }
    }
  }
}
