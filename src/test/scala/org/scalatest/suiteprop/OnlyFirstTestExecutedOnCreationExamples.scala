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

class OnlyFirstTestExecutedOnCreationExamples extends PathSuiteExamples {

  case class Counts(
    var firstTestCount: Int,
    var secondTestCount: Int,
    var instanceCount: Int
  )
  
  trait Services {
    val expectedInstanceCount = 2
    val expectedTotalTestsCount = 2
    val expectFirstTestToRunInInitialInstance = true
    val counts: Counts
  }

  type FixtureServices = Services

  class EmptyPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services {
    import counts._
    instanceCount += 1
    override def newInstance = new EmptyPathFunSpecExample(counts)
    override val expectedInstanceCount = 1
    override val expectedTotalTestsCount = 0
  }

  class EmptyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services {
    import counts._
    instanceCount += 1
    describe("A subject") {
    }
    override def newInstance = new EmptyNestedPathFunSpecExample(counts)
    override val expectedInstanceCount = 1
    override val expectedTotalTestsCount = 0
  }

  class SiblingEmptyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services {
    import counts._
    instanceCount += 1
    describe("A subject") {
    }
    describe("Another subject") {
    }
    override def newInstance = new SiblingEmptyNestedPathFunSpecExample(counts)
    override val expectedTotalTestsCount = 0
  }

  class OneTestSiblingEmptyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services {
    import counts._
    instanceCount += 1
    describe("A subject") {
    }
    describe("Another subject") {
      it("first test") { firstTestCount += 1 }
    }
    override def newInstance = new OneTestSiblingEmptyNestedPathFunSpecExample(counts)
    override val expectedTotalTestsCount = 1
    override val expectFirstTestToRunInInitialInstance = false
  }
  
  class OneTestSiblingEmptyDeeplyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services {
    import counts._
    instanceCount += 1
    describe("A subject") {
    }
    describe("Another subject") {
      describe("when created") {
        it("first test") { firstTestCount += 1 }
      }
    }
    override def newInstance = new OneTestSiblingEmptyDeeplyNestedPathFunSpecExample(counts)
    override val expectedTotalTestsCount = 1
    override val expectFirstTestToRunInInitialInstance = false
  }

  class PathFunSpecExample(val counts: Counts) extends path.FunSpec with Services {
    import counts._
    instanceCount += 1
    it("first test") { firstTestCount += 1 }
    it("second test") { secondTestCount += 1 }
    override def newInstance = new PathFunSpecExample(counts)
  }

  class NestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services {
    import counts._
    instanceCount += 1
    describe("A subject") {
      it("should first test") { firstTestCount += 1 }
      it("should second test") { secondTestCount += 1 }
    }
    override def newInstance = new NestedPathFunSpecExample(counts)
  }

  class SiblingNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services {
    import counts._
    instanceCount += 1
    describe("A subject") {
      it("should first test") { firstTestCount += 1 }
    }
    describe("Another subject") {
      it("should second test") { secondTestCount += 1 }
    }
    override def newInstance = new SiblingNestedPathFunSpecExample(counts)
  }

  class DeeplyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services {
    import counts._
    instanceCount += 1
    describe("A subject") {
      describe("when created") {
        it("should first test") { firstTestCount += 1 }
        it("should second test") { secondTestCount += 1 }
      }
    }
    override def newInstance = new DeeplyNestedPathFunSpecExample(counts)
  }

  class SiblingDeeplyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services {
    import counts._
    instanceCount += 1
    describe("A subject") {
      describe("when created") {
        it("should first test") { firstTestCount += 1 }
      }
    }
    describe("Another subject") {
      describe("when created") {
        it("should second test") { secondTestCount += 1 }
      }
    }
    override def newInstance = new SiblingDeeplyNestedPathFunSpecExample(counts)
  }

  class AsymetricalDeeplyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services {
    import counts._
    instanceCount += 1
    describe("A subject") {
      describe("when created") {
        it("should first test") { firstTestCount += 1 }
      }
      it("should second test") { secondTestCount += 1 }
    }
    override def newInstance = new AsymetricalDeeplyNestedPathFunSpecExample(counts)
  }

  class EmptyPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services {
    import counts._
    instanceCount += 1
    override def newInstance = new EmptyPathFreeSpecExample(counts)
    override val expectedInstanceCount = 1
    override val expectedTotalTestsCount = 0
  }

  class EmptyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services {
    import counts._
    instanceCount += 1
    "A subject" - {
    }
    override def newInstance = new EmptyNestedPathFreeSpecExample(counts)
    override val expectedInstanceCount = 1
    override val expectedTotalTestsCount = 0
  }

  class SiblingEmptyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services {
    import counts._
    instanceCount += 1
    "A subject" - {
    }
    "Another subject" - {
    }
    override def newInstance = new SiblingEmptyNestedPathFreeSpecExample(counts)
    override val expectedTotalTestsCount = 0
  }

  class OneTestSiblingEmptyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services {
    import counts._
    instanceCount += 1
    "A subject" - {
    }
    "Another subject" - {
      "first test" in { firstTestCount += 1 }
    }
    override def newInstance = new OneTestSiblingEmptyNestedPathFreeSpecExample(counts)
    override val expectedTotalTestsCount = 1
    override val expectFirstTestToRunInInitialInstance = false
  }
  
  class OneTestSiblingEmptyDeeplyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services {
    import counts._
    instanceCount += 1
    "A subject" - {
    }
    "Another subject" - {
      "when created" - {
        "first test" in { firstTestCount += 1 }
      }
    }
    override def newInstance = new OneTestSiblingEmptyDeeplyNestedPathFreeSpecExample(counts)
    override val expectedTotalTestsCount = 1
    override val expectFirstTestToRunInInitialInstance = false
  }

  class PathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services {
    import counts._
    instanceCount += 1
    "first test" in { firstTestCount += 1 }
    "second test" in { secondTestCount += 1 }
    override def newInstance = new PathFreeSpecExample(counts)
  }

  class NestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services {
    import counts._
    instanceCount += 1
    "A subject" - {
      "should first test" in { firstTestCount += 1 }
      "should second test" in { secondTestCount += 1 }
    }
    override def newInstance = new NestedPathFreeSpecExample(counts)
  }

  class SiblingNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services {
    import counts._
    instanceCount += 1
    "A subject" - {
      "should first test" in { firstTestCount += 1 }
    }
    "Another subject" - {
      "should second test" in { secondTestCount += 1 }
    }
    override def newInstance = new SiblingNestedPathFreeSpecExample(counts)
  }

  class DeeplyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services {
    import counts._
    instanceCount += 1
    "A subject" - {
      "when created" - {
        "should first test" in { firstTestCount += 1 }
        "should second test" in { secondTestCount += 1 }
      }
    }
    override def newInstance = new DeeplyNestedPathFreeSpecExample(counts)
  }

  class SiblingDeeplyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services {
    import counts._
    instanceCount += 1
    "A subject" - {
      "when created" - {
        "should first test" in { firstTestCount += 1 }
      }
    }
    "Another subject" - {
      "when created" - {
        "should second test" in { secondTestCount += 1 }
      }
    }
    override def newInstance = new SiblingDeeplyNestedPathFreeSpecExample(counts)
  }

  class AsymetricalDeeplyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services {
    import counts._
    instanceCount += 1
    "A subject" - {
      "when created" - {
        "should first test" in { firstTestCount += 1 }
      }
      "should second test" in { secondTestCount += 1 }
    }
    override def newInstance = new AsymetricalDeeplyNestedPathFreeSpecExample(counts)
  }

  lazy val emptyPathFunSpec = new EmptyPathFunSpecExample(Counts(0, 0, 0))
  lazy val emptyNestedPathFunSpec = new EmptyNestedPathFunSpecExample(Counts(0, 0, 0))
  lazy val siblingEmptyNestedPathFunSpec = new SiblingEmptyNestedPathFunSpecExample(Counts(0, 0, 0))
  lazy val oneTestSiblingEmptyNestedPathFunSpec = new OneTestSiblingEmptyNestedPathFunSpecExample(Counts(0, 0, 0))
  lazy val oneTestSiblingEmptyDeeplyNestedPathFunSpec = new OneTestSiblingEmptyDeeplyNestedPathFunSpecExample(Counts(0, 0, 0))
  lazy val pathFunSpec = new PathFunSpecExample(Counts(0, 0, 0))
  lazy val nestedPathFunSpec = new NestedPathFunSpecExample(Counts(0, 0, 0))
  lazy val siblingNestedPathFunSpec = new SiblingNestedPathFunSpecExample(Counts(0, 0, 0))
  lazy val deeplyNestedPathFunSpec = new DeeplyNestedPathFunSpecExample(Counts(0, 0, 0))
  lazy val siblingDeeplyNestedPathFunSpec = new SiblingDeeplyNestedPathFunSpecExample(Counts(0, 0, 0))
  lazy val asymetricalDeeplyNestedPathFunSpec = new AsymetricalDeeplyNestedPathFunSpecExample(Counts(0, 0, 0))
  lazy val emptyPathFreeSpec = new EmptyPathFreeSpecExample(Counts(0, 0, 0))
  lazy val emptyNestedPathFreeSpec = new EmptyNestedPathFreeSpecExample(Counts(0, 0, 0))
  lazy val siblingEmptyNestedPathFreeSpec = new SiblingEmptyNestedPathFreeSpecExample(Counts(0, 0, 0))
  lazy val oneTestSiblingEmptyNestedPathFreeSpec = new OneTestSiblingEmptyNestedPathFreeSpecExample(Counts(0, 0, 0))
  lazy val oneTestSiblingEmptyDeeplyNestedPathFreeSpec = new OneTestSiblingEmptyDeeplyNestedPathFreeSpecExample(Counts(0, 0, 0))
  lazy val pathFreeSpec = new PathFreeSpecExample(Counts(0, 0, 0))
  lazy val nestedPathFreeSpec = new NestedPathFreeSpecExample(Counts(0, 0, 0))
  lazy val siblingNestedPathFreeSpec = new SiblingNestedPathFreeSpecExample(Counts(0, 0, 0))
  lazy val deeplyNestedPathFreeSpec = new DeeplyNestedPathFreeSpecExample(Counts(0, 0, 0))
  lazy val siblingDeeplyNestedPathFreeSpec = new SiblingDeeplyNestedPathFreeSpecExample(Counts(0, 0, 0))
  lazy val asymetricalDeeplyNestedPathFreeSpec = new AsymetricalDeeplyNestedPathFreeSpecExample(Counts(0, 0, 0))
}

