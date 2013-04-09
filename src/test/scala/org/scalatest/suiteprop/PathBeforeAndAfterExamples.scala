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

class PathBeforeAndAfterExamples extends PathSuiteExamples {

  case class Counts(
    var before0: Int = 0,
    var before00: Int = 0,
    var before000: Int = 0,
    var before01: Int = 0,
    var before010: Int = 0,
    var middle: Int = 0,
    var after010: Int = 0,
    var after01: Int = 0,
    var after000: Int = 0,
    var after00: Int = 0,
    var after0: Int = 0
  ) {
    val arr = new Array[Int](0)
  }
  
  trait Services {
    val counts: Counts
    var firstTestCounts: Counts = Counts()
    var secondTestCounts: Counts = Counts()
    val expectedFirstTestCounts: Counts
    val expectedSecondTestCounts: Counts
    val expectedCounts: Counts
  }

  type FixtureServices = Services

  class EmptyPathFunSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FunSpec with Services {
    import counts._
    middle += 1
    override def newInstance = new EmptyPathFunSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts()
    val expectedSecondTestCounts = Counts()
    val expectedCounts = Counts(middle = 1)
  }

  class EmptyNestedPathFunSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FunSpec with Services {
    import counts._
    before0 += 1
    describe("A subject") {
      middle += 1
    }
    after0 += 1
    override def newInstance = new EmptyNestedPathFunSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts()
    val expectedSecondTestCounts = Counts()
    val expectedCounts = Counts(before0 = 1, middle = 1, after0 = 1)
  }

  class SiblingEmptyNestedPathFunSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FunSpec with Services {
    import counts._
    before0 += 1
    describe("A subject") {
      before00 += 1
    }
    middle += 1
    describe("Another subject") {
      before01 += 1
    }
    after0 += 1
    override def newInstance = new SiblingEmptyNestedPathFunSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts()
    val expectedSecondTestCounts = Counts()
    val expectedCounts = Counts(before0 = 2, before00 = 1, middle = 2, before01 = 1, after0 = 2)
  }

  class OneTestSiblingEmptyNestedPathFunSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FunSpec with Services {
    import counts._
    before0 += 1
    describe("A subject") {
      before00 += 1
    }
    middle += 1
    describe("Another subject") {
      before01 += 1
      it("first test") { initialInstance.get.firstTestCounts = counts.copy() }
      after01 += 1
    }
    after0 += 1
    override def newInstance = new OneTestSiblingEmptyNestedPathFunSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 2, before00 = 1, middle = 2, before01 = 1, after0 = 1)
    val expectedSecondTestCounts = Counts()
    val expectedCounts = Counts(before0 = 2, before00 = 1, middle = 2, before01 = 1, after01 = 1, after0 = 2)
  }

  class OneTestSiblingEmptyDeeplyNestedPathFunSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FunSpec with Services {
    import counts._
    before0 += 1
    describe("A subject") {
      before00 += 1
    }
    middle += 1
    describe("Another subject") {
      before01 += 1
      describe("when created") {
        before010 += 1
        it("first test") { initialInstance.get.firstTestCounts = counts.copy() }
        after010 += 1
      }
      after01 += 1
    }
    after0 += 1
    override def newInstance = new OneTestSiblingEmptyDeeplyNestedPathFunSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 2, before00 = 1, middle = 2, before01 = 1, before010 = 1, after0 = 1)
    val expectedSecondTestCounts = Counts()
    val expectedCounts = Counts(before0 = 2, before00 = 1, middle = 2, before01 = 1, before010 = 1, after010 = 1, after01 = 1, after0 = 2)
  }

  class PathFunSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FunSpec with Services {
    import counts._
    before0 += 1
    it("first test") { firstTestCounts = counts.copy() }
    middle += 1
    it("second test") { initialInstance.get.secondTestCounts = counts.copy() }
    after0 += 1
    override def newInstance = new PathFunSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, middle = 2, after0 = 1)
    val expectedCounts = Counts(before0 = 2, middle = 2, after0 = 2)
  }

  class NestedPathFunSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FunSpec with Services {
    import counts._
    before0 += 1
    describe("A subject") {
      before00 += 1
      it("should first test") { firstTestCounts = counts.copy() }
      middle += 1
      it("should second test") { initialInstance.get.secondTestCounts = counts.copy() }
      after00 += 1
    }
    after0 += 1
    override def newInstance = new NestedPathFunSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1, before00 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, before00 = 2, middle = 2, after00 = 1, after0 = 1)
    val expectedCounts = Counts(before0 = 2, before00 = 2, middle = 2, after00 = 2, after0 = 2)
  }

  class SiblingNestedPathFunSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FunSpec with Services {
    import counts._
    before0 += 1
    describe("A subject") {
      before00 += 1
      it("should first test") { firstTestCounts = counts.copy() }
      after00 += 1
    }
    middle += 1
    describe("Another subject") {
      before01 += 1
      it("should second test") { initialInstance.get.secondTestCounts = counts.copy() }
      after01 += 1
    }
    after0 += 1
    override def newInstance = new SiblingNestedPathFunSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1, before00 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, before00 = 1, after00 = 1, middle = 2, before01 = 1, after0 = 1)
    val expectedCounts = Counts(before0 = 2, before00 = 1, after01 = 1, middle = 2, before01 = 1, after00 = 1, after0 = 2)
  }

  class DeeplyNestedPathFunSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FunSpec with Services {
    import counts._
    before0 += 1
    describe("A subject") {
      before00 += 1
      describe("when created") {
        before000 += 1
        it("should first test") { firstTestCounts = counts.copy() }
        middle += 1
        it("should second test") { initialInstance.get.secondTestCounts = counts.copy() }
        after000 += 1
      }
      after00 += 1
    }
    after0 += 1
    override def newInstance = new DeeplyNestedPathFunSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1, before00 = 1, before000 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, before00 = 2, before000 = 2, middle = 2, after000 = 1, after00 = 1, after0 = 1)
    val expectedCounts = Counts(before0 = 2, before00 = 2, before000 = 2, middle = 2, after000 = 2, after00 = 2, after0 = 2)
  }

  class SiblingDeeplyNestedPathFunSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FunSpec with Services {
    import counts._
    before0 += 1
    describe("A subject") {
      before00 += 1
      describe("when created") {
        before000 += 1
        it("should first test") { firstTestCounts = counts.copy() }
        after000 += 1
      }
      after00 += 1
    }
    middle += 1
    describe("Another subject") {
      before01 += 1
      describe("when created") {
        before010 += 1
        it("should second test") { initialInstance.get.secondTestCounts = counts.copy() }
        after010 += 1
      }
      after01 += 1
    }
    after0 += 1
    override def newInstance = new SiblingDeeplyNestedPathFunSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1, before00 = 1, before000 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, before00 = 1, before000 = 1, after000 = 1, after00 = 1, middle = 2, before01 = 1, before010 = 1, after0 = 1)
    val expectedCounts = Counts(before0 = 2, before00 = 1, before000 = 1, after000 = 1, after00 = 1, middle = 2, before01 = 1, before010 = 1, after010 = 1, after01 = 1,  after0 = 2)
  }

  class AsymetricalDeeplyNestedPathFunSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FunSpec with Services {
    import counts._
    before0 += 1
    describe("A subject") {
      before00 += 1
      describe("when created") {
        before000 += 1
        it("should first test") { firstTestCounts = counts.copy() }
        after000 += 1
      }
      middle += 1
      it("should second test") { initialInstance.get.secondTestCounts = counts.copy() }
      after00 += 1
    }
    after0 += 1
    override def newInstance = new AsymetricalDeeplyNestedPathFunSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1, before00 = 1, before000 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, before00 = 2, before000 = 1, after000 = 1, middle = 2, after00 = 1, after0 = 1)
    val expectedCounts = Counts(before0 = 2, before00 = 2, before000 = 1, after000 = 1, middle = 2, after00 = 2, after0 = 2)
  }

  class EmptyPathFreeSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FreeSpec with Services {
    import counts._
    middle += 1
    override def newInstance = new EmptyPathFreeSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts()
    val expectedSecondTestCounts = Counts()
    val expectedCounts = Counts(middle = 1)
  }
  
  class EmptyNestedPathFreeSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FreeSpec with Services {
    import counts._
    before0 += 1
    "A subject" - {
      middle += 1
    }
    after0 += 1
    override def newInstance = new EmptyNestedPathFreeSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts()
    val expectedSecondTestCounts = Counts()
    val expectedCounts = Counts(before0 = 1, middle = 1, after0 = 1)
  }

  class SiblingEmptyNestedPathFreeSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FreeSpec with Services {
    import counts._
    before0 += 1
    "A subject" - {
      before00 += 1
    }
    middle += 1
    "Another subject" - {
      before01 += 1
    }
    after0 += 1
    override def newInstance = new SiblingEmptyNestedPathFreeSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts()
    val expectedSecondTestCounts = Counts()
    val expectedCounts = Counts(before0 = 2, before00 = 1, middle = 2, before01 = 1, after0 = 2)
  }

  class OneTestSiblingEmptyNestedPathFreeSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FreeSpec with Services {
    import counts._
    before0 += 1
    "A subject" - {
      before00 += 1
    }
    middle += 1
    "Another subject" - {
      before01 += 1
      "first test" in { initialInstance.get.firstTestCounts = counts.copy() }
      after01 += 1
    }
    after0 += 1
    override def newInstance = new OneTestSiblingEmptyNestedPathFreeSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 2, before00 = 1, middle = 2, before01 = 1, after0 = 1)
    val expectedSecondTestCounts = Counts()
    val expectedCounts = Counts(before0 = 2, before00 = 1, middle = 2, before01 = 1, after01 = 1, after0 = 2)
  }

  class OneTestSiblingEmptyDeeplyNestedPathFreeSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FreeSpec with Services {
    import counts._
    before0 += 1
    "A subject" - {
      before00 += 1
    }
    middle += 1
    "Another subject" - {
      before01 += 1
      "when created" - {
        before010 += 1
        "first test" in { initialInstance.get.firstTestCounts = counts.copy() }
        after010 += 1
      }
      after01 += 1
    }
    after0 += 1
    override def newInstance = new OneTestSiblingEmptyDeeplyNestedPathFreeSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 2, before00 = 1, middle = 2, before01 = 1, before010 = 1, after0 = 1)
    val expectedSecondTestCounts = Counts()
    val expectedCounts = Counts(before0 = 2, before00 = 1, middle = 2, before01 = 1, before010 = 1, after010 = 1, after01 = 1, after0 = 2)
  }

  class PathFreeSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FreeSpec with Services {
    import counts._
    before0 += 1
    "first test" in { firstTestCounts = counts.copy() }
    middle += 1
    "second test" in { initialInstance.get.secondTestCounts = counts.copy() }
    after0 += 1
    override def newInstance = new PathFreeSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, middle = 2, after0 = 1)
    val expectedCounts = Counts(before0 = 2, middle = 2, after0 = 2)
  }

  class NestedPathFreeSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FreeSpec with Services {
    import counts._
    before0 += 1
    "A subject" - {
      before00 += 1
      "should first test" in { firstTestCounts = counts.copy() }
      middle += 1
      "should second test" in { initialInstance.get.secondTestCounts = counts.copy() }
      after00 += 1
    }
    after0 += 1
    override def newInstance = new NestedPathFreeSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1, before00 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, before00 = 2, middle = 2, after00 = 1, after0 = 1)
    val expectedCounts = Counts(before0 = 2, before00 = 2, middle = 2, after00 = 2, after0 = 2)
  }

  class SiblingNestedPathFreeSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FreeSpec with Services {
    import counts._
    before0 += 1
    "A subject" - {
      before00 += 1
      "should first test" in { firstTestCounts = counts.copy() }
      after00 += 1
    }
    middle += 1
    "Another subject" - {
      before01 += 1
      "should second test" in { initialInstance.get.secondTestCounts = counts.copy() }
      after01 += 1
    }
    after0 += 1
    override def newInstance = new SiblingNestedPathFreeSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1, before00 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, before00 = 1, after00 = 1, middle = 2, before01 = 1, after0 = 1)
    val expectedCounts = Counts(before0 = 2, before00 = 1, after01 = 1, middle = 2, before01 = 1, after00 = 1, after0 = 2)
  }

  class DeeplyNestedPathFreeSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FreeSpec with Services {
    import counts._
    before0 += 1
    "A subject" - {
      before00 += 1
      "when created" - {
        before000 += 1
        "should first test" in { firstTestCounts = counts.copy() }
        middle += 1
        "should second test" in { initialInstance.get.secondTestCounts = counts.copy() }
        after000 += 1
      }
      after00 += 1
    }
    after0 += 1
    override def newInstance = new DeeplyNestedPathFreeSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1, before00 = 1, before000 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, before00 = 2, before000 = 2, middle = 2, after000 = 1, after00 = 1, after0 = 1)
    val expectedCounts = Counts(before0 = 2, before00 = 2, before000 = 2, middle = 2, after000 = 2, after00 = 2, after0 = 2)
  }

  class SiblingDeeplyNestedPathFreeSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FreeSpec with Services {
    import counts._
    before0 += 1
    "A subject" - {
      before00 += 1
      "when created" - {
        before000 += 1
        "should first test" in { firstTestCounts = counts.copy() }
        after000 += 1
      }
      after00 += 1
    }
    middle += 1
    "Another subject" - {
      before01 += 1 
      "when created" - {
        before010 += 1
        "should second test" in { initialInstance.get.secondTestCounts = counts.copy() }
        after010 += 1
      }
      after01 += 1
    }
    after0 += 1
    override def newInstance = new SiblingDeeplyNestedPathFreeSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1, before00 = 1, before000 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, before00 = 1, before000 = 1, after000 = 1, after00 = 1, middle = 2, before01 = 1, before010 = 1, after0 = 1)
    val expectedCounts = Counts(before0 = 2, before00 = 1, before000 = 1, after000 = 1, after00 = 1, middle = 2, before01 = 1, before010 = 1, after010 = 1, after01 = 1,  after0 = 2)
  }

  class AsymetricalDeeplyNestedPathFreeSpecExample(val counts: Counts, initialInstance: Option[Services] = None) extends path.FreeSpec with Services {
    import counts._
    before0 += 1
    "A subject" - {
      before00 += 1
      "when created" - {
        before000 += 1
        "should first test" in { firstTestCounts = counts.copy() }
        after000 += 1
      }
      middle += 1
      "should second test" in { initialInstance.get.secondTestCounts = counts.copy() }
      after00 += 1
    }
    after0 += 1
    override def newInstance = new AsymetricalDeeplyNestedPathFreeSpecExample(counts, Some(this))
    val expectedFirstTestCounts = Counts(before0 = 1, before00 = 1, before000 = 1)
    val expectedSecondTestCounts = Counts(before0 = 2, before00 = 2, before000 = 1, after000 = 1, middle = 2, after00 = 1, after0 = 1)
    val expectedCounts = Counts(before0 = 2, before00 = 2, before000 = 1, after000 = 1, middle = 2, after00 = 2, after0 = 2)
  }
  
  lazy val emptyPathFunSpec = new EmptyPathFunSpecExample(Counts())
  lazy val emptyNestedPathFunSpec = new EmptyNestedPathFunSpecExample(Counts())
  lazy val siblingEmptyNestedPathFunSpec = new SiblingEmptyNestedPathFunSpecExample(Counts())
  lazy val oneTestSiblingEmptyNestedPathFunSpec = new OneTestSiblingEmptyNestedPathFunSpecExample(Counts())
  lazy val oneTestSiblingEmptyDeeplyNestedPathFunSpec = new OneTestSiblingEmptyDeeplyNestedPathFunSpecExample(Counts())
  lazy val pathFunSpec = new PathFunSpecExample(Counts())
  lazy val nestedPathFunSpec = new NestedPathFunSpecExample(Counts())
  lazy val siblingNestedPathFunSpec = new SiblingNestedPathFunSpecExample(Counts())
  lazy val deeplyNestedPathFunSpec = new DeeplyNestedPathFunSpecExample(Counts())
  lazy val siblingDeeplyNestedPathFunSpec = new SiblingDeeplyNestedPathFunSpecExample(Counts())
  lazy val asymetricalDeeplyNestedPathFunSpec = new AsymetricalDeeplyNestedPathFunSpecExample(Counts())
  lazy val emptyPathFreeSpec = new EmptyPathFreeSpecExample(Counts())
  lazy val emptyNestedPathFreeSpec = new EmptyNestedPathFreeSpecExample(Counts())
  lazy val siblingEmptyNestedPathFreeSpec = new SiblingEmptyNestedPathFreeSpecExample(Counts())
  lazy val oneTestSiblingEmptyNestedPathFreeSpec = new OneTestSiblingEmptyNestedPathFreeSpecExample(Counts())
  lazy val oneTestSiblingEmptyDeeplyNestedPathFreeSpec = new OneTestSiblingEmptyDeeplyNestedPathFreeSpecExample(Counts())
  lazy val pathFreeSpec = new PathFreeSpecExample(Counts())
  lazy val nestedPathFreeSpec = new NestedPathFreeSpecExample(Counts())
  lazy val siblingNestedPathFreeSpec = new SiblingNestedPathFreeSpecExample(Counts())
  lazy val deeplyNestedPathFreeSpec = new DeeplyNestedPathFreeSpecExample(Counts())
  lazy val siblingDeeplyNestedPathFreeSpec = new SiblingDeeplyNestedPathFreeSpecExample(Counts())
  lazy val asymetricalDeeplyNestedPathFreeSpec = new AsymetricalDeeplyNestedPathFreeSpecExample(Counts())
}

