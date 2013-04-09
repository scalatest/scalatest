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
import matchers.ShouldMatchers
import prop.TableDrivenPropertyChecks
import scala.collection.mutable.ListBuffer

class PathListBufferExamples extends PathSuiteExamples {

  case class Counts(
    var instanceCount: Int
  )
  
  trait Services {
    val expectedInstanceCount: Int
    val counts: Counts
  }

  type FixtureServices = Services

  class EmptyPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services with ShouldMatchers {

   counts.instanceCount += 1
   val expectedInstanceCount = 7
   
   describe("A ListBuffer") {
      val buf = ListBuffer.empty[Int]
      it("should be empty when created") {
        buf should be ('empty)
        buf += 99 // Mutate to make sure no other test sees this
      }
      describe("when 1 is appended") {
        buf += 1
        it("should contain 1") {
          buf should equal (Seq(1))
          buf += 99 // Mutate to make sure no other test sees this
        }
        describe("when 2 is appended") {
          buf += 2
          it("should contain 1 and 2") {
            buf should equal (Seq(1, 2))
            buf += 99 // Mutate to make sure no other test sees this
          }
          describe("when 2 is removed") {
            buf -= 2
            it("should contain only 1 again") {
              buf should equal (Seq(1))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
          describe("when 3 is appended") { // This describe should not see the removal of 2 done in earlier sibling describe
            buf += 3
            it("should contain 1, 2, and 3") {
              buf should equal (Seq(1, 2, 3))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
        }
        describe("when 88 is appended") {
          buf += 88
          it("should contain 1 and 88") {
            buf should equal (Seq(1, 88))
            buf += 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      it("should again be empty") {
        buf should be ('empty)
      }
    }

    override def newInstance = new EmptyPathFunSpecExample(counts)
  }

  class EmptyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 8
   
    describe("A ListBuffer") {
      val buf = ListBuffer.empty[Int]
      describe("A subject") {
      }
      it("should be empty when created") {
        buf should be ('empty)
        buf += 99 // Mutate to make sure no other test sees this
      }
      describe("when 1 is appended") {
        buf += 1
        it("should contain 1") {
          buf should equal (Seq(1))
          buf += 99 // Mutate to make sure no other test sees this
        }
        describe("when 2 is appended") {
          buf += 2
          it("should contain 1 and 2") {
            buf should equal (Seq(1, 2))
            buf += 99 // Mutate to make sure no other test sees this
          }
          describe("when 2 is removed") {
            buf -= 2
            it("should contain only 1 again") {
              buf should equal (Seq(1))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
          describe("when 3 is appended") { // This describe should not see the removal of 2 done in earlier sibling describe
            buf += 3
            it("should contain 1, 2, and 3") {
              buf should equal (Seq(1, 2, 3))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
        }
        describe("when 88 is appended") {
          buf += 88
          it("should contain 1 and 88") {
            buf should equal (Seq(1, 88))
            buf += 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      it("should again be empty") {
        buf should be ('empty)
      }
    }

    override def newInstance = new EmptyNestedPathFunSpecExample(counts)
  }

  class SiblingEmptyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services with ShouldMatchers {

   counts.instanceCount += 1
   val expectedInstanceCount = 9
   
   describe("A ListBuffer") {
      val buf = ListBuffer.empty[Int]
      describe("A subject") {
      }
      describe("Another subject") {
      }
      it("should be empty when created") {
        buf should be ('empty)
        buf += 99 // Mutate to make sure no other test sees this
      }
      describe("when 1 is appended") {
        buf += 1
        it("should contain 1") {
          buf should equal (Seq(1))
          buf += 99 // Mutate to make sure no other test sees this
        }
        describe("when 2 is appended") {
          buf += 2
          it("should contain 1 and 2") {
            buf should equal (Seq(1, 2))
            buf += 99 // Mutate to make sure no other test sees this
          }
          describe("when 2 is removed") {
            buf -= 2
            it("should contain only 1 again") {
              buf should equal (Seq(1))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
          describe("when 3 is appended") { // This describe should not see the removal of 2 done in earlier sibling describe
            buf += 3
            it("should contain 1, 2, and 3") {
              buf should equal (Seq(1, 2, 3))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
        }
        describe("when 88 is appended") {
          buf += 88
          it("should contain 1 and 88") {
            buf should equal (Seq(1, 88))
            buf += 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      it("should again be empty") {
        buf should be ('empty)
      }
    }

    override def newInstance = new SiblingEmptyNestedPathFunSpecExample(counts)
  }

  class OneTestSiblingEmptyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services with ShouldMatchers {

   counts.instanceCount += 1
   val expectedInstanceCount = 9
   
   describe("A ListBuffer") {
      val buf = ListBuffer.empty[Int]
      describe("A subject") {
      }
      describe("Another subject") {
        it("first test") {
          buf += 1000 // To ensure this isn't seen by others
        }
      }
      it("should be empty when created") {
        buf should be ('empty)
        buf += 99 // Mutate to make sure no other test sees this
      }
      describe("when 1 is appended") {
        buf += 1
        it("should contain 1") {
          buf should equal (Seq(1))
          buf += 99 // Mutate to make sure no other test sees this
        }
        describe("when 2 is appended") {
          buf += 2
          it("should contain 1 and 2") {
            buf should equal (Seq(1, 2))
            buf += 99 // Mutate to make sure no other test sees this
          }
          describe("when 2 is removed") {
            buf -= 2
            it("should contain only 1 again") {
              buf should equal (Seq(1))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
          describe("when 3 is appended") { // This describe should not see the removal of 2 done in earlier sibling describe
            buf += 3
            it("should contain 1, 2, and 3") {
              buf should equal (Seq(1, 2, 3))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
        }
        describe("when 88 is appended") {
          buf += 88
          it("should contain 1 and 88") {
            buf should equal (Seq(1, 88))
            buf += 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      it("should again be empty") {
        buf should be ('empty)
      }
    }

    override def newInstance = new OneTestSiblingEmptyNestedPathFunSpecExample(counts)
  }
  
  class OneTestSiblingEmptyDeeplyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services with ShouldMatchers {

   counts.instanceCount += 1
   val expectedInstanceCount = 9
   
   describe("A ListBuffer") {
      val buf = ListBuffer.empty[Int]
      describe("A subject") {
      }
      describe("Another subject") {
        describe("when created") {
          it("first test") {
            buf += 1000 // To ensure this isn't seen by others
          }
        }
      }
      it("should be empty when created") {
        buf should be ('empty)
        buf += 99 // Mutate to make sure no other test sees this
      }
      describe("when 1 is appended") {
        buf += 1
        it("should contain 1") {
          buf should equal (Seq(1))
          buf += 99 // Mutate to make sure no other test sees this
        }
        describe("when 2 is appended") {
          buf += 2
          it("should contain 1 and 2") {
            buf should equal (Seq(1, 2))
            buf += 99 // Mutate to make sure no other test sees this
          }
          describe("when 2 is removed") {
            buf -= 2
            it("should contain only 1 again") {
              buf should equal (Seq(1))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
          describe("when 3 is appended") { // This describe should not see the removal of 2 done in earlier sibling describe
            buf += 3
            it("should contain 1, 2, and 3") {
              buf should equal (Seq(1, 2, 3))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
        }
        describe("when 88 is appended") {
          buf += 88
          it("should contain 1 and 88") {
            buf should equal (Seq(1, 88))
            buf += 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      it("should again be empty") {
        buf should be ('empty)
      }
    }

    override def newInstance = new OneTestSiblingEmptyDeeplyNestedPathFunSpecExample(counts)
  }

  // These path.FunSpec examples use a Vector an a var
  class PathFunSpecExample(val counts: Counts) extends path.FunSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 7
   
    describe("A Vector") {
      var vec = Vector.empty[Int]
      it("should be empty when created") {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      describe("when 1 is appended") {
        vec :+= 1
        it("should contain 1") {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        describe("when 2 is appended") {
          vec :+= 2
          it("should contain 1 and 2") {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          describe("when 2 is removed") {
            vec = vec.init
            it("should contain only 1 again") {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          describe("when 3 is appended") { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            it("should contain 1, 2, and 3") {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        describe("when 88 is appended") {
          vec :+= 88
          it("should contain 1 and 88") {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      it("should again be empty") {
        vec should be ('empty)
      }
    }

    override def newInstance = new PathFunSpecExample(counts)
  }

  class NestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    describe("A Vector") {
      var vec = Vector.empty[Int]
      describe("A subject") {
        it("should first test") { vec ++= Seq(1, 2, 3) }
        it("should second test") { vec ++= Vector(4, 5, 6) }
      }
      it("should be empty when created") {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      describe("when 1 is appended") {
        vec :+= 1
        it("should contain 1") {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        describe("when 2 is appended") {
          vec :+= 2
          it("should contain 1 and 2") {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          describe("when 2 is removed") {
            vec = vec.init
            it("should contain only 1 again") {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          describe("when 3 is appended") { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            it("should contain 1, 2, and 3") {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        describe("when 88 is appended") {
          vec :+= 88
          it("should contain 1 and 88") {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      it("should again be empty") {
        vec should be ('empty)
      }
    }

    override def newInstance = new NestedPathFunSpecExample(counts)
  }

  class SiblingNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    describe("A Vector") {
      var vec = Vector.empty[Int]
      describe("A subject") {
        it("should first test") { vec ++= Seq(1, 2, 3) }
      }
      describe("Another subject") {
        it("should second test") { vec ++= Vector(4, 5, 6) }
      }
      it("should be empty when created") {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      describe("when 1 is appended") {
        vec :+= 1
        it("should contain 1") {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        describe("when 2 is appended") {
          vec :+= 2
          it("should contain 1 and 2") {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          describe("when 2 is removed") {
            vec = vec.init
            it("should contain only 1 again") {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          describe("when 3 is appended") { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            it("should contain 1, 2, and 3") {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        describe("when 88 is appended") {
          vec :+= 88
          it("should contain 1 and 88") {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      it("should again be empty") {
        vec should be ('empty)
      }
    }

    override def newInstance = new SiblingNestedPathFunSpecExample(counts)
  }

  class DeeplyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    describe("A Vector") {
      var vec = Vector.empty[Int]
      describe("A subject") {
        describe("when created") {
          it("should first test") { vec ++= Seq(1, 2, 3) }
          it("should second test") { vec ++= Vector(4, 5, 6) }
        }
      }
      it("should be empty when created") {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      describe("when 1 is appended") {
        vec :+= 1
        it("should contain 1") {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        describe("when 2 is appended") {
          vec :+= 2
          it("should contain 1 and 2") {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          describe("when 2 is removed") {
            vec = vec.init
            it("should contain only 1 again") {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          describe("when 3 is appended") { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            it("should contain 1, 2, and 3") {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        describe("when 88 is appended") {
          vec :+= 88
          it("should contain 1 and 88") {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      it("should again be empty") {
        vec should be ('empty)
      }
    }

    override def newInstance = new DeeplyNestedPathFunSpecExample(counts)
  }

  class SiblingDeeplyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    describe("A Vector") {
      var vec = Vector.empty[Int]
      describe("A subject") {
        describe("when created") {
          it("should first test") { vec ++= Seq(1, 2, 3) }
        }
      }
      describe("Another subject") {
        describe("when created") {
          it("should second test") { vec ++= Vector(4, 5, 6) }
        }
      }
      it("should be empty when created") {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      describe("when 1 is appended") {
        vec :+= 1
        it("should contain 1") {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        describe("when 2 is appended") {
          vec :+= 2
          it("should contain 1 and 2") {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          describe("when 2 is removed") {
            vec = vec.init
            it("should contain only 1 again") {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          describe("when 3 is appended") { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            it("should contain 1, 2, and 3") {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        describe("when 88 is appended") {
          vec :+= 88
          it("should contain 1 and 88") {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      it("should again be empty") {
        vec should be ('empty)
      }
    }

    override def newInstance = new SiblingDeeplyNestedPathFunSpecExample(counts)
  }

  class AsymetricalDeeplyNestedPathFunSpecExample(val counts: Counts) extends path.FunSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    describe("A Vector") {
      var vec = Vector.empty[Int]
      describe("A subject") {
        describe("when created") {
          it("should first test") { vec ++= Seq(1, 2, 3) }
        }
        it("should second test") { vec ++= Vector(4, 5, 6) }
      }
      it("should be empty when created") {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      describe("when 1 is appended") {
        vec :+= 1
        it("should contain 1") {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        describe("when 2 is appended") {
          vec :+= 2
          it("should contain 1 and 2") {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          describe("when 2 is removed") {
            vec = vec.init
            it("should contain only 1 again") {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          describe("when 3 is appended") { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            it("should contain 1, 2, and 3") {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        describe("when 88 is appended") {
          vec :+= 88
          it("should contain 1 and 88") {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      it("should again be empty") {
        vec should be ('empty)
      }
    }

    override def newInstance = new AsymetricalDeeplyNestedPathFunSpecExample(counts)
  }

  // These path.FreeSpec examples use a ListBuffer and a val
  class EmptyPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 7
   
    "A ListBuffer" - {
      val buf = ListBuffer.empty[Int]
      "should be empty when created" in {
        buf should be ('empty)
        buf += 99 // Mutate to make sure no other test sees this
      }
      "when 1 is appended" - {
        buf += 1
        "should contain 1" in {
          buf should equal (Seq(1))
          buf += 99 // Mutate to make sure no other test sees this
        }
        "when 2 is appended" - {
          buf += 2
          "should contain 1 and 2" in {
            buf should equal (Seq(1, 2))
            buf += 99 // Mutate to make sure no other test sees this
          }
          "when 2 is removed" - {
            buf -= 2
            "should contain only 1 again" in {
              buf should equal (Seq(1))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
          "when 3 is appended" - { // This describe should not see the removal of 2 done in earlier sibling describe
            buf += 3
            "should contain 1, 2, and 3" in {
              buf should equal (Seq(1, 2, 3))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
        }
        "when 88 is appended" - {
          buf += 88
          "should contain 1 and 88" in {
            buf should equal (Seq(1, 88))
            buf += 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      "should again be empty" in {
        buf should be ('empty)
      }
    }

    override def newInstance = new EmptyPathFreeSpecExample(counts)
  }

  class EmptyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 8
   
    "A ListBuffer" - {
      val buf = ListBuffer.empty[Int]
      "A subject" - {
      }
      "should be empty when created" in {
        buf should be ('empty)
        buf += 99 // Mutate to make sure no other test sees this
      }
      "when 1 is appended" - {
        buf += 1
        "should contain 1" in {
          buf should equal (Seq(1))
          buf += 99 // Mutate to make sure no other test sees this
        }
        "when 2 is appended" - {
          buf += 2
          "should contain 1 and 2" in {
            buf should equal (Seq(1, 2))
            buf += 99 // Mutate to make sure no other test sees this
          }
          "when 2 is removed" - {
            buf -= 2
            "should contain only 1 again" in {
              buf should equal (Seq(1))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
          "when 3 is appended" - { // This describe should not see the removal of 2 done in earlier sibling describe
            buf += 3
            "should contain 1, 2, and 3" in {
              buf should equal (Seq(1, 2, 3))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
        }
        "when 88 is appended" - {
          buf += 88
          "should contain 1 and 88" in {
            buf should equal (Seq(1, 88))
            buf += 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      "should again be empty" in {
        buf should be ('empty)
      }
    }

    override def newInstance = new EmptyNestedPathFreeSpecExample(counts)
  }

  class SiblingEmptyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    "A ListBuffer" - {
      val buf = ListBuffer.empty[Int]
      "A subject" - {
      }
      "Another subject" - {
      }
      "should be empty when created" in {
        buf should be ('empty)
        buf += 99 // Mutate to make sure no other test sees this
      }
      "when 1 is appended" - {
        buf += 1
        "should contain 1" in {
          buf should equal (Seq(1))
          buf += 99 // Mutate to make sure no other test sees this
        }
        "when 2 is appended" - {
          buf += 2
          "should contain 1 and 2" in {
            buf should equal (Seq(1, 2))
            buf += 99 // Mutate to make sure no other test sees this
          }
          "when 2 is removed" - {
            buf -= 2
            "should contain only 1 again" in {
              buf should equal (Seq(1))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
          "when 3 is appended" - { // This describe should not see the removal of 2 done in earlier sibling describe
            buf += 3
            "should contain 1, 2, and 3" in {
              buf should equal (Seq(1, 2, 3))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
        }
        "when 88 is appended" - {
          buf += 88
          "should contain 1 and 88" in {
            buf should equal (Seq(1, 88))
            buf += 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      "should again be empty" in {
        buf should be ('empty)
      }
    }

    override def newInstance = new SiblingEmptyNestedPathFreeSpecExample(counts)
  }

  class OneTestSiblingEmptyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
   
    "A ListBuffer" - {
      val buf = ListBuffer.empty[Int]
      "A subject" - {
      }
      "Another subject" - {
        "first test" - {
          buf += 1000 // To ensure this isn't seen by others
        }
      }
      "should be empty when created" in {
        buf should be ('empty)
        buf += 99 // Mutate to make sure no other test sees this
      }
      "when 1 is appended" - {
        buf += 1
        "should contain 1" in {
          buf should equal (Seq(1))
          buf += 99 // Mutate to make sure no other test sees this
        }
        "when 2 is appended" - {
          buf += 2
          "should contain 1 and 2" in {
            buf should equal (Seq(1, 2))
            buf += 99 // Mutate to make sure no other test sees this
          }
          "when 2 is removed" - {
            buf -= 2
            "should contain only 1 again" in {
              buf should equal (Seq(1))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
          "when 3 is appended" - { // This describe should not see the removal of 2 done in earlier sibling describe
            buf += 3
            "should contain 1, 2, and 3" in {
              buf should equal (Seq(1, 2, 3))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
        }
        "when 88 is appended" - {
          buf += 88
          "should contain 1 and 88" in {
            buf should equal (Seq(1, 88))
            buf += 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      "should again be empty" in {
        buf should be ('empty)
      }
    }

    override def newInstance = new OneTestSiblingEmptyNestedPathFreeSpecExample(counts)
  }
  
  class OneTestSiblingEmptyDeeplyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    "A ListBuffer" - {
      val buf = ListBuffer.empty[Int]
      "A subject" - {
      }
      "Another subject" - {
        "when created" - {
          "first test" - {
            buf += 1000 // To ensure this isn't seen by others
          }
        }
      }
      "should be empty when created" in {
        buf should be ('empty)
        buf += 99 // Mutate to make sure no other test sees this
      }
      "when 1 is appended" - {
        buf += 1
        "should contain 1" in {
          buf should equal (Seq(1))
          buf += 99 // Mutate to make sure no other test sees this
        }
        "when 2 is appended" - {
          buf += 2
          "should contain 1 and 2" in {
            buf should equal (Seq(1, 2))
            buf += 99 // Mutate to make sure no other test sees this
          }
          "when 2 is removed" - {
            buf -= 2
            "should contain only 1 again" in {
              buf should equal (Seq(1))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
          "when 3 is appended" - { // This describe should not see the removal of 2 done in earlier sibling describe
            buf += 3
            "should contain 1, 2, and 3" in {
              buf should equal (Seq(1, 2, 3))
              buf += 99 // Mutate to make sure no other test sees this
            }
          }
        }
        "when 88 is appended" - {
          buf += 88
          "should contain 1 and 88" in {
            buf should equal (Seq(1, 88))
            buf += 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      "should again be empty" in {
        buf should be ('empty)
      }
    }

    override def newInstance = new OneTestSiblingEmptyDeeplyNestedPathFreeSpecExample(counts)
  }

   // These path.FreeSpec examples use a Vector and a var
  class PathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 7
   
    "A Vector" - {
      var vec = Vector.empty[Int]
      "should be empty when created" in {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      "when 1 is appended" - {
        vec :+= 1
        "should contain 1" in {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        "when 2 is appended" - {
          vec :+= 2
          "should contain 1 and 2" in {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          "when 2 is removed" - {
            vec = vec.init
            "should contain only 1 again" in {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          "when 3 is appended" - { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            "should contain 1, 2, and 3" in {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        "when 88 is appended" - {
          vec :+= 88
          "should contain 1 and 88" in {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      "should again be empty" in {
        vec should be ('empty)
      }
    }

    override def newInstance = new PathFreeSpecExample(counts)
  }

  class NestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    "A Vector" - {
      var vec = Vector.empty[Int]
      "A subject" - {
        "should first test" in {
          vec = Vector(1, 2, 3)
        }
        "should second test" in {
          vec ++= Vector(1, 2, 3)
        }
      }
      "should be empty when created" in {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      "when 1 is appended" - {
        vec :+= 1
        "should contain 1" in {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        "when 2 is appended" - {
          vec :+= 2
          "should contain 1 and 2" in {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          "when 2 is removed" - {
            vec = vec.init
            "should contain only 1 again" in {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          "when 3 is appended" - { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            "should contain 1, 2, and 3" in {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        "when 88 is appended" - {
          vec :+= 88
          "should contain 1 and 88" in {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      "should again be empty" in {
        vec should be ('empty)
      }
    }

    override def newInstance = new NestedPathFreeSpecExample(counts)
  }

  class SiblingNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    "A Vector" - {
      var vec = Vector.empty[Int]
      "A subject" - {
        "should first test" in {
          vec = Vector(1, 2, 3)
        }
      }
      "Another subject" - {
        "should second test" in {
          vec ++= Vector(1, 2, 3)
        }
      }
      "should be empty when created" in {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      "when 1 is appended" - {
        vec :+= 1
        "should contain 1" in {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        "when 2 is appended" - {
          vec :+= 2
          "should contain 1 and 2" in {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          "when 2 is removed" - {
            vec = vec.init
            "should contain only 1 again" in {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          "when 3 is appended" - { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            "should contain 1, 2, and 3" in {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        "when 88 is appended" - {
          vec :+= 88
          "should contain 1 and 88" in {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      "should again be empty" in {
        vec should be ('empty)
      }
    }

    override def newInstance = new SiblingNestedPathFreeSpecExample(counts)
  }

  class DeeplyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    "A Vector" - {
      var vec = Vector.empty[Int]
      "A subject" - {
        "when created" - {
          "should first test" in {
            vec = Vector(1, 2, 3)
          }
          "should second test" in {
            vec ++= Vector(1, 2, 3)
          }
        }
      }
      "should be empty when created" in {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      "when 1 is appended" - {
        vec :+= 1
        "should contain 1" in {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        "when 2 is appended" - {
          vec :+= 2
          "should contain 1 and 2" in {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          "when 2 is removed" - {
            vec = vec.init
            "should contain only 1 again" in {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          "when 3 is appended" - { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            "should contain 1, 2, and 3" in {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        "when 88 is appended" - {
          vec :+= 88
          "should contain 1 and 88" in {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      "should again be empty" in {
        vec should be ('empty)
      }
    }

    override def newInstance = new DeeplyNestedPathFreeSpecExample(counts)
  }

  class SiblingDeeplyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    "A Vector" - {
      var vec = Vector.empty[Int]
      "A subject" - {
        "when created" - {
          "should first test" in {
            vec = Vector(1, 2, 3)
          }
        }
      }
      "Another subject" - {
        "when created" - {
          "should second test" in {
            vec ++= Vector(1, 2, 3)
          }
        }
      }
      "should be empty when created" in {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      "when 1 is appended" - {
        vec :+= 1
        "should contain 1" in {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        "when 2 is appended" - {
          vec :+= 2
          "should contain 1 and 2" in {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          "when 2 is removed" - {
            vec = vec.init
            "should contain only 1 again" in {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          "when 3 is appended" - { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            "should contain 1, 2, and 3" in {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        "when 88 is appended" - {
          vec :+= 88
          "should contain 1 and 88" in {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      "should again be empty" in {
        vec should be ('empty)
      }
    }

    override def newInstance = new SiblingDeeplyNestedPathFreeSpecExample(counts)
  }

  class AsymetricalDeeplyNestedPathFreeSpecExample(val counts: Counts) extends path.FreeSpec with Services with ShouldMatchers {

    counts.instanceCount += 1
    val expectedInstanceCount = 9
   
    "A Vector" - {
      var vec = Vector.empty[Int]
      "A subject" - {
        "when created" - {
          "should first test" in {
            vec = Vector(1, 2, 3)
          }
        }
        "should second test" in {
          vec ++= Vector(1, 2, 3)
        }
      }
      "should be empty when created" in {
        vec should be ('empty)
        vec :+= 99 // Mutate to make sure no other test sees this
      }
      "when 1 is appended" - {
        vec :+= 1
        "should contain 1" in {
          vec should equal (Seq(1))
          vec :+= 99 // Mutate to make sure no other test sees this
        }
        "when 2 is appended" - {
          vec :+= 2
          "should contain 1 and 2" in {
            vec should equal (Seq(1, 2))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
          "when 2 is removed" - {
            vec = vec.init
            "should contain only 1 again" in {
              vec should equal (Seq(1))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
          "when 3 is appended" - { // This describe should not see the removal of 2 done in earlier sibling describe
            vec :+= 3
            "should contain 1, 2, and 3" in {
              vec should equal (Seq(1, 2, 3))
              vec :+= 99 // Mutate to make sure no other test sees this
            }
          }
        }
        "when 88 is appended" - {
          vec :+= 88
          "should contain 1 and 88" in {
            vec should equal (Seq(1, 88))
            vec :+= 99 // Mutate to make sure no other test sees this
          }
        }
      }
      // At end of previous describe, buf equaled List(1). Now doing it again to make
      // sure that it is empty
      "should again be empty" in {
        vec should be ('empty)
      }
    }

    override def newInstance = new AsymetricalDeeplyNestedPathFreeSpecExample(counts)
  }

  lazy val emptyPathFunSpec = new EmptyPathFunSpecExample(Counts(0))
  lazy val emptyNestedPathFunSpec = new EmptyNestedPathFunSpecExample(Counts(0))
  lazy val siblingEmptyNestedPathFunSpec = new SiblingEmptyNestedPathFunSpecExample(Counts(0))
  lazy val oneTestSiblingEmptyNestedPathFunSpec = new OneTestSiblingEmptyNestedPathFunSpecExample(Counts(0))
  lazy val oneTestSiblingEmptyDeeplyNestedPathFunSpec = new OneTestSiblingEmptyDeeplyNestedPathFunSpecExample(Counts(0))
  lazy val pathFunSpec = new PathFunSpecExample(Counts(0))
  lazy val nestedPathFunSpec = new NestedPathFunSpecExample(Counts(0))
  lazy val siblingNestedPathFunSpec = new SiblingNestedPathFunSpecExample(Counts(0))
  lazy val deeplyNestedPathFunSpec = new DeeplyNestedPathFunSpecExample(Counts(0))
  lazy val siblingDeeplyNestedPathFunSpec = new SiblingDeeplyNestedPathFunSpecExample(Counts(0))
  lazy val asymetricalDeeplyNestedPathFunSpec = new AsymetricalDeeplyNestedPathFunSpecExample(Counts(0))
  lazy val emptyPathFreeSpec = new EmptyPathFreeSpecExample(Counts(0))
  lazy val emptyNestedPathFreeSpec = new EmptyNestedPathFreeSpecExample(Counts(0))
  lazy val siblingEmptyNestedPathFreeSpec = new SiblingEmptyNestedPathFreeSpecExample(Counts(0))
  lazy val oneTestSiblingEmptyNestedPathFreeSpec = new OneTestSiblingEmptyNestedPathFreeSpecExample(Counts(0))
  lazy val oneTestSiblingEmptyDeeplyNestedPathFreeSpec = new OneTestSiblingEmptyDeeplyNestedPathFreeSpecExample(Counts(0))
  lazy val pathFreeSpec = new PathFreeSpecExample(Counts(0))
  lazy val nestedPathFreeSpec = new NestedPathFreeSpecExample(Counts(0))
  lazy val siblingNestedPathFreeSpec = new SiblingNestedPathFreeSpecExample(Counts(0))
  lazy val deeplyNestedPathFreeSpec = new DeeplyNestedPathFreeSpecExample(Counts(0))
  lazy val siblingDeeplyNestedPathFreeSpec = new SiblingDeeplyNestedPathFreeSpecExample(Counts(0))
  lazy val asymetricalDeeplyNestedPathFreeSpec = new AsymetricalDeeplyNestedPathFreeSpecExample(Counts(0))
}

