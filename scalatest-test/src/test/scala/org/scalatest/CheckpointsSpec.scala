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
package org.scalatest

import org.scalatest.Checkpoints._
import org.scalatest.OptionValues._
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.junit.JUnitTestFailedError
import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.exceptions.TestRegistrationClosedException
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.DuplicateTestNameException
import Matchers._

class CheckpointsSpec extends FunSpec with AssertionsForJUnit {

  describe("a Checkpoint") {
    describe("with a failure condition") {

      it("should throw a TestFailedException when reportAll is called") {
        val cp = new Checkpoint
        cp { 1 should equal (2) }
        val caught = 
          the [TestFailedException] thrownBy {
            cp.reportAll()
          }

        val failConditionLineNumber = thisLineNumber - 6
        val reportAllLineNumber = failConditionLineNumber + 3

        caught.failedCodeLineNumber.value should equal (reportAllLineNumber)
        caught.failedCodeFileName.value should be ("CheckpointsSpec.scala")
        caught.getMessage should include (Resources.atCheckpointAt +
                                          " CheckpointsSpec.scala:" +
                                          failConditionLineNumber)
      }

      it("should not throw an exception if reportAll is not called") {
        val cp = new Checkpoint
        cp { 1 should equal (2) }
      }
    }

    describe("with multiple failure conditions") {

      it("should report all failures when reportAll is called") {
        val cp = new Checkpoint
        val caught = 
          the [TestFailedException] thrownBy {
            cp { 1 should equal (2) }
            cp { 3 should equal (2) }
            cp.reportAll()
          }

        val failCondition1LineNumber = thisLineNumber - 5
        val failCondition2LineNumber = failCondition1LineNumber + 1
        val reportAllLineNumber      = failCondition1LineNumber + 2

        caught.failedCodeLineNumber.value should equal (reportAllLineNumber)
        caught.failedCodeFileName.value should be ("CheckpointsSpec.scala")

        caught.getMessage should include (Resources.atCheckpointAt +
                                          " CheckpointsSpec.scala:" +
                                          failCondition1LineNumber)

        caught.getMessage should include (Resources.atCheckpointAt +
                                          " CheckpointsSpec.scala:" +
                                          failCondition2LineNumber)
      }
    }

    describe("with a success condition") {
      it("should not throw an exception") {
        val cp = new Checkpoint
        cp { 1 should equal (1) }
        cp.reportAll()
      }
    } 
    describe("when a TestCanceledException is thrown") {
      it("should pass the TestCanceledException through immediately") {
        val cp = new Checkpoint
        a [TestCanceledException] should be thrownBy {
          cp { cancel("This should not be captured by the Checkpoint") }
        }
      }
    } 
    describe("when a TestRegistrationClosedException is thrown") {
      it("should pass the TestRegistrationClosedException through immediately") {
        val cp = new Checkpoint
        a [TestRegistrationClosedException] should be thrownBy {
          cp { throw new TestRegistrationClosedException("This should not be captured by the Checkpoint", 0) }
        }
      }
    } 
    describe("when a NotAllowedException is thrown") {
      it("should pass the NotAllowedException through immediately") {
        val cp = new Checkpoint
        a [NotAllowedException] should be thrownBy {
          cp { throw new NotAllowedException("This should not be captured by the Checkpoint", 0) }
        }
      }
    } 
    describe("when a DuplicateTestNameException is thrown") {
      it("should pass the DuplicateTestNameException through immediately") {
        val cp = new Checkpoint
        a [DuplicateTestNameException] should be thrownBy {
          cp { throw new DuplicateTestNameException("This should not be captured by the Checkpoint", 0) }
        }
      }
    } 
  } 

  describe("a Checkpoint using AssertionsForJUnit") {
    describe("with a failure condition") {

      it("should throw a JUnitTestFailedError when reportAll is called") {
        val cp = new Checkpoint
        val caught = 
          the [TestFailedException] thrownBy {
            cp { assert(1 === 2) }
            cp.reportAll()
          }

        val failConditionLineNumber = thisLineNumber - 4
        val reportAllLineNumber = failConditionLineNumber + 1

        caught.failedCodeLineNumber.value should equal (reportAllLineNumber)
        caught.failedCodeFileName.value should be ("CheckpointsSpec.scala")
        caught.getMessage should include (Resources.atCheckpointAt +
                                          " CheckpointsSpec.scala:" +
                                          failConditionLineNumber)
      }
    }
  }
}
