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
package org.scalatest

import SharedHelpers._
import org.scalatest.funspec.AnyFunSpec

class StepwiseNestedSuiteExecutionSpec extends AnyFunSpec {
  describe("the StepwiseNestedSuiteExecution trait") {
    describe("when mixed into a Suite") {
      it("should override runNestedSuites such that it calls run on nested suites in stepwise order, but with the distributor passed as is") {
        class SuperSuite extends Suite {
          @volatile var distributorWasPropagated = false
          @volatile var firstNestedSuiteHasBeenRun = false
          @volatile var lastNestedSuiteWasRunAfterFirst = false
          class FirstSuite extends Suite {
            override def run(testName: Option[String], args: Args): Status = {
              firstNestedSuiteHasBeenRun = true
              if (args.distributor.isDefined)
                distributorWasPropagated = true
              super.run(testName, args)
            }
          }
          class LastSuite extends Suite {
            override def run(testName: Option[String], args: Args): Status = {
              if (firstNestedSuiteHasBeenRun)
                lastNestedSuiteWasRunAfterFirst = true
              super.run(testName, args)
            }
          }
          var distributorWasDefined: Boolean = false
          var superRunNestedSuitesWasInvoked = false
          override def nestedSuites = Vector(new FirstSuite, new LastSuite)
          override protected def runNestedSuites(args: Args): Status = {
            superRunNestedSuitesWasInvoked = true
            if (args.distributor.isDefined)
              distributorWasDefined = true
            super.runNestedSuites(args)
          }
        }
        class ParSubSuite extends SuperSuite
        class SeqSubSuite extends SuperSuite with StepwiseNestedSuiteExecution
        val par = new ParSubSuite
        val stp = new SeqSubSuite
        // SKIP-SCALATESTJS-START
        val execService = java.util.concurrent.Executors.newFixedThreadPool(2)
        val execService2 = java.util.concurrent.Executors.newFixedThreadPool(2)
        val distributor = new TestConcurrentDistributor(execService)
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY val distributor = new TestConcurrentDistributor()
        try {
          val parStatus = par.run(None, Args(SilentReporter, distributor = Some(distributor)))
          // SKIP-SCALATESTJS-START
          parStatus.waitUntilCompleted()
          // SKIP-SCALATESTJS-END
          assert(par.superRunNestedSuitesWasInvoked )
          assert(par.distributorWasDefined)
          assert(par.distributorWasPropagated) // This flickers. Is this a problem with volatile?

          // SKIP-SCALATESTJS-START
          val distributor2 = new TestConcurrentDistributor(execService2)
          // SKIP-SCALATESTJS-END
          //SCALATESTJS-ONLY val distributor2 = new TestConcurrentDistributor()

          val stpStatus = stp.run(None, Args(SilentReporter, distributor = Some(distributor2)))
          assert(stpStatus.isCompleted()) // When a stepwise execution returns, the whole thing should be completed already, even though some of it may have run in parallel
          assert(!stp.superRunNestedSuitesWasInvoked )
          assert(!stp.distributorWasDefined)
          assert(stp.distributorWasPropagated)
          assert(stp.lastNestedSuiteWasRunAfterFirst)
        }
        finally {
          // SKIP-SCALATESTJS-START
          execService.shutdown()
          execService2.shutdown()
          // SKIP-SCALATESTJS-END
        }
      }
    }
  }
}
