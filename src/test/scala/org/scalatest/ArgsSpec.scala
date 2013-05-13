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

import matchers.ShouldMatchers
import prop.TableDrivenPropertyChecks
import SharedHelpers._

class ArgsSpec extends WordSpec with TableDrivenPropertyChecks with ShouldMatchers with SeveredStackTraces {
  "The Args constructor" should {
    "throw NullPointerExcepion when passed a null" in {

      val rep = SilentReporter
      val stp = Stopper.default
      val flt = Filter()
      val cnf = ConfigMap.empty
      val dst = None
      val trk = new Tracker
      val sty = Set.empty[String]
      
      val invalidCombos =
        Table(
          ("reporter", "stopper", "filter", "configMap", "distributor", "tracker", "chosenStyles"),
          (      null,       stp,      flt,         cnf,           dst,       trk,            sty),
          (       rep,      null,      flt,         cnf,           dst,       trk,            sty),
          (       rep,       stp,     null,         cnf,           dst,       trk,            sty),
          (       rep,       stp,      flt,        null,           dst,       trk,            sty),
          (       rep,       stp,      flt,         cnf,           null,      trk,            sty),
          (       rep,       stp,      flt,         cnf,           dst,      null,            sty),
          (       rep,       stp,      flt,         cnf,           dst,       trk,           null)
        )

      forAll (invalidCombos) { (reporter, stopper, filter, configMap, distributor, tracker, chosenStyles) =>
        evaluating {
          Args(reporter, stopper, filter, configMap, distributor, tracker, chosenStyles)
        } should produce [NullPointerException]
      }
    }
  }

  "The deprecated run method" should {
    "call the new run method" in {
      class MySuite extends Suite {
        var newRunGotCalled = false
        override def run(testName: Option[String], args: Args): Status = {
          newRunGotCalled = true
          SucceededStatus
        }
      }
      val s = new MySuite
      s.run(None, SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker)
      assert(s.newRunGotCalled)
    }
  }
}
