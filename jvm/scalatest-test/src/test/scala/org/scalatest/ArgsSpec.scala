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

import prop.TableDrivenPropertyChecks
import SharedHelpers._
import TableDrivenPropertyChecks._
import matchers.should.Matchers._
import org.scalactic.exceptions.NullArgumentException

class ArgsSpec extends wordspec.AnyWordSpec {
  "The Args constructor" should {
    "throw NullArgumentException when passed a null" in {

      val rep = SilentReporter
      val stp = Stopper.default
      val flt = Filter()
      val cnf = ConfigMap.empty
      val dst = None
      val trk = new Tracker
      
      val invalidCombos =
        Table[Reporter, Stopper, Filter, ConfigMap, Option[Distributor], Tracker](
          ("reporter", "stopper", "filter", "configMap", "distributor", "tracker"),
          (      null,       stp,      flt,         cnf,           dst,       trk),
          (       rep,      null,      flt,         cnf,           dst,       trk),
          (       rep,       stp,     null,         cnf,           dst,       trk),
          (       rep,       stp,      flt,        null,           dst,       trk),
          (       rep,       stp,      flt,         cnf,           null,      trk),
          (       rep,       stp,      flt,         cnf,           dst,      null)
        )

      forAll (invalidCombos) { (reporter, stopper, filter, configMap, distributor, tracker) =>
        a [NullArgumentException] should be thrownBy {
          Args(reporter, stopper, filter, configMap, distributor, tracker)
        }
      }
    }
  }
}
