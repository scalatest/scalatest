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
package org.scalatest.concurrent

import org.scalatest.FeatureSpec
import org.scalatest.time.{Millis, Seconds, Span}

class IntegrationPatienceSpec extends FeatureSpec with Eventually with IntegrationPatience {
  feature("IntegrationPatience") {
    scenario("User mixes in after Eventually") {
      // It should compile (had a bug where it didn't)
      val expectedPatienceConfig = PatienceConfig(
        timeout = scaled(Span(15, Seconds)),
        interval = scaled(Span(150, Millis))
      )
      assert(patienceConfig == expectedPatienceConfig)
    }
  }
}
