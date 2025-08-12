/*
 * Copyright 2001-2025 Artima, Inc.
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

// SKIP-SCALATESTJS,NATIVE-START
import org.scalatest.tools.Runner
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ScaledTimeSpansSpec extends AnyFunSpec with Matchers with ScaledTimeSpans {

    describe("ScaledTimeSpans") {
      
      it("should use Runner's spanScaleFactor by default") {
        // SKIP-SCALATESTJS,NATIVE-START
        assert(spanScaleFactor === Runner.spanScaleFactor)
        // SKIP-SCALATESTJS,NATIVE-END
        //SCALATESTJS,NATIVE-ONLY assert(spanScaleFactor === 1.0)
        // These test may cause other test that use eventually to failed if run in concurrent.
        // May be we could find a better way to test this.
        //Runner.spanScaleFactor = 2.0
        //assert(spanScaleFactor === 2.0)
        // Reset back to original, else it'll affect other tests.
        //Runner.spanScaleFactor = original
      }
      
    }
  
}
