package org.scalatest.concurrent

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.tools.Runner

class ScaledTimeSpansSpec extends FunSpec with ShouldMatchers with ScaledTimeSpans {

    describe("ScaledTimeSpans") {
      
      it("should use Runner's spanScaleFactor by default") {
        assert(spanScaleFactor === Runner.spanScaleFactor)
        // These test may cause other test that use eventually to failed if run in concurrent.
        // May be we could find a better way to test this.
        //Runner.spanScaleFactor = 2.0
        //assert(spanScaleFactor === 2.0)
        // Reset back to original, else it'll affect other tests.
        //Runner.spanScaleFactor = original
      }
      
    }
  
}