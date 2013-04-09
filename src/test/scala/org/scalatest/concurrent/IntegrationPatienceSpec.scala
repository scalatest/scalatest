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
