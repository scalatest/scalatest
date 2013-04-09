package org.scalatest.examples.fixture.configmapfixture

import org.scalatest._

class ExampleSpec extends fixture.FlatSpec with fixture.ConfigMapFixture with ShouldMatchers {

  "The config map" should "contain hello" in { configMap =>
    // Use the configMap passed to runTest in the test
    configMap should contain key "hello"
  }

  it should "contain world" in { configMap =>
    configMap should contain key "world"
  }
}
