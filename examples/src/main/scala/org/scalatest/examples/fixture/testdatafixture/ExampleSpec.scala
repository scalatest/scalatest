package org.scalatest.examples.fixture.testdatafixture

import org.scalatest._

class ExampleSpec extends fixture.FlatSpec with fixture.TestDataFixture {

  "Accessing the test data" should "be easy!" in { td =>
    assert(td.name == "Accessing the test data should be easy!")
  }

  it should "be fun!" in { td =>
    assert(td.name == "Accessing the test data should be fun!")
  }
}

