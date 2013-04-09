package org.scalatest.examples.funsuite.info

import collection.mutable
import org.scalatest._

class SetSuite extends FunSuite with GivenWhenThen {

  test("An element can be added to an empty mutable Set") {

    Given("an empty mutable Set")
    val set = mutable.Set.empty[String]

    When("an element is added")
    set += "clarity"

    Then("the Set should have size 1")
    assert(set.size === 1)

    And("the Set should contain the added element")
    assert(set.contains("clarity"))

    info("That's all folks!")
  }
}
