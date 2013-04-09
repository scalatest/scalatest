/*
package org.scalatest.examples.suite.info

import collection.mutable
import org.scalatest._

class SetSuite extends Suite with GivenWhenThen {

  def `test: an element can be added to an empty mutable Set` {

    given("an empty mutable Set")
    val set = mutable.Set.empty[String]

    when("an element is added")
    set += "clarity"

    then("the Set should have size 1")
    assert(set.size === 1)

    and("the Set should contain the added element")
    assert(set.contains("clarity"))

    info("That's all folks!")
  }
}
*/
