package org.scalatest.examples.suite.ignore

import org.scalatest._
import prop._
import scala.collection.immutable._
import java.util.NoSuchElementException

class SetSpec extends PropSpec with TableDrivenPropertyChecks with ShouldMatchers {

  val examples =
    Table(
      "set",
      BitSet.empty,
      HashSet.empty[Int],
      TreeSet.empty[Int]
    )

  ignore("an empty Set should have size 0") {
    forAll(examples) { set =>
      set.size should be (0)
    }
  }

  property("invoking head on an empty set should produce NoSuchElementException") {
    forAll(examples) { set =>
      evaluating { set.head } should produce [NoSuchElementException]
    }
  }
}