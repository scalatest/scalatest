package org.scalatest.examples.propspec.pending

import org.scalatest._
import prop._
import scala.collection.immutable._

class SetSpec extends PropSpec with TableDrivenPropertyChecks with ShouldMatchers {

  val examples =
    Table(
      "set",
      BitSet.empty,
      HashSet.empty[Int],
      TreeSet.empty[Int]
    )

  property("an empty Set should have size 0") (pending)

  property("invoking head on an empty set should produce NoSuchElementException") {
    forAll(examples) { set =>
      evaluating { set.head } should produce [NoSuchElementException]
    }
  }
}