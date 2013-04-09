package org.scalatest.examples.propspec.tagging

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

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

  property("an empty Set should have size 0", SlowTest) {
    forAll(examples) { set =>
      set.size should be (0)
    }
  }

  property("invoking head on an empty set should produce NoSuchElementException",
      SlowTest, DbTest) {

    forAll(examples) { set =>
      evaluating { set.head } should produce [NoSuchElementException]
    }
  }
}