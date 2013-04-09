package org.scalatest.examples.propspec.fixturecontext

import org.scalatest._
import org.scalatest.prop._
import scala.collection.immutable._

trait SetExamples extends Tables {
  def examples =
    Table(
      "set",
      bitSet,
      hashSet,
      treeSet
    )

  def bitSet: BitSet
  def hashSet: HashSet[Int]
  def treeSet: TreeSet[Int]
}

class EmptySetExamples extends SetExamples {
  def bitSet = BitSet.empty
  def hashSet = HashSet.empty[Int]
  def treeSet = TreeSet.empty[Int]
}

class SetSpec extends PropSpec with TableDrivenPropertyChecks with ShouldMatchers {

  property("an empty Set should have size 0") {
    new EmptySetExamples {
      forAll(examples) { set =>
        set.size should be (0)
      }
    }
  }

  property("invoking head on an empty set should produce NoSuchElementException") {
    new EmptySetExamples {
      forAll(examples) { set =>
        evaluating { set.head } should produce [NoSuchElementException]
      }
    }
  }
}



