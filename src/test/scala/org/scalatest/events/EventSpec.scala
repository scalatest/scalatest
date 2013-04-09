package org.scalatest.events

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._

class EventSpec extends FunSpec with Checkers {
/*
  describe("An TestStarting Event") {
    describe("(with different runStamps)") {
      it("should sort into order by runStamp") {
        check(
          (runStampA: Int, runStampB: Int, suiteStamp: List[Int], testStamp: Int, ordinal: Int) =>
            (runStampA != runStampB) ==> {
              val unsorted =
                List(
                  TestStarting("X - test", "X", Some("com.acme.X"), "test", runStampA, suiteStamp, testStamp, ordinal),
                  TestStarting("Y - test", "Y", Some("com.acme.Y"), "test", runStampB, suiteStamp, testStamp, ordinal)
                )
              val sorted = unsorted.sort(_ < _)
              sorted.head.runStamp < sorted.tail.head.runStamp
            }
        )
      }
    }

    describe("(with same runStamps but different suiteStamps of the same length)") {
      it("should sort into order by suiteStamp") {
        check(
          (runStamp: Int, suiteStampA: List[Int], suiteStampB: List[Int], testStamp: Int, ordinal: Int) =>
            (suiteStampA != suiteStampB && !suiteStampA.isEmpty && !suiteStampB.isEmpty) ==> {
              val length = if (suiteStampA.length > suiteStampB.length) suiteStampB.length else suiteStampA.length
              val properLengthA = if (suiteStampA.length != length) suiteStampA take length else suiteStampA
              val properLengthB = if (suiteStampB.length != length) suiteStampB take length else suiteStampB
              val unequalA = if (properLengthA == properLengthB) (properLengthA.head + 1) :: properLengthB.tail else properLengthA
              val unsorted =
                List(
                  TestStarting("X - test", "X", Some("com.acme.X"), "test", runStamp, unequalA, testStamp, ordinal),
                  TestStarting("Y - test", "Y", Some("com.acme.Y"), "test", runStamp, properLengthB, testStamp, ordinal)
                )
              val sorted = unsorted.sort(_ < _)
              val first = sorted.head.suiteStamp.get.reverse
              val second = sorted.tail.head.suiteStamp.get.reverse
              val zipped = first zip second
              val unequalPair = zipped find (pair => pair._1 != pair._2)
              unequalPair match {
                case Some((firstElement, secondElement)) => firstElement < secondElement
                case None => fail() // should never happen
              }
            }
        )
      }
    }

    describe("(with same runStamps, suiteStamps, but different ordinals)") {
      it("should sort into order by ordinal") {
        check(
          (runStamp: Int, suiteStamp: List[Int], testStamp: Int, ordinalA: Int, ordinalB: Int) =>
            (ordinalA != ordinalB) ==> {
              val length = if (suiteStampA.length > suiteStampB.length) suiteStampB.length else suiteStampA.length
              val properLengthA = if (suiteStampA.length != length) suiteStampA take length else suiteStampA
              val properLengthB = if (suiteStampB.length != length) suiteStampB take length else suiteStampB
              val unequalA = if (properLengthA == properLengthB) (properLengthA.head + 1) :: properLengthB.tail else properLengthA
              val unsorted =
                List(
                  TestStarting("X - test", "X", Some("com.acme.X"), "test", runStamp, unequalA, testStamp, ordinal),
                  TestStarting("Y - test", "Y", Some("com.acme.Y"), "test", runStamp, properLengthB, testStamp, ordinal)
                )
              val sorted = unsorted.sort(_ < _)
              val first = sorted.head.suiteStamp.get.reverse
              val second = sorted.tail.head.suiteStamp.get.reverse
              val zipped = first zip second
              val unequalPair = zipped find (pair => pair._1 != pair._2)
              unequalPair match {
                case Some((firstElement, secondElement)) => firstElement < secondElement
                case None => fail() // should never happen
              }
            }
        )
      }
    }
  }
*/
}
