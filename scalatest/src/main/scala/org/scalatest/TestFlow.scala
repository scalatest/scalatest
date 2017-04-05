/*
 * Copyright 2001-2017 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest

import org.scalatest.events.{MotionToSuppress, TestStarting, TestSucceeded}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

import org.scalatest.exceptions.DuplicateTestNameException
import org.scalactic.source

trait Test0[A] { thisTest0 =>
  def apply(): A // This is the test function, like what we pass into withFixture
  def name: String
  def testNames: Set[String]
  def andThen[B](next: TestFlow[A, B])(implicit pos: source.Position): Test0[B] = {
    thisTest0.testNames.find(tn => next.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }
    new Test0[B] {
      def apply(): B = next(thisTest0())
      val name = thisTest0.name
      def testNames: Set[String] = thisTest0.testNames ++ next.testNames // TODO: Ensure iterator order is reasonable, either depth or breadth first
      override def runTests(testName: Option[String], args: Args): B = {
        args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
          None, None))
        val res0 = thisTest0()
        args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None,
          None, None))
        next.runTests(testName, args, res0)
      }
    }
  }
  def runTests(testName: Option[String], args: Args): A = {
    args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
      None, None))
    val result = thisTest0()
    args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None,
      None, None))
    result
  }
}

object Test0 {
  def apply[A](testName: String)(f: => A): Test0[A] =
    new Test0[A] {
      def apply(): A = f
      val name: String = testName
      def testNames: Set[String] = Set(testName) 
    }
}

trait TestFlow[A, B] { thisTestFlow =>
  def apply(a: A): B // This is the test function, like what we pass into withFixture
  def name: String
  def andThen[C](next: TestFlow[B, C])(implicit pos: source.Position): TestFlow[A, C] = {
    thisTestFlow.testNames.find(tn => next.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }

    new TestFlow[A, C] {
      def apply(a: A): C = next(thisTestFlow(a))

      val name = thisTestFlow.name

      def testNames: Set[String] = thisTestFlow.testNames ++ next.testNames // TODO: Ensure iterator order is reasonable, either depth or breadth first
      override def runTests(testName: Option[String], args: Args, input: A): C = {
        args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
          None, None))
        val res0 = thisTestFlow(input)
        args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None,
          None, None))
        next.runTests(testName, args, res0)
      }
    }
  }
  def compose[C](prev: TestFlow[C, A])(implicit pos: source.Position): TestFlow[C, B] = {
    thisTestFlow.testNames.find(tn => prev.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }

    new TestFlow[C, B] {
      def apply(c: C): B = thisTestFlow(prev(c))

      val name = prev.name

      def testNames: Set[String] = prev.testNames ++ thisTestFlow.testNames

      override def runTests(testName: Option[String], args: Args, input: C): B = {
        val res0 = prev.runTests(testName, args, input)
        args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
          None, None))
        val result = thisTestFlow(res0)
        args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None,
          None, None))
        result
      }
    }
  }
  def compose(prev: Test0[A])(implicit pos: source.Position): Test0[B] = {
    thisTestFlow.testNames.find(tn => prev.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }

    new Test0[B] {
      def apply(): B = thisTestFlow(prev())

      val name = prev.name

      def testNames: Set[String] = prev.testNames ++ thisTestFlow.testNames

      override def runTests(testName: Option[String], args: Args): B = {
        val res0 = prev.runTests(testName, args)
        args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
          None, None))
        val result = thisTestFlow(res0)
        args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None,
          None, None))
        result
      }
    }
  }
  def testNames: Set[String]
  def runTests(testName: Option[String], args: Args, input: A): B = {
    args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
      None, None))
    val result = thisTestFlow(input)
    args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None,
      None, None))
    result
  }
}

object TestFlow {
  def apply[A, B](testName: String)(f: A => B): TestFlow[A, B] =
    new TestFlow[A, B] {
      def apply(a: A): B = f(a)
      val name: String = testName
      def testNames: Set[String] = Set(testName) 
    }
}

/*
// Ability to join and split
trait TestSplitter {
  // holds onto a collection of TestFlows all of which have the same input type, but could different
  // output types.
}

trait TestJoiner {

}
*/
