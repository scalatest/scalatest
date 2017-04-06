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

import org.scalatest.events.{MotionToSuppress, TestFailed, TestStarting, TestSucceeded, TestCanceled}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.scalatest.exceptions.{DuplicateTestNameException, PayloadField}
import org.scalactic.source

trait Test0[A] { thisTest0 =>
  def apply(): A // This is the test function, like what we pass into withFixture
  def name: String
  def testNames: Set[String]
  def andThen[B](next: Test1[A, B])(implicit pos: source.Position): Test0[B] = {
    thisTest0.testNames.find(tn => next.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }
    new Test0[B] {
      def apply(): B = next(thisTest0())
      val name = thisTest0.name
      def testNames: Set[String] = thisTest0.testNames ++ next.testNames // TODO: Ensure iterator order is reasonable, either depth or breadth first
      override def runTests(testName: Option[String], args: Args): (Option[B], Status) = {
        args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
          None, None))
        try {
          val res0 = thisTest0()
          args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None,
            None, None))
          next.runTests(testName, args, res0)
        }
        catch {
          case t: Throwable =>
            val message = Suite.getMessageForException(t)
            val payload =
              t match {
                case optPayload: PayloadField =>
                  optPayload.payload
                case _ =>
                  None
              }
            args.reporter(TestFailed(args.tracker.nextOrdinal(), message, "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, Some(t), None, Some(MotionToSuppress), None, None, payload))
            (None, FailedStatus)
        }
      }
    }
  }
  def runTests(testName: Option[String], args: Args): (Option[A], Status) = {
    args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
      None, None))
    try {
      val result = thisTest0()
      args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None,
        None, None))
      (Some(result), SucceededStatus)
    }
    catch {
      case t: Throwable =>
        val message = Suite.getMessageForException(t)
        val payload =
          t match {
            case optPayload: PayloadField =>
              optPayload.payload
            case _ =>
              None
          }
        args.reporter(TestFailed(args.tracker.nextOrdinal(), message, "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, Some(t), None, Some(MotionToSuppress), None, None, payload))
        (None, FailedStatus)
    }
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

trait Test1[A, B] { thisTest1 =>
  def apply(a: A): B // This is the test function, like what we pass into withFixture
  def name: String
  def cancel(args: Args): Unit = {
    args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
      None, None))
    args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None, None, None, None, None))
  }
  def andThen[C](next: Test1[B, C])(implicit pos: source.Position): Test1[A, C] = {
    thisTest1.testNames.find(tn => next.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }

    new Test1[A, C] {
      def apply(a: A): C = next(thisTest1(a))

      val name = thisTest1.name

      def testNames: Set[String] = thisTest1.testNames ++ next.testNames // TODO: Ensure iterator order is reasonable, either depth or breadth first
      override def cancel(args: Args): Unit = {
        args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
          None, None))
        args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None, None, None, None, None))
        next.cancel(args)
      }
      override def runTests(testName: Option[String], args: Args, input: A): (Option[C], Status) = {
        args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
          None, None))
        try {
          val res0 = thisTest1(input)
          args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None,
            None, None))
          next.runTests(testName, args, res0)
        }
        catch {
          case t: Throwable =>
            val message = Suite.getMessageForException(t)
            val payload =
              t match {
                case optPayload: PayloadField =>
                  optPayload.payload
                case _ =>
                  None
              }
            args.reporter(TestFailed(args.tracker.nextOrdinal(), message, "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, Some(t), None, Some(MotionToSuppress), None, None, payload))
            next.cancel(args)
            (None, FailedStatus)
        }

      }
    }
  }
  def compose[C](prev: Test1[C, A])(implicit pos: source.Position): Test1[C, B] = {
    thisTest1.testNames.find(tn => prev.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }

    new Test1[C, B] {
      def apply(c: C): B = thisTest1(prev(c))

      val name = prev.name

      def testNames: Set[String] = prev.testNames ++ thisTest1.testNames

      override def runTests(testName: Option[String], args: Args, input: C): (Option[B], Status) = {
        val (res0, status) = prev.runTests(testName, args, input)
        args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
          None, None))
        res0 match {
          case Some(res0) =>
            try {
              val result = thisTest1(res0)
              args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None, None, None))
              (Some(result), SucceededStatus)
            }
            catch {
              case t: Throwable =>
                val message = Suite.getMessageForException(t)
                val payload =
                  t match {
                    case optPayload: PayloadField =>
                      optPayload.payload
                    case _ =>
                      None
                  }
                args.reporter(TestFailed(args.tracker.nextOrdinal(), message, "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, Some(t), None, Some(MotionToSuppress), None, None, payload))
                (None, FailedStatus)
            }

          case None =>
            args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None, None, None, None, None))
            (None, SucceededStatus)
        }
      }
    }
  }
  def compose(prev: Test0[A])(implicit pos: source.Position): Test0[B] = {
    thisTest1.testNames.find(tn => prev.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }

    new Test0[B] {
      def apply(): B = thisTest1(prev())

      val name = prev.name

      def testNames: Set[String] = prev.testNames ++ thisTest1.testNames

      override def runTests(testName: Option[String], args: Args): (Option[B], Status) = {
        val (res0, status) = prev.runTests(testName, args)
        args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress),
          None, None))
        res0 match {
          case Some(res0) =>
            try {
              val result = thisTest1(res0)
              args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None, None, None))
              (Some(result), SucceededStatus)
            }
            catch {
              case t: Throwable =>
                val message = Suite.getMessageForException(t)
                val payload =
                  t match {
                    case optPayload: PayloadField =>
                      optPayload.payload
                    case _ =>
                      None
                  }
                args.reporter(TestFailed(args.tracker.nextOrdinal(), message, "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, Some(t), None, Some(MotionToSuppress), None, None, payload))
                (None, FailedStatus)
            }

          case None =>
            args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None, None, None, None, None))
            (None, SucceededStatus)
        }
      }
    }
  }
  def testNames: Set[String]
  def runTests(testName: Option[String], args: Args, input: A): (Option[B], Status) = {
    args.reporter(TestStarting(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TOTO suite class name"), name, "", Some(MotionToSuppress), None, None))
    try {
      val result = thisTest1(input)
      args.reporter(TestSucceeded(args.tracker.nextOrdinal(), "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, None, None, None, None))
      (Some(result), SucceededStatus)
    }
    catch {
      case t: Throwable =>
        val message = Suite.getMessageForException(t)
        val payload =
          t match {
            case optPayload: PayloadField =>
              optPayload.payload
            case _ =>
              None
          }
        args.reporter(TestFailed(args.tracker.nextOrdinal(), message, "TODO suiteName", "TODO suiteId", Some("TODO suite class name"), name, "", collection.immutable.IndexedSeq.empty, Some(t), None, Some(MotionToSuppress), None, None, payload))
        (None, FailedStatus)
    }
  }
}

object Test1 {
  def apply[A, B](testName: String)(f: A => B): Test1[A, B] =
    new Test1[A, B] {
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
