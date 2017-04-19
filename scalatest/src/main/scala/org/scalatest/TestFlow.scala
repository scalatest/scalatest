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

import org.scalatest.events.{MotionToSuppress, TestFailed, TestStarting, TestSucceeded, TestCanceled, TestPending, Location, LineInFile, SeeStackDepthException}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.scalatest.exceptions.{DuplicateTestNameException, PayloadField, TestCanceledException, TestPendingException}
import org.scalactic.source

trait Flow0[A] { thisNode =>

  def testNames: Set[String]

  def run(suite: Suite, testName: Option[String], args: Args): (Option[A], Status)

  def andThen[B](next: Flow1[A, B])(implicit pos: source.Position): Flow0[B] = {
    thisNode.testNames.find(tn => next.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }
    new Flow0[B] {
      def testNames: Set[String] = thisNode.testNames ++ next.testNames // TODO: Ensure iterator order is reasonable, either depth or breadth first
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[B], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) => next.run(suite, testName, args, res0)
          case None =>
            next.cancel(suite, args)
            (None, status)
        }
      }
    }
  }

  def andThen[B, C](first: Flow1[A, B], second: Flow1[A, C]): Flow0[(B, C)] =
    new Flow0[(B, C)] {
      def testNames: Set[String] = thisNode.testNames ++ first.testNames ++ second.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = first.run(suite, testName, args, res0)
            val (res2, s2) = second.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined) Some((res1.get, res2.get)) else None
            val retS = if (s1.succeeds && s2.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            first.cancel(suite, args)
            second.cancel(suite, args)
            (None, status)
        }
      }
    }
}

trait Test0[A] extends Flow0[A] { thisTest0 =>
  def apply(): A // This is the test function, like what we pass into withFixture
  def name: String
  def location: Option[Location]
  def run(suite: Suite, testName: Option[String], args: Args): (Option[A], Status) = {
    Suite.reportTestStarting(suite, args.reporter, args.tracker, name, name, None, location)
    try {
      val result = thisTest0()
      Suite.reportTestSucceeded(suite, args.reporter, args.tracker, name, name, collection.immutable.IndexedSeq.empty, 0L, Suite.getEscapedIndentedTextForTest(name, 1, true), None, location)
      (Some(result), SucceededStatus)
    }
    catch {
      case tce: TestCanceledException =>
        val message = Suite.getMessageForException(tce)
        val payload =
          tce match {
            case optPayload: PayloadField =>
              optPayload.payload
            case _ =>
              None
          }
        val loc =
          tce.position match {
            case Some(pos) => Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname)))
            case None => location
          }
        Suite.reportTestCanceled(suite, args.reporter, tce, name, name, collection.immutable.IndexedSeq.empty, None, args.tracker, 0L, Suite.getEscapedIndentedTextForTest(name, 1, true), loc)
        (None, SucceededStatus)

      case tpe: TestPendingException =>
        Suite.reportTestPending(suite, args.reporter, args.tracker, name, name, collection.immutable.IndexedSeq.empty, 0L, Suite.getEscapedIndentedTextForTest(name, 1, true), location)
        (None, SucceededStatus)

      case t: Throwable =>
        val message = Suite.getMessageForException(t)
        val payload =
          t match {
            case optPayload: PayloadField =>
              optPayload.payload
            case _ =>
              None
          }
        Suite.reportTestFailed(suite, args.reporter, t, name, name, collection.immutable.IndexedSeq.empty, None, args.tracker, 0L, Suite.getEscapedIndentedTextForTest(name, 1, true), Some(SeeStackDepthException))
        (None, FailedStatus)
    }
  }
}

object Test0 {
  def apply[A](testName: String)(f: => A)(implicit pos: source.Position): Test0[A] =
    new Test0[A] {
      def apply(): A = f
      val name: String = testName
      val location: Option[Location] = Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname)))
      def testNames: Set[String] = Set(testName)
    }
}

trait BeforeNode[A] extends Flow0[A] { thisBeforeNode =>
  def apply(): A // This is the test function, like what we pass into withFixture
  def location: Option[Location]
  def run(suite: Suite, testName: Option[String], args: Args): (Option[A], Status) = {
    try {
      val result = thisBeforeNode()
      (Some(result), SucceededStatus)
    }
    catch {
      case tce: TestCanceledException =>
        (None, SucceededStatus)

      case tpe: TestPendingException =>
        (None, SucceededStatus)

      case t: Throwable =>
        (None, FailedStatus)
    }
  }
}

object BeforeNode {
  def apply[A](f: => A)(implicit pos: source.Position): BeforeNode[A] =
    new BeforeNode[A] {
      def apply(): A = f
      val testNames = Set.empty[String]
      val location: Option[Location] = Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname)))
    }
}

trait Flow1[A, B] { self =>

  def testNames: Set[String]

  def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[B], Status)

  def cancel(suite: Suite, args: Args): Unit

  def andThen[C](next: Flow1[B, C])(implicit pos: source.Position): Flow1[A, C] = {
    self.testNames.find(tn => next.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }

    new Flow1[A, C] {
      def testNames: Set[String] = self.testNames ++ next.testNames // TODO: Ensure iterator order is reasonable, either depth or breadth first
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        next.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[C], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) => next.run(suite, testName, args, res0)
          case None =>
            next.cancel(suite, args)
            (None, status)
        }

      }
    }
  }

  def compose[C](prev: Flow1[C, A])(implicit pos: source.Position): Flow1[C, B] = {
    self.testNames.find(tn => prev.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }

    new Flow1[C, B] {

      def testNames: Set[String] = prev.testNames ++ self.testNames

      def cancel(suite: Suite, args: Args): Unit = {
        prev.cancel(suite, args)
        self.cancel(suite, args)
      }

      override def run(suite: Suite, testName: Option[String], args: Args, input: C): (Option[B], Status) = {
        val (res0, status) = prev.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            self.run(suite, testName, args, res0)

          case None =>
            self.cancel(suite, args)
            (None, SucceededStatus)
        }
      }
    }
  }

  def compose(prev: Flow0[A])(implicit pos: source.Position): Flow0[B] = {
    self.testNames.find(tn => prev.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }

    new Flow0[B] {

      lazy val testNames: Set[String] = prev.testNames ++ self.testNames

      override def run(suite: Suite, testName: Option[String], args: Args): (Option[B], Status) = {
        val (res0, status) = prev.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            self.run(suite, testName, args, res0)

          case None =>
            self.cancel(suite, args)
            (None, SucceededStatus)
        }
      }
    }
  }
}

trait Test1[A, B] extends Flow1[A, B] { thisTest1 =>
  def apply(a: A): B // This is the test function, like what we pass into withFixture
  def name: String
  def location: Option[Location]
  def cancel(suite: Suite, args: Args): Unit = {
    Suite.reportTestStarting(suite, args.reporter, args.tracker, name, name, None, location)
    args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", suite.suiteName, suite.suiteId, Some(suite.getClass.getName), name, name, collection.immutable.IndexedSeq.empty, None, None, Some(Suite.getEscapedIndentedTextForTest(name, 1, true)), location, None, None))
  }
  def testNames: Set[String]
  def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[B], Status) = {
    Suite.reportTestStarting(suite, args.reporter, args.tracker, name, name, None, location)
    try {
      val result = thisTest1(input)
      Suite.reportTestSucceeded(suite, args.reporter, args.tracker, name, name, collection.immutable.IndexedSeq.empty, 0L, Suite.getEscapedIndentedTextForTest(name, 1, true), None, location)
      (Some(result), SucceededStatus)
    }
    catch {
      case tce: TestCanceledException =>
        val message = Suite.getMessageForException(tce)
        val payload =
          tce match {
            case optPayload: PayloadField =>
              optPayload.payload
            case _ =>
              None
          }
        val loc =
          tce.position match {
            case Some(pos) => Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname)))
            case None => location
          }
        Suite.reportTestCanceled(suite, args.reporter, tce, name, name, collection.immutable.IndexedSeq.empty, None, args.tracker, 0L, Suite.getEscapedIndentedTextForTest(name, 1, true), loc)
        (None, SucceededStatus)

      case tce: TestPendingException =>
        Suite.reportTestPending(suite, args.reporter, args.tracker, name, name, collection.immutable.IndexedSeq.empty, 0L, Suite.getEscapedIndentedTextForTest(name, 1, true), location)
        (None, SucceededStatus)

      case t: Throwable =>
        val message = Suite.getMessageForException(t)
        val payload =
          t match {
            case optPayload: PayloadField =>
              optPayload.payload
            case _ =>
              None
          }
        Suite.reportTestFailed(suite, args.reporter, t, name, name, collection.immutable.IndexedSeq.empty, None, args.tracker, 0L, Suite.getEscapedIndentedTextForTest(name, 1, true), Some(SeeStackDepthException))
        (None, FailedStatus)
    }
  }
}

object Test1 {
  def apply[A, B](testName: String)(f: A => B)(implicit pos: source.Position): Test1[A, B] =
    new Test1[A, B] {
      def apply(a: A): B = f(a)
      val name: String = testName
      val location: Option[Location] = Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname)))
      def testNames: Set[String] = Set(testName)
    }
}

trait InBetweenNode[A, B] extends Flow1[A, B] { thisTest1 =>
  def apply(a: A): B // This is the test function, like what we pass into withFixture
  def testNames: Set[String] = Set.empty[String]
  def location: Option[Location]
  def cancel(suite: Suite, args: Args): Unit = {}
  def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[B], Status) = {
    try {
      val result = thisTest1(input)
      (Some(result), SucceededStatus)
    }
    catch {
      case tce: TestCanceledException =>
        (None, SucceededStatus)

      case tce: TestPendingException =>
        (None, SucceededStatus)

      case t: Throwable =>
        (None, FailedStatus)
    }
  }
}

object InBetweenNode {
  def apply[A, B](f: A => B)(implicit pos: source.Position): InBetweenNode[A, B] =
    new InBetweenNode[A, B] {
      def apply(a: A): B = f(a)
      val location: Option[Location] = Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname)))
    }
}

trait AfterNode[A] extends Flow1[A, Unit] { thisTest1 =>
  def apply(a: A): Unit // This is the test function, like what we pass into withFixture
  def location: Option[Location]
  val testNames = Set.empty[String]
  def cancel(suite: Suite, args: Args): Unit = {}
  def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[Unit], Status) = {
    try {
      val result = thisTest1(input)
      (Some(result), SucceededStatus)
    }
    catch {
      case tce: TestCanceledException =>
        (None, SucceededStatus)

      case tce: TestPendingException =>
        (None, SucceededStatus)

      case t: Throwable =>
        (None, FailedStatus)
    }
  }
}

object AfterNode {
  def apply[A](f: A => Unit)(implicit pos: source.Position): AfterNode[A] =
    new AfterNode[A] {
      def apply(a: A): Unit = f(a)
      val location: Option[Location] = Some(LineInFile(pos.lineNumber, pos.fileName, Some(pos.filePathname)))
    }
}

trait TestFlow[A] extends Suite {

  def flow: Flow0[A]

  override def testNames = flow.testNames

  override def runTests(testName: Option[String], args: Args): Status = {
    val (res, status) = flow.run(this, testName, args)
    status
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
