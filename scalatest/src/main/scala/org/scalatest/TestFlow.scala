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

trait StartNode[A] { thisNode =>

  def testNames: Set[String]

  def runTests(suite: Suite, testName: Option[String], args: Args): (Option[A], Status)

  def andThen[B](next: Test1[A, B])(implicit pos: source.Position): StartNode[B] = {
    thisNode.testNames.find(tn => next.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }
    new StartNode[B] {
      def testNames: Set[String] = thisNode.testNames ++ next.testNames // TODO: Ensure iterator order is reasonable, either depth or breadth first
      override def runTests(suite: Suite, testName: Option[String], args: Args): (Option[B], Status) = {
        val (res0, status) = thisNode.runTests(suite, testName, args)
        res0 match {
          case Some(res0) => next.runTests(suite, testName, args, res0)
          case None =>
            next.cancel(suite, args)
            (None, status)
        }
      }
    }
  }

  def andThen[B](next: InBetweenNode[A, B])(implicit pos: source.Position): StartNode[B] = {
    new StartNode[B] {
      def testNames: Set[String] = thisNode.testNames // TODO: Ensure iterator order is reasonable, either depth or breadth first
      override def runTests(suite: Suite, testName: Option[String], args: Args): (Option[B], Status) = {
        val (res0, status) = thisNode.runTests(suite, testName, args)
        res0 match {
          case Some(res0) => next.runTests(suite, testName, args, res0)
          case None =>
            next.cancel(suite, args)
            (None, status)
        }
      }
    }
  }

  def andThen(next: AfterNode[A])(implicit pos: source.Position): StartNode[Unit] = {
    new StartNode[Unit] {
      def testNames: Set[String] = thisNode.testNames
      override def runTests(suite: Suite, testName: Option[String], args: Args): (Option[Unit], Status) = {
        val (res0, status) = thisNode.runTests(suite, testName, args)
        res0 match {
          case Some(res0) => next.runTests(suite, testName, args, res0)
          case None =>
            next.cancel(suite, args)
            (None, status)
        }
      }
    }
  }

  def andThen(first: FunctionFlow[A, Assertion], second: FunctionFlow[A, Assertion], more: FunctionFlow[A, Assertion]*): StartNode[Assertion] =
    new StartNode[Assertion] {
      def testNames: Set[String] = thisNode.testNames
      override def runTests(suite: Suite, testName: Option[String], args: Args): (Option[Assertion], Status) = {
        val (res0, status) = thisNode.runTests(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = first.runTests(suite, testName, args, res0)
            val (res2, s2) = second.runTests(suite, testName, args, res0)
            val resList = more.map(_.runTests(suite, testName, args, res0))

            val retV = if (res1.isDefined && res2.isDefined && resList.forall(_._1.isDefined)) Some(org.scalatest.Succeeded) else None
            val retS = if (s1.succeeds && s2.succeeds && resList.forall(_._2.succeeds())) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            first.cancel(suite, args)
            second.cancel(suite, args)
            more.foreach(_.cancel(suite, args))
            (None, status)
        }
      }
    }

  //def andThen(next: AfterNode[A])(implicit pos: source.Position): StartNode[Unit]

  //def andThen[B](next: InBetweenNode[A, B])(implicit pos: source.Position): StartNode[B]
}

trait Test0[A] extends StartNode[A] { thisTest0 =>
  def apply(): A // This is the test function, like what we pass into withFixture
  def name: String
  def location: Option[Location]
  def runTests(suite: Suite, testName: Option[String], args: Args): (Option[A], Status) = {
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

trait BeforeNode[A] extends StartNode[A] { thisBeforeNode =>
  def apply(): A // This is the test function, like what we pass into withFixture
  def location: Option[Location]
  def runTests(suite: Suite, testName: Option[String], args: Args): (Option[A], Status) = {
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

trait FunctionFlow[A, B] {

  def runTests(suite: Suite, testName: Option[String], args: Args, input: A): (Option[B], Status)

  def cancel(suite: Suite, args: Args): Unit
}

trait Test1[A, B] extends FunctionFlow[A, B] { thisTest1 =>
  def apply(a: A): B // This is the test function, like what we pass into withFixture
  def name: String
  def location: Option[Location]
  def cancel(suite: Suite, args: Args): Unit = {
    Suite.reportTestStarting(suite, args.reporter, args.tracker, name, name, None, location)
    args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", suite.suiteName, suite.suiteId, Some(suite.getClass.getName), name, name, collection.immutable.IndexedSeq.empty, None, None, Some(Suite.getEscapedIndentedTextForTest(name, 1, true)), location, None, None))
  }
  def andThen[C](next: Test1[B, C])(implicit pos: source.Position): Test1[A, C] = {
    thisTest1.testNames.find(tn => next.testNames.contains(tn)) match {
      case Some(testName) => throw new DuplicateTestNameException(testName, pos)
      case _ =>
    }

    new Test1[A, C] {
      def apply(a: A): C = next(thisTest1(a))

      val name = thisTest1.name
      val location = thisTest1.location

      def testNames: Set[String] = thisTest1.testNames ++ next.testNames // TODO: Ensure iterator order is reasonable, either depth or breadth first
      override def cancel(suite: Suite, args: Args): Unit = {
        args.reporter(TestStarting(args.tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suite.getClass.getName), name, name, Some(MotionToSuppress), location, None))
        args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", suite.suiteName, suite.suiteId, Some(suite.getClass.getName), name, name, collection.immutable.IndexedSeq.empty, None, None, Some(Suite.getEscapedIndentedTextForTest(name, 1, true)), location, None, None))
        next.cancel(suite, args)
      }
      override def runTests(suite: Suite, testName: Option[String], args: Args, input: A): (Option[C], Status) = {
        val (res0, status) = thisTest1.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) => next.runTests(suite, testName, args, res0)
          case None =>
            next.cancel(suite, args)
            (None, status)
        }

      }
    }
  }
  def andThen[C](next: InBetweenNode[B, C])(implicit pos: source.Position): Test1[A, C] = {
    new Test1[A, C] {
      def apply(a: A): C = next(thisTest1(a))

      val name = thisTest1.name
      val location = thisTest1.location

      def testNames: Set[String] = thisTest1.testNames // TODO: Ensure iterator order is reasonable, either depth or breadth first
      override def cancel(suite: Suite, args: Args): Unit = {
        args.reporter(TestStarting(args.tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suite.getClass.getName), name, name, Some(MotionToSuppress), location, None))
        args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", suite.suiteName, suite.suiteId, Some(suite.getClass.getName), name, name, collection.immutable.IndexedSeq.empty, None, None, Some(Suite.getEscapedIndentedTextForTest(name, 1, true)), location, None, None))
        next.cancel(suite, args)
      }
      override def runTests(suite: Suite, testName: Option[String], args: Args, input: A): (Option[C], Status) = {
        val (res0, status) = thisTest1.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) => next.runTests(suite, testName, args, res0)
          case None =>
            next.cancel(suite, args)
            (None, status)
        }

      }
    }
  }
  def andThen(next: AfterNode[B])(implicit pos: source.Position): Test1[A, Unit] = {
    new Test1[A, Unit] {
      def apply(a: A): Unit = next(thisTest1(a))

      val name = thisTest1.name
      val location = thisTest1.location

      def testNames: Set[String] = thisTest1.testNames // TODO: Ensure iterator order is reasonable, either depth or breadth first
      override def cancel(suite: Suite, args: Args): Unit = {
        args.reporter(TestStarting(args.tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suite.getClass.getName), name, name, Some(MotionToSuppress), location, None))
        args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", suite.suiteName, suite.suiteId, Some(suite.getClass.getName), name, name, collection.immutable.IndexedSeq.empty, None, None, Some(Suite.getEscapedIndentedTextForTest(name, 1, true)), location, None, None))
        next.cancel(suite, args)
      }
      override def runTests(suite: Suite, testName: Option[String], args: Args, input: A): (Option[Unit], Status) = {
        val (res0, status) = thisTest1.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) => next.runTests(suite, testName, args, res0)
          case None =>
            next.cancel(suite, args)
            (None, status)
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
      val location = prev.location

      def testNames: Set[String] = prev.testNames ++ thisTest1.testNames

      override def runTests(suite: Suite, testName: Option[String], args: Args, input: C): (Option[B], Status) = {
        val (res0, status) = prev.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            thisTest1.runTests(suite, testName, args, res0)

          case None =>
            args.reporter(TestStarting(args.tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suite.getClass.getName), thisTest1.name, thisTest1.name, Some(MotionToSuppress), thisTest1.location, None))
            args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", suite.suiteName, suite.suiteId, Some(suite.getClass.getName), thisTest1.name, thisTest1.name, collection.immutable.IndexedSeq.empty, None, None, Some(Suite.getEscapedIndentedTextForTest(thisTest1.name, 1, true)), thisTest1.location, None, None))
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

      val location = prev.location

      def testNames: Set[String] = prev.testNames ++ thisTest1.testNames

      override def runTests(suite: Suite, testName: Option[String], args: Args): (Option[B], Status) = {
        val (res0, status) = prev.runTests(suite, testName, args)
        res0 match {
          case Some(res0) =>
            thisTest1.runTests(suite, testName, args, res0)

          case None =>
            args.reporter(TestStarting(args.tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suite.getClass.getName), thisTest1.name, thisTest1.name, Some(MotionToSuppress), thisTest1.location, None))
            args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", suite.suiteName, suite.suiteId, Some(suite.getClass.getName), thisTest1.name, thisTest1.name, collection.immutable.IndexedSeq.empty, None, None, Some(Suite.getEscapedIndentedTextForTest(thisTest1.name, 1, true)), thisTest1.location, None, None))
            (None, SucceededStatus)
        }
      }
    }
  }
  def compose[C](prev: InBetweenNode[C, A])(implicit pos: source.Position): InBetweenNode[C, B] = {
    new InBetweenNode[C, B] {
      def apply(c: C): B = thisTest1(prev(c))
      val location = prev.location

      override def runTests(suite: Suite, testName: Option[String], args: Args, input: C): (Option[B], Status) = {
        val (res0, status) = prev.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            thisTest1.runTests(suite, testName, args, res0)

          case None =>
            args.reporter(TestStarting(args.tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suite.getClass.getName), thisTest1.name, thisTest1.name, Some(MotionToSuppress), thisTest1.location, None))
            args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", suite.suiteName, suite.suiteId, Some(suite.getClass.getName), thisTest1.name, thisTest1.name, collection.immutable.IndexedSeq.empty, None, None, Some(Suite.getEscapedIndentedTextForTest(thisTest1.name, 1, true)), thisTest1.location, None, None))
            (None, SucceededStatus)
        }
      }
    }
  }
  def testNames: Set[String]
  def runTests(suite: Suite, testName: Option[String], args: Args, input: A): (Option[B], Status) = {
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

trait InBetweenNode[A, B] extends FunctionFlow[A, B] { thisTest1 =>
  def apply(a: A): B // This is the test function, like what we pass into withFixture
  def location: Option[Location]
  def cancel(suite: Suite, args: Args): Unit = {}
  def andThen[C](next: Test1[B, C])(implicit pos: source.Position): InBetweenNode[A, C] = {
    new InBetweenNode[A, C] {
      def apply(a: A): C = next(thisTest1(a))

      val location = thisTest1.location

      override def cancel(suite: Suite, args: Args): Unit = {
        next.cancel(suite, args)
      }
      override def runTests(suite: Suite, testName: Option[String], args: Args, input: A): (Option[C], Status) = {
        val (res0, status) = thisTest1.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) => next.runTests(suite, testName, args, res0)
          case None =>
            next.cancel(suite, args)
            (None, status)
        }

      }
    }
  }
  def andThen[C](next: InBetweenNode[B, C])(implicit pos: source.Position): InBetweenNode[A, C] = {
    new InBetweenNode[A, C] {
      def apply(a: A): C = next(thisTest1(a))

      val location = thisTest1.location

      override def cancel(suite: Suite, args: Args): Unit = {
        next.cancel(suite, args)
      }
      override def runTests(suite: Suite, testName: Option[String], args: Args, input: A): (Option[C], Status) = {
        val (res0, status) = thisTest1.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) => next.runTests(suite, testName, args, res0)
          case None =>
            next.cancel(suite, args)
            (None, status)
        }

      }
    }
  }
  def andThen(next: AfterNode[B])(implicit pos: source.Position): InBetweenNode[A, Unit] = {
    new InBetweenNode[A, Unit] {
      def apply(a: A): Unit = next(thisTest1(a))

      val location = thisTest1.location

      override def cancel(suite: Suite, args: Args): Unit = {
        next.cancel(suite, args)
      }
      override def runTests(suite: Suite, testName: Option[String], args: Args, input: A): (Option[Unit], Status) = {
        val (res0, status) = thisTest1.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) => next.runTests(suite, testName, args, res0)
          case None =>
            next.cancel(suite, args)
            (None, status)
        }

      }
    }
  }
  def compose[C](prev: Test1[C, A])(implicit pos: source.Position): InBetweenNode[C, B] = {
    new InBetweenNode[C, B] {
      def apply(c: C): B = thisTest1(prev(c))

      val location = prev.location

      override def runTests(suite: Suite, testName: Option[String], args: Args, input: C): (Option[B], Status) = {
        val (res0, status) = prev.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            thisTest1.runTests(suite, testName, args, res0)

          case None =>
            (None, SucceededStatus)
        }
      }
    }
  }
  def compose[C](prev: InBetweenNode[C, A])(implicit pos: source.Position): InBetweenNode[C, B] = {
    new InBetweenNode[C, B] {
      def apply(c: C): B = thisTest1(prev(c))

      val location = prev.location

      override def runTests(suite: Suite, testName: Option[String], args: Args, input: C): (Option[B], Status) = {
        val (res0, status) = prev.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            thisTest1.runTests(suite, testName, args, res0)

          case None =>
            (None, SucceededStatus)
        }
      }
    }
  }
  def compose(prev: Test0[A])(implicit pos: source.Position): Test0[B] = {

    new Test0[B] {
      def apply(): B = thisTest1(prev())

      val name = prev.name

      val location = prev.location

      def testNames: Set[String] = prev.testNames

      override def runTests(suite: Suite, testName: Option[String], args: Args): (Option[B], Status) = {
        val (res0, status) = prev.runTests(suite, testName, args)
        res0 match {
          case Some(res0) =>
            thisTest1.runTests(suite, testName, args, res0)

          case None =>
            (None, SucceededStatus)
        }
      }
    }
  }
  def runTests(suite: Suite, testName: Option[String], args: Args, input: A): (Option[B], Status) = {
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

trait AfterNode[A] { thisTest1 =>
  def apply(a: A): Unit // This is the test function, like what we pass into withFixture
  def location: Option[Location]
  def cancel(suite: Suite, args: Args): Unit = {}
  def compose[C](prev: Test1[C, A])(implicit pos: source.Position): Test1[C, Unit] = {
    new Test1[C, Unit] {
      def apply(c: C): Unit = thisTest1(prev(c))

      val name = prev.name
      val location = prev.location

      def testNames: Set[String] = prev.testNames

      override def runTests(suite: Suite, testName: Option[String], args: Args, input: C): (Option[Unit], Status) = {
        val (res0, status) = prev.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            thisTest1.runTests(suite, testName, args, res0)

          case None =>
            (None, SucceededStatus)
        }
      }
    }
  }
  def compose(prev: Test0[A])(implicit pos: source.Position): Test0[Unit] = {

    new Test0[Unit] {
      def apply(): Unit = thisTest1(prev())

      val name = prev.name

      val location = prev.location

      def testNames: Set[String] = prev.testNames

      override def runTests(suite: Suite, testName: Option[String], args: Args): (Option[Unit], Status) = {
        val (res0, status) = prev.runTests(suite, testName, args)
        res0 match {
          case Some(res0) =>
            thisTest1.runTests(suite, testName, args, res0)

          case None =>
            (None, SucceededStatus)
        }
      }
    }
  }
  def compose(prev: BeforeNode[A])(implicit pos: source.Position): BeforeNode[Unit] = {
    new BeforeNode[Unit] {
      def apply(): Unit = thisTest1(prev())

      val testNames = Set.empty[String]
      val location = prev.location

      override def runTests(suite: Suite, testName: Option[String], args: Args): (Option[Unit], Status) = {
        val (res0, status) = prev.runTests(suite, testName, args)
        res0 match {
          case Some(res0) =>
            thisTest1.runTests(suite, testName, args, res0)

          case None =>
            (None, SucceededStatus)
        }
      }
    }
  }
  def compose[B](prev: InBetweenNode[B, A])(implicit pos: source.Position): InBetweenNode[B, Unit] = {
    new InBetweenNode[B, Unit] {
      def apply(b: B): Unit = thisTest1(prev(b))

      val location = prev.location

      override def runTests(suite: Suite, testName: Option[String], args: Args, input: B): (Option[Unit], Status) = {
        val (res0, status) = prev.runTests(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            thisTest1.runTests(suite, testName, args, res0)

          case None =>
            (None, SucceededStatus)
        }
      }
    }
  }
  def runTests(suite: Suite, testName: Option[String], args: Args, input: A): (Option[Unit], Status) = {
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

  def flow: StartNode[A]

  override def runTests(testName: Option[String], args: Args): Status = {
    val (res, status) = flow.runTests(this, testName, args)
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
