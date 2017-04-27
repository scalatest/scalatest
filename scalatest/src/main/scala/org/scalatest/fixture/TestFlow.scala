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
package org.scalatest.fixture

import org.scalatest.events.{MotionToSuppress, TestFailed, TestStarting, TestSucceeded, TestCanceled, TestPending, Location, LineInFile, SeeStackDepthException}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.scalatest.exceptions.{DuplicateTestNameException, PayloadField, TestCanceledException, TestPendingException}
import org.scalactic.source
import org.scalatest.{Args, Status, SucceededStatus, FailedStatus}
import org.scalatest.Suite._
import scala.compat.Platform.currentTime

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

  def andThen[B, C](fun1: Flow1[A, B], fun2: Flow1[A, C]): Flow0[(B, C)] =
    new Flow0[(B, C)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined) Some((res1.get, res2.get)) else None
            val retS = if (s1.succeeds && s2.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D]): Flow0[(B, C, D)] =
    new Flow0[(B, C, D)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined) Some((res1.get, res2.get, res3.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E]): Flow0[(B, C, D, E)] =
    new Flow0[(B, C, D, E)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined) Some((res1.get, res2.get, res3.get, res4.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F]): Flow0[(B, C, D, E, F)] =
    new Flow0[(B, C, D, E, F)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G]): Flow0[(B, C, D, E, F, G)] =
    new Flow0[(B, C, D, E, F, G)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H]): Flow0[(B, C, D, E, F, G, H)] =
    new Flow0[(B, C, D, E, F, G, H)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I]): Flow0[(B, C, D, E, F, G, H, I)] =
    new Flow0[(B, C, D, E, F, G, H, I)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J]): Flow0[(B, C, D, E, F, G, H, I, J)] =
    new Flow0[(B, C, D, E, F, G, H, I, J)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K]): Flow0[(B, C, D, E, F, G, H, I, J, K)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K, L](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K], fun11: Flow1[A, L]): Flow0[(B, C, D, E, F, G, H, I, J, K, L)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K, L)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K, L)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K, L, M](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K], fun11: Flow1[A, L], fun12: Flow1[A, M]): Flow0[(B, C, D, E, F, G, H, I, J, K, L, M)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K, L, M)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K, L, M)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K, L, M, N](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K], fun11: Flow1[A, L], fun12: Flow1[A, M], fun13: Flow1[A, N]): Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K, L, M, N)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K, L, M, N, O](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K], fun11: Flow1[A, L], fun12: Flow1[A, M], fun13: Flow1[A, N], fun14: Flow1[A, O]): Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K, L, M, N, O)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K], fun11: Flow1[A, L], fun12: Flow1[A, M], fun13: Flow1[A, N], fun14: Flow1[A, O], fun15: Flow1[A, P]): Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K], fun11: Flow1[A, L], fun12: Flow1[A, M], fun13: Flow1[A, N], fun14: Flow1[A, O], fun15: Flow1[A, P], fun16: Flow1[A, Q]): Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K], fun11: Flow1[A, L], fun12: Flow1[A, M], fun13: Flow1[A, N], fun14: Flow1[A, O], fun15: Flow1[A, P], fun16: Flow1[A, Q], fun17: Flow1[A, R]): Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames ++ fun17.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)
            val (res17, s17) = fun17.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined && res17.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get, res17.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds && s17.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            fun17.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K], fun11: Flow1[A, L], fun12: Flow1[A, M], fun13: Flow1[A, N], fun14: Flow1[A, O], fun15: Flow1[A, P], fun16: Flow1[A, Q], fun17: Flow1[A, R], fun18: Flow1[A, S]): Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames ++ fun17.testNames ++ fun18.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)
            val (res17, s17) = fun17.run(suite, testName, args, res0)
            val (res18, s18) = fun18.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined && res17.isDefined && res18.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get, res17.get, res18.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds && s17.succeeds && s18.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            fun17.cancel(suite, args)
            fun18.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K], fun11: Flow1[A, L], fun12: Flow1[A, M], fun13: Flow1[A, N], fun14: Flow1[A, O], fun15: Flow1[A, P], fun16: Flow1[A, Q], fun17: Flow1[A, R], fun18: Flow1[A, S], fun19: Flow1[A, T]): Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames ++ fun17.testNames ++ fun18.testNames ++ fun19.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)
            val (res17, s17) = fun17.run(suite, testName, args, res0)
            val (res18, s18) = fun18.run(suite, testName, args, res0)
            val (res19, s19) = fun19.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined && res17.isDefined && res18.isDefined && res19.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get, res17.get, res18.get, res19.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds && s17.succeeds && s18.succeeds && s19.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            fun17.cancel(suite, args)
            fun18.cancel(suite, args)
            fun19.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K], fun11: Flow1[A, L], fun12: Flow1[A, M], fun13: Flow1[A, N], fun14: Flow1[A, O], fun15: Flow1[A, P], fun16: Flow1[A, Q], fun17: Flow1[A, R], fun18: Flow1[A, S], fun19: Flow1[A, T], fun20: Flow1[A, U]): Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames ++ fun17.testNames ++ fun18.testNames ++ fun19.testNames ++ fun20.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)
            val (res17, s17) = fun17.run(suite, testName, args, res0)
            val (res18, s18) = fun18.run(suite, testName, args, res0)
            val (res19, s19) = fun19.run(suite, testName, args, res0)
            val (res20, s20) = fun20.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined && res17.isDefined && res18.isDefined && res19.isDefined && res20.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get, res17.get, res18.get, res19.get, res20.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds && s17.succeeds && s18.succeeds && s19.succeeds && s20.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            fun17.cancel(suite, args)
            fun18.cancel(suite, args)
            fun19.cancel(suite, args)
            fun20.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](fun1: Flow1[A, B], fun2: Flow1[A, C], fun3: Flow1[A, D], fun4: Flow1[A, E], fun5: Flow1[A, F], fun6: Flow1[A, G], fun7: Flow1[A, H], fun8: Flow1[A, I], fun9: Flow1[A, J], fun10: Flow1[A, K], fun11: Flow1[A, L], fun12: Flow1[A, M], fun13: Flow1[A, N], fun14: Flow1[A, O], fun15: Flow1[A, P], fun16: Flow1[A, Q], fun17: Flow1[A, R], fun18: Flow1[A, S], fun19: Flow1[A, T], fun20: Flow1[A, U], fun21: Flow1[A, V]): Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    new Flow0[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] {
      def testNames: Set[String] = thisNode.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames ++ fun17.testNames ++ fun18.testNames ++ fun19.testNames ++ fun20.testNames ++ fun21.testNames
      override def run(suite: Suite, testName: Option[String], args: Args): (Option[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)], Status) = {
        val (res0, status) = thisNode.run(suite, testName, args)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)
            val (res17, s17) = fun17.run(suite, testName, args, res0)
            val (res18, s18) = fun18.run(suite, testName, args, res0)
            val (res19, s19) = fun19.run(suite, testName, args, res0)
            val (res20, s20) = fun20.run(suite, testName, args, res0)
            val (res21, s21) = fun21.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined && res17.isDefined && res18.isDefined && res19.isDefined && res20.isDefined && res21.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get, res17.get, res18.get, res19.get, res20.get, res21.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds && s17.succeeds && s18.succeeds && s19.succeeds && s20.succeeds && s21.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            fun17.cancel(suite, args)
            fun18.cancel(suite, args)
            fun19.cancel(suite, args)
            fun20.cancel(suite, args)
            fun21.cancel(suite, args)
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
    val startTime = currentTime
    reportTestStarting(suite, args.reporter, args.tracker, name, name, None, location)
    try {
      val result = thisTest0()
      val duration = currentTime - startTime
      reportTestSucceeded(suite, args.reporter, args.tracker, name, name, collection.immutable.IndexedSeq.empty, duration, getEscapedIndentedTextForTest(name, 1, true), None, location)
      (Some(result), SucceededStatus)
    }
    catch {
      case tce: TestCanceledException =>
        val message = getMessageForException(tce)
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
        val duration = currentTime - startTime
        reportTestCanceled(suite, args.reporter, tce, name, name, collection.immutable.IndexedSeq.empty, None, args.tracker, duration, getEscapedIndentedTextForTest(name, 1, true), loc)
        (None, SucceededStatus)

      case tpe: TestPendingException =>
        val duration = currentTime - startTime
        reportTestPending(suite, args.reporter, args.tracker, name, name, collection.immutable.IndexedSeq.empty, duration, getEscapedIndentedTextForTest(name, 1, true), location)
        (None, SucceededStatus)

      case t: Throwable =>
        val message = getMessageForException(t)
        val payload =
          t match {
            case optPayload: PayloadField =>
              optPayload.payload
            case _ =>
              None
          }
        val duration = currentTime - startTime
        reportTestFailed(suite, args.reporter, t, name, name, collection.immutable.IndexedSeq.empty, None, args.tracker, duration, getEscapedIndentedTextForTest(name, 1, true), Some(SeeStackDepthException))
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

  def andThen[C, D](fun1: Flow1[B, C], fun2: Flow1[B, D]): Flow1[A, (C, D)] =
    new Flow1[A, (C, D)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined) Some((res1.get, res2.get)) else None
            val retS = if (s1.succeeds && s2.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            (None, status)
        }
      }
    }


  def andThen[C, D, E](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E]): Flow1[A, (C, D, E)] =
    new Flow1[A, (C, D, E)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined) Some((res1.get, res2.get, res3.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F]): Flow1[A, (C, D, E, F)] =
    new Flow1[A, (C, D, E, F)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined) Some((res1.get, res2.get, res3.get, res4.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G]): Flow1[A, (C, D, E, F, G)] =
    new Flow1[A, (C, D, E, F, G)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H]): Flow1[A, (C, D, E, F, G, H)] =
    new Flow1[A, (C, D, E, F, G, H)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I]): Flow1[A, (C, D, E, F, G, H, I)] =
    new Flow1[A, (C, D, E, F, G, H, I)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J]): Flow1[A, (C, D, E, F, G, H, I, J)] =
    new Flow1[A, (C, D, E, F, G, H, I, J)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K]): Flow1[A, (C, D, E, F, G, H, I, J, K)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L]): Flow1[A, (C, D, E, F, G, H, I, J, K, L)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M, N](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M], fun12: Flow1[B, N]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
        fun12.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M, N)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M, N, O](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M], fun12: Flow1[B, N], fun13: Flow1[B, O]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
        fun12.cancel(suite, args)
        fun13.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M, N, O)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M, N, O, P](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M], fun12: Flow1[B, N], fun13: Flow1[B, O], fun14: Flow1[B, P]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
        fun12.cancel(suite, args)
        fun13.cancel(suite, args)
        fun14.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M, N, O, P)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M], fun12: Flow1[B, N], fun13: Flow1[B, O], fun14: Flow1[B, P], fun15: Flow1[B, Q]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
        fun12.cancel(suite, args)
        fun13.cancel(suite, args)
        fun14.cancel(suite, args)
        fun15.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M], fun12: Flow1[B, N], fun13: Flow1[B, O], fun14: Flow1[B, P], fun15: Flow1[B, Q], fun16: Flow1[B, R]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
        fun12.cancel(suite, args)
        fun13.cancel(suite, args)
        fun14.cancel(suite, args)
        fun15.cancel(suite, args)
        fun16.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M], fun12: Flow1[B, N], fun13: Flow1[B, O], fun14: Flow1[B, P], fun15: Flow1[B, Q], fun16: Flow1[B, R], fun17: Flow1[B, S]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames ++ fun17.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
        fun12.cancel(suite, args)
        fun13.cancel(suite, args)
        fun14.cancel(suite, args)
        fun15.cancel(suite, args)
        fun16.cancel(suite, args)
        fun17.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)
            val (res17, s17) = fun17.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined && res17.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get, res17.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds && s17.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            fun17.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M], fun12: Flow1[B, N], fun13: Flow1[B, O], fun14: Flow1[B, P], fun15: Flow1[B, Q], fun16: Flow1[B, R], fun17: Flow1[B, S], fun18: Flow1[B, T]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames ++ fun17.testNames ++ fun18.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
        fun12.cancel(suite, args)
        fun13.cancel(suite, args)
        fun14.cancel(suite, args)
        fun15.cancel(suite, args)
        fun16.cancel(suite, args)
        fun17.cancel(suite, args)
        fun18.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)
            val (res17, s17) = fun17.run(suite, testName, args, res0)
            val (res18, s18) = fun18.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined && res17.isDefined && res18.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get, res17.get, res18.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds && s17.succeeds && s18.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            fun17.cancel(suite, args)
            fun18.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M], fun12: Flow1[B, N], fun13: Flow1[B, O], fun14: Flow1[B, P], fun15: Flow1[B, Q], fun16: Flow1[B, R], fun17: Flow1[B, S], fun18: Flow1[B, T], fun19: Flow1[B, U]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames ++ fun17.testNames ++ fun18.testNames ++ fun19.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
        fun12.cancel(suite, args)
        fun13.cancel(suite, args)
        fun14.cancel(suite, args)
        fun15.cancel(suite, args)
        fun16.cancel(suite, args)
        fun17.cancel(suite, args)
        fun18.cancel(suite, args)
        fun19.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)
            val (res17, s17) = fun17.run(suite, testName, args, res0)
            val (res18, s18) = fun18.run(suite, testName, args, res0)
            val (res19, s19) = fun19.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined && res17.isDefined && res18.isDefined && res19.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get, res17.get, res18.get, res19.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds && s17.succeeds && s18.succeeds && s19.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            fun17.cancel(suite, args)
            fun18.cancel(suite, args)
            fun19.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M], fun12: Flow1[B, N], fun13: Flow1[B, O], fun14: Flow1[B, P], fun15: Flow1[B, Q], fun16: Flow1[B, R], fun17: Flow1[B, S], fun18: Flow1[B, T], fun19: Flow1[B, U], fun20: Flow1[B, V]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames ++ fun17.testNames ++ fun18.testNames ++ fun19.testNames ++ fun20.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
        fun12.cancel(suite, args)
        fun13.cancel(suite, args)
        fun14.cancel(suite, args)
        fun15.cancel(suite, args)
        fun16.cancel(suite, args)
        fun17.cancel(suite, args)
        fun18.cancel(suite, args)
        fun19.cancel(suite, args)
        fun20.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)
            val (res17, s17) = fun17.run(suite, testName, args, res0)
            val (res18, s18) = fun18.run(suite, testName, args, res0)
            val (res19, s19) = fun19.run(suite, testName, args, res0)
            val (res20, s20) = fun20.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined && res17.isDefined && res18.isDefined && res19.isDefined && res20.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get, res17.get, res18.get, res19.get, res20.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds && s17.succeeds && s18.succeeds && s19.succeeds && s20.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            fun17.cancel(suite, args)
            fun18.cancel(suite, args)
            fun19.cancel(suite, args)
            fun20.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M], fun12: Flow1[B, N], fun13: Flow1[B, O], fun14: Flow1[B, P], fun15: Flow1[B, Q], fun16: Flow1[B, R], fun17: Flow1[B, S], fun18: Flow1[B, T], fun19: Flow1[B, U], fun20: Flow1[B, V], fun21: Flow1[B, W]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames ++ fun17.testNames ++ fun18.testNames ++ fun19.testNames ++ fun20.testNames ++ fun21.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
        fun12.cancel(suite, args)
        fun13.cancel(suite, args)
        fun14.cancel(suite, args)
        fun15.cancel(suite, args)
        fun16.cancel(suite, args)
        fun17.cancel(suite, args)
        fun18.cancel(suite, args)
        fun19.cancel(suite, args)
        fun20.cancel(suite, args)
        fun21.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)
            val (res17, s17) = fun17.run(suite, testName, args, res0)
            val (res18, s18) = fun18.run(suite, testName, args, res0)
            val (res19, s19) = fun19.run(suite, testName, args, res0)
            val (res20, s20) = fun20.run(suite, testName, args, res0)
            val (res21, s21) = fun21.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined && res17.isDefined && res18.isDefined && res19.isDefined && res20.isDefined && res21.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get, res17.get, res18.get, res19.get, res20.get, res21.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds && s17.succeeds && s18.succeeds && s19.succeeds && s20.succeeds && s21.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            fun17.cancel(suite, args)
            fun18.cancel(suite, args)
            fun19.cancel(suite, args)
            fun20.cancel(suite, args)
            fun21.cancel(suite, args)
            (None, status)
        }
      }
    }

  def andThen[C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X](fun1: Flow1[B, C], fun2: Flow1[B, D], fun3: Flow1[B, E], fun4: Flow1[B, F], fun5: Flow1[B, G], fun6: Flow1[B, H], fun7: Flow1[B, I], fun8: Flow1[B, J], fun9: Flow1[B, K], fun10: Flow1[B, L], fun11: Flow1[B, M], fun12: Flow1[B, N], fun13: Flow1[B, O], fun14: Flow1[B, P], fun15: Flow1[B, Q], fun16: Flow1[B, R], fun17: Flow1[B, S], fun18: Flow1[B, T], fun19: Flow1[B, U], fun20: Flow1[B, V], fun21: Flow1[B, W], fun22: Flow1[B, X]): Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X)] =
    new Flow1[A, (C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X)] {
      def testNames: Set[String] = self.testNames ++ fun1.testNames ++ fun2.testNames ++ fun3.testNames ++ fun4.testNames ++ fun5.testNames ++ fun6.testNames ++ fun7.testNames ++ fun8.testNames ++ fun9.testNames ++ fun10.testNames ++ fun11.testNames ++ fun12.testNames ++ fun13.testNames ++ fun14.testNames ++ fun15.testNames ++ fun16.testNames ++ fun17.testNames ++ fun18.testNames ++ fun19.testNames ++ fun20.testNames ++ fun21.testNames ++ fun22.testNames
      override def cancel(suite: Suite, args: Args): Unit = {
        self.cancel(suite, args)
        fun1.cancel(suite, args)
        fun2.cancel(suite, args)
        fun3.cancel(suite, args)
        fun4.cancel(suite, args)
        fun5.cancel(suite, args)
        fun6.cancel(suite, args)
        fun7.cancel(suite, args)
        fun8.cancel(suite, args)
        fun9.cancel(suite, args)
        fun10.cancel(suite, args)
        fun11.cancel(suite, args)
        fun12.cancel(suite, args)
        fun13.cancel(suite, args)
        fun14.cancel(suite, args)
        fun15.cancel(suite, args)
        fun16.cancel(suite, args)
        fun17.cancel(suite, args)
        fun18.cancel(suite, args)
        fun19.cancel(suite, args)
        fun20.cancel(suite, args)
        fun21.cancel(suite, args)
        fun22.cancel(suite, args)
      }
      override def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[(C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X)], Status) = {
        val (res0, status) = self.run(suite, testName, args, input)
        res0 match {
          case Some(res0) =>
            val (res1, s1) = fun1.run(suite, testName, args, res0)
            val (res2, s2) = fun2.run(suite, testName, args, res0)
            val (res3, s3) = fun3.run(suite, testName, args, res0)
            val (res4, s4) = fun4.run(suite, testName, args, res0)
            val (res5, s5) = fun5.run(suite, testName, args, res0)
            val (res6, s6) = fun6.run(suite, testName, args, res0)
            val (res7, s7) = fun7.run(suite, testName, args, res0)
            val (res8, s8) = fun8.run(suite, testName, args, res0)
            val (res9, s9) = fun9.run(suite, testName, args, res0)
            val (res10, s10) = fun10.run(suite, testName, args, res0)
            val (res11, s11) = fun11.run(suite, testName, args, res0)
            val (res12, s12) = fun12.run(suite, testName, args, res0)
            val (res13, s13) = fun13.run(suite, testName, args, res0)
            val (res14, s14) = fun14.run(suite, testName, args, res0)
            val (res15, s15) = fun15.run(suite, testName, args, res0)
            val (res16, s16) = fun16.run(suite, testName, args, res0)
            val (res17, s17) = fun17.run(suite, testName, args, res0)
            val (res18, s18) = fun18.run(suite, testName, args, res0)
            val (res19, s19) = fun19.run(suite, testName, args, res0)
            val (res20, s20) = fun20.run(suite, testName, args, res0)
            val (res21, s21) = fun21.run(suite, testName, args, res0)
            val (res22, s22) = fun22.run(suite, testName, args, res0)

            val retV = if (res1.isDefined && res2.isDefined && res3.isDefined && res4.isDefined && res5.isDefined && res6.isDefined && res7.isDefined && res8.isDefined && res9.isDefined && res10.isDefined && res11.isDefined && res12.isDefined && res13.isDefined && res14.isDefined && res15.isDefined && res16.isDefined && res17.isDefined && res18.isDefined && res19.isDefined && res20.isDefined && res21.isDefined && res22.isDefined) Some((res1.get, res2.get, res3.get, res4.get, res5.get, res6.get, res7.get, res8.get, res9.get, res10.get, res11.get, res12.get, res13.get, res14.get, res15.get, res16.get, res17.get, res18.get, res19.get, res20.get, res21.get, res22.get)) else None
            val retS = if (s1.succeeds && s2.succeeds && s3.succeeds && s4.succeeds && s5.succeeds && s6.succeeds && s7.succeeds && s8.succeeds && s9.succeeds && s10.succeeds && s11.succeeds && s12.succeeds && s13.succeeds && s14.succeeds && s15.succeeds && s16.succeeds && s17.succeeds && s18.succeeds && s19.succeeds && s20.succeeds && s21.succeeds && s22.succeeds) SucceededStatus else FailedStatus

            (retV, retS)

          case None =>
            fun1.cancel(suite, args)
            fun2.cancel(suite, args)
            fun3.cancel(suite, args)
            fun4.cancel(suite, args)
            fun5.cancel(suite, args)
            fun6.cancel(suite, args)
            fun7.cancel(suite, args)
            fun8.cancel(suite, args)
            fun9.cancel(suite, args)
            fun10.cancel(suite, args)
            fun11.cancel(suite, args)
            fun12.cancel(suite, args)
            fun13.cancel(suite, args)
            fun14.cancel(suite, args)
            fun15.cancel(suite, args)
            fun16.cancel(suite, args)
            fun17.cancel(suite, args)
            fun18.cancel(suite, args)
            fun19.cancel(suite, args)
            fun20.cancel(suite, args)
            fun21.cancel(suite, args)
            fun22.cancel(suite, args)
            (None, status)
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
    val startTime = currentTime
    reportTestStarting(suite, args.reporter, args.tracker, name, name, None, location)
    val duration = currentTime - startTime
    args.reporter(TestCanceled(args.tracker.nextOrdinal(), "Dependent test did not pass.", suite.suiteName, suite.suiteId, Some(suite.getClass.getName), name, name, collection.immutable.IndexedSeq.empty, None, Some(duration), Some(getEscapedIndentedTextForTest(name, 1, true)), location, None, None))
  }
  def testNames: Set[String]
  def run(suite: Suite, testName: Option[String], args: Args, input: A): (Option[B], Status) = {
    val startTime = currentTime
    reportTestStarting(suite, args.reporter, args.tracker, name, name, None, location)
    try {
      val result = thisTest1(input)
      val duration = currentTime - startTime
      reportTestSucceeded(suite, args.reporter, args.tracker, name, name, collection.immutable.IndexedSeq.empty, duration, getEscapedIndentedTextForTest(name, 1, true), None, location)
      (Some(result), SucceededStatus)
    }
    catch {
      case tce: TestCanceledException =>
        val message = getMessageForException(tce)
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
        val duration = currentTime - startTime
        reportTestCanceled(suite, args.reporter, tce, name, name, collection.immutable.IndexedSeq.empty, None, args.tracker, duration, getEscapedIndentedTextForTest(name, 1, true), loc)
        (None, SucceededStatus)

      case tce: TestPendingException =>
        val duration = currentTime - startTime
        reportTestPending(suite, args.reporter, args.tracker, name, name, collection.immutable.IndexedSeq.empty, duration, getEscapedIndentedTextForTest(name, 1, true), location)
        (None, SucceededStatus)

      case t: Throwable =>
        val message = getMessageForException(t)
        val payload =
          t match {
            case optPayload: PayloadField =>
              optPayload.payload
            case _ =>
              None
          }
        val duration = currentTime - startTime
        reportTestFailed(suite, args.reporter, t, name, name, collection.immutable.IndexedSeq.empty, None, args.tracker, duration, getEscapedIndentedTextForTest(name, 1, true), Some(SeeStackDepthException))
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

trait TestFlow extends Suite {

  def flow: Flow0[_]

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
