/*
 * Copyright 2001-2023 Artima, Inc.
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
package org.scalatest.enablers

import org.scalactic.{source, Prettifier}
import org.scalactic.anyvals.PosZInt
import org.scalatest.{Assertion, FailureMessages, Expectation, Fact}
import org.scalatest.Assertions.succeed
import org.scalatest.prop.{SizeParam, Randomizer, Generator, StatefulPropertyCheckModel, SystemUnderTest, AsyncSystemUnderTest}
import org.scalatest.exceptions.{StackDepthException, GeneratorDrivenPropertyCheckFailedException}

import scala.annotation.tailrec
import scala.compat.Platform.EOL
import scala.concurrent.Future

trait StatefulPropCheckerAsserting[TCommand, TState, TSystemUnderTest, R] {
  def check(model: StatefulPropertyCheckModel[TCommand, TState, TSystemUnderTest, R], szp: SizeParam, pos: source.Position, prettifier: Prettifier): R
  private[scalatest] def indicateSuccess(message: => String, prettifier: Prettifier): R
  private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, prettifier: Prettifier, optionalCause: Option[Throwable], pos: source.Position, 
                                         initState: TState, initRnd: Randomizer, failingCmds: List[TCommand]): R
}

abstract class UnitStatefulPropCheckerAsserting {

  abstract class StatefulPropCheckerAssertingImpl[TCommand, TState, R] extends StatefulPropCheckerAsserting[TCommand, TState, SystemUnderTest[TCommand, TState], R] {
    private def checkSut(model: StatefulPropertyCheckModel[TCommand, TState, SystemUnderTest[TCommand, TState], R], szp: SizeParam, sut: SystemUnderTest[TCommand, TState], initState: TState, gen: Generator[TCommand], initRnd: Randomizer)(implicit pos: source.Position, prettifier: Prettifier): (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]) = {

      @tailrec def loop(count: Int, state: TState, rnd: Randomizer, accCmd: IndexedSeq[TCommand], accRnd: IndexedSeq[Randomizer], accRes: IndexedSeq[TState], failedPreconditionCount: Int): (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]) = {
        if (count > 0) {
          val (cmd, newRnd) = model.command(state, gen, rnd)
          if (model.preCondition(state, cmd, accCmd, accRes)) {
            val newState = model.nextState(state, cmd)
            val sutNewState = sut.nextState(state, cmd)
            if (newState != sutNewState) 
              (accCmd :+ cmd, accRnd :+ rnd, accRes :+ newState, Some(sutNewState))
            else if (!model.postCondition(state, newState, cmd, accCmd, accRes)) 
              (accCmd :+ cmd, accRnd :+ rnd, accRes :+ newState, Some(sutNewState))
            else
              loop(count - 1, newState, newRnd, accCmd :+ cmd, accRnd :+ newRnd, accRes :+ newState, 0)
          }
          else {
            if (failedPreconditionCount < Generator.MaxLoopCount)
              loop(count, state, newRnd, accCmd, accRnd, accRes, failedPreconditionCount + 1)
            else
              throw new IllegalStateException(FailureMessages.commandGeneratorExceededMaxLoopCount(prettifier, Generator.MaxLoopCount))  
          }
        }
        else 
          (accCmd, accRnd, accRes, None)
      }

      loop(szp.size, initState, initRnd, IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty, 0)
    }

    def tryRun(model: StatefulPropertyCheckModel[TCommand, TState, SystemUnderTest[TCommand, TState], R], trySzp: SizeParam, initState: TState, gen: Generator[TCommand], initRnd: Randomizer): (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]) = {
      val sut = model.createSystemUnderTest(initState)
      checkSut(model, trySzp, sut, initState, gen, initRnd)
    }

    def shrinkLoop(model: StatefulPropertyCheckModel[TCommand, TState, SystemUnderTest[TCommand, TState], R], trySzp: SizeParam, gen: Generator[TCommand], base: (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]), remainings: (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState])): (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]) = {
      val (remainingCmd, remainingRnd, remainingRes) = remainings
      val (baseCmd, baseRnd, baseRes, baseFailingSutState) = base
      if (remainingCmd.length > 2) {  
        val halfSize = remainingCmd.length / 2

        val firstHalfRemainingCmd = remainingCmd.take(halfSize)
        val secondHalfRemainingCmd = remainingCmd.drop(halfSize)

        val firstHalfRemainingRnd = remainingRnd.take(halfSize)
        val secondHalfRemainingRnd = remainingRnd.drop(halfSize)

        val firstHalfRemainingRes = remainingRes.take(halfSize)
        val secondHalfRemainingRes = remainingRes.drop(halfSize)

        val (secondHalfCmd, secondHalfRnd, secondHalfRes, secondHalfFailingSutState) = tryRun(model, trySzp, firstHalfRemainingRes.last, gen, secondHalfRemainingRnd.head)
        secondHalfFailingSutState match {
          case Some(failingSutState) => // Successfully got failure using second half, we'll continue shrinking from there and forget about first half.
            shrinkLoop(model, trySzp, gen, (baseCmd, baseRnd, baseRes, secondHalfFailingSutState), (secondHalfRemainingCmd, secondHalfRemainingRnd, secondHalfRemainingRes))
          case None => // Failed to get failure using second half, we'll continue with first half by using second half as base.
            shrinkLoop(model, trySzp, gen, (secondHalfCmd ++ baseCmd, secondHalfRnd ++ baseRnd, secondHalfRes ++ baseRes, baseFailingSutState), (firstHalfRemainingCmd, firstHalfRemainingRnd, firstHalfRemainingRes))
        }
      }
      else 
        (remainingCmd ++ baseCmd, remainingRnd ++ baseRnd, remainingRes ++ baseRes, baseFailingSutState)
    }

    def check(model: StatefulPropertyCheckModel[TCommand, TState, SystemUnderTest[TCommand, TState], R], szp: SizeParam, pos: source.Position, prettifier: Prettifier): R = {
      val (initState, gen, initRnd) = model.initialize

      val (firstAccCmd, firstAccRnd, firstAccRes, firstFailingSutState) = tryRun(model, szp, initState, gen, initRnd)
      val (accCmd, accRnd, accRes, failingSutState) = 
      if (firstFailingSutState.isEmpty) 
        (firstAccCmd, firstAccRnd, firstAccRes, firstFailingSutState)
      else 
        shrinkLoop(model, szp, gen, (IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty, firstFailingSutState), (firstAccCmd, firstAccRnd, firstAccRes))
      failingSutState match {
        case Some(sutState) =>
          val resInitState = accRes.head 
          val resInitRnd = accRnd.head
          val failureMsg = 
            if (accRes.last == sutState)
              FailureMessages.postConditionFailedAfterExecutingCommands + EOL + accCmd.map(_.toString).mkString("\n") + EOL + FailureMessages.initState(prettifier, resInitState) + EOL + FailureMessages.initSeed(prettifier, resInitRnd.seed)
            else
              FailureMessages.sutReturnedDifferentStateAfterExecutingCommands + EOL + accCmd.map(_.toString).mkString("\n") + EOL + FailureMessages.initState(prettifier, resInitState) + EOL + FailureMessages.initSeed(prettifier, resInitRnd.seed)
          indicateFailure(sde => failureMsg, failureMsg, prettifier, None, pos, resInitState, resInitRnd, accCmd.toList)
        case None =>
          indicateSuccess(FailureMessages.propertyCheckSucceeded, prettifier)
      }
    }
  }

}

trait FutureStatefulPropCheckerAsserting {

  abstract class FutureStatefulPropCheckerAssertingImpl[TCommand, TState] extends StatefulPropCheckerAsserting[TCommand, TState, AsyncSystemUnderTest[TCommand, TState], Future[Assertion]] {

    implicit val executionContext: scala.concurrent.ExecutionContext

    private def checkSut(model: StatefulPropertyCheckModel[TCommand, TState, AsyncSystemUnderTest[TCommand, TState], Future[Assertion]], szp: SizeParam, sut: AsyncSystemUnderTest[TCommand, TState], initState: TState, gen: Generator[TCommand], initRnd: Randomizer)(implicit pos: source.Position, prettifier: Prettifier): Future[(IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState])] = {

      def loop(count: Int, state: TState, rnd: Randomizer, accCmd: IndexedSeq[TCommand], accRnd: IndexedSeq[Randomizer], accRes: IndexedSeq[TState], failedPreconditionCount: Int): Future[(IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState])] = {
        if (count > 0) {
          val (cmd, newRnd) = model.command(state, gen, rnd)
          if (model.preCondition(state, cmd, accCmd, accRes)) {
            val newState = model.nextState(state, cmd)
            val sutNewStateFut = sut.nextState(state, cmd)
            sutNewStateFut.flatMap { sutNewState =>
              if (newState != sutNewState) 
                Future.successful((accCmd :+ cmd, accRnd :+ rnd, accRes :+ newState, Some(sutNewState)))
              else if (!model.postCondition(state, newState, cmd, accCmd, accRes)) 
                Future.successful((accCmd :+ cmd, accRnd :+ rnd, accRes :+ newState, Some(sutNewState)))
              else
                loop(count - 1, newState, newRnd, accCmd :+ cmd, accRnd :+ newRnd, accRes :+ newState, 0)
            }
          }
          else {
            if (failedPreconditionCount < Generator.MaxLoopCount)
              loop(count, state, newRnd, accCmd, accRnd, accRes, failedPreconditionCount + 1)
            else
              throw new IllegalStateException(FailureMessages.commandGeneratorExceededMaxLoopCount(prettifier, Generator.MaxLoopCount))  
          }
        }
        else 
          Future.successful((accCmd, accRnd, accRes, None))
      }

      loop(szp.size, initState, initRnd, IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty, 0)
    }

    def tryRun(model: StatefulPropertyCheckModel[TCommand, TState, AsyncSystemUnderTest[TCommand, TState], Future[Assertion]], trySzp: SizeParam, initState: TState, gen: Generator[TCommand], initRnd: Randomizer): Future[(IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState])] = {
      val sut = model.createSystemUnderTest(initState)
      checkSut(model, trySzp, sut, initState, gen, initRnd)
    }

    def shrinkLoop(model: StatefulPropertyCheckModel[TCommand, TState, AsyncSystemUnderTest[TCommand, TState], Future[Assertion]], trySzp: SizeParam, gen: Generator[TCommand], base: (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]), remainings: (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState])): Future[(IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState])] = {
      val (remainingCmd, remainingRnd, remainingRes) = remainings
      val (baseCmd, baseRnd, baseRes, baseFailingSutState) = base
      if (remainingCmd.length > 2) {  
        val halfSize = remainingCmd.length / 2

        val firstHalfRemainingCmd = remainingCmd.take(halfSize)
        val secondHalfRemainingCmd = remainingCmd.drop(halfSize)

        val firstHalfRemainingRnd = remainingRnd.take(halfSize)
        val secondHalfRemainingRnd = remainingRnd.drop(halfSize)

        val firstHalfRemainingRes = remainingRes.take(halfSize)
        val secondHalfRemainingRes = remainingRes.drop(halfSize)

        tryRun(model, trySzp, firstHalfRemainingRes.last, gen, secondHalfRemainingRnd.head).flatMap { case (secondHalfCmd, secondHalfRnd, secondHalfRes, secondHalfFailingSutState) =>
          secondHalfFailingSutState match {
            case Some(failingSutState) => // Successfully got failure using second half, we'll continue shrinking from there and forget about first half.
              shrinkLoop(model, trySzp, gen, (baseCmd, baseRnd, baseRes, secondHalfFailingSutState), (secondHalfRemainingCmd, secondHalfRemainingRnd, secondHalfRemainingRes))
            case None => // Failed to get failure using second half, we'll continue with first half by using second half as base.
              shrinkLoop(model, trySzp, gen, (secondHalfCmd ++ baseCmd, secondHalfRnd ++ baseRnd, secondHalfRes ++ baseRes, baseFailingSutState), (firstHalfRemainingCmd, firstHalfRemainingRnd, firstHalfRemainingRes))
          }
        }
      }
      else 
        Future.successful((remainingCmd ++ baseCmd, remainingRnd ++ baseRnd, remainingRes ++ baseRes, baseFailingSutState))
    }

    def check(model: StatefulPropertyCheckModel[TCommand, TState, AsyncSystemUnderTest[TCommand, TState], Future[Assertion]], szp: SizeParam, pos: source.Position, prettifier: Prettifier): Future[Assertion] = {
      val (initState, gen, initRnd) = model.initialize

      tryRun(model, szp, initState, gen, initRnd).flatMap { case ((firstAccCmd, firstAccRnd, firstAccRes, firstFailingSutState)) =>
        if (firstFailingSutState.isEmpty) 
          Future.successful((firstAccCmd, firstAccRnd, firstAccRes, firstFailingSutState))
        else 
          shrinkLoop(model, szp, gen, (IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty, firstFailingSutState), (firstAccCmd, firstAccRnd, firstAccRes))
      }.flatMap { case (accCmd, accRnd, accRes, failingSutState) =>
        failingSutState match {
          case Some(sutState) =>
            val resInitState = accRes.head 
            val resInitRnd = accRnd.head
            val failureMsg = 
              if (accRes.last == sutState)
                FailureMessages.postConditionFailedAfterExecutingCommands + EOL + accCmd.map(_.toString).mkString("\n") + EOL + FailureMessages.initState(prettifier, resInitState) + EOL + FailureMessages.initSeed(prettifier, resInitRnd.seed)
              else
                FailureMessages.sutReturnedDifferentStateAfterExecutingCommands + EOL + accCmd.map(_.toString).mkString("\n") + EOL + FailureMessages.initState(prettifier, resInitState) + EOL + FailureMessages.initSeed(prettifier, resInitRnd.seed)
            indicateFailure(sde => failureMsg, failureMsg, prettifier, None, pos, resInitState, resInitRnd, accCmd.toList)
          case None =>
            indicateSuccess(FailureMessages.propertyCheckSucceeded, prettifier)
        }
      }
    }

  }

}

abstract class ExpectationStatefulPropCheckerAsserting extends UnitStatefulPropCheckerAsserting with FutureStatefulPropCheckerAsserting {

  implicit def assertingNatureOfExpectation[TCommand, TState]: StatefulPropCheckerAsserting[TCommand, TState, SystemUnderTest[TCommand, TState], Expectation] =
    new StatefulPropCheckerAssertingImpl[TCommand, TState, Expectation] {
      private[scalatest] def indicateSuccess(message: => String, prettifier: Prettifier): Expectation = Fact.Yes(message, prettifier)

      private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, prettifier: Prettifier, optionalCause: Option[Throwable], pos: source.Position, 
                                             initState: TState, initRnd: Randomizer, failingCmds: List[TCommand]): Expectation = {
        val gdpcfe =
          new GeneratorDrivenPropertyCheckFailedException(
            messageFun,
            optionalCause,
            pos,
            Some((initState, initRnd)),
            undecoratedMessage,
            failingCmds,
            None,
            List.empty
          )
          val message: String = gdpcfe.getMessage
          Fact.No(message, prettifier)  
      }
    }

}

object StatefulPropCheckerAsserting extends ExpectationStatefulPropCheckerAsserting {

  implicit def assertingNatureOfAssertion[TCommand, TState]: StatefulPropCheckerAsserting[TCommand, TState, SystemUnderTest[TCommand, TState], Assertion] =
    new StatefulPropCheckerAssertingImpl[TCommand, TState, Assertion] {
      private[scalatest] def indicateSuccess(message: => String, prettifier: Prettifier): Assertion = succeed
      private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, prettifier: Prettifier, optionalCause: Option[Throwable], pos: source.Position, 
                                             initState: TState, initRnd: Randomizer, failingCmds: List[TCommand]): Assertion = 
        throw new GeneratorDrivenPropertyCheckFailedException(
          messageFun,
          optionalCause,
          pos,
          Some((initState, initRnd)),
          undecoratedMessage,
          failingCmds,
          None,
          List.empty
        )  
    }

  implicit def assertingNatureOfFutureAssertion[TCommand, TState](implicit exeCtx: scala.concurrent.ExecutionContext): StatefulPropCheckerAsserting[TCommand, TState, AsyncSystemUnderTest[TCommand, TState], Future[Assertion]] =
    new FutureStatefulPropCheckerAssertingImpl[TCommand, TState] {
      implicit val executionContext: scala.concurrent.ExecutionContext = exeCtx
      private[scalatest] def indicateSuccess(message: => String, prettifier: Prettifier): Future[Assertion] = Future.successful(succeed)
      private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, prettifier: Prettifier, optionalCause: Option[Throwable], pos: source.Position, 
                                             initState: TState, initRnd: Randomizer, failingCmds: List[TCommand]): Future[Assertion] = 
        Future.failed(
          new GeneratorDrivenPropertyCheckFailedException(
            messageFun,
            optionalCause,
            pos,
            Some((initState, initRnd)),
            undecoratedMessage,
            failingCmds,
            None,
            List.empty
          )
        )  
    }    
    
}