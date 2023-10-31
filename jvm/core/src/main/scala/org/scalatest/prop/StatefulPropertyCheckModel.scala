package org.scalatest.prop

import scala.annotation.tailrec
import scala.compat.Platform.EOL

import org.scalatest.{Assertion, Expectation, Fact}
import org.scalatest.Assertions.succeed
import org.scalatest.exceptions.{StackDepthException, GeneratorDrivenPropertyCheckFailedException}
import org.scalatest.FailureMessages

import org.scalactic.{source, Prettifier}
import org.scalactic.anyvals.PosZInt

import org.scalactic.ColCompatHelper.LazyListOrStream

trait StatefulPropertyCheckModel[R] {

  type TCommand
  type TState

  trait SystemUnderTest {
    def nextState(state: TState, command: TCommand): TState
    def state(): TState
  }

  def initialize: (TState, Generator[TCommand], Randomizer)

  def createSystemUnderTest(initState: TState): SystemUnderTest

  def nextState(state: TState, command: TCommand): TState

  def command(state: TState, gen: Generator[TCommand], rnd: Randomizer): (TCommand, Randomizer)

  def preCondition(state: TState, command: TCommand, accCmd: IndexedSeq[TCommand], accRes: IndexedSeq[TState]): Boolean

  def postCondition(oldState: TState, newState: TState, command: TCommand, accCmd: IndexedSeq[TCommand], accRes: IndexedSeq[TState]): Boolean

  private[scalatest] def indicateSuccess(message: => String, prettifier: Prettifier): R

  private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, prettifier: Prettifier, optionalCause: Option[Throwable], pos: source.Position, 
                                         initState: TState, initRnd: Randomizer, failingCmds: List[TCommand]): R

  case class NextRoseTree(value: Seq[TCommand]) extends RoseTree[Seq[TCommand]] {
    def shrinks: LazyListOrStream[RoseTree[Seq[TCommand]]] = {
      def resLazyListOrStream(theValue: Seq[TCommand]): LazyListOrStream[RoseTree[Seq[TCommand]]] = {
        if (theValue.length > 1) {
          val half: Int = theValue.length / 2
          val (left, right) = theValue.splitAt(half)
          Rose(right) #:: LazyListOrStream.empty
        }
        else 
          LazyListOrStream.empty
      }
      resLazyListOrStream(value)
    }
  }

  private def checkSut(szp: SizeParam, sut: SystemUnderTest, initState: TState, gen: Generator[TCommand], initRnd: Randomizer)(implicit pos: source.Position, prettifier: Prettifier): (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]) = {

    @tailrec def loop(count: Int, state: TState, rnd: Randomizer, accCmd: IndexedSeq[TCommand], accRnd: IndexedSeq[Randomizer], accRes: IndexedSeq[TState], failedPreconditionCount: Int): (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]) = {
      if (count > 0) {
        val (cmd, newRnd) = command(state, gen, rnd)
        if (preCondition(state, cmd, accCmd, accRes)) {
          val newState = nextState(state, cmd)
          val sutNewState = sut.nextState(state, cmd)
          if (newState != sutNewState) 
            (accCmd :+ cmd, accRnd :+ rnd, accRes :+ newState, Some(sutNewState))
          else if (!postCondition(state, newState, cmd, accCmd, accRes)) 
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


  def check(szp: SizeParam)(implicit pos: source.Position, prettifier: Prettifier): R = {

    def tryRun(trySzp: SizeParam, initState: TState, gen: Generator[TCommand], initRnd: Randomizer): (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]) = {
      val sut = createSystemUnderTest(initState)
      checkSut(trySzp, sut, initState, gen, initRnd)
    }

    def shrinkLoop(gen: Generator[TCommand], base: (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]), remainings: (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState])): (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]) = {
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

        val nextLength = PosZInt.ensuringValid(secondHalfRemainingRnd.length + 1)
      
        val (secondHalfCmd, secondHalfRnd, secondHalfRes, secondHalfFailingSutState) = tryRun(szp, firstHalfRemainingRes.last, gen, secondHalfRemainingRnd.head)
        secondHalfFailingSutState match {
          case Some(failingSutState) => // Successfully got failure using second half, we'll continue shrinking from there and forget about first half.
            shrinkLoop(gen, (baseCmd, baseRnd, baseRes, secondHalfFailingSutState), (secondHalfRemainingCmd, secondHalfRemainingRnd, secondHalfRemainingRes))
          case None => // Failed to get failure using second half, we'll continue with first half by using second half as base.
            shrinkLoop(gen, (secondHalfCmd ++ baseCmd, secondHalfRnd ++ baseRnd, secondHalfRes ++ baseRes, baseFailingSutState), (firstHalfRemainingCmd, firstHalfRemainingRnd, firstHalfRemainingRes))
        }
      }
      else 
        (remainingCmd ++ baseCmd, remainingRnd ++ baseRnd, remainingRes ++ baseRes, baseFailingSutState)
    }

    val (initState, gen, initRnd) = initialize

    val (firstAccCmd, firstAccRnd, firstAccRes, firstFailingSutState) = tryRun(szp, initState, gen, initRnd)
    val (accCmd, accRnd, accRes, failingSutState) = 
      if (firstFailingSutState.isEmpty) 
        (firstAccCmd, firstAccRnd, firstAccRes, firstFailingSutState)
      else 
        shrinkLoop(gen, (IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty, firstFailingSutState), (firstAccCmd, firstAccRnd, firstAccRes))

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

trait AssertiongStatefulPropertyCheckModel extends StatefulPropertyCheckModel[Assertion] {

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

trait ExpectationStatefulPropertyCheckModel extends StatefulPropertyCheckModel[Expectation] {

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