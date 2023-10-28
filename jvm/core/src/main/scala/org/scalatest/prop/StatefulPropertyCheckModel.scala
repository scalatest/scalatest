package org.scalatest.prop

import scala.annotation.tailrec
import scala.compat.Platform.EOL

import org.scalatest.Assertion
import org.scalatest.Assertions.{succeed, fail}
import org.scalatest.exceptions.StackDepthException
import org.scalatest.FailureMessages

import org.scalactic.{source, Prettifier}

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

  private[scalatest] def indicateSuccess(message: => String): R

  private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, optionalCause: Option[Throwable], pos: source.Position): R

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

  private def checkSut(szp: SizeParam, sut: SystemUnderTest, initState: TState, gen: Generator[TCommand], initRnd: Randomizer)(implicit pos: source.Position, prettifier: Prettifier): (IndexedSeq[TCommand], IndexedSeq[TState], Option[TState]) = {

    @tailrec def loop(count: Int, state: TState, rnd: Randomizer, accCmd: IndexedSeq[TCommand], accRes: IndexedSeq[TState]): (IndexedSeq[TCommand], IndexedSeq[TState], Option[TState]) = {
      if (count > 0) {
        val (cmd, newRnd) = command(state, gen, rnd)
        if (preCondition(state, cmd, accCmd, accRes)) {
          val newState = nextState(state, cmd)
          val sutNewState = sut.nextState(state, cmd)
          if (newState != sutNewState) 
            (accCmd :+ cmd, accRes :+ newState, Some(sutNewState))
          else if (!postCondition(state, newState, cmd, accCmd, accRes)) 
            (accCmd :+ cmd, accRes :+ newState, Some(sutNewState))
          else
            loop(count - 1, newState, newRnd, accCmd :+ cmd, accRes :+ newState)
        }
        else
          loop(count, state, newRnd, accCmd, accRes)
      }
      else
        (accCmd, accRes, None)
    }

    loop(szp.size, initState, initRnd, IndexedSeq.empty, IndexedSeq.empty)
  }


  def check(szp: SizeParam)(implicit pos: source.Position, prettifier: Prettifier): R = {

    val (initState, gen, initRnd) = initialize

    val sut = createSystemUnderTest(initState)

    val (accCmd, accRes, failingSutState) = checkSut(szp, sut, initState, gen, initRnd)

    failingSutState match {
      case Some(sutState) =>
        val failureMsg = 
          if (accRes.last == sutState)
            FailureMessages.postConditionFailedAfterExecutingCommands + EOL + accCmd.map(_.toString).mkString("\n") + EOL + FailureMessages.initState(prettifier, initState) + EOL + FailureMessages.initSeed(prettifier, initRnd.seed)
          else
            FailureMessages.sutReturnedDifferentStateAfterExecutingCommands + EOL + accCmd.map(_.toString).mkString("\n") + EOL + FailureMessages.initState(prettifier, initState) + EOL + FailureMessages.initSeed(prettifier, initRnd.seed)
        indicateFailure(sde => failureMsg, failureMsg, None, pos)
      case None =>
        indicateSuccess(FailureMessages.propertyCheckSucceeded)
    }      
  }

}

trait AssertiongStatefulPropertyCheckModel extends StatefulPropertyCheckModel[Assertion] {

  private[scalatest] def indicateSuccess(message: => String): Assertion = succeed

  private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, optionalCause: Option[Throwable], pos: source.Position): Assertion = 
    fail(undecoratedMessage)

}