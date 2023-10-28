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

  def check(szp: SizeParam, sut: SystemUnderTest, initState: TState, gen: Generator[TCommand], initRnd: Randomizer)(implicit pos: source.Position, prettifier: Prettifier): R = {

    @tailrec def loop(count: Int, state: TState, rnd: Randomizer, accCmd: IndexedSeq[TCommand], accRes: IndexedSeq[TState]): R = {
      if (count > 0) {
        val (cmd, newRnd) = command(state, gen, rnd)
        if (preCondition(state, cmd, accCmd, accRes)) {
          val newState = nextState(state, cmd)
          val sutNewState = sut.nextState(state, cmd)
          if (newState != sutNewState) {
            val failingCommands = accCmd :+ cmd
            val failureMsg = FailureMessages.sutReturnedDifferentStateAfterExecutingCommands + EOL + failingCommands.map(_.toString).mkString("\n") + EOL + FailureMessages.initState(prettifier, initState) + EOL + FailureMessages.initSeed(prettifier, initRnd.seed)
            indicateFailure(sde => failureMsg, failureMsg, None, pos)
          }
          else if (!postCondition(state, newState, cmd, accCmd, accRes)) {
            val failingCommands = accCmd :+ cmd
            val failureMsg = FailureMessages.postConditionFailedAfterExecutingCommands + EOL + failingCommands.map(_.toString).mkString("\n") + EOL + FailureMessages.initState(prettifier, initState) + EOL + FailureMessages.initSeed(prettifier, initRnd.seed)
            indicateFailure(sde => failureMsg, failureMsg, None, pos)
          }
          else
            loop(count - 1, newState, newRnd, accCmd :+ cmd, accRes :+ newState)
        }
        else
          loop(count, state, newRnd, accCmd, accRes)
      }
      else
        indicateSuccess(FailureMessages.propertyCheckSucceeded)
    }

    loop(szp.size, initState, initRnd, IndexedSeq.empty, IndexedSeq.empty)
  }


  def check(szp: SizeParam)(implicit pos: source.Position, prettifier: Prettifier): R = {

    val (initState, gen, initRnd) = initialize

    val sut = createSystemUnderTest(initState)

    check(szp, sut, initState, gen, initRnd)
  }

}

trait AssertiongStatefulPropertyCheckModel extends StatefulPropertyCheckModel[Assertion] {

  private[scalatest] def indicateSuccess(message: => String): Assertion = succeed

  private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, optionalCause: Option[Throwable], pos: source.Position): Assertion = 
    fail(undecoratedMessage)

}