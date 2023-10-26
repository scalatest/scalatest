package org.scalatest.prop

import scala.annotation.tailrec
import org.scalatest.Assertion
import org.scalatest.Assertions.{succeed, fail}
import org.scalatest.exceptions.StackDepthException
import org.scalactic.source

trait StatefulPropertyCheckModel[R] {

  type TCommand
  type TState

  trait SystemUnderTest {
    def nextState(state: TState, command: TCommand): TState
  }

  def initialize: (TState, Generator[TCommand], Randomizer)

  def createSystemUnderTest(initState: TState): SystemUnderTest

  def nextState(state: TState, command: TCommand): TState

  def command(state: TState, gen: Generator[TCommand], rnd: Randomizer): (TCommand, Randomizer)

  def preCondition(state: TState, command: TCommand, accCmd: IndexedSeq[TCommand], accRes: IndexedSeq[TState]): Boolean

  def postCondition(oldState: TState, newState: TState, command: TCommand, accCmd: IndexedSeq[TCommand], accRes: IndexedSeq[TState]): Boolean

  private[scalatest] def indicateSuccess(message: => String): R

  private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, optionalCause: Option[Throwable], pos: source.Position): R

  def check(szp: SizeParam)(implicit pos: source.Position): Unit = {

    val (initState, gen, initRnd) = initialize

    val sut = createSystemUnderTest(initState)

    @tailrec def loop(count: Int, state: TState, rnd: Randomizer, accCmd: IndexedSeq[TCommand], accRes: IndexedSeq[TState]): R = {
      if (count > 0) {
        val (cmd, newRnd) = command(state, gen, rnd)
        if (preCondition(state, cmd, accCmd, accRes)) {
          val newState = nextState(state, cmd)
          val sutNewState = sut.nextState(state, cmd)
          if (newState != sutNewState) {
            val failureMsg = "SUT returned different state." // TODO: shrink and construct more meaning full message.
            indicateFailure(sde => failureMsg, failureMsg, None, pos)
          }
          else if (!postCondition(state, newState, cmd, accCmd, accRes)) {
            val failureMsg = "Post condition failed." // TODO: shrink and construct more meaning full message.
            indicateFailure(sde => failureMsg, failureMsg, None, pos)
          }
          else
            loop(count - 1, newState, newRnd, accCmd :+ cmd, accRes :+ newState)
        }
        else
          loop(count, state, newRnd, accCmd, accRes)
      }
      else
        indicateSuccess("OK, passed " + count + " tests")
    }

    loop(szp.size, initState, initRnd, IndexedSeq.empty, IndexedSeq.empty)
  }

}

trait AssertiongStatefulPropertyCheckModel extends StatefulPropertyCheckModel[Assertion] {

  private[scalatest] def indicateSuccess(message: => String): Assertion = succeed

  private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, optionalCause: Option[Throwable], pos: source.Position): Assertion = 
    fail(undecoratedMessage)

}