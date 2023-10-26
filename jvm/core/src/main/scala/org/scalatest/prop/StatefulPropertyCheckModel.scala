package org.scalatest.prop

import scala.annotation.tailrec
import org.scalatest.exceptions.StackDepthException
import org.scalactic.source

trait StatefulPropertyCheckModel[R] {

  type Command
  type State

  trait SystemUnderTest {
    def nextState(state: State, command: Command): State
  }

  def initialState: (State, Generator[Command])

  def createSystemUnderTest(initState: State): SystemUnderTest

  def nextState(state: State, command: Command): State

  def command(state: State, gen: Generator[Command]): (Command, Generator[Command])

  def preCondition(state: State, command: Command, accCmd: IndexedSeq[Command], accRes: IndexedSeq[State]): Boolean

  def postCondition(oldState: State, newState: State, command: Command, accCmd: IndexedSeq[Command], accRes: IndexedSeq[State]): Boolean

  private[scalatest] def indicateSuccess(message: => String): R

  private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, optionalCause: Option[Throwable], pos: source.Position): R

  def check(szp: SizeParam)(implicit pos: source.Position): Unit = {

    val (initState, initGen) = initialState

    val sut = createSystemUnderTest(initState)

    @tailrec def loop(count: Int, state: State, gen: Generator[Command], accCmd: IndexedSeq[Command], accRes: IndexedSeq[State]): R = {
      if (count > 0) {
        val (cmd, newGen) = command(state, gen)
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
            loop(count - 1, newState, newGen, accCmd :+ cmd, accRes :+ newState)
        }
        else
          loop(count, state, newGen, accCmd, accRes)
      }
      else
        indicateSuccess("OK, passed " + count + " tests")
    }

    loop(szp.size, initState, initGen, IndexedSeq.empty, IndexedSeq.empty)
  }

}