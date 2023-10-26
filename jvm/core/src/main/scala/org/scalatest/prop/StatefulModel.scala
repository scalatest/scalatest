package org.scalatest.prop

import scala.annotation.tailrec
import org.scalatest.Assertions.fail

trait StatefulModel {

  type Command
  type State

  trait SystemUnderTest {
    def nextState(state: State, command: Command): State
  }

  def initialState: (State, Generator[Command])

  def createSystemUnderTest(initState: State): SystemUnderTest

  def nextState(state: State, command: Command): State

  def command(state: State, gen: Generator[Command]): (Command, Generator[Command])

  def preCondition(state: State, command: Command): Boolean

  def postCondition(state: State, command: Command): Boolean

  def test(): Unit = {

    val (initState, initGen) = initialState

    val sut = createSystemUnderTest(initState)

    @tailrec def loop(count: Int, state: State, gen: Generator[Command]): Unit = {
      if (count > 0) {
        val (cmd, newGen) = command(state, gen)
        if (preCondition(state, cmd)) {
          val newState = nextState(state, cmd)
          val sutNewState = sut.nextState(state, cmd)
          if (newState != sutNewState)
            fail("SUT returned different state")
          else if (!postCondition(newState, cmd))
            fail("Post condition failed")
          else
            loop(count - 1, newState, newGen)
        }
        else
          loop(count, state, newGen)
      }
    }

    loop(100, initState, initGen)
  }

}