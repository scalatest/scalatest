package org.scalatest.prop

import scala.annotation.tailrec
import org.scalatest.Assertions.fail

trait StatefulModel {

  type Command
  type State

  trait SystemUnderTest {
    def nextState(state: State, command: Command): State
  }

  def initialState: State

  def createSystemUnderTest(initState: State): SystemUnderTest

  def nextState(state: State, command: Command): State

  def command(state: State): Command

  def preCondition(state: State, command: Command): Boolean

  def postCondition(state: State, command: Command): Boolean

  def test(): Unit = {

    var state = initialState

    val sut = createSystemUnderTest(state)

    @tailrec def loop(count: Int, state: State): Unit = {
      if (count > 0) {
        val cmd = command(state)
        if (preCondition(state, cmd)) {
          val newState = nextState(state, cmd)
          val sutNewState = sut.nextState(state, cmd)
          if (newState != sutNewState)
            fail("SUT returned different state")
          else if (!postCondition(newState, cmd))
            fail("Post condition failed")
          else
            loop(count - 1, newState)
        }
        else
          loop(count, state)
      }
    }

    loop(100, state)
  }

}