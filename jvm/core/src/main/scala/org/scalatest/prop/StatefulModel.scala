package org.scalatest.prop

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

    while (true) {
      val cmd = command(state)
      if (preCondition(state, cmd)) {
        val newState = nextState(state, cmd)
        val sutNewState = sut.nextState(state, cmd)
        if (newState != sutNewState)
          fail("SUT returned different state")
        else if (!postCondition(newState, cmd))
          fail("Post condition failed")
        else
          state = newState
      }
    }
  }

}