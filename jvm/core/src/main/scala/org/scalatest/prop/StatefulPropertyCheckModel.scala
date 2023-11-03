package org.scalatest.prop

import scala.annotation.tailrec
import scala.compat.Platform.EOL
import scala.concurrent.Future

import org.scalatest.{Assertion, Expectation, Fact}
import org.scalatest.Assertions.succeed
import org.scalatest.exceptions.{StackDepthException, GeneratorDrivenPropertyCheckFailedException}
import org.scalatest.enablers.StatefulPropCheckerAsserting
import org.scalatest.FailureMessages

import org.scalactic.{source, Prettifier}
import org.scalactic.anyvals.PosZInt

import org.scalactic.ColCompatHelper.LazyListOrStream

/**
  * The trait for the system under test
  */
trait SystemUnderTest[TCommand, TState] {
  /**
    * Execute the given command and return the next state.
    *
    * @param state the current state
    * @param command the command to execute
    * @return the next state
    */
  def nextState(state: TState, command: TCommand): TState
  /**
    * Return the current state.
    *
    * @return the current state
    */
  def state(): TState
}

/**
  * The trait for the asynchronous system under test
  */
trait AsyncSystemUnderTest[TCommand, TState] {
  /**
    * Execute the given command and return the future next state.
    *
    * @param state the current state
    * @param command the command to execute
    * @return the future of next state
    */
  def nextState(state: TState, command: TCommand): Future[TState]
  /**
    * Return the future current state.
    *
    * @return the future current state
    */
  def state(): Future[TState]
}

/**
 * Stateful property check model trait that can be used for building model for stateful property-based testing.
 *
 * @tparam TCommand the type of the command
 * @tparam TState the type of the state
 * @tparam R the result type of the property check
 */
trait StatefulPropertyCheckModel[TCommand, TState, TSystemUnderTest, R] {

  /**
   * Initialize state, command generator and randomizer.
   *
   * @return a tuple of initialized state, command generator and randomizer
   */
  def initialize: (TState, Generator[TCommand], Randomizer)

  /**
   * Create system under test instance with the given initial state.
   */
  def createSystemUnderTest(initState: TState): TSystemUnderTest

  /**
   * Return the next state after executing the given command.
   * 
   * @param state the current state
   * @param command the command to execute
   * @return the next state
   */
  def nextState(state: TState, command: TCommand): TState

  /**
   * Generate the next command and randomizer.
   *
   * @param state the current state
   * @param gen the command generator
   * @param rnd the current randomizer
   * @return a tuple of the next command and randomizer
   */
  def command(state: TState, gen: Generator[TCommand], rnd: Randomizer): (TCommand, Randomizer) = {
    val (nextV, _, newRnd) = gen.next(SizeParam(1, 1, 1), List.empty, rnd)
    (nextV.value, newRnd)
  }

  /**
   * Pre-condition check if the given command can be executed in the given state.
   * If this function returned <code>false</code>, the command will be discarded and a new one will be generated.
   *
   * @param state the current state
   * @param command the command to execute
   * @param accCmd the accumulated commands
   * @param accRes the accumulated states
   * @return true if the given command can be executed in the given state
   */
  def preCondition(state: TState, command: TCommand, accCmd: IndexedSeq[TCommand], accRes: IndexedSeq[TState]): Boolean

  /**
   * Post-condition check if the given <code>oldState</code>, <code>newState</code>, <code>command</code> and accumulated values is valid, 
   * if this function returned <code>false</code>, the check shall fail.
   *
   * @param oldState the old state
   * @param newState the new state
   * @param command the command executed
   * @param accCmd the accumulated commands
   * @param accRes the accumulated states
   * @return true if pre-condition is valid
   */
  def postCondition(oldState: TState, newState: TState, command: TCommand, accCmd: IndexedSeq[TCommand], accRes: IndexedSeq[TState]): Boolean

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

  /**
   * Check the property with the given size parameter.
   *
   * @param szp the size parameter, which contains the size of the valid command data to be executed.
   * @param pos the source position
   * @param prettifier the prettifier
   * @return the result of the property check
   */
  def check(szp: SizeParam)(implicit asserting: StatefulPropCheckerAsserting[TCommand, TState, TSystemUnderTest, R], pos: source.Position, prettifier: Prettifier): R = {
    asserting.check(this, szp, pos, prettifier)
  }

}