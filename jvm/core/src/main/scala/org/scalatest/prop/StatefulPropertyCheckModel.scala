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

/**
  * the trait for the system under test
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
 * Stateful property check model trait that can be used for building model for stateful property-based testing.
 *
 * @tparam TCommand the type of the command
 * @tparam TState the type of the state
 * @tparam R the result type of the property check
 */
trait StatefulPropertyCheckModel[TCommand, TState, R] {

  /**
   * Initialize state, command generator and randomizer.
   *
   * @return a tuple of initialized state, command generator and randomizer
   */
  def initialize: (TState, Generator[TCommand], Randomizer)

  /**
   * Create system under test instance with the given initial state.
   */
  def createSystemUnderTest(initState: TState): SystemUnderTest[TCommand, TState]

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

  private def checkSut(szp: SizeParam, sut: SystemUnderTest[TCommand, TState], initState: TState, gen: Generator[TCommand], initRnd: Randomizer)(implicit pos: source.Position, prettifier: Prettifier): (IndexedSeq[TCommand], IndexedSeq[Randomizer], IndexedSeq[TState], Option[TState]) = {

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

  /**
   * Check the property with the given size parameter.
   *
   * @param szp the size parameter, which contains the size of the valid command data to be executed.
   * @param pos the source position
   * @param prettifier the prettifier
   * @return the result of the property check
   */
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

/**
 * Stateful property check model trait that can be used for building model for stateful property-based testing using assertions.
 *
 * This trait is used for ScalaTest assertions.
 */
trait AssertiongStatefulPropertyCheckModel[TCommand, TState] extends StatefulPropertyCheckModel[TCommand, TState, Assertion] {

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

/**
 * Stateful property check model trait that can be used for building model for stateful property-based testing using expectations.
 *
 * This trait is used for ScalaTest expectations.
 */
trait ExpectationStatefulPropertyCheckModel[TCommand, TState] extends StatefulPropertyCheckModel[TCommand, TState, Expectation] {

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