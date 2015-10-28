/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.enablers

import org.scalatest._
import org.scalatest.exceptions.StackDepthException
import scala.annotation.tailrec
import scala.collection.GenTraversable
import Suite.indentLines
import org.scalatest.FailureMessages.decorateToStringValue
import org.scalatest.exceptions.StackDepthExceptionHelper.{getStackDepthFun, getStackDepth}

trait InspectorAsserting[T] {
  type Result
  def doForAll[E](xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result
  def doForAtLeast[E](min: Int, xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result
  def doForAtMost[E](max: Int, xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result
  def doForExactly[E](succeededCount: Int, xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result
  def doForNo[E](xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result
  def doForBetween[E](from: Int, upTo: Int, xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result
  def doForEvery[E](xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result
}

abstract class UnitInspectorAsserting {

  abstract class InspectorAssertingImpl[T] extends InspectorAsserting[T] {

    import InspectorAsserting._

    def doForAll[E](xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result = {
      val xsIsMap = isMap(original)
      val result =
        runFor(xs.toIterator, xsIsMap, 0, new ForResult[E], fun, _.failedElements.length > 0)
      if (result.failedElements.length > 0)
        indicateFailure(
          if (shorthand)
            Resources.allShorthandFailed(indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
          else
            Resources.forAllFailed(indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original)),
          Some(result.failedElements(0)._3),
          getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
        )
      else indicateSuccess("forAll succeeded")
    }

    def doForAtLeast[E](min: Int, xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result = {
      @tailrec
      def forAtLeastAcc(itr: Iterator[E], includeIndex: Boolean, index: Int, passedCount: Int, messageAcc: IndexedSeq[String]): (Int, IndexedSeq[String]) = {
        if (itr.hasNext) {
          val head = itr.next
          val (newPassedCount, newMessageAcc) =
            try {
              fun(head)
              (passedCount + 1, messageAcc)
            }
            catch {
              case e if !shouldPropagate(e) =>
                val xsIsMap = isMap(original)
                val messageKey = head match {
                  case tuple: Tuple2[_, _] if xsIsMap => tuple._1.toString
                  case entry: Entry[_, _] if xsIsMap => entry.getKey.toString
                  case _ => index.toString
                }
                (passedCount, messageAcc :+ createMessage(messageKey, e, xsIsMap))
            }
          if (newPassedCount < min)
            forAtLeastAcc(itr, includeIndex, index + 1, newPassedCount, newMessageAcc)
          else
            (newPassedCount, newMessageAcc)
        }
        else
          (passedCount, messageAcc)
      }

      if (min <= 0)
        throw new IllegalArgumentException(Resources.forAssertionsMoreThanZero("'min'"))

      val (passedCount, messageAcc) = forAtLeastAcc(xs.toIterator, xs.isInstanceOf[Seq[E]], 0, 0, IndexedSeq.empty)
      if (passedCount < min)
        indicateFailure(
          if (shorthand)
            if (passedCount > 0)
              Resources.atLeastShorthandFailed(min.toString, elementLabel(passedCount), indentErrorMessages(messageAcc).mkString(", \n"), decorateToStringValue(original))
            else
              Resources.atLeastShorthandFailedNoElement(min.toString, indentErrorMessages(messageAcc).mkString(", \n"), decorateToStringValue(original))
          else
            if (passedCount > 0)
              Resources.forAtLeastFailed(min.toString, elementLabel(passedCount), indentErrorMessages(messageAcc).mkString(", \n"), decorateToStringValue(original))
            else
              Resources.forAtLeastFailedNoElement(min.toString, indentErrorMessages(messageAcc).mkString(", \n"), decorateToStringValue(original)),
          None,
          getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
        )
      else indicateSuccess("forAtLeast succeeded")
    }

    def doForAtMost[E](max: Int, xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result = {
      if (max <= 0)
        throw new IllegalArgumentException(Resources.forAssertionsMoreThanZero("'max'"))

      val xsIsMap = isMap(original)
      val result =
        runFor(xs.toIterator, xsIsMap, 0, new ForResult[E], fun, _.passedCount > max)
      if (result.passedCount > max)
        indicateFailure(
          if (shorthand)
            Resources.atMostShorthandFailed(max.toString, result.passedCount.toString, keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original))
          else
            Resources.forAtMostFailed(max.toString, result.passedCount.toString, keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original)),
          None,
          getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
        )
      else indicateSuccess("forAtMost succeeded")
    }

    def doForExactly[E](succeededCount: Int, xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result = {
      if (succeededCount <= 0)
        throw new IllegalArgumentException(Resources.forAssertionsMoreThanZero("'succeededCount'"))

      val xsIsMap = isMap(original)
      val result =
        runFor(xs.toIterator, xsIsMap, 0, new ForResult[E], fun, _.passedCount > succeededCount)
      if (result.passedCount != succeededCount)
        indicateFailure(
          if (shorthand)
            if (result.passedCount == 0)
              Resources.exactlyShorthandFailedNoElement(succeededCount.toString, indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
            else {
              if (result.passedCount < succeededCount)
                Resources.exactlyShorthandFailedLess(succeededCount.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
              else
                Resources.exactlyShorthandFailedMore(succeededCount.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original))
            }
          else
            if (result.passedCount == 0)
              Resources.forExactlyFailedNoElement(succeededCount.toString, indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
            else {
              if (result.passedCount < succeededCount)
                Resources.forExactlyFailedLess(succeededCount.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
              else
                Resources.forExactlyFailedMore(succeededCount.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original))
            },
          None,
          getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
        )
      else indicateSuccess("forExactly succeeded")
    }

    def doForNo[E](xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result = {
      val xsIsMap = isMap(original)
      val result =
        runFor(xs.toIterator, xsIsMap, 0, new ForResult[E], fun, _.passedCount != 0)
      if (result.passedCount != 0)
        indicateFailure(
          if (shorthand)
            Resources.noShorthandFailed(keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original))
          else
            Resources.forNoFailed(keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original)),
          None,
          getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
        )
      else indicateSuccess("forNo succeeded")
    }

    def doForBetween[E](from: Int, upTo: Int, xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result = {
      if (from < 0)
        throw new IllegalArgumentException(Resources.forAssertionsMoreThanEqualZero("'from'"))
      if (upTo <= 0)
        throw new IllegalArgumentException(Resources.forAssertionsMoreThanZero("'upTo'"))
      if (upTo <= from)
        throw new IllegalArgumentException(Resources.forAssertionsMoreThan("'upTo'", "'from'"))

      val xsIsMap = isMap(original)
      val result =
        runFor(xs.toIterator, xsIsMap, 0, new ForResult[E], fun, _.passedCount > upTo)
      if (result.passedCount < from || result.passedCount > upTo)
        indicateFailure(
          if (shorthand)
            if (result.passedCount == 0)
              Resources.betweenShorthandFailedNoElement(from.toString, upTo.toString, indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
            else {
              if (result.passedCount < from)
                Resources.betweenShorthandFailedLess(from.toString, upTo.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
              else
                Resources.betweenShorthandFailedMore(from.toString, upTo.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original))
            }
          else
            if (result.passedCount == 0)
              Resources.forBetweenFailedNoElement(from.toString, upTo.toString, indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
            else {
              if (result.passedCount < from)
                Resources.forBetweenFailedLess(from.toString, upTo.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), indentErrorMessages(result.messageAcc).mkString(", \n"), decorateToStringValue(original))
              else
                Resources.forBetweenFailedMore(from.toString, upTo.toString, elementLabel(result.passedCount), keyOrIndexLabel(original, result.passedElements), decorateToStringValue(original))
            },
          None,
          getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
        )
      else indicateSuccess("forBetween succeeded")
    }

    def doForEvery[E](xs: GenTraversable[E], original: Any, shorthand: Boolean, sourceFileName: String, methodName: String, stackDepthAdjustment: Int)(fun: E => T): Result = {
      @tailrec
      def runAndCollectErrorMessage[E](itr: Iterator[E], messageList: IndexedSeq[String], index: Int)(fun: E => T): IndexedSeq[String] = {
        if (itr.hasNext) {
          val head = itr.next
          val newMessageList =
            try {
              fun(head)
              messageList
            }
            catch {
              case e if !shouldPropagate(e) =>
                val xsIsMap = isMap(original)
                val messageKey = head match {
                  case tuple: Tuple2[_, _] if xsIsMap => tuple._1.toString
                  case entry: Entry[_, _] if xsIsMap => entry.getKey.toString
                  case _ => index.toString
                }
                messageList :+ createMessage(messageKey, e, xsIsMap)
            }

          runAndCollectErrorMessage(itr, newMessageList, index + 1)(fun)
        }
        else
          messageList
      }
      val messageList = runAndCollectErrorMessage(xs.toIterator, IndexedSeq.empty, 0)(fun)
      if (messageList.size > 0)
        indicateFailure(
          if (shorthand)
            Resources.everyShorthandFailed(indentErrorMessages(messageList).mkString(", \n"), decorateToStringValue(original))
          else
            Resources.forEveryFailed(indentErrorMessages(messageList).mkString(", \n"), decorateToStringValue(original)),
          None,
          getStackDepthFun(sourceFileName, methodName, stackDepthAdjustment)
        )
      else indicateSuccess("forEvery succeeded")
    }

    // TODO: Why is this a by-name? Well, I made it a by-name because it was one in MatchersHelper.
    // Why is it a by-name there?
    // CS: because we want to construct the message lazily.
    def indicateSuccess(message: => String): Result

    def indicateFailure(message: => String, optionalCause: Option[Throwable], stackDepthFun: StackDepthException => Int): Result
  }

  implicit def assertingNatureOfT[T]: InspectorAsserting[T] { type Result = Unit } =
    new InspectorAssertingImpl[T] {
      type Result = Unit
      def indicateSuccess(message: => String): Unit = ()
      def indicateFailure(message: => String, optionalCause: Option[Throwable], stackDepthFun: StackDepthException => Int): Unit = {
        throw new exceptions.TestFailedException(
          sde => Some(message),
          optionalCause,
          stackDepthFun
        )
      }
    }
}

abstract class FactInspectorAsserting extends UnitInspectorAsserting {

  implicit def assertingNatureOfExpectation: InspectorAsserting[Expectation] { type Result = Expectation } = {
    new InspectorAssertingImpl[Expectation] {
      type Result = Expectation
      def indicateSuccess(message: => String): Expectation = Fact.Yes(message)
      def indicateFailure(message: => String, optionalCause: Option[Throwable], stackDepthFun: StackDepthException => Int): Expectation = Fact.No(message)
    }
  }
}

object InspectorAsserting extends FactInspectorAsserting {

  implicit def assertingNatureOfAssertion: InspectorAsserting[Assertion] { type Result = Assertion } =
    new InspectorAssertingImpl[Assertion] {
      type Result = Assertion
      def indicateSuccess(message: => String): Assertion = Succeeded
      def indicateFailure(message: => String, optionalCause: Option[Throwable], stackDepthFun: StackDepthException => Int): Assertion = {
        throw new exceptions.TestFailedException(
          sde => Some(message),
          optionalCause,
          stackDepthFun
        )
      }
    }

  private[scalatest] final def indentErrorMessages(messages: IndexedSeq[String]) = indentLines(1, messages)

  private[scalatest] final def isMap(xs: Any): Boolean =
    xs match {
      case _: collection.GenMap[_, _] => true
      // SKIP-SCALATESTJS-START
      case _: java.util.Map[_, _] => true
      // SKIP-SCALATESTJS-END
      case _ => false
    }

  private[scalatest] final def shouldPropagate(throwable: Throwable): Boolean =
    throwable match {
      case _: exceptions.NotAllowedException |
           _: exceptions.TestPendingException |
           _: exceptions.TestCanceledException => true
      case _ if Suite.anExceptionThatShouldCauseAnAbort(throwable) => true
      case _ => false
    }

  private[scalatest] final def createMessage(messageKey: String, t: Throwable, xsIsMap: Boolean): String =
    t match {
      case sde: exceptions.StackDepthException =>
        sde.failedCodeFileNameAndLineNumberString match {
          case Some(failedCodeFileNameAndLineNumber) =>
            if (xsIsMap)
              Resources.forAssertionsGenMapMessageWithStackDepth(messageKey, sde.getMessage, failedCodeFileNameAndLineNumber)
            else
              Resources.forAssertionsGenTraversableMessageWithStackDepth(messageKey, sde.getMessage, failedCodeFileNameAndLineNumber)
          case None =>
            if (xsIsMap)
              Resources.forAssertionsGenMapMessageWithoutStackDepth(messageKey, sde.getMessage)
            else
              Resources.forAssertionsGenTraversableMessageWithoutStackDepth(messageKey, sde.getMessage)
        }
      case _ =>
        if (xsIsMap)
          Resources.forAssertionsGenMapMessageWithoutStackDepth(messageKey, if (t.getMessage != null) t.getMessage else "null")
        else
          Resources.forAssertionsGenTraversableMessageWithoutStackDepth(messageKey, if (t.getMessage != null) t.getMessage else "null")
    }

  private[scalatest] final def elementLabel(count: Int): String =
    if (count > 1) Resources.forAssertionsElements(count.toString) else Resources.forAssertionsElement(count.toString)

  private[scalatest] final case class ForResult[T](passedCount: Int = 0, messageAcc: IndexedSeq[String] = IndexedSeq.empty,
                          passedElements: IndexedSeq[(Int, T)] = IndexedSeq.empty, failedElements: IndexedSeq[(Int, T, Throwable)] = IndexedSeq.empty)

  @tailrec
  private[scalatest] final def runFor[T, ASSERTION](itr: Iterator[T], xsIsMap: Boolean, index:Int, result: ForResult[T], fun: T => ASSERTION, stopFun: ForResult[_] => Boolean): ForResult[T] = {
    if (itr.hasNext) {
      val head = itr.next
      val newResult =
        try {
          fun(head)
          result.copy(passedCount = result.passedCount + 1, passedElements = result.passedElements :+ (index, head))
        }
        catch {
          case e if !shouldPropagate(e) =>
            val messageKey = head match {
              case tuple: Tuple2[_, _] if xsIsMap => tuple._1.toString
              case entry: Entry[_, _] if xsIsMap => entry.getKey.toString
              case _ => index.toString
            }
            result.copy(messageAcc = result.messageAcc :+ createMessage(messageKey, e, xsIsMap), failedElements = result.failedElements :+ (index, head, e))
        }
      if (stopFun(newResult))
        newResult
      else
        runFor(itr, xsIsMap, index + 1, newResult, fun, stopFun)
    }
    else
      result
  }

  private[scalatest] final def keyOrIndexLabel(xs: Any, passedElements: IndexedSeq[(Int, _)]): String = {
    def makeAndLabel(indexes: IndexedSeq[Int]): String =
      if (indexes.length > 1)
        indexes.dropRight(1).mkString(", ") + " and " + indexes.last
      else
        indexes.mkString(", ")

    val (xsIsMap, elements) = xs match {
      // SKIP-SCALATESTJS-START
      case _: collection.GenMap[_, _] | _: java.util.Map[_, _] =>
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY case _: collection.GenMap[_, _] =>
        val elements = passedElements.map{ case (index, e) =>
          e match {
            case tuple2: Tuple2[_, _] => tuple2._1
            // SKIP-SCALATESTJS-START
            case entry: java.util.Map.Entry[_, _] => entry.getKey
            // SKIP-SCALATESTJS-END
            case _ => index
          }
        }
        (true, elements)
      case _ =>
        (false, passedElements.map(_._1))
    }

    if (elements.length > 1)
      if (xsIsMap)
        Resources.forAssertionsKeyAndLabel(elements.dropRight(1).mkString(", "), elements.last.toString)
      else
        Resources.forAssertionsIndexAndLabel(elements.dropRight(1).mkString(", "), elements.last.toString)
    else
    if (xsIsMap)
      Resources.forAssertionsKeyLabel(elements.mkString(", "))
    else
      Resources.forAssertionsIndexLabel(elements.mkString(", "))
  }
}

