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
package org.scalautils

import java.text.MessageFormat

/**
 * A trait that represents a fact that:
 *
 * <ul>
 * <li>represents a boolean value</li>
 * <li>methods useful for failure messages construction</li>
 * <li>logical expression methods that makes <code>Fact</code> composable</li>
 * </ul>
 *
 */
trait Fact {

  private def makeString(rawString: String, args: IndexedSeq[Any]): String = {
    val msgFmt = new MessageFormat(rawString)
    msgFmt.format(args.map(Prettifier.default).toArray)
  }

  /**
   * Construct and return failure message, by applying arguments returned from <code>failureMessageArgs</code> to
   * raw message returned from <code>rawFailureMessage</code>
   */
  def failureMessage: String =
    if (failureMessageArgs.isEmpty) rawFailureMessage else makeString(rawFailureMessage, failureMessageArgs)

  /**
   * Construct and return negated failure message, by applying arguments returned from <code>negatedFailureMessageArgs</code> to
   * raw message returned from <code>rawNegatedFailureMessage</code>
   */
  def negatedFailureMessage: String =
    if (negatedFailureMessageArgs.isEmpty) rawNegatedFailureMessage else makeString(rawNegatedFailureMessage, negatedFailureMessageArgs)

  /**
   * Construct and return mid sentence failure message, by applying arguments returned from <code>midSentenceFailureMessageArgs</code> to
   * raw message returned from <code>rawMidSentenceFailureMessage</code>
   */
  def midSentenceFailureMessage: String =
    if (midSentenceFailureMessageArgs.isEmpty) rawMidSentenceFailureMessage else makeString(rawMidSentenceFailureMessage, midSentenceFailureMessageArgs)

  /**
   * Construct and return mid sentence negated failure message, by applying arguments returned from <code>midSentenceNegatedFailureMessageArgs</code> to
   * raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>
   */
  def midSentenceNegatedFailureMessage: String =
    if (midSentenceNegatedFailureMessageArgs.isEmpty) rawMidSentenceNegatedFailureMessage else makeString(rawMidSentenceNegatedFailureMessage, midSentenceNegatedFailureMessageArgs)

  /**
   * the <code>Boolean</code> value of this <code>Fact</code>
   */
  def value: Boolean

  /**
   * raw message to report a failure
   */
  def rawFailureMessage: String

  /**
   * raw message with a meaning opposite to that of the failure message
   */
  def rawNegatedFailureMessage: String

  /**
   * raw mid sentence message to report a failure
   */
  def rawMidSentenceFailureMessage: String

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   */
  def rawMidSentenceNegatedFailureMessage: String

  /**
   * Arguments to construct final failure message with raw message returned from <code>rawFailureMessage</code>.
   */
  def failureMessageArgs: IndexedSeq[Any]

  /**
   * Arguments to construct final negated failure message with raw message returned from <code>rawNegatedFailureMessage</code>.
   */
  def negatedFailureMessageArgs: IndexedSeq[Any]

  /**
   * Arguments to construct final mid sentence failure message with raw message returned from <code>rawMidSentenceFailureMessage</code>.
   */
  def midSentenceFailureMessageArgs: IndexedSeq[Any]

  /**
   * Arguments to construct final negated mid sentence failure message with raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>.
   */
  def midSentenceNegatedFailureMessageArgs: IndexedSeq[Any]

  /**
   * Logical <code>and</code> this <code>Fact</code> with another <code>Fact</code>
   *
   * @param fact another <code>Fact</code>
   * @return a <code>Fact</code> that represents the result of logical <code>and</code>
   */
  def &&(fact: => Fact): Fact =
    if (value) {
        val myValue = value
        new Fact {

          lazy val value: Boolean = myValue && fact.value

          def rawFailureMessage: String = Resources("commaBut")
          def rawNegatedFailureMessage: String = Resources("commaAnd")
          def rawMidSentenceFailureMessage: String = Resources("commaBut")
          def rawMidSentenceNegatedFailureMessage: String = Resources("commaAnd")

          def failureMessageArgs = Vector(this.negatedFailureMessage, fact.midSentenceFailureMessage)
          def negatedFailureMessageArgs = Vector(this.negatedFailureMessage, fact.midSentenceNegatedFailureMessage)
          def midSentenceFailureMessageArgs = Vector(this.midSentenceNegatedFailureMessage, fact.midSentenceFailureMessage)
          def midSentenceNegatedFailureMessageArgs = Vector(this.midSentenceNegatedFailureMessage, fact.midSentenceNegatedFailureMessage)
        }
      }
    else
      this

  /**
   * Logical <code>and</code> this <code>Fact</code> with another <code>Fact</code>
   *
   * @param fact another <code>Fact</code>
   * @return a <code>Fact</code> that represents the result of logical <code>and</code>
   */
  def &(fact: => Fact): Fact = &&(fact)

  /**
   * Logical <code>or</code> this <code>Fact</code> with another <code>Fact</code>
   *
   * @param fact another <code>Fact</code>
   * @return a <code>Fact</code> that represents the result of logical <code>or</code>
   */
  def ||(fact: => Fact): Fact = {
    def myValue = this.value // by-name to be lazy
    new Fact {

      lazy val value: Boolean = myValue || fact.value

      def rawFailureMessage: String = Resources("commaAnd")
      def rawNegatedFailureMessage: String = Resources("commaAnd")
      def rawMidSentenceFailureMessage: String = Resources("commaAnd")
      def rawMidSentenceNegatedFailureMessage: String = Resources("commaAnd")

      def failureMessageArgs = Vector(this.failureMessage, fact.midSentenceFailureMessage)
      def negatedFailureMessageArgs = Vector(this.failureMessage, fact.midSentenceNegatedFailureMessage)
      def midSentenceFailureMessageArgs = Vector(this.midSentenceFailureMessage, fact.midSentenceFailureMessage)
      def midSentenceNegatedFailureMessageArgs = Vector(this.midSentenceFailureMessage, fact.midSentenceNegatedFailureMessage)
    }
  }

  /**
   * Logical <code>or</code> this <code>Fact</code> with another <code>Fact</code>
   *
   * @param fact another <code>Fact</code>
   * @return a <code>Fact</code> that represents the result of logical <code>or</code>
   */
  def |(fact: => Fact): Fact = ||(fact)

  /**
   * Negate this <code>Fact</code>
   *
   * @return a <code>Fact</code> that represents the result of negating the original <code>Fact</code>
   */
  def unary_! : Fact =
    new Fact {

      lazy val value: Boolean = !this.value

      def rawFailureMessage: String = this.rawNegatedFailureMessage
      def rawNegatedFailureMessage: String = this.rawFailureMessage
      def rawMidSentenceFailureMessage: String = this.rawMidSentenceNegatedFailureMessage
      def rawMidSentenceNegatedFailureMessage: String = this.rawMidSentenceFailureMessage

      def failureMessageArgs: IndexedSeq[Any] = this.negatedFailureMessageArgs
      def negatedFailureMessageArgs: IndexedSeq[Any] = this.failureMessageArgs
      def midSentenceFailureMessageArgs: IndexedSeq[Any] = this.midSentenceNegatedFailureMessageArgs
      def midSentenceNegatedFailureMessageArgs: IndexedSeq[Any] = this.midSentenceFailureMessageArgs
    }

  /*
  def &&(bool: => Boolean): Fact = ...
  def ||(bool: => Boolean): Fact = ...
  */
}

/**
 * Simple macro fact that is used by <code>BooleanMacro</code> to wrap an unrecognized <code>Boolean</code> expression.
 *
 * @param expression the <code>Boolean</code> expression
 * @param expressionText the original expression text (source code)
 */
class SimpleMacroFact(expression: => Boolean, val expressionText: String) extends Fact {

  /**
   * the <code>Boolean</code> value of this <code>Fact</code>, evaluated from the passed in expression
   */
  lazy val value: Boolean = expression

  def rawFailureMessage: String = if (expressionText.isEmpty) Resources("expressionWasFalse") else Resources("wasFalse")
  def rawNegatedFailureMessage: String = if (expressionText.isEmpty) Resources("expressionWasTrue") else Resources("wasTrue")
  def rawMidSentenceFailureMessage: String = if (expressionText.isEmpty) Resources("expressionWasFalse") else Resources("wasFalse")
  def rawMidSentenceNegatedFailureMessage: String = if (expressionText.isEmpty) Resources("expressionWasTrue") else Resources("wasTrue")

  def failureMessageArgs: IndexedSeq[Any] = if (expressionText.isEmpty) Vector.empty else Vector(UnquotedString(expressionText))
  def negatedFailureMessageArgs: IndexedSeq[Any] = if (expressionText.isEmpty) Vector.empty else Vector(UnquotedString(expressionText))
  def midSentenceFailureMessageArgs: IndexedSeq[Any] = if (expressionText.isEmpty) Vector.empty else Vector(UnquotedString(expressionText))
  def midSentenceNegatedFailureMessageArgs: IndexedSeq[Any] = if (expressionText.isEmpty) Vector.empty else Vector(UnquotedString(expressionText))

}

class NotMacroFact(fact: Fact) extends Fact {

  lazy val value: Boolean = !fact.value

  def rawFailureMessage: String = fact.rawNegatedFailureMessage
  def rawNegatedFailureMessage: String = fact.rawFailureMessage
  def rawMidSentenceFailureMessage: String = fact.rawMidSentenceNegatedFailureMessage
  def rawMidSentenceNegatedFailureMessage: String = fact.rawMidSentenceFailureMessage

  def failureMessageArgs: IndexedSeq[Any] = fact.negatedFailureMessageArgs
  def negatedFailureMessageArgs: IndexedSeq[Any] = fact.failureMessageArgs
  def midSentenceFailureMessageArgs: IndexedSeq[Any] = fact.midSentenceNegatedFailureMessageArgs
  def midSentenceNegatedFailureMessageArgs: IndexedSeq[Any] = fact.midSentenceFailureMessageArgs
}

class BinaryMacroFact(left: Any, operator: String, right: Any, expression: => Boolean) extends Fact {

  def this(left: Any, operator: String, right: Any, fact: Fact) =
    this(left, operator, right, fact.value)

  lazy val value: Boolean = expression

  def getObjectsForFailureMessage =
    left match {
      case aEqualizer: org.scalautils.TripleEqualsSupport#Equalizer[_] =>
        Prettifier.getObjectsForFailureMessage(aEqualizer.leftSide, right)
      case aEqualizer: org.scalautils.TripleEqualsSupport#CheckingEqualizer[_] =>
        Prettifier.getObjectsForFailureMessage(aEqualizer.leftSide, right)
      case _ => Prettifier.getObjectsForFailureMessage(left, right)
    }

  def rawFailureMessage: String =
    operator match {
      case "==" => Resources("didNotEqual")
      case "===" => Resources("didNotEqual")
      case "!=" => Resources("equaled")
      case "!==" => Resources("equaled")
      case ">" => Resources("wasNotGreaterThan")
      case ">=" => Resources("wasNotGreaterThanOrEqualTo")
      case "<" => Resources("wasNotLessThan")
      case "<=" => Resources("wasNotLessThanOrEqualTo")
      case "&&" | "&" =>
        (left, right) match {
          case (leftFact: Fact, rightFact: Fact) =>
            if (leftFact.value)
              Resources("commaBut")
            else
              leftFact.rawFailureMessage
          case (leftFact: Fact, rightAny: Any) =>
            if (leftFact.value)
              Resources("commaBut")
            else
              leftFact.rawFailureMessage
          case _ =>
            Resources("commaBut")
        }
      case "||" | "|" => Resources("commaAnd")
      case _ => Resources("expressionWasFalse")
    }

  def rawNegatedFailureMessage: String =
    operator match {
      case "==" => Resources("equaled")
      case "===" => Resources("equaled")
      case "!=" => Resources("didNotEqual")
      case "!==" => Resources("didNotEqual")
      case ">" => Resources("wasGreaterThan")
      case ">=" => Resources("wasGreaterThanOrEqualTo")
      case "<" => Resources("wasLessThan")
      case "<=" => Resources("wasLessThanOrEqualTo")
      case "&&" | "&" => Resources("commaAnd")
      case "||" | "|" => Resources("commaAnd")
      case _ => Resources("expressionWasTrue")
    }

  def rawMidSentenceFailureMessage: String = rawFailureMessage
  def rawMidSentenceNegatedFailureMessage: String = rawNegatedFailureMessage

  def failureMessageArgs: IndexedSeq[Any] =
    operator match {
      case "==" | "===" | "!=" | "!==" | ">" | ">=" | "<" | "<=" =>
        val (leftee, rightee) = getObjectsForFailureMessage
        Vector(leftee, rightee)
      case "&&" | "&" =>
        (left, right) match {
          case (leftFact: Fact, rightFact: Fact) =>
            if (leftFact.value)
              Vector(UnquotedString(leftFact.negatedFailureMessage), UnquotedString(rightFact.midSentenceFailureMessage))
            else
              leftFact.failureMessageArgs
          case (leftFact: Fact, rightAny: Any) =>
            if (leftFact.value)
              Vector(UnquotedString(leftFact.negatedFailureMessage), rightAny)
            else
              leftFact.failureMessageArgs
          case (leftAny: Any, rightFact: Fact) =>
            Vector(leftAny, UnquotedString(if (rightFact.value) rightFact.midSentenceNegatedFailureMessage else rightFact.midSentenceFailureMessage))
          case _ =>
            Vector(left, right)
        }
      case "||" | "|" =>
        (left, right) match {
          case (leftFact: Fact, rightFact: Fact) =>
            Vector(UnquotedString(leftFact.failureMessage), UnquotedString(rightFact.midSentenceFailureMessage))
          case (leftFact: Fact, rightAny: Any) =>
            Vector(UnquotedString(leftFact.failureMessage), rightAny)
          case (leftAny: Any, rightFact: Fact) =>
            Vector(leftAny, UnquotedString(rightFact.midSentenceFailureMessage))
          case _ =>
            Vector(left, right)
        }
      case _ => Vector.empty
    }

  def negatedFailureMessageArgs: IndexedSeq[Any] =
    operator match {
      case "==" | "===" | "!=" | "!==" | ">" | ">=" | "<" | "<=" =>
        val (leftee, rightee) = getObjectsForFailureMessage
        Vector(leftee, rightee)
      case "&&" | "&" =>
        (left, right) match {
          case (leftFact: Fact, rightFact: Fact) =>
            Vector(
              UnquotedString(if (leftFact.value) leftFact.negatedFailureMessage else leftFact.failureMessage),
              UnquotedString(if (rightFact.value) rightFact.midSentenceNegatedFailureMessage else rightFact.midSentenceFailureMessage)
            )
          case (leftFact: Fact, rightAny: Any) =>
            Vector(UnquotedString(if (leftFact.value) leftFact.negatedFailureMessage else leftFact.failureMessage), rightAny)
          case (leftAny: Any, rightFact: Fact) =>
            Vector(leftAny, UnquotedString(if (rightFact.value) rightFact.midSentenceNegatedFailureMessage else rightFact.negatedFailureMessage))
          case _ =>
            Vector(left, right)
        }
      case "||" | "|" =>
        (left, right) match {
          case (leftFact: Fact, rightFact: Fact) =>
            Vector(
              UnquotedString(if (leftFact.value) leftFact.negatedFailureMessage else leftFact.failureMessage),
              UnquotedString(if (rightFact.value) rightFact.midSentenceNegatedFailureMessage else rightFact.midSentenceFailureMessage)
            )
          case (leftFact: Fact, rightAny: Any) =>
            Vector(UnquotedString(if (leftFact.value) leftFact.negatedFailureMessage else leftFact.failureMessage), rightAny)
          case (leftAny: Any, rightFact: Fact) =>
            Vector(leftAny, UnquotedString(if (rightFact.value) rightFact.midSentenceNegatedFailureMessage else rightFact.midSentenceFailureMessage))
          case _ =>
            Vector(left, right)
        }
      case _ => Vector.empty
    }

  def midSentenceFailureMessageArgs: IndexedSeq[Any] = failureMessageArgs
  def midSentenceNegatedFailureMessageArgs: IndexedSeq[Any] = negatedFailureMessageArgs

}

//case class Yes extends Fact

//case class No extends Fact