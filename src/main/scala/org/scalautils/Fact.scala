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
  def &&(fact: Fact): Fact =
    if (value)
      new AndFact(this, fact)
    else
      this

  /**
   * Logical <code>and</code> this <code>Fact</code> with another <code>Fact</code>
   *
   * @param fact another <code>Fact</code>
   * @return a <code>Fact</code> that represents the result of logical <code>and</code>
   */
  def &(fact: Fact): Fact = &&(fact)

  /**
   * Logical <code>or</code> this <code>Fact</code> with another <code>Fact</code>
   *
   * @param fact another <code>Fact</code>
   * @return a <code>Fact</code> that represents the result of logical <code>or</code>
   */
  def ||(fact: => Fact): Fact = new OrFact(this, fact)

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
  def unary_! : Fact = new NotFact(this)

  //def &&(boolValue: Boolean): Fact = &&(new SimpleFact(boolValue))
  //def ||(boolValue: Boolean): Fact = ||(new SimpleFact(boolValue))
}

class SimpleFact(expression: Boolean) extends Fact {

  /**
   * the <code>Boolean</code> value of this <code>Fact</code>
   */
  val value: Boolean = expression

  /**
   * raw message to report a failure
   *
   * @return Localized string for "Expression was false"
   */
  def rawFailureMessage: String = Resources("expressionWasFalse")

  /**
   * raw message with a meaning opposite to that of the failure message
   *
   * @return Localized string for "Expression was true"
   */
  def rawNegatedFailureMessage: String = Resources("expressionWasTrue")

  /**
   * raw mid sentence message to report a failure
   *
   * @return Localized string for "Expression was false"
   */
  def rawMidSentenceFailureMessage: String = Resources("expressionWasFalse")

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   *
   * @return Localized string for "Expression was false"
   */
  def rawMidSentenceNegatedFailureMessage: String = Resources("expressionWasTrue")

  /**
   * Arguments to construct final failure message with raw message returned from <code>rawFailureMessage</code>.
   *
   * @return empty <code>Vector</code>
   */
  def failureMessageArgs: IndexedSeq[Any] = Vector.empty

  /**
   * Arguments to construct final negated failure message with raw message returned from <code>rawNegatedFailureMessage</code>.
   *
   * @return empty <code>Vector</code>
   */
  def negatedFailureMessageArgs: IndexedSeq[Any] = Vector.empty

  /**
   * Arguments to construct final mid sentence failure message with raw message returned from <code>rawMidSentenceFailureMessage</code>.
   *
   * @return empty <code>Vector</code>
   */
  def midSentenceFailureMessageArgs: IndexedSeq[Any] = Vector.empty

  /**
   * Arguments to construct final negated mid sentence failure message with raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>.
   *
   * @return empty <code>Vector</code>
   */
  def midSentenceNegatedFailureMessageArgs: IndexedSeq[Any] = Vector.empty
}

/**
 * Fact that represents the result of logical <code>AND</code> of two <code>Fact</code>.
 *
 * @param fact1 the first <code>Fact</code>
 * @param fact2 the second <code>Fact</code>
 */
class AndFact(fact1: Fact, fact2: Fact) extends Fact {

  /**
   * the result of <code>fact1.value</code> logical <code>AND</code> <code>fact2.value</code>
   */
  lazy val value: Boolean = fact1.value && fact2.value

  /**
   * raw message to report a failure
   *
   * @return Localized raw string for "{0}, but {1}"
   */
  def rawFailureMessage: String = Resources("commaBut")

  /**
   * raw message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawNegatedFailureMessage: String = Resources("commaAnd")

  /**
   * raw mid sentence message to report a failure
   *
   * @return Localized raw string for "{0}, but {1}"
   */
  def rawMidSentenceFailureMessage: String = Resources("commaBut")

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawMidSentenceNegatedFailureMessage: String = Resources("commaAnd")

  /**
   * Arguments to construct final failure message with raw message returned from <code>rawFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>fact1.negatedFailureMessage</code> and <code>fact2.midSentenceFailureMessage</code>
   */
  def failureMessageArgs = Vector(fact1.negatedFailureMessage, fact2.midSentenceFailureMessage)

  /**
   * Arguments to construct final negated failure message with raw message returned from <code>rawNegatedFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>fact1.negatedFailureMessage</code> and <code>fact2.midSentenceNegatedFailureMessage</code>
   */
  def negatedFailureMessageArgs = Vector(fact1.negatedFailureMessage, fact2.midSentenceNegatedFailureMessage)

  /**
   * Arguments to construct final mid sentence failure message with raw message returned from <code>rawMidSentenceFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>fact1.midSentenceNegatedFailureMessage</code> and <code>fact2.midSentenceFailureMessage</code>
   */
  def midSentenceFailureMessageArgs = Vector(fact1.midSentenceNegatedFailureMessage, fact2.midSentenceFailureMessage)

  /**
   * Arguments to construct final negated mid sentence failure message with raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>fact1.midSentenceNegatedFailureMessage</code> and <code>fact2.midSentenceNegatedFailureMessage</code>
   */
  def midSentenceNegatedFailureMessageArgs = Vector(fact1.midSentenceNegatedFailureMessage, fact2.midSentenceNegatedFailureMessage)
}

/**
 * Fact that represents the result of logical <code>OR</code> of two <code>Fact</code>.
 *
 * @param fact1 the first <code>Fact</code>
 * @param fact2 the second <code>Fact</code>
 */
class OrFact(fact1: Fact, fact2: Fact) extends Fact {

  /**
   * the result of <code>fact1.value</code> logical <code>OR</code> <code>fact2.value</code>
   */
  lazy val value: Boolean = fact1.value || fact2.value

  /**
   * raw message to report a failure
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawFailureMessage: String = Resources("commaAnd")

  /**
   * raw message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawNegatedFailureMessage: String = Resources("commaAnd")

  /**
   * raw mid sentence message to report a failure
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawMidSentenceFailureMessage: String = Resources("commaAnd")

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawMidSentenceNegatedFailureMessage: String = Resources("commaAnd")

  /**
   * Arguments to construct final failure message with raw message returned from <code>rawFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>fact1.failureMessage</code> and <code>fact2.midSentenceFailureMessage</code>
   */
  def failureMessageArgs = Vector(fact1.failureMessage, fact2.midSentenceFailureMessage)

  /**
   * Arguments to construct final negated failure message with raw message returned from <code>rawNegatedFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>fact1.failureMessage</code> and <code>fact2.midSentenceNegatedFailureMessage</code>
   */
  def negatedFailureMessageArgs = Vector(fact1.failureMessage, fact2.midSentenceNegatedFailureMessage)

  /**
   * Arguments to construct final mid sentence failure message with raw message returned from <code>rawMidSentenceFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>fact1.midSentenceFailureMessage</code> and <code>fact2.midSentenceFailureMessage</code>
   */
  def midSentenceFailureMessageArgs = Vector(fact1.midSentenceFailureMessage, fact2.midSentenceFailureMessage)

  /**
   * Arguments to construct final negated mid sentence failure message with raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>fact1.midSentenceFailureMessage</code> and <code>fact2.midSentenceNegatedFailureMessage</code>
   */
  def midSentenceNegatedFailureMessageArgs = Vector(fact1.midSentenceFailureMessage, fact2.midSentenceNegatedFailureMessage)
}

/**
 * Fact that represents the result of logical <code>NOT</code> of the given <code>Fact</code>.
 *
 * @param fact the given <code>Fact</code>
 */
class NotFact(fact: Fact) extends Fact {

  /**
   * the negated result of <code>fact1.value</code>
   */
  lazy val value: Boolean = !fact.value

  /**
   * raw message to report a failure
   *
   * @return the passed in <code>fact.rawNegatedFailureMessage</code>
   */
  def rawFailureMessage: String = fact.rawNegatedFailureMessage

  /**
   * raw message with a meaning opposite to that of the failure message
   *
   * @return the passed in <code>fact.rawFailureMessage</code>
   */
  def rawNegatedFailureMessage: String = fact.rawFailureMessage

  /**
   * raw mid sentence message to report a failure
   *
   * @return the passed in <code>fact.rawMidSentenceNegatedFailureMessage</code>
   */
  def rawMidSentenceFailureMessage: String = fact.rawMidSentenceNegatedFailureMessage

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   *
   * @return the passed in <code>fact.rawMidSentenceFailureMessage</code>
   */
  def rawMidSentenceNegatedFailureMessage: String = fact.rawMidSentenceFailureMessage

  /**
   * Arguments to construct final failure message with raw message returned from <code>rawFailureMessage</code>.
   *
   * @return the passed in <code>fact.negatedFailureMessageArgs</code>
   */
  def failureMessageArgs: IndexedSeq[Any] = fact.negatedFailureMessageArgs

  /**
   * Arguments to construct final negated failure message with raw message returned from <code>rawNegatedFailureMessage</code>.
   *
   * @return the passed in <code>fact.failureMessageArgs</code>
   */
  def negatedFailureMessageArgs: IndexedSeq[Any] = fact.failureMessageArgs

  /**
   * Arguments to construct final mid sentence failure message with raw message returned from <code>rawMidSentenceFailureMessage</code>.
   *
   * @return the passed in <code>fact.midSentenceNegatedFailureMessageArgs</code>
   */
  def midSentenceFailureMessageArgs: IndexedSeq[Any] = fact.midSentenceNegatedFailureMessageArgs

  /**
   * Arguments to construct final negated mid sentence failure message with raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>.
   *
   * @return the passed in <code>fact.midSentenceFailureMessageArgs</code>
   */
  def midSentenceNegatedFailureMessageArgs: IndexedSeq[Any] = fact.midSentenceFailureMessageArgs
}

/**
 * Simple macro fact that is used by <code>BooleanMacro</code> to wrap an unrecognized <code>Boolean</code> expression.
 *
 * @param expression the <code>Boolean</code> expression
 * @param expressionText the original expression text (source code)
 */
class SimpleMacroFact(expression: Boolean, val expressionText: String) extends Fact {

  /**
   * the <code>Boolean</code> value of this <code>Fact</code>, holding the passed in expression value.
   */
  val value: Boolean = expression

  /**
   * raw message to report a failure
   *
   * @return Localized raw string of "Expression was false" if passed in <code>expressionText</code> is empty, else "{0} was false"
   */
  def rawFailureMessage: String = if (expressionText.isEmpty) Resources("expressionWasFalse") else Resources("wasFalse")

  /**
   * raw message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string of "Expression was true" if passed in <code>expressionText</code> is empty, else "{0} was true"
   */
  def rawNegatedFailureMessage: String = if (expressionText.isEmpty) Resources("expressionWasTrue") else Resources("wasTrue")

  /**
   * raw mid sentence message to report a failure
   *
   * @return Localized raw string of "Expression was false" if passed in <code>expressionText</code> is empty, else "{0} was false"
   */
  def rawMidSentenceFailureMessage: String = if (expressionText.isEmpty) Resources("expressionWasFalse") else Resources("wasFalse")

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string of "Expression was true" if passed in <code>expressionText</code> is empty, else "{0} was true"
   */
  def rawMidSentenceNegatedFailureMessage: String = if (expressionText.isEmpty) Resources("expressionWasTrue") else Resources("wasTrue")

  /**
   * Arguments to construct final failure message with raw message returned from <code>rawFailureMessage</code>.
   *
   * @return empty <code>Vector</code> if passed in <code>expressionText</code> is empty, else <code>Vector</code> that contains the unquoted <code>expressionText</code>
   */
  def failureMessageArgs: IndexedSeq[Any] = if (expressionText.isEmpty) Vector.empty else Vector(UnquotedString(expressionText))

  /**
   * Arguments to construct final negated failure message with raw message returned from <code>rawNegatedFailureMessage</code>.
   *
   * @return empty <code>Vector</code> if passed in <code>expressionText</code> is empty, else <code>Vector</code> that contains the unquoted <code>expressionText</code>
   */
  def negatedFailureMessageArgs: IndexedSeq[Any] = if (expressionText.isEmpty) Vector.empty else Vector(UnquotedString(expressionText))

  /**
   * Arguments to construct final mid sentence failure message with raw message returned from <code>rawMidSentenceFailureMessage</code>.
   *
   * @return empty <code>Vector</code> if passed in <code>expressionText</code> is empty, else <code>Vector</code> that contains the unquoted <code>expressionText</code>
   */
  def midSentenceFailureMessageArgs: IndexedSeq[Any] = if (expressionText.isEmpty) Vector.empty else Vector(UnquotedString(expressionText))

  /**
   * Arguments to construct final negated mid sentence failure message with raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>.
   *
   * @return empty <code>Vector</code> if passed in <code>expressionText</code> is empty, else <code>Vector</code> that contains the unquoted <code>expressionText</code>
   */
  def midSentenceNegatedFailureMessageArgs: IndexedSeq[Any] = if (expressionText.isEmpty) Vector.empty else Vector(UnquotedString(expressionText))

}

/**
 * Binary macro fact that is used by <code>BooleanMacro</code> to wrap a recognized <code>Boolean</code> expression, which includes <code>Boolean</code> expression that
 * uses <code>==</code>, <code>===</code>, <code>!=</code>, <code>!==</code>, <code>&gt;</code>, <code>&gt;=</code>, <code>&lt;</code>, <code>&lt;=</code>, <code>&&</code>,
 * <code>&</code>, <code>||</code> and <code>|</code>.
 *
 * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
 * @param operator the operator (method name) of the <code>Boolean</code> expression
 * @param right the right-hand-side (RHS) of the <code>Boolean</code> expression
 * @param expression the <code>Boolean</code> expression
 */
class BinaryMacroFact(left: Any, operator: String, right: Any, expression: Boolean) extends Fact {

  /**
   * Overloaded constructor that takes a <code>Fact</code> in place of <code>Boolean</code> expression.
   *
   * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
   * @param operator the operator (method name) of the <code>Boolean</code> expression
   * @param right the right-hand-side (RHS) of the <code>Boolean</code> expression
   * @param fact the <code>Fact</code> that will provide the <code>Boolean</code> expression value with <code>fact.value</code>
   */
  def this(left: Any, operator: String, right: Any, fact: Fact) =
    this(left, operator, right, fact.value)

  /**
   * the <code>Boolean</code> value of this <code>Fact</code>.
   */
  val value: Boolean = expression

  private def getObjectsForFailureMessage =
    left match {
      case aEqualizer: org.scalautils.TripleEqualsSupport#Equalizer[_] =>
        Prettifier.getObjectsForFailureMessage(aEqualizer.leftSide, right)
      case aEqualizer: org.scalautils.TripleEqualsSupport#CheckingEqualizer[_] =>
        Prettifier.getObjectsForFailureMessage(aEqualizer.leftSide, right)
      case _ => Prettifier.getObjectsForFailureMessage(left, right)
    }

  /**
   * raw message to report a failure, this method implementation will return the friendly raw message based on the passed
   * in <code>operator</code>.
   *
   * @return Localized friendly raw message based on the passed in <code>operator</code>
   */
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

  /**
   * raw message with a meaning opposite to that of the failure message, this method implementation will return the
   * friendly raw message based on the passed in <code>operator</code>.
   *
   * @return Localized negated friendly raw message based on the passed in <code>operator</code>
   */
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

  /**
   * raw mid sentence message to report a failure
   *
   * @return the same result as <code>rawFailureMessage</code>
   */
  def rawMidSentenceFailureMessage: String = rawFailureMessage

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   *
   * @return the same result as <code>rawNegatedFailureMessage</code>
   */
  def rawMidSentenceNegatedFailureMessage: String = rawNegatedFailureMessage

  /**
   * Arguments to construct final failure message with raw message returned from <code>rawFailureMessage</code>.  Based
   * on the passed in operator, this implementation will return the arguments needed by <code>rawFailureMessage</code>
   * to construct the final friendly failure message.
   *
   * @return Vector that contains arguments needed by <code>rawFailureMessage</code> to construct the final friendly failure message
   */
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

  /**
   * Arguments to construct final negated failure message with raw message returned from <code>rawNegatedFailureMessage</code>.  Based
   * on the passed in operator, this implementation will return the arguments needed by <code>rawNegatedFailureMessage</code> to construct
   * the final negated friendly failure message.
   *
   * @return Vector that contains arguments needed by <code>rawNegatedFailureMessage</code> to construct the final negated friendly failure message
   */
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

  /**
   * Arguments to construct final mid sentence failure message with raw message returned from <code>rawMidSentenceFailureMessage</code>.
   *
   * @return the same result as <code>failureMessageArgs</code>
   */
  def midSentenceFailureMessageArgs: IndexedSeq[Any] = failureMessageArgs

  /**
   * Arguments to construct final negated mid sentence failure message with raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>.
   *
   * @return the same result as <code>negatedFailureMessageArgs</code>
   */
  def midSentenceNegatedFailureMessageArgs: IndexedSeq[Any] = negatedFailureMessageArgs

}