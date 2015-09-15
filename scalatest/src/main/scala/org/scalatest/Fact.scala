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
package org.scalatest

import org.scalactic.Prettifier
import org.scalatest.exceptions.TestFailedException

sealed abstract class Fact {

  val rawFactMessage: String
  val rawNegatedFactMessage: String
  val rawMidSentenceFactMessage: String
  val rawMidSentenceNegatedFactMessage: String

  val factMessageArgs: IndexedSeq[Any]
  val negatedFactMessageArgs: IndexedSeq[Any]
  val midSentenceFactMessageArgs: IndexedSeq[Any]
  val midSentenceNegatedFactMessageArgs: IndexedSeq[Any]

  val composite: Boolean
  val prettifier: Prettifier

  val cause: Option[Throwable] = None

  def isYes: Boolean

  final def isNo: Boolean = !isYes

  final def toBoolean: Boolean = isYes

  final def toAssertion: Assertion =
    if (isYes) Succeeded
    else throw new TestFailedException(factMessage, 2)

  /**
   * Get a negated version of this Fact, sub type will be negated and all messages field will be substituted with its counter-part.
   *
   * @return a negated version of this Fact
   */
  def unary_!(): Fact = Fact.Unary_!(this)

  def ||(rhs: => Fact): Fact = if (isYes) this else Fact.Binary_||(this, rhs)

  def &&(rhs: => Fact): Fact = if (isNo) this else Fact.Binary_&&(this, rhs)

  final val stringPrefix: String = if (isYes) "Yes" else "No"

  /**
   * Construct failure message to report if a fact fails, using <code>rawFactMessage</code>, <code>factMessageArgs</code> and <code>prettifier</code>
   *
   * @return failure message to report if a fact fails
   */
  def factMessage: String =
    if (factMessageArgs.isEmpty) rawFactMessage
    else makeString(rawFactMessage, factMessageArgs)

  def negatedFactMessage: String =
    if (negatedFactMessageArgs.isEmpty) rawNegatedFactMessage
    else makeString(rawNegatedFactMessage, negatedFactMessageArgs)

  /**
   * Construct failure message suitable for appearing mid-sentence, using <code>rawMidSentenceFactMessage</code>, <code>midSentenceFactMessageArgs</code> and <code>prettifier</code>
   *
   * @return failure message suitable for appearing mid-sentence
   */
  def midSentenceFactMessage: String =
    if (midSentenceFactMessageArgs.isEmpty) rawMidSentenceFactMessage
    else makeString(rawMidSentenceFactMessage, midSentenceFactMessageArgs)

  def midSentenceNegatedFactMessage: String =
    if (midSentenceNegatedFactMessageArgs.isEmpty) rawMidSentenceNegatedFactMessage
    else makeString(rawMidSentenceNegatedFactMessage, midSentenceNegatedFactMessageArgs)

  private def makeString(raw: String, args: IndexedSeq[Any]): String =
    Resources.formatString(raw, args.map(Prettifier.default).toArray)

  override def toString: String = stringPrefix + "(" + factMessage + ")"
}

object Fact {

  case class No(
    rawFactMessage: String,
    rawNegatedFactMessage: String,
    rawMidSentenceFactMessage: String,
    rawMidSentenceNegatedFactMessage: String,
    factMessageArgs: IndexedSeq[Any],
    negatedFactMessageArgs: IndexedSeq[Any],
    midSentenceFactMessageArgs: IndexedSeq[Any],
    midSentenceNegatedFactMessageArgs: IndexedSeq[Any],
    composite: Boolean = false,
    override val cause: Option[Throwable] = None,
    prettifier: Prettifier = Prettifier.default
  ) extends Fact {

    def isYes: Boolean = false

  }

/*
factMessage is the negated one, if need be, and negatedFactMessage is a simpler one.
*/

  /**
   * Companion object for the <code>No</code> case class.
   *
   * @author Bill Venners
   */
  object No {

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>factMessage</code>, 
     * <code>negativeFailureMessage</code>, <code>midSentenceFactMessage</code>, 
     * <code>midSentenceNegatedFailureMessage</code>, <code>factMessageArgs</code>, and <code>negatedFailureMessageArgs</code> fields.
     * <code>factMessageArgs</code>, and <code>negatedFailureMessageArgs</code> will be used in place of <code>midSentenceFactMessageArgs</code>
     * and <code>midSentenceNegatedFailureMessageArgs</code>.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawMidSentenceFactMessage raw failure message to report if a match fails
     * @param factMessageArgs arguments for constructing failure message to report if a match fails
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any]
    ): No =
      new No(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        false,
        None,
        Prettifier.default
      )
  
    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceNegatedFailureMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawMidSentenceFactMessage raw failure message to report if a match fails
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String
    ): No =
      new No(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        None,
        Prettifier.default
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegatedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceNegatedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its negated and mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawNegatedFactMessage raw negated to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceNegatedFactMessage raw mid-sentence negated message to report for this fact
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawNegatedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceNegatedFactMessage: String
    ): No =
      new No(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        None,
        Prettifier.default
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegatedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceNegatedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its negated and mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawNegatedFactMessage raw negated to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceNegatedFactMessage raw mid-sentence negated message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code> and <code>rawMidSentenceFactMessage</code>
     * @param negatedFactMessageArgs arguments for <code>rawNegatedFactMessage</code> and <code>rawMidSentenceNegatedFactMessage</code>
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawNegatedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceNegatedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      negatedFactMessageArgs: IndexedSeq[Any]
    ): No =
      new No(
        rawFactMessage,
        rawNegatedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceNegatedFactMessage,
        factMessageArgs,
        negatedFactMessageArgs,
        factMessageArgs,
        negatedFactMessageArgs,
        false,
        None,
        Prettifier.default
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegatedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceNegatedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its negated and mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawNegatedFactMessage raw negated to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceNegatedFactMessage raw mid-sentence negated message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code>
     * @param negatedFactMessageArgs arguments for <code>rawNegatedFactMessage</code>
     * @param midSentenceFactMessageArgs arguments for <code>rawMidSentenceFactMessage</code>
     * @param midSentenceNegatedFactMessageArgs arguments for <code>rawMidSentenceNegatedFactMessage</code>
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawNegatedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceNegatedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      negatedFactMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any],
      midSentenceNegatedFactMessageArgs: IndexedSeq[Any]
    ): No =
      new No(
        rawFactMessage,
        rawNegatedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceNegatedFactMessage,
        factMessageArgs,
        negatedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceNegatedFactMessageArgs,
        false,
        None,
        Prettifier.default
      )
  
    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>, and
     * <code>rawNegativeFailureMessage</code> fields. The <code>rawMidSentenceFactMessage</code> will return the same
     * string as <code>rawFactMessage</code>, and the <code>rawMidSentenceNegatedFailureMessage</code> will return the
     * same string as <code>rawNegatedFailureMessage</code>.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages that have same mid-sentence messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String
    ): No =
      new No(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        None,
        Prettifier.default
      )
  
    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>factMessageArgs</code> and <code>negatedFailureMessageArgs</code> fields.
     * The <code>rawMidSentenceFactMessage</code> will return the same string as <code>rawFactMessage</code>, and the
     * <code>rawMidSentenceNegatedFailureMessage</code> will return the same string as <code>rawNegatedFailureMessage</code>.
     * The <code>midSentenceFactMessageArgs</code> will return the same as <code>factMessageArgs</code>, and the
     * <code>midSentenceNegatedFailureMessageArgs</code> will return the same as <code>negatedFailureMessageArgs</code>.
     * This is suitable to create No with lazy error messages that have same mid-sentence and use different arguments for
     * negated messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
     * @param factMessageArgs arguments for constructing failure message to report if a match fails
     * @param negatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      factMessageArgs: IndexedSeq[Any]
    ) =
      new No(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        false,
        None,
        Prettifier.default
      )
  }
  
  case class Yes(
    rawFactMessage: String,
    rawNegatedFactMessage: String,
    rawMidSentenceFactMessage: String,
    rawMidSentenceNegatedFactMessage: String,
    factMessageArgs: IndexedSeq[Any],
    negatedFactMessageArgs: IndexedSeq[Any],
    midSentenceFactMessageArgs: IndexedSeq[Any],
    midSentenceNegatedFactMessageArgs: IndexedSeq[Any],
    composite: Boolean = false,
    override val cause: Option[Throwable] = None,
    prettifier: Prettifier = Prettifier.default
  ) extends Fact {
  
    def isYes: Boolean = true
  }
  
  /**
   * Companion object for the <code>Yes</code> case class.
   *
   * @author Bill Venners
   */
  object Yes {
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed code>factMessage</code>, 
     * <code>negativeFailureMessage</code>, <code>midSentenceFactMessage</code>, 
     * <code>midSentenceNegatedFailureMessage</code>, <code>factMessageArgs</code>, and <code>negatedFailureMessageArgs</code> fields.
     * <code>factMessageArgs</code>, and <code>negatedFailureMessageArgs</code> will be used in place of <code>midSentenceFactMessageArgs</code>
     * and <code>midSentenceNegatedFailureMessageArgs</code>.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawMidSentenceFactMessage raw failure message to report if a match fails
     * @param factMessageArgs arguments for constructing failure message to report if a match fails
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any]
    ): Yes =
      new Yes(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        false,
        None,
        Prettifier.default
      )
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceNegatedFailureMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create Yes with eager error messages, and its mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawMidSentenceFactMessage raw failure message to report if a match fails
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String
    ): Yes =
      new Yes(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        None,
        Prettifier.default
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawFactMessage</code>, <code>rawNegatedFactMessage</code>, <code>rawMidSentenceFactMessage</code> and
     * <code>rawMidSentenceNegatedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code>
     * values.  This is suitable to create Yes with eager error messages, and its negated and mid-sentence messages
     * need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawNegatedFactMessage raw negated message to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceNegatedFactMessage raw mid-sentence negated message to report for this fact
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawNegatedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceNegatedFactMessage: String
    ): Yes =
      new Yes(
        rawFactMessage,
        rawNegatedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceNegatedFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        None,
        Prettifier.default
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawFactMessage</code>, <code>rawNegatedFactMessage</code>, <code>rawMidSentenceFactMessage</code> and
     * <code>rawMidSentenceNegatedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code>
     * values.  This is suitable to create Yes with eager error messages, and its negated and mid-sentence messages
     * need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawNegatedFactMessage raw negated message to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceNegatedFactMessage raw mid-sentence negated message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code> and <code>rawMidSentenceFactMessage</code>
     * @param negatedFactMessageArgs arguments for <code>rawNegatedFactMessage</code> and <code>rawMidSentenceNegatedFactMessage</code>
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawNegatedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceNegatedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      negatedFactMessageArgs: IndexedSeq[Any]
    ): Yes =
      new Yes(
        rawFactMessage,
        rawNegatedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceNegatedFactMessage,
        factMessageArgs,
        negatedFactMessageArgs,
        factMessageArgs,
        negatedFactMessageArgs,
        false,
        None,
        Prettifier.default
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawFactMessage</code>, <code>rawNegatedFactMessage</code>, <code>rawMidSentenceFactMessage</code> and
     * <code>rawMidSentenceNegatedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code>
     * values.  This is suitable to create Yes with eager error messages, and its negated and mid-sentence messages
     * need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawNegatedFactMessage raw negated message to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceNegatedFactMessage raw mid-sentence negated message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code>
     * @param negatedFactMessageArgs arguments for <code>rawNegatedFactMessage</code>
     * @param midSentenceFactMessageArgs arguments for <code>rawMidSentenceFactMessage</code>
     * @param midSentenceNegatedFactMessageArgs arguments for <code>rawMidSentenceNegatedFactMessage</code>
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawNegatedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceNegatedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      negatedFactMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any],
      midSentenceNegatedFactMessageArgs: IndexedSeq[Any]
    ): Yes =
      new Yes(
        rawFactMessage,
        rawNegatedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceNegatedFactMessage,
        factMessageArgs,
        negatedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceNegatedFactMessageArgs,
        false,
        None,
        Prettifier.default
      )
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>, and
     * <code>rawNegativeFailureMessage</code> fields. The <code>rawMidSentenceFactMessage</code> will return the same
     * string as <code>rawFactMessage</code>, and the <code>rawMidSentenceNegatedFailureMessage</code> will return the
     * same string as <code>rawNegatedFailureMessage</code>.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create Yes with eager error messages that have same mid-sentence messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String
    ): Yes =
      new Yes(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        None,
        Prettifier.default
      )
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>factMessageArgs</code> and <code>negatedFailureMessageArgs</code> fields.
     * The <code>rawMidSentenceFactMessage</code> will return the same string as <code>rawFactMessage</code>, and the
     * <code>rawMidSentenceNegatedFailureMessage</code> will return the same string as <code>rawNegatedFailureMessage</code>.
     * The <code>midSentenceFactMessageArgs</code> will return the same as <code>factMessageArgs</code>, and the
     * <code>midSentenceNegatedFailureMessageArgs</code> will return the same as <code>negatedFailureMessageArgs</code>.
     * This is suitable to create Yes with lazy error messages that have same mid-sentence and use different arguments for
     * negated messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param factMessageArgs arguments for constructing failure message to report if a match fails
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      factMessageArgs: IndexedSeq[Any]
    ) =
      new Yes(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        false,
        None,
        Prettifier.default
      )
  }

  case class Unary_!(underlying: Fact) extends Fact {

    // Ah, need to do the !({0}) thing
    val rawFactMessage: String = underlying.rawNegatedFactMessage
    val rawNegatedFactMessage: String = underlying.rawFactMessage
    val rawMidSentenceFactMessage: String = underlying.rawMidSentenceNegatedFactMessage
    val rawMidSentenceNegatedFactMessage: String = underlying.rawMidSentenceFactMessage
    val factMessageArgs: IndexedSeq[Any] = underlying.negatedFactMessageArgs
    val negatedFactMessageArgs: IndexedSeq[Any] = underlying.factMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceNegatedFactMessageArgs
    val midSentenceNegatedFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceFactMessageArgs
    val composite: Boolean = underlying.composite
    val prettifier: Prettifier = underlying.prettifier

    def isYes: Boolean = !(underlying.isYes)

    override def unary_!(): org.scalatest.Fact = underlying

    override def factMessage: String = super.factMessage

    override def negatedFactMessage: String = super.negatedFactMessage

    override def midSentenceFactMessage: String = super.midSentenceFactMessage

    override def midSentenceNegatedFactMessage: String = super.midSentenceNegatedFactMessage
  }

  class Binary_&&(left: Fact, right: => Fact) extends Fact {

    private lazy val rightResult = right

    val rawFactMessage: String = {
      if (left.isNo) left.rawFactMessage
      else Resources.rawCommaDoubleAmpersand
    }
    val rawNegatedFactMessage: String = {
      if (left.isNo) left.rawNegatedFactMessage
      else Resources.rawCommaDoubleAmpersand
    }
    val rawMidSentenceFactMessage: String = {
      if (left.isNo) left.rawMidSentenceFactMessage
      else Resources.rawCommaDoubleAmpersand
    }
    val rawMidSentenceNegatedFactMessage: String = {
      if (left.isNo) left.rawMidSentenceNegatedFactMessage
      else Resources.rawCommaDoubleAmpersand
    }
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isNo) Vector(FactMessage(left)) // Keep full message if short circuiting the error message
      else Vector(NegatedFactMessage(left), MidSentenceFactMessage(rightResult)) // Simplify if combining
    }
    val negatedFactMessageArgs: IndexedSeq[Any] = {
      if (left.isNo) Vector(NegatedFactMessage(left))
      else Vector(NegatedFactMessage(left), MidSentenceNegatedFactMessage(rightResult))
    }
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isNo) Vector(MidSentenceFactMessage(left)) // Keep full message if short circuiting the error message
      else Vector(MidSentenceNegatedFactMessage(left), MidSentenceFactMessage(rightResult)) // Simplify if combining
    }
    val midSentenceNegatedFactMessageArgs: IndexedSeq[Any] = {
      if (left.isNo) Vector(MidSentenceFactMessage(left))
      else Vector(MidSentenceNegatedFactMessage(left), MidSentenceNegatedFactMessage(rightResult))
    }

    val composite: Boolean = true
    val prettifier: Prettifier = left.prettifier

    def isYes: Boolean = left.isYes && rightResult.isYes
  }

  object Binary_&& {
    def apply(left: Fact, right: => Fact): Fact = new Binary_&&(left, right)
  }

  class Binary_||(left: Fact, right: => Fact) extends Fact {

    private lazy val rightResult = right

    val rawFactMessage: String = {
      if (left.isYes) left.rawFactMessage
      else Resources.rawCommaDoublePipe
    }
    val rawNegatedFactMessage: String = {
      if (left.isYes) left.rawNegatedFactMessage
      else Resources.rawCommaDoublePipe
    }
    val rawMidSentenceFactMessage: String = {
      if (left.isYes) left.rawMidSentenceFactMessage
      else Resources.rawCommaDoublePipe
    }
    val rawMidSentenceNegatedFactMessage: String = {
      if (left.isYes) left.rawMidSentenceNegatedFactMessage
      else Resources.rawCommaDoublePipe
    }
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isYes) Vector(FactMessage(left))
      else Vector(FactMessage(left), MidSentenceFactMessage(rightResult))
    }
    val negatedFactMessageArgs: IndexedSeq[Any] = {
      if (left.isYes) Vector(NegatedFactMessage(left))
      else Vector(FactMessage(left), MidSentenceNegatedFactMessage(rightResult))
    }
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isYes) Vector(MidSentenceFactMessage(left))
      else Vector(MidSentenceNegatedFactMessage(left), MidSentenceFactMessage(rightResult))
    }
    val midSentenceNegatedFactMessageArgs: IndexedSeq[Any] = {
      if (left.isYes) Vector(MidSentenceNegatedFactMessage(left))
      else Vector(MidSentenceNegatedFactMessage(left), MidSentenceNegatedFactMessage(rightResult))
    }

    val composite: Boolean = true
    val prettifier: Prettifier = left.prettifier

    def isYes: Boolean = left.isYes || right.isYes
  }

  object Binary_|| {
    def apply(left: Fact, right: => Fact): Fact = new Binary_||(left, right)
  }

  private[scalatest] def commaAnd(leftComposite: Boolean, rightComposite: Boolean): String = (leftComposite,rightComposite) match {
    case (false,false) => Resources.rawCommaAnd
    case (false,true) => Resources.rawRightParensCommaAnd
    case (true,false) => Resources.rawLeftParensCommaAnd
    case (true,true) => Resources.rawBothParensCommaAnd
  }

  private[scalatest] def commaBut(leftComposite: Boolean, rightComposite: Boolean): String = (leftComposite,rightComposite) match {
    case (false,false) => Resources.rawCommaBut
    case (false,true) => Resources.rawRightParensCommaBut
    case (true,false) => Resources.rawLeftParensCommaBut
    case (true,true) => Resources.rawBothParensCommaBut
  }

  private[scalatest] class MyLazyMessage(raw: String, args: IndexedSeq[Any]) {
    override def toString: String = Resources.formatString(raw, args.map(Prettifier.default).toArray)
  }

  // Idea is to override toString each time it is used.
  private[scalatest] sealed abstract class LazyMessage {
    val nestedArgs: IndexedSeq[Any]
  }

  private[scalatest] case class FactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.factMessageArgs
    override def toString: String = fact.factMessage
  }

  private[scalatest] case class MidSentenceFactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.midSentenceFactMessageArgs
    override def toString: String = fact.midSentenceFactMessage
  }

  private[scalatest] case class NegatedFactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.negatedFactMessageArgs
    override def toString: String = fact.negatedFactMessage
  }

  private[scalatest] case class MidSentenceNegatedFactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.midSentenceNegatedFactMessageArgs
    override def toString: String = fact.midSentenceNegatedFactMessage
  }
}
