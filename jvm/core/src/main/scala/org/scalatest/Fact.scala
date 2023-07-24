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

import org.scalactic.{UnquotedString => _, _}
import org.scalatest.exceptions._

/**
 * An abstract class representing a Fact that can be evaluated as either is yes or no.
 * @param rawSimplifiedFactMessage 
 * @param rawMidSentenceFactMessage 
 * @param rawMidSentenceSimplifiedFactMessage 
 * @param factMessageArgs 
 * @param simplifiedFactMessageArgs 
 * @param midSentenceFactMessageArgs 
 * @param midSentenceSimplifiedFactMessageArgs 
 * @param isLeaf 
 * @param isVacuousYes 
 * @param prettifier 
 * @param cause 
 * @param isYes 
 */
sealed abstract class Fact {
  // As it stands, this should not extend Product with Serializable because
  // subclasses exists that anen't case classes.

  /**
   * The raw message representing the fact.
   */
  val rawFactMessage: String
  /**
   * The raw simplified message representing the fact.
   */
  val rawSimplifiedFactMessage: String
  /**
   * The raw mid-sentence message representing the fact.
   */
  val rawMidSentenceFactMessage: String
  /**
   * The raw mid-sentence simplified message representing the fact.
   */
  val rawMidSentenceSimplifiedFactMessage: String
  /**
   * Arguments used to format the rawFactMessage.
   */
  val factMessageArgs: IndexedSeq[Any]
  /**
   * Arguments used to format the rawSimplifiedFactMessage.
   */
  val simplifiedFactMessageArgs: IndexedSeq[Any]
  /**
   * Arguments used to format the rawMidSentenceFactMessage.
   */
  val midSentenceFactMessageArgs: IndexedSeq[Any]
  /**
   * Arguments used to format the rawMidSentenceSimplifiedFactMessage.
   */
  val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any]
  /**
   * Indicates whether the fact is a leaf (terminal) node in a fact tree.
   */
  val isLeaf: Boolean
  /**
   * Indicates whether the fact is a vacuous yes, which means true in a sense but without meaningful assertions.
   */
  val isVacuousYes: Boolean
  /**
   * A prettifier used to format the messages when constructing failure messages.
   */
  val prettifier: Prettifier
  /**
   * An optional cause Throwable associated with the fact.
   */
  val cause: Option[Throwable] = None
  /**
   * Indicates whether the fact is a yes.
   */
  val isYes: Boolean

  /**
   * Indicates whether the fact is a no.
   */
  final def isNo: Boolean = !isYes

  /**
   * Convert the fact to <code>Boolean</code>, same as the value returned by <code>isYes</code>.
   */
  final def toBoolean: Boolean = isYes

  final def toAssertion(implicit pos: source.Position): Assertion = {
    if (isYes) {
      if (!isVacuousYes) Succeeded
      else throw new TestCanceledException((e: StackDepthException) => Some(factMessage), None, pos, None)
    }
    else throw new TestFailedException((e: StackDepthException) => Some(factMessage), None, pos)
  }

  // This is called internally by implicit conversions, which has different stack depth
  private[scalatest] final def internalToAssertion(pos: source.Position): Assertion = {
    if (isYes) {
      if (!isVacuousYes) Succeeded
      else throw new TestCanceledException((e: StackDepthException) => Some(factMessage), None, pos, None)
    }
    else throw new TestFailedException((e: StackDepthException) => Some(factMessage), None, pos)
  }

  /**
   * Negates this Fact, creating a version with the opposite value.
   *
   * @return A version of this Fact with the opposite value.
   */ 
  def unary_! : Fact = Fact.Unary_!(this)

  /**
   * Creates a new Fact that represents a logical OR between this Fact and the provided Fact.
   *
   * @param rhs The Fact to be combined with this Fact using a logical OR.
   * @return A new Fact representing the logical OR between this Fact and the provided Fact.
   */
  final def ||(rhs: => Fact): Fact = if (isYes) this else Fact.Binary_||(this, rhs)

  /**
   * Creates a new Fact that represents a logical AND between this Fact and the provided Fact.
   *
   * @param rhs The Fact to be combined with this Fact using a logical AND.
   * @return A new Fact representing the logical AND between this Fact and the provided Fact.
   */
  final def &&(rhs: => Fact): Fact = if (isNo) this else Fact.Binary_&&(this, rhs)

  /**
   * Creates a new Fact that represents a logical OR between this Fact and the provided Fact.
   *
   * @param rhs The Fact to be combined with this Fact using a logical OR.
   * @return A new Fact representing the logical OR between this Fact and the provided Fact.
   */
  final def |(rhs: Fact): Fact = Fact.Binary_|(this, rhs)

  /**
   * Creates a new Fact that represents a logical AND between this Fact and the provided Fact.
   *
   * @param rhs The Fact to be combined with this Fact using a logical AND.
   * @return A new Fact representing the logical AND between this Fact and the provided Fact.
   */
  final def &(rhs: Fact): Fact = Fact.Binary_&(this, rhs)

  final def stringPrefix: String =
    if (isYes) {
      if (isVacuousYes) "VacuousYes" else "Yes"
    }
    else "No"

  /**
   * Creates a new Fact that represents the implication (=>) between this Fact and the provided Fact.
   *
   * @param rhs The Fact representing the implication's consequent.
   * @return A new Fact representing the implication between this Fact and the provided Fact.
   */
  final def implies(rhs: => Fact): Fact = if (isNo) Fact.VacuousYes(this) else Fact.Implies(this, rhs)

  /**
   * Creates a new Fact that represents the equivalence (eqv) between this Fact and the provided Fact.
   *
   * @param rhs The Fact representing the other side of the equivalence.
   * @return A new Fact representing the equivalence between this Fact and the provided Fact.
   */
  final def isEqvTo(rhs: Fact): Fact = Fact.IsEqvTo(this, rhs)

  /**
   * Construct failure message to report if a fact fails, using <code>rawFactMessage</code>, <code>factMessageArgs</code> and <code>prettifier</code>
   *
   * @return failure message to report if a fact fails
   */
  def factMessage: String =
    if (factMessageArgs.isEmpty) rawFactMessage
    else makeString(rawFactMessage, factMessageArgs)

  /**
   * Construct simplified failure message to report if a fact fails, using <code>rawSimplifiedFactMessage</code>, <code>simplifiedFactMessageArgs</code>, and <code>prettifier</code>
   *
   * @return simplified failure message to report if a fact fails
   */
  def simplifiedFactMessage: String =
    if (simplifiedFactMessageArgs.isEmpty) rawSimplifiedFactMessage
    else makeString(rawSimplifiedFactMessage, simplifiedFactMessageArgs)

  /**
   * Construct failure message suitable for appearing mid-sentence, using <code>rawMidSentenceFactMessage</code>, <code>midSentenceFactMessageArgs</code> and <code>prettifier</code>
   *
   * @return failure message suitable for appearing mid-sentence
   */
  def midSentenceFactMessage: String =
    if (midSentenceFactMessageArgs.isEmpty) rawMidSentenceFactMessage
    else makeString(rawMidSentenceFactMessage, midSentenceFactMessageArgs)

  /**
   * Construct simplified failure message suitable for appearing mid-sentence, using <code>rawMidSentenceSimplifiedFactMessage</code>, <code>midSentenceSimplifiedFactMessageArgs</code> and <code>prettifier</code>
   *
   * @return simplified failure message suitable for appearing mid-sentence
   */
  def midSentenceSimplifiedFactMessage: String =
    if (midSentenceSimplifiedFactMessageArgs.isEmpty) rawMidSentenceSimplifiedFactMessage
    else makeString(rawMidSentenceSimplifiedFactMessage, midSentenceSimplifiedFactMessageArgs)

  private def makeString(raw: String, args: IndexedSeq[Any]): String =
    Resources.formatString(raw, args.map(prettifier.apply).toArray)

  private[scalatest] val NEWLINE = scala.compat.Platform.EOL

  /**
   * Generates a fact diagram, which is a textual representation of the fact and its structure.
   *
   * @param level The indentation level for formatting the diagram.
   * @return The fact diagram as a string.
   */
  def factDiagram(level: Int): String = {
    // This one makes sense for Yes and No only. The other subclassess override it.
    val msg = midSentenceFactMessage // just compute this once
    val padding = "  " * level
    if (msg.contains("\n")) {
      val padding = "  " * (level)
      padding + stringPrefix + "(" + NEWLINE + msg.split("\n").map(line => padding + "  " + line).mkString("\n") + NEWLINE + ")"
    }
    else
      padding + stringPrefix + "(" + msg + ")"
  }

  /**
   * Converts this Fact to its string representation.
   *
   * @return The string representation of this Fact.
   */
  override def toString: String = factDiagram(0)
}

object Fact {

  case class Leaf(
    rawFactMessage: String,
    rawSimplifiedFactMessage: String,
    rawMidSentenceFactMessage: String,
    rawMidSentenceSimplifiedFactMessage: String,
    factMessageArgs: IndexedSeq[Any],
    simplifiedFactMessageArgs: IndexedSeq[Any],
    midSentenceFactMessageArgs: IndexedSeq[Any],
    midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any],
    isYes: Boolean,
    isVacuousYes: Boolean,
    prettifier: Prettifier,
    override val cause: Option[Throwable] = None
  ) extends Fact {
    require(!isVacuousYes || isYes)
    val isLeaf: Boolean = true
  }

  class VacuousYes(underlying: Fact) extends Fact {

    require(underlying.isNo)
    
    val rawFactMessage: String = underlying.rawFactMessage
    val rawSimplifiedFactMessage: String = underlying.rawSimplifiedFactMessage
    val rawMidSentenceFactMessage: String = underlying.rawMidSentenceFactMessage
    val rawMidSentenceSimplifiedFactMessage: String = underlying.rawMidSentenceSimplifiedFactMessage

    val factMessageArgs: IndexedSeq[Any] = underlying.factMessageArgs
    val simplifiedFactMessageArgs: IndexedSeq[Any] = underlying.simplifiedFactMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceFactMessageArgs
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceSimplifiedFactMessageArgs

    val isLeaf: Boolean = underlying.isLeaf
    val prettifier: Prettifier = underlying.prettifier

    override val cause: Option[Throwable] = underlying.cause

    val isYes: Boolean = true
    val isVacuousYes: Boolean = true
  }

  object VacuousYes {
    def apply(underlying: Fact): VacuousYes = new VacuousYes(underlying)
  }

  /**
   * Companion object for the <code>No</code> case class.
   *
   * @author Bill Venners
   */
  object No {

    // TODO: Does the Prettifier really need to be curried and implicit? It seems to be only used
    // explicitly by us. Possibly this is desired, though, so people can just say No(...). But then
    // they would need to fill in all the fields, so that seems hard anyway. When the time comes to
    // make this public, look into this question.
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any],
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any],
      cause: Option[Throwable] = None
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        false,
        false,
        prettifier,
        cause
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>factMessage</code>, 
     * <code>negativeFailureMessage</code>, <code>midSentenceFactMessage</code>, 
     * <code>midSentenceSimplifiedFailureMessage</code>, <code>factMessageArgs</code>, and <code>simplifiedFailureMessageArgs</code> fields.
     * <code>factMessageArgs</code>, and <code>simplifiedFailureMessageArgs</code> will be used in place of <code>midSentenceFactMessageArgs</code>
     * and <code>midSentenceSimplifiedFailureMessageArgs</code>.
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
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
	false,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>factMessage</code>,
     * <code>negativeFactMessage</code>, <code>midSentenceFactMessage</code>,
     * <code>midSentenceSimplifiedFactMessage</code>, <code>factMessageArgs</code>, and <code>simplifiedFactMessageArgs</code> fields.
     * <code>factMessageArgs</code>, and <code>simplifiedFactMessageArgs</code> will be used in place of <code>midSentenceFactMessageArgs</code>
     * and <code>midSentenceSimplifiedFactMessageArgs</code>.
     *
     * @param rawFactMessage raw fact message to report if a match fails
     * @param rawMidSentenceFactMessage raw mid sentence fact message to report if a match fails
     * @param factMessageArgs arguments for constructing fact message to report if a match fails
     * @param midSentenceFactMessageArgs arguments for constructing mid sentence fact message to report if a match fails
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any]
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceFactMessageArgs,
        false,
        false,
        prettifier,
        None
      )
  
    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceSimplifiedFailureMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawMidSentenceFactMessage raw failure message to report if a match fails
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its simplified and mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its simplified and mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code> and <code>rawMidSentenceFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code> and <code>rawMidSentenceSimplifiedFactMessage</code>
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any]
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        factMessageArgs,
        simplifiedFactMessageArgs,
        false,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its simplified and mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code>
     * @param midSentenceFactMessageArgs arguments for <code>rawMidSentenceFactMessage</code>
     * @param midSentenceSimplifiedFactMessageArgs arguments for <code>rawMidSentenceSimplifiedFactMessage</code>
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any],
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any]
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        false,
        false,
        prettifier,
        None
      )
  
    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>, and
     * <code>rawNegativeFailureMessage</code> fields. The <code>rawMidSentenceFactMessage</code> will return the same
     * string as <code>rawFactMessage</code>, and the <code>rawMidSentenceSimplifiedFailureMessage</code> will return the
     * same string as <code>rawSimplifiedFailureMessage</code>.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages that have same mid-sentence messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawSimplifiedFailureMessage raw message with a meaning opposite to that of the failure message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        false,
        prettifier,
        None
      )
  
    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>factMessageArgs</code> and <code>simplifiedFailureMessageArgs</code> fields.
     * The <code>rawMidSentenceFactMessage</code> will return the same string as <code>rawFactMessage</code>, and the
     * <code>rawMidSentenceSimplifiedFailureMessage</code> will return the same string as <code>rawSimplifiedFailureMessage</code>.
     * The <code>midSentenceFactMessageArgs</code> will return the same as <code>factMessageArgs</code>, and the
     * <code>midSentenceSimplifiedFailureMessageArgs</code> will return the same as <code>simplifiedFailureMessageArgs</code>.
     * This is suitable to create No with lazy error messages that have same mid-sentence and use different arguments for
     * simplified messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawSimplifiedFailureMessage raw message with a meaning opposite to that of the failure message
     * @param factMessageArgs arguments for constructing failure message to report if a match fails
     * @param simplifiedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      factMessageArgs: IndexedSeq[Any]
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        false,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>, and
     * <code>cause</code> fields. The <code>rawMidSentenceFactMessage</code>, <code>rawSimplifiedFailureMessage</code> and
     * <code>rawMidSentenceSimplifiedFailureMessage</code>will return the same string as <code>rawFactMessage</code>.
     * All argument fields will have <code>Vector.empty</code> values.  This is suitable to create No with eager error messages
     * that have same mid-sentence messages.
     *
     * @param rawFactMessage raw fact message
     * @param cause the causing throwable of this No instance
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      cause: Throwable
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        false,
        prettifier,
        Some(cause)
      )
  }
  
  /**
   * Companion object for the <code>Yes</code> case class.
   *
   * @author Bill Venners
   */
  object Yes {
  
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any],
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any],
      isVacuousYes: Boolean = false,
      cause: Option[Throwable] = None
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        true,
        isVacuousYes,
        prettifier,
        cause
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed code>factMessage</code>, 
     * <code>negativeFailureMessage</code>, <code>midSentenceFactMessage</code>, 
     * <code>midSentenceSimplifiedFailureMessage</code>, <code>factMessageArgs</code>, and <code>simplifiedFailureMessageArgs</code> fields.
     * <code>factMessageArgs</code>, and <code>simplifiedFailureMessageArgs</code> will be used in place of <code>midSentenceFactMessageArgs</code>
     * and <code>midSentenceSimplifiedFailureMessageArgs</code>.
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
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        true,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed code>factMessage</code>,
     * <code>negativeFailureMessage</code>, <code>midSentenceFactMessage</code>,
     * <code>midSentenceSimplifiedFactMessage</code>, <code>factMessageArgs</code>, and <code>simplifiedFactMessageArgs</code> fields.
     * <code>factMessageArgs</code>, and <code>simplifiedFactMessageArgs</code> will be used in place of <code>midSentenceFactMessageArgs</code>
     * and <code>midSentenceSimplifiedFactMessageArgs</code>.
     *
     * @param rawFactMessage raw fact message to report if a match fails
     * @param rawMidSentenceFactMessage raw mid-sentence fact message to report if a match fails
     * @param factMessageArgs arguments for constructing fact message to report if a match fails
     * @param midSentenceFactMessageArgs arguments for constructing mid-sentence fact message to report if a match fails
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any]
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceFactMessageArgs,
        true,
        false,
        prettifier,
        None
      )
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceSimplifiedFailureMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create Yes with eager error messages, and its mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawMidSentenceFactMessage raw failure message to report if a match fails
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        true,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawFactMessage</code>, <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code> and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code>
     * values.  This is suitable to create Yes with eager error messages, and its simplified and mid-sentence messages
     * need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified message to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        true,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawFactMessage</code>, <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code> and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code>
     * values.  This is suitable to create Yes with eager error messages, and its simplified and mid-sentence messages
     * need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified message to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code> and <code>rawMidSentenceFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code> and <code>rawMidSentenceSimplifiedFactMessage</code>
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any]
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        factMessageArgs,
        simplifiedFactMessageArgs,
        true,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawFactMessage</code>, <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code> and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code>
     * values.  This is suitable to create Yes with eager error messages, and its simplified and mid-sentence messages
     * need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified message to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code>
     * @param midSentenceFactMessageArgs arguments for <code>rawMidSentenceFactMessage</code>
     * @param midSentenceSimplifiedFactMessageArgs arguments for <code>rawMidSentenceSimplifiedFactMessage</code>
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any],
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any]
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        true,
        false,
        prettifier,
        None
      )
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>, and
     * <code>rawNegativeFailureMessage</code> fields. The <code>rawMidSentenceFactMessage</code> will return the same
     * string as <code>rawFactMessage</code>, and the <code>rawMidSentenceSimplifiedFailureMessage</code> will return the
     * same string as <code>rawSimplifiedFailureMessage</code>.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create Yes with eager error messages that have same mid-sentence messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        true,
        false,
        prettifier,
        None
      )
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>factMessageArgs</code> and <code>simplifiedFailureMessageArgs</code> fields.
     * The <code>rawMidSentenceFactMessage</code> will return the same string as <code>rawFactMessage</code>, and the
     * <code>rawMidSentenceSimplifiedFailureMessage</code> will return the same string as <code>rawSimplifiedFailureMessage</code>.
     * The <code>midSentenceFactMessageArgs</code> will return the same as <code>factMessageArgs</code>, and the
     * <code>midSentenceSimplifiedFailureMessageArgs</code> will return the same as <code>simplifiedFailureMessageArgs</code>.
     * This is suitable to create Yes with lazy error messages that have same mid-sentence and use different arguments for
     * simplified messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param factMessageArgs arguments for constructing failure message to report if a match fails
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      factMessageArgs: IndexedSeq[Any]
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        true,
        false,
        prettifier,
        None
      )
  }

  // The simplified Fact message is used when a Fact is negated, because
  // sometimes factMessage can include info about what was expected, as in
  // "Expected 3, but got 4", for expectResult(3) { x } . But if this is inverted
  // to !expectResult(3) { x }, then it is confusing to say Expected 3 (because
  // now anything *but* 3 is expected. So the simplified message just says, "3 did not equal 4".
  // Of course, that means x was 4, and so the inverted form would be a Yes. But if x were 3, then
  // the regular factMessage would be "Expected 3, but got 3" and the simplified fact message would be "3 equaled 3"
  // TODO: Write a test that ensures !(!(<vacuous yes>)).isVacuousYes stays true
  case class Unary_!(underlying: Fact) extends Fact {

    val rawFactMessage: String = underlying.rawSimplifiedFactMessage
    val rawSimplifiedFactMessage: String = underlying.rawSimplifiedFactMessage
    val rawMidSentenceFactMessage: String = underlying.rawMidSentenceSimplifiedFactMessage
    val rawMidSentenceSimplifiedFactMessage: String = underlying.rawMidSentenceSimplifiedFactMessage
    val factMessageArgs: IndexedSeq[Any] = underlying.simplifiedFactMessageArgs
    val simplifiedFactMessageArgs: IndexedSeq[Any] = underlying.simplifiedFactMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceSimplifiedFactMessageArgs
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceSimplifiedFactMessageArgs
    val isLeaf: Boolean = underlying.isLeaf
    val prettifier: Prettifier = underlying.prettifier

    val isYes: Boolean = !(underlying.isYes)
    val isVacuousYes: Boolean = false

    override def unary_! : org.scalatest.Fact = underlying

    override def factDiagram(level: Int): String = {
      val padding = "  " * level
      val msg = underlying.factDiagram(0)
      padding + (if (isYes) "Yes(" else "No(") + NEWLINE + {
        if (msg.contains("\n"))
          ("!" + msg).split("\n").map(line => padding + "  " + line).mkString("\n") + NEWLINE
        else
          padding + "  !" + msg + NEWLINE
      } +
      padding + ")"
    }
  }

  class Binary_&(left: Fact, right: Fact) extends Fact {

    private[scalatest] def operatorName: String = "&"

    val rawFactMessage: String = {
      if (left.isLeaf && right.isLeaf) {
        if (left.isYes && right.isNo)
          Resources.rawCommaBut
        else
          Resources.rawCommaAnd
      }
      else factDiagram(0)
    }
    val rawSimplifiedFactMessage: String = rawFactMessage
    val rawMidSentenceFactMessage: String = rawFactMessage
    val rawMidSentenceSimplifiedFactMessage: String = rawFactMessage
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(
          SimplifiedFactMessage(left),
          MidSentenceSimplifiedFactMessage(right)
        ) // Simplify if combining
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(
          MidSentenceSimplifiedFactMessage(left),
          MidSentenceSimplifiedFactMessage(right)
        ) // Simplify if combining
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }

    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = midSentenceFactMessageArgs

    val isLeaf: Boolean = false
    val isYes: Boolean = left.isYes && right.isYes
    val isVacuousYes: Boolean = isYes && (left.isVacuousYes || right.isVacuousYes)
    val prettifier: Prettifier = left.prettifier


    override def factDiagram(level: Int): String = {
      val padding = "  " * level
      padding + stringPrefix + "(" + NEWLINE +
        left.factDiagram(level + 1) + " " + operatorName + NEWLINE +
        right.factDiagram(level + 1) + NEWLINE +
        padding + ")"
    }
  }

  object Binary_& {
    def apply(left: Fact, right: Fact): Fact = new Binary_&(left, right)
  }

  class Binary_&&(left: Fact, right: Fact) extends Binary_&(left, right) {
    require(left.isYes)
    override private[scalatest] def operatorName: String = "&&"
  }

  object Binary_&& {
    def apply(left: Fact, right: Fact): Fact = new Binary_&&(left, right)
  }

  class Binary_|(left: Fact, right: Fact) extends Fact {

    private[scalatest] def operatorName: String = "|"

    val rawFactMessage: String = {
      if (left.isLeaf && right.isLeaf) {
        Resources.rawCommaAnd
      }
      else factDiagram(0)
    }
    val rawSimplifiedFactMessage: String = rawFactMessage
    val rawMidSentenceFactMessage: String = rawFactMessage
    val rawMidSentenceSimplifiedFactMessage: String = rawFactMessage
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(SimplifiedFactMessage(left), MidSentenceSimplifiedFactMessage(right))
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(MidSentenceSimplifiedFactMessage(left), MidSentenceSimplifiedFactMessage(right))
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = midSentenceFactMessageArgs

    val isLeaf: Boolean = false
    val isYes: Boolean = left.isYes || right.isYes
    val isVacuousYes: Boolean = (left.isVacuousYes && (right.isVacuousYes || right.isNo)) || ((left.isVacuousYes || left.isNo) && right.isVacuousYes)
    val prettifier: Prettifier = left.prettifier

    override def factDiagram(level: Int): String = {
      val padding = "  " * level
      padding + stringPrefix + "(" + NEWLINE +
      left.factDiagram(level + 1) + " " + operatorName + NEWLINE +
      right.factDiagram(level + 1) + NEWLINE +
      padding + ")"
    }
  }

  object Binary_| {
    def apply(left: Fact, right: Fact): Fact = new Binary_|(left, right)
  }

  class Binary_||(left: Fact, right: Fact) extends Binary_|(left, right) {
    require(left.isNo)
    override private[scalatest] def operatorName: String = "||"
  }

  object Binary_|| {
    def apply(left: Fact, right: Fact): Fact = new Binary_||(left, right)
  }

/*
  Yes implies No // x, but y
  Yes implies Yes // x, and y
*/
  class Implies(left: Fact, right: Fact) extends Fact {

    require(left.isYes)

    val rawFactMessage: String = {
      if (left.isLeaf && right.isLeaf) {
        if (left.isYes && right.isNo)
          Resources.rawCommaBut
        else
          Resources.rawCommaAnd
      }
      else factDiagram(0)
    }
    val rawSimplifiedFactMessage: String = rawFactMessage
    val rawMidSentenceFactMessage: String = rawFactMessage
    val rawMidSentenceSimplifiedFactMessage: String = rawFactMessage
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(
          SimplifiedFactMessage(left),
          MidSentenceSimplifiedFactMessage(right)
        ) // Simplify if combining
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(
          MidSentenceSimplifiedFactMessage(left),
          MidSentenceSimplifiedFactMessage(right)
        ) // Simplify if combining
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }

    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = midSentenceFactMessageArgs

    val isLeaf: Boolean = false
    val isVacuousYes: Boolean = false // TODO
    val prettifier: Prettifier = left.prettifier

    val isYes: Boolean = left.isYes && right.isYes

    override def factDiagram(level: Int): String = {
      val padding = "  " * level
      padding + stringPrefix + "(" + NEWLINE +
        left.factDiagram(level + 1) + " implies" + NEWLINE +
        right.factDiagram(level + 1) + NEWLINE +
        padding + ")"
    }
  }

  object Implies {
    def apply(left: Fact, right: Fact): Fact = new Implies(left, right)
  }

  class IsEqvTo(left: Fact, right: Fact) extends Fact {

    val rawFactMessage: String = {
      if (left.isLeaf && right.isLeaf) {
        Resources.rawCommaAnd
      }
      else factDiagram(0)
    }
    val rawSimplifiedFactMessage: String = rawFactMessage
    val rawMidSentenceFactMessage: String = rawFactMessage
    val rawMidSentenceSimplifiedFactMessage: String = rawFactMessage
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(SimplifiedFactMessage(left), MidSentenceSimplifiedFactMessage(right))
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(MidSentenceSimplifiedFactMessage(left), MidSentenceSimplifiedFactMessage(right))
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = midSentenceFactMessageArgs

    val isLeaf: Boolean = false
    val isYes: Boolean = (left.isYes && right.isYes) || (left.isNo && right.isNo)
    val isVacuousYes: Boolean = isYes && (left.isVacuousYes || right.isVacuousYes)
    val prettifier: Prettifier = left.prettifier

    override def factDiagram(level: Int): String = {
      val padding = "  " * level
      padding + stringPrefix + "(" + NEWLINE +
      left.factDiagram(level + 1) + " isEqvTo" + NEWLINE +
      right.factDiagram(level + 1) + NEWLINE +
      padding + ")"
    }
  }

  object IsEqvTo {
    def apply(left: Fact, right: Fact): Fact = new IsEqvTo(left, right)
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

  private[scalatest] case class SimplifiedFactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.simplifiedFactMessageArgs
    override def toString: String = fact.simplifiedFactMessage
  }

  private[scalatest] case class MidSentenceSimplifiedFactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.midSentenceSimplifiedFactMessageArgs
    override def toString: String = fact.midSentenceSimplifiedFactMessage
  }
}
