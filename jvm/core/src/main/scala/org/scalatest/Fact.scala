/*
 * Copyright 2001-2024 Artima, Inc.
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
 * 
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
   * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
   */
  def modifyMessage(fun: Option[String] => Option[String]): Fact
  /**
   * Indicates whether the fact is a no.
   */
  final def isNo: Boolean = !isYes

  /**
   * Convert the fact to <code>Boolean</code>, same as the value returned by <code>isYes</code>.
   */
  final def toBoolean: Boolean = isYes

  /**
   * Convert this fact to <code>Assertion</code>.
   *
   * @param pos the related <code>Position</code> for this fact.
   * @return <code>Succeeded</code> if this fact is a yes.
   * @throws TestFailedException if this fact is not yes or vacuous yes.
   * @throws TestCanceledException if this fact is a vacuous yes.
   */
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

  /**
   * String prefix for this fact, return either "Yes", "VacuousYes" or "No".
   */
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

  private[scalatest] val NEWLINE = System.lineSeparator

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

/**
 * Companion object for the <code>Fact</code> trait.
 */
object Fact {

  /**
   * Represents a leaf node in the Fact tree, which is a concrete Fact with known boolean values.
   *
   * @param rawFactMessage The raw message representing the fact.
   * @param rawSimplifiedFactMessage The raw simplified message representing the fact.
   * @param rawMidSentenceFactMessage The raw mid-sentence message representing the fact.
   * @param rawMidSentenceSimplifiedFactMessage The raw mid-sentence simplified message representing the fact.
   * @param factMessageArgs Arguments used to format the rawFactMessage.
   * @param simplifiedFactMessageArgs Arguments used to format the rawSimplifiedFactMessage.
   * @param midSentenceFactMessageArgs Arguments used to format the rawMidSentenceFactMessage.
   * @param midSentenceSimplifiedFactMessageArgs Arguments used to format the rawMidSentenceSimplifiedFactMessage.
   * @param isYes Indicates whether the fact is evaluated to true.
   * @param isVacuousYes Indicates whether the fact is a vacuous yes, which means true in a sense but without meaningful assertions.
   * @param prettifier A prettifier used to format the messages when constructing failure messages.
   * @param cause An optional cause Throwable associated with the fact.
   * @throws IllegalArgumentException if `isVacuousYes` is true but `isYes` is false.
   */
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

    /**
     * Indicates whether this Fact is a leaf node in the Fact tree, return <code>true</code>.
     */
    val isLeaf: Boolean = true
    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = 
      Leaf(
        fun(Some(rawFactMessage)).getOrElse(rawFactMessage),
        fun(Some(rawSimplifiedFactMessage)).getOrElse(rawSimplifiedFactMessage),
        fun(Some(rawMidSentenceFactMessage)).getOrElse(rawMidSentenceFactMessage),
        fun(Some(rawMidSentenceSimplifiedFactMessage)).getOrElse(rawMidSentenceSimplifiedFactMessage),
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        isYes,
        isVacuousYes,
        prettifier,
        cause
      )
  }

  /**
   * Represents a vacuous "Yes" Fact, which is a special case of Fact where the underlying Fact is No,
   * but it is treated as Yes without meaningful assertions.
   *
   * @param underlying The underlying Fact that is treated as No but represented as Yes.
   * @throws IllegalArgumentException if the underlying Fact's <code>isNo</code> is <code>false</code>.
   */
  class VacuousYes(underlying: Fact) extends Fact {

    require(underlying.isNo)
    
    /**
     * The raw message representing the vacuous "Yes" Fact.
     */
    val rawFactMessage: String = underlying.rawFactMessage
    /**
     * The raw simplified message representing the vacuous "Yes" Fact.
     */
    val rawSimplifiedFactMessage: String = underlying.rawSimplifiedFactMessage
    /**
     * The raw mid-sentence message representing the vacuous "Yes" Fact.
     */
    val rawMidSentenceFactMessage: String = underlying.rawMidSentenceFactMessage
    /**
     * The raw mid-sentence simplified message representing the vacuous "Yes" Fact.
     */
    val rawMidSentenceSimplifiedFactMessage: String = underlying.rawMidSentenceSimplifiedFactMessage

    /**
     * Arguments used to format the rawFactMessage.
     */
    val factMessageArgs: IndexedSeq[Any] = underlying.factMessageArgs
    /**
     * Arguments used to format the rawSimplifiedFactMessage.
     */
    val simplifiedFactMessageArgs: IndexedSeq[Any] = underlying.simplifiedFactMessageArgs
    /**
     * Arguments used to format the rawMidSentenceFactMessage.
     */
    val midSentenceFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceFactMessageArgs
    /**
     * Arguments used to format the rawMidSentenceSimplifiedFactMessage.
     */
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceSimplifiedFactMessageArgs

    /**
     * Indicates whether this Fact is a leaf node in the Fact tree.
     */
    val isLeaf: Boolean = underlying.isLeaf
    /**
     * The prettifier used to format the messages when constructing failure messages.
     */
    val prettifier: Prettifier = underlying.prettifier

    /**
     * An optional cause Throwable associated with the vacuous "Yes" Fact.
     */
    override val cause: Option[Throwable] = underlying.cause

    /**
     * Indicates whether this Fact is evaluated to Yes, return <code>true</code>.
     */
    val isYes: Boolean = true
    /**
     * Indicates whether this Fact is a vacuous "Yes", which means true in a sense but without meaningful assertions, return <code>true</code>
     */
    val isVacuousYes: Boolean = true

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = new VacuousYes(underlying.modifyMessage(fun))
  }

  /**
   * A companion object for the <code>VacuousYes</code> class, providing factory methods to create instances of <code>VacuousYes</code> fact.
   */
  object VacuousYes {
    /**
     * Creates a new <code>VacuousYes</code> fact with the provided underlying <code>Fact</code>.
     *
     * @param underlying The underlying <code>Fact</code> that is treated as false (No) but represented as true (Yes).
     * @return A new <code>VacuousYes</code> fact instance.
     * @throws IllegalArgumentException if the underlying Fact is not No.
     */
    def apply(underlying: Fact): VacuousYes = new VacuousYes(underlying)
  }

  /**
   * Companion object for the <code>No</code> case class.
   *
   * @author Bill Venners
   */
  object No {

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, 
     * <code>rawMidSentenceSimplifiedFactMessage</code>, <code>factMessageArgs</code>, <code>simplifiedFactMessageArgs</code>, 
     * <code>midSentenceFactMessageArgs</code>, <code>midSentenceSimplifiedFactMessageArgs</code>, and <code>cause</code> fields.  
     * This is suitable to create <code>No</code> with eager error messages.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code>
     * @param midSentenceFactMessageArgs arguments for <code>rawMidSentenceFactMessage</code>
     * @param midSentenceSimplifiedFactMessageArgs arguments for <code>rawMidSentenceSimplifiedFactMessage</code>
     * @param cause the causing throwable of this No instance
     * @param prettifier the prettifier used to prettify message
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
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any],
      cause: Option[Throwable], 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message 
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
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
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      factMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      cause: Throwable, 
      prettifier: Prettifier
    ): Leaf =
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
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, 
     * <code>rawMidSentenceSimplifiedFactMessage</code>, <code>factMessageArgs</code>, <code>simplifiedFactMessageArgs</code>, 
     * <code>midSentenceFactMessageArgs</code>, <code>midSentenceSimplifiedFactMessageArgs</code>, and <code>cause</code> fields.  
     * This is suitable to create <code>Yes</code> with eager error messages.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code>
     * @param midSentenceFactMessageArgs arguments for <code>rawMidSentenceFactMessage</code>
     * @param midSentenceSimplifiedFactMessageArgs arguments for <code>rawMidSentenceSimplifiedFactMessage</code>
     * @param cause the causing throwable of this No instance
     * @param prettifier the prettifier used to prettify message
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
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any],
      isVacuousYes: Boolean = false,
      cause: Option[Throwable], 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
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
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
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
     * @param cause optional cause <code>Throwable</code> associated with the fact.
     * @param prettifier the prettifier used to prettify message
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
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any], 
      cause: Option[Throwable], 
      prettifier: Prettifier
    ): Leaf =
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
        cause
      )  
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>, and
     * <code>rawNegativeFailureMessage</code> fields. The <code>rawMidSentenceFactMessage</code> will return the same
     * string as <code>rawFactMessage</code>, and the <code>rawMidSentenceSimplifiedFailureMessage</code> will return the
     * same string as <code>rawSimplifiedFailureMessage</code>.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create Yes with eager error messages that have same mid-sentence messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
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
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      factMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
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

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = Unary_!(underlying.modifyMessage(fun))
  }

  /**
   * Represents a binary logical AND operation between two `Fact` instances.
   *
   * @param left  The left-hand side `Fact` instance of the AND operation.
   * @param right The right-hand side `Fact` instance of the AND operation.
   * @param messageFun An optional message function to modify messages.
   */
  class Binary_&(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Fact {

    private[scalatest] def operatorName: String = "&"

    /**
     * The raw message representing the logical AND operation result between the `left` and `right` `Fact` instances.
     *
     * @note If both `left` and `right` are leaf nodes, the result will be a comma-separated message.
     *       Otherwise, a recursive representation of the combined facts will be used.
     */
    val rawFactMessage: String = {
      val msg = 
        if (left.isLeaf && right.isLeaf) {
          if (left.isYes && right.isNo)
            Resources.rawCommaBut
          else
            Resources.rawCommaAnd
        }
        else factDiagram(0)
      messageFun.flatMap(msgFun => msgFun(Some(msg))).getOrElse(msg)  
    }
    /**
     * The simplified version of the raw fact message, which is the same as `rawFactMessage`.
     */
    val rawSimplifiedFactMessage: String = rawFactMessage
    /**
     * The raw message used in the middle of a sentence, which is the same as `rawFactMessage`.
     */
    val rawMidSentenceFactMessage: String = rawFactMessage
    /**
     * The simplified version of the raw message used in the middle of a sentence, which is the same as `rawFactMessage`.
     */
    val rawMidSentenceSimplifiedFactMessage: String = rawFactMessage
    /**
     * The arguments used in the fact message. If both `left` and `right` are leaf nodes, the result will be a `Vector`
     * containing simplified fact messages for both sides. Otherwise, a `Vector` of unquoted strings representing the
     * fact diagrams of the `left` and `right` instances will be used.
     */
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
    /**
     * The simplified version of the fact message arguments, which is the same as `factMessageArgs`.
     */
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs
    /**
     * The arguments used in the middle of a sentence fact message. If both `left` and `right` are leaf nodes, the result
     * will be a `Vector` containing middle of a sentence simplified fact messages for both sides. Otherwise, a `Vector`
     * of unquoted strings representing the fact diagrams of the `left` and `right` instances will be used.
     */
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
    /**
     * The simplified version of the middle of a sentence fact message arguments, which is the same as `midSentenceFactMessageArgs`.
     */
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = midSentenceFactMessageArgs
    /**
     * Returns whether this `Binary_&` instance is a leaf node or not. Since it represents an AND operation, it is always `false`.
     */
    val isLeaf: Boolean = false
    /**
     * Returns `true` if both the `left` and `right` `Fact` instances are `yes`, `false` otherwise.
     */
    val isYes: Boolean = left.isYes && right.isYes
    /**
     * Returns `true` if both the `left` and `right` `Fact` instances are `yes` and at least one of them is a vacuous `yes`,
     * `false` otherwise.
     */
    val isVacuousYes: Boolean = isYes && (left.isVacuousYes || right.isVacuousYes)
    /**
     * The prettifier used in this `Binary_&` instance, which is taken from the `left` `Fact` instance.
     */
    val prettifier: Prettifier = left.prettifier

    /**
     * Generates the fact diagram for this `Binary_&` instance.
     *
     * @param level The indentation level used for pretty printing the fact diagram.
     * @return The fact diagram as a string representation.
     */
    override def factDiagram(level: Int): String = {
      val padding = "  " * level
      padding + stringPrefix + "(" + NEWLINE +
        left.factDiagram(level + 1) + " " + operatorName + NEWLINE +
        right.factDiagram(level + 1) + NEWLINE +
        padding + ")"
    }

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = new Binary_&(left, right, Some(fun))
  }

  /**
   * Represents a binary logical AND operation between two `Fact` instances.
   * This is a factory object used to create `Binary_&` instances.
   */
  object Binary_& {
    /**
     * Creates a new `Binary_&` instance with the specified left and right `Fact` instances.
     *
     * @param left  The left-hand side `Fact` instance of the AND operation.
     * @param right The right-hand side `Fact` instance of the AND operation.
     * @return A new `Binary_&` instance representing the logical AND operation of the two `Fact` instances.
     */
    def apply(left: Fact, right: Fact): Fact = new Binary_&(left, right, None)
    /**
     * Creates a new `Binary_&` instance with the specified left and right `Fact` instances, and an optional clue.
     *
     * @param left  The left-hand side `Fact` instance of the AND operation.
     * @param right The right-hand side `Fact` instance of the AND operation.
     * @param messageFun An optional messaage function to modify the messages.
     * @return A new `Binary_&` instance representing the logical AND operation of the two `Fact` instances.
     */
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Binary_&(left, right, messageFun)
  }

  /**
   * Represents a binary logical AND operation between two `Fact` instances, enforcing that the left-hand side `Fact` instance is `yes`.
   *
   * @param left  The left-hand side `Fact` instance of the AND operation. It must be a `yes` fact.
   * @param right The right-hand side `Fact` instance of the AND operation.
   * @param messageFun An optional message function to modify the messages.
   * @throws IllegalArgumentException If the `left` `Fact` instance is not a `yes` fact.
   */
  class Binary_&&(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Binary_&(left, right, messageFun) {
    require(left.isYes)
    override private[scalatest] def operatorName: String = "&&"
    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    override def modifyMessage(fun: Option[String] => Option[String]): Fact = new Binary_&&(left, right, Some(fun))
  }

  /**
   * Represents a binary logical AND operation between two `Fact` instances, enforcing that the left-hand side `Fact` instance is `yes`.
   * This is a factory object used to create `Binary_&&` instances.
   */
  object Binary_&& {
    /**
     * Creates a new `Binary_&&` instance with the specified left and right `Fact` instances, enforcing that the left-hand side `Fact` instance is `yes`.
     *
     * @param left  The left-hand side `Fact` instance of the AND operation. It must be a `yes` fact.
     * @param right The right-hand side `Fact` instance of the AND operation.
     * @return A new `Binary_&&` instance representing the logical AND operation of the two `Fact` instances.
     * @throws IllegalArgumentException If the `left` `Fact` instance is not a `yes` fact.
     */
    def apply(left: Fact, right: Fact): Fact = new Binary_&&(left, right, None)
    /**
     * Creates a new `Binary_&&` instance with the specified left and right `Fact` instances and an optional clue, enforcing that the left-hand side `Fact` instance is `yes`.
     *
     * @param left  The left-hand side `Fact` instance of the AND operation. It must be a `yes` fact.
     * @param right The right-hand side `Fact` instance of the AND operation.
     * @param messageFun An optional message function to modify the messages.
     * @return A new `Binary_&&` instance representing the logical AND operation of the two `Fact` instances.
     * @throws IllegalArgumentException If the `left` `Fact` instance is not a `yes` fact.
     */
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Binary_&&(left, right, messageFun)
  }

  class Binary_|(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Fact {

    private[scalatest] def operatorName: String = "|"

    val rawFactMessage: String = {
      val msg = 
        if (left.isLeaf && right.isLeaf) {
          Resources.rawCommaAnd
        }
        else factDiagram(0)
      messageFun.flatMap(fun => fun(Some(msg))).getOrElse(msg)
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

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = new Binary_|(left, right, Some(fun))
  }

  object Binary_| {
    def apply(left: Fact, right: Fact): Fact = new Binary_|(left, right, None)
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Binary_|(left, right, messageFun)
  }

  class Binary_||(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Binary_|(left, right, messageFun) {
    require(left.isNo)
    override private[scalatest] def operatorName: String = "||"
    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    override def modifyMessage(fun: Option[String] => Option[String]): Fact = new Binary_||(left, right, Some(fun))
  }

  object Binary_|| {
    def apply(left: Fact, right: Fact): Fact = new Binary_||(left, right, None)
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Binary_||(left, right, messageFun)
  }

/*
  Yes implies No // x, but y
  Yes implies Yes // x, and y
*/
  class Implies(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Fact {

    require(left.isYes)

    val rawFactMessage: String = {
      val msg = 
        if (left.isLeaf && right.isLeaf) {
          if (left.isYes && right.isNo)
            Resources.rawCommaBut
          else
            Resources.rawCommaAnd
        }
        else factDiagram(0)
      messageFun.flatMap(fun => fun(Some(msg))).getOrElse(msg)  
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

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = new Implies(left, right, Some(fun))
  }

  object Implies {
    def apply(left: Fact, right: Fact): Fact = new Implies(left, right, None)
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Implies(left, right, messageFun)
  }

  class IsEqvTo(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Fact {

    val rawFactMessage: String = {
      val msg = 
        if (left.isLeaf && right.isLeaf) {
          Resources.rawCommaAnd
        }
        else factDiagram(0)
      messageFun.flatMap(fun => fun(Some(msg))).getOrElse(msg)  
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

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = new IsEqvTo(left, right, Some(fun))
  }

  object IsEqvTo {
    def apply(left: Fact, right: Fact): Fact = new IsEqvTo(left, right, None)
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new IsEqvTo(left, right, messageFun)
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
