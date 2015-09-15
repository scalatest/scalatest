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
  val rawSimplifiedFactMessage: String
  val rawMidSentenceFactMessage: String
  val rawMidSentenceSimplifiedFactMessage: String

  val factMessageArgs: IndexedSeq[Any]
  val simplifiedFactMessageArgs: IndexedSeq[Any]
  val midSentenceFactMessageArgs: IndexedSeq[Any]
  val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any]

  val isLeaf: Boolean
  val prettifier: Prettifier

  val cause: Option[Throwable] = None

  def isYes: Boolean

  final def isNo: Boolean = !isYes

  final def toBoolean: Boolean = isYes

  final def toAssertion: Assertion =
    if (isYes) Succeeded
    else throw new TestFailedException(factMessage, 2)

  /**
   * Get a simplified version of this Fact, sub type will be simplified and all messages field will be substituted with its counter-part.
   *
   * @return a simplified version of this Fact
   */
  def unary_!(): Fact = Fact.Unary_!(this)

  def ||(rhs: => Fact): Fact = if (isYes) this else Fact.Binary_||(this, rhs)

  def &&(rhs: => Fact): Fact = if (isNo) this else Fact.Binary_&&(this, rhs)

  final def stringPrefix: String = if (isYes) "Yes" else "No"

  /**
   * Construct failure message to report if a fact fails, using <code>rawFactMessage</code>, <code>factMessageArgs</code> and <code>prettifier</code>
   *
   * @return failure message to report if a fact fails
   */
  def factMessage: String =
    if (factMessageArgs.isEmpty) rawFactMessage
    else makeString(rawFactMessage, factMessageArgs)

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

  def midSentenceSimplifiedFactMessage: String =
    if (midSentenceSimplifiedFactMessageArgs.isEmpty) rawMidSentenceSimplifiedFactMessage
    else makeString(rawMidSentenceSimplifiedFactMessage, midSentenceSimplifiedFactMessageArgs)

  private def makeString(raw: String, args: IndexedSeq[Any]): String =
    Resources.formatString(raw, args.map(Prettifier.default).toArray)

  override def toString: String = stringPrefix + "(" + factMessage + ")"

  def midSentenceToString: String = stringPrefix + "(" + midSentenceFactMessage + ")"
}

object Fact {

  case class No(
    rawFactMessage: String,
    rawSimplifiedFactMessage: String,
    rawMidSentenceFactMessage: String,
    rawMidSentenceSimplifiedFactMessage: String,
    factMessageArgs: IndexedSeq[Any],
    simplifiedFactMessageArgs: IndexedSeq[Any],
    midSentenceFactMessageArgs: IndexedSeq[Any],
    midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any],
    override val cause: Option[Throwable] = None,
    prettifier: Prettifier = Prettifier.default
  ) extends Fact {

    val isLeaf: Boolean = true
    val isYes: Boolean = false
  }

/*
factMessage is the simplified one, if need be, and simplifiedFactMessage is a simpler one.
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
        None,
        Prettifier.default
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
    ): No =
      new No(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceFactMessageArgs,
        None,
        Prettifier.default
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
        None,
        Prettifier.default
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
    ): No =
      new No(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        None,
        Prettifier.default
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
    ): No =
      new No(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        factMessageArgs,
        simplifiedFactMessageArgs,
        None,
        Prettifier.default
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
    ): No =
      new No(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        None,
        Prettifier.default
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
        None,
        Prettifier.default
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
        None,
        Prettifier.default
      )
  }
  
  case class Yes(
    rawFactMessage: String,
    rawSimplifiedFactMessage: String,
    rawMidSentenceFactMessage: String,
    rawMidSentenceSimplifiedFactMessage: String,
    factMessageArgs: IndexedSeq[Any],
    simplifiedFactMessageArgs: IndexedSeq[Any],
    midSentenceFactMessageArgs: IndexedSeq[Any],
    midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any],
    override val cause: Option[Throwable] = None,
    prettifier: Prettifier = Prettifier.default
  ) extends Fact {
  
    val isYes: Boolean = true
    val isLeaf: Boolean = true
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
        None,
        Prettifier.default
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
    ): Yes =
      new Yes(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceFactMessageArgs,
        None,
        Prettifier.default
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
        None,
        Prettifier.default
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
    ): Yes =
      new Yes(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        None,
        Prettifier.default
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
    ): Yes =
      new Yes(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        factMessageArgs,
        simplifiedFactMessageArgs,
        None,
        Prettifier.default
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
    ): Yes =
      new Yes(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        None,
        Prettifier.default
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
        None,
        Prettifier.default
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
        None,
        Prettifier.default
      )
  }

  // The simplified Fact message is used when a Fact is negated, because
  // sometimes factMessage can include info about what was expected, as in
  // "Expected 3, but got 4", for expectResult(3) { x } . But if this is inverted
  // to !expectResult(3) { x }, then it is confusing to say Expected 3 (because
  // now anything *but* 3 is expected. So the simplified message just says, "3 did not equal 4".
  // Of course, that means x was 4, and so the inverted form would be a Yes. But if x were 3, then
  // the regular factMessage would be "Expected 3, but got 3" and the simplified fact message would be "3 equaled 3"
  case class Unary_!(underlying: Fact) extends Fact {

    val rawFactMessage: String = underlying.rawSimplifiedFactMessage
    val rawSimplifiedFactMessage: String = underlying.rawFactMessage
    val rawMidSentenceFactMessage: String = underlying.rawMidSentenceSimplifiedFactMessage
    val rawMidSentenceSimplifiedFactMessage: String = underlying.rawMidSentenceFactMessage
    val factMessageArgs: IndexedSeq[Any] = underlying.simplifiedFactMessageArgs
    val simplifiedFactMessageArgs: IndexedSeq[Any] = underlying.factMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceSimplifiedFactMessageArgs
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceFactMessageArgs
    val isLeaf: Boolean = underlying.isLeaf
    val prettifier: Prettifier = underlying.prettifier

    def isYes: Boolean = !(underlying.isYes)

    override def unary_!(): org.scalatest.Fact = underlying
  }

  private val NEWLINE = scala.compat.Platform.EOL

  private def rawDiagram(level: Int, operator: String): String = {
    val padding = "  " * level
    padding + "{0} " + operator + NEWLINE +
    padding + "{1}"
  }

  private def rawNestedDiagram(isTrue: Boolean, level: Int, operator: String): String = {
    val padding = "  " * level
    padding + (if (isTrue) "Yes(" else "No(") + NEWLINE +
    rawDiagram(level + 1, operator) + NEWLINE +
    padding + ")"
  }

  private def diagramToString(fact: Fact, level: Int): String =
    fact match {
      case binaryDoubleAmpersand: Binary_&& =>
        val raw = rawNestedDiagram(fact.isYes, level, "&&")
        val left = diagramToString(binaryDoubleAmpersand.left, level + 1)
        val right = diagramToString(binaryDoubleAmpersand.rightResult, level + 1)
        Resources.formatString(raw, Array(left, right))

      case binaryDoublePipe: Binary_|| =>
        val raw = rawNestedDiagram(fact.isYes, level, "||")
        val left = diagramToString(binaryDoublePipe.left, level + 1)
        val right = diagramToString(binaryDoublePipe.rightResult, level + 1)
        Resources.formatString(raw, Array(left, right))

      case unaryNot: Unary_! =>
        ""

      case other => fact.midSentenceToString
    }

  class Binary_&&(private[scalatest] val left: Fact, right: => Fact) extends Fact {

    private[scalatest] lazy val rightResult = right

    val rawFactMessage: String = {
      if (left.isLeaf && rightResult.isLeaf) {
        if (left.isNo) left.rawFactMessage
        else Resources.rawCommaBut
      }
      else
        rawDiagram(0, "&&")
    }
    val rawSimplifiedFactMessage: String = {
      if (left.isNo) left.rawSimplifiedFactMessage
      else Resources.rawCommaBut
    }
    val rawMidSentenceFactMessage: String = {
      if (left.isNo) left.rawMidSentenceFactMessage
      else Resources.rawCommaBut
    }
    val rawMidSentenceSimplifiedFactMessage: String = {
      if (left.isNo) left.rawMidSentenceSimplifiedFactMessage
      else Resources.rawCommaBut
    }
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && rightResult.isLeaf) {
        if (left.isNo)
          Vector(FactMessage(left)) // Keep full message if short circuiting the error message
        else
          Vector(
            SimplifiedFactMessage(left),
            if (rightResult.isInstanceOf[Unary_!]) MidSentenceFactMessage(rightResult) else MidSentenceSimplifiedFactMessage(rightResult)
          ) // Simplify if combining
      }
      else {
        Vector(UnquotedString(diagramToString(left, 0)), UnquotedString(diagramToString(rightResult, 0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = {
      if (left.isNo) Vector(SimplifiedFactMessage(left))
      else Vector(SimplifiedFactMessage(left), MidSentenceSimplifiedFactMessage(rightResult))
    }
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isNo) Vector(MidSentenceFactMessage(left)) // Keep full message if short circuiting the error message
      else Vector(MidSentenceSimplifiedFactMessage(left), MidSentenceFactMessage(rightResult)) // Simplify if combining
    }
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = {
      if (left.isNo) Vector(MidSentenceFactMessage(left))
      else Vector(MidSentenceSimplifiedFactMessage(left), MidSentenceSimplifiedFactMessage(rightResult))
    }

    val isLeaf: Boolean = false
    val prettifier: Prettifier = left.prettifier

    def isYes: Boolean = left.isYes && rightResult.isYes
  }

  object Binary_&& {
    def apply(left: Fact, right: => Fact): Fact = new Binary_&&(left, right)
  }

  class Binary_||(private[scalatest] val left: Fact, right: => Fact) extends Fact {

    private[scalatest] lazy val rightResult = right

    val rawFactMessage: String = {
      if (left.isLeaf && rightResult.isLeaf) {
        if (left.isYes) left.rawFactMessage
        else Resources.rawCommaAnd
      }
      else
        rawDiagram(0, "||")
    }
    val rawSimplifiedFactMessage: String = {
      if (left.isYes) left.rawSimplifiedFactMessage
      else Resources.rawCommaAnd
    }
    val rawMidSentenceFactMessage: String = {
      if (left.isYes) left.rawMidSentenceFactMessage
      else Resources.rawCommaAnd
    }
    val rawMidSentenceSimplifiedFactMessage: String = {
      if (left.isYes) left.rawMidSentenceSimplifiedFactMessage
      else Resources.rawCommaAnd
    }
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && rightResult.isLeaf) {
        if (left.isYes) Vector(FactMessage(left))
        else Vector(FactMessage(left), MidSentenceFactMessage(rightResult))
      }
      else {
        Vector(UnquotedString(diagramToString(left, 0)), UnquotedString(diagramToString(rightResult, 0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = {
      if (left.isYes) Vector(SimplifiedFactMessage(left))
      else Vector(FactMessage(left), MidSentenceSimplifiedFactMessage(rightResult))
    }
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isYes) Vector(MidSentenceFactMessage(left))
      else Vector(MidSentenceFactMessage(left), MidSentenceFactMessage(rightResult))
    }
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = {
      if (left.isYes) Vector(MidSentenceSimplifiedFactMessage(left))
      else Vector(MidSentenceFactMessage(left), MidSentenceSimplifiedFactMessage(rightResult))
    }

    val isLeaf: Boolean = false
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

  private[scalatest] case class SimplifiedFactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.simplifiedFactMessageArgs
    override def toString: String = fact.simplifiedFactMessage
  }

  private[scalatest] case class MidSentenceSimplifiedFactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.midSentenceSimplifiedFactMessageArgs
    override def toString: String = fact.midSentenceSimplifiedFactMessage
  }
}
