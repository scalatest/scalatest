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

import org.scalactic._
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestCanceledException

// As it stands, this should not extend Product with Serializable because
// subclasses exists that anen't case classes.
sealed abstract class Fact {

  val rawFactMessage: String
  val rawSimplifiedFactMessage: String

  val factMessageArgs: IndexedSeq[Any]
  val simplifiedFactMessageArgs: IndexedSeq[Any]

  val isLeaf: Boolean
  val isVacuousYes: Boolean
  val prettifier: Prettifier

  val cause: Option[Throwable] = None

  val isYes: Boolean

  final def isNo: Boolean = !isYes

  final def toBoolean: Boolean = isYes

  final def toAssertion(implicit pos: source.Position): Assertion = {
    if (isYes) {
      if (!isVacuousYes) Succeeded
      else throw new TestCanceledException((e: StackDepthException) => Some(factMessage), None, pos, None)
    }
    else throw new TestFailedException((e: StackDepthException) => Some(factMessage), None, pos)
  }

  // TODO: When things are stable, dryign deleting this as it is identical to toAssertion now.
  // This is called internally by implicit conversions, which has different stack depth
  private[scalatest] final def internalToAssertion(pos: source.Position): Assertion = {
    if (isYes) {
      if (!isVacuousYes) Succeeded
      else throw new TestCanceledException((e: StackDepthException) => Some(factMessage), None, pos, None)
    }
    else throw new TestFailedException((e: StackDepthException) => Some(factMessage), None, pos)
  }

  /**
   * Get a simplified version of this Fact, sub type will be simplified and all messages field will be substituted with its counter-part.
   *
   * @return a simplified version of this Fact
   */
  def unary_!(): Fact = Fact.Unary_!(this)

  final def ||(rhs: => Fact): Fact = if (isYes) this else Fact.Binary_||(this, rhs)

  final def &&(rhs: => Fact): Fact = if (isNo) this else Fact.Binary_&&(this, rhs)

  final def |(rhs: Fact): Fact = Fact.Binary_|(this, rhs)

  final def &(rhs: Fact): Fact = Fact.Binary_&(this, rhs)

  final def stringPrefix: String =
    if (isYes) {
      if (isVacuousYes) "VacuousYes" else "Yes"
    }
    else "No"

  final def implies(rhs: => Fact): Fact = if (isNo) Fact.VacuousYes(this) else Fact.Implies(this, rhs)

  final def isEqvTo(rhs: Fact): Fact = Fact.IsEqvTo(this, rhs)

  /**
   * Construct failure message to report if a fact fails, using <code>rawFactMessage</code> and <code>factMessageArgs</code>
   *
   * @return failure message to report if a fact fails
   */
  def factMessage: String =
    if (factMessageArgs.isEmpty) rawFactMessage
    else makeString(rawFactMessage, factMessageArgs)

  def simplifiedFactMessage: String =
    if (simplifiedFactMessageArgs.isEmpty) rawSimplifiedFactMessage
    else makeString(rawSimplifiedFactMessage, simplifiedFactMessageArgs)

  private def makeString(raw: String, args: IndexedSeq[Any]): String =
    Resources.formatString(raw, args.map(prettifier.apply).toArray)

  private[scalatest] val NEWLINE = scala.compat.Platform.EOL

  // This one makes sense for Yes and No only. The other subclassess override it.
  def factDiagram(level: Int): String = {
    val msg = factMessage // just compute this once
    val padding = "  " * level
    if (msg.contains("\n")) {
      val padding = "  " * (level)
      padding + stringPrefix + "(" + NEWLINE + msg.split("\n").map(line => padding + "  " + line).mkString("\n") + NEWLINE + padding + ")"
    }
    else
      padding + stringPrefix + "(" + msg + ")"
  }

  override def toString: String = factDiagram(0)
}

object Fact {

  case class Leaf(
    rawFactMessage: String,
    rawSimplifiedFactMessage: String,
    factMessageArgs: IndexedSeq[Any],
    simplifiedFactMessageArgs: IndexedSeq[Any],
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

    val factMessageArgs: IndexedSeq[Any] = underlying.factMessageArgs
    val simplifiedFactMessageArgs: IndexedSeq[Any] = underlying.simplifiedFactMessageArgs

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

    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any],
      cause: Option[Throwable] = None
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        false,
        false,
        prettifier,
        cause
      )

    def apply( // Just used in tests
      rawFactMessage: String,
      rawSimplifiedFactMessage: String
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        Vector.empty,
        Vector.empty,
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
        Vector.empty,
        Vector.empty,
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
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any],
      isVacuousYes: Boolean = false,
      cause: Option[Throwable] = None
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        true,
        isVacuousYes,
        prettifier,
        cause
      )

    def apply( // Just used in tests
      rawFactMessage: String,
      rawSimplifiedFactMessage: String
    )(implicit prettifier: Prettifier): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        Vector.empty,
        Vector.empty,
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
        Vector.empty,
        Vector.empty,
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
    val factMessageArgs: IndexedSeq[Any] = underlying.simplifiedFactMessageArgs
    val simplifiedFactMessageArgs: IndexedSeq[Any] = underlying.simplifiedFactMessageArgs
    val isLeaf: Boolean = underlying.isLeaf
    val prettifier: Prettifier = underlying.prettifier

    val isYes: Boolean = !(underlying.isYes)
    val isVacuousYes: Boolean = false

    override def unary_!(): org.scalatest.Fact = underlying

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
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(
          SimplifiedFactMessage(left),
          SimplifiedFactMessage(right)
        ) // Simplify if combining
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs


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
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(SimplifiedFactMessage(left), SimplifiedFactMessage(right))
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs

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
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(
          SimplifiedFactMessage(left),
          SimplifiedFactMessage(right)
        ) // Simplify if combining
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs


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
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(SimplifiedFactMessage(left), SimplifiedFactMessage(right))
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs

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

  private[scalatest] case class SimplifiedFactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.simplifiedFactMessageArgs
    override def toString: String = fact.simplifiedFactMessage
  }
}
