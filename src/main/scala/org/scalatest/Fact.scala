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
import java.text.MessageFormat

private[scalatest] sealed abstract class Fact {
    val rawFailureMessage: String
    val rawNegatedFailureMessage: String
    val rawMidSentenceFailureMessage: String
    val rawMidSentenceNegatedFailureMessage: String
    val failureMessageArgs: IndexedSeq[Any]
    val negatedFailureMessageArgs: IndexedSeq[Any]
    val midSentenceFailureMessageArgs: IndexedSeq[Any]
    val midSentenceNegatedFailureMessageArgs: IndexedSeq[Any]
    val composite: Boolean
    val prettifier: Prettifier


  /**
   * Get a negated version of this Fact, sub type will be negated and all messages field will be substituted with its counter-part.
   *
   * @return a negated version of this Fact
   */
    def unary_!(): Fact

    def ||(rhs: => Fact): Fact

    def &&(rhs: => Fact): Fact

  /**
   * Construct failure message to report if a fact fails, using <code>rawFailureMessage</code>, <code>failureMessageArgs</code> and <code>prettifier</code>
   *
   * @return failure message to report if a fact fails
   */
  def failureMessage: String = if (failureMessageArgs.isEmpty) rawFailureMessage else makeString(rawFailureMessage, failureMessageArgs)

  /**
   * Construct message with a meaning opposite to that of the failure message, using <code>rawNegatedFailureMessage</code>, <code>negatedFailureMessageArgs</code> and <code>prettifier</code>
   *
   * @return message with a meaning opposite to that of the failure message
   */
  def negatedFailureMessage: String = if (negatedFailureMessageArgs.isEmpty) rawNegatedFailureMessage else makeString(rawNegatedFailureMessage, negatedFailureMessageArgs)

  /**
   * Construct failure message suitable for appearing mid-sentence, using <code>rawMidSentenceFailureMessage</code>, <code>midSentenceFailureMessageArgs</code> and <code>prettifier</code>
   *
   * @return failure message suitable for appearing mid-sentence
   */
  def midSentenceFailureMessage: String = if (midSentenceFailureMessageArgs.isEmpty) rawMidSentenceFailureMessage else makeString(rawMidSentenceFailureMessage, midSentenceFailureMessageArgs)

  /**
   * Construct negated failure message suitable for appearing mid-sentence, using <code>rawMidSentenceNegatedFailureMessage</code>, <code>midSentenceNegatedFailureMessageArgs</code> and <code>prettifier</code>
   *
   * @return negated failure message suitable for appearing mid-sentence
   */
  def midSentenceNegatedFailureMessage: String = if (midSentenceNegatedFailureMessageArgs.isEmpty) rawMidSentenceNegatedFailureMessage else makeString(rawMidSentenceNegatedFailureMessage, midSentenceNegatedFailureMessageArgs)

  private def makeString(rawString: String, args: IndexedSeq[Any]): String = {
    val msgFmt = new MessageFormat(rawString)
    msgFmt.format(args.map(prettifier).toArray)
  }
}

private[scalatest] object Fact {
  def commaAnd(leftComposite: Boolean, rightComposite: Boolean): String = (leftComposite,rightComposite) match {
    case (false,false) => Resources("commaAnd")
    case (false,true) => Resources("rightParensCommaAnd")
    case (true,false) => Resources("leftParensCommaAnd")
    case (true,true) => Resources("bothParensCommaAnd")
  }

  def commaBut(leftComposite: Boolean, rightComposite: Boolean): String = (leftComposite,rightComposite) match {
    case (false,false) => Resources("commaBut")
    case (false,true) => Resources("rightParensCommaBut")
    case (true,false) => Resources("leftParensCommaBut")
    case (true,true) => Resources("bothParensCommaBut")
  }

  // Idea is to override toString each time it is used.
  private[scalatest] sealed abstract class LazyMessage {
    val nestedArgs: IndexedSeq[Any]
  }

  private[scalatest] case class FailureMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.failureMessageArgs
    override def toString: String = fact.failureMessage
  }

  private[scalatest] case class NegatedFailureMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.negatedFailureMessageArgs
    override def toString: String = fact.negatedFailureMessage
  }

  private[scalatest] case class MidSentenceFailureMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.failureMessageArgs
    override def toString: String = fact.midSentenceFailureMessage
  }

  private[scalatest] case class MidSentenceNegatedFailureMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.negatedFailureMessageArgs
    override def toString: String = fact.midSentenceNegatedFailureMessage
  }
}

import org.scalatest.Fact._

private[scalatest] case class No(
	rawFailureMessage: String,
    rawNegatedFailureMessage: String,
    rawMidSentenceFailureMessage: String,
    rawMidSentenceNegatedFailureMessage: String,
    failureMessageArgs: IndexedSeq[Any],
    negatedFailureMessageArgs: IndexedSeq[Any],
    midSentenceFailureMessageArgs: IndexedSeq[Any],
    midSentenceNegatedFailureMessageArgs: IndexedSeq[Any],
    composite: Boolean = false,
    prettifier: Prettifier = Prettifier.default
) extends Fact {
	def unary_!() = Yes(
    rawNegatedFailureMessage,
    rawFailureMessage,
    rawMidSentenceNegatedFailureMessage,
    rawMidSentenceFailureMessage,
    negatedFailureMessageArgs,
    failureMessageArgs,
    midSentenceNegatedFailureMessageArgs,
    midSentenceFailureMessageArgs,
    composite,
    prettifier)

  def &&(rhs: => Fact) = this
  def ||(rhs: => Fact) = rhs match {
      case yes: Yes => Yes(
        commaAnd(this.composite, yes.composite),
        commaAnd(this.composite, yes.composite),
        commaAnd(this.composite, yes.composite),
        commaAnd(this.composite, yes.composite),
        Vector(FailureMessage(this), MidSentenceFailureMessage(yes)),
        Vector(FailureMessage(this), MidSentenceNegatedFailureMessage(yes)),
        Vector(MidSentenceFailureMessage(this), MidSentenceFailureMessage(yes)),
        Vector(MidSentenceFailureMessage(this), MidSentenceNegatedFailureMessage(yes)),
        true
      )
      case no:  No  =>  No(
        commaAnd(this.composite, no.composite),
        commaAnd(this.composite, no.composite),
        commaAnd(this.composite, no.composite),
        commaAnd(this.composite, no.composite),
        Vector(FailureMessage(this), MidSentenceFailureMessage(no)),
        Vector(FailureMessage(this), MidSentenceNegatedFailureMessage(no)),
        Vector(MidSentenceFailureMessage(this), MidSentenceFailureMessage(no)),
        Vector(MidSentenceFailureMessage(this), MidSentenceNegatedFailureMessage(no)),
        true
      )
  }
  override def toString: String = s"No($failureMessage)"
}

/**
 * Companion object for the <code>No</code> case class.
 *
 * @author Bill Venners
 */
private[scalatest] object No {

  /**
   * Factory method that constructs a new <code>No</code> with passed <code>failureMessage</code>, 
   * <code>negativeFailureMessage</code>, <code>midSentenceFailureMessage</code>, 
   * <code>midSentenceNegatedFailureMessage</code>, <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> fields.
   * <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> will be used in place of <code>midSentenceFailureMessageArgs</code>
   * and <code>midSentenceNegatedFailureMessageArgs</code>.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param rawMidSentenceFailureMessage raw failure message to report if a match fails
   * @param rawMidSentenceNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param failureMessageArgs arguments for constructing failure message to report if a match fails
   * @param negatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @return a <code>No</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, rawMidSentenceFailureMessage: String,
      rawMidSentenceNegatedFailureMessage: String, failureMessageArgs: IndexedSeq[Any], negatedFailureMessageArgs: IndexedSeq[Any]): No =
    new No(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawMidSentenceFailureMessage,
      rawMidSentenceNegatedFailureMessage,
      failureMessageArgs,
      negatedFailureMessageArgs,
      failureMessageArgs,
      negatedFailureMessageArgs,
      false,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>No</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code>, <code>rawMidSentenceFailureMessage</code>, and
   * <code>rawMidSentenceNegatedFailureMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
   * This is suitable to create No with eager error messages, and its mid-sentence messages need to be different.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param rawMidSentenceFailureMessage raw failure message to report if a match fails
   * @param rawMidSentenceNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @return a <code>No</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, rawMidSentenceFailureMessage: String,
      rawMidSentenceNegatedFailureMessage: String): No =
    new No(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawMidSentenceFailureMessage,
      rawMidSentenceNegatedFailureMessage,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      false,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>No</code> with passed <code>rawFailureMessage</code>, and
   * <code>rawNegativeFailureMessage</code> fields. The <code>rawMidSentenceFailureMessage</code> will return the same
   * string as <code>rawFailureMessage</code>, and the <code>rawMidSentenceNegatedFailureMessage</code> will return the
   * same string as <code>rawNegatedFailureMessage</code>.  All argument fields will have <code>Vector.empty</code> values.
   * This is suitable to create No with eager error messages that have same mid-sentence messages.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @return a <code>No</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String): No =
    new No(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      false,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>No</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code> and <code>args</code> fields.  The <code>rawMidSentenceFailureMessage</code> will return the same
   * string as <code>rawFailureMessage</code>, and the <code>rawMidSentenceNegatedFailureMessage</code> will return the
   * same string as <code>rawNegatedFailureMessage</code>.  All argument fields will use <code>args</code> as arguments.
   * This is suitable to create No with lazy error messages that have same mid-sentence messages and arguments.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param args arguments for error messages construction
   * @return a <code>No</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, args: IndexedSeq[Any]) =
    new No(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      args,
      args,
      args,
      args,
      false,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>No</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code>, <code>failureMessageArgs</code> and <code>negatedFailureMessageArgs</code> fields.
   * The <code>rawMidSentenceFailureMessage</code> will return the same string as <code>rawFailureMessage</code>, and the
   * <code>rawMidSentenceNegatedFailureMessage</code> will return the same string as <code>rawNegatedFailureMessage</code>.
   * The <code>midSentenceFailureMessageArgs</code> will return the same as <code>failureMessageArgs</code>, and the
   * <code>midSentenceNegatedFailureMessageArgs</code> will return the same as <code>negatedFailureMessageArgs</code>.
   * This is suitable to create No with lazy error messages that have same mid-sentence and use different arguments for
   * negated messages.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param failureMessageArgs arguments for constructing failure message to report if a match fails
   * @param negatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @return a <code>No</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, failureMessageArgs: IndexedSeq[Any], negatedFailureMessageArgs: IndexedSeq[Any]) =
    new No(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      failureMessageArgs,
      negatedFailureMessageArgs,
      failureMessageArgs,
      negatedFailureMessageArgs,
      false,
      Prettifier.default
    )
}


private[scalatest] case class Yes(
	rawFailureMessage: String,
    rawNegatedFailureMessage: String,
    rawMidSentenceFailureMessage: String,
    rawMidSentenceNegatedFailureMessage: String,
    failureMessageArgs: IndexedSeq[Any],
    negatedFailureMessageArgs: IndexedSeq[Any],
    midSentenceFailureMessageArgs: IndexedSeq[Any],
    midSentenceNegatedFailureMessageArgs: IndexedSeq[Any],
    composite: Boolean = false,
    prettifier: Prettifier = Prettifier.default) extends Fact {

	def unary_!() = No(
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawMidSentenceNegatedFailureMessage,
      rawMidSentenceFailureMessage,
      negatedFailureMessageArgs,
      failureMessageArgs,
      midSentenceNegatedFailureMessageArgs,
      midSentenceFailureMessageArgs,
      composite,
      prettifier)

  def &&(rhs: => Fact) = rhs match {
      case yes: Yes => Yes(
        commaBut(this.composite, yes.composite),
        commaAnd(this.composite, yes.composite),
        commaBut(this.composite, yes.composite),
        commaAnd(this.composite, yes.composite),
        Vector(NegatedFailureMessage(this), MidSentenceFailureMessage(yes)),
        Vector(NegatedFailureMessage(this), MidSentenceNegatedFailureMessage(yes)),
        Vector(MidSentenceNegatedFailureMessage(this), MidSentenceFailureMessage(yes)),
        Vector(MidSentenceNegatedFailureMessage(this), MidSentenceNegatedFailureMessage(yes)),
        true
      )
      case no: No  =>  No(
        commaBut(this.composite, no.composite),
        commaAnd(this.composite, no.composite),
        commaBut(this.composite, no.composite),
        commaAnd(this.composite, no.composite),
        Vector(NegatedFailureMessage(this), MidSentenceFailureMessage(no)),
        Vector(NegatedFailureMessage(this), MidSentenceNegatedFailureMessage(no)),
        Vector(MidSentenceNegatedFailureMessage(this), MidSentenceFailureMessage(no)),
        Vector(MidSentenceNegatedFailureMessage(this), MidSentenceNegatedFailureMessage(no)),
        true
      )
  }

  def ||(rhs: => Fact) = this

  override def toString: String = s"Yes($negatedFailureMessage)"
}

/**
 * Companion object for the <code>Yes</code> case class.
 *
 * @author Bill Venners
 */
private[scalatest] object Yes {

  /**
   * Factory method that constructs a new <code>Yes</code> with passed code>failureMessage</code>, 
   * <code>negativeFailureMessage</code>, <code>midSentenceFailureMessage</code>, 
   * <code>midSentenceNegatedFailureMessage</code>, <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> fields.
   * <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> will be used in place of <code>midSentenceFailureMessageArgs</code>
   * and <code>midSentenceNegatedFailureMessageArgs</code>.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param rawMidSentenceFailureMessage raw failure message to report if a match fails
   * @param rawMidSentenceNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param failureMessageArgs arguments for constructing failure message to report if a match fails
   * @param negatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @return a <code>Yes</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, rawMidSentenceFailureMessage: String,
      rawMidSentenceNegatedFailureMessage: String, failureMessageArgs: IndexedSeq[Any], negatedFailureMessageArgs: IndexedSeq[Any]): Yes =
    new Yes(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawMidSentenceFailureMessage,
      rawMidSentenceNegatedFailureMessage,
      failureMessageArgs,
      negatedFailureMessageArgs,
      failureMessageArgs,
      negatedFailureMessageArgs,
      false,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>Yes</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code>, <code>rawMidSentenceFailureMessage</code>, and
   * <code>rawMidSentenceNegatedFailureMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
   * This is suitable to create Yes with eager error messages, and its mid-sentence messages need to be different.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param rawMidSentenceFailureMessage raw failure message to report if a match fails
   * @param rawMidSentenceNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @return a <code>Yes</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, rawMidSentenceFailureMessage: String,
      rawMidSentenceNegatedFailureMessage: String): Yes =
    new Yes(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawMidSentenceFailureMessage,
      rawMidSentenceNegatedFailureMessage,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      false,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>Yes</code> with passed <code>rawFailureMessage</code>, and
   * <code>rawNegativeFailureMessage</code> fields. The <code>rawMidSentenceFailureMessage</code> will return the same
   * string as <code>rawFailureMessage</code>, and the <code>rawMidSentenceNegatedFailureMessage</code> will return the
   * same string as <code>rawNegatedFailureMessage</code>.  All argument fields will have <code>Vector.empty</code> values.
   * This is suitable to create Yes with eager error messages that have same mid-sentence messages.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @return a <code>Yes</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String): Yes =
    new Yes(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      false,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>Yes</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code> and <code>args</code> fields.  The <code>rawMidSentenceFailureMessage</code> will return the same
   * string as <code>rawFailureMessage</code>, and the <code>rawMidSentenceNegatedFailureMessage</code> will return the
   * same string as <code>rawNegatedFailureMessage</code>.  All argument fields will use <code>args</code> as arguments.
   * This is suitable to create Yes with lazy error messages that have same mid-sentence messages and arguments.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param args arguments for error messages construction
   * @return a <code>Yes</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, args: IndexedSeq[Any]) =
    new Yes(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      args,
      args,
      args,
      args,
      false,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>Yes</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code>, <code>failureMessageArgs</code> and <code>negatedFailureMessageArgs</code> fields.
   * The <code>rawMidSentenceFailureMessage</code> will return the same string as <code>rawFailureMessage</code>, and the
   * <code>rawMidSentenceNegatedFailureMessage</code> will return the same string as <code>rawNegatedFailureMessage</code>.
   * The <code>midSentenceFailureMessageArgs</code> will return the same as <code>failureMessageArgs</code>, and the
   * <code>midSentenceNegatedFailureMessageArgs</code> will return the same as <code>negatedFailureMessageArgs</code>.
   * This is suitable to create Yes with lazy error messages that have same mid-sentence and use different arguments for
   * negated messages.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param failureMessageArgs arguments for constructing failure message to report if a match fails
   * @param negatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @return a <code>Yes</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, failureMessageArgs: IndexedSeq[Any], negatedFailureMessageArgs: IndexedSeq[Any]) =
    new Yes(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      failureMessageArgs,
      negatedFailureMessageArgs,
      failureMessageArgs,
      negatedFailureMessageArgs,
      false,
      Prettifier.default
    )
}