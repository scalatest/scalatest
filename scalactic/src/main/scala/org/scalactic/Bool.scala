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
package org.scalactic

/**
 * A trait that represent a rich-featured boolean value, which includes the following members:
 *
 * <ul>
 * <li>a boolean value</li>
 * <li>methods useful for failure messages construction</li>
 * <li>logical expression methods that makes <code>Bool</code> composable</li>
 * </ul>
 *
 * <code>Bool</code> is used by code generated from <code>BooleanMacro</code> (which <code>AssertionsMacro</code> and <code>RequirementsMacro</code> uses),
 * it needs to be public so that the generated code can be compiled.  It is expected that ScalaTest users would ever need to use <code>Bool</code> directly.
 */
trait Bool {

  val prettifier: Prettifier

  private def makeString(raw: String, args: Array[Any]): String =
    Resources.formatString(raw, args.map(prettifier.apply))

  lazy val analysis: scala.collection.immutable.IndexedSeq[String] = Vector.empty

  /**
   * Construct and return failure message, by applying arguments returned from <code>failureMessageArgs</code> to
   * raw message returned from <code>rawFailureMessage</code>
   */
  def failureMessage: String =
    if (failureMessageArgs.isEmpty) rawFailureMessage else makeString(rawFailureMessage, failureMessageArgs.toArray)

  /**
   * Construct and return negated failure message, by applying arguments returned from <code>negatedFailureMessageArgs</code> to
   * raw message returned from <code>rawNegatedFailureMessage</code>
   */
  def negatedFailureMessage: String =
    if (negatedFailureMessageArgs.isEmpty) rawNegatedFailureMessage else makeString(rawNegatedFailureMessage, negatedFailureMessageArgs.toArray)

  /**
   * Construct and return mid sentence failure message, by applying arguments returned from <code>midSentenceFailureMessageArgs</code> to
   * raw message returned from <code>rawMidSentenceFailureMessage</code>
   */
  def midSentenceFailureMessage: String =
    if (midSentenceFailureMessageArgs.isEmpty) rawMidSentenceFailureMessage else makeString(rawMidSentenceFailureMessage, midSentenceFailureMessageArgs.toArray)

  /**
   * Construct and return mid sentence negated failure message, by applying arguments returned from <code>midSentenceNegatedFailureMessageArgs</code> to
   * raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>
   */
  def midSentenceNegatedFailureMessage: String =
    if (midSentenceNegatedFailureMessageArgs.isEmpty) rawMidSentenceNegatedFailureMessage else makeString(rawMidSentenceNegatedFailureMessage, midSentenceNegatedFailureMessageArgs.toArray)

  /**
   * the <code>Boolean</code> value of this <code>Bool</code>
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
   * Logical <code>and</code> this <code>Bool</code> with another <code>Bool</code>
   *
   * @param bool another <code>Bool</code>
   * @return a <code>Bool</code> that represents the result of logical <code>and</code>
   */
  def &&(bool: => Bool): Bool =
    if (value)
      new AndBool(this, bool, prettifier)
    else
      this

  /**
   * Logical <code>and</code> this <code>Bool</code> with another <code>Bool</code>
   *
   * @param bool another <code>Bool</code>
   * @return a <code>Bool</code> that represents the result of logical <code>and</code>
   */
  def &(bool: => Bool): Bool = &&(bool)

  /**
   * Logical <code>or</code> this <code>Bool</code> with another <code>Bool</code>
   *
   * @param bool another <code>Bool</code>
   * @return a <code>Bool</code> that represents the result of logical <code>or</code>
   */
  def ||(bool: => Bool): Bool = new OrBool(this, bool, prettifier)

  /**
   * Logical <code>or</code> this <code>Bool</code> with another <code>Bool</code>
   *
   * @param bool another <code>Bool</code>
   * @return a <code>Bool</code> that represents the result of logical <code>or</code>
   */
  def |(bool: => Bool): Bool = ||(bool)

  /**
   * Negate this <code>Bool</code>
   *
   * @return a <code>Bool</code> that represents the result of negating the original <code>Bool</code>
   */
  def unary_! : Bool = new NotBool(this, prettifier)
}

/**
 * <code>Bool</code> companion object that provides factory methods to create different sub types of <code>Bool</code>
 *
 * <code>Bool</code> is used by code generated from <code>BooleanMacro</code> (which <code>AssertionsMacro</code> and <code>RequirementsMacro</code> uses),
 * it needs to be public so that the generated code can be compiled.  It is expected that ScalaTest users would ever need to use <code>Bool</code> directly.
 */
object Bool {

  /**
   * Create a negated version of the given <code>Bool</code>
   *
   * @param bool the given <code>Bool</code>
   * @param prettifier the <code>Prettifier</code> used for prettifying messages, this need to be implicit to let the compiler resolve it implicitly, it will be too tricky for our macro to resolve it and pass it over explicitly.
   * @return a negated version of the given <code>Bool</code>
   */
  def notBool(bool: Bool, prettifier: Prettifier): Bool = new NotBool(bool, prettifier)

  /**
   * Create simple macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap an unrecognized <code>Boolean</code> expression.
   *
   * @param expression the <code>Boolean</code> expression
   * @param expressionText the original expression text (source code)
   * @param prettifier the <code>Prettifier</code> used for prettifying messages, this need to be implicit to let the compiler resolve it implicitly, it will be too tricky for our macro to resolve it and pass it over explicitly.
   * @return a simple macro <code>Bool</code>
   */
  def simpleMacroBool(expression: Boolean, expressionText: String, prettifier: Prettifier): Bool = new SimpleMacroBool(expression, expressionText, prettifier)

  /**
   * Create binary macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap a recognized <code>Boolean</code> expression, which includes <code>Boolean</code> expression that
   * uses <code>==</code>, <code>===</code>, <code>!=</code>, <code>!==</code>, <code>&gt;</code>, <code>&gt;=</code>, <code>&lt;</code>, <code>&lt;=</code>, <code>&&</code>,
   * <code>&</code>, <code>||</code> and <code>|</code>.
   *
   * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
   * @param operator the operator (method name) of the <code>Boolean</code> expression
   * @param right the right-hand-side (RHS) of the <code>Boolean</code> expression
   * @param expression the <code>Boolean</code> expression
   * @param prettifier the <code>Prettifier</code> used for prettifying messages, this need to be implicit to let the compiler resolve it implicitly, it will be too tricky for our macro to resolve it and pass it over explicitly.
   * @return a binary macro <code>Bool</code>
   */
  def binaryMacroBool(left: Any, operator: String, right: Any, expression: Boolean, prettifier: Prettifier): Bool = new BinaryMacroBool(left, operator, right, expression, prettifier)

  /**
   * Overloaded method that takes a <code>Bool</code> in place of <code>Boolean</code> expression to create a new binary macro <code>Bool</code>.
   *
   * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
   * @param operator the operator (method name) of the <code>Boolean</code> expression
   * @param right the right-hand-side (RHS) of the <code>Boolean</code> expression
   * @param bool the <code>Bool</code> that will provide the <code>Boolean</code> expression value with <code>bool.value</code>
   * @param prettifier the <code>Prettifier</code> used for prettifying messages, this need to be implicit to let the compiler resolve it implicitly, it will be too tricky for our macro to resolve it and pass it over explicitly.
   * @return a binary macro <code>Bool</code>
   */
  def binaryMacroBool(left: Any, operator: String, right: Any, bool: Bool, prettifier: Prettifier): Bool = new BinaryMacroBool(left, operator, right, bool, prettifier)

  /**
   * Create unary macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap a recognized <code>Boolean</code> expression represented by a unary method call,
   * which includes <code>Boolean</code> expression that uses <code>isEmpty</code>.
   *
   * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
   * @param operator the operator (method name) of the <code>Boolean</code> expression
   * @param expression the <code>Boolean</code> expression
   * @param prettifier the <code>Prettifier</code> used for prettifying messages, this need to be implicit to let the compiler resolve it implicitly, it will be too tricky for our macro to resolve it and pass it over explicitly.
   * @return a unary macro <code>Bool</code>
   */
  def unaryMacroBool(left: Any, operator: String, expression: Boolean, prettifier: Prettifier): Bool =
    new UnaryMacroBool(left, operator, expression, prettifier)

  /**
   * Overloaded method that takes a <code>Bool</code> in place of <code>Boolean</code> expression to create a new unary macro <code>Bool</code>.
   *
   * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
   * @param operator the operator (method name) of the <code>Boolean</code> expression
   * @param bool the <code>Bool</code> that will provide the <code>Boolean</code> expression value with <code>bool.value</code>
   * @param prettifier the <code>Prettifier</code> used for prettifying messages, this need to be implicit to let the compiler resolve it implicitly, it will be too tricky for our macro to resolve it and pass it over explicitly.
   * @return a binary macro <code>Bool</code>
   */
  def unaryMacroBool(left: Any, operator: String, bool: Bool, prettifier: Prettifier): Bool =
    new UnaryMacroBool(left, operator, bool.value, prettifier)

  /**
   * Create macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap a recognized <code>Boolean</code> expression
   * represented by a <code>isInstanceOf</code> method call,
   *
   * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
   * @param operator the operator (method name) of the <code>Boolean</code> expression
   * @param className the class name passed to <code>isInstanceOf</code> method call
   * @param expression the <code>Boolean</code> expression
   * @param prettifier the <code>Prettifier</code> used for prettifying messages, this need to be implicit to let the compiler resolve it implicitly, it will be too tricky for our macro to resolve it and pass it over explicitly.
   * @return a <code>Bool</code> instance that represents a <code>isInstanceOf</code> method call
   */
  def isInstanceOfMacroBool(left: Any, operator: String, className: String, expression: Boolean, prettifier: Prettifier): Bool =
    new IsInstanceOfMacroBool(left, operator, className, expression, prettifier)

  /**
   * Overloaded method that takes a <code>Bool</code> in place of <code>Boolean</code> expression to create a new <code>isInstanceOf</code>
   * macro <code>Bool</code>.
   *
   * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
   * @param operator the operator (method name) of the <code>Boolean</code> expression
   * @param className the class name passed to <code>isInstanceOf</code> method call
   * @param bool the <code>Bool</code> that will provide the <code>Boolean</code> expression value with <code>bool.value</code>
   * @param prettifier the <code>Prettifier</code> used for prettifying messages, this need to be implicit to let the compiler resolve it implicitly, it will be too tricky for our macro to resolve it and pass it over explicitly.
   * @return a <code>Bool</code> instance that represents a <code>isInstanceOf</code> method call
   */
  def isInstanceOfMacroBool(left: Any, operator: String, className: String, bool: Bool, prettifier: Prettifier): Bool =
    new IsInstanceOfMacroBool(left, operator, className, bool.value, prettifier)

  /**
   * Create macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap a recognized <code>Boolean</code> expression
   * represented by <code>length</code> and <code>size</code> method call,
   *
   * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
   * @param operator the operator (method name) of the <code>Boolean</code> expression
   * @param actual the actual value returned from <code>length</code> or <code>size</code> method call
   * @param expected the expected value returned from <code>length</code> or <code>size</code> method call
   * @param prettifier the <code>Prettifier</code> used for prettifying messages, this need to be implicit to let the compiler resolve it implicitly, it will be too tricky for our macro to resolve it and pass it over explicitly.
   * @return a <code>Bool</code> instance that represents a <code>length</code> or <code>size</code> method call
   */
  def lengthSizeMacroBool(left: Any, operator: String, actual: Any, expected: Any, prettifier: Prettifier): Bool =
    new LengthSizeMacroBool(left, operator, actual, expected, prettifier)

  /**
   * Create exists macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap a recognized <code>Boolean</code> expression
   * represented by <code>exists</code> method call.
   *
   * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
   * @param right the right-hand-side (RHS) of the <code>Boolean</code> expression
   * @param expression the <code>Boolean</code> expression
   * @param prettifier the <code>Prettifier</code> used for prettifying messages, this need to be implicit to let the compiler resolve it implicitly, it will be too tricky for our macro to resolve it and pass it over explicitly.
   * @return a exists macro <code>Bool</code>
   */
  def existsMacroBool(left: Any, right: Any, expression: Boolean, prettifier: Prettifier): Bool =
    new ExistsMacroBool(left, right, expression, prettifier)

  /**
   * A helper method to check is the given <code>Bool</code> is a simple macro <code>Bool</code> and contains empty expression text.
   *
   * @param bool the <code>Bool</code> to check
   * @return <code>true</code> if the given <code>Bool</code> is a simple macro <code>Bool</code> and contains empty expression text, <code>false</code> otherwise.
   */
  def isSimpleWithoutExpressionText(bool: Bool): Boolean =
    bool match {
      case s: org.scalactic.SimpleMacroBool if s.expressionText.isEmpty => true
      case _ => false
    }
}

private[scalactic] class SimpleBool(expression: Boolean, val prettifier: Prettifier) extends Bool {

  /**
   * the <code>Boolean</code> value of this <code>Bool</code>
   */
  val value: Boolean = expression

  /**
   * raw message to report a failure
   *
   * @return Localized string for "Expression was false"
   */
  def rawFailureMessage: String = Resources.rawExpressionWasFalse

  /**
   * raw message with a meaning opposite to that of the failure message
   *
   * @return Localized string for "Expression was true"
   */
  def rawNegatedFailureMessage: String = Resources.rawExpressionWasTrue

  /**
   * raw mid sentence message to report a failure
   *
   * @return Localized string for "Expression was false"
   */
  def rawMidSentenceFailureMessage: String = Resources.rawExpressionWasFalse

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   *
   * @return Localized string for "Expression was false"
   */
  def rawMidSentenceNegatedFailureMessage: String = Resources.rawExpressionWasTrue

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
 * Bool that represents the result of logical <code>AND</code> of two <code>Bool</code>.
 *
 * @param bool1 the first <code>Bool</code>
 * @param bool2 the second <code>Bool</code>
 */
private[scalactic] class AndBool(bool1: Bool, bool2: => Bool, val prettifier: Prettifier) extends Bool {

  /**
   * the result of <code>bool1.value</code> logical <code>AND</code> <code>bool2.value</code>
   */
  lazy val value: Boolean = bool1.value && bool2.value

  /**
   * raw message to report a failure
   *
   * @return Localized raw string for "{0}, but {1}"
   */
  def rawFailureMessage: String = Resources.rawCommaBut

  /**
   * raw message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawNegatedFailureMessage: String = Resources.rawCommaAnd

  /**
   * raw mid sentence message to report a failure
   *
   * @return Localized raw string for "{0}, but {1}"
   */
  def rawMidSentenceFailureMessage: String = Resources.rawCommaBut

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawMidSentenceNegatedFailureMessage: String = Resources.rawCommaAnd

  /**
   * Arguments to construct final failure message with raw message returned from <code>rawFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>bool1.negatedFailureMessage</code> and <code>bool2.midSentenceFailureMessage</code>
   */
  def failureMessageArgs = Vector(bool1.negatedFailureMessage, bool2.midSentenceFailureMessage)

  /**
   * Arguments to construct final negated failure message with raw message returned from <code>rawNegatedFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>bool1.negatedFailureMessage</code> and <code>bool2.midSentenceNegatedFailureMessage</code>
   */
  def negatedFailureMessageArgs = Vector(bool1.negatedFailureMessage, bool2.midSentenceNegatedFailureMessage)

  /**
   * Arguments to construct final mid sentence failure message with raw message returned from <code>rawMidSentenceFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>bool1.midSentenceNegatedFailureMessage</code> and <code>bool2.midSentenceFailureMessage</code>
   */
  def midSentenceFailureMessageArgs = Vector(bool1.midSentenceNegatedFailureMessage, bool2.midSentenceFailureMessage)

  /**
   * Arguments to construct final negated mid sentence failure message with raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>bool1.midSentenceNegatedFailureMessage</code> and <code>bool2.midSentenceNegatedFailureMessage</code>
   */
  def midSentenceNegatedFailureMessageArgs = Vector(bool1.midSentenceNegatedFailureMessage, bool2.midSentenceNegatedFailureMessage)
}

/**
 * Bool that represents the result of logical <code>OR</code> of two <code>Bool</code>.
 *
 * @param bool1 the first <code>Bool</code>
 * @param bool2 the second <code>Bool</code>
 */
private[scalactic] class OrBool(bool1: Bool, bool2: => Bool, val prettifier: Prettifier) extends Bool {

  /**
   * the result of <code>bool1.value</code> logical <code>OR</code> <code>bool2.value</code>
   */
  lazy val value: Boolean = bool1.value || bool2.value

  /**
   * raw message to report a failure
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawFailureMessage: String = Resources.rawCommaAnd

  /**
   * raw message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawNegatedFailureMessage: String = Resources.rawCommaAnd

  /**
   * raw mid sentence message to report a failure
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawMidSentenceFailureMessage: String = Resources.rawCommaAnd

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string for "{0}, and {1}"
   */
  def rawMidSentenceNegatedFailureMessage: String = Resources.rawCommaAnd

  /**
   * Arguments to construct final failure message with raw message returned from <code>rawFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>bool1.failureMessage</code> and <code>bool2.midSentenceFailureMessage</code>
   */
  def failureMessageArgs = Vector(bool1.failureMessage, bool2.midSentenceFailureMessage)

  /**
   * Arguments to construct final negated failure message with raw message returned from <code>rawNegatedFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>bool1.failureMessage</code> and <code>bool2.midSentenceNegatedFailureMessage</code>
   */
  def negatedFailureMessageArgs = Vector(bool1.failureMessage, bool2.midSentenceNegatedFailureMessage)

  /**
   * Arguments to construct final mid sentence failure message with raw message returned from <code>rawMidSentenceFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>bool1.midSentenceFailureMessage</code> and <code>bool2.midSentenceFailureMessage</code>
   */
  def midSentenceFailureMessageArgs = Vector(bool1.midSentenceFailureMessage, bool2.midSentenceFailureMessage)

  /**
   * Arguments to construct final negated mid sentence failure message with raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>.
   *
   * @return <code>Vector</code> that contains <code>bool1.midSentenceFailureMessage</code> and <code>bool2.midSentenceNegatedFailureMessage</code>
   */
  def midSentenceNegatedFailureMessageArgs = Vector(bool1.midSentenceFailureMessage, bool2.midSentenceNegatedFailureMessage)
}

private[scalactic] class NotBool(bool: Bool, val prettifier: Prettifier) extends Bool {

  val value: Boolean = !bool.value

  /**
   * raw message to report a failure
   *
   * @return the passed in <code>bool.rawNegatedFailureMessage</code>
   */
  def rawFailureMessage: String = bool.rawNegatedFailureMessage

  /**
   * raw message with a meaning opposite to that of the failure message
   *
   * @return the passed in <code>bool.rawFailureMessage</code>
   */
  def rawNegatedFailureMessage: String = bool.rawFailureMessage

  /**
   * raw mid sentence message to report a failure
   *
   * @return the passed in <code>bool.rawMidSentenceNegatedFailureMessage</code>
   */
  def rawMidSentenceFailureMessage: String = bool.rawMidSentenceNegatedFailureMessage

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   *
   * @return the passed in <code>bool.rawMidSentenceFailureMessage</code>
   */
  def rawMidSentenceNegatedFailureMessage: String = bool.rawMidSentenceFailureMessage

  /**
   * Arguments to construct final failure message with raw message returned from <code>rawFailureMessage</code>.
   *
   * @return the passed in <code>bool.negatedFailureMessageArgs</code>
   */
  def failureMessageArgs: IndexedSeq[Any] = bool.negatedFailureMessageArgs

  /**
   * Arguments to construct final negated failure message with raw message returned from <code>rawNegatedFailureMessage</code>.
   *
   * @return the passed in <code>bool.failureMessageArgs</code>
   */
  def negatedFailureMessageArgs: IndexedSeq[Any] = bool.failureMessageArgs

  /**
   * Arguments to construct final mid sentence failure message with raw message returned from <code>rawMidSentenceFailureMessage</code>.
   *
   * @return the passed in <code>bool.midSentenceNegatedFailureMessageArgs</code>
   */
  def midSentenceFailureMessageArgs: IndexedSeq[Any] = bool.midSentenceNegatedFailureMessageArgs

  /**
   * Arguments to construct final negated mid sentence failure message with raw message returned from <code>rawMidSentenceNegatedFailureMessage</code>.
   *
   * @return the passed in <code>bool.midSentenceFailureMessageArgs</code>
   */
  def midSentenceNegatedFailureMessageArgs: IndexedSeq[Any] = bool.midSentenceFailureMessageArgs

}

/**
 * Simple macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap an unrecognized <code>Boolean</code> expression.
 *
 * @param expression the <code>Boolean</code> expression
 * @param expressionText the original expression text (source code)
 */
private[scalactic] class SimpleMacroBool(expression: Boolean, val expressionText: String, val prettifier: Prettifier) extends Bool {

  /**
   * the <code>Boolean</code> value of this <code>Bool</code>, holding the passed in expression value.
   */
  val value: Boolean = expression

  /**
   * raw message to report a failure
   *
   * @return Localized raw string of "Expression was false" if passed in <code>expressionText</code> is empty, else "{0} was false"
   */
  def rawFailureMessage: String = if (expressionText.isEmpty) Resources.rawExpressionWasFalse else Resources.rawWasFalse

  /**
   * raw message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string of "Expression was true" if passed in <code>expressionText</code> is empty, else "{0} was true"
   */
  def rawNegatedFailureMessage: String = if (expressionText.isEmpty) Resources.rawExpressionWasTrue else Resources.rawWasTrue

  /**
   * raw mid sentence message to report a failure
   *
   * @return Localized raw string of "Expression was false" if passed in <code>expressionText</code> is empty, else "{0} was false"
   */
  def rawMidSentenceFailureMessage: String = if (expressionText.isEmpty) Resources.rawExpressionWasFalse else Resources.rawWasFalse

  /**
   * raw mid sentence message with a meaning opposite to that of the failure message
   *
   * @return Localized raw string of "Expression was true" if passed in <code>expressionText</code> is empty, else "{0} was true"
   */
  def rawMidSentenceNegatedFailureMessage: String = if (expressionText.isEmpty) Resources.rawExpressionWasTrue else Resources.rawWasTrue

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
 * Binary macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap a recognized <code>Boolean</code> expression, which includes <code>Boolean</code> expression that
 * uses <code>==</code>, <code>===</code>, <code>!=</code>, <code>!==</code>, <code>&gt;</code>, <code>&gt;=</code>, <code>&lt;</code>, <code>&lt;=</code>, <code>&&</code>,
 * <code>&</code>, <code>||</code> and <code>|</code>.
 *
 * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
 * @param operator the operator (method name) of the <code>Boolean</code> expression
 * @param right the right-hand-side (RHS) of the <code>Boolean</code> expression
 * @param expression the <code>Boolean</code> expression
 */
private[scalactic] class BinaryMacroBool(left: Any, operator: String, right: Any, expression: Boolean, val prettifier: Prettifier) extends Bool {

  /**
   * Overloaded constructor that takes a <code>Bool</code> in place of <code>Boolean</code> expression.
   *
   * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
   * @param operator the operator (method name) of the <code>Boolean</code> expression
   * @param right the right-hand-side (RHS) of the <code>Boolean</code> expression
   * @param bool the <code>Bool</code> that will provide the <code>Boolean</code> expression value with <code>bool.value</code>
   */
  def this(left: Any, operator: String, right: Any, bool: Bool, prettifier: Prettifier) =
    this(left, operator, right, bool.value, prettifier)

  private lazy val prettyPair: PrettyPair = {
    val leftValue =
      left match {
        case aEqualizer: org.scalactic.TripleEqualsSupport#Equalizer[_] => aEqualizer.leftSide
        case aEqualizer: org.scalactic.TripleEqualsSupport#CheckingEqualizer[_] => aEqualizer.leftSide
        case _ => left
      }
    prettifier.apply(leftValue, right)
  }

  override lazy val analysis: scala.collection.immutable.IndexedSeq[String] = {
    operator match {
      case "==" | "===" => prettyPair.analysis.map(Vector(_)).getOrElse(Vector.empty)
      case _ => Vector.empty
    }
  }

  /**
   * the <code>Boolean</code> value of this <code>Bool</code>.
   */
  val value: Boolean = expression

  private def getObjectsForFailureMessage =
    left match {
      case aEqualizer: org.scalactic.TripleEqualsSupport#Equalizer[_] =>
        Prettifier.getObjectsForFailureMessage(aEqualizer.leftSide, right)
      case aEqualizer: org.scalactic.TripleEqualsSupport#CheckingEqualizer[_] =>
        Prettifier.getObjectsForFailureMessage(aEqualizer.leftSide, right)
      case _ => Prettifier.getObjectsForFailureMessage(left, right)
    }

  /**
   * raw message to report a failure, this method implementation will return the friendly raw message based on the passed
   * in <code>operator</code>.
   *
   * @return Localized friendly raw message based on the passed in <code>operator</code>
   */
  def rawFailureMessage: String = {
    operator match {
      case "==" => Resources.rawDidNotEqual
      case "===" => Resources.rawDidNotEqual
      case "!=" => Resources.rawEqualed
      case "!==" => Resources.rawEqualed
      case ">" => Resources.rawWasNotGreaterThan
      case ">=" => Resources.rawWasNotGreaterThanOrEqualTo
      case "<" => Resources.rawWasNotLessThan
      case "<=" => Resources.rawWasNotLessThanOrEqualTo
      case "startsWith" => Resources.rawDidNotStartWith
      case "endsWith" => Resources.rawDidNotEndWith
      case "contains" =>
        left match {
          case leftMap: scala.collection.GenMap[_, _] => Resources.rawDidNotContainKey
          case _ => Resources.rawDidNotContain
        }
      case "eq" => Resources.rawWasNotTheSameInstanceAs
      case "ne" => Resources.rawWasTheSameInstanceAs
      case "&&" | "&" =>
        (left, right) match {
          case (leftBool: Bool, rightBool: Bool) =>
            if (leftBool.value)
              Resources.rawCommaBut
            else
              leftBool.rawFailureMessage
          case (leftBool: Bool, rightAny: Any) =>
            if (leftBool.value)
              Resources.rawCommaBut
            else
              leftBool.rawFailureMessage
          case _ =>
            Resources.rawCommaBut
        }
      case "||" | "|" => Resources.rawCommaAnd
      case _ => Resources.rawExpressionWasFalse
    }
  }

  /**
   * raw message with a meaning opposite to that of the failure message, this method implementation will return the
   * friendly raw message based on the passed in <code>operator</code>.
   *
   * @return Localized negated friendly raw message based on the passed in <code>operator</code>
   */
  def rawNegatedFailureMessage: String =
    operator match {
      case "==" => Resources.rawEqualed
      case "===" => Resources.rawEqualed
      case "!=" => Resources.rawDidNotEqual
      case "!==" => Resources.rawDidNotEqual
      case ">" => Resources.rawWasGreaterThan
      case ">=" => Resources.rawWasGreaterThanOrEqualTo
      case "<" => Resources.rawWasLessThan
      case "<=" => Resources.rawWasLessThanOrEqualTo
      case "startsWith" => Resources.rawStartedWith
      case "endsWith" => Resources.rawEndedWith
      case "contains" =>
        left match {
          case leftMap: scala.collection.GenMap[_, _] => Resources.rawContainedKey
          case _ => Resources.rawContained
        }
      case "eq" => Resources.rawWasTheSameInstanceAs
      case "ne" => Resources.rawWasNotTheSameInstanceAs
      case "&&" | "&" => Resources.rawCommaAnd
      case "||" | "|" => Resources.rawCommaAnd
      case _ => Resources.rawExpressionWasTrue
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
      case "startsWith" | "endsWith" | "contains" | "eq" | "ne" =>
        Vector(left, right)
      case "&&" | "&" =>
        (left, right) match {
          case (leftBool: Bool, rightBool: Bool) =>
            if (leftBool.value)
              Vector(UnquotedString(leftBool.negatedFailureMessage), UnquotedString(rightBool.midSentenceFailureMessage))
            else
              leftBool.failureMessageArgs
          case (leftBool: Bool, rightAny: Any) =>
            if (leftBool.value)
              Vector(UnquotedString(leftBool.negatedFailureMessage), rightAny)
            else
              leftBool.failureMessageArgs
          case (leftAny: Any, rightBool: Bool) =>
            Vector(leftAny, UnquotedString(if (rightBool.value) rightBool.midSentenceNegatedFailureMessage else rightBool.midSentenceFailureMessage))
          case _ =>
            Vector(left, right)
        }
      case "||" | "|" =>
        (left, right) match {
          case (leftBool: Bool, rightBool: Bool) =>
            Vector(UnquotedString(leftBool.failureMessage), UnquotedString(rightBool.midSentenceFailureMessage))
          case (leftBool: Bool, rightAny: Any) =>
            Vector(UnquotedString(leftBool.failureMessage), rightAny)
          case (leftAny: Any, rightBool: Bool) =>
            Vector(leftAny, UnquotedString(rightBool.midSentenceFailureMessage))
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
      case "startsWith" | "endsWith" | "contains" | "eq" | "ne" =>
        Vector(left, right)
      case "&&" | "&" =>
        (left, right) match {
          case (leftBool: Bool, rightBool: Bool) =>
            Vector(
              UnquotedString(if (leftBool.value) leftBool.negatedFailureMessage else leftBool.failureMessage),
              UnquotedString(if (rightBool.value) rightBool.midSentenceNegatedFailureMessage else rightBool.midSentenceFailureMessage)
            )
          case (leftBool: Bool, rightAny: Any) =>
            Vector(UnquotedString(if (leftBool.value) leftBool.negatedFailureMessage else leftBool.failureMessage), rightAny)
          case (leftAny: Any, rightBool: Bool) =>
            Vector(leftAny, UnquotedString(if (rightBool.value) rightBool.midSentenceNegatedFailureMessage else rightBool.negatedFailureMessage))
          case _ =>
            Vector(left, right)
        }
      case "||" | "|" =>
        (left, right) match {
          case (leftBool: Bool, rightBool: Bool) =>
            Vector(
              UnquotedString(if (leftBool.value) leftBool.negatedFailureMessage else leftBool.failureMessage),
              UnquotedString(if (rightBool.value) rightBool.midSentenceNegatedFailureMessage else rightBool.midSentenceFailureMessage)
            )
          case (leftBool: Bool, rightAny: Any) =>
            Vector(UnquotedString(if (leftBool.value) leftBool.negatedFailureMessage else leftBool.failureMessage), rightAny)
          case (leftAny: Any, rightBool: Bool) =>
            Vector(leftAny, UnquotedString(if (rightBool.value) rightBool.midSentenceNegatedFailureMessage else rightBool.midSentenceFailureMessage))
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

/**
 * Unary macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap a recognized <code>Boolean</code> expression represents a unary method call, which includes
 * <code>Boolean</code> expression that uses <code>isEmpty</code>.
 *
 * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
 * @param operator the operator (method name) of the <code>Boolean</code> expression
 * @param expression the <code>Boolean</code> expression
 */
private[scalactic] class UnaryMacroBool(left: Any, operator: String, expression: Boolean, val prettifier: Prettifier) extends Bool {

  /**
   * the <code>Boolean</code> value of this <code>Bool</code>.
   */
  val value: Boolean = expression

  /**
   * raw message to report a failure, this method implementation will return the friendly raw message based on the passed
   * in <code>operator</code>.
   *
   * @return Localized friendly raw message based on the passed in <code>operator</code>
   */
  def rawFailureMessage: String = {
    operator match {
      case "isEmpty" => Resources.rawWasNotEmpty
      case "nonEmpty" => Resources.rawWasEmpty
      case _ => Resources.rawExpressionWasFalse
    }
  }

  /**
   * raw message with a meaning opposite to that of the failure message, this method implementation will return the
   * friendly raw message based on the passed in <code>operator</code>.
   *
   * @return Localized negated friendly raw message based on the passed in <code>operator</code>
   */
  def rawNegatedFailureMessage: String =
    operator match {
      case "isEmpty" => Resources.rawWasEmpty
      case "nonEmpty" => Resources.rawWasNotEmpty
      case _ => Resources.rawExpressionWasTrue
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
      case "isEmpty" | "nonEmpty" =>
        Vector(left)
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
      case "isEmpty" | "nonEmpty" =>
        Vector(left)
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

/**
 * Macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap a recognized <code>Boolean</code> expression
 * that represents a <code>isInstanceOf</code> method call.
 *
 * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
 * @param operator the operator (method name) of the <code>Boolean</code> expression
 * @param className the class name passed to <code>isInstanceOf</code> method call
 * @param expression the <code>Boolean</code> expression
 */
private[scalactic] class IsInstanceOfMacroBool(left: Any, operator: String, className: String, expression: Boolean, val prettifier: Prettifier) extends Bool {

  /**
   * the <code>Boolean</code> value of this <code>Bool</code>.
   */
  val value: Boolean = expression

  /**
   * raw message to report a failure, this method implementation will return the friendly raw message based on the passed
   * in <code>operator</code>.
   *
   * @return Localized friendly raw message based on the passed in <code>operator</code>
   */
  def rawFailureMessage: String = {
    operator match {
      case "isInstanceOf" => Resources.rawWasNotInstanceOf
      case _ => Resources.rawExpressionWasFalse
    }
  }

  /**
   * raw message with a meaning opposite to that of the failure message, this method implementation will return the
   * friendly raw message based on the passed in <code>operator</code>.
   *
   * @return Localized negated friendly raw message based on the passed in <code>operator</code>
   */
  def rawNegatedFailureMessage: String =
    operator match {
      case "isInstanceOf" => Resources.rawWasInstanceOf
      case _ => Resources.rawExpressionWasTrue
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
      case "isInstanceOf" =>
        Vector(left, UnquotedString(className))
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
      case "isInstanceOf" =>
        Vector(left, UnquotedString(className))
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

/**
 * Macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap a recognized <code>Boolean</code> expression
 * that represents a <code>length</code> or <code>size</code> method call.
 *
 * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
 * @param operator the operator (method name) of the <code>Boolean</code> expression
 * @param actual the actual length or size of <code>left</code>
 * @param expected the expected length or size of <code>left</code>
 */
private[scalactic] class LengthSizeMacroBool(left: Any, operator: String, actual: Any, expected: Any, val prettifier: Prettifier) extends Bool {

  /**
   * the <code>Boolean</code> value of this <code>Bool</code>.
   */
  val value: Boolean = actual == expected

  /**
   * raw message to report a failure, this method implementation will return the friendly raw message based on the passed
   * in <code>operator</code>.
   *
   * @return Localized friendly raw message based on the passed in <code>operator</code>
   */
  def rawFailureMessage: String = {
    operator match {
      case "length" => Resources.rawHadLengthInsteadOfExpectedLength
      case "size" => Resources.rawHadSizeInsteadOfExpectedSize
      case _ => Resources.rawExpressionWasFalse
    }
  }

  /**
   * raw message with a meaning opposite to that of the failure message, this method implementation will return the
   * friendly raw message based on the passed in <code>operator</code>.
   *
   * @return Localized negated friendly raw message based on the passed in <code>operator</code>
   */
  def rawNegatedFailureMessage: String =
    operator match {
      case "length" => Resources.rawHadLength
      case "size" => Resources.rawHadSize
      case _ => Resources.rawExpressionWasTrue
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
      case "length" | "size" =>
        Vector(left, actual, expected)
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
      case "length" | "size" =>
        Vector(left, actual)
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

/**
 * Macro <code>Bool</code> that is used by <code>BooleanMacro</code> to wrap a recognized <code>Boolean</code> expression
 * that represents a <code>length</code> or <code>size</code> method call.
 *
 * @param left the left-hand-side (LHS) of the <code>Boolean</code> expression
 * @param right the right-hand-side (RHS) of the <code>Boolean</code> expression
 * @param expression the <code>Boolean</code> expression
 */
private[scalactic] class ExistsMacroBool(left: Any, right: Any, expression: Boolean, val prettifier: Prettifier) extends Bool {

  /**
   * the <code>Boolean</code> value of this <code>Bool</code>.
   */
  val value: Boolean = expression

  /**
   * raw message to report a failure, this method implementation will return localized "x did not contain y" message.
   *
   * @return Localized friendly raw message based on the passed in <code>operator</code>
   */
  def rawFailureMessage: String =
    Resources.rawDidNotContain

  /**
   * raw message with a meaning opposite to that of the failure message, this method implementation will return localized
   * "x contained y" message.
   *
   * @return Localized negated friendly raw message based on the passed in <code>operator</code>
   */
  def rawNegatedFailureMessage: String =
    Resources.rawContained

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
   * Arguments to construct final failure message with raw message returned from <code>rawFailureMessage</code>.  This
   * implementation will return a <code>Vector</code> that contains the passed in <code>left</code> and <code>right</code>.
   *
   * @return Vector that contains arguments needed by <code>rawFailureMessage</code> to construct the final friendly failure message
   */
  def failureMessageArgs: IndexedSeq[Any] =
    Vector(left, right)

  /**
   * Arguments to construct final negated failure message with raw message returned from <code>rawNegatedFailureMessage</code>.
   * This implementation will return a <code>Vector</code> that contains the passed in <code>left</code> and <code>right</code>.
   *
   * @return Vector that contains arguments needed by <code>rawNegatedFailureMessage</code> to construct the final negated friendly failure message
   */
  def negatedFailureMessageArgs: IndexedSeq[Any] =
    Vector(left, right)

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
