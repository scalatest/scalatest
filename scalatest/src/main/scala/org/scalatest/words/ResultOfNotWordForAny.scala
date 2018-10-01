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
package org.scalatest.words

import org.scalactic._
import org.scalatest.enablers._
import org.scalatest.matchers._
import java.lang.reflect.Field
import java.lang.reflect.Method
import java.lang.reflect.Modifier

import org.scalatest.Assertions.areEqualComparingArraysStructurally
import org.scalatest.MatchersHelper.andMatchersAndApply
import org.scalatest.MatchersHelper.orMatchersAndApply
import org.scalatest.MatchersHelper.transformOperatorChars

import scala.collection.GenMap
import scala.collection.GenSeq
import scala.collection.GenTraversable
import scala.collection.Traversable
import scala.reflect.Manifest
import scala.util.matching.Regex
import TripleEqualsSupport.Spread
import TripleEqualsSupport.TripleEqualsInvocation
import org.scalatest.MessageBuilder
// SKIP-SCALATESTJS-START
import org.scalatest.MatchersHelper.matchSymbolToPredicateMethod
// SKIP-SCALATESTJS-END
import org.scalatest.Assertion
import org.scalatest.MatchersHelper.endWithRegexWithGroups
import org.scalatest.MatchersHelper.fullyMatchRegexWithGroups
import org.scalatest.MatchersHelper.includeRegexWithGroups
import org.scalatest.MatchersHelper.indicateFailure
import org.scalatest.MatchersHelper.indicateSuccess
import org.scalatest.MatchersHelper.newTestFailedException
import org.scalatest.MatchersHelper.startWithRegexWithGroups
import org.scalatest.Succeeded
import org.scalatest.{FailureMessages, UnquotedString}
import scala.annotation.tailrec
import org.scalatest.exceptions.NotAllowedException

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfNotWordForAny[T](val left: T, val shouldBeTrue: Boolean, val prettifier: Prettifier, val pos: source.Position) {

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not equal (7)
   *                   ^
   * </pre>
   **/
  def equal(right: Any)(implicit equality: Equality[T]): Assertion = {
    if (equality.areEqual(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotEqual), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.equaled), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.equaled(prettifier, left, right), FailureMessages.didNotEqual(prettifier, left, right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be (7)
   *                   ^
   * </pre>
   **/
  def be(right: Any): Assertion = {
    if ((left == right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.wasNotEqualTo), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.wasEqualTo), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasEqualTo(prettifier, left, right), FailureMessages.wasNotEqualTo(prettifier, left, right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be &lt;= (7)
   *                   ^
   * </pre>
   **/
  def be(comparison: ResultOfLessThanOrEqualToComparison[T]): Assertion = {
    if (comparison(left) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, comparison.right, FailureMessages.wasNotLessThanOrEqualTo), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, comparison.right, FailureMessages.wasLessThanOrEqualTo), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasLessThanOrEqualTo(prettifier, left, comparison.right), FailureMessages.wasNotLessThanOrEqualTo(prettifier, left, comparison.right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be &gt;= (7)
   *                   ^
   * </pre>
   **/
  def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]): Assertion = {
    if (comparison(left) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, comparison.right, FailureMessages.wasNotGreaterThanOrEqualTo), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, comparison.right, FailureMessages.wasGreaterThanOrEqualTo), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasGreaterThanOrEqualTo(prettifier, left, comparison.right), FailureMessages.wasNotGreaterThanOrEqualTo(prettifier, left, comparison.right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be &lt; (7)
   *                   ^
   * </pre>
   **/
  def be(comparison: ResultOfLessThanComparison[T]): Assertion = {
    if (comparison(left) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, comparison.right, FailureMessages.wasNotLessThan), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, comparison.right, FailureMessages.wasLessThan), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasLessThan(prettifier, left, comparison.right), FailureMessages.wasNotLessThan(prettifier, left, comparison.right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be &gt; (7)
   *                   ^
   * </pre>
   **/
  def be(comparison: ResultOfGreaterThanComparison[T]): Assertion = {
    if (comparison(left) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, comparison.right, FailureMessages.wasNotGreaterThan), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, comparison.right, FailureMessages.wasGreaterThan), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasGreaterThan(prettifier, left, comparison.right), FailureMessages.wasNotGreaterThan(prettifier, left, comparison.right))
  }

  /**
   * <strong>
   * The deprecation period for the "be ===" syntax has expired, and the syntax 
   * will now throw <code>NotAllowedException</code>.  Please use should equal, should ===, shouldEqual,
   * should be, or shouldBe instead.
   * </strong>
   *
   * <p>
   * Note: usually syntax will be removed after its deprecation period. This was left in because otherwise the syntax could in some
   * cases still compile, but silently wouldn't work.
   * </p>
   */
  @deprecated("The deprecation period for the be === syntax has expired. Please use should equal, should ===, shouldEqual, should be, or shouldBe instead.")
  def be(comparison: TripleEqualsInvocation[_]): Matcher[Any] = {
    throw new NotAllowedException(FailureMessages.beTripleEqualsNotAllowed, pos)
  }

  /**
   * This method enables the following syntax, where <code>odd</code> refers to
   * a <code>BeMatcher[Int]</code>:
   *
   * <pre class="stHighlight">
   * 2 should not be (odd)
   *              ^
   * </pre>
   **/
  def be(beMatcher: BeMatcher[T]): Assertion = {
    val result = beMatcher(left)
    if (result.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, result.failureMessage(_)), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, result.negatedFailureMessage(_)), None, pos)
    else
      indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
  }
  
  /**
   * This method enables the following syntax, where <code>positiveNumber</code> refers to
   * an <code>AMatcher[Int]</code>:
   *
   * <pre class="stHighlight">
   * 2 should not be a (positiveNumber)
   *              ^
   * </pre>
   **/
  def be(resultOfAWordToAMatcherApplication: ResultOfAWordToAMatcherApplication[T]): Assertion = {
    val aMatcher = resultOfAWordToAMatcherApplication.aMatcher
    val result = aMatcher(left)
    if (result.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, result.failureMessage(_)), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, result.negatedFailureMessage(_)), None, pos)
    else
      indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
  }
  
  /**
   * This method enables the following syntax, where <code>oddNumber</code> refers to
   * an <code>AnMatcher[Int]</code>:
   *
   * <pre class="stHighlight">
   * 2 should not be an (oddNumber)
   *              ^
   * </pre>
   **/
  def be(resultOfAnWordToAnMatcherApplication: ResultOfAnWordToAnMatcherApplication[T]): Assertion = {
    val anMatcher = resultOfAnWordToAnMatcherApplication.anMatcher
    val result = anMatcher(left)
    if (result.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, result.failureMessage(_)), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, result.negatedFailureMessage(_)), None, pos)
    else
      indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
  }

  import scala.language.experimental.macros
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be a [Book]
   *                   ^
   * </pre>
   **/
  def be(aType: ResultOfATypeInvocation[_]): Assertion = macro TypeMatcherMacro.assertATypeShouldBeTrueImpl
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be a [Book]
   *                   ^
   * </pre>
   **/
  def be(anType: ResultOfAnTypeInvocation[_]): Assertion = macro TypeMatcherMacro.assertAnTypeShouldBeTrueImpl

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * sevenDotOh should not be (6.5 +- 0.2)
   *                       ^
   * </pre>
   **/
  def be(spread: Spread[T]): Assertion = {
    if (spread.isWithin(left) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, spread.pivot, spread.tolerance, FailureMessages.wasNotPlusOrMinus), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, spread.pivot, spread.tolerance, FailureMessages.wasPlusOrMinus), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasPlusOrMinus(prettifier, left, spread.pivot, spread.tolerance), FailureMessages.wasNotPlusOrMinus(prettifier, left, spread.pivot, spread.tolerance))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * partialFun should not be definedAt ("apple")
   *                       ^
   * </pre>
   **/
  def be[U](resultOfDefinedAt: ResultOfDefinedAt[U])(implicit ev: T <:< PartialFunction[U, _]): Assertion = {
    if (left.isDefinedAt(resultOfDefinedAt.right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, resultOfDefinedAt.right, FailureMessages.wasNotDefinedAt), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, resultOfDefinedAt.right, FailureMessages.wasDefinedAt), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasDefinedAt(prettifier, left, resultOfDefinedAt.right), FailureMessages.wasNotDefinedAt(prettifier, left, resultOfDefinedAt.right))
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * sevenDotOh should not equal (6.5 +- 0.2)
   *                       ^
   * </pre>
   **/
  def equal(spread: Spread[T]): Assertion = {
    if (spread.isWithin(left) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, spread.pivot, spread.tolerance, FailureMessages.didNotEqualPlusOrMinus), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, spread.pivot, spread.tolerance, FailureMessages.equaledPlusOrMinus), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.equaledPlusOrMinus(prettifier, left, spread.pivot, spread.tolerance), FailureMessages.didNotEqualPlusOrMinus(prettifier, left, spread.pivot, spread.tolerance))
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should not equal null
   *                   ^
   * </pre>
   **/
  def equal(right: Null): Assertion = {
    if ((left == null) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.didNotEqualNull), None, pos)
      else
        indicateFailure(MessageBuilder.of(FailureMessages.equaledNull), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.equaledNull, FailureMessages.didNotEqualNull(prettifier, left))
  }

  /**
   * Enables parentheses to be placed around <code>length (N)</code> in expressions of the
   * form: <code>should not have (length (N))</code>.
   */
  def have(resultOfLengthWordApplication: ResultOfLengthWordApplication)(implicit len: Length[T]): Assertion = {
    val right = resultOfLengthWordApplication.expectedLength
    val leftLength = len.lengthOf(left)
    if ((leftLength == right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, leftLength, right, FailureMessages.hadLengthInsteadOfExpectedLength), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.hadLength), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.hadLength(prettifier, left, right), FailureMessages.hadLengthInsteadOfExpectedLength(prettifier, left, leftLength, right))
  }

  /**
   * Enables parentheses to be placed around <code>size (N)</code> in expressions of the
   * form: <code>should not have (size (N))</code>.
   */
  def have(resultOfSizeWordApplication: ResultOfSizeWordApplication)(implicit sz: Size[T]): Assertion = {
    val right = resultOfSizeWordApplication.expectedSize
    val leftSize = sz.sizeOf(left)
    if ((leftSize == right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, leftSize, right, FailureMessages.hadSizeInsteadOfExpectedSize), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.hadSize), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.hadSize(prettifier, left, right), FailureMessages.hadSizeInsteadOfExpectedSize(prettifier, left, leftSize, right))
  }

  // TODO: See about putting U <: T back in here, now that I got rid of the implicit conversion
  // that required me to get rid of the U and just use T. The idea is if I have a phoneBook should have (...)
  // that I could pass HavePropertyMatchers for any supertype of PhoneBook. I could use HavePropertyMatcher[Book]s
  // for example. So write a test and see if that doesn't work, and then if not, go ahead and put the U back.
  // Actually because the T is contravariant, a HavePropertyMatcher[Book] is already a subtype of HavePropertyMatcher[PhoneBook]
  // So I don't need the U <: T anyway. But maybe test to verify this would be worthwhile.
  // The type parameter U has T as its lower bound, which means that U must be T or a supertype of T. Left is T, oh, because
  // HavePropertyMatcher is contravariant in its type parameter T, and that nmakes sense, because a HavePropertyMatcher of Any should
  // be able to match on a String.
  // <code>not have (a (1), b (2))</code> must mean the opposite of <code>have (a (1), b (2))</code>, which means that 
  // <code>not have (a (1), b (2))</code> will be true if either <code>(a (1)).matches</code> or <code>(b (1)).matches</code> is false.
  // Only if both <code>(a (1)).matches</code> or <code>(b (1)).matches</code> are true will <code>not have (a (1), b (2))</code> be false.
  // title/author matches | have | have not
  // 0 0 | 0 | 1 
  // 0 1 | 0 | 1
  // 1 0 | 0 | 1
  // 1 1 | 1 | 0
  // 
  /**
   * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
   * <code>title ("One Hundred Years of Solitude")</code> results in a <code>HavePropertyMatcher[Book]</code>:
   *
   * <pre class="stHighlight">
   * book should not have (title ("One Hundred Years of Solitude"))
   *                 ^
   * </pre>
   **/
  def have(firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*): Assertion = {

    val results =
      for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
        propertyVerifier(left)

    val firstFailureOption = results.find(pv => !pv.matches)

    val justOneProperty = propertyMatchers.isEmpty

    // if shouldBeTrue is false, then it is like "not have ()", and should throw TFE if firstFailureOption.isDefined is false
    // if shouldBeTrue is true, then it is like "not (not have ()), which should behave like have ()", and should throw TFE if firstFailureOption.isDefined is true
    if (firstFailureOption.isDefined == shouldBeTrue) {
      firstFailureOption match {
        case Some(firstFailure) =>
          // This is one of these cases, thus will only get here if shouldBeTrue is true
          // 0 0 | 0 | 1
          // 0 1 | 0 | 1
          // 1 0 | 0 | 1
          indicateFailure(
            MessageBuilder.of(prettifier, UnquotedString(firstFailure.propertyName), firstFailure.expectedValue, firstFailure.actualValue, left, FailureMessages.propertyDidNotHaveExpectedValue),
            None,
            pos
          )
        case None =>
          // This is this cases, thus will only get here if shouldBeTrue is false
          // 1 1 | 1 | 0
          val failureMessage =
            if (justOneProperty) {
              val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
              FailureMessages.propertyHadExpectedValue(prettifier,
                UnquotedString(firstPropertyResult.propertyName),
                firstPropertyResult.expectedValue,
                left
              )
            }
            else FailureMessages.allPropertiesHadExpectedValues(prettifier, left)

          indicateFailure(MessageBuilder.of(failureMessage), None, pos)
      } 
    }
    else {
      indicateSuccess(
        if (shouldBeTrue)
          FailureMessages.allPropertiesHadExpectedValues(prettifier, left)
        else {
          firstFailureOption match {
            case Some(firstFailure) =>
              // This is one of these cases, thus will only get here if shouldBeTrue is true
              // 0 0 | 0 | 1
              // 0 1 | 0 | 1
              // 1 0 | 0 | 1
              FailureMessages.propertyDidNotHaveExpectedValue(prettifier,
                UnquotedString(firstFailure.propertyName),
                firstFailure.expectedValue,
                firstFailure.actualValue,
                left
              )

            case None =>
              // This is this cases, thus will only get here if shouldBeTrue is false
              // 1 1 | 1 | 0
              val failureMessage =
                if (justOneProperty) {
                  val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                  FailureMessages.propertyHadExpectedValue(prettifier,
                    UnquotedString(firstPropertyResult.propertyName),
                    firstPropertyResult.expectedValue,
                    left
                  )
                }
                else FailureMessages.allPropertiesHadExpectedValues(prettifier, left)

              failureMessage
          }
        }
      )
    }
  }
  
  def have(resultOfMessageWordApplication: ResultOfMessageWordApplication)(implicit messaging: Messaging[T]): Assertion = {
    val right = resultOfMessageWordApplication.expectedMessage
    val actualMessage = messaging.messageOf(left)
    if ((actualMessage == right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, actualMessage, right, FailureMessages.hadMessageInsteadOfExpectedMessage), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.hadExpectedMessage), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.hadExpectedMessage(prettifier, left, right), FailureMessages.hadMessageInsteadOfExpectedMessage(prettifier, left, actualMessage, right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * object should not contain (null)
   *                   ^
   * </pre>
   **/
  def contain(nullValue: Null)(implicit containing: Containing[T]): Assertion = {
    if (containing.contains(left, null) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.didNotContainNull), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.containedNull), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedNull(prettifier, left), FailureMessages.didNotContainNull(prettifier, left))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * object should not contain ("one")
   *                   ^
   * </pre>
   **/
  def contain(expectedElement: Any)(implicit containing: Containing[T]): Assertion = {
    val right = expectedElement
    if (containing.contains(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotContainExpectedElement), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.containedExpectedElement), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedExpectedElement(prettifier, left, right), FailureMessages.didNotContainExpectedElement(prettifier, left, right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should not be (null)
   *                ^
   * </pre>
   **/
  def be(o: Null)(implicit ev: T <:< AnyRef): Assertion = {
    if ((left == null) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.wasNotNull), None, pos)
      else
        indicateFailure(MessageBuilder.of(FailureMessages.wasNull), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasNull, FailureMessages.wasNotNull(prettifier, left))
  }

  // SKIP-SCALATESTJS-START
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * stack should not be ('empty)
   *                  ^
   * </pre>
   **/
  def be(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef, prettifier: Prettifier, pos: source.Position): Assertion = {
    val matcherResult = matchSymbolToPredicateMethod(toAnyRef(left), symbol, false, false, prettifier, pos)
    if (matcherResult.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, matcherResult.failureMessage(_)), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, matcherResult.negatedFailureMessage(_)), None, pos)
    else
      indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
  }
  // SKIP-SCALATESTJS-END

  /**
   * This method enables the following syntax, where <code>stack</code> is, for example, of type <code>Stack</code> and
   * <code>empty</code> refers to a <code>BePropertyMatcher[Stack]</code>:
   *
   * <pre class="stHighlight">
   * stack should not be (empty)
   *                      ^
   * </pre>
   **/
  def be(bePropertyMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef): Assertion = {
    val result = bePropertyMatcher(left)
    if (result.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(result.propertyName), FailureMessages.wasNot), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(result.propertyName), FailureMessages.was), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.was(prettifier, left, UnquotedString(result.propertyName)), FailureMessages.wasNot(prettifier, left, UnquotedString(result.propertyName)))
  }

  // SKIP-SCALATESTJS-START
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * notFileMock should not be a ('file)
   *                        ^
   * </pre>
   **/
  def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef, prettifier: Prettifier, pos: source.Position): Assertion = {
    val matcherResult = matchSymbolToPredicateMethod(toAnyRef(left), resultOfAWordApplication.symbol, true, true, prettifier, pos)
    if (matcherResult.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, matcherResult.failureMessage(_)), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, matcherResult.negatedFailureMessage(_)), None, pos)
    else
      indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
  }
  // SKIP-SCALATESTJS-END

  /**
   * This method enables the following syntax, where <code>notFileMock</code> is, for example, of type <code>File</code> and
   * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
   *
   * <pre class="stHighlight">
   * notFileMock should not be a (file)
   *                        ^
   * </pre>
   **/
  def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {
    val result = resultOfAWordApplication.bePropertyMatcher(left)
    if (result.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(result.propertyName), FailureMessages.wasNotA), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(result.propertyName), FailureMessages.wasA), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasA(prettifier, left, UnquotedString(result.propertyName)), FailureMessages.wasNotA(prettifier, left, UnquotedString(result.propertyName)))
  }

  // SKIP-SCALATESTJS-START
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * keyEvent should not be an ('actionKey)
   *                     ^
   * </pre>
   **/
  def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef, prettifier: Prettifier, pos: source.Position): Assertion = {
    val matcherResult = matchSymbolToPredicateMethod(toAnyRef(left), resultOfAnWordApplication.symbol, true, false, prettifier, pos)
    if (matcherResult.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, matcherResult.failureMessage(_)), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, matcherResult.negatedFailureMessage(_)), None, pos)
    else
      indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
  }
  // SKIP-SCALATESTJS-END

  /**
   * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
   * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
   *
   * <pre class="stHighlight">
   * keyEvent should not be an (actionKey)
   *                     ^
   * </pre>
   **/
  def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {
    val result = resultOfAnWordApplication.bePropertyMatcher(left)
    if (result.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(result.propertyName), FailureMessages.wasNotAn), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(result.propertyName), FailureMessages.wasAn), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasAn(prettifier, left, UnquotedString(result.propertyName)), FailureMessages.wasNotAn(prettifier, left, UnquotedString(result.propertyName)))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * otherString should not be theSameInstanceAs (string)
   *                        ^
   * </pre>
   **/
  def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    if ((resultOfSameInstanceAsApplication.right eq toAnyRef(left)) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, resultOfSameInstanceAsApplication.right, FailureMessages.wasNotSameInstanceAs), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, resultOfSameInstanceAsApplication.right, FailureMessages.wasSameInstanceAs), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasSameInstanceAs(prettifier, left, resultOfSameInstanceAsApplication.right), FailureMessages.wasNotSameInstanceAs(prettifier, left, resultOfSameInstanceAsApplication.right))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should not be sorted
   *                  ^
   * </pre>
   **/
  def be[U](sortedWord: SortedWord)(implicit sortable: Sortable[T]): Assertion = {
    if (sortable.isSorted(left) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.wasNotSorted), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.wasSorted), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasSorted(prettifier, left), FailureMessages.wasNotSorted(prettifier, left))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * file should not be readable
   *                    ^
   * </pre>
   **/
  def be[U](readableWord: ReadableWord)(implicit readability: Readability[T]): Assertion = {
    if (readability.isReadable(left) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.wasNotReadable), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.wasReadable), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasReadable(prettifier, left), FailureMessages.wasNotReadable(prettifier, left))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * file should not be writable
   *                    ^
   * </pre>
   **/
  def be[U](writableWord: WritableWord)(implicit writability: Writability[T]): Assertion = {
    if (writability.isWritable(left) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.wasNotWritable), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.wasWritable), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasWritable(prettifier, left), FailureMessages.wasNotWritable(prettifier, left))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * list should not be empty
   *                    ^
   * </pre>
   **/
  def be[U](emptyWord: EmptyWord)(implicit emptiness: Emptiness[T]): Assertion = {
    if (emptiness.isEmpty(left) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.wasNotEmpty), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.wasEmpty), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasEmpty(prettifier, left), FailureMessages.wasNotEmpty(prettifier, left))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * option should not be defined
   *                      ^
   * </pre>
   **/
  def be[U](definedWord: DefinedWord)(implicit definition: Definition[T]): Assertion = {
    if (definition.isDefined(left) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.wasNotDefined), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, FailureMessages.wasDefined), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasDefined(prettifier, left), FailureMessages.wasNotDefined(prettifier, left))
  }

  def contain(newOneOf: ResultOfOneOfApplication)(implicit containing: Containing[T]): Assertion = {

    val right = newOneOf.right

    if (containing.containsOneOf(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.didNotContainOneOfElements), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.containedOneOfElements), None, pos)
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedOneOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainOneOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  def contain(oneElementOf: ResultOfOneElementOfApplication)(implicit evidence: Containing[T]): Assertion = {

    val right = oneElementOf.right

    if (evidence.containsOneOf(left, right.distinct) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotContainOneElementOf), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.containedOneElementOf), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedOneElementOf(prettifier, left, right), FailureMessages.didNotContainOneElementOf(prettifier, left, right))
  }

  def contain(atLeastOneOf: ResultOfAtLeastOneOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = atLeastOneOf.right

    if (aggregating.containsAtLeastOneOf(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.didNotContainAtLeastOneOf), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.containedAtLeastOneOf), None, pos)
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  def contain(atLeastOneElementOf: ResultOfAtLeastOneElementOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = atLeastOneElementOf.right

    if (aggregating.containsAtLeastOneOf(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotContainAtLeastOneElementOf), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.containedAtLeastOneElementOf), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedAtLeastOneElementOf(prettifier, left, right), FailureMessages.didNotContainAtLeastOneElementOf(prettifier, left, right))
  }

  def contain(noneOf: ResultOfNoneOfApplication)(implicit containing: Containing[T]): Assertion = {

    val right = noneOf.right

    if (containing.containsNoneOf(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.containedAtLeastOneOf), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.didNotContainAtLeastOneOf), None, pos)
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.didNotContainAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.containedAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  def contain(noElementsOf: ResultOfNoElementsOfApplication)(implicit containing: Containing[T]): Assertion = {

    val right = noElementsOf.right

    if (containing.containsNoneOf(left, right.distinct) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.containedAtLeastOneElementOf), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotContainAtLeastOneElementOf), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, left, right), FailureMessages.containedAtLeastOneElementOf(prettifier, left, right))
  }
  
  def contain(theSameElementsAs: ResultOfTheSameElementsAsApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = theSameElementsAs.right

    if (aggregating.containsTheSameElementsAs(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotContainSameElements), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.containedSameElements), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedSameElements(prettifier, left, right), FailureMessages.didNotContainSameElements(prettifier, left, right))
  }

  def contain(theSameElementsInOrderAs: ResultOfTheSameElementsInOrderAsApplication)(implicit sequencing: Sequencing[T]): Assertion = {

    val right = theSameElementsInOrderAs.right

    if (sequencing.containsTheSameElementsInOrderAs(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotContainSameElementsInOrder), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.containedSameElementsInOrder), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedSameElementsInOrder(prettifier, left, right), FailureMessages.didNotContainSameElementsInOrder(prettifier, left, right))
  }
  
  def contain(only: ResultOfOnlyApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    val right = only.right
    val withFriendlyReminder = right.size == 1 && (right(0).isInstanceOf[scala.collection.GenTraversable[_]] || right(0).isInstanceOf[Every[_]])
    if (aggregating.containsOnly(left, right) != shouldBeTrue) {
      if (shouldBeTrue)
        if (withFriendlyReminder)
          indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.didNotContainOnlyElementsWithFriendlyReminder), None, pos)
        else
          indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.didNotContainOnlyElements), None, pos)
      else
        if (withFriendlyReminder)
          indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.containedOnlyElementsWithFriendlyReminder), None, pos)
        else
          indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.containedOnlyElements), None, pos)
    }
    else
      indicateSuccess(
        if (shouldBeTrue)
          if (withFriendlyReminder)
            FailureMessages.containedOnlyElementsWithFriendlyReminder(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          else
            FailureMessages.containedOnlyElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        else
          if (withFriendlyReminder)
            FailureMessages.didNotContainOnlyElementsWithFriendlyReminder(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          else
            FailureMessages.didNotContainOnlyElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  def contain(only: ResultOfInOrderOnlyApplication)(implicit sequencing: Sequencing[T]): Assertion = {
    val right = only.right
    if (sequencing.containsInOrderOnly(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.didNotContainInOrderOnlyElements), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.containedInOrderOnlyElements), None, pos)
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedInOrderOnlyElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainInOrderOnlyElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }
  
  def contain(allOf: ResultOfAllOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = allOf.right
    if (aggregating.containsAllOf(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.didNotContainAllOfElements), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.containedAllOfElements), None, pos)
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAllOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainAllOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  def contain(allElementsOf: ResultOfAllElementsOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = allElementsOf.right
    if (aggregating.containsAllOf(left, right.distinct) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotContainAllElementsOf), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.containedAllElementsOf), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedAllElementsOf(prettifier, left, right), FailureMessages.didNotContainAllElementsOf(prettifier, left, right))
  }
  
  def contain(only: ResultOfInOrderApplication)(implicit sequencing: Sequencing[T]): Assertion = {

    val right = only.right
    if (sequencing.containsInOrder(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.didNotContainAllOfElementsInOrder), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.containedAllOfElementsInOrder), None, pos)
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAllOfElementsInOrder(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainAllOfElementsInOrder(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  def contain(only: ResultOfInOrderElementsOfApplication)(implicit sequencing: Sequencing[T]): Assertion = {

    val right = only.right
    if (sequencing.containsInOrder(left, right.distinct) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotContainAllElementsOfInOrder), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.containedAllElementsOfInOrder), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedAllElementsOfInOrder(prettifier, left, right), FailureMessages.didNotContainAllElementsOfInOrder(prettifier, left, right))
  }
  
  def contain(atMostOneOf: ResultOfAtMostOneOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = atMostOneOf.right

    if (aggregating.containsAtMostOneOf(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.didNotContainAtMostOneOf), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")), FailureMessages.containedAtMostOneOf), None, pos)
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAtMostOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainAtMostOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  def contain(atMostOneElementOf: ResultOfAtMostOneElementOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = atMostOneElementOf.right

    if (aggregating.containsAtMostOneOf(left, right.distinct) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotContainAtMostOneElementOf), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.containedAtMostOneElementOf), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedAtMostOneElementOf(prettifier, left, right), FailureMessages.didNotContainAtMostOneElementOf(prettifier, left, right))
  }

  def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication)(implicit keyMapping: KeyMapping[T]): Assertion = {
    val right = resultOfKeyWordApplication.expectedKey
    if (keyMapping.containsKey(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotContainKey), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.containedKey), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedKey(prettifier, left, right), FailureMessages.didNotContainKey(prettifier, left, right))
  }

  def contain(resultOfValueWordApplication: ResultOfValueWordApplication)(implicit valueMapping: ValueMapping[T]): Assertion = {
    val right = resultOfValueWordApplication.expectedValue
    if (valueMapping.containsValue(left, right) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.didNotContainValue), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, right, FailureMessages.containedValue), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedValue(prettifier, left, right), FailureMessages.didNotContainValue(prettifier, left, right))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
   *                   ^
   * </pre>
   *
   * <p>
   * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
   * or a <code>scala.util.matching.Regex</code>.
   * </p>
   */
  def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    val result = fullyMatchRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, result.failureMessage(_)), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, result.negatedFailureMessage(_)), None, pos)
    else
      indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should not include regex ("wo.ld")
   *                   ^
   * </pre>
   *
   * <p>
   * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
   * or a <code>scala.util.matching.Regex</code>.
   * </p>
   */
  def include(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    val result = includeRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, result.failureMessage(_)), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, result.negatedFailureMessage(_)), None, pos)
    else
      indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should not include ("world")
   *                   ^
   * </pre>
   **/
  def include(expectedSubstring: String)(implicit ev: T <:< String): Assertion = {
    if ((left.indexOf(expectedSubstring) >= 0) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, expectedSubstring, FailureMessages.didNotIncludeSubstring), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, expectedSubstring, FailureMessages.includedSubstring), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.includedSubstring(prettifier, left, expectedSubstring), FailureMessages.didNotIncludeSubstring(prettifier, left, expectedSubstring))
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should not startWith regex ("Hel*o")
   *                   ^
   * </pre>
   *
   * <p>
   * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
   * or a <code>scala.util.matching.Regex</code>.
   * </p>
   */
  def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    val result = startWithRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, result.failureMessage(_)), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, result.negatedFailureMessage(_)), None, pos)
    else
      indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "eight" should not startWith ("1.7")
   *                    ^
   * </pre>
   **/
  def startWith(expectedSubstring: String)(implicit ev: T <:< String): Assertion = {
    if ((left.indexOf(expectedSubstring) == 0) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, expectedSubstring, FailureMessages.didNotStartWith), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, expectedSubstring, FailureMessages.startedWith), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.startedWith(prettifier, left, expectedSubstring), FailureMessages.didNotStartWith(prettifier, left, expectedSubstring))
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * greeting should not endWith regex ("wor.d")
   *                     ^
   * </pre>
   **/
  def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    val result = endWithRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, result.failureMessage(_)), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, result.negatedFailureMessage(_)), None, pos)
    else
      indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "eight" should not endWith ("1.7")
   *                    ^
   * </pre>
   **/
  def endWith(expectedSubstring: String)(implicit ev: T <:< String): Assertion = {
    if ((left endsWith expectedSubstring) != shouldBeTrue)
      if (shouldBeTrue)
        indicateFailure(MessageBuilder.of(prettifier, left, expectedSubstring, FailureMessages.didNotEndWith), None, pos)
      else
        indicateFailure(MessageBuilder.of(prettifier, left, expectedSubstring, FailureMessages.endedWith), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.endedWith(prettifier, left, expectedSubstring), FailureMessages.didNotEndWith(prettifier, left, expectedSubstring))
  }

  import scala.language.experimental.macros

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not matchPattern { case Person("Bob", _) => }
   *                   ^
   * </pre>
   **/
  def matchPattern(right: PartialFunction[Any, _]) = macro MatchPatternMacro.matchPattern
  
  /**
   * Overrides toString to return pretty text.
   */
  override def toString: String = "ResultOfNotWordForAny(" + Prettifier.default(left) + ", " + Prettifier.default(shouldBeTrue) + ")"
}

