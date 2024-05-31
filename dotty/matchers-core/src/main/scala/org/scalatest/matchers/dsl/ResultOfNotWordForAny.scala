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
package org.scalatest.matchers.dsl

import org.scalactic._
import org.scalatest.enablers._
import org.scalatest.matchers._
import java.lang.reflect.Field
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import org.scalactic.DefaultEquality.areEqualComparingArraysStructurally
import org.scalatest.matchers.MatchersHelper.andMatchersAndApply
import org.scalatest.matchers.MatchersHelper.orMatchersAndApply
import org.scalatest.matchers.MatchersHelper.transformOperatorChars
import scala.collection.GenMap
import scala.collection.GenSeq
import org.scalactic.ColCompatHelper.Iterable
import scala.reflect.Manifest
import scala.util.matching.Regex
import TripleEqualsSupport.Spread
import TripleEqualsSupport.TripleEqualsInvocation
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatest.matchers.MatchersHelper.matchSymbolToPredicateMethod
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.Assertion
import org.scalatest.matchers.MatchersHelper.endWithRegexWithGroups
import org.scalatest.matchers.MatchersHelper.fullyMatchRegexWithGroups
import org.scalatest.matchers.MatchersHelper.includeRegexWithGroups
import org.scalatest.matchers.MatchersHelper.indicateFailure
import org.scalatest.matchers.MatchersHelper.indicateSuccess
import org.scalatest.matchers.MatchersHelper.newTestFailedException
import org.scalatest.matchers.MatchersHelper.startWithRegexWithGroups
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
  infix def equal(right: Any)(implicit equality: Equality[T]): Assertion = {
    if (equality.areEqual(left, right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotEqual(prettifier, left, right) else FailureMessages.equaled(prettifier, left, right), None, pos)
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
  infix def be(right: Any): Assertion = {
    if ((left == right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotEqualTo(prettifier, left, right) else FailureMessages.wasEqualTo(prettifier, left, right), None, pos)
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
  infix def be(comparison: ResultOfLessThanOrEqualToComparison[T]): Assertion = {
    if (comparison(left) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotLessThanOrEqualTo(prettifier, left, comparison.right) else FailureMessages.wasLessThanOrEqualTo(prettifier, left, comparison.right), None, pos)
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
  infix def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]): Assertion = {
    if (comparison(left) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotGreaterThanOrEqualTo(prettifier, left, comparison.right) else FailureMessages.wasGreaterThanOrEqualTo(prettifier, left, comparison.right), None, pos)
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
  infix def be(comparison: ResultOfLessThanComparison[T]): Assertion = {
    if (comparison(left) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotLessThan(prettifier, left, comparison.right) else FailureMessages.wasLessThan(prettifier, left, comparison.right), None, pos)
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
  infix def be(comparison: ResultOfGreaterThanComparison[T]): Assertion = {
    if (comparison(left) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotGreaterThan(prettifier, left, comparison.right) else FailureMessages.wasGreaterThan(prettifier, left, comparison.right), None, pos)
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
  infix def be(comparison: TripleEqualsInvocation[_]): Matcher[Any] = {
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
  infix def be(beMatcher: BeMatcher[T]): Assertion = {
    val result = beMatcher(left)
    if (result.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
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
  infix def be(resultOfAWordToAMatcherApplication: ResultOfAWordToAMatcherApplication[T]): Assertion = {
    val aMatcher = resultOfAWordToAMatcherApplication.aMatcher
    val result = aMatcher(left)
    if (result.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
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
  infix def be(resultOfAnWordToAnMatcherApplication: ResultOfAnWordToAnMatcherApplication[T]): Assertion = {
    val anMatcher = resultOfAnWordToAnMatcherApplication.anMatcher
    val result = anMatcher(left)
    if (result.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
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
  infix inline def be(aType: ResultOfATypeInvocation[_]): Assertion =
    ${ TypeMatcherMacro.assertATypeShouldBeTrueImpl('{this}, '{aType}) }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be a [Book]
   *                   ^
   * </pre>
   **/
  infix inline def be(anType: ResultOfAnTypeInvocation[_]): Assertion =
    ${ TypeMatcherMacro.assertAnTypeShouldBeTrueImpl('{this}, '{anType}) }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * sevenDotOh should not be (6.5 +- 0.2)
   *                       ^
   * </pre>
   **/
  infix def be(spread: Spread[T]): Assertion = {
    if (spread.isWithin(left) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotPlusOrMinus(prettifier, left, spread.pivot, spread.tolerance) else FailureMessages.wasPlusOrMinus(prettifier, left, spread.pivot, spread.tolerance), None, pos)
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
  infix def be[U](resultOfDefinedAt: ResultOfDefinedAt[U])(implicit ev: T <:< PartialFunction[U, _]): Assertion = {
    if (left.isDefinedAt(resultOfDefinedAt.right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotDefinedAt(prettifier, left, resultOfDefinedAt.right) else FailureMessages.wasDefinedAt(prettifier, left, resultOfDefinedAt.right), None, pos)
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
  infix def equal(spread: Spread[T]): Assertion = {
    if (spread.isWithin(left) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotEqualPlusOrMinus(prettifier, left, spread.pivot, spread.tolerance) else FailureMessages.equaledPlusOrMinus(prettifier, left, spread.pivot, spread.tolerance), None, pos)
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
  infix def equal(right: Null): Assertion = {
    if ((left == null) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotEqualNull(prettifier, left) else FailureMessages.equaledNull, None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.equaledNull, FailureMessages.didNotEqualNull(prettifier, left))
  }

  /**
   * Enables parentheses to be placed around <code>length (N)</code> in expressions of the
   * form: <code>should not have (length (N))</code>.
   */
  infix def have(resultOfLengthWordApplication: ResultOfLengthWordApplication)(implicit len: Length[T]): Assertion = {
    val right = resultOfLengthWordApplication.expectedLength
    val leftLength = len.lengthOf(left)
    if ((leftLength == right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.hadLengthInsteadOfExpectedLength(prettifier, left, leftLength, right) else FailureMessages.hadLength(prettifier, left, right), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.hadLength(prettifier, left, right), FailureMessages.hadLengthInsteadOfExpectedLength(prettifier, left, leftLength, right))
  }

  /**
   * Enables parentheses to be placed around <code>size (N)</code> in expressions of the
   * form: <code>should not have (size (N))</code>.
   */
  infix def have(resultOfSizeWordApplication: ResultOfSizeWordApplication)(implicit sz: Size[T]): Assertion = {
    val right = resultOfSizeWordApplication.expectedSize
    val leftSize = sz.sizeOf(left)
    if ((leftSize == right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.hadSizeInsteadOfExpectedSize(prettifier, left, leftSize, right) else FailureMessages.hadSize(prettifier, left, right), None, pos)
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
  infix def have(firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*): Assertion = {

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
            FailureMessages.propertyDidNotHaveExpectedValue(prettifier,
               UnquotedString(firstFailure.propertyName),
               firstFailure.expectedValue,
               firstFailure.actualValue,
               left
            ),
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

          indicateFailure(failureMessage, None, pos)
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

  infix def have(resultOfMessageWordApplication: ResultOfMessageWordApplication)(implicit messaging: Messaging[T]): Assertion = {
    val right = resultOfMessageWordApplication.expectedMessage
    val actualMessage = messaging.messageOf(left)
    if ((actualMessage == right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.hadMessageInsteadOfExpectedMessage(prettifier, left, actualMessage, right) else FailureMessages.hadExpectedMessage(prettifier, left, right), None, pos)
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
  infix def contain(nullValue: Null)(implicit containing: Containing[T]): Assertion = {
    if (containing.contains(left, null) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainNull(prettifier, left) else FailureMessages.containedNull(prettifier, left), None, pos)
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
  infix def contain(expectedElement: Any)(implicit containing: Containing[T]): Assertion = {
    val right = expectedElement
    if (containing.contains(left, right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainExpectedElement(prettifier, left, right) else FailureMessages.containedExpectedElement(prettifier, left, right), None, pos)
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
  infix def be(o: Null)(implicit ev: T <:< AnyRef): Assertion = {
    if ((left == null) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotNull(prettifier, left) else FailureMessages.wasNull, None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasNull, FailureMessages.wasNotNull(prettifier, left))
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * stack should not be ('empty)
   *                  ^
   * </pre>
   **/
  infix def be(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef, prettifier: Prettifier, pos: source.Position): Assertion = {
    val matcherResult = matchSymbolToPredicateMethod(toAnyRef(left), symbol, false, false, prettifier, pos)
    if (matcherResult.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) matcherResult.failureMessage(prettifier) else matcherResult.negatedFailureMessage(prettifier), None, pos)
    else
      indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * This method enables the following syntax, where <code>stack</code> is, for example, of type <code>Stack</code> and
   * <code>empty</code> refers to a <code>BePropertyMatcher[Stack]</code>:
   *
   * <pre class="stHighlight">
   * stack should not be (empty)
   *                      ^
   * </pre>
   **/
  infix def be(bePropertyMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef): Assertion = {
    val result = bePropertyMatcher(left)
    if (result.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNot(prettifier, left, UnquotedString(result.propertyName)) else FailureMessages.was(prettifier, left, UnquotedString(result.propertyName)), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.was(prettifier, left, UnquotedString(result.propertyName)), FailureMessages.wasNot(prettifier, left, UnquotedString(result.propertyName)))
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * notFileMock should not be a ('file)
   *                        ^
   * </pre>
   **/
  infix def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef, prettifier: Prettifier, pos: source.Position): Assertion = {
    val matcherResult = matchSymbolToPredicateMethod(toAnyRef(left), resultOfAWordApplication.symbol, true, true, prettifier, pos)
    if (matcherResult.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) matcherResult.failureMessage(prettifier) else matcherResult.negatedFailureMessage(prettifier), None, pos)
    else
      indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * This method enables the following syntax, where <code>notFileMock</code> is, for example, of type <code>File</code> and
   * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
   *
   * <pre class="stHighlight">
   * notFileMock should not be a (file)
   *                        ^
   * </pre>
   **/
  infix def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {
    val result = resultOfAWordApplication.bePropertyMatcher(left)
    if (result.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotA(prettifier, left, UnquotedString(result.propertyName)) else FailureMessages.wasA(prettifier, left, UnquotedString(result.propertyName)), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasA(prettifier, left, UnquotedString(result.propertyName)), FailureMessages.wasNotA(prettifier, left, UnquotedString(result.propertyName)))
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * keyEvent should not be an ('actionKey)
   *                     ^
   * </pre>
   **/
  infix def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef, prettifier: Prettifier, pos: source.Position): Assertion = {
    val matcherResult = matchSymbolToPredicateMethod(toAnyRef(left), resultOfAnWordApplication.symbol, true, false, prettifier, pos)
    if (matcherResult.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) matcherResult.failureMessage(prettifier) else matcherResult.negatedFailureMessage(prettifier), None, pos)
    else
      indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
   * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
   *
   * <pre class="stHighlight">
   * keyEvent should not be an (actionKey)
   *                     ^
   * </pre>
   **/
  infix def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {
    val result = resultOfAnWordApplication.bePropertyMatcher(left)
    if (result.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotAn(prettifier, left, UnquotedString(result.propertyName)) else FailureMessages.wasAn(prettifier, left, UnquotedString(result.propertyName)), None, pos)
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
  infix def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    if ((resultOfSameInstanceAsApplication.right eq toAnyRef(left)) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotSameInstanceAs(prettifier, left, resultOfSameInstanceAsApplication.right) else FailureMessages.wasSameInstanceAs(prettifier, left, resultOfSameInstanceAsApplication.right), None, pos)
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
  infix def be[U](sortedWord: SortedWord)(implicit sortable: Sortable[T]): Assertion = {
    if (sortable.isSorted(left) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotSorted(prettifier, left) else FailureMessages.wasSorted(prettifier, left), None, pos)
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
  infix def be[U](readableWord: ReadableWord)(implicit readability: Readability[T]): Assertion = {
    if (readability.isReadable(left) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotReadable(prettifier, left) else FailureMessages.wasReadable(prettifier, left), None, pos)
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
  infix def be[U](writableWord: WritableWord)(implicit writability: Writability[T]): Assertion = {
    if (writability.isWritable(left) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotWritable(prettifier, left) else FailureMessages.wasWritable(prettifier, left), None, pos)
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
  infix def be[U](emptyWord: EmptyWord)(implicit emptiness: Emptiness[T]): Assertion = {
    if (emptiness.isEmpty(left) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotEmpty(prettifier, left) else FailureMessages.wasEmpty(prettifier, left), None, pos)
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
  infix def be[U](definedWord: DefinedWord)(implicit definition: Definition[T]): Assertion = {
    if (definition.isDefined(left) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.wasNotDefined(prettifier, left) else FailureMessages.wasDefined(prettifier, left), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.wasDefined(prettifier, left), FailureMessages.wasNotDefined(prettifier, left))
  }

  infix def contain(newOneOf: ResultOfOneOfApplication)(implicit containing: Containing[T]): Assertion = {

    val right = newOneOf.right

    if (containing.containsOneOf(left, right) != shouldBeTrue)
      indicateFailure(
        if (shouldBeTrue)
          FailureMessages.didNotContainOneOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        else
          FailureMessages.containedOneOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        None,
        pos
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedOneOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainOneOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  infix def contain(oneElementOf: ResultOfOneElementOfApplication)(implicit evidence: Containing[T]): Assertion = {

    val right = oneElementOf.right

    if (evidence.containsOneOf(left, right.distinct) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainOneElementOf(prettifier, left, right) else FailureMessages.containedOneElementOf(prettifier, left, right), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedOneElementOf(prettifier, left, right), FailureMessages.didNotContainOneElementOf(prettifier, left, right))
  }

  infix def contain(atLeastOneOf: ResultOfAtLeastOneOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = atLeastOneOf.right

    if (aggregating.containsAtLeastOneOf(left, right) != shouldBeTrue)
      indicateFailure(
        if (shouldBeTrue)
          FailureMessages.didNotContainAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        else
          FailureMessages.containedAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        None,
        pos
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  infix def contain(atLeastOneElementOf: ResultOfAtLeastOneElementOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = atLeastOneElementOf.right

    if (aggregating.containsAtLeastOneOf(left, right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainAtLeastOneElementOf(prettifier, left, right) else FailureMessages.containedAtLeastOneElementOf(prettifier, left, right), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedAtLeastOneElementOf(prettifier, left, right), FailureMessages.didNotContainAtLeastOneElementOf(prettifier, left, right))
  }

  infix def contain(noneOf: ResultOfNoneOfApplication)(implicit containing: Containing[T]): Assertion = {

    val right = noneOf.right

    if (containing.containsNoneOf(left, right) != shouldBeTrue)
      indicateFailure(
        if (shouldBeTrue)
          FailureMessages.containedAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        else
          FailureMessages.didNotContainAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        None,
        pos
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.didNotContainAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.containedAtLeastOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  infix def contain(noElementsOf: ResultOfNoElementsOfApplication)(implicit containing: Containing[T]): Assertion = {

    val right = noElementsOf.right

    if (containing.containsNoneOf(left, right.distinct) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.containedAtLeastOneElementOf(prettifier, left, right) else FailureMessages.didNotContainAtLeastOneElementOf(prettifier, left, right), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, left, right), FailureMessages.containedAtLeastOneElementOf(prettifier, left, right))
  }

  infix def contain(theSameElementsAs: ResultOfTheSameElementsAsApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = theSameElementsAs.right

    if (aggregating.containsTheSameElementsAs(left, right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainSameElements(prettifier, left, right) else FailureMessages.containedSameElements(prettifier, left, right), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedSameElements(prettifier, left, right), FailureMessages.didNotContainSameElements(prettifier, left, right))
  }

  infix def contain(theSameElementsInOrderAs: ResultOfTheSameElementsInOrderAsApplication)(implicit sequencing: Sequencing[T]): Assertion = {

    val right = theSameElementsInOrderAs.right

    if (sequencing.containsTheSameElementsInOrderAs(left, right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainSameElementsInOrder(prettifier, left, right) else FailureMessages.containedSameElementsInOrder(prettifier, left, right), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedSameElementsInOrder(prettifier, left, right), FailureMessages.didNotContainSameElementsInOrder(prettifier, left, right))
  }

  infix def contain(only: ResultOfOnlyApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    val right = only.right
    val withFriendlyReminder = right.size == 1 && (right(0).isInstanceOf[Iterable[_]] || right(0).isInstanceOf[Every[_]])
    if (aggregating.containsOnly(left, right) != shouldBeTrue) {
      indicateFailure(
        if (shouldBeTrue)
          if (withFriendlyReminder)
            FailureMessages.didNotContainOnlyElementsWithFriendlyReminder(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          else
            FailureMessages.didNotContainOnlyElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        else
          if (withFriendlyReminder)
            FailureMessages.containedOnlyElementsWithFriendlyReminder(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          else
            FailureMessages.containedOnlyElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        None,
        pos
      )
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

  infix def contain(only: ResultOfInOrderOnlyApplication)(implicit sequencing: Sequencing[T]): Assertion = {
    val right = only.right
    if (sequencing.containsInOrderOnly(left, right) != shouldBeTrue)
      indicateFailure(
        if (shouldBeTrue)
          FailureMessages.didNotContainInOrderOnlyElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        else
          FailureMessages.containedInOrderOnlyElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        None,
        pos
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedInOrderOnlyElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainInOrderOnlyElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  infix def contain(allOf: ResultOfAllOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = allOf.right
    if (aggregating.containsAllOf(left, right) != shouldBeTrue)
      indicateFailure(
        if (shouldBeTrue)
          FailureMessages.didNotContainAllOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        else
          FailureMessages.containedAllOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        None,
        pos
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAllOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainAllOfElements(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  infix def contain(allElementsOf: ResultOfAllElementsOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = allElementsOf.right
    if (aggregating.containsAllOf(left, right.distinct) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainAllElementsOf(prettifier, left, right) else FailureMessages.containedAllElementsOf(prettifier, left, right), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedAllElementsOf(prettifier, left, right), FailureMessages.didNotContainAllElementsOf(prettifier, left, right))
  }

  infix def contain(only: ResultOfInOrderApplication)(implicit sequencing: Sequencing[T]): Assertion = {

    val right = only.right
    if (sequencing.containsInOrder(left, right) != shouldBeTrue)
      indicateFailure(
        if (shouldBeTrue)
          FailureMessages.didNotContainAllOfElementsInOrder(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        else
          FailureMessages.containedAllOfElementsInOrder(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        None,
        pos
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAllOfElementsInOrder(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainAllOfElementsInOrder(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  infix def contain(only: ResultOfInOrderElementsOfApplication)(implicit sequencing: Sequencing[T]): Assertion = {

    val right = only.right
    if (sequencing.containsInOrder(left, right.distinct) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainAllElementsOfInOrder(prettifier, left, right) else FailureMessages.containedAllElementsOfInOrder(prettifier, left, right), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedAllElementsOfInOrder(prettifier, left, right), FailureMessages.didNotContainAllElementsOfInOrder(prettifier, left, right))
  }

  infix def contain(atMostOneOf: ResultOfAtMostOneOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = atMostOneOf.right

    if (aggregating.containsAtMostOneOf(left, right) != shouldBeTrue)
      indicateFailure(
	      if (shouldBeTrue)
          FailureMessages.didNotContainAtMostOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        else
	        FailureMessages.containedAtMostOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        None,
        pos
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAtMostOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
        FailureMessages.didNotContainAtMostOneOf(prettifier, left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
      )
  }

  infix def contain(atMostOneElementOf: ResultOfAtMostOneElementOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {

    val right = atMostOneElementOf.right

    if (aggregating.containsAtMostOneOf(left, right.distinct) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainAtMostOneElementOf(prettifier, left, right) else FailureMessages.containedAtMostOneElementOf(prettifier, left, right), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedAtMostOneElementOf(prettifier, left, right), FailureMessages.didNotContainAtMostOneElementOf(prettifier, left, right))
  }

  infix def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication)(implicit keyMapping: KeyMapping[T]): Assertion = {
    val right = resultOfKeyWordApplication.expectedKey
    if (keyMapping.containsKey(left, right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainKey(prettifier, left, right) else FailureMessages.containedKey(prettifier, left, right), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedKey(prettifier, left, right), FailureMessages.didNotContainKey(prettifier, left, right))
  }
  infix def contain(resultOfValueWordApplication: ResultOfValueWordApplication)(implicit valueMapping: ValueMapping[T]): Assertion = {
    val right = resultOfValueWordApplication.expectedValue
    if (valueMapping.containsValue(left, right) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainValue(prettifier, left, right) else FailureMessages.containedValue(prettifier, left, right), None, pos)
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
  infix def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    val result = fullyMatchRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
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
  infix def include(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    val result = includeRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
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
  infix def include(expectedSubstring: String)(implicit ev: T <:< String): Assertion = {
    if ((left.indexOf(expectedSubstring) >= 0) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotIncludeSubstring(prettifier, left, expectedSubstring) else FailureMessages.includedSubstring(prettifier, left, expectedSubstring), None, pos)
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
  infix def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    val result = startWithRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
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
  infix def startWith(expectedSubstring: String)(implicit ev: T <:< String): Assertion = {
    if ((left.indexOf(expectedSubstring) == 0) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotStartWith(prettifier, left, expectedSubstring) else FailureMessages.startedWith(prettifier, left, expectedSubstring), None, pos)
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
  infix def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    val result = endWithRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
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
  infix def endWith(expectedSubstring: String)(implicit ev: T <:< String): Assertion = {
    if ((left endsWith expectedSubstring) != shouldBeTrue)
      indicateFailure(if (shouldBeTrue) FailureMessages.didNotEndWith(prettifier, left, expectedSubstring) else FailureMessages.endedWith(prettifier, left, expectedSubstring), None, pos)
    else
      indicateSuccess(shouldBeTrue, FailureMessages.endedWith(prettifier, left, expectedSubstring), FailureMessages.didNotEndWith(prettifier, left, expectedSubstring))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not matchPattern { case Person("Bob", _) => }
   *                   ^
   * </pre>
   **/
  infix inline def matchPattern(inline right: PartialFunction[Any, _]) =
    ${ MatchPatternMacro.matchPattern('{this}, '{right}) }

  /**
   * Overrides toString to return pretty text.
   */
  override def toString: String = "ResultOfNotWordForAny(" + Prettifier.default(left) + ", " + Prettifier.default(shouldBeTrue) + ")"
}

