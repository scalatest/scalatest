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

import org.scalatest.matchers._
import org.scalatest.enablers._
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import scala.util.matching.Regex
import java.lang.reflect.Field
import scala.reflect.Manifest
import org.scalatest.MatchersHelper.transformOperatorChars
import scala.collection.Traversable
import org.scalatest.Assertions.areEqualComparingArraysStructurally
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap
import org.scalautils.Tolerance
import org.scalautils.Explicitly
import org.scalautils.Interval
import org.scalautils.TripleEqualsInvocation
import scala.annotation.tailrec
import org.scalautils.Equality
import org.scalautils.TripleEqualsInvocationOnInterval
import org.scalautils.EqualityConstraint
import org.scalatest.MatchersHelper.andMatchersAndApply
import org.scalatest.MatchersHelper.orMatchersAndApply
import org.scalatest.MatchersHelper.matchSymbolToPredicateMethod
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.MatchersHelper.newTestFailedException
import org.scalatest.MatchersHelper.fullyMatchRegexWithGroups
import org.scalatest.MatchersHelper.startWithRegexWithGroups
import org.scalatest.MatchersHelper.endWithRegexWithGroups
import org.scalatest.MatchersHelper.includeRegexWithGroups

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
sealed class ResultOfNotWordForAny[T](left: T, shouldBeTrue: Boolean) {

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not equal (7)
   *                   ^
   * </pre>
   */
  def equal(right: Any)(implicit equality: Equality[T]) {
    if (equality.areEqual(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
         if (shouldBeTrue) "didNotEqual" else "equaled",
          left,
          right
        )
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be (7)
   *                   ^
   * </pre>
   */
  def be(right: Any) {
    if ((left == right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
         if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
          left,
          right
        )
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be &lt;= (7)
   *                   ^
   * </pre>
   */
  def be(comparison: ResultOfLessThanOrEqualToComparison[T]) {
    if (comparison(left) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotLessThanOrEqualTo" else "wasLessThanOrEqualTo",
          left,
          comparison.right
        )
      )
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be &gt;= (7)
   *                   ^
   * </pre>
   */
  def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]) {
    if (comparison(left) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotGreaterThanOrEqualTo" else "wasGreaterThanOrEqualTo",
          left,
          comparison.right
        )
      )
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be &lt; (7)
   *                   ^
   * </pre>
   */
  def be(comparison: ResultOfLessThanComparison[T]) {
    if (comparison(left) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotLessThan" else "wasLessThan",
          left,
          comparison.right
        )
      )
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be &gt; (7)
   *                   ^
   * </pre>
   */
  def be(comparison: ResultOfGreaterThanComparison[T]) {
    if (comparison(left) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotGreaterThan" else "wasGreaterThan",
          left,
          comparison.right
        )
      )
    }
  }

  /**
   * <strong>
   * The should be === syntax has been deprecated and will be removed in a future version of ScalaTest. Please use should equal, should ===, shouldEqual,
   * should be, or shouldBe instead. Note, the reason this was deprecated was so that === would mean only one thing in ScalaTest: a customizable, type-
   * checkable equality comparison.
   * </strong>
   *
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should not be === (7)
   *                   ^
   * </pre>
   */
  @deprecated("The should be === syntax has been deprecated. Please use should equal, should ===, shouldEqual, should be, or shouldBe instead.")
  def be(comparison: TripleEqualsInvocation[_]) {
    if ((left == comparison.right) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
          left,
          comparison.right
        )
      )
    }
  }

  /**
   * This method enables the following syntax, where <code>odd</code> refers to
   * a <code>BeMatcher[Int]</code>:
   *
   * <pre class="stHighlight">
   * 2 should not be (odd)
   *              ^
   * </pre>
   */
  def be(beMatcher: BeMatcher[T]) {
    val result = beMatcher(left)
    if (result.matches != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue)
          result.failureMessage
        else
          result.negatedFailureMessage
      )
    }
  }
  
  /**
   * This method enables the following syntax, where <code>positiveNumber</code> refers to
   * an <code>AMatcher[Int]</code>:
   *
   * <pre class="stHighlight">
   * 2 should not be a (positiveNumber)
   *              ^
   * </pre>
   */
  def be(resultOfAWordToAMatcherApplication: ResultOfAWordToAMatcherApplication[T]) {
    val aMatcher = resultOfAWordToAMatcherApplication.aMatcher
    val result = aMatcher(left)
    if (result.matches != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue)
          result.failureMessage
        else
          result.negatedFailureMessage
      )
    }
  }
  
  /**
   * This method enables the following syntax, where <code>oddNumber</code> refers to
   * an <code>AnMatcher[Int]</code>:
   *
   * <pre class="stHighlight">
   * 2 should not be an (oddNumber)
   *              ^
   * </pre>
   */
  def be(resultOfAnWordToAnMatcherApplication: ResultOfAnWordToAnMatcherApplication[T]) {
    val anMatcher = resultOfAnWordToAnMatcherApplication.anMatcher
    val result = anMatcher(left)
    if (result.matches != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue)
          result.failureMessage
        else
          result.negatedFailureMessage
      )
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * sevenDotOh should not be (6.5 +- 0.2)
   *                       ^
   * </pre>
   */
  def be(interval: Interval[T]) {
    if (interval.isWithin(left) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotPlusOrMinus" else "wasPlusOrMinus",
          left,
          interval.pivot,
          interval.tolerance
        )
      )
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * partialFun should not be definedAt ("apple")
   *                       ^
   * </pre>
   */
  def be[U](resultOfDefinedAt: ResultOfDefinedAt[U])(implicit ev: T <:< PartialFunction[U, _]) {
    if (left.isDefinedAt(resultOfDefinedAt.right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotDefinedAt" else "wasDefinedAt", 
          left, 
          resultOfDefinedAt.right
        )    
      )
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * sevenDotOh should not equal (6.5 +- 0.2)
   *                       ^
   * </pre>
   */
  def equal(interval: Interval[T]) {
    if (interval.isWithin(left) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotEqualPlusOrMinus" else "equaledPlusOrMinus",
          left,
          interval.pivot,
          interval.tolerance
        )
      )
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should not equal null
   *                   ^
   * </pre>
   */
  def equal(right: Null) {
    if ((left == null) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotEqualNull" else "equaledNull",
          left
        )
      )
    }
  }

// For Any, TODO: Scaladoc
  def have(resultOfLengthWordApplication: ResultOfLengthWordApplication)(implicit len: Length[T]) {
    val right = resultOfLengthWordApplication.expectedLength
    val leftLength = len.lengthOf(left)
    if ((leftLength == right) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue)
            FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, right)
          else
            FailureMessages("hadExpectedLength", left, right)
        )
      )
    }
  }

// For Any, TODO: Scaladoc
  def have(resultOfSizeWordApplication: ResultOfSizeWordApplication)(implicit sz: Size[T]) {
    val right = resultOfSizeWordApplication.expectedSize
    val leftSize = sz.sizeOf(left)
    if ((leftSize == right) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, right)
          else
            FailureMessages("hadExpectedSize", left, right)
        )
      )
    }
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
   */
  def have(firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*) {

    val results =
      for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
        propertyVerifier(left)

    val firstFailureOption = results.find(pv => !pv.matches)

    val justOneProperty = propertyMatchers.length == 0

    // if shouldBeTrue is false, then it is like "not have ()", and should throw TFE if firstFailureOption.isDefined is false
    // if shouldBeTrue is true, then it is like "not (not have ()), which should behave like have ()", and should throw TFE if firstFailureOption.isDefined is true
    if (firstFailureOption.isDefined == shouldBeTrue) {
      firstFailureOption match {
        case Some(firstFailure) =>
          // This is one of these cases, thus will only get here if shouldBeTrue is true
          // 0 0 | 0 | 1
          // 0 1 | 0 | 1
          // 1 0 | 0 | 1
          throw newTestFailedException(
            FailureMessages(
              "propertyDidNotHaveExpectedValue",
               UnquotedString(firstFailure.propertyName),
               firstFailure.expectedValue,
               firstFailure.actualValue,
               left
            )
          )
        case None =>
          // This is this cases, thus will only get here if shouldBeTrue is false
          // 1 1 | 1 | 0
          val failureMessage =
            if (justOneProperty) {
              val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
              FailureMessages(
                "propertyHadExpectedValue",
                UnquotedString(firstPropertyResult.propertyName),
                firstPropertyResult.expectedValue,
                left
              )
            }
            else FailureMessages("allPropertiesHadExpectedValues", left)

          throw newTestFailedException(failureMessage)
      } 
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * object should not contain ("one")
   *                   ^
   * </pre>
   */
  def contain(expectedElement: Any)(implicit containing: Containing[T]) {
    val right = expectedElement
    if (containing.contains(left, right) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
            left,
            right
          )
        )
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should not be (null)
   *                ^
   * </pre>
   */
  def be(o: Null)(implicit ev: T <:< AnyRef) {
    if ((left == null) != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages("wasNotNull", left) 
        else
          FailureMessages("wasNull")
      )
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * stack should not be ('empty)
   *                  ^
   * </pre>
   */
  def be(symbol: Symbol)(implicit ev: T <:< AnyRef) {
    val matcherResult = matchSymbolToPredicateMethod(left, symbol, false, false)
    if (matcherResult.matches != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
      )
    }
  }

  /**
   * This method enables the following syntax, where <code>stack</code> is, for example, of type <code>Stack</code> and
   * <code>empty</code> refers to a <code>BePropertyMatcher[Stack]</code>:
   *
   * <pre class="stHighlight">
   * stack should not be (empty)
   *                      ^
   * </pre>
   */
  def be(bePropertyMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef) {
    val result = bePropertyMatcher(left)
    if (result.matches != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages("wasNot", left, UnquotedString(result.propertyName))
        else
          FailureMessages("was", left, UnquotedString(result.propertyName))
      )
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * notFileMock should not be a ('file)
   *                        ^
   * </pre>
   */
  def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit ev: T <:< AnyRef) {
    val matcherResult = matchSymbolToPredicateMethod(left, resultOfAWordApplication.symbol, true, true)
    if (matcherResult.matches != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
      )
    }
  }

  /**
   * This method enables the following syntax, where <code>notFileMock</code> is, for example, of type <code>File</code> and
   * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
   *
   * <pre class="stHighlight">
   * notFileMock should not be a (file)
   *                        ^
   * </pre>
   */
  def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef) {
    val result = resultOfAWordApplication.bePropertyMatcher(left)
    if (result.matches != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages("wasNotA", left, UnquotedString(result.propertyName))
        else
          FailureMessages("wasA", left, UnquotedString(result.propertyName))
      )
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * keyEvent should not be an ('actionKey)
   *                     ^
   * </pre>
   */
  def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit ev: T <:< AnyRef) {
    val matcherResult = matchSymbolToPredicateMethod(left, resultOfAnWordApplication.symbol, true, false)
    if (matcherResult.matches != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
      )
    }
  }

  /**
   * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
   * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
   *
   * <pre class="stHighlight">
   * keyEvent should not be an (actionKey)
   *                     ^
   * </pre>
   */
  def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef) {
    val result = resultOfAnWordApplication.bePropertyMatcher(left)
    if (result.matches != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages("wasNotAn", left, UnquotedString(result.propertyName))
        else
          FailureMessages("wasAn", left, UnquotedString(result.propertyName))
      )
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * otherString should not be theSameInstanceAs (string)
   *                        ^
   * </pre>
   */
  def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication)(implicit ev: T <:< AnyRef) {
    if ((resultOfSameInstanceAsApplication.right eq left) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
          left,
          resultOfSameInstanceAsApplication.right
        )
      )
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should not be sorted
   *                  ^
   * </pre>
   */
  def be[U](sortedWord: SortedWord)(implicit sortable: Sortable[T]) {
    if (sortable.isSorted(left) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotSorted" else "wasSorted", 
          left
        )    
      )
  }

  def contain(newOneOf: ResultOfOneOfApplication)(implicit containing: Containing[T]) {

    val right = newOneOf.right

    if (containing.containsOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainOneOfElements" else "containedOneOfElements",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }

  def contain(atLeastOneOf: ResultOfAtLeastOneOfApplication)(implicit aggregating: Aggregating[T]) {

    val right = atLeastOneOf.right

    if (aggregating.containsAtLeastOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainAtLeastOneOf" else "containedAtLeastOneOf",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }

  def contain(noneOf: ResultOfNoneOfApplication)(implicit containing: Containing[T]) {

    val right = noneOf.right

    if (containing.containsNoneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "containedOneOfElements" else "didNotContainOneOfElements",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }
  
  def contain(theSameElementsAs: ResultOfTheSameElementsAsApplication)(implicit aggregating: Aggregating[T]) {

    val right = theSameElementsAs.right

    if (aggregating.containsTheSameElementsAs(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainSameElements" else "containedSameElements",
          left,
          right
        )
      )
  }

  def contain(theSameElementsInOrderAs: ResultOfTheSameElementsInOrderAsApplication)(implicit sequencing: Sequencing[T]) {

    val right = theSameElementsInOrderAs.right

    if (sequencing.containsTheSameElementsInOrderAs(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainSameElementsInOrder" else "containedSameElementsInOrder",
          left,
          right
        )
      )
  }
  
  def contain(only: ResultOfOnlyApplication)(implicit aggregating: Aggregating[T]) {

    val right = only.right
    if (aggregating.containsOnly(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainOnlyElements" else "containedOnlyElements",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }

  def contain(only: ResultOfInOrderOnlyApplication)(implicit sequencing: Sequencing[T]) {
    val right = only.right
    if (sequencing.containsInOrderOnly(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainInOrderOnlyElements" else "containedInOrderOnlyElements",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }
  
  def contain(only: ResultOfAllOfApplication)(implicit aggregating: Aggregating[T]) {

    val right = only.right
    if (aggregating.containsAllOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainAllOfElements" else "containedAllOfElements",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }
  
  def contain(only: ResultOfInOrderApplication)(implicit sequencing: Sequencing[T]) {

    val right = only.right
    if (sequencing.containsInOrder(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainAllOfElementsInOrder" else "containedAllOfElementsInOrder",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }

  def contain(resultOfNewKeyWordApplication: ResultOfNewKeyWordApplication)(implicit keyMapping: KeyMapping[T]) {
    val right = resultOfNewKeyWordApplication.expectedKey
    if (keyMapping.containsKey(left, right) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainKey" else "containedKey",
            left,
            right
          )
        )
    }
  }
}

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfNotWordForString(left: String, shouldBeTrue: Boolean)
    extends ResultOfNotWordForAny[String](left, shouldBeTrue) {

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
  def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
    val result = fullyMatchRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
      )
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
  def include(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
    val result = includeRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
      )
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * string should not include ("world")
   *                   ^
   * </pre>
   */
  def include(expectedSubstring: String) {
    if ((left.indexOf(expectedSubstring) >= 0) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotIncludeSubstring" else "includedSubstring",
          left,
          expectedSubstring
        )
      )
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
  def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
    val result = startWithRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
      )
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "eight" should not startWith ("1.7")
   *                    ^
   * </pre>
   */
  def startWith(expectedSubstring: String) {
    if ((left.indexOf(expectedSubstring) == 0) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotStartWith" else "startedWith",
          left,
          expectedSubstring
        )
      )
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * greeting should not endWith regex ("wor.d")
   *                     ^
   * </pre>
   */
  def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
    val result = endWithRegexWithGroups(left, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
    if (result.matches != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
      )
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "eight" should not endWith ("1.7")
   *                    ^
   * </pre>
   */
  def endWith(expectedSubstring: String) {
    if ((left endsWith expectedSubstring) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotEndWith" else "endedWith",
          left,
          expectedSubstring
        )
      )
  }
}

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfNotWordForGenMap[K, V, L[_, _] <: scala.collection.GenMap[_, _]](left: L[K, V], shouldBeTrue: Boolean)
    extends ResultOfNotWordForAny(left, shouldBeTrue) {

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should not contain key ("three")
   *                ^
   * </pre>
   */
  def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
    val right = resultOfKeyWordApplication.expectedKey
    if ((left.asInstanceOf[GenMap[K, V]].exists(_._1 == right)) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainKey" else "containedKey",
            left,
            right
          )
        )
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should not contain value (3)
   *                                        ^
   * </pre>
   */
  def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
    val right = resultOfValueWordApplication.expectedValue
    if ((left.asInstanceOf[scala.collection.GenMap[K, V]].exists(_._2 == right)) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainValue" else "containedValue",
            left,
            right
          )
        )
    }
  }

  // TODO: Had to pull these methods out of ReusltOfNotWordForTraversable, because can't exent
  // it without losing precision on the inferred types. Map[String, Int] becomes GenIterable[(Any, Any)]
  // So the wrong Equality type class was chosen. By going around ResultOfNotWordForTraversable, I can
  // get the precise Map type up to ResultOfNotWord's equal method, which requires the Equality type class.

  /* I think this was wrong.
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * iterable should not contain ("one")
   *                     ^
   * </pre>
   */
/*
  def contain(expectedElement: (K, V)) {
    val right = expectedElement
    if ((left.exists(_ == right)) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
            left,
            right
          )
        )
    }
  }
*/
}

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfNotWordForJavaMap[K, V, L[_, _] <: java.util.Map[_, _]](left: L[K, V], shouldBeTrue: Boolean)
    extends ResultOfNotWordForAny(left, shouldBeTrue) {

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * javaMap should not contain key ("three")
   *                    ^
   * </pre>
   */
  def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
    val right = resultOfKeyWordApplication.expectedKey
    if ((left.containsKey(right)) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainKey" else "containedKey",
            left,
            right
          )
        )
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * javaMap should not contain value (3)
   *                            ^
   * </pre>
   */
  def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
    val right = resultOfValueWordApplication.expectedValue
    if ((left.containsValue(right)) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainValue" else "containedValue",
            left,
            right
          )
        )
    }
  }
}

