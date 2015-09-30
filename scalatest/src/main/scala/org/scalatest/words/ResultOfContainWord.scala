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

import scala.collection.GenTraversable
import org.scalatest.enablers.Containing
import org.scalatest.enablers.Aggregating
import org.scalatest.enablers.Sequencing
import org.scalatest.enablers.KeyMapping
import org.scalatest.enablers.ValueMapping
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import org.scalactic.{Prettifier, Every}
import org.scalatest.Assertion
import org.scalatest.MatchersHelper.indicateSuccess
import org.scalatest.MatchersHelper.indicateFailure

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
class ResultOfContainWord[L](left: L, shouldBeTrue: Boolean = true) {

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain oneOf (1, 2)
   *                   ^
   * </pre>
   */
  def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit containing: Containing[L]): Assertion = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.oneOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "oneOf"))
    if (containing.containsOneOf(left, right) != shouldBeTrue)
      indicateFailure(
        shouldBeTrue,
        FailureMessages.didNotContainOneOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.containedOneOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedOneOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.didNotContainOneOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain oneElementOf List(1, 2)
   *                   ^
   * </pre>
   */
  def oneElementOf(elements: GenTraversable[Any])(implicit containing: Containing[L]): Assertion = {
    val right = elements.toList
    if (containing.containsOneOf(left, right.distinct) != shouldBeTrue)
      indicateFailure(shouldBeTrue, FailureMessages.didNotContainOneElementOf(left, right), FailureMessages.containedOneElementOf(left, right))
    else indicateSuccess(shouldBeTrue, FailureMessages.containedOneElementOf(left, right), FailureMessages.didNotContainOneElementOf(left, right))
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain atLeastOneOf (1, 2)
   *                   ^
   * </pre>
   */
  def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[L]): Assertion = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.atLeastOneOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "atLeastOneOf"))
    if (aggregating.containsAtLeastOneOf(left, right) != shouldBeTrue)
      indicateFailure(
        shouldBeTrue,
        FailureMessages.didNotContainAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.containedAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.didNotContainAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain atLeastOneElementOf List(1, 2)
   *                   ^
   * </pre>
   */
  def atLeastOneElementOf(elements: GenTraversable[Any])(implicit aggregating: Aggregating[L]): Assertion = {
    val right = elements.toList
    if (aggregating.containsAtLeastOneOf(left, right.distinct) != shouldBeTrue)
      indicateFailure(shouldBeTrue, FailureMessages.didNotContainAtLeastOneElementOf(left, right), FailureMessages.containedAtLeastOneElementOf(left, right))
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedAtLeastOneElementOf(left, right), FailureMessages.didNotContainAtLeastOneElementOf(left, right))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain noneOf (1, 2)
   *                   ^
   * </pre>
   */
  def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit containing: Containing[L]): Assertion = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.noneOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "noneOf"))
    if (containing.containsNoneOf(left, right) != shouldBeTrue)
      indicateFailure(
        shouldBeTrue,
        FailureMessages.containedAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.didNotContainAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.didNotContainAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.containedAtLeastOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain noElementsOf List(1, 2)
   *                   ^
   * </pre>
   */
  def noElementsOf(elements: GenTraversable[Any])(implicit containing: Containing[L]): Assertion = {
    val right = elements.toList
    if (containing.containsNoneOf(left, right.distinct) != shouldBeTrue)
      indicateFailure(shouldBeTrue, FailureMessages.containedAtLeastOneElementOf(left, right), FailureMessages.didNotContainAtLeastOneElementOf(left, right))
    else
      indicateSuccess(shouldBeTrue, FailureMessages.didNotContainAtLeastOneElementOf(left, right), FailureMessages.containedAtLeastOneElementOf(left, right))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain theSameElementsAs (List(1, 2))
   *                   ^
   * </pre>
   */
  def theSameElementsAs(right: GenTraversable[_])(implicit aggregating: Aggregating[L]): Assertion = {
    if (aggregating.containsTheSameElementsAs(left, right) != shouldBeTrue)
      indicateFailure(shouldBeTrue, FailureMessages.didNotContainSameElements(left, right), FailureMessages.containedSameElements(left, right))
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedSameElements(left, right), FailureMessages.didNotContainSameElements(left, right))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain theSameElementsInOrderAs (List(1, 2))
   *                   ^
   * </pre>
   */
  def theSameElementsInOrderAs(right: GenTraversable[_])(implicit sequencing: Sequencing[L]): Assertion = {
    if (sequencing.containsTheSameElementsInOrderAs(left, right) != shouldBeTrue)
      indicateFailure(shouldBeTrue, FailureMessages.didNotContainSameElementsInOrder(left, right), FailureMessages.containedSameElementsInOrder(left, right))
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedSameElementsInOrder(left, right), FailureMessages.didNotContainSameElementsInOrder(left, right))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain only (1, 2)
   *                   ^
   * </pre>
   */
  def only(right: Any*)(implicit aggregating: Aggregating[L]): Assertion = {
    if (right.isEmpty)
      throw new NotAllowedException(FailureMessages.onlyEmpty, getStackDepthFun("ResultOfContainWord.scala", "only"))
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.onlyDuplicate, getStackDepthFun("ResultOfContainWord.scala", "only"))
    val withFriendlyReminder = right.size == 1 && (right(0).isInstanceOf[scala.collection.GenTraversable[_]] || right(0).isInstanceOf[Every[_]])
    if (aggregating.containsOnly(left, right) != shouldBeTrue) {
      indicateFailure(
        if (shouldBeTrue)
          if (withFriendlyReminder)
            FailureMessages.didNotContainOnlyElementsWithFriendlyReminder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
          else
            FailureMessages.didNotContainOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
        else
          if (withFriendlyReminder)
            FailureMessages.containedOnlyElementsWithFriendlyReminder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
          else
            FailureMessages.containedOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
    }
    else
      indicateSuccess(
        if (shouldBeTrue)
          if (withFriendlyReminder)
            FailureMessages.containedOnlyElementsWithFriendlyReminder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
          else
            FailureMessages.containedOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
        else
          if (withFriendlyReminder)
            FailureMessages.didNotContainOnlyElementsWithFriendlyReminder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
          else
            FailureMessages.didNotContainOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))

      )
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain inOrderOnly (1, 2)
   *                   ^
   * </pre>
   */
  def inOrderOnly(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit sequencing: Sequencing[L]): Assertion = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.inOrderOnlyDuplicate, getStackDepthFun("ResultOfContainWord.scala", "inOrderOnly"))
    if (sequencing.containsInOrderOnly(left, right) != shouldBeTrue)
      indicateFailure(
        shouldBeTrue,
        FailureMessages.didNotContainInOrderOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.containedInOrderOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedInOrderOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.didNotContainInOrderOnlyElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain allOf (1, 2)
   *                   ^
   * </pre>
   */
  def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[L]): Assertion = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.allOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "allOf"))
    if (aggregating.containsAllOf(left, right) != shouldBeTrue)
      indicateFailure(
        shouldBeTrue,
        FailureMessages.didNotContainAllOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.containedAllOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.didNotContainAllOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.containedAllOfElements(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain allElementsOf (1, 2)
   *                   ^
   * </pre>
   */
  def allElementsOf[R](elements: GenTraversable[R])(implicit aggregating: Aggregating[L]): Assertion = {
    val right = elements.toList
    if (aggregating.containsAllOf(left, right.distinct) != shouldBeTrue)
      indicateFailure(shouldBeTrue, FailureMessages.didNotContainAllElementsOf(left, right), FailureMessages.containedAllElementsOf(left, right))
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedAllElementsOf(left, right), FailureMessages.didNotContainAllElementsOf(left, right))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain inOrder (1, 2)
   *                   ^
   * </pre>
   */
  def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit sequencing: Sequencing[L]): Assertion = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.inOrderDuplicate, getStackDepthFun("ResultOfContainWord.scala", "inOrder"))
    if (sequencing.containsInOrder(left, right) != shouldBeTrue)
      indicateFailure(
        shouldBeTrue,
        FailureMessages.didNotContainAllOfElementsInOrder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.containedAllOfElementsInOrder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAllOfElementsInOrder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.didNotContainAllOfElementsInOrder(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain inOrderElementsOf List(1, 2)
   *                   ^
   * </pre>
   */
  def inOrderElementsOf[R](elements: GenTraversable[R])(implicit sequencing: Sequencing[L]): Assertion = {
    val right = elements.toList
    if (sequencing.containsInOrder(left, right.distinct) != shouldBeTrue)
      indicateFailure(shouldBeTrue, FailureMessages.didNotContainAllElementsOfInOrder(left, right), FailureMessages.containedAllElementsOfInOrder(left, right))
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedAllElementsOfInOrder(left, right), FailureMessages.didNotContainAllElementsOfInOrder(left, right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should contain key ("one")
   *                    ^
   * </pre>
   */
  def key(expectedKey: Any)(implicit keyMapping: KeyMapping[L]): Assertion = {
    if (keyMapping.containsKey(left, expectedKey) != shouldBeTrue)
      indicateFailure(shouldBeTrue, FailureMessages.didNotContainKey(left, expectedKey), FailureMessages.containedKey(left, expectedKey))
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedKey(left, expectedKey), FailureMessages.didNotContainKey(left, expectedKey))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should contain value ("one")
   *                    ^
   * </pre>
   */
  def value(expectedValue: Any)(implicit valueMapping: ValueMapping[L]): Assertion = {
    if (valueMapping.containsValue(left, expectedValue) != shouldBeTrue)
      indicateFailure(shouldBeTrue, FailureMessages.didNotContainValue(left, expectedValue), FailureMessages.containedValue(left, expectedValue))
    else
      indicateSuccess(shouldBeTrue, FailureMessages.containedValue(left, expectedValue), FailureMessages.didNotContainValue(left, expectedValue))
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain atMostOneOf (1, 2)
   *                   ^
   * </pre>
   */
  def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[L]): Assertion = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.atMostOneOfDuplicate, getStackDepthFun("ResultOfContainWord.scala", "atMostOneOf"))
    if (aggregating.containsAtMostOneOf(left, right) != shouldBeTrue)
      indicateFailure(
        shouldBeTrue,
        FailureMessages.didNotContainAtMostOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.containedAtMostOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAtMostOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
        FailureMessages.didNotContainAtMostOneOf(left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * xs should contain atMostOneElementOf (1, 2)
   *                   ^
   * </pre>
   */
  def atMostOneElementOf[R](elements: GenTraversable[R])(implicit aggregating: Aggregating[L]): Assertion = {
    val right = elements.toList
    if (aggregating.containsAtMostOneOf(left, right.distinct) != shouldBeTrue)
      indicateFailure(
        shouldBeTrue,
        FailureMessages.didNotContainAtMostOneElementOf(left, right),
        FailureMessages.containedAtMostOneElementOf(left, right)
      )
    else
      indicateSuccess(
        shouldBeTrue,
        FailureMessages.containedAtMostOneElementOf(left, right),
        FailureMessages.didNotContainAtMostOneElementOf(left, right)
      )
  }
  
  override def toString: String = "ResultOfContainWord(" + Prettifier.default(left) + ", " + Prettifier.default(shouldBeTrue) + ")"
}

