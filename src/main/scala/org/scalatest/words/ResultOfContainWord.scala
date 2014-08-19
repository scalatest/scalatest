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
import org.scalatest.enablers.EvidenceThat
import org.scalatest.enablers.Aggregating
import org.scalatest.enablers.Sequencing
import org.scalatest.enablers.KeyMapping
import org.scalatest.enablers.ValueMapping
import org.scalatest.MatchersHelper.newTestFailedException
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import org.scalactic.{Prettifier, Every}

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
  def oneOf[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit evidence: EvidenceThat[R]#CanBeContainedIn[L]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages("oneOfDuplicate"), getStackDepthFun("ResultOfContainWord.scala", "oneOf"))
    if (evidence.containsOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainOneOfElements" else "containedOneOfElements",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
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
  def oneElementOf[R](elements: GenTraversable[R])(implicit evidence: EvidenceThat[R]#CanBeContainedIn[L]) {
    val right = elements.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages("oneElementOfDuplicate"), getStackDepthFun("ResultOfContainWord.scala", "oneElementOf"))
    if (evidence.containsOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainOneElementOf" else "containedOneElementOf",
          left,
          right
        )
      )
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain atLeastOneOf (1, 2)
   *                   ^
   * </pre>
   */
  def atLeastOneOf[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit aggregating: EvidenceThat[R]#CanBeContainedInAggregation[L]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages("atLeastOneOfDuplicate"), getStackDepthFun("ResultOfContainWord.scala", "atLeastOneOf"))
    if (aggregating.containsAtLeastOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainAtLeastOneOf" else "containedAtLeastOneOf",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain noneOf (1, 2)
   *                   ^
   * </pre>
   */
  def noneOf[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit containing: EvidenceThat[R]#CanBeContainedIn[L]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages("noneOfDuplicate"), getStackDepthFun("ResultOfContainWord.scala", "noneOf"))
    if (containing.containsNoneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "containedAtLeastOneOf" else "didNotContainAtLeastOneOf",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain theSameElementsAs (List(1, 2))
   *                   ^
   * </pre>
   */
  def theSameElementsAs[R](right: GenTraversable[R])(implicit aggregating: EvidenceThat[R]#CanBeContainedInAggregation[L]) {
    if (aggregating.containsTheSameElementsAs(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainSameElements" else "containedSameElements",
          left,
          right
        )
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain theSameElementsInOrderAs (List(1, 2))
   *                   ^
   * </pre>
   */
  def theSameElementsInOrderAs[R](right: GenTraversable[R])(implicit sequencing: EvidenceThat[R]#CanBeContainedInSequence[L]) {
    if (sequencing.containsTheSameElementsInOrderAs(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainSameElementsInOrder" else "containedSameElementsInOrder",
          left,
          right
        )
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain only (1, 2)
   *                   ^
   * </pre>
   */
  def only[R](right: R*)(implicit aggregating: EvidenceThat[R]#CanBeContainedInAggregation[L]) {
    if (right.isEmpty)
      throw new NotAllowedException(FailureMessages("onlyEmpty"), getStackDepthFun("ResultOfContainWord.scala", "only"))
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages("onlyDuplicate"), getStackDepthFun("ResultOfContainWord.scala", "only"))
    if (aggregating.containsOnly(left, right) != shouldBeTrue) {
      val postfix =
        if (right.size == 1 && (right(0).isInstanceOf[scala.collection.GenTraversable[_]] || right(0).isInstanceOf[Every[_]]))
          "WithFriendlyReminder"
        else
          ""
      throw newTestFailedException(
        FailureMessages(
          (if (shouldBeTrue) "didNotContainOnlyElements" else "containedOnlyElements") + postfix,
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
    }
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain inOrderOnly (1, 2)
   *                   ^
   * </pre>
   */
  def inOrderOnly[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit sequencing: EvidenceThat[R]#CanBeContainedInSequence[L]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages("inOrderOnlyDuplicate"), getStackDepthFun("ResultOfContainWord.scala", "inOrderOnly"))
    if (sequencing.containsInOrderOnly(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainInOrderOnlyElements" else "containedInOrderOnlyElements",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
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
  def allOf[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit aggregating: EvidenceThat[R]#CanBeContainedInAggregation[L]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages("allOfDuplicate"), getStackDepthFun("ResultOfContainWord.scala", "allOf"))
    if (aggregating.containsAllOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainAllOfElements" else "containedAllOfElements",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain inOrder (1, 2)
   *                   ^
   * </pre>
   */
  def inOrder[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit sequencing: EvidenceThat[R]#CanBeContainedInSequence[L]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages("inOrderDuplicate"), getStackDepthFun("ResultOfContainWord.scala", "inOrder"))
    if (sequencing.containsInOrder(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainAllOfElementsInOrder" else "containedAllOfElementsInOrder",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should contain key ("one")
   *                    ^
   * </pre>
   */
  def key(expectedKey: Any)(implicit keyMapping: KeyMapping[L]) {
    if (keyMapping.containsKey(left, expectedKey) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainKey" else "containedKey",
          left,
          expectedKey)
      )
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should contain value ("one")
   *                    ^
   * </pre>
   */
  def value(expectedValue: Any)(implicit valueMapping: ValueMapping[L]) {
    if (valueMapping.containsValue(left, expectedValue) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainValue" else "containedValue",
          left,
          expectedValue)
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * xs should contain atMostOneOf (1, 2)
   *                   ^
   * </pre>
   */
  def atMostOneOf[R](firstEle: R, secondEle: R, remainingEles: R*)(implicit aggregating: EvidenceThat[R]#CanBeContainedInAggregation[L]) {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages("atMostOneOfDuplicate"), getStackDepthFun("ResultOfContainWord.scala", "atMostOneOf"))
    if (aggregating.containsAtMostOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainAtMostOneOf" else "containedAtMostOneOf",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }
  
  override def toString: String = "ResultOfContainWord(" + Prettifier.default(left) + ", " + Prettifier.default(shouldBeTrue) + ")"
}

