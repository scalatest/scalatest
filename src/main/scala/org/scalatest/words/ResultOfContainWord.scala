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
import org.scalatest.MatchersHelper.newTestFailedException
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString

/*
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
  def oneOf(right: Any*)(implicit containing: Containing[L]) {
    if (containing.containsOneOf(left, right) != shouldBeTrue)
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
   * xs should contain atLeastOneOf (1, 2)
   *                   ^
   * </pre>
   */
  def atLeastOneOf(right: Any*)(implicit aggregating: Aggregating[L]) {
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
  def noneOf(right: Any*)(implicit containing: Containing[L]) {
    if (containing.containsNoneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "containedOneOfElements" else "didNotContainOneOfElements",
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
  def theSameElementsAs(right: GenTraversable[_])(implicit aggregating: Aggregating[L]) {
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
  def theSameElementsInOrderAs(right: GenTraversable[_])(implicit sequencing: Sequencing[L]) {
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
  def only(right: Any*)(implicit aggregating: Aggregating[L]) {
    if (aggregating.containsOnly(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainOnlyElements" else "containedOnlyElements",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
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
  def inOrderOnly(right: Any*)(implicit sequencing: Sequencing[L]) {
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
  def allOf(right: Any*)(implicit aggregating: Aggregating[L]) {
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
  def inOrder(right: Any*)(implicit sequencing: Sequencing[L]) {
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
  def newKey(expectedKey: Any)(implicit keyMapping: KeyMapping[L]) {
    // if (left.exists(_._1 == expectedKey) != shouldBeTrue)
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
  def newValue(expectedValue: Any)(implicit valueMapping: ValueMapping[L]) {
    if (valueMapping.containsValue(left, expectedValue) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainValue" else "containedValue",
          left,
          expectedValue)
      )
  }
}

