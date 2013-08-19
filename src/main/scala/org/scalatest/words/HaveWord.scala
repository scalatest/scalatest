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
import org.scalautils._
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.Resources
import scala.collection.GenTraversable
import scala.collection.GenSeq
import org.scalatest.MatchersHelper.accessProperty

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class HaveWord {

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * book should have length (9)
   *                  ^
   * </pre>
   */
  def length(expectedLength: Long): MatcherFactory1[Any, Length] =
    new MatcherFactory1[Any, Length] {
      def matcher[T <: Any : Length]: Matcher[T] = {
        val length = implicitly[Length[T]]
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val lengthOfLeft = length.lengthOf(left)
            MatchResult(
              lengthOfLeft == expectedLength,
              // FailureMessages("hadLengthInsteadOfExpectedLength", left, lengthOfLeft, expectedLength),
              FailureMessages("hadLengthInsteadOfExpectedLength", left, lengthOfLeft, expectedLength),
              FailureMessages("hadLength", left, expectedLength)
            )
          }
        }
      }
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * book should have size (9)
   *                  ^
   * </pre>
   *
   * <p>
   * Currently, this method will produce a <code>Matcher[AnyRef]</code>, and if the
   * <code>AnyRef</code> passed to that matcher's <code>apply</code> method does not have the appropriate <code>size</code> property
   * structure, all will compile but a <code>TestFailedException</code> will result at runtime explaining the problem.
   * In a future ScalaTest release, this may be tightened so that all is statically checked at compile time.
   * </p>
   */
  def size(expectedSize: Long): MatcherFactory1[Any, Size] =
    new MatcherFactory1[Any, Size] {
      def matcher[T <: Any : Size]: Matcher[T] = {
        val size = implicitly[Size[T]]
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val sizeOfLeft = size.sizeOf(left)
            MatchResult(
              sizeOfLeft == expectedSize,
              // FailureMessages("hadSizeInsteadOfExpectedSize", left, lengthOfLeft, expectedSize),
              FailureMessages("hadSizeInsteadOfExpectedSize", left, sizeOfLeft, expectedSize),
              FailureMessages("hadSize", left, expectedSize)
            )
          }
        }
      }
    }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should have message ("A message from Mars!")
   *                    ^
   * </pre>
   */
  def message(expectedMessage: String): MatcherFactory1[Any, Messaging] =
    new MatcherFactory1[Any, Messaging] {
      def matcher[T <: Any : Messaging]: Matcher[T] = {
        val messaging = implicitly[Messaging[T]]
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val messageOfLeft = messaging.messageOf(left)
            MatchResult(
              messageOfLeft == expectedMessage,
              FailureMessages("hadMessageInsteadOfExpectedMessage"),
              FailureMessages("hadExpectedMessage"), 
              Vector(left, messageOfLeft, expectedMessage), 
              Vector(left, expectedMessage)
            )
          }
        }
      }
    }

    // TODO: Write tests and implement cases for:
    // have(length (9), title ("hi")) (this one we'll use this apply method but add a HavePropertyMatcher* arg)
    // have(size (9), title ("hi")) (this one we'll use the next apply method but add a HavePropertyMatcher* arg)
    // have(length(9), size (9), title ("hi")) (for this one we'll need a new overloaded apply(ROLWA, ROSWA, HPM*))
    // have(size(9), length (9), title ("hi")) (for this one we'll need a new overloaded apply(ROSWA, ROLWA, HPM*))
  // TODO: Scaladoc, and figure out a way that length can be used as a general have property matcher, or give them another method maybe?
  def apply[T](resultOfLengthWordApplication: ResultOfLengthWordApplication): MatcherFactory1[Any, Length] = length(resultOfLengthWordApplication.expectedLength)


  // TODO: Scaladoc, and figure out a way that length can be used as a general have property matcher, or give them another method maybe?
  def apply[T](resultOfSizeWordApplication: ResultOfSizeWordApplication): MatcherFactory1[Any, Size] = size(resultOfSizeWordApplication.expectedSize)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * book should have (title ("A Tale of Two Cities"))
   *                  ^
   * </pre>
   */
  def apply[T](firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*): Matcher[T] =

    new Matcher[T] {

      def apply(left: T): MatchResult = {

        val results =
          for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
            propertyVerifier(left)

        val firstFailureOption = results.find(pv => !pv.matches)

        val justOneProperty = propertyMatchers.length == 0

        firstFailureOption match {

          case Some(firstFailure) =>

            val failedVerification = firstFailure
            val failureMessage =
              FailureMessages(
                "propertyDidNotHaveExpectedValue",
                UnquotedString(failedVerification.propertyName),
                failedVerification.expectedValue,
                failedVerification.actualValue,
                left
              )
            val midSentenceFailureMessage =
              FailureMessages(
                "midSentencePropertyDidNotHaveExpectedValue",
                UnquotedString(failedVerification.propertyName),
                failedVerification.expectedValue,
                failedVerification.actualValue,
                left
              )

            MatchResult(false, failureMessage, failureMessage, midSentenceFailureMessage, midSentenceFailureMessage)

          case None =>

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

            val midSentenceFailureMessage =
              if (justOneProperty) {
                val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                FailureMessages(
                  "midSentencePropertyHadExpectedValue",
                  UnquotedString(firstPropertyResult.propertyName),
                  firstPropertyResult.expectedValue,
                  left
                )
              }
              else FailureMessages("midSentenceAllPropertiesHadExpectedValues", left)

            MatchResult(true, failureMessage, failureMessage, midSentenceFailureMessage, midSentenceFailureMessage)
        }
      }
    }
}
