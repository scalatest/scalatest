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
import org.scalactic._
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
              Resources.rawHadLengthInsteadOfExpectedLength,
              Resources.rawHadLength,
              Vector(left, lengthOfLeft, expectedLength), 
              Vector(left, expectedLength)
            )
          }
          override def toString: String = "have length " + expectedLength
        }
      }
      override def toString: String = "have length " + expectedLength
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
              Resources.rawHadSizeInsteadOfExpectedSize,
              Resources.rawHadSize,
              Vector(left, sizeOfLeft, expectedSize), 
              Vector(left, expectedSize)
            )
          }
          override def toString: String = "have size " + expectedSize
        }
      }
      override def toString: String = "have size " + expectedSize
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
              Resources.rawHadMessageInsteadOfExpectedMessage,
              Resources.rawHadExpectedMessage,
              Vector(left, messageOfLeft, expectedMessage), 
              Vector(left, expectedMessage)
            )
          }
          override def toString: String = "have message " + Prettifier.default(expectedMessage)
        }
      }
      override def toString: String = "have message " + Prettifier.default(expectedMessage)
    }

  /**
   * Enables parentheses to be placed around <code>length (N)</code> in expressions of the form: <code>should have (length (N))</code>.
   */
  def apply[T](resultOfLengthWordApplication: ResultOfLengthWordApplication): MatcherFactory1[Any, Length] = length(resultOfLengthWordApplication.expectedLength)


  /**
   * Enables parentheses to be placed around <code>size (N)</code> in expressions of the form: <code>should have (size (N))</code>.
   */
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
            val (rawFailureMessage, failureMessageArgs) =
              (
                Resources.rawPropertyDidNotHaveExpectedValue,
                Vector(
                  UnquotedString(failedVerification.propertyName),
                  failedVerification.expectedValue,
                  failedVerification.actualValue,
                  left  
                )
              )
            val (rawMidSentenceFailureMessage, midSentenceFailureMessageArgs) =
              (
                Resources.rawMidSentencePropertyDidNotHaveExpectedValue,
                Vector(
                  UnquotedString(failedVerification.propertyName),
                  failedVerification.expectedValue,
                  failedVerification.actualValue,
                  left  
                )
              )

            MatchResult(false, rawFailureMessage, rawFailureMessage, rawMidSentenceFailureMessage, rawMidSentenceFailureMessage, failureMessageArgs, midSentenceFailureMessageArgs)

          case None =>

            val (rawFailureMessage, failureMessageArgs) =
              if (justOneProperty) {
                val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                (
                  Resources.rawPropertyHadExpectedValue,
                  Vector(
                    UnquotedString(firstPropertyResult.propertyName),
                    firstPropertyResult.expectedValue,
                    left  
                  )
                )
              }
              else (Resources.rawAllPropertiesHadExpectedValues, Vector(left))

            val (rawMidSentenceFailureMessage, rawMidSentenceFailureMessageArgs) =
              if (justOneProperty) {
                val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                (
                  Resources.rawMidSentencePropertyHadExpectedValue,
                  Vector(
                    UnquotedString(firstPropertyResult.propertyName),
                    firstPropertyResult.expectedValue,
                    left    
                  )
                )
              }
              else (Resources.rawMidSentenceAllPropertiesHadExpectedValues, Vector(left))

            MatchResult(true, rawFailureMessage, rawFailureMessage, rawMidSentenceFailureMessage, rawMidSentenceFailureMessage, failureMessageArgs, rawMidSentenceFailureMessageArgs)
        }
      }
      
      override def toString: String = "have (" + Prettifier.default(firstPropertyMatcher) + ")"
    }
  
  /**
   * Overrides toString to return "length"
   */
  override def toString: String = "have"
}
