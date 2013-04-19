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
import org.scalatest.Matchers.newTestFailedException
import org.scalatest.Helper.accessProperty

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
            val lengthOfLeft = length.extentOf(left)
            MatchResult(
              lengthOfLeft == expectedLength,
              // FailureMessages("hadLengthInsteadOfExpectedLength", left, lengthOfLeft, expectedLength),
              FailureMessages("hadLengthInsteadOfExpectedLength", left, lengthOfLeft, expectedLength),
              FailureMessages("hadExpectedLength", left, expectedLength)
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
        val length = implicitly[Size[T]]
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val lengthOfLeft = length.extentOf(left)
            MatchResult(
              lengthOfLeft == expectedSize,
              // FailureMessages("hadSizeInsteadOfExpectedSize", left, lengthOfLeft, expectedSize),
              FailureMessages("hadSizeInsteadOfExpectedSize", left, lengthOfLeft, expectedSize),
              FailureMessages("hadExpectedSize", left, expectedSize)
            )
          }
        }
      }
    }

/*
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult =
        left match {
          case leftArray: Array[_] =>
            MatchResult(
              leftArray.length == expectedSize, 
              FailureMessages("hadSizeInsteadOfExpectedSize", left, leftArray.length, expectedSize),
              FailureMessages("hadExpectedSize", left, expectedSize)
            )
          case leftTrav: GenTraversable[_] =>
            MatchResult(
              leftTrav.size == expectedSize, 
              FailureMessages("hadSizeInsteadOfExpectedSize", left, leftTrav.size, expectedSize),
              FailureMessages("hadExpectedSize", left, expectedSize)
            )
          case leftJavaList: java.util.List[_] =>
            MatchResult(
              leftJavaList.size == expectedSize,
              FailureMessages("hadSizeInsteadOfExpectedSize", left, leftJavaList.size, expectedSize),
              FailureMessages("hadExpectedSize", left, expectedSize)
            )
          case _ =>

            accessProperty(left, 'size, false) match {

              case None =>

                throw newTestFailedException(Resources("noSizeStructure", expectedSize.toString))

              case Some(result) =>

                MatchResult(
                  result == expectedSize,
                  FailureMessages("hadSizeInsteadOfExpectedSize", left, result, expectedSize),
                  FailureMessages("hadExpectedSize", left, expectedSize)
                )
            }
        }
    }
*/

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
