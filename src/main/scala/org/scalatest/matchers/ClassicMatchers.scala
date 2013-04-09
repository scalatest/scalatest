/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.matchers

import org.scalatest._
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import scala.util.matching.Regex
import java.lang.reflect.Field
import scala.reflect.Manifest
import Helper.transformOperatorChars
import scala.collection.Traversable
import Assertions.areEqualComparingArraysStructurally
import org.scalatest.exceptions.TestFailedException
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap

// TODO: drop generic support for be as an equality comparison, in favor of specific ones.
// TODO: mention on JUnit and TestNG docs that you can now mix in ShouldMatchers or MustMatchers
// TODO: Put links from ShouldMatchers to wherever I reveal the matrix and algo of how properties are checked dynamically.
// TODO: double check that I wrote tests for (length (7)) and (size (8)) in parens
// TODO: document how to turn off the === implicit conversion
// TODO: Document you can use JMock, EasyMock, etc.

private[scalatest] object Helper {

  // If the symbol passed is 'title, this will look for a field named "title", a method named "title", or a
  // method named "getTitle". The method must take no parameters.
  //
  // F (field) | M (method) | G (get or is method) | Result
  // 0           0            0                      None
  // 0           0            1                      Some(G)
  // 0           1            0                      Some(M)
  // 0           1            1                      Some(M) prefer a Scala style one of a Java style, such as when using BeanProperty annotation
  // 1           0            0                      Some(F) ignore the field if there's a method. in Java often name a field and get method the same
  // 1           0            1                      Some(G)
  // 1           1            0                      Some(M)
  // 1           1            1                      Some(M) prefer a Scala style one of a Java style, such as when using BeanProperty annotation
  // 
  def accessProperty(objectWithProperty: AnyRef, propertySymbol: Symbol, isBooleanProperty: Boolean): Option[Any] = {

    // If 'title passed, propertyName would be "title"
    val propertyName = propertySymbol.name

    // if propertyName is '>, mangledPropertyName would be "$greater"
    val mangledPropertyName = transformOperatorChars(propertyName)

    // fieldNameToAccess and methodNameToInvoke would also be "title"
    val fieldNameToAccess = mangledPropertyName
    val methodNameToInvoke = mangledPropertyName

    // methodNameToInvokeWithGet would be "getTitle"
    val prefix = if (isBooleanProperty) "is" else "get"
    val methodNameToInvokeWithGet = prefix + mangledPropertyName(0).toUpper + mangledPropertyName.substring(1)

    val firstChar = propertyName(0).toLower
    val methodNameStartsWithVowel = firstChar == 'a' || firstChar == 'e' || firstChar == 'i' ||
      firstChar == 'o' || firstChar == 'u'

    def isFieldToAccess(field: Field): Boolean = field.getName == fieldNameToAccess

    // If it is a predicate, I check the result type, otherwise I don't. Maybe I should just do that. Could be a later enhancement.
    def isMethodToInvoke(method: Method): Boolean =
      method.getName == methodNameToInvoke && method.getParameterTypes.length == 0 && !Modifier.isStatic(method.getModifiers()) &&
        (!isBooleanProperty || method.getReturnType == classOf[Boolean])

    def isGetMethodToInvoke(method: Method): Boolean =
      method.getName == methodNameToInvokeWithGet && method.getParameterTypes.length == 0 && !Modifier.isStatic(method.getModifiers()) &&
        (!isBooleanProperty || method.getReturnType == classOf[Boolean])

    val fieldOption = objectWithProperty.getClass.getFields.find(isFieldToAccess)

    val methodOption = objectWithProperty.getClass.getMethods.find(isMethodToInvoke)

    val getMethodOption = objectWithProperty.getClass.getMethods.find(isGetMethodToInvoke)

    (fieldOption, methodOption, getMethodOption) match {

      case (_, Some(method), _) => Some(method.invoke(objectWithProperty, Array[AnyRef](): _*))

      case (_, None, Some(getMethod)) => Some(getMethod.invoke(objectWithProperty, Array[AnyRef](): _*))

      case (Some(field), None, None) => Some(field.get(objectWithProperty))

      case (None, None, None) => None
    }
  }

  def transformOperatorChars(s: String): String = {
    val builder = new StringBuilder
    for (i <- 0 until s.length) {
      val ch = s.charAt(i)
      val replacement =
        ch match {
          case '!' => "$bang"
          case '#' => "$hash"
          case '~' => "$tilde"
          case '|' => "$bar"
          case '^' => "$up"
          case '\\' => "$bslash"
          case '@' => "$at"
          case '?' => "$qmark"
          case '>' => "$greater"
          case '=' => "$eq"
          case '<' => "$less"
          case ':' => "$colon"
          case '/' => "$div"
          case '-' => "$minus"
          case '+' => "$plus"
          case '*' => "$times"
          case '&' => "$amp"
          case '%' => "$percent"
          case _ => ""
        }

      if (replacement.length > 0)
        builder.append(replacement)
      else
        builder.append(ch)
    }
    builder.toString
  }
}

import Helper.accessProperty

/**
 * This trait is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html">ShouldMatchers</a> or
 * <a href="MustMatchers.html">MustMatchers</a> for an overview of the matchers DSL.
 *
 * @author Bill Venners
 */
trait ClassicMatchers extends Assertions { matchers =>

  // TODO: Can probably rewrite this with a Thread.currentStackTrace or whatever the method is. No need
  // to create the temporary RuntimeException
  private[scalatest] def newTestFailedException(message: String, optionalCause: Option[Throwable] = None): Throwable = {
    val fileNames = List("Matchers.scala", "ShouldMatchers.scala", "MustMatchers.scala")
    val temp = new RuntimeException
    val stackDepth = temp.getStackTrace.takeWhile(stackTraceElement => fileNames.exists(_ == stackTraceElement.getFileName) || stackTraceElement.getMethodName == "newTestFailedException").length
    // if (stackDepth != 4) throw new OutOfMemoryError("stackDepth in Matchers.scala is: " + stackDepth)
    optionalCause match {
      case Some(cause) => new TestFailedException(message, cause, stackDepth)
      case None => new TestFailedException(message, stackDepth)
    }
  }

  private def matchSymbolToPredicateMethod[S <: AnyRef](left: S, right: Symbol, hasArticle: Boolean, articleIsA: Boolean): MatchResult = {

    // If 'empty passed, rightNoTick would be "empty"
    val propertyName = right.name

    accessProperty(left, right, true) match {

      case None =>

        // if propertyName is '>, mangledPropertyName would be "$greater"
        val mangledPropertyName = transformOperatorChars(propertyName)

        // methodNameToInvoke would also be "empty"
        val methodNameToInvoke = mangledPropertyName

        // methodNameToInvokeWithIs would be "isEmpty"
        val methodNameToInvokeWithIs = "is"+ mangledPropertyName(0).toUpper + mangledPropertyName.substring(1)

        val firstChar = propertyName(0).toLower
        val methodNameStartsWithVowel = firstChar == 'a' || firstChar == 'e' || firstChar == 'i' ||
          firstChar == 'o' || firstChar == 'u'

        throw newTestFailedException(
          FailureMessages(
            if (methodNameStartsWithVowel) "hasNeitherAnOrAnMethod" else "hasNeitherAOrAnMethod",
            left,
            UnquotedString(methodNameToInvoke),
            UnquotedString(methodNameToInvokeWithIs)
          )
        )

      case Some(result) =>

        val (wasNot, was) =
          if (hasArticle) {
            if (articleIsA) ("wasNotA", "wasA") else ("wasNotAn", "wasAn")
          }
          else ("wasNot", "was")

        MatchResult(
          result == true, // Right now I just leave the return value of accessProperty as Any
          FailureMessages(wasNot, left, UnquotedString(propertyName)),
          FailureMessages(was, left, UnquotedString(propertyName))
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class MatcherWrapper[T](leftMatcher: Matcher[T]) { matchersWrapper =>

// TODO: mention not short circuited, and the precendence is even between and and or

    /**
     * Returns a matcher whose <code>apply</code> method returns a <code>MatchResult</code>
     * that represents the logical-and of the results of the wrapped and the passed matcher applied to
     * the same value.
     *
     * <p>
     * The reason <code>and</code> has an upper bound on its type parameter is so that the <code>Matcher</code>
     * resulting from an invocation of <code>and</code> will have the correct type parameter. If you call
     * <code>and</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Valencia]</code>,
     * the result will have type <code>Matcher[Valencia]</code>. This is correct because both a
     * <code>Matcher[Orange]</code> and a <code>Matcher[Valencia]</code> know how to match a
     * <code>Valencia</code> (but a <code>Matcher[Valencia]</code> doesn't know how to
     * match any old <code>Orange</code>).  If you call
     * <code>and</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Fruit]</code>,
     * the result will have type <code>Matcher[Orange]</code>. This is also correct because both a
     * <code>Matcher[Orange]</code> and a <code>Matcher[Fruit]</code> know how to match an
     * <code>Orange</code> (but a <code>Matcher[Orange]</code> doesn't know how to
     * match any old <code>Fruit</code>).
     * </p>
     *
     * @param the matcher to logical-and with this matcher
     * @return a matcher that performs the logical-and of this and the passed matcher
     */
    def and[U <: T](rightMatcher: Matcher[U]): Matcher[U] =
      new Matcher[U] {
        def apply(left: U): MatchResult = {
          val leftMatchResult = leftMatcher(left)
          val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
          if (!leftMatchResult.matches)
            MatchResult(
              false,
              leftMatchResult.failureMessage,
              leftMatchResult.negatedFailureMessage,
              leftMatchResult.midSentenceFailureMessage,
              leftMatchResult.midSentenceNegatedFailureMessage
            )
          else {
            MatchResult(
              rightMatchResult.matches,
              Resources("commaBut", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
              Resources("commaAnd", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
              Resources("commaBut", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
              Resources("commaAnd", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
            )
          }
        }
      }

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndHaveWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (have length (2) and have length (3 - 1))
       *                                              ^
       * </pre>
       */
      def length(expectedLength: Long): Matcher[T with AnyRef] = and(have.length(expectedLength))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (have size (2) and have size (3 - 1))
       *                                            ^ 
       * </pre>
       */
      def size(expectedSize: Long): Matcher[T with AnyRef] = and(have.size(expectedSize))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (have size (2) and have size (3 - 1))
     *                                   ^ 
     * </pre>
     */
    def and(haveWord: HaveWord): AndHaveWord = new AndHaveWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndContainWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (contain (2) and contain (3 - 1))
       *                                     ^
       * </pre>
       */
      def apply[U](expectedElement: U): Matcher[T with GenTraversable[U]] = matchersWrapper.and(matchers.contain(expectedElement))
      // def element[T](expectedElement: T) = matchersWrapper.and(matchers.contain.apply(expectedElement))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (contain key ("two") and contain key ("one"))
       *                                                                     ^
       * </pre>
       */
      def key[U](expectedElement: U): Matcher[T with scala.collection.GenMap[U, Any]] = matchersWrapper.and(matchers.contain.key(expectedElement))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (contain value (2) and contain value (1))
       *                                                                   ^
       * </pre>
       */
      def value[U](expectedValue: U): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] = matchersWrapper.and(matchers.contain.value(expectedValue))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (contain key ("two") and contain key ("one"))
     *                                                         ^ 
     * </pre>
     */
    def and(containWord: ContainWord): AndContainWord = new AndContainWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndBeWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isFileMock should (be a ('file) and be a ('file))
       *                                        ^
       * </pre>
       */
      def a(symbol: Symbol): Matcher[T with AnyRef] = and(be.a(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (be a (file) and be a (file))
       *                                        ^
       * </pre>
       */
      def a[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = and(be.a(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isAppleMock should (be an ('apple) and be an ('apple))
       *                                           ^
       * </pre>
       */
      def an(symbol: Symbol): Matcher[T with AnyRef] = and(be.an(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isAppleMock should (be an (apple) and be an (apple))
       *                                           ^
       * </pre>
       */
      def an[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = and(be.an(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * obj should (be theSameInstanceAs (string) and be theSameInstanceAs (string))
       *                                                  ^
       * </pre>
       */
      def theSameInstanceAs(anyRef: AnyRef): Matcher[T with AnyRef] = and(be.theSameInstanceAs(anyRef))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isFileMock should (be a ('file) and be a ('file))
     *                                 ^
     * </pre>
     */
    def and(beWord: BeWord): AndBeWord = new AndBeWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndFullyMatchWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (fullyMatch regex (decimal) and fullyMatch regex (decimal))
       *                                                         ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = and(fullyMatch.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
       *                                                              ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = and(fullyMatch.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
     *                                               ^
     * </pre>
     */
    def and(fullyMatchWord: FullyMatchWord): AndFullyMatchWord = new AndFullyMatchWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndIncludeWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (include regex (decimal) and include regex (decimal))
       *                                                   ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = and(include.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (include regex (decimalRegex) and include regex (decimalRegex))
       *                                                        ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = and(include.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "hello, world" should (include regex ("hel*o") and include regex ("wor.d"))
     *                                           ^
     * </pre>
     */
    def and(includeWord: IncludeWord): AndIncludeWord = new AndIncludeWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndStartWithWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (startWith regex (decimal) and startWith regex (decimal))
       *                                                       ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = and(startWith.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (startWith regex (decimalRegex) and startWith regex (decimalRegex))
       *                                                            ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = and(startWith.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.78" should (have length (4) and startWith regex ("1.7"))
     *                                ^
     * </pre>
     */
    def and(startWithWord: StartWithWord): AndStartWithWord = new AndStartWithWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndEndWithWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (endWith regex (decimal) and endWith regex (decimal))
       *                                                   ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = and(endWith.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
       *                                                        ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = and(endWith.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
     *                                            ^
     * </pre>
     */
    def and(endWithWord: EndWithWord): AndEndWithWord = new AndEndWithWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndNotWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 1 should (not equal (2) and not equal (3 - 1))
       *                                 ^
       * </pre>
       */
      def equal(any: Any): Matcher[T] =
        matchersWrapper.and(matchers.not.apply(matchers.equal(any)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 1 should (not be (2) and not be (3 - 1))
       *                              ^
       * </pre>
       */
      def be(any: Any): Matcher[T] =
        matchersWrapper.and(matchers.not.apply(matchers.be(any)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not have size (5) and not have length (3))
       *                                               ^
       * </pre>
       */
      def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): Matcher[T with AnyRef] =
        matchersWrapper.and(matchers.not.apply(matchers.have.length(resultOfLengthWordApplication.expectedLength)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not have size (5) and not have size (3))
       *                                               ^
       * </pre>
       */
      def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): Matcher[T with AnyRef] =
        matchersWrapper.and(matchers.not.apply(matchers.have.size(resultOfSizeWordApplication.expectedSize)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * book should (not have (title ("Moby Dick")) and not have (author ("Melville")))
       *                                                     ^
       * </pre>
       */
      def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Matcher[T with U] =
        matchersWrapper.and(matchers.not.apply(matchers.have(firstPropertyMatcher, propertyMatchers: _*)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 5 should (not be < (2) and not be < (6))
       *                                ^
       * </pre>
       */
      def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): Matcher[T with U] =
        matchersWrapper.and(matchers.not.be(resultOfLessThanComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * map should (contain key (7) and not be (null))
       *                                     ^
       * </pre>
       */
      def be(o: Null): Matcher[T with AnyRef] = matchersWrapper.and(matchers.not.be(o))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 7 should (not be > (8) and not be > (6))
       *                                ^
       * </pre>
       */
      def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): Matcher[T with U] =
        matchersWrapper.and(matchers.not.be(resultOfGreaterThanComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 2 should (not be <= (1) and not be <= (2))
       *                                 ^
       * </pre>
       */
      def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): Matcher[T with U] =
        matchersWrapper.and(matchers.not.be(resultOfLessThanOrEqualToComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 7 should (not be >= (8) and not be >= (6))
       *                                 ^
       * </pre>
       */
      def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): Matcher[T with U] =
        matchersWrapper.and(matchers.not.be(resultOfGreaterThanOrEqualToComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 5 should (not be === (2) and not be === (6))
       *                                  ^
       * </pre>
       */
      def be(resultOfTripleEqualsApplication: ResultOfTripleEqualsApplication): Matcher[T] =
        matchersWrapper.and(matchers.not.be(resultOfTripleEqualsApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * notEmptyMock should (not be ('empty) and not be ('empty))
       *                                              ^
       * </pre>
       */
      def be(symbol: Symbol): Matcher[T with AnyRef] = matchersWrapper.and(matchers.not.be(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 2 should (not be (odd) and not be (odd))
       *                                ^
       * </pre>
       */
      def be[U](beMatcher: BeMatcher[U]): Matcher[T with U] = matchersWrapper.and(matchers.not.be(beMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be (directory) and not be (directory))
       *                                              ^
       * </pre>
       */
      def be[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = matchersWrapper.and(matchers.not.be(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isNotFileMock should (not be a ('file) and not be a ('file))
       *                                                ^
       * </pre>
       */
      def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T with AnyRef] = matchersWrapper.and(matchers.not.be(resultOfAWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be a (directory) and not be a (directory))
       *                                             ^
       * </pre>
       */
      def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Matcher[T with U] = matchersWrapper.and(matchers.not.be(resultOfAWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isNotAppleMock should (not be an ('apple) and not be an ('apple)) 
       *                                                   ^
       * </pre>
       */
      def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T with AnyRef] = matchersWrapper.and(matchers.not.be(resultOfAnWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be an (directory) and not be an (directory))
       *                                              ^
       * </pre>
       */
      def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[T]) = matchersWrapper.and(matchers.not.be(resultOfAnWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * obj should (not be theSameInstanceAs (otherString) and not be theSameInstanceAs (otherString))
       *                                                            ^
       * </pre>
       */
      def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T with AnyRef] = matchersWrapper.and(matchers.not.be(resultOfTheSameInstanceAsApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenDotOh should (not be (17.0 plusOrMinus 0.2) and not be (17.0 plusOrMinus 0.2))
       *                                                          ^
       * </pre>
       */
      def be(doubleTolerance: DoubleTolerance): Matcher[T with Double] = matchersWrapper.and(matchers.not.be(doubleTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenDotOhFloat should (not be (17.0f plusOrMinus 0.2f) and not be (17.0f plusOrMinus 0.2f))
       *                                                                 ^
       * </pre>
       */
      def be(floatTolerance: FloatTolerance): Matcher[T with Float] = matchersWrapper.and(matchers.not.be(floatTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenLong should (not be (17L plusOrMinus 2L) and not be (17L plusOrMinus 2L))
       *                                                       ^
       * </pre>
       */
      def be(longTolerance: LongTolerance): Matcher[T with Long] = matchersWrapper.and(matchers.not.be(longTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenInt should (not be (17 plusOrMinus 2) and not be (17 plusOrMinus 2))
       *                                                    ^
       * </pre>
       */
      def be(intTolerance: IntTolerance): Matcher[T with Int] = matchersWrapper.and(matchers.not.be(intTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenShort should (not be (17.toShort plusOrMinus 2.toShort) and not be (17.toShort plusOrMinus 2.toShort))
       *                                                                      ^
       * </pre>
       */
      def be(shortTolerance: ShortTolerance): Matcher[T with Short] = matchersWrapper.and(matchers.not.be(shortTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenByte should ((not be (19.toByte plusOrMinus 2.toByte)) and (not be (19.toByte plusOrMinus 2.toByte)))
       *                                                                      ^
       * </pre>
       */
      def be(byteTolerance: ByteTolerance): Matcher[T with Byte] = matchersWrapper.and(matchers.not.be(byteTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not fullyMatch regex ("bob") and not fullyMatch regex (decimal))
       *                                                     ^
       * </pre>
       */
      def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.and(matchers.not.fullyMatch(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not include regex ("bob") and not include regex (decimal))
       *                                                     ^
       * </pre>
       */
      def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.and(matchers.not.include(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not include ("bob") and not include ("1.7"))
       *                                            ^
       * </pre>
       */
      def include(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.and(matchers.not.include(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not startWith regex ("bob") and not startWith regex (decimal))
       *                                                    ^
       * </pre>
       */
      def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.and(matchers.not.startWith(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not startWith ("red") and not startWith ("1.7"))
       *                                              ^
       * </pre>
       */
      def startWith(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.and(matchers.not.startWith(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not endWith regex ("bob") and not endWith regex (decimal))
       *                                                  ^
       * </pre>
       */
      def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.and(matchers.not.endWith(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not endWith ("fre") and not endWith ("1.7"))
       *                                            ^
       * </pre>
       */
      def endWith(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.and(matchers.not.endWith(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not contain (5) and not contain (3))
       *                                                     ^
       * </pre>
       */
      def contain[U](expectedElement: U): Matcher[T with GenTraversable[U]] =
        matchersWrapper.and(matchers.not.contain(expectedElement))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (not contain key ("five") and not contain key ("three"))
       *                                                                      ^
       * </pre>
       */
      def contain[U](resultOfKeyWordApplication: ResultOfKeyWordApplication[U]): Matcher[T with scala.collection.GenMap[U, Any]] =
        matchersWrapper.and(matchers.not.contain(resultOfKeyWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (not contain value (5) and not contain value (3))
       *                                                                   ^
       * </pre>
       */
      def contain[U](resultOfValueWordApplication: ResultOfValueWordApplication[U]): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] =
        matchersWrapper.and(matchers.not.contain(resultOfValueWordApplication))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain value (5) and not contain value (3))
     *                                                           ^
     * </pre>
     */
    def and(notWord: NotWord): AndNotWord = new AndNotWord

    /**
     * Returns a matcher whose <code>apply</code> method returns a <code>MatchResult</code>
     * that represents the logical-or of the results of this and the passed matcher applied to
     * the same value.
     *
     * <p>
     * The reason <code>or</code> has an upper bound on its type parameter is so that the <code>Matcher</code>
     * resulting from an invocation of <code>or</code> will have the correct type parameter. If you call
     * <code>or</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Valencia]</code>,
     * the result will have type <code>Matcher[Valencia]</code>. This is correct because both a
     * <code>Matcher[Orange]</code> and a <code>Matcher[Valencia]</code> know how to match a
     * <code>Valencia</code> (but a <code>Matcher[Valencia]</code> doesn't know how to
     * match any old <code>Orange</code>).  If you call
     * <code>or</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Fruit]</code>,
     * the result will have type <code>Matcher[Orange]</code>. This is also correct because both a
     * <code>Matcher[Orange]</code> and a <code>Matcher[Fruit]</code> know how to match an
     * <code>Orange</code> (but a <code>Matcher[Orange]</code> doesn't know how to
     * match any old <code>Fruit</code>).
     * </p>
     *
     * @param the matcher to logical-or with this matcher
     * @return a matcher that performs the logical-or of this and the passed matcher
     */
    def or[U <: T](rightMatcher: Matcher[U]): Matcher[U] =
      new Matcher[U] {
        def apply(left: U): MatchResult = {
          val leftMatchResult = leftMatcher(left)
          val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
          if (leftMatchResult.matches)
            MatchResult(
              true,
              leftMatchResult.negatedFailureMessage,
              leftMatchResult.failureMessage,
              leftMatchResult.midSentenceNegatedFailureMessage,
              leftMatchResult.midSentenceFailureMessage
            )
          else {
            MatchResult(
              rightMatchResult.matches,
              Resources("commaAnd", leftMatchResult.failureMessage, rightMatchResult.midSentenceFailureMessage),
              Resources("commaAnd", leftMatchResult.failureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
              Resources("commaAnd", leftMatchResult.midSentenceFailureMessage, rightMatchResult.midSentenceFailureMessage),
              Resources("commaAnd", leftMatchResult.midSentenceFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
            )
          }
        }
      }

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrHaveWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (have length (2) and have length (3 - 1))
       *                                              ^
       * </pre>
       */
      def length(expectedLength: Long): Matcher[T with AnyRef] = or(have.length(expectedLength))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (have size (2) and have size (3 - 1))
       *                                       ^
       * </pre>
       */
      def size(expectedSize: Long): Matcher[T with AnyRef] = or(have.size(expectedSize))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (have size (2) and have size (3 - 1))
     *                                   ^
     * </pre>
     */
    def or(haveWord: HaveWord): OrHaveWord = new OrHaveWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrContainWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (contain (2) or contain (3 - 1))
       *                                            ^
       * </pre>
       */
      def apply[U](expectedElement: U): Matcher[T with GenTraversable[U]] = matchersWrapper.or(matchers.contain(expectedElement))
      // def element[T](expectedElement: T) = matchersWrapper.or(matchers.contain.apply(expectedElement))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (contain key ("cat") or contain key ("one"))
       *                                                                    ^
       * </pre>
       */
      def key[U](expectedKey: U): Matcher[T with scala.collection.GenMap[U, Any]] = matchersWrapper.or(matchers.contain.key(expectedKey))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (contain value (7) or contain value (1))
       *                                                                  ^
       * </pre>
       */
      def value[U](expectedValue: U): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] = matchersWrapper.or(matchers.contain.value(expectedValue))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (contain value (7) or contain value (1))
     *                                                       ^
     * </pre>
     */
    def or(containWord: ContainWord): OrContainWord = new OrContainWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrBeWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isFileMock should (be a ('file) or be a ('directory))
       *                                       ^
       * </pre>
       */
      def a(symbol: Symbol): Matcher[T with AnyRef] = or(be.a(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isFileMock should (be a (file) or be a (directory))
       *                                      ^
       * </pre>
       */
      def a[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = or(be.a(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * appleMock should (be an ('orange) or be an ('apple))
       *                                         ^
       * </pre>
       */
      def an(symbol: Symbol): Matcher[T with AnyRef] = or(be.an(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * appleMock should (be an (orange) or be an (apple))
       *                                        ^
       * </pre>
       */
      def an[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = or(be.an(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * obj should (be theSameInstanceAs (string) or be theSameInstanceAs (otherString))
       *                                                 ^
       * </pre>
       */
      def theSameInstanceAs(anyRef: AnyRef): Matcher[T with AnyRef] = or(be.theSameInstanceAs(anyRef))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isFileMock should (be a ('file) or be a ('directory))
     *                                 ^
     * </pre>
     */
    def or(beWord: BeWord): OrBeWord = new OrBeWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrFullyMatchWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
       *                                                        ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = or(fullyMatch.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
       *                                                        ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = or(fullyMatch.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
     *                                          ^
     * </pre>
     */
    def or(fullyMatchWord: FullyMatchWord): OrFullyMatchWord = new OrFullyMatchWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrIncludeWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (include regex ("hello") or include regex (decimal))
       *                                                  ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = or(include.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (include regex ("hello") or include regex (decimal))
       *                                                  ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = or(include.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "a1.7b" should (include regex ("1.7") or include regex ("1.7"))
     *                                          ^
     * </pre>
     */
    def or(includeWord: IncludeWord): OrIncludeWord = new OrIncludeWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrStartWithWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (startWith regex ("hello") or startWith regex (decimal))
       *                                                      ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = or(startWith.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (startWith regex ("hello") or startWith regex (decimal))
       *                                                      ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = or(startWith.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (startWith regex ("hello") or startWith regex ("1.7"))
     *                                            ^
     * </pre>
     */
    def or(startWithWord: StartWithWord): OrStartWithWord = new OrStartWithWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrEndWithWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (endWith regex ("hello") or endWith regex (decimal))
       *                                                  ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = or(endWith.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (endWith regex ("hello") or endWith regex (decimal))
       *                                                  ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = or(endWith.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7b" should (endWith regex ("hello") or endWith regex ("7b"))
     *                                           ^
     * </pre>
     */
    def or(endWithWord: EndWithWord): OrEndWithWord = new OrEndWithWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrNotWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 1 should (not equal (1) or not equal (2))
       *                                ^
       * </pre>
       */
      def equal(any: Any): Matcher[T] =
        matchersWrapper.or(matchers.not.apply(matchers.equal(any)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 1 should (not be (1) or not be (2))
       *                             ^
       * </pre>
       */
      def be(any: Any): Matcher[T] =
        matchersWrapper.or(matchers.not.apply(matchers.be(any)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not have length (2) or not have length (3))
       *                                                ^
       * </pre>
       */
      def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): Matcher[T with AnyRef] =
        matchersWrapper.or(matchers.not.apply(matchers.have.length(resultOfLengthWordApplication.expectedLength)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not have size (2) or not have size (3))
       *                                              ^
       * </pre>
       */
      def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): Matcher[T with AnyRef] =
        matchersWrapper.or(matchers.not.apply(matchers.have.size(resultOfSizeWordApplication.expectedSize)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * book should (not have (title ("Moby Dick")) or not have (author ("Melville")))
       *                                                    ^
       * </pre>
       */
      def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Matcher[T with U] =
        matchersWrapper.or(matchers.not.apply(matchers.have(firstPropertyMatcher, propertyMatchers: _*)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * map should (contain key (7) or not be (null))
       *                                    ^
       * </pre>
       */
      def be(o: Null): Matcher[T with AnyRef] = matchersWrapper.or(matchers.not.be(o))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 5 should (not be < (7) or not be < (8))
       *                               ^
       * </pre>
       */
      def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): Matcher[T with U] =
        matchersWrapper.or(matchers.not.be(resultOfLessThanComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 7 should (not be > (5) or not be > (6))
       *                               ^
       * </pre>
       */
      def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): Matcher[T with U] =
        matchersWrapper.or(matchers.not.be(resultOfGreaterThanComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 2 should (not be <= (3) or not be <= (2))
       *                                ^
       * </pre>
       */
      def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): Matcher[T with U] =
        matchersWrapper.or(matchers.not.be(resultOfLessThanOrEqualToComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 8 should (not be >= (7) or not be >= (6))
       *                                ^
       * </pre>
       */
      def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): Matcher[T with U] =
        matchersWrapper.or(matchers.not.be(resultOfGreaterThanOrEqualToComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 5 should (not be === (7) or not be === (8))
       *                                 ^
       * </pre>
       */
      def be(resultOfTripleEqualsApplication: ResultOfTripleEqualsApplication): Matcher[T] =
        matchersWrapper.or(matchers.not.be(resultOfTripleEqualsApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * notEmptyMock should (not be ('full) or not be ('empty))
       *                                            ^
       * </pre>
       */
      def be(symbol: Symbol): Matcher[T with AnyRef] = matchersWrapper.or(matchers.not.be(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 2 should (not be (even) or not be (odd))
       *                                ^
       * </pre>
       */
      def be[U](beMatcher: BeMatcher[U]): Matcher[T with U] = matchersWrapper.or(matchers.not.be(beMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be (directory) or not be (file))
       *                                          ^
       * </pre>
       */
      def be[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = matchersWrapper.or(matchers.not.be(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isNotFileMock should (not be a ('directory) or not be a ('file))
       *                                                    ^
       * </pre>
       */
      def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T with AnyRef] = matchersWrapper.or(matchers.not.be(resultOfAWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be a (directory) or not be a (file))
       *                                            ^
       * </pre>
       */
      def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Matcher[T with U] = matchersWrapper.or(matchers.not.be(resultOfAWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * notAppleMock should (not be an ('apple) or not be an ('apple))
       *                                                ^
       * </pre>
       */
      def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T with AnyRef] = matchersWrapper.or(matchers.not.be(resultOfAnWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be an (directory) or not be an (file))
       *                                             ^
       * </pre>
       */
      def be[U <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]): Matcher[T with U] = matchersWrapper.or(matchers.not.be(resultOfAnWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * obj should (not be theSameInstanceAs (otherString) or not be theSameInstanceAs (string))
       *                                                           ^
       * </pre>
       */
      def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T with AnyRef] = matchersWrapper.or(matchers.not.be(resultOfTheSameInstanceAsApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenDotOh should (not be (17.0 plusOrMinus 0.2) or not be (17.0 plusOrMinus 0.2))
       *                                                         ^
       * </pre>
       */
      def be(doubleTolerance: DoubleTolerance): Matcher[T with Double] = matchersWrapper.or(matchers.not.be(doubleTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenDotOhFloat should (not be (17.0f plusOrMinus 0.2f) or not be (17.0f plusOrMinus 0.2f))
       *                                                                ^
       * </pre>
       */
      def be(floatTolerance: FloatTolerance): Matcher[T with Float] = matchersWrapper.or(matchers.not.be(floatTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenLong should (not be (17L plusOrMinus 2L) or not be (17L plusOrMinus 2L))
       *                                                      ^
       * </pre>
       */
      def be(longTolerance: LongTolerance): Matcher[T with Long] = matchersWrapper.or(matchers.not.be(longTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenInt should (not be (17 plusOrMinus 2) or not be (17 plusOrMinus 2))
       *                                                   ^
       * </pre>
       */
      def be(intTolerance: IntTolerance): Matcher[T with Int] = matchersWrapper.or(matchers.not.be(intTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenShort should (not be (17.toShort plusOrMinus 2.toShort) or not be (17.toShort plusOrMinus 2.toShort))
       *                                                                     ^
       * </pre>
       */
      def be(shortTolerance: ShortTolerance): Matcher[T with Short] = matchersWrapper.or(matchers.not.be(shortTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * sevenByte should ((not be (19.toByte plusOrMinus 2.toByte)) or (not be (19.toByte plusOrMinus 2.toByte)))
       *                                                                     ^
       * </pre>
       */
      def be(byteTolerance: ByteTolerance): Matcher[T with Byte] = matchersWrapper.or(matchers.not.be(byteTolerance))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not fullyMatch regex ("fred") or not fullyMatch regex (decimal))
       *                                                     ^
       * </pre>
       */
      def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.or(matchers.not.fullyMatch(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not include regex ("fred") or not include regex (decimal))
       *                                                  ^
       * </pre>
       */
      def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.or(matchers.not.include(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not include ("bob") or not include ("1.7"))
       *                                           ^
       * </pre>
       */
      def include(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.or(matchers.not.include(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not startWith regex ("bob") or not startWith regex (decimal))
       *                                                   ^
       * </pre>
       */
      def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.or(matchers.not.startWith(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not startWith ("fred") or not startWith ("1.7"))
       *                                              ^
       * </pre>
       */
      def startWith(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.or(matchers.not.startWith(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not endWith regex ("bob") or not endWith regex (decimal))
       *                                                 ^
       * </pre>
       */
      def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.or(matchers.not.endWith(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not endWith ("fred") or not endWith ("1.7"))
       *                                            ^
       * </pre>
       */
      def endWith(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.or(matchers.not.endWith(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not contain (1) or not contain (3))
       *                                            ^
       * </pre>
       */
      def contain[U](expectedElement: U): Matcher[T with GenTraversable[U]] =
        matchersWrapper.or(matchers.not.contain(expectedElement))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (not contain key ("two") or not contain key ("three"))
       *                                                                    ^
       * </pre>
       */
      def contain[U](resultOfKeyWordApplication: ResultOfKeyWordApplication[U]): Matcher[T with scala.collection.GenMap[U, Any]] =
        matchersWrapper.or(matchers.not.contain(resultOfKeyWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (3))
       *                                                                  ^
       * </pre>
       */
      def contain[U](resultOfValueWordApplication: ResultOfValueWordApplication[U]): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] =
        matchersWrapper.or(matchers.not.contain(resultOfValueWordApplication))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (3))
     *                                                           ^
     * </pre>
     */
    def or(notWord: NotWord): OrNotWord = new OrNotWord
  }

  /**
   * This implicit conversion method enables ScalaTest matchers expressions that involve <code>and</code> and <code>or</code>.
   */
  implicit def convertToMatcherWrapper[T](leftMatcher: Matcher[T]): MatcherWrapper[T] = new MatcherWrapper(leftMatcher)

  //
  // This class is used as the return type of the overloaded should method (in MapShouldWrapper)
  // that takes a HaveWord. It's key method will be called in situations like this:
  //
  // map should have key 1
  //
  // This gets changed to :
  //
  // convertToMapShouldWrapper(map).should(have).key(1)
  //
  // Thus, the map is wrapped in a convertToMapShouldWrapper call via an implicit conversion, which results in
  // a MapShouldWrapper. This has a should method that takes a HaveWord. That method returns a
  // ResultOfHaveWordPassedToShould that remembers the map to the left of should. Then this class
  // ha a key method that takes a K type, they key type of the map. It does the assertion thing.
  // 
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfContainWordForMap[K, V](left: scala.collection.GenMap[K, V], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should contain key ("one")
     *                    ^
     * </pre>
     */
    def key(expectedKey: K) {
      if (left.exists(_._1 == expectedKey) != shouldBeTrue)
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
     * map should contain value (1)
     *                    ^
     * </pre>
     */
    def value(expectedValue: V) {
      // if (left.values.contains(expectedValue) != shouldBeTrue) CHANGING FOR 2.8.0 RC1
      if (left.exists(expectedValue == _._2) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainValue" else "containedValue",
            left,
            expectedValue)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfContainWordForJavaMap[K, V](left: java.util.Map[K, V], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * javaMap should contain key ("two")
     *                        ^
     * </pre>
     */
    def key(expectedKey: K) {
      if (left.containsKey(expectedKey) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainKey" else "containedKey",
            left,
            expectedKey)
        )
    }

    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * javaMap should contain value ("2")
     *                        ^
     * </pre>
     */
    def value(expectedValue: V) {
      if (left.containsValue(expectedValue) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainValue" else "containedValue",
            left,
            expectedValue)
        )
    }
  }

  /** 
   * This implicit conversion method enables the following syntax (<code>javaColl</code> is a <code>java.util.Collection</code>):
   *
   * <pre class="stHighlight">
   * javaColl should contain ("two")
   * </pre>
   *
   * The <code>(contain ("two"))</code> expression will result in a <code>Matcher[GenTraversable[String]]</code>. This
   * implicit conversion method will convert that matcher to a <code>Matcher[java.util.Collection[String]]</code>.
   */
  implicit def convertTraversableMatcherToJavaCollectionMatcher[T](traversableMatcher: Matcher[GenTraversable[T]]): Matcher[java.util.Collection[T]] =
    new Matcher[java.util.Collection[T]] {
      def apply(left: java.util.Collection[T]): MatchResult = {
        val traversable = new Traversable[T] {
          def foreach[U](f: (T) => U) {
            val javaIterator = left.iterator
            while (javaIterator.hasNext)
              f(javaIterator.next)
          }
          override def toString: String = left.toString
        }
        traversableMatcher.apply(traversable)
      }
    }

  /**
   * This implicit conversion method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain (3) and not contain (2))
   * </pre>
   *
   * The <code>(not contain ("two"))</code> expression will result in a <code>Matcher[GenTraversable[String]]</code>. This
   * implicit conversion method will convert that matcher to a <code>Matcher[Array[String]]</code>.
  */
  implicit def convertTraversableMatcherToArrayMatcher[T](traversableMatcher: Matcher[GenTraversable[T]]): Matcher[Array[T]] =
    new Matcher[Array[T]] {
      def apply(left: Array[T]): MatchResult = {
        val traversable = new Traversable[T] {
          def foreach[U](f: (T) => U) {
            var index = 0
            while (index < left.length) {
              index += 1
              f(left(index - 1))
            }
          }
          // Need to prettify the array's toString, because by the time it gets to decorateToStringValue, the array
          // has been wrapped in this Traversable and so it won't get prettified anymore by FailureMessages.decorateToStringValue.
          override def toString: String = FailureMessages.prettifyArrays(left).toString
        }
        traversableMatcher.apply(traversable)
      }
    }

  /**
   * This implicit conversion method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
   *
   * <pre class="stHighlight">
   * javaMap should (contain key ("two"))
   * </pre>
   *
   * The <code>(contain key ("two"))</code> expression will result in a <code>Matcher[scala.collection.GenMap[String, Any]]</code>. This
   * implicit conversion method will convert that matcher to a <code>Matcher[java.util.Map[String, Any]]</code>.
   */
  implicit def convertMapMatcherToJavaMapMatcher[K, V](mapMatcher: Matcher[scala.collection.GenMap[K, V]]): Matcher[java.util.Map[K, V]] =
    new Matcher[java.util.Map[K, V]] {
      def apply(left: java.util.Map[K, V]): MatchResult = {
        // Even though the java map is mutable I just wrap it it to a plain old Scala map, because
        // I have no intention of mutating it.
        class MapWrapper[Z](javaMap: java.util.Map[K, Z]) extends scala.collection.Map[K, Z] {
          override def size: Int = javaMap.size
          def get(key: K): Option[Z] =
            if (javaMap.containsKey(key)) Some(javaMap.get(key)) else None
          override def iterator: Iterator[(K, Z)] = new Iterator[(K, Z)] {
            private val javaIterator = javaMap.keySet.iterator
            def next: (K, Z) = {
              val nextKey = javaIterator.next
              (nextKey, javaMap.get(nextKey))
            }
            def hasNext: Boolean = javaIterator.hasNext
          }
          override def +[W >: Z] (kv: (K, W)): scala.collection.Map[K, W] = {
            val newJavaMap = new java.util.HashMap[K, W](javaMap)
            val (key, value) = kv
            newJavaMap.put(key, value)
            new MapWrapper[W](newJavaMap)
          }
          override def - (key: K): scala.collection.Map[K, Z] = {
            val newJavaMap = new java.util.HashMap[K, Z](javaMap)
            newJavaMap.remove(key)
            new MapWrapper[Z](newJavaMap)
          }
          override def toString: String = javaMap.toString
        }
        val scalaMap = new MapWrapper[V](left)
        mapMatcher.apply(scalaMap)
      }
    }

  // Ack. The above conversion doesn't apply to java.util.Maps, because java.util.Map is not a subinterface
  // of java.util.Collection. But right now Matcher[Traversable] supports only "contain" and "have size"
  // syntax, and thus that should work on Java maps too, why not. Well I'll tell you why not. It is too complicated.
  // Since java Map is not a java Collection, I'll say the contain syntax doesn't work on it. But you can say
  // have key.

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ContainWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (contain (2) and contain (1))
     *                             ^
     * </pre>
     */
    def apply[T](expectedElement: T): Matcher[GenTraversable[T]] =
      new Matcher[GenTraversable[T]] {
        def apply(left: GenTraversable[T]): MatchResult =
          MatchResult(
            left.exists(_ == expectedElement), 
            FailureMessages("didNotContainExpectedElement", left, expectedElement),
            FailureMessages("containedExpectedElement", left, expectedElement)
          )
      }

    //
    // This key method is called when "contain" is used in a logical expression, such as:
    // map should { contain key 1 and equal (Map(1 -> "Howdy")) }. It results in a matcher
    // that remembers the key value. By making the value type Any, it causes overloaded shoulds
    // to work, because for example a Matcher[GenMap[Int, Any]] is a subtype of Matcher[GenMap[Int, String]],
    // given Map is covariant in its V (the value type stored in the map) parameter and Matcher is
    // contravariant in its lone type parameter. Thus, the type of the Matcher resulting from contain key 1
    // is a subtype of the map type that has a known value type parameter because its that of the map
    // to the left of should. This means the should method that takes a map will be selected by Scala's
    // method overloading rules.
    //
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should (contain key ("fifty five") or contain key ("twenty two"))
     *                     ^
     * </pre>
     *
     * The map's value type parameter cannot be inferred because only a key type is provided in
     * an expression like <code>(contain key ("fifty five"))</code>. The matcher returned
     * by this method matches <code>scala.collection.Map</code>s with the inferred key type and value type <code>Any</code>. Given
     * <code>Map</code> is covariant in its value type, and <code>Matcher</code> is contravariant in
     * its type parameter, a <code>Matcher[Map[Int, Any]]</code>, for example, is a subtype of <code>Matcher[Map[Int, String]]</code>.
     * This will enable the matcher returned by this method to be used against any <code>Map</code> that has
     * the inferred key type.
     */
    def key[K](expectedKey: K): Matcher[scala.collection.GenMap[K, Any]] =
      new Matcher[scala.collection.GenMap[K, Any]] {
        def apply(left: scala.collection.GenMap[K, Any]): MatchResult =
          MatchResult(
            left.exists(_._1 == expectedKey),
            FailureMessages("didNotContainKey", left, expectedKey),
            FailureMessages("containedKey", left, expectedKey)
          )
      }

    // Holy smokes I'm starting to scare myself. I fixed the problem of the compiler not being
    // able to infer the value type in contain value 1 and ... like expressions, because the
    // value type is there, with an existential type. Since I don't know what K is, I decided to
    // try just saying that with an existential type, and it compiled and ran. Pretty darned
    // amazing compiler. The problem could not be fixed like I fixed the key method above, because
    // Maps are nonvariant in their key type parameter, whereas they are covariant in their value
    // type parameter, so the same trick wouldn't work. But this existential type trick seems to
    // work like a charm.
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain value (5) and not contain value (3))
     *                                                 ^
     * </pre>
     *
     * The map's key type parameter cannot be inferred because only a value type is provided in
     * an expression like <code>(contain value (5))</code>. The matcher returned
     * by this method matches <code>scala.collection.Map</code>s with the inferred value type and the existential key
     * type <code>[K] forSome { type K }</code>. Even though <code>Matcher</code> is contravariant in its type parameter, because
     * <code>Map</code> is nonvariant in its key type, 
     * a <code>Matcher[Map[Any, Int]]</code>, for example, is <em>not</em> a subtype of <code>Matcher[Map[String, Int]]</code>,
     * so the key type parameter of the <code>Map</code> returned by this method cannot be <code>Any</code>. By making it
     * an existential type, the Scala compiler will not infer it to anything more specific.
     * This will enable the matcher returned by this method to be used against any <code>Map</code> that has
     * the inferred value type.
     *
     */
    def value[V](expectedValue: V): Matcher[scala.collection.GenMap[K, V] forSome { type K }] =
      new Matcher[scala.collection.GenMap[K, V] forSome { type K }] {
        def apply(left: scala.collection.GenMap[K, V] forSome { type K }): MatchResult =
          MatchResult(
            // left.values.contains(expectedValue), CHANGING FOR 2.8.0 RC1
            left.exists(expectedValue == _._2),
            FailureMessages("didNotContainValue", left, expectedValue),
            FailureMessages("containedValue", left, expectedValue)
          )
      }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class IncludeWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (include ("1.7") and include ("1.8"))
     *                       ^
     * </pre>
     */
    def apply(expectedSubstring: String): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            left.indexOf(expectedSubstring) >= 0, 
            FailureMessages("didNotIncludeSubstring", left, expectedSubstring),
            FailureMessages("includedSubstring", left, expectedSubstring)
          )
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimal = """(-)?(\d+)(\.\d*)?"""
     * "a1.7b" should (include regex (decimal) and include regex (decimal))
     *                         ^
     * </pre>
     */
    def regex[T <: String](right: T): Matcher[T] = regex(right.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimalRegex = """(-)?(\d+)(\.\d*)?""".r
     * "a1.7" should (include regex (decimalRegex) and include regex (decimalRegex))
     *                        ^
     * </pre>
     */
    def regex(expectedRegex: Regex): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            expectedRegex.findFirstIn(left).isDefined,
            FailureMessages("didNotIncludeRegex", left, expectedRegex),
            FailureMessages("includedRegex", left, expectedRegex)
          )
      }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class StartWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7b" should (startWith ("1.7") and startWith ("1.7b"))
     *                          ^
     * </pre>
     */
    def apply(right: String): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            left startsWith right,
            FailureMessages("didNotStartWith", left, right),
            FailureMessages("startedWith", left, right)
          )
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimal = """(-)?(\d+)(\.\d*)?"""
     * "1.7b" should (startWith regex (decimal) and startWith regex (decimal))
     *                          ^
     * </pre>
     */
    def regex[T <: String](right: T): Matcher[T] = regex(right.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimalRegex = """(-)?(\d+)(\.\d*)?""".r
     * "1.7" should (startWith regex (decimalRegex) and startWith regex (decimalRegex))
     *                         ^
     * </pre>
     */
    def regex(rightRegex: Regex): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            rightRegex.pattern.matcher(left).lookingAt,
            FailureMessages("didNotStartWithRegex", left, rightRegex),
            FailureMessages("startedWithRegex", left, rightRegex)
          )
      }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class EndWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7b" should (endWith ("1.7b") and endWith ("7b"))
     *                        ^
     * </pre>
     */
    def apply(right: String): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            left endsWith right,
            FailureMessages("didNotEndWith", left, right),
            FailureMessages("endedWith", left, right)
          )
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimal = """(-)?(\d+)(\.\d*)?"""
     * "b1.7" should (endWith regex (decimal) and endWith regex (decimal))
     *                        ^
     * </pre>
     */
    def regex[T <: String](right: T): Matcher[T] = regex(right.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimalRegex = """(-)?(\d+)(\.\d*)?""".r
     * "b1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
     *                        ^
     * </pre>
     */
    def regex(rightRegex: Regex): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult = {
          val allMatches = rightRegex.findAllIn(left)
          MatchResult(
            allMatches.hasNext && (allMatches.end == left.length),
            FailureMessages("didNotEndWithRegex", left, rightRegex),
            FailureMessages("endedWithRegex", left, rightRegex)
          )
        }
      }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class FullyMatchWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimal = """(-)?(\d+)(\.\d*)?"""
     * "1.7" should (fullyMatch regex (decimal) and fullyMatch regex (decimal))
     *                          ^
     * </pre>
     */
    def regex(rightRegexString: String): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            java.util.regex.Pattern.matches(rightRegexString, left),
            FailureMessages("didNotFullyMatchRegex", left, UnquotedString(rightRegexString)),
            FailureMessages("fullyMatchedRegex", left, UnquotedString(rightRegexString))
          )
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimalRegex = """(-)?(\d+)(\.\d*)?""".r
     * "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
     *                          ^
     * </pre>
     */
    def regex(rightRegex: Regex): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            rightRegex.pattern.matcher(left).matches,
            FailureMessages("didNotFullyMatchRegex", left, rightRegex),
            FailureMessages("fullyMatchedRegex", left, rightRegex)
          )
      }
  }

// The getLength and getSize field conversions seem inconsistent with
// what I do in symbol HavePropertyMatchers. It isn't, though because the difference is here
// it's a Scala field and there a Java field: a val getLength is a 
// perfectly valid Scala way to get a JavaBean property Java method in the bytecodes.

  /**
   * Sealed supertrait for <code>Length</code> and <code>Size</code> type classes.
   *
   * <p>
   * These traits enable you to use ScalaTest matchers' &ldquo;<code>have length</code>&rdquo; and
   * &ldquo;<code>have size</code>&rdquo; syntax to be used with arbitrary objects. As an example, consider
   * <code>java.net.DatagramPacket</code>, which has a <code>getLength</code> method. By default, this
   * can't be used with ScalaTest's <code>have length</code> syntax. 
   * </p>
   *
   * <pre>
   * scala> import java.net.DatagramPacket
   * import java.net.DatagramPacket
   * 
   * scala> import org.scalatest.matchers.ShouldMatchers._
   * import org.scalatest.matchers.ShouldMatchers._
   *
   * scala> val dp = new DatagramPacket(Array(0x0, 0x1, 0x2, 0x3), 4)
   * dp: java.net.DatagramPacket = java.net.DatagramPacket@54906181
   * 
   * scala> dp.getLength
   * res0: Int = 4
   *
   * scala> dp should have length 4
   * <console>:13: error: could not find implicit value for parameter ev: org.scalatest.matchers.ShouldMatchers.Extent[java.net.DatagramPacket]
   *          dp should have length 4
   *             ^
   *
   * scala> implicit val lengthOfDatagramPacket =
   *     |   new Length[DatagramPacket] {
   *     |     def extentOf(dp: DatagramPacket): Long = dp.getLength
   *     |   }
   * lengthOfDatagramPacket: java.lang.Object with org.scalatest.matchers.ShouldMatchers.Length[java.net.DatagramPacket] = $anon$1@550c6b37
   *
   * scala> dp should have length 4
   *
   * scala> dp should have length 3
   * org.scalatest.exceptions.TestFailedException:  java.net.DatagramPacket@54906181 had length 4, not length 3
   * </pre>
   *
   * @author Bill Venners
   */
  sealed trait Extent[T] {
    def extentOf(o: T): Long
  }

  /**
   * Supertrait for <code>Length</code> type classes.
   *
   * <p>
   * This traits enable you to use ScalaTest matchers' &ldquo;<code>have length</code>&rdquo; 
   * syntax to be used with arbitrary objects. As an example, consider
   * <code>java.net.DatagramPacket</code>, which has a <code>getLength</code> method. By default, this
   * can't be used with ScalaTest's <code>have length</code> syntax. 
   * </p>
   *
   * <pre>
   * scala> import java.net.DatagramPacket
   * import java.net.DatagramPacket
   * 
   * scala> import org.scalatest.matchers.ShouldMatchers._
   * import org.scalatest.matchers.ShouldMatchers._
   *
   * scala> val dp = new DatagramPacket(Array(0x0, 0x1, 0x2, 0x3), 4)
   * dp: java.net.DatagramPacket = java.net.DatagramPacket@54906181
   * 
   * scala> dp.getLength
   * res0: Int = 4
   *
   * scala> dp should have length 4
   * <console>:13: error: could not find implicit value for parameter ev: org.scalatest.matchers.ShouldMatchers.Extent[java.net.DatagramPacket]
   *          dp should have length 4
   *             ^
   *
   * scala> implicit val lengthOfDatagramPacket =
   *     |   new Length[DatagramPacket] {
   *     |     def extentOf(dp: DatagramPacket): Long = dp.getLength
   *     |   }
   * lengthOfDatagramPacket: java.lang.Object with org.scalatest.matchers.ShouldMatchers.Length[java.net.DatagramPacket] = $anon$1@550c6b37
   *
   * scala> dp should have length 4
   *
   * scala> dp should have length 3
   * org.scalatest.exceptions.TestFailedException:  java.net.DatagramPacket@54906181 had length 4, not length 3
   * </pre>
   *
   * @author Bill Venners
   */
  trait Length[T] extends Extent[T] // TODO: Fix the ScalaDoc

  /**
   * Supertrait for <code>Size</code> type classes.
   *
   * <p>
   * This traits enable you to use ScalaTest matchers' &ldquo;<code>have size</code>&rdquo; 
   * syntax to be used with arbitrary objects. As an example, consider
   * <code>java.net.DatagramPacket</code>, which has a <code>getLength</code> method. By default, this
   * can't be used with ScalaTest's <code>have length</code> syntax. 
   * </p>
   *
   * <pre>
   * scala> import java.awt.image.DataBufferByte
   * import java.awt.image.DataBufferByte
   * 
   * scala> import org.scalatest.matchers.ShouldMatchers._
   * import org.scalatest.matchers.ShouldMatchers._
   *
   * scala> val db = new DataBufferByte(4)
   * db: java.awt.image.DataBufferByte = java.awt.image.DataBufferByte@33d5e94f
   * 
   * scala> db.getSize
   * res0: Int = 4
   *
   * scala> db should have size 4
   * <console>:17: error: could not find implicit value for parameter ev: org.scalatest.matchers.ShouldMatchers.Extent[java.awt.image.DataBufferByte]
   *               db should have size 4
   *                  ^
   * scala> implicit val sizeOfDataBufferByte =
   *      |   new Size[DataBufferByte] {
   *      |     def extentOf(db: DataBufferByte): Long = db.getSize
   *      |   }
   * sizeOfDataBufferByte: java.lang.Object with org.scalatest.matchers.ShouldMatchers.Size[java.awt.image.DataBufferByte] = $anon$1@4c69bdf8
   *
   * scala> db should have size 4
   *
   * scala> db should have size 3
   * org.scalatest.exceptions.TestFailedException:  java.awt.image.DataBufferByte@33d5e94f had size 4, not size 3
   * </pre>
   *
   * @author Bill Venners
   */
  trait Size[T] extends Extent[T] // TODO Fix the scaladoc

  // This guy is generally done through an implicit conversion from a symbol. It takes that symbol, and 
  // then represents an object with an apply method. So it gives an apply method to symbols.
  // book should have ('author ("Gibson"))
  //                   ^ // Basically this 'author symbol gets converted into this class, and its apply  method takes "Gibson"
  // TODO, put the documentation of the details of the algo for selecting a method or field to use here.
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used as the result of an implicit conversion from class <code>Symbol</code>, to enable symbols to be
   * used in <code>have ('author ("Dickens"))</code> syntax. The name of the implicit conversion method is
   * <code>convertSymbolToHavePropertyMatcherGenerator</code>.
   * </p>
   *
   * <p>
   * Class <code>HavePropertyMatcherGenerator</code>'s primary constructor takes a <code>Symbol</code>. The 
   * <code>apply</code> method uses reflection to find and access a property that has the name specified by the
   * <code>Symbol</code> passed to the constructor, so it can determine if the property has the expected value
   * passed to <code>apply</code>.
   * If the symbol passed is <code>'title</code>, for example, the <code>apply</code> method
   * will use reflection to look for a public Java field named
   * "title", a public method named "title", or a public method named "getTitle". 
   * If a method, it must take no parameters. If multiple candidates are found,
   * the <code>apply</code> method will select based on the following algorithm:
   * </p>
   * 
   * <table class="stTable">
   * <tr><th class="stHeadingCell">Field</th><th class="stHeadingCell">Method</th><th class="stHeadingCell">"get" Method</th><th class="stHeadingCell">Result</th></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Throws <code>TestFailedException</code>, because no candidates found</td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>getTitle()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>title()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>title()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>title()</code></td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>title()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Accesses field <code>title</code></td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>getTitle()</code></td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell"><code>title()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>title()</code></td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell"><code>title()</code></td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>title()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * </table>
   * 
   *
   * @author Bill Venners
   */
  final class HavePropertyMatcherGenerator(symbol: Symbol) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should have ('title ("A Tale of Two Cities"))
     *                          ^
     * </pre>
     * 
     * <p>
     * This class has an <code>apply</code> method that will produce a <code>HavePropertyMatcher[AnyRef, Any]</code>.
     * The implicit conversion method, <code>convertSymbolToHavePropertyMatcherGenerator</code>, will cause the 
     * above line of code to be eventually transformed into:
     * </p>
     * 
     * <pre class="stHighlight">
     * book should have (convertSymbolToHavePropertyMatcherGenerator('title).apply("A Tale of Two Cities"))
     * </pre>
     */
    def apply(expectedValue: Any): HavePropertyMatcher[AnyRef, Any] =
      new HavePropertyMatcher[AnyRef, Any] {

        /**
         * This method enables the following syntax:
         *
         * <pre class="stHighlight">
         * book should have ('title ("A Tale of Two Cities"))
         * </pre>
         * 
         * <p>
         * This method uses reflection to discover a field or method with a name that indicates it represents
         * the value of the property with the name contained in the <code>Symbol</code> passed to the 
         * <code>HavePropertyMatcherGenerator</code>'s constructor. The field or method must be public. To be a
         * candidate, a field must have the name <code>symbol.name</code>, so if <code>symbol</code> is <code>'title</code>,
         * the field name sought will be <code>"title"</code>. To be a candidate, a method must either have the name
         * <code>symbol.name</code>, or have a JavaBean-style <code>get</code> or <code>is</code>. If the type of the
         * passed <code>expectedValue</code> is <code>Boolean</code>, <code>"is"</code> is prepended, else <code>"get"</code>
         * is prepended. Thus if <code>'title</code> is passed as <code>symbol</code>, and the type of the <code>expectedValue</code> is
         * <code>String</code>, a method named <code>getTitle</code> will be considered a candidate (the return type
         * of <code>getTitle</code> will not be checked, so it need not be <code>String</code>. By contrast, if <code>'defined</code>
         * is passed as <code>symbol</code>, and the type of the <code>expectedValue</code> is <code>Boolean</code>, a method
         * named <code>isTitle</code> will be considered a candidate so long as its return type is <code>Boolean</code>.
         * </p>
         * TODO continue the story
         */
        def apply(objectWithProperty: AnyRef): HavePropertyMatchResult[Any] = {

          // If 'empty passed, propertyName would be "empty"
          val propertyName = symbol.name

          val isBooleanProperty =
            expectedValue match {
              case o: Boolean => true
              case _ => false
            }

          accessProperty(objectWithProperty, symbol, isBooleanProperty) match {

            case None =>

              // if propertyName is '>, mangledPropertyName would be "$greater"
              val mangledPropertyName = transformOperatorChars(propertyName)

              // methodNameToInvoke would also be "title"
              val methodNameToInvoke = mangledPropertyName

              // methodNameToInvokeWithGet would be "getTitle"
              val methodNameToInvokeWithGet = "get"+ mangledPropertyName(0).toUpper + mangledPropertyName.substring(1)

              throw newTestFailedException(Resources("propertyNotFound", methodNameToInvoke, expectedValue.toString, methodNameToInvokeWithGet))

            case Some(result) =>

              new HavePropertyMatchResult[Any](
                result == expectedValue,
                propertyName,
                expectedValue,
                result
              )
          }
        }
      }
  }

  /**
   * This implicit conversion method converts a <code>Symbol</code> to a
   * <code>HavePropertyMatcherGenerator</code>, to enable the symbol to be used with the <code>have ('author ("Dickens"))</code> syntax.
   */
  implicit def convertSymbolToHavePropertyMatcherGenerator(symbol: Symbol): HavePropertyMatcherGenerator = new HavePropertyMatcherGenerator(symbol)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class HaveWord {

    // TODO: How about returning a Matcher[Gazornimplatz] and then providing implicit conversion
    // methods from Matcher[Gazornimplatz] to Matcher[Seq], Matcher[String], Matcher[java.util.List], and
    // Matcher[the structural length methods]. This is similar to the technique I used with "contain (7)"
    // to get it to work with java.util.Collection.
    // I couldn't figure out how to combine view bounds with existential types. May or may not
    // be possible, but going dynamic for now at least.
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should have length (9)
     *                  ^
     * </pre>
     *
     * <p>
     * Currently (as of ScalaTest 0.9.5), this method will produce a <code>Matcher[AnyRef]</code>, and if the
     * <code>AnyRef</code> passed to that matcher's <code>apply</code> method does not have the appropriate <code>length</code> property
     * structure, all will compile but a <code>TestFailedException</code> will result at runtime explaining the problem. The one exception is that it will work on
     * <code>java.util.List</code>, even though that type has no <code>length</code> structure (its <code>size</code> property
     * will be used instead.) In a future ScalaTest release, this may be tightened so that all is statically checked at compile time.
     * </p>
     */
    def length(expectedLength: Long): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult =
          left match {
            case leftArray: Array[_] =>
              MatchResult(
                leftArray.length == expectedLength, 
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case leftSeq: GenSeq[_] =>
              MatchResult(
                leftSeq.length == expectedLength, 
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case leftString: String =>
              MatchResult(
                leftString.length == expectedLength, 
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case leftJavaList: java.util.List[_] =>
              MatchResult(
                leftJavaList.size == expectedLength,
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case _ =>

              accessProperty(left, 'length, false) match {

                case None =>

                  throw newTestFailedException(Resources("noLengthStructure", expectedLength.toString))

                case Some(result) =>

                  MatchResult(
                    result == expectedLength,
                    FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                    FailureMessages("hadExpectedLength", left, expectedLength)
                  )
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
    def size(expectedSize: Long): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult =
          left match {
            case leftArray: Array[_] =>
              MatchResult(
                leftArray.length == expectedSize, 
                FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                FailureMessages("hadExpectedSize", left, expectedSize)
              )
            case leftTrav: GenTraversable[_] =>
              MatchResult(
                leftTrav.size == expectedSize, 
                FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                FailureMessages("hadExpectedSize", left, expectedSize)
              )
            case leftJavaList: java.util.List[_] =>
              MatchResult(
                leftJavaList.size == expectedSize,
                FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                FailureMessages("hadExpectedSize", left, expectedSize)
              )
            case _ =>

              accessProperty(left, 'size, false) match {

                case None =>

                  throw newTestFailedException(Resources("noSizeStructure", expectedSize.toString))

                case Some(result) =>

                  MatchResult(
                    result == expectedSize,
                    FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                    FailureMessages("hadExpectedSize", left, expectedSize)
                  )
              }
          }
      }

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

  //
  // This class is used as the return type of the overloaded should method (in TraversableShouldWrapper) 
  // that takes a HaveWord. It's size method will be called in situations like this:
  //
  // list should have size 1
  //
  // This gets changed to :
  //
  // convertToTraversableShouldWrapper(list).should(have).size(1)
  //
  // Thus, the list is wrapped in a convertToTraversableShouldWrapper call via an implicit conversion, which results in
  // a TraversableShouldWrapper. This has a should method that takes a HaveWord. That method returns a
  // ResultOfHaveWordForTraverablePassedToShould that remembers the map to the left of should. Then this class
  // has a size method that takes a T type, type parameter of the Traversable. It does the assertion thing.
  // 
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfHaveWordForTraversable[T](left: GenTraversable[T], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should have size (10)
     *                        ^
     * </pre>
     */
    def size(expectedSize: Int) {
      if ((left.size == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfHaveWordForJavaCollection[T](left: java.util.Collection[T], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaCollection should have size (10)
     *                       ^
     * </pre>
     */
    def size(expectedSize: Int) {
      if ((left.size == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForJavaMap(left: java.util.Map[_, _], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaMap should have size (10)
     *                     ^
     * </pre>
     */
    def size(expectedSize: Int) {
      if ((left.size == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForSeq[T](left: GenSeq[T], shouldBeTrue: Boolean) extends ResultOfHaveWordForTraversable[T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * seq should have length (20)
     *                 ^
     * </pre>
     */
    def length(expectedLength: Int) {
      if ((left.length == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><cod
e>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
class ResultOfHaveWordForArray[T](left: Array[T], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * array should have size (10)
     *                   ^
     * </pre>
     */
    def size(expectedSize: Int) {
      if ((left.size == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * array should have length (20)
     *                   ^
     * </pre>
     */
    def length(expectedLength: Int) {
      if ((left.length == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWordForTraversable[E, T <: GenTraversable[E]](left: T, shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * iterable should not contain ("one")
     *                     ^
     * </pre>
     */
    def contain(expectedElement: E) {
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

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should not have size (3)
     *                       ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      val right = resultOfSizeWordApplication.expectedSize
      if ((left.size == right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWordForJavaCollection[E, T <: java.util.Collection[E]](left: T, shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaCollection should not have size (3)
     *                           ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      val right = resultOfSizeWordApplication.expectedSize
      if ((left.size == right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
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
     * javaCollection should not contain ("elephant")
     *                           ^
     * </pre>
     */
    def contain(expectedElement: E) {
      val right = expectedElement
      if ((left.contains(right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForMap[K, V](left: scala.collection.GenMap[K, V], shouldBeTrue: Boolean)
      extends ResultOfNotWordForTraversable[(K, V), scala.collection.GenMap[K, V]](left, shouldBeTrue) {


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
      if ((left.exists(_._1 == right)) != shouldBeTrue) {
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
     * Map("one" -> 1, "two" -> 2) should not contain value (3)
     *                                        ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
      val right = resultOfValueWordApplication.expectedValue
      if ((left.exists(_._2 == right)) != shouldBeTrue) {
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

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForJavaMap[K, V](left: java.util.Map[K, V], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

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

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForSeq[E, T <: GenSeq[E]](left: T, shouldBeTrue: Boolean)
      extends ResultOfNotWordForTraversable[E, T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * List(1, 2) should not have length (12)
     *                       ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
             if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html">
<code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForArray[E](left: Array[E], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array("two", "three") should not contain ("one")
     *                                  ^
     * </pre>
     */
    def contain(expectedElement: E) {
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

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should not have size (3)
     *                        ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      val right = resultOfSizeWordApplication.expectedSize
      if ((left.size == right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
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
     * Array(1, 2) should not have length (12)
     *                        ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
             if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForJavaList[T](left: java.util.List[T], shouldBeTrue: Boolean) extends ResultOfHaveWordForJavaCollection[T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaList should have length (12)
     *                      ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def length(expectedLength: Int) {
      if ((left.size == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForJavaList[E, T <: java.util.List[E]](left: T, shouldBeTrue: Boolean)
      extends ResultOfNotWordForJavaCollection[E, T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaList should not have length (12)
     *                     ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.size == right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfBeWordForAnyRef[T <: AnyRef](left: T, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be theSameInstanceAs anotherObject
     *                  ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef) {
      if ((left eq right) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
            left,
            right
          )
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fileMock should be a ('file)
     *                    ^
     * </pre>
     */
    def a(symbol: Symbol) {
      val matcherResult = matchSymbolToPredicateMethod(left, symbol, true, true)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    // TODO: Check the shouldBeTrues, are they sometimes always false or true?
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should be a (goodRead)
     *                   ^
     * </pre>
     */
    def a(bePropertyMatcher: BePropertyMatcher[T]) {
      val result = bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName))
          else
            FailureMessages("wasA", left, UnquotedString(result.propertyName))
        )
      }
    }

    // TODO, in both of these, the failure message doesn't have a/an
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fruit should be an ('orange)
     *                 ^
     * </pre>
     */
    def an(symbol: Symbol) {
      val matcherResult = matchSymbolToPredicateMethod(left, symbol, true, false)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * book should be an (excellentRead)
     *                ^
     * </pre>
     */
    def an(beTrueMatcher: BePropertyMatcher[T]) {
      val beTrueMatchResult = beTrueMatcher(left)
      if (beTrueMatchResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotAn", left, UnquotedString(beTrueMatchResult.propertyName))
          else
            FailureMessages("wasAn", left, UnquotedString(beTrueMatchResult.propertyName))
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWord[T](left: T, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not equal (7)
     *                   ^
     * </pre>
     */
    def equal(right: Any) {
      if ((left == right) != shouldBeTrue)
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
     * result should not be <= (7)
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
     * result should not be >= (7)
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
     * result should not be < (7)
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
     * result should not be > (7)
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
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be === (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfTripleEqualsApplication) {
      if (comparison(left) != shouldBeTrue) {
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
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWordForAnyRef[T <: AnyRef](left: T, shouldBeTrue: Boolean)
      extends ResultOfNotWord[T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should not be (null)
     *                ^
     * </pre>
     */
    def be(o: Null) {
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
    def be(symbol: Symbol) {
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
    def be(bePropertyMatcher: BePropertyMatcher[T]) {
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
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication) {
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
    def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]) {
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
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication) {
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
    def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]) {
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
    def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication) {
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

    // TODO: Explain this matrix somewhere
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
    def have[U >: T](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*) {

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
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForString(left: String, shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef[String](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should not have length (12)
     *                   ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
             if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
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
      val rightRegex = resultOfRegexWordApplication.regex
      if (rightRegex.pattern.matcher(left).matches != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotFullyMatchRegex" else "fullyMatchedRegex",
            left,
            rightRegex
          )
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
      val rightRegex = resultOfRegexWordApplication.regex
      if (rightRegex.findFirstIn(left).isDefined != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotIncludeRegex" else "includedRegex",
            left,
            rightRegex
          )
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
      val rightRegex = resultOfRegexWordApplication.regex
      if (rightRegex.pattern.matcher(left).lookingAt != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotStartWithRegex" else "startedWithRegex",
            left,
            rightRegex
          )
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
      val rightRegex = resultOfRegexWordApplication.regex
      val allMatches = rightRegex.findAllIn(left)
      if (allMatches.hasNext && (allMatches.end == left.length) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
            left,
            rightRegex
          )
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
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForDouble(left: Double, shouldBeTrue: Boolean)
      extends ResultOfNotWord[Double](left, shouldBeTrue) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should not be (6.5 plusOrMinus 0.2)
     *                       ^
     * </pre>
     */
    def be(doubleTolerance: DoubleTolerance) {
      import doubleTolerance._
      if ((left <= right + tolerance && left >= right - tolerance) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotPlusOrMinus" else "wasPlusOrMinus",
            left,
            right,
            tolerance
          )
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForFloat(left: Float, shouldBeTrue: Boolean)
      extends ResultOfNotWord[Float](left, shouldBeTrue) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOhFloat should not be (6.5f plusOrMinus 0.2f)
     *                            ^
     * </pre>
     */
    def be(floatTolerance: FloatTolerance) {
      import floatTolerance._
      if ((left <= right + tolerance && left >= right - tolerance) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotPlusOrMinus" else "wasPlusOrMinus",
            left,
            right,
            tolerance
          )
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForLong(left: Long, shouldBeTrue: Boolean)
      extends ResultOfNotWord[Long](left, shouldBeTrue) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOhLong should not be (4L plusOrMinus 2L)
     *                           ^
     * </pre>
     */
    def be(longTolerance: LongTolerance) {
      import longTolerance._
      if ((left <= right + tolerance && left >= right - tolerance) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotPlusOrMinus" else "wasPlusOrMinus",
            left,
            right,
            tolerance
          )
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForInt(left: Int, shouldBeTrue: Boolean)
      extends ResultOfNotWord[Int](left, shouldBeTrue) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOhInt should not be (4 plusOrMinus 2)
     *                          ^
     * </pre>
     */
    def be(intTolerance: IntTolerance) {
      import intTolerance._
      if ((left <= right + tolerance && left >= right - tolerance) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotPlusOrMinus" else "wasPlusOrMinus",
            left,
            right,
            tolerance
          )
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForShort(left: Short, shouldBeTrue: Boolean)
      extends ResultOfNotWord[Short](left, shouldBeTrue) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOhShort should not be (4.toShort plusOrMinus 2.toShort)
     *                            ^
     * </pre>
     */
    def be(shortTolerance: ShortTolerance) {
      import shortTolerance._
      if ((left <= right + tolerance && left >= right - tolerance) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotPlusOrMinus" else "wasPlusOrMinus",
            left,
            right,
            tolerance
          )
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForByte(left: Byte, shouldBeTrue: Boolean)
      extends ResultOfNotWord[Byte](left, shouldBeTrue) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOhByte should not be (4.toByte plusOrMinus 2.toByte)
     *                            ^
     * </pre>
     */
    def be(byteTolerance: ByteTolerance) {
      import byteTolerance._
      if ((left <= right + tolerance && left >= right - tolerance) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotPlusOrMinus" else "wasPlusOrMinus",
            left,
            right,
            tolerance
          )
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class RegexWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""".r)
     *                                     ^
     * </pre>
     */
    def apply(regexString: String): ResultOfRegexWordApplication = new ResultOfRegexWordApplication(regexString)

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                                     ^
     * </pre>
     */
    def apply(regex: Regex): ResultOfRegexWordApplication = new ResultOfRegexWordApplication(regex)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * The primary constructor enables the following syntax (with a passed <code>scala.util.matching.Regex</code>): 
   * </p>
   *
   * <pre class="stHighlight">
   * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""".r)
   *                               ^
   * </pre>
   *
   * @author Bill Venners
   */
  final class ResultOfRegexWordApplication(val regex: Regex) {

    /**
     * This auxiliary constructor enables the following syntax (with a passed <code>java.lang.String</code>): 
     *
     * <pre class="stHighlight">
     * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                               ^
     * </pre>
     */
    def this(regexString: String) = this(new Regex(regexString))
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should have length (12)
     *                    ^
     * </pre>
     */
    def length(expectedLength: Int) {
      if ((left.length == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength
          )
        )
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfIncludeWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should include regex ("world")
     *                       ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should include regex ("wo.ld".r)
     *                       ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      if (rightRegex.findFirstIn(left).isDefined != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotIncludeRegex" else "includedRegex",
            left,
            rightRegex
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfStartWithWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should startWith regex ("Hel*o")
     *                         ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should startWith regex ("Hel*o".r)
     *                         ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      if (rightRegex.pattern.matcher(left).lookingAt != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotStartWithRegex" else "startedWithRegex",
            left,
            rightRegex
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfEndWithWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should endWith regex ("wor.d")
     *                       ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should endWith regex ("wor.d".r)
     *                       ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      val allMatches = rightRegex.findAllIn(left)
      if ((allMatches.hasNext && (allMatches.end == left.length)) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
            left,
            rightRegex
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfFullyMatchWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should fullMatch regex ("Hel*o world")
     *                         ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should fullymatch regex ("Hel*o world".r)
     *                          ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      if (rightRegex.pattern.matcher(left).matches != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotFullyMatchRegex" else "fullyMatchedRegex",
            left,
            rightRegex
          )
        )
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should equal (7)
   *               ^
   * </pre>
   *
   * <p>
   * The <code>left should equal (right)</code> syntax works by calling <code>==</code> on the <code>left</code>
   * value, passing in the <code>right</code> value, on every type except arrays. If both <code>left</code> and right are arrays, <code>deep</code>
   * will be invoked on both <code>left</code> and <code>right</code> before comparing them with <em>==</em>. Thus, even though this expression
   * will yield false, because <code>Array</code>'s <code>equals</code> method compares object identity:
   * </p>
   * 
   * <pre class="stHighlight">
   * Array(1, 2) == Array(1, 2) // yields false
   * </pre>
   *
   * <p>
   * The following expression will <em>not</em> result in a <code>TestFailedException</code>, because ScalaTest will compare
   * the two arrays structurally, taking into consideration the equality of the array's contents:
   * </p>
   *
   * <pre class="stHighlight">
   * Array(1, 2) should equal (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
   * </pre>
   *
   * <p>
   * If you ever do want to verify that two arrays are actually the same object (have the same identity), you can use the
   * <code>be theSameInstanceAs</code> syntax.
   * </p>
   *
   */
  def equal(right: Any): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
          MatchResult(
            areEqualComparingArraysStructurally(left, right),
            FailureMessages("didNotEqual", leftee, rightee),
            FailureMessages("equaled", left, right)
          )
        }
      }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
/*
  final class TreatedAsOrderedWrapper {
    def <[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T) =
          MatchResult(
            left < right,
            FailureMessages("wasNotLessThan", left, right),
            FailureMessages("wasLessThan", left, right)
          )
      }
    def >[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T) =
          MatchResult(
            left > right,
            FailureMessages("wasNotGreaterThan", left, right),
            FailureMessages("wasGreaterThan", left, right)
          )
      }
    def <=[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T) =
          MatchResult(
            left <= right,
            FailureMessages("wasNotLessThanOrEqualTo", left, right),
            FailureMessages("wasLessThanOrEqualTo", left, right)
          )
      }
    def >=[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T) =
          MatchResult(
            left >= right,
            FailureMessages("wasNotGreaterThanOrEqualTo", left, right),
            FailureMessages("wasGreaterThanOrEqualTo", left, right)
          )
      }
  }

  // This one is for one should be < (7)
  implicit def convertBeWordToForOrdered(beWord: BeWord): TreatedAsOrderedWrapper = new TreatedAsOrderedWrapper
*/

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * Class <code>BeWord</code> contains an <code>apply</code> method that takes a <code>Symbol</code>, which uses reflection
   * to find and access a <code>Boolean</code> property and determine if it is <code>true</code>.
   * If the symbol passed is <code>'empty</code>, for example, the <code>apply</code> method
   * will use reflection to look for a public Java field named
   * "empty", a public method named "empty", or a public method named "isEmpty". If a field, it must be of type <code>Boolean</code>.
   * If a method, it must take no parameters and return <code>Boolean</code>. If multiple candidates are found,
   * the <code>apply</code> method will select based on the following algorithm:
   * </p>
   * 
   * <table class="stTable">
   * <tr><th class="stHeadingCell">Field</th><th class="stHeadingCell">Method</th><th class="stHeadingCell">"is" Method</th><th class="stHeadingCell">Result</th></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Throws <code>TestFailedException</code>, because no candidates found</td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>isEmpty()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>empty()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>empty()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Accesses field <code>empty</code></td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>isEmpty()</code></td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>empty()</code></td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>empty()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * </table>
   * 
   * @author Bill Venners
   */
  final class BeWord {


    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &lt; (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the less than operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the less than operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &lt; (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &lt; (7))
     *                       ^
     * </pre>
     */
    def <[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left < right,
            FailureMessages("wasNotLessThan", left, right),
            FailureMessages("wasLessThan", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &gt; (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the greater than operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the greater than operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &gt; (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &gt; (7))
     *                       ^
     * </pre>
     */
    def >[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left > right,
            FailureMessages("wasNotGreaterThan", left, right),
            FailureMessages("wasGreaterThan", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &lt;= (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the less than or equal to operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the less than or equal to operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &lt;= (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &lt;= (7))
     *                       ^
     * </pre>
     */
    def <=[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left <= right,
            FailureMessages("wasNotLessThanOrEqualTo", left, right),
            FailureMessages("wasLessThanOrEqualTo", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &gt;= (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the greater than or equal to operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the greater than or equal to operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &gt;= (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &gt;= (7))
     *                       ^
     * </pre>
     */
    def >=[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left >= right,
            FailureMessages("wasNotGreaterThanOrEqualTo", left, right),
            FailureMessages("wasGreaterThanOrEqualTo", left, right)
          )
      }


    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be === (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the === operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the ===n operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be === (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be === (7))
     *                       ^
     * </pre>
     */
    def ===(right: Any): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
          MatchResult(
            areEqualComparingArraysStructurally(left, right),
            FailureMessages("wasNotEqualTo", leftee, rightee),
            FailureMessages("wasEqualTo", left, right)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * fileMock should not { be a ('file) }
     *                          ^
     * </pre>
     */
    def a[S <: AnyRef](right: Symbol): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, true, true)
      }

    /**
     * This method enables the following syntax, where <code>fileMock</code> is, for example, of type <code>File</code> and
     * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
     *
     * <pre class="stHighlight">
     * fileMock should not { be a (file) }
     *                          ^
     * </pre>
     */
    def a[S <: AnyRef](bePropertyMatcher: BePropertyMatcher[S]): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            result.matches,
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName)), 
            FailureMessages("wasA", left, UnquotedString(result.propertyName))
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * animal should not { be an ('elephant) }
     *                        ^
     * </pre>
     */
    def an[S <: AnyRef](right: Symbol): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, true, false)
      }

    /**
     * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
     * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
     *
     * <pre class="stHighlight">
     * keyEvent should not { be an (actionKey) }
     *                          ^
     * </pre>
     */
    def an[S <: AnyRef](bePropertyMatcher: BePropertyMatcher[S]): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            result.matches,
            FailureMessages("wasNotAn", left, UnquotedString(result.propertyName)),
            FailureMessages("wasAn", left, UnquotedString(result.propertyName))
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should be (7.1 plusOrMinus 0.2)
     *                      ^
     * </pre>
     */
    def apply(doubleTolerance: DoubleTolerance): Matcher[Double] =
      new Matcher[Double] {
        def apply(left: Double): MatchResult = {
          import doubleTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOhFloat should be (7.1f plusOrMinus 0.2f)
     *                           ^
     * </pre>
     */
    def apply(floatTolerance: FloatTolerance): Matcher[Float] =
      new Matcher[Float] {
        def apply(left: Float): MatchResult = {
          import floatTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenLong should be (7L plusOrMinus 2L)
     *                     ^
     * </pre>
     */
    def apply(longTolerance: LongTolerance): Matcher[Long] =
      new Matcher[Long] {
        def apply(left: Long): MatchResult = {
          import longTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenInt should be (7 plusOrMinus 2)
     *                     ^
     * </pre>
     */
    def apply(intTolerance: IntTolerance): Matcher[Int] =
      new Matcher[Int] {
        def apply(left: Int): MatchResult = {
          import intTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenShort should be (7.toShort plusOrMinus 2.toShort)
     *                     ^
     * </pre>
     */
    def apply(shortTolerance: ShortTolerance): Matcher[Short] =
      new Matcher[Short] {
        def apply(left: Short): MatchResult = {
          import shortTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenByte should be (7.toByte plusOrMinus 2.toByte)
     *                     ^
     * </pre>
     */
    def apply(byteTolerance: ByteTolerance): Matcher[Byte] =
      new Matcher[Byte] {
        def apply(left: Byte): MatchResult = {
          import byteTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be theSameInstancreAs (anotherObject)
     *                  ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult =
          MatchResult(
            left eq right,
            FailureMessages("wasNotSameInstanceAs", left, right),
            FailureMessages("wasSameInstanceAs", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be (true)
     *                  ^
     * </pre>
     */
    def apply(right: Boolean): Matcher[Boolean] = 
      new Matcher[Boolean] {
        def apply(left: Boolean): MatchResult =
          MatchResult(
            left == right,
            FailureMessages("wasNot", left, right),
            FailureMessages("was", left, right)
          )
      }

/* Well heck if I don't need this one
      [fsc] both method apply in class BeWord of type [T](org.scalatest.BePropertyMatcher[T])org.scalatest.Matcher[T]
      [fsc] and  method apply in class BeWord of type [T](org.scalatest.BeMatcher[T])org.scalatest.Matcher[T]
      [fsc] match argument types (Null)
      [fsc]         o should be (null)
      [fsc]                  ^
*/

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be (null)
     *                  ^
     * </pre>
     */
    def apply(o: Null): Matcher[AnyRef] = 
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult = {
          MatchResult(
            left == null,
            FailureMessages("wasNotNull", left),
            FailureMessages("wasNull"),
            FailureMessages("wasNotNull", left),
            FailureMessages("midSentenceWasNull")
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * set should be ('empty)
     *               ^
     * </pre>
     */
    def apply[S <: AnyRef](right: Symbol): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, false, false)
      }

    /**
     * This method enables the following syntax, where <code>num</code> is, for example, of type <code>Int</code> and
     * <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * num should be (odd)
     *               ^
     * </pre>
     */
    def apply[T](right: BeMatcher[T]): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult = right(left)
      }

    /**
     * This method enables the following syntax, where <code>open</code> refers to a <code>BePropertyMatcher</code>:
     *
     * <pre class="stHighlight">
     * door should be (open)
     *                ^
     * </pre>
     */
    def apply[T](bePropertyMatcher: BePropertyMatcher[T]): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            result.matches,
            FailureMessages("wasNot", left, UnquotedString(result.propertyName)), 
            FailureMessages("was", left, UnquotedString(result.propertyName))
          )
        }
      }

    /**
     * This method enables <code>be</code> to be used for equality comparison. Here are some examples: 
     *
     * <pre class="stHighlight">
     * result should be (None)
     *                  ^
     * result should be (Some(1))
     *                  ^
     * result should be (true)
     *                  ^
     * result should be (false)
     *                  ^
     * sum should be (19)
     *               ^
     * </pre>
     */
    def apply(right: Any): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult =
          left match {
            case null =>
              MatchResult(
                right == null,
                FailureMessages("wasNotNull", right),
                FailureMessages("wasNull"),
                FailureMessages("wasNotNull", right),
                FailureMessages("midSentenceWasNull")
              )
            case _ => {
              val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
              MatchResult(
                areEqualComparingArraysStructurally(left, right),
                FailureMessages("wasNotEqualTo", leftee, rightee),
                FailureMessages("wasEqualTo", left, right)
              )
            }
        }
      }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class NotWord {

    /**
     * This method enables the following syntax, where <code>tempFile</code>, for example, refers to a <code>java.io.File</code>
     * and <code>exist</code> is a <code>Matcher[java.io.File]</code>: 
     *
     * <pre class="stHighlight">
     * tempFile should not (exist)
     *                     ^
     * </pre>
     */
    def apply[S <: Any](matcher: Matcher[S]): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult =
          matcher(left) match {
            case MatchResult(bool, s1, s2, s3, s4) => MatchResult(!bool, s2, s1, s4, s3)
          }
      }

    /**
     * This method enables any <code>BeMatcher</code> to be negated by passing it to <code>not</code>. 
     * For example, if you have a <code>BeMatcher[Int]</code> called <code>odd</code>, which matches
     * <code>Int</code>s that are odd, you can negate it to get a <code>BeMatcher[Int]</code> that matches
     * even <code>Int</code>s, like this:
     *
     * <pre class="stHighlight">
     * val even = not (odd)
     *                ^
     * </pre>
     *
     * <p>
     * In addition, this method enables you to negate a <code>BeMatcher</code> at its point of use, like this:
     * </p>
     *
     * <pre class="stHighlight">
     * num should be (not (odd))
     * </pre>
     *
     * <p>
     * Nevertheless, in such as case it would be more idiomatic to write:
     * </p>
     *
     * <pre class="stHighlight">
     * num should not be (odd)
     * </pre>
     */
    def apply[S <: Any](beMatcher: BeMatcher[S]): BeMatcher[S] =
      new BeMatcher[S] {
        def apply(left: S): MatchResult =
          beMatcher(left) match {
            case MatchResult(bool, s1, s2, s3, s4) => MatchResult(!bool, s2, s1, s4, s3)
          }
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * num should (not equal (7) and be < (9))
     *                 ^
     * </pre>
     */
    def equal(right: Any): Matcher[Any] = apply(matchers.equal(right))

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have length (5) and not have length (3))
     *                         ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): Matcher[AnyRef] =
      apply(matchers.have.length(resultOfLengthWordApplication.expectedLength))

    // This looks similar to the AndNotWord one, but not quite the same because no and
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have size (5) and not have size (3))
     *                         ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): Matcher[AnyRef] =
      apply(matchers.have.size(resultOfSizeWordApplication.expectedSize))

    /**
     * This method enables the following syntax, where, for example, <code>book</code> is of type <code>Book</code> and <code>title</code> and <code>author</code>
     * are both of type <code>HavePropertyMatcher[Book, String]</code>:
     *
     * <pre class="stHighlight">
     * book should (not have (title ("Moby Dick")) and (not have (author ("Melville"))))
     *                  ^
     * </pre>
     */
    def have[T](firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*): Matcher[T] =
      apply(matchers.have(firstPropertyMatcher, propertyMatchers: _*))

    /**
     * This method enables the following syntax, where, for example, <code>num</code> is an <code>Int</code> and <code>odd</code>
     * of type <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * num should (not be (odd) and be <= (8))
     *                 ^
     * </pre>
     */
    def be[T](beMatcher: BeMatcher[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          beMatcher(left) match {
            case MatchResult(bool, s1, s2, s3, s4) => MatchResult(!bool, s2, s1, s4, s3)
          }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should (not be (null))
     *                 ^
     * </pre>
     */
    def be(o: Null): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult = {
          MatchResult(
            left != null,
            FailureMessages("wasNull"),
            FailureMessages("wasNotNull", left),
            FailureMessages("midSentenceWasNull"),
            FailureMessages("wasNotNull", left)
          )
        }
      }

    // These next four are for things like not be </>/<=/>=:
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be < (7) and not be > (10))
     *                 ^
     * </pre>
     */
    def be[T](resultOfLessThanComparison: ResultOfLessThanComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfLessThanComparison(left),
            FailureMessages("wasLessThan", left, resultOfLessThanComparison.right),
            FailureMessages("wasNotLessThan", left, resultOfLessThanComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be > (10) and not be < (7))
     *                 ^
     * </pre>
     */
    def be[T](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfGreaterThanComparison(left),
            FailureMessages("wasGreaterThan", left, resultOfGreaterThanComparison.right),
            FailureMessages("wasNotGreaterThan", left, resultOfGreaterThanComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be <= (7) and not be > (10))
     *                 ^
     * </pre>
     */
    def be[T](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfLessThanOrEqualToComparison(left),
            FailureMessages("wasLessThanOrEqualTo", left, resultOfLessThanOrEqualToComparison.right),
            FailureMessages("wasNotLessThanOrEqualTo", left, resultOfLessThanOrEqualToComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be >= (10) and not be < (7))
     *                 ^
     * </pre>
     */
    def be[T](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfGreaterThanOrEqualToComparison(left),
            FailureMessages("wasGreaterThanOrEqualTo", left, resultOfGreaterThanOrEqualToComparison.right),
            FailureMessages("wasNotGreaterThanOrEqualTo", left, resultOfGreaterThanOrEqualToComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * num should (not be === (7) and not be === (10))
     *                 ^
     * </pre>
     */
    def be(resultOfTripleEqualsApplication: ResultOfTripleEqualsApplication): Matcher[Any] = {
      new Matcher[Any] {
        def apply(left: Any): MatchResult =
          MatchResult(
            !resultOfTripleEqualsApplication(left),
            FailureMessages("wasEqualTo", left, resultOfTripleEqualsApplication.right),
            FailureMessages("wasNotEqualTo", left, resultOfTripleEqualsApplication.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * myFile should (not be ('hidden) and have (name ("temp.txt")))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](symbol: Symbol): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val positiveMatchResult = matchSymbolToPredicateMethod(left, symbol, false, false)
          MatchResult(
            !positiveMatchResult.matches,
            positiveMatchResult.negatedFailureMessage,
            positiveMatchResult.failureMessage
          )
        }
      }
    }

    /**
     * This method enables the following syntax, where <code>tempFile</code>, for example, refers to a <code>java.io.File</code>
     * and <code>hidden</code> is a <code>BePropertyMatcher[java.io.File]</code>: 
     *
     * <pre class="stHighlight">
     * tempFile should (not be (hidden) and have ('name ("temp.txt")))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](bePropertyMatcher: BePropertyMatcher[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            !result.matches,
            FailureMessages("was", left, UnquotedString(result.propertyName)),
            FailureMessages("wasNot", left, UnquotedString(result.propertyName))
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * isNotFileMock should (not be a ('file) and have ('name ("temp.txt"))))
     *                           ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val positiveMatchResult = matchSymbolToPredicateMethod(left, resultOfAWordApplication.symbol, true, true)
          MatchResult(
            !positiveMatchResult.matches,
            positiveMatchResult.negatedFailureMessage,
            positiveMatchResult.failureMessage
          )
        }
      }
    }

    /**
     * This method enables the following syntax, where <code>notSoSecretFile</code>, for example, refers to a <code>java.io.File</code>
     * and <code>directory</code> is a <code>BePropertyMatcher[java.io.File]</code>: 
     *
     * <pre class="stHighlight">
     * notSoSecretFile should (not be a (directory) and have ('name ("passwords.txt")))
     *                             ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = resultOfAWordApplication.bePropertyMatcher(left)
          MatchResult(
            !result.matches,
            FailureMessages("wasA", left, UnquotedString(result.propertyName)),
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName))
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * isNotAppleMock should (not be an ('apple) and not be ('rotten))
     *                            ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val positiveMatchResult = matchSymbolToPredicateMethod(left, resultOfAnWordApplication.symbol, true, false)
          MatchResult(
            !positiveMatchResult.matches,
            positiveMatchResult.negatedFailureMessage,
            positiveMatchResult.failureMessage
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * myFile should (not be an (directory) and not be an (directory))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = resultOfAnWordApplication.bePropertyMatcher(left)
          MatchResult(
            !result.matches,
            FailureMessages("wasAn", left, UnquotedString(result.propertyName)),
            FailureMessages("wasNotAn", left, UnquotedString(result.propertyName))
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * myFish should (not be theSameInstanceAs (redFish) and not be theSameInstanceAs (blueFish))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          MatchResult(
            resultOfTheSameInstanceAsApplication.right ne left,
            FailureMessages("wasSameInstanceAs", left, resultOfTheSameInstanceAsApplication.right),
            FailureMessages("wasNotSameInstanceAs", left, resultOfTheSameInstanceAsApplication.right)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should ((not be (17.1 plusOrMinus 0.2)) and (not be (27.1 plusOrMinus 0.2)))
     *                         ^
     * </pre>
     */
    def be(doubleTolerance: DoubleTolerance): Matcher[Double] = {
      import doubleTolerance._
      new Matcher[Double] {
        def apply(left: Double): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOhFloat should ((not be (17.1f plusOrMinus 0.2f)) and (not be (27.1f plusOrMinus 0.2f)))
     *                         ^
     * </pre>
     */
    def be(floatTolerance: FloatTolerance): Matcher[Float] = {
      import floatTolerance._
      new Matcher[Float] {
        def apply(left: Float): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenLong should ((not be (19L plusOrMinus 2L)) and (not be (29L plusOrMinus 2L)))
     *                        ^
     * </pre>
     */
    def be(longTolerance: LongTolerance): Matcher[Long] = {
      import longTolerance._
      new Matcher[Long] {
        def apply(left: Long): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenInt should ((not be (19 plusOrMinus 2)) and (not be (29 plusOrMinus 2)))
     *                       ^
     * </pre>
     */
    def be(intTolerance: IntTolerance): Matcher[Int] = {
      import intTolerance._
      new Matcher[Int] {
        def apply(left: Int): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenShort should ((not be (19.toShort plusOrMinus 2.toShort)) and (not be (29.toShort plusOrMinus 2.toShort)))
     *                         ^
     * </pre>
     */
    def be(shortTolerance: ShortTolerance): Matcher[Short] = {
      import shortTolerance._
      new Matcher[Short] {
        def apply(left: Short): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenByte should ((not be (19.toByte plusOrMinus 2.toByte)) and (not be (29.toByte plusOrMinus 2.toByte)))
     *                        ^
     * </pre>
     */
    def be(byteTolerance: ByteTolerance): Matcher[Byte] = {
      import byteTolerance._
      new Matcher[Byte] {
        def apply(left: Byte): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables <code>be</code> to be used for inequality comparison. Here are some examples:
     *
     * <pre class="stHighlight">
     * result should not be (None)
     *                      ^
     * result should not be (Some(1))
     *                      ^
     * result should not be (true)
     *                      ^
     * result should not be (false)
     *                      ^
     * sum should not be (19)
     *                   ^
     * </pre>
     */
    def be(right: Any): Matcher[Any] = {
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          left match {
            case null =>
              MatchResult(
                right != null, 
                FailureMessages("wasNull"),
                FailureMessages("wasNotNull", right),
                FailureMessages("midSentenceWasNull"),
                FailureMessages("wasNotNull", right)
              )
            case _ => 
              MatchResult(
                !areEqualComparingArraysStructurally(left, right),
                FailureMessages("wasEqualTo", left, right),
                FailureMessages("wasNotEqualTo", left, right)
              )
          }
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not fullyMatch regex ("Hel*o") and not include ("orld"))
     *                    ^
     * </pre>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegexString = resultOfRegexWordApplication.regex.toString
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !java.util.regex.Pattern.matches(rightRegexString, left),
            FailureMessages("fullyMatchedRegex", left, UnquotedString(rightRegexString)),
            FailureMessages("didNotFullyMatchRegex", left, UnquotedString(rightRegexString))
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not include regex ("Hel.o") and not include regex ("""(-)?(\d+)(\.\d*)?"""))
     *                    ^
     * </pre>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegex = resultOfRegexWordApplication.regex
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !rightRegex.findFirstIn(left).isDefined,
            FailureMessages("includedRegex", left, rightRegex),
            FailureMessages("didNotIncludeRegex", left, rightRegex)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not include ("cat") and not include ("1.7"))
     *                    ^
     * </pre>
     */
    def include(expectedSubstring: String): Matcher[String] = {
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !(left.indexOf(expectedSubstring) >= 0), 
            FailureMessages("includedSubstring", left, expectedSubstring),
            FailureMessages("didNotIncludeSubstring", left, expectedSubstring)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not startWith regex ("hel*o") and not endWith regex ("wor.d"))
     *                    ^
     * </pre>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegex = resultOfRegexWordApplication.regex
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !rightRegex.pattern.matcher(left).lookingAt,
            FailureMessages("startedWithRegex", left, rightRegex),
            FailureMessages("didNotStartWithRegex", left, rightRegex)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should ((not startWith ("red")) and (not startWith ("1.7")))
     *                     ^
     * </pre>
     */
    def startWith(expectedSubstring: String): Matcher[String] = {
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            left.indexOf(expectedSubstring) != 0,
            FailureMessages("startedWith", left, expectedSubstring),
            FailureMessages("didNotStartWith", left, expectedSubstring)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not endWith regex ("wor.d") and not startWith regex ("Hel*o"))
     *                    ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegex = resultOfRegexWordApplication.regex
      new Matcher[String] {
        def apply(left: String): MatchResult = {
          val allMatches = rightRegex.findAllIn(left)
          MatchResult(
            !(allMatches.hasNext && (allMatches.end == left.length)),
            FailureMessages("endedWithRegex", left, rightRegex),
            FailureMessages("didNotEndWithRegex", left, rightRegex)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not endWith ("blue") and not endWith ("1.7"))
     *                    ^
     * </pre>
     */
    def endWith(expectedSubstring: String): Matcher[String] = {
      new Matcher[String] {
        def apply(left: String): MatchResult = {
          MatchResult(
            !(left endsWith expectedSubstring),
            FailureMessages("endedWith", left, expectedSubstring),
            FailureMessages("didNotEndWith", left, expectedSubstring)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not contain (5) and not contain (3))
     *                         ^
     * </pre>
     */
    def contain[T](expectedElement: T): Matcher[GenTraversable[T]] = {
      new Matcher[GenTraversable[T]] {
        def apply(left: GenTraversable[T]): MatchResult = {
          MatchResult(
            !(left.exists(_ == expectedElement)),
            FailureMessages("containedExpectedElement", left, expectedElement),
            FailureMessages("didNotContainExpectedElement", left, expectedElement)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain key ("three"))
     *                                         ^
     * </pre>
     */
    def contain[K](resultOfKeyWordApplication: ResultOfKeyWordApplication[K]): Matcher[scala.collection.GenMap[K, Any]] = {
      val expectedKey = resultOfKeyWordApplication.expectedKey
      new Matcher[scala.collection.GenMap[K, Any]] {
        def apply(left: scala.collection.GenMap[K, Any]): MatchResult = {
          MatchResult(
            !(left.exists(_._1 == expectedKey)),
            FailureMessages("containedKey", left, expectedKey),
            FailureMessages("didNotContainKey", left, expectedKey)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain value (3))
     *                                         ^
     * </pre>
     */
    def contain[K, V](resultOfValueWordApplication: ResultOfValueWordApplication[V]): Matcher[scala.collection.GenMap[K, V] forSome { type K }] = {
      val expectedValue = resultOfValueWordApplication.expectedValue
      new Matcher[scala.collection.GenMap[K, V] forSome { type K }] {
        def apply(left: scala.collection.GenMap[K, V] forSome { type K }): MatchResult = {
          MatchResult(
            !(left.exists(_._2 == expectedValue)),
            FailureMessages("containedValue", left, expectedValue),
            FailureMessages("didNotContainValue", left, expectedValue)
          )
        }
      }
    }
  }

  /**
   * This field enables syntax like the following: 
   *
   * <pre class="stHighlight">
   * myFile should (not be an (directory) and not have ('name ("foo.bar")))
   *                ^
   * </pre>
   */
  val not = new NotWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * obj should (be theSameInstanceAs (string) and be theSameInstanceAs (string))
   *             ^
   * </pre>
   */
  val be = new BeWord

/*
    In HaveWord's methods key, value, length, and size, I can give type parameters.
    The type HaveWord can contain a key method that takes a S or what not, and returns a matcher, which
    stores the key value in a val and whose apply method checks the passed map for the remembered key. This one would be used in things like:

    map should { have key 9 and have value "bob" }

    There's an overloaded should method on Shouldifier that takes a HaveWord. This method results in
    a different type that also has a key method that takes an S. So when you say:

    map should have key 9

    what happens is that this alternate should method gets invoked. The result is this other class that
    has a key method, and its constructor takes the map and stores it in a val. So this time when key is
    invoked, it checks to make sure the passed key is in the remembered map, and does the assertion.

    length and size can probably use structural types, because I want to use length on string and array for
    starters, and other people may create classes that have length methods. Would be nice to be able to use them.
  */

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * list should (have length (3) and not contain ('a'))
   *              ^
   * </pre>
   */
  val have = new HaveWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * list should (contain ('a') and have length (7))
   *              ^
   * </pre>
   */
  val contain = new ContainWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (include ("hope") and not startWith ("no"))
   *                ^
   * </pre>
   */
  val include = new IncludeWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (fullyMatch regex ("Hel*o, wor.d") and not have length (99))
   *                ^
   * </pre>
   */
  val fullyMatch = new FullyMatchWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (startWith ("Four") and include ("year"))
   *                ^
   * </pre>
   */
  val startWith = new StartWithWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (endWith ("ago") and include ("score"))
   *                ^
   * </pre>
   */
  val endWith = new EndWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfLengthWordApplication(val expectedLength: Long) extends HavePropertyMatcher[AnyRef, Long] {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "hi" should not have (length (3))
     *                      ^
     * </pre>
     *
     * <p>
     * This reason <code>ResultOfLengthWordApplication</code> is a <code>HavePropertyMatcher[AnyRef, Long]</code> is
     * so that you don't have to remember whether <code>length</code> needs to be surrounded by parentheses when following
     * <code>have</code>. Only <code>length</code> and <code>size</code> can be used without parentheses: everything else
     * needs the parentheses. So this approach means that if you use the unneeded parentheses with <code>length</code> and
     * <code>size</code>, it will still work. This <code>apply</code> method uses reflection to find and access the <code>length</code>
     * property on the passed <code>objectWithProperty</code>. Therefore if the object does not have the appropriate structure, the expression
     * will compile, but at will produce a <code>TestFailedException</code> at runtime.
     * </p>
     */
    def apply(objectWithProperty: AnyRef): HavePropertyMatchResult[Long] = {

      accessProperty(objectWithProperty, 'length, false) match {

        case None =>

          throw newTestFailedException(Resources("propertyNotFound", "length", expectedLength.toString, "getLength"))

        case Some(result) =>

          new HavePropertyMatchResult[Long](
            result == expectedLength,
            "length",
            expectedLength,
            result match {
              case value: Byte => value.toLong
              case value: Short => value.toLong
              case value: Int => value.toLong
              case value: Long => value
              case _ => throw newTestFailedException(Resources("lengthPropertyNotAnInteger"))
            }
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class LengthWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "hi" should not have length (3)
     *                             ^
     * </pre>
     */
    def apply(expectedLength: Long): ResultOfLengthWordApplication = new ResultOfLengthWordApplication(expectedLength)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "hi" should not have length (3)
   *                      ^
   * </pre>
   */
  val length = new LengthWord
 
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfSizeWordApplication(val expectedSize: Long) extends HavePropertyMatcher[AnyRef, Long] {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * set should not have (size (3))
     *                     ^
     * </pre>
     *
     * <p>
     * This reason <code>ResultOfSizeWordApplication</code> is a <code>HavePropertyMatcher[AnyRef, Long]</code> is
     * so that you don't have to remember whether <code>size</code> needs to be surrounded by parentheses when following
     * <code>have</code>. Only <code>length</code> and <code>size</code> can be used without parentheses: everything else
     * needs the parentheses. So this approach means that if you use the unneeded parentheses with <code>length</code> and
     * <code>size</code>, it will still work. This <code>apply</code> method uses reflection to find and access the <code>size</code>
     * property on the passed <code>objectWithProperty</code>. Therefore if the object does not have the appropriate structure, the expression
     * will compile, but at will produce a <code>TestFailedException</code> at runtime.
     * </p>
     */
    def apply(objectWithProperty: AnyRef): HavePropertyMatchResult[Long] = {

      accessProperty(objectWithProperty, 'size, false) match {

        case None =>

          throw newTestFailedException(Resources("propertyNotFound", "size", expectedSize.toString, "getSize"))

        case Some(result) =>

          new HavePropertyMatchResult[Long](
            result == expectedSize,
            "size",
            expectedSize,
            result match {
              case value: Byte => value.toLong
              case value: Short => value.toLong
              case value: Int => value.toLong
              case value: Long => value
              case _ => throw newTestFailedException(Resources("sizePropertyNotAnInteger"))
            }
          )
      }
    }
  }


  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class SizeWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * set should not have size (3)
     *                          ^
     * </pre>
     */
    def apply(expectedSize: Long): ResultOfSizeWordApplication = new ResultOfSizeWordApplication(expectedSize)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * set should not have size (3)
   *                     ^
   * </pre>
   */
  val size = new SizeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfElementWordApplication[T](val expectedElement: T)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfKeyWordApplication[T](val expectedKey: T)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class KeyWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should not contain key (10)
     *                            ^
     * </pre>
     */
    def apply[T](expectedKey: T): ResultOfKeyWordApplication[T] = new ResultOfKeyWordApplication(expectedKey)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * map should not contain key (10)
   *                        ^
   * </pre>
   */
  val key = new KeyWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfValueWordApplication[T](val expectedValue: T)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ValueWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should not contain value (10)
     *                              ^
     * </pre>
     */
    def apply[T](expectedValue: T): ResultOfValueWordApplication[T] = new ResultOfValueWordApplication(expectedValue)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * map should not contain value (10)
   *                        ^
   * </pre>
   */
  val value = new ValueWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAWordToSymbolApplication(val symbol: Symbol)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAWordToBePropertyMatcherApplication[T](val bePropertyMatcher: BePropertyMatcher[T])

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * badBook should not be a ('goodRead)
     *                         ^
     * </pre>
     */
    def apply(symbol: Symbol): ResultOfAWordToSymbolApplication = new ResultOfAWordToSymbolApplication(symbol)

    /**
     * This method enables the following syntax, where, for example, <code>badBook</code> is of type <code>Book</code> and <code>goodRead</code>
     * is a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should not be a (goodRead)
     *                         ^
     * </pre>
     */
    def apply[T](beTrueMatcher: BePropertyMatcher[T]): ResultOfAWordToBePropertyMatcherApplication[T] = new ResultOfAWordToBePropertyMatcherApplication(beTrueMatcher)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * badBook should not be a ('goodRead)
   *                       ^
   * </pre>
   */
  val a = new AWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAnWordToSymbolApplication(val symbol: Symbol)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAnWordToBePropertyMatcherApplication[T](val bePropertyMatcher: BePropertyMatcher[T])

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AnWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * badBook should not be an ('excellentRead)
     *                          ^
     * </pre>
     */
    def apply(symbol: Symbol): ResultOfAnWordToSymbolApplication = new ResultOfAnWordToSymbolApplication(symbol)

    /**
     * This method enables the following syntax, where, for example, <code>badBook</code> is of type <code>Book</code> and <code>excellentRead</code>
     * is a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should not be an (excellentRead)
     *                          ^
     * </pre>
     */
    def apply[T](beTrueMatcher: BePropertyMatcher[T]): ResultOfAnWordToBePropertyMatcherApplication[T] = new ResultOfAnWordToBePropertyMatcherApplication(beTrueMatcher)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * badBook should not be an (excellentRead)
   *                       ^
   * </pre>
   */
  val an = new AnWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfTheSameInstanceAsApplication(val right: AnyRef)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class TheSameInstanceAsPhrase {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * oneString should not be theSameInstanceAs (anotherString)
     *                                           ^
     * </pre>
     */
    def apply(anyRef: AnyRef): ResultOfTheSameInstanceAsApplication = new ResultOfTheSameInstanceAsApplication(anyRef)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * oneString should not be theSameInstanceAs (anotherString)
   *                         ^
   * </pre>
   */
  val theSameInstanceAs: TheSameInstanceAsPhrase = new TheSameInstanceAsPhrase

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""".r)
   *                               ^
   * </pre>
   */
  val regex = new RegexWord

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "eight" should not include substring ("seven")
   *                            ^
   * </pre>
  val substring = new SubstringWord
   */

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class DoubleTolerance(right: Double, tolerance: Double)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class DoublePlusOrMinusWrapper(right: Double) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * sevenDotOh should be (17.0 plusOrMinus 0.2)
     *                            ^
     * </pre>
     */
    def plusOrMinus(tolerance: Double): DoubleTolerance = {
      if (tolerance <= 0.0)
        throw newTestFailedException(Resources("negativeOrZeroRange", tolerance.toString))
      DoubleTolerance(right, tolerance)
    }
  }

  /**
   * Implicitly converts an object of type <code>Double</code> to a <code>DoublePlusOrMinusWrapper</code>,
   * to enable a <code>plusOrMinus</code> method to be invokable on that object.
   */
  implicit def convertDoubleToPlusOrMinusWrapper(right: Double): DoublePlusOrMinusWrapper = new DoublePlusOrMinusWrapper(right)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class FloatTolerance(right: Float, tolerance: Float)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class FloatPlusOrMinusWrapper(right: Float) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * sevenDotOh should be (17.0f plusOrMinus 0.2f)
     *                             ^
     * </pre>
     */
    def plusOrMinus(tolerance: Float): FloatTolerance = {
      if (tolerance <= 0.0f)
        throw newTestFailedException(Resources("negativeOrZeroRange", tolerance.toString))
      FloatTolerance(right, tolerance)
    }
  }

  /**
   * Implicitly converts an object of type <code>Float</code> to a <code>FloatPlusOrMinusWrapper</code>,
   * to enable a <code>plusOrMinus</code> method to be invokable on that object.
   */
  implicit def convertFloatToPlusOrMinusWrapper(right: Float): FloatPlusOrMinusWrapper = new FloatPlusOrMinusWrapper(right)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class LongTolerance(right: Long, tolerance: Long)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class LongPlusOrMinusWrapper(right: Long) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * seven should be (17L plusOrMinus 2)
     *                      ^
     * </pre>
     */
    def plusOrMinus(tolerance: Long): LongTolerance = {
      if (tolerance <= 0L)
        throw newTestFailedException(Resources("negativeOrZeroRange", tolerance.toString))
      LongTolerance(right, tolerance)
    }
  }

  /**
   * Implicitly converts an object of type <code>Long</code> to a <code>LongPlusOrMinusWrapper</code>,
   * to enable a <code>plusOrMinus</code> method to be invokable on that object.
   */
  implicit def convertLongToPlusOrMinusWrapper(right: Long): LongPlusOrMinusWrapper = new LongPlusOrMinusWrapper(right)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class IntTolerance(right: Int, tolerance: Int)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class IntPlusOrMinusWrapper(right: Int) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * seven should be (17 plusOrMinus 2)
     *                     ^
     * </pre>
     */
    def plusOrMinus(tolerance: Int): IntTolerance = {
      if (tolerance <= 0)
        throw newTestFailedException(Resources("negativeOrZeroRange", tolerance.toString))
      IntTolerance(right, tolerance)
    }
  }

  /**
   * Implicitly converts an object of type <code>Int</code> to a <code>IntPlusOrMinusWrapper</code>,
   * to enable a <code>plusOrMinus</code> method to be invokable on that object.
   */
  implicit def convertIntToPlusOrMinusWrapper(right: Int): IntPlusOrMinusWrapper = new IntPlusOrMinusWrapper(right)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class ShortTolerance(right: Short, tolerance: Short)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ShortPlusOrMinusWrapper(right: Short) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * seven should be (17.toShort plusOrMinus 2.toShort)
     *                             ^
     * </pre>
     */
    def plusOrMinus(tolerance: Short): ShortTolerance = {
      if (tolerance <= 0)
        throw newTestFailedException(Resources("negativeOrZeroRange", tolerance.toString))
      ShortTolerance(right, tolerance)
    }
  }

  /**
   * Implicitly converts an object of type <code>Short</code> to a <code>ShortPlusOrMinusWrapper</code>,
   * to enable a <code>plusOrMinus</code> method to be invokable on that object.
   */
  implicit def convertShortToPlusOrMinusWrapper(right: Short): ShortPlusOrMinusWrapper = new ShortPlusOrMinusWrapper(right)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class ByteTolerance(right: Byte, tolerance: Byte)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class BytePlusOrMinusWrapper(right: Byte) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * seven should be (17.toByte plusOrMinus 2.toByte)
     *                            ^
     * </pre>
     */
    def plusOrMinus(tolerance: Byte): ByteTolerance = {
      if (tolerance <= 0)
        throw newTestFailedException(Resources("negativeOrZeroRange", tolerance.toString))
      ByteTolerance(right, tolerance)
    }
  }

  /**
   * Implicitly converts an object of type <code>Byte</code> to a <code>BytePlusOrMinusWrapper</code>,
   * to enable a <code>plusOrMinus</code> method to be invokable on that object.
   */
  implicit def convertByteToPlusOrMinusWrapper(right: Byte): BytePlusOrMinusWrapper = new BytePlusOrMinusWrapper(right)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForSize[A <: AnyRef : Size](left: A, shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

/*  I just added this whole thing in here for completeness when doing SizeShouldWrapper. Write some tests to prove it is needed.
// TODO: This should be for "sizey should not have size (12)" Try that test.
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
             if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              left,
              right
            )
          )
      }
    }
*/
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForLength[A <: AnyRef : Length](left: A, shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

/* TODO What's going on? Why can I drop this and still get a compile
// TODO: This should be for "lengthy should not have length (12)" Try that test.
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
             if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              left,
              right
            )
          )
      }
    }
*/
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForExtent[A : Extent](left: A, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2)
     *                      ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    def length(expectedLength: Int)(implicit len: Length[A]) {
      // val len = implicitly[Length[A]]
      // if ((len.extentOf(left.asInstanceOf[A]) == expectedLength) != shouldBeTrue)
      if ((len.extentOf(left) == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2L)
     *                      ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Long</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    def length(expectedLength: Long)(implicit len: Length[A]) {
      // val len = implicitly[Length[A]]
      // if ((len.extentOf(left.asInstanceOf[A]) == expectedLength) != shouldBeTrue)
      if ((len.extentOf(left) == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have size (2)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>size</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>Traversable</code> and <code>java.util.Collection</code>.
     * </p>
     */
    def size(expectedSize: Int)(implicit sz: Size[A]) {
      // val sz = implicitly[Size[T]]
      // if ((sz.extentOf(left.asInstanceOf[T]) == expectedSize) != shouldBeTrue)
      if ((sz.extentOf(left) == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have size (2L)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>size</code> property structure
     * of type <code>Long</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>Traversable</code> and <code>java.util.Collection</code>.
     * </p>
     */
    def size(expectedSize: Long)(implicit sz: Size[A]) {
      // val sz = implicitly[Size[T]]
      // if ((sz.extentOf(left.asInstanceOf[T]) == expectedSize) != shouldBeTrue)
      if ((sz.extentOf(left) == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }
  }

/*
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForLength[A : Length](left: A, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2)
     *                      ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    def length(expectedLength: Int) {
      val len = implicitly[Length[A]]
      if ((len.extentOf(left) == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2L)
     *                      ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Long</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    def length(expectedLength: Long) {
      val len = implicitly[Length[A]]
      if ((len.extentOf(left) == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForSize[A : Size](left: A, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have size (2)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>size</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>Traversable</code> and <code>java.util.Collection</code>.
     * </p>
     */
    def size(expectedSize: Int) {
      val sz = implicitly[Size[A]]
      if ((sz.extentOf(left) == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have size (2L)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>size</code> property structure
     * of type <code>Long</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>Traversable</code> and <code>java.util.Collection</code>.
     * </p>
     */
    def size(expectedSize: Long) {
      val sz = implicitly[Size[A]]
      if ((sz.extentOf(left) == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }
  }
*/

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfLessThanComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be < (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be < (10) and not be > (17))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left < right
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfGreaterThanComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be > (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be > (10) and not be < (7))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left > right
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfLessThanOrEqualToComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be <= (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be <= (10) and not be > (17))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left <= right
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfGreaterThanOrEqualToComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be >= (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be >= (10) and not be < (7))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left >= right
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfTripleEqualsApplication(val right: Any) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be === (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be === (10) and not be > (17))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: Any): Boolean = left == right
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be < (10) and not be > (17))
   *                    ^
   * </pre>
   */
  def <[T <% Ordered[T]] (right: T): ResultOfLessThanComparison[T] =
    new ResultOfLessThanComparison(right)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be > (10) and not be < (7))
   *                    ^
   * </pre>
   */
  def >[T <% Ordered[T]] (right: T): ResultOfGreaterThanComparison[T] =
    new ResultOfGreaterThanComparison(right)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be <= (10) and not be > (17))
   *                    ^
   * </pre>
   */
  def <=[T <% Ordered[T]] (right: T): ResultOfLessThanOrEqualToComparison[T] =
    new ResultOfLessThanOrEqualToComparison(right)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be >= (10) and not be < (7))
   *                    ^
   * </pre>
   */
  def >=[T <% Ordered[T]] (right: T): ResultOfGreaterThanOrEqualToComparison[T] =
    new ResultOfGreaterThanOrEqualToComparison(right)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * num should not be === (10)
   *                   ^
   * </pre>
   */
  def === (right: Any): ResultOfTripleEqualsApplication =
    new ResultOfTripleEqualsApplication(right)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfEvaluatingApplication(val fun: () => Any)

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * evaluating { "hi".charAt(-1) } should produce [StringIndexOutOfBoundsException]
   * ^
   * </pre>
   */
  def evaluating(fun: => Any): ResultOfEvaluatingApplication =
    new ResultOfEvaluatingApplication(fun _)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfProduceInvocation[T](val clazz: Class[T])

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * evaluating { "hi".charAt(-1) } should produce [StringIndexOutOfBoundsException]
   * ^
   * </pre>
   */
  def produce[T](implicit manifest: Manifest[T]): ResultOfProduceInvocation[T] =
    new ResultOfProduceInvocation(manifest.erasure.asInstanceOf[Class[T]])

  // For safe keeping
  private implicit def nodeToCanonical(node: scala.xml.Node) = new Canonicalizer(node)

  private class Canonicalizer(node: scala.xml.Node) {

    def toCanonical: scala.xml.Node = {
      node match {
        case elem: scala.xml.Elem =>
          val canonicalizedChildren =
            for (child <- node.child if !child.toString.trim.isEmpty) yield {
              child match {
                case elem: scala.xml.Elem => elem.toCanonical
                case other => other
              }
            }
          new scala.xml.Elem(elem.prefix, elem.label, elem.attributes, elem.scope, canonicalizedChildren: _*)
        case other => other
      }
    }
  }
}

