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
package org.scalatest

import org.scalatest.matchers._
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import scala.util.matching.Regex
import java.lang.reflect.Field
import scala.collection.Traversable
import Assertions.areEqualComparingArraysStructurally
import org.scalatest.exceptions.TestFailedException
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap
import org.scalactic.Tolerance
import scala.annotation.tailrec
import org.scalactic.Equality
import org.scalatest.words.ShouldVerb
import org.scalatest.matchers.HavePropertyMatcher
import org.scalatest.matchers.HavePropertyMatchResult
import org.scalatest.matchers.BePropertyMatcher
import org.scalatest.matchers.BePropertyMatchResult
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import words.RegexWithGroups

// TODO: drop generic support for be as an equality comparison, in favor of specific ones.
// TODO: mention on JUnit and TestNG docs that you can now mix in ShouldMatchers or MustMatchers
// TODO: Put links from ShouldMatchers to wherever I reveal the matrix and algo of how properties are checked dynamically.
// TODO: double check that I wrote tests for (length (7)) and (size (8)) in parens
// TODO: document how to turn off the === implicit conversion
// TODO: Document you can use JMock, EasyMock, etc.

private[scalatest] object MatchersHelper {

  // SKIP-SCALATESTJS-START
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
  // SKIP-SCALATESTJS-END

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

  // SKIP-SCALATESTJS-START
  def newTestFailedException(message: String, optionalCause: Option[Throwable] = None, stackDepthAdjustment: Int = 0): Throwable = {
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY def newTestFailedException(message: String, optionalCause: Option[Throwable] = None, stackDepthAdjustment: Int = 9): Throwable = {
    val temp = new RuntimeException
    // should not look for anything in the first 2 elements, caller stack element is at 3rd/4th
    // also, it solves the problem when the suite file that mixin in Matchers has the [suiteFileName]:newTestFailedException appears in the top 2 elements
    // this approach should be better than adding && _.getMethodName == newTestFailedException we used previously.
    val elements = temp.getStackTrace.drop(2) 
    // TODO: Perhaps we should add org.scalatest.enablers also here later?
    // TODO: Probably need a MatchersHelper.scala here also
    val stackDepth = elements.indexWhere(st => st.getFileName != "Matchers.scala" && st.getFileName != "MustMatchers.scala" && !st.getClassName.startsWith("org.scalatest.words.")) + 2 // the first 2 elements dropped previously
    optionalCause match {
      case Some(cause) => new TestFailedException(message, cause, stackDepth + stackDepthAdjustment)
      case None => new TestFailedException(message, stackDepth + stackDepthAdjustment)
    }
  }

  def andMatchersAndApply[T](left: T, leftMatcher: Matcher[T], rightMatcher: Matcher[T]): MatchResult = {
    val leftMatchResult = leftMatcher(left)
    val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
    if (!leftMatchResult.matches) leftMatchResult
    else {
      MatchResult(
        rightMatchResult.matches,
        Resources.rawCommaBut,
        Resources.rawCommaAnd,
        Resources.rawCommaBut,
        Resources.rawCommaAnd,
        Vector(NegatedFailureMessage(leftMatchResult), MidSentenceFailureMessage(rightMatchResult)),
        Vector(NegatedFailureMessage(leftMatchResult), MidSentenceNegatedFailureMessage(rightMatchResult)),
        Vector(MidSentenceNegatedFailureMessage(leftMatchResult), MidSentenceFailureMessage(rightMatchResult)),
        Vector(MidSentenceNegatedFailureMessage(leftMatchResult), MidSentenceNegatedFailureMessage(rightMatchResult))
      )
    }
  }

  def orMatchersAndApply[T](left: T, leftMatcher: Matcher[T], rightMatcher: Matcher[T]): MatchResult = {
    val leftMatchResult = leftMatcher(left)
    val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
    if (leftMatchResult.matches) leftMatchResult.copy(matches = true)
    else {
      MatchResult(
        rightMatchResult.matches,
        Resources.rawCommaAnd,
        Resources.rawCommaAnd,
        Resources.rawCommaAnd,
        Resources.rawCommaAnd,
        Vector(FailureMessage(leftMatchResult), MidSentenceFailureMessage(rightMatchResult)),
        Vector(FailureMessage(leftMatchResult), MidSentenceNegatedFailureMessage(rightMatchResult)),
        Vector(MidSentenceFailureMessage(leftMatchResult), MidSentenceFailureMessage(rightMatchResult)),
        Vector(MidSentenceFailureMessage(leftMatchResult), MidSentenceNegatedFailureMessage(rightMatchResult))
      )
    }
  }

  // SKIP-SCALATESTJS-START
  def matchSymbolToPredicateMethod(left: AnyRef, right: Symbol, hasArticle: Boolean, articleIsA: Boolean, stackDepth: Int = 0): MatchResult = {

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
          if (methodNameStartsWithVowel)
            FailureMessages.hasNeitherAnOrAnMethod(left, UnquotedString(methodNameToInvoke), UnquotedString(methodNameToInvokeWithIs))
          else
            FailureMessages.hasNeitherAOrAnMethod(left, UnquotedString(methodNameToInvoke), UnquotedString(methodNameToInvokeWithIs)),
          None, 
          stackDepth
        )

      case Some(result) =>

        val (wasNot, was) =
          if (hasArticle) {
            if (articleIsA) (Resources.rawWasNotA, Resources.rawWasA) else (Resources.rawWasNotAn, Resources.rawWasAn)
          }
          else (Resources.rawWasNot, Resources.rawWas)

        MatchResult(
          result == true, // Right now I just leave the return value of accessProperty as Any
          wasNot,
          was,
          Vector(left, UnquotedString(propertyName))
        )
    }
  }
  // SKIP-SCALATESTJS-END

  def checkPatternMatchAndGroups(matches: Boolean, left: String, pMatcher: java.util.regex.Matcher, regex: Regex, groups: IndexedSeq[String], 
                                 didNotMatchMessage: => String, matchMessage: => String, notGroupAtIndexMessage:  => String, notGroupMessage: => String,
                                 andGroupMessage: => String): MatchResult = {
    if (groups.size == 0 || !matches)
      MatchResult(
        matches, 
        didNotMatchMessage,
        matchMessage,
        Vector(left, UnquotedString(regex.toString))
      )
    else {
      val count = pMatcher.groupCount
      val failed = // Find the first group that fails
        groups.zipWithIndex.find { case (group, idx) => 
          val groupIdx = idx + 1
          !(groupIdx <= count && pMatcher.group(groupIdx) == group)
        }
      failed match {
        case Some((group, idx)) =>
          MatchResult(
            false, 
            if (groups.size > 1) notGroupAtIndexMessage else notGroupMessage,
            andGroupMessage,
            if (groups.size > 1) Vector(left, UnquotedString(regex.toString), pMatcher.group(idx + 1), UnquotedString(group), idx) else Vector(left, UnquotedString(regex.toString), pMatcher.group(1), UnquotedString(group)), 
            Vector(left, UnquotedString(regex.toString), UnquotedString(groups.mkString(", ")))
          )
        case None => 
          // None of group failed
          MatchResult(
            true, 
            notGroupMessage,
            andGroupMessage,
            Vector(left, UnquotedString(regex.toString), pMatcher.group(1),  UnquotedString(groups.mkString(", "))), 
            Vector(left, UnquotedString(regex.toString), UnquotedString(groups.mkString(", ")))
          )
      }
    }
  }
  
  def fullyMatchRegexWithGroups(left: String, regex: Regex, groups: IndexedSeq[String]): MatchResult = {
    val pMatcher = regex.pattern.matcher(left)
    val matches = pMatcher.matches
    checkPatternMatchAndGroups(matches, left, pMatcher, regex, groups, Resources.rawDidNotFullyMatchRegex, Resources.rawFullyMatchedRegex, Resources.rawFullyMatchedRegexButNotGroupAtIndex,
                               Resources.rawFullyMatchedRegexButNotGroup, Resources.rawFullyMatchedRegexAndGroup)
  }
  
  def startWithRegexWithGroups(left: String, regex: Regex, groups: IndexedSeq[String]): MatchResult = {
    val pMatcher = regex.pattern.matcher(left)
    val matches = pMatcher.lookingAt
    checkPatternMatchAndGroups(matches, left, pMatcher, regex, groups, Resources.rawDidNotStartWithRegex, Resources.rawStartedWithRegex, Resources.rawStartedWithRegexButNotGroupAtIndex,
      Resources.rawStartedWithRegexButNotGroup, Resources.rawStartedWithRegexAndGroup)
  }
  
  def endWithRegexWithGroups(left: String, regex: Regex, groups: IndexedSeq[String]): MatchResult = {
    val pMatcher = regex.pattern.matcher(left)
    val found = pMatcher.find
    val matches = found && pMatcher.end == left.length
    checkPatternMatchAndGroups(matches, left, pMatcher, regex, groups, Resources.rawDidNotEndWithRegex, Resources.rawEndedWithRegex, Resources.rawEndedWithRegexButNotGroupAtIndex,
                               Resources.rawEndedWithRegexButNotGroup, Resources.rawEndedWithRegexAndGroup)
  }
  
  def includeRegexWithGroups(left: String, regex: Regex, groups: IndexedSeq[String]): MatchResult = {
    val pMatcher = regex.pattern.matcher(left)
    val matches = pMatcher.find
    checkPatternMatchAndGroups(matches, left, pMatcher, regex, groups, Resources.rawDidNotIncludeRegex, Resources.rawIncludedRegex, Resources.rawIncludedRegexButNotGroupAtIndex,
                               Resources.rawIncludedRegexButNotGroup, Resources.rawIncludedRegexAndGroup)
  }

  def indicateSuccess(message: => String): Assertion = Succeeded

  def indicateFailure(failureMessage: => String): Assertion =
    throw newTestFailedException(failureMessage)

  def indicateFailure(shouldBeTrue: Boolean, failureMessage: => String, negatedFailureMessage: => String): Assertion =
    throw newTestFailedException(
      if (shouldBeTrue) failureMessage else negatedFailureMessage
    )

  def indicateFailure(shouldBeTrue: Boolean, failureMessage: => String, negatedFailureMessage: => String, optionalCause: Option[Throwable] = None, stackDepthAdjustment: Int = 0): Assertion =
    throw newTestFailedException(
      if (shouldBeTrue) failureMessage else negatedFailureMessage,
      None,
      stackDepthAdjustment
    )

  def indicateFailure(failureMessage: => String, optionalCause: Option[Throwable], stackDepthAdjustment: Int): Assertion =
    throw newTestFailedException(
      failureMessage,
      optionalCause,
      stackDepthAdjustment
    )

  def indicateFailure(shouldBeTrue: Boolean, withFriendlyReminder: Boolean, failureMessageWithFriendlyReminder: => String, failureMessageWithoutFriendlyReminder: => String,
                                         negatedFailureMessageWithFriendlyReminder: => String, negatedFailureMessageWithoutFriendlyReminder: => String, optionalCause: Option[Throwable],
                                         stackDepthAdjustment: Int): Assertion =
    throw newTestFailedException(
      if (shouldBeTrue)
        if (withFriendlyReminder)
          failureMessageWithFriendlyReminder
        else
          failureMessageWithoutFriendlyReminder
      else
      if (withFriendlyReminder)
        negatedFailureMessageWithFriendlyReminder
      else
        negatedFailureMessageWithoutFriendlyReminder,
      None,
      stackDepthAdjustment
    )
}
