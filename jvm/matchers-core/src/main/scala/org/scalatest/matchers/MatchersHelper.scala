/*
 * Copyright 2001-2025 Artima, Inc.
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
package matchers

import org.scalatest.exceptions._
import org.scalatest.matchers._
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import scala.util.matching.Regex
import java.lang.reflect.Field
import org.scalactic.{source, Prettifier}
import org.scalatest.UnquotedString
import org.scalatest.matchers.dsl.ResultOfThrownByApplication
import org.scalatest.matchers.dsl.ResultOfBeThrownBy

// TODO: drop generic support for be as an equality comparison, in favor of specific ones.
// TODO: mention on JUnit and TestNG docs that you can now mix in ShouldMatchers or MustMatchers
// TODO: Put links from ShouldMatchers to wherever I reveal the matrix and algo of how properties are checked dynamically.
// TODO: double check that I wrote tests for (length (7)) and (size (8)) in parens
// TODO: document how to turn off the === implicit conversion
// TODO: Document you can use JMock, EasyMock, etc.

private[scalatest] object MatchersHelper {

  // SKIP-SCALATESTJS,NATIVE-START
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

      case (_, Some(method), _) => 
        //DOTTY-ONLY method.setAccessible(true)
        Some(method.invoke(objectWithProperty, Array[AnyRef](): _*))

      case (_, None, Some(getMethod)) => 
        //DOTTY-ONLY getMethod.setAccessible(true)
        Some(getMethod.invoke(objectWithProperty, Array[AnyRef](): _*))

      case (Some(field), None, None) => Some(field.get(objectWithProperty))

      case (None, None, None) => None
    }
  }
  // SKIP-SCALATESTJS,NATIVE-END

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

  def newTestFailedException(message: String, optionalCause: Option[Throwable] = None, pos: source.Position): Throwable = {
    new TestFailedException((_: StackDepthException) => Some(message), optionalCause, pos)
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

  // SKIP-SCALATESTJS,NATIVE-START
  def matchSymbolToPredicateMethod(left: AnyRef, right: Symbol, hasArticle: Boolean, articleIsA: Boolean, prettifier: Prettifier, pos: source.Position): MatchResult = {

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
            FailureMessages.hasNeitherAnOrAnMethod(prettifier, left, UnquotedString(methodNameToInvoke), UnquotedString(methodNameToInvokeWithIs))
          else
            FailureMessages.hasNeitherAOrAnMethod(prettifier, left, UnquotedString(methodNameToInvoke), UnquotedString(methodNameToInvokeWithIs)),
          None,
          pos
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
  // SKIP-SCALATESTJS,NATIVE-END

  def checkPatternMatchAndGroups(matches: Boolean, left: String, pMatcher: java.util.regex.Matcher, regex: Regex, groups: IndexedSeq[String], 
                                 didNotMatchMessage: => String, matchMessage: => String, notGroupAtIndexMessage:  => String, notGroupMessage: => String,
                                 andGroupMessage: => String): MatchResult = {
    if (groups.isEmpty || !matches)
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

  private[scalatest] def checkExpectedException[T](f: => Any, clazz: Class[T], wrongExceptionMessageFun: (Any, Any) => String, exceptionExpectedMessageFun: String => String, pos: source.Position): T = {
    val caught = try {
      f
      None
    }
    catch {
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass)) {
          val s = wrongExceptionMessageFun(clazz.getName, u.getClass.getName)
          throw newTestFailedException(s, Some(u), pos)
        }
        else {
          Some(u)
        }
      }
    }
    caught match {
      case None =>
        val message = exceptionExpectedMessageFun(clazz.getName)
        throw newTestFailedException(message, None, pos)
      case Some(e) => e.asInstanceOf[T] // I know this cast will succeed, becuase iSAssignableFrom succeeded above
    }
  }

  def checkNoException(fun: => Any, pos: source.Position): Assertion = {
    try {
      fun
      Succeeded
    }
    catch {
      case u: Throwable => {
        val message = Resources.exceptionNotExpected(u.getClass.getName)
        throw new TestFailedException((sde: StackDepthException) => Some(message), Some(u), pos)
      }
    }
  }

  def checkThrownBy(clazz: Class[_], thrownBy: ResultOfThrownByApplication, pos: source.Position): Assertion = {
    val caught = try {
      thrownBy.execute()
      None
    }
    catch {
      case u: Throwable => Some(u)
    }
    if (caught.isEmpty) {
      val message = Resources.exceptionExpected(clazz.getName)
      indicateFailure(message, None, pos)
    } else {
      val u = caught.get
      if (!clazz.isAssignableFrom(u.getClass)) {
        val s = Resources.wrongException(clazz.getName, u.getClass.getName)
        indicateFailure(s, Some(u), pos)
      } else indicateSuccess(Resources.exceptionThrown(u.getClass.getName))
    }
  }

  //DOTTY-ONLY import scala.quoted.*
  //DOTTY-ONLY def checkThrownByMacro(clazz: Expr[Class[_]], thrownBy: Expr[ResultOfThrownByApplication])(using quotes: Quotes): Expr[Assertion] = {
  //DOTTY-ONLY   source.Position.withPosition[Assertion]('{(pos: source.Position) => checkThrownBy(${clazz}, ${thrownBy}, pos) })
  //DOTTY-ONLY }

  def checkBeThrownBy(clazz: Class[_], beThrownBy: ResultOfBeThrownBy, pos: source.Position): Assertion = {
    val throwables = beThrownBy.throwables
    val noThrowable = throwables.find(_.isEmpty)
    if (noThrowable.isDefined) {
      val message = Resources.exceptionExpected(clazz.getName)
      indicateFailure(message, None, pos)
    }
    else {
      val unmatch = throwables.map(_.get).find(t => !clazz.isAssignableFrom(t.getClass))
      if (unmatch.isDefined) {
        val u = unmatch.get
        val s = Resources.wrongException(clazz.getName, u.getClass.getName)
        indicateFailure(s, Some(u), pos)
      }
      else indicateSuccess(Resources.exceptionThrown(clazz.getClass.getName))
    }
  }

  //DOTTY-ONLY def checkBeThrownByMacro(clazz: Expr[Class[_]], beThrownBy: Expr[ResultOfBeThrownBy])(using quotes: Quotes): Expr[Assertion] = {
  //DOTTY-ONLY   source.Position.withPosition[Assertion]('{(pos: source.Position) => checkBeThrownBy(${clazz}, ${beThrownBy}, pos) })
  //DOTTY-ONLY }

  def indicateSuccess(message: => String): Assertion = Succeeded

  def indicateSuccess(shouldBeTrue: Boolean, message: => String, negatedMessage: => String): Assertion = Succeeded

  def indicateFailure(failureMessage: => String, optionalCause: Option[Throwable], pos: source.Position): Assertion = {
    val message: String = failureMessage
    throw new TestFailedException((sde: StackDepthException) => Some(message), optionalCause, pos)
  }

  def indicateFailure(failureMessage: => String, optionalCause: Option[Throwable], pos: source.Position, analysis: Option[String]): Assertion = {
    val message: String = failureMessage
    throw new TestFailedException((sde: StackDepthException) => Some(message), optionalCause, Left(pos), None, analysis.map(Vector(_)).getOrElse(Vector.empty))
  }

  def indicateFailure(e: TestFailedException): Assertion =
    throw e
}
