/*
 * Copyright 2001-2014 Artima, Inc.
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

import org.scalatest.{UnquotedString, Suite, FailureMessages, Resources}
import org.scalactic.Prettifier
import org.scalatest.MatchersHelper._
import org.scalatest.words.{ResultOfAnTypeInvocation, ResultOfATypeInvocation}

/**
 * <code>TypeMatcherHelper</code> is called by <code>TypeMatcherMacro</code> to support <code>a [Type]</code> and <code>an [Type]</code> syntax.
 */
object TypeMatcherHelper {

  /**
   * Create a type matcher for the given <code>ResultOfATypeInvocation</code>.
   *
   * @param aType an instance of <code>ResultOfATypeInvocation</code>
   * @return a type <code>Matcher</code>
   */
  def aTypeMatcher(aType: ResultOfATypeInvocation[_]): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val clazz = aType.clazz
        MatchResult(
          clazz.isAssignableFrom(left.getClass),
          Resources("wasNotAnInstanceOf"),
          Resources("wasAnInstanceOf"),
          Vector(left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName)),
          Vector(left, UnquotedString(clazz.getName))
        )
      }
      override def toString: String = "be (" + Prettifier.default(aType) + ")"
    }

  /**
   * Create a type matcher for the given <code>ResultOfAnTypeInvocation</code>.
   *
   * @param anType an instance of <code>ResultOfAnTypeInvocation</code>
   * @return a type <code>Matcher</code>
   */
  def anTypeMatcher(anType: ResultOfAnTypeInvocation[_]): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val clazz = anType.clazz
        MatchResult(
          clazz.isAssignableFrom(left.getClass),
          Resources("wasNotAnInstanceOf"),
          Resources("wasAnInstanceOf"),
          Vector(left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName)),
          Vector(left, UnquotedString(clazz.getName))
        )
      }
      override def toString: String = "be (" + Prettifier.default(anType) + ")"
    }

  /**
   * Create a negated type matcher for the given <code>ResultOfATypeInvocation</code>.
   *
   * @param aType an instance of <code>ResultOfATypeInvocation</code>
   * @return a negated type <code>Matcher</code>
   */
  def notATypeMatcher(aType: ResultOfATypeInvocation[_]): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val clazz = aType.clazz
        MatchResult(
          !clazz.isAssignableFrom(left.getClass),
          Resources("wasAnInstanceOf"),
          Resources("wasNotAnInstanceOf"),
          Vector(left, UnquotedString(clazz.getName)),
          Vector(left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName))
        )
      }
      override def toString: String = "not be " + Prettifier.default(aType)
    }

  /**
   * Create a negated type matcher for the given <code>ResultOfAnTypeInvocation</code>.
   *
   * @param anType an instance of <code>ResultOfAnTypeInvocation</code>
   * @return a negated type <code>Matcher</code>
   */
  def notAnTypeMatcher(anType: ResultOfAnTypeInvocation[_]): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val clazz = anType.clazz
        MatchResult(
          !clazz.isAssignableFrom(left.getClass),
          Resources("wasAnInstanceOf"),
          Resources("wasNotAnInstanceOf"),
          Vector(left, UnquotedString(clazz.getName)),
          Vector(left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName))
        )
      }
      override def toString: String = "not be " + Prettifier.default(anType)
    }

  /**
   * Check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfATypeInvocation</code>.
   * A <code>TestFailedException</code> will be thrown if <code>left</code> is not an instance of the type given by <code>ResultOfATypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param aType an instance of <code>ResultOfATypeInvocation</code>
   */
  def checkAType(left: Any, aType: ResultOfATypeInvocation[_]) {
    val clazz = aType.clazz
    if (!clazz.isAssignableFrom(left.getClass)) {
      val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, clazz.getName)
      throw newTestFailedException(FailureMessages("wasNotAnInstanceOf", left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName)))
    }
  }

  /**
   * Check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfAnTypeInvocation</code>.
   * A <code>TestFailedException</code> will be thrown if <code>left</code> is not an instance of the type given by <code>ResultOfAnTypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param anType an instance of <code>ResultOfAnTypeInvocation</code>
   */
  def checkAnType(left: Any, anType: ResultOfAnTypeInvocation[_]) {
    val clazz = anType.clazz
    if (!clazz.isAssignableFrom(left.getClass)) {
      val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, clazz.getName)
      throw newTestFailedException(FailureMessages("wasNotAnInstanceOf", left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName)))
    }
  }

  /**
   * Based on <code>shouldBeTrue</code> value, check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfATypeInvocation</code>.
   * If <code>shouldBeTrue</code> is true, a <code>TestFailedException</code> will be thrown if <code>left</code> is not an instance of the type given by <code>ResultOfATypeInvocation</code>.
   * If <code>shouldBeTrue</code> is false, a <code>TestFailedException</code> will be thrown if <code>left</code> is an instance of the type given by <code>ResultOfATypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param aType an instance of <code>ResultOfATypeInvocation</code>
   */
  def checkATypeShouldBeTrue(left: Any, aType: ResultOfATypeInvocation[_], shouldBeTrue: Boolean) {
    val clazz = aType.clazz
    if (clazz.isAssignableFrom(left.getClass) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotAnInstanceOf" else "wasAnInstanceOf",
          left,
          UnquotedString(clazz.getName),
          UnquotedString(left.getClass.getName)
        )
      )
    }
  }

  /**
   * Based on <code>shouldBeTrue</code> value, check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfAnTypeInvocation</code>.
   * If <code>shouldBeTrue</code> is true, a <code>TestFailedException</code> will be thrown if <code>left</code> is not an instance of the type given by <code>ResultOfAnTypeInvocation</code>.
   * If <code>shouldBeTrue</code> is false, a <code>TestFailedException</code> will be thrown if <code>left</code> is an instance of the type given by <code>ResultOfAnTypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param anType an instance of <code>ResultOfAnTypeInvocation</code>
   */
  def checkAnTypeShouldBeTrue(left: Any, anType: ResultOfAnTypeInvocation[_], shouldBeTrue: Boolean) {
    val clazz = anType.clazz
    if (clazz.isAssignableFrom(left.getClass) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotAnInstanceOf" else "wasAnInstanceOf",
          left,
          UnquotedString(clazz.getName),
          UnquotedString(left.getClass.getName)
        )
      )
    }
  }

}