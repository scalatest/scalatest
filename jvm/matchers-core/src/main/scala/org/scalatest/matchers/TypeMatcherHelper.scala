/*
 * Copyright 2001-2024 Artima, Inc.
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

import org.scalactic.source
import org.scalatest.{FailureMessages, Resources, Suite, UnquotedString}
import org.scalactic.Prettifier
import org.scalatest.matchers.MatchersHelper._
import org.scalatest.matchers.dsl.{ResultOfATypeInvocation, ResultOfAnTypeInvocation}

import scala.reflect.ClassTag
//import org.scalatest.words.{FactResultOfAnTypeInvocation, FactResultOfATypeInvocation}

/**
 * <code>TypeMatcherHelper</code> is called by <code>TypeMatcherMacro</code> to support <code>a [Type]</code> and <code>an [Type]</code> syntax.
 *
 * <p>
 * This object needs to be public so that the macro-generated code can be compiled. It is expected that ScalaTest
 * users would ever need to use <code>TypeMatcherHelper</code> directly.
 * </p>
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
        val clazzTag = aType.clazzTag
        MatchResult(
          conform(clazzTag, left),
          Resources.rawWasNotAnInstanceOf,
          Resources.rawWasAnInstanceOf,
          Vector(left, UnquotedString(clazzTag.toString), UnquotedString(left.getClass.getName)),
          Vector(left, UnquotedString(clazzTag.toString))
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
        val clazzTag = anType.clazzTag
        MatchResult(
          conform(clazzTag, left),
          Resources.rawWasNotAnInstanceOf,
          Resources.rawWasAnInstanceOf,
          Vector(left, UnquotedString(clazzTag.toString), UnquotedString(left.getClass.getName)),
          Vector(left, UnquotedString(clazzTag.toString))
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
        val clazzTag = aType.clazzTag
        MatchResult(
          !conform(clazzTag, left),
          Resources.rawWasAnInstanceOf,
          Resources.rawWasNotAnInstanceOf,
          Vector(left, UnquotedString(clazzTag.toString)),
          Vector(left, UnquotedString(clazzTag.toString), UnquotedString(left.getClass.getName))
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
        val clazzTag = anType.clazzTag
        MatchResult(
          !conform(clazzTag, left),
          Resources.rawWasAnInstanceOf,
          Resources.rawWasNotAnInstanceOf,
          Vector(left, UnquotedString(clazzTag.toString)),
          Vector(left, UnquotedString(clazzTag.toString), UnquotedString(left.getClass.getName))
        )
      }
      override def toString: String = "not be " + Prettifier.default(anType)
    }

  private def conform[T](classTag: ClassTag[T], x: Any): Boolean =
    if (classTag == ClassTag.AnyVal)
      x.isInstanceOf[Byte] ||
      x.isInstanceOf[Short] ||
      x.isInstanceOf[Char] ||
      x.isInstanceOf[Int] ||
      x.isInstanceOf[Long] ||
      x.isInstanceOf[Float] ||
      x.isInstanceOf[Double] ||
      x.isInstanceOf[Boolean]
    else 
      classTag.unapply(x).isDefined

  /**
   * Check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfATypeInvocation</code>.
   * A <code>TestFailedException</code> will be thrown if <code>left</code> is not an instance of the type given by <code>ResultOfATypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param aType an instance of <code>ResultOfATypeInvocation</code>
   */
  def assertAType(left: Any, aType: ResultOfATypeInvocation[_], prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = {
    val clazz = aType.clazz
    if (!conform(aType.clazzTag, left)) {
      val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, aType.clazzTag.toString)
      throw newTestFailedException(FailureMessages.wasNotAnInstanceOf(prettifier, left, UnquotedString(aType.clazzTag.toString), UnquotedString(left.getClass.getName)), None, pos)
    }
    org.scalatest.Succeeded
  }

  /**
   * Check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfATypeInvocation</code>.
   * A <code>Fact.No</code> will be returned if <code>left</code> is not an instance of the type given by <code>ResultOfATypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param aType an instance of <code>ResultOfATypeInvocation</code>
   */
  /*private[scalatest] def expectAType(left: Any, aType: ResultOfATypeInvocation[_], prettifier: Prettifier): org.scalatest.Fact = {
    val clazz = aType.clazz
    if (!conform(clazz, left.getClass)) {
      val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, clazz.getName)
      org.scalatest.Fact.No(FailureMessages.wasNotAnInstanceOf(aType.prettifier, left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName)))(aType.prettifier)
    }
    else org.scalatest.Fact.Yes(FailureMessages.wasAnInstanceOf(aType.prettifier, left, UnquotedString(clazz.getName)))(aType.prettifier)
  }*/

  /**
   * Check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfAnTypeInvocation</code>.
   * A <code>TestFailedException</code> will be thrown if <code>left</code> is not an instance of the type given by <code>ResultOfAnTypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param anType an instance of <code>ResultOfAnTypeInvocation</code>
   */
  def assertAnType(left: Any, anType: ResultOfAnTypeInvocation[_], prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = {
    val clazz = anType.clazz
    if (!conform(anType.clazzTag, left)) {
      val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, anType.clazzTag.toString)
      throw newTestFailedException(FailureMessages.wasNotAnInstanceOf(prettifier, left, UnquotedString(anType.clazzTag.toString), UnquotedString(left.getClass.getName)), None, pos)
    }
    org.scalatest.Succeeded
  }

  /**
   * Check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfAnTypeInvocation</code>.
   * A <code>Fact.No</code> will be returned if <code>left</code> is not an instance of the type given by <code>ResultOfAnTypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param anType an instance of <code>ResultOfAnTypeInvocation</code>
   */
  /*private[scalatest] def expectAnType(left: Any, anType: ResultOfAnTypeInvocation[_]): org.scalatest.Fact = {
    val clazz = anType.clazz
    if (!conform(clazz, left)) {
      val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, clazz.getName)
      org.scalatest.Fact.No(FailureMessages.wasNotAnInstanceOf(anType.prettifier, left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName)))(anType.prettifier)
    } else org.scalatest.Fact.Yes(FailureMessages.wasAnInstanceOf(anType.prettifier, left, UnquotedString(clazz.getName)))(anType.prettifier)
  }*/

  /**
   * Based on <code>shouldBeTrue</code> value, check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfATypeInvocation</code>.
   * If <code>shouldBeTrue</code> is true, a <code>TestFailedException</code> will be thrown if <code>left</code> is not an instance of the type given by <code>ResultOfATypeInvocation</code>.
   * If <code>shouldBeTrue</code> is false, a <code>TestFailedException</code> will be thrown if <code>left</code> is an instance of the type given by <code>ResultOfATypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param aType an instance of <code>ResultOfATypeInvocation</code>
   */
  def assertATypeShouldBeTrue(left: Any, aType: ResultOfATypeInvocation[_], shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = {
    val clazz = aType.clazz
    if (conform(aType.clazzTag, left) != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.wasNotAnInstanceOf(prettifier, left, UnquotedString(aType.clazzTag.toString), UnquotedString(left.getClass.getName))
        else
          FailureMessages.wasAnInstanceOf(prettifier, left, UnquotedString(aType.clazzTag.toString)),
        None,
        pos
      )
    } else org.scalatest.Succeeded
  }

  /**
   * Based on <code>shouldBeTrue</code> value, check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfATypeInvocation</code>.
   * If <code>shouldBeTrue</code> is true, a <code>Fact.No</code> will be returned if <code>left</code> is not an instance of the type given by <code>ResultOfATypeInvocation</code>.
   * If <code>shouldBeTrue</code> is false, a <code>Fact.No</code> will be returned if <code>left</code> is an instance of the type given by <code>ResultOfATypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param aType an instance of <code>ResultOfATypeInvocation</code>
   */
  /*def expectATypeWillBeTrue(left: Any, aType: FactResultOfATypeInvocation[_], shouldBeTrue: Boolean): org.scalatest.Fact = {
    val clazz = aType.clazz
    if (conform(clazz, left) != shouldBeTrue) {
      org.scalatest.Fact.No(
        if (shouldBeTrue)
          FailureMessages.wasNotAnInstanceOf(prettifier, left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName))
        else
          FailureMessages.wasAnInstanceOf(prettifier, left, UnquotedString(clazz.getName))
      )
    }
    else
      org.scalatest.Fact.Yes(
        if (shouldBeTrue)
          FailureMessages.wasAnInstanceOf(prettifier, left, UnquotedString(clazz.getName))
        else
          FailureMessages.wasNotAnInstanceOf(prettifier, left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName))
      )
  }*/

  /**
   * Based on <code>shouldBeTrue</code> value, check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfAnTypeInvocation</code>.
   * If <code>shouldBeTrue</code> is true, a <code>TestFailedException</code> will be thrown if <code>left</code> is not an instance of the type given by <code>ResultOfAnTypeInvocation</code>.
   * If <code>shouldBeTrue</code> is false, a <code>TestFailedException</code> will be thrown if <code>left</code> is an instance of the type given by <code>ResultOfAnTypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param anType an instance of <code>ResultOfAnTypeInvocation</code>
   */
  def assertAnTypeShouldBeTrue(left: Any, anType: ResultOfAnTypeInvocation[_], shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = {
    val clazz = anType.clazz
    if (conform(anType.clazzTag, left) != shouldBeTrue) {
      throw newTestFailedException(
        if (shouldBeTrue)
          FailureMessages.wasNotAnInstanceOf(prettifier, left, UnquotedString(anType.clazzTag.toString), UnquotedString(left.getClass.getName))
        else
          FailureMessages.wasAnInstanceOf(prettifier, left, UnquotedString(anType.clazzTag.toString)),
        None,
        pos
      )
    } else org.scalatest.Succeeded
  }

  /*
   * Based on <code>shouldBeTrue</code> value, check if the given <code>left</code> is an instance of the type as described in the given <code>ResultOfAnTypeInvocation</code>.
   * If <code>shouldBeTrue</code> is true, a <code>Fact.No</code> will be returned if <code>left</code> is not an instance of the type given by <code>ResultOfAnTypeInvocation</code>.
   * If <code>shouldBeTrue</code> is false, a <code>Fact.No</code> will be returned if <code>left</code> is an instance of the type given by <code>ResultOfAnTypeInvocation</code>.
   *
   * @param left the left-hand-side (LHS) to be checked for the type
   * @param anType an instance of <code>ResultOfAnTypeInvocation</code>
   */
  /*def expectAnTypeWillBeTrue(left: Any, anType: FactResultOfAnTypeInvocation[_], shouldBeTrue: Boolean): org.scalatest.Fact = {
    val clazz = anType.clazz
    if (conform(clazz, left) != shouldBeTrue) {
      org.scalatest.Fact.No(
        if (shouldBeTrue)
          FailureMessages.wasNotAnInstanceOf(prettifier, left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName))
        else
          FailureMessages.wasAnInstanceOf(prettifier, left, UnquotedString(clazz.getName))
      )
    }
    else
      org.scalatest.Fact.Yes(
        if (shouldBeTrue)
          FailureMessages.wasAnInstanceOf(prettifier, left, UnquotedString(clazz.getName))
        else
          FailureMessages.wasNotAnInstanceOf(prettifier, left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName))
      )
  }*/

}
