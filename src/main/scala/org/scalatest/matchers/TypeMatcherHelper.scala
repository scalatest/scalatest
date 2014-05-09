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
import org.scalatest.words.{ResultOfATypeInvocation}

/**
 * <code>TypeMatcherHelper</code> is called by <code>TypeMatcherMacro</code> to support <code>a [Type]</code> and <code>an [Type]</code> syntax.
 */
object TypeMatcherHelper {

  def aTypeMatcher(aType: ResultOfATypeInvocation[_]): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val clazz = aType.clazz
        MatchResult(
          clazz.isAssignableFrom(left.getClass),
          Resources("wasNotAnInstanceOf"),
          Resources("wasAnInstanceOf"),
          Vector(left, UnquotedString(clazz.getName))
        )
      }
      override def toString: String = "be (" + Prettifier.default(aType) + ")"
    }

  def notATypeMatcher(aType: ResultOfATypeInvocation[_]): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val clazz = aType.clazz
        MatchResult(
          !clazz.isAssignableFrom(left.getClass),
          Resources("wasAnInstanceOf"),
          Resources("wasNotAnInstanceOf"),
          Vector(left, UnquotedString(clazz.getName))
        )
      }
      override def toString: String = "not be " + Prettifier.default(aType)
    }

  def checkAType(left: Any, aType: ResultOfATypeInvocation[_]) {
    val clazz = aType.clazz
    if (!clazz.isAssignableFrom(left.getClass)) {
      val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, clazz.getName)
      throw newTestFailedException(FailureMessages("wasNotAnInstanceOf", left, UnquotedString(clazz.getName)))
    }
  }

  def checkATypeShouldBeTrue(left: Any, aType: ResultOfATypeInvocation[_], shouldBeTrue: Boolean) {
    val clazz = aType.clazz
    if (clazz.isAssignableFrom(left.getClass) != shouldBeTrue) {
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "wasNotAnInstanceOf" else "wasAnInstanceOf",
          left,
          UnquotedString(clazz.getName)
        )
      )
    }
  }


}