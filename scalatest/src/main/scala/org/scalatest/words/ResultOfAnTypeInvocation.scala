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

import org.scalactic.source.SourceInfo
import org.scalatest.Resources
import org.scalatest.MatchersHelper.indicateSuccess
import org.scalatest.MatchersHelper.indicateFailure
import org.scalactic.Prettifier

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfAnTypeInvocation[T](val clazz: Class[T], val prettifier: Prettifier, val sourceInfo: SourceInfo) {

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * an [Exception] should be thrownBy { ... }
   *                ^
   * </pre>
   */
  def should(beWord: BeWord): ResultOfBeWordForAnType[T] = 
    new ResultOfBeWordForAnType[T](clazz, prettifier, sourceInfo)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * an [RuntimeException] should not
   *                       ^
   * </pre>
   *
   * This method is here to direct people trying to use the above syntax to use <code>noException</code> instead.
   */
  def should(notWord: NotWord): PleaseUseNoExceptionShouldSyntaxInstead =
    new PleaseUseNoExceptionShouldSyntaxInstead
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * an [RuntimeException] shouldBe thrownBy { ... }
   *                       ^
   * </pre>
   */
  def shouldBe(thrownBy: ResultOfThrownByApplication): org.scalatest.Assertion = {
    
    val caught = try {
      thrownBy.execute()
      None
    }
    catch {
      case u: Throwable => Some(u)
    }

    if (caught.isEmpty) {
      val message = Resources.exceptionExpected(clazz.getName)
      indicateFailure(message, None, sourceInfo)
    } else {
      val u = caught.get
      if (!clazz.isAssignableFrom(u.getClass)) {
        val s = Resources.wrongException(clazz.getName, u.getClass.getName)
        indicateFailure(s, Some(u), sourceInfo)
      } else indicateSuccess(Resources.exceptionThrown(u.getClass.getName))
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * an [RuntimeException] should (be thrownBy { ... })
   *                       ^
   * </pre>
   */
  def should(beThrownBy: ResultOfBeThrownBy): org.scalatest.Assertion = {
    val throwables = beThrownBy.throwables
    val noThrowable = throwables.find(_.isEmpty)
    if (noThrowable.isDefined) {
      val message = Resources.exceptionExpected(clazz.getName)
      indicateFailure(message, None, sourceInfo)
    }
    else {
      val unmatch = throwables.map(_.get).find(t => !clazz.isAssignableFrom(t.getClass))
      if (unmatch.isDefined) {
        val u = unmatch.get
        val s = Resources.wrongException(clazz.getName, u.getClass.getName)
        indicateFailure(s, Some(u), sourceInfo)
      }
      else indicateSuccess(Resources.exceptionThrown(clazz.getName))
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * an [Exception] must be thrownBy { ... }
   *                ^
   * </pre>
   */
  def must(beWord: BeWord): ResultOfBeWordForAnType[T] =
    new ResultOfBeWordForAnType[T](clazz, prettifier, sourceInfo)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * an [RuntimeException] must not
   *                       ^
   * </pre>
   *
   * This method is here to direct people trying to use the above syntax to use <code>noException</code> instead.
   */
  def must(notWord: NotWord): PleaseUseNoExceptionShouldSyntaxInstead =
    new PleaseUseNoExceptionShouldSyntaxInstead

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * an [RuntimeException] mustBe thrownBy { ... }
   *                       ^
   * </pre>
   */
  def mustBe(thrownBy: ResultOfThrownByApplication): org.scalatest.Assertion = {

    val caught = try {
      thrownBy.execute()
      None
    }
    catch {
      case u: Throwable => Some(u)
    }

    if (caught.isEmpty) {
      val message = Resources.exceptionExpected(clazz.getName)
      indicateFailure(message, None, sourceInfo)
    } else {
      val u = caught.get
      if (!clazz.isAssignableFrom(u.getClass)) {
        val s = Resources.wrongException(clazz.getName, u.getClass.getName)
        indicateFailure(s, Some(u), sourceInfo)
      } else indicateSuccess(Resources.exceptionThrown(u.getClass.getName))
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * an [RuntimeException] must (be thrownBy { ... })
   *                       ^
   * </pre>
   */
  def must(beThrownBy: ResultOfBeThrownBy): org.scalatest.Assertion = {
    val throwables = beThrownBy.throwables
    val noThrowable = throwables.find(_.isEmpty)
    if (noThrowable.isDefined) {
      val message = Resources.exceptionExpected(clazz.getName)
      indicateFailure(message, None, sourceInfo)
    }
    else {
      val unmatch = throwables.map(_.get).find(t => !clazz.isAssignableFrom(t.getClass))
      if (unmatch.isDefined) {
        val u = unmatch.get
        val s = Resources.wrongException(clazz.getName, u.getClass.getName)
        indicateFailure(s, Some(u), sourceInfo)
      }
      else indicateSuccess(Resources.exceptionThrown(clazz.getName))
    }
  }
  
  override def toString: String = "an [" + clazz.getName + "]"
}
