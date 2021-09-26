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
package org.scalatest.matchers.dsl

import org.scalatest.matchers.MatchersHelper.checkExpectedException
import org.scalatest.Resources
import org.scalatest.matchers.MatchersHelper.indicateSuccess
import org.scalatest.matchers.MatchersHelper.indicateFailure
import org.scalactic._

import scala.reflect.ClassTag

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfATypeInvocation[T](val clazzTag: ClassTag[T]) {

  val clazz: Class[T] = clazzTag.runtimeClass.asInstanceOf[Class[T]]

  def this(c: Class[_]) = this(ClassTag(c).asInstanceOf[ClassTag[T]])

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * a [RuntimeException] should be thrownBy { ... }
   *                      ^
   * </pre>
   **/
  def should(beWord: BeWord)(implicit prettifier: Prettifier, pos: source.Position): ResultOfBeWordForAType[T] =
    new ResultOfBeWordForAType[T](clazz, prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [RuntimeException] should not
   *                      ^
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
   * a [RuntimeException] shouldBe thrownBy { ... }
   *                      ^
   * </pre>
   **/
  def shouldBe(thrownBy: ResultOfThrownByApplication)(implicit prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = {
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

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [IllegalArgumentException] should (be thrownBy { ... })
   *                              ^
   * </pre>
   **/
  def should(beThrownBy: ResultOfBeThrownBy)(implicit prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = {
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

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [RuntimeException] must be thrownBy { ... }
   *                      ^
   * </pre>
   **/
  def must(beWord: BeWord)(implicit prettifier: Prettifier, pos: source.Position): ResultOfBeWordForAType[T] =
    new ResultOfBeWordForAType[T](clazz, prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [RuntimeException] must not
   *                      ^
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
   * a [RuntimeException] mustBe thrownBy { ... }
   *                      ^
   * </pre>
   **/
  def mustBe(thrownBy: ResultOfThrownByApplication)(implicit prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = {
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

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [IllegalArgumentException] must (be thrownBy { ... })
   *                              ^
   * </pre>
   **/
  def must(beThrownBy: ResultOfBeThrownBy)(implicit prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = {
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
  
  override def toString: String = "a [" + clazz.getName + "]"
}
