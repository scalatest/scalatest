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

import org.scalatest.Resources
import org.scalatest.Assertions.newAssertionFailedException

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfAnTypeInvocation[T](val clazz: Class[T]) {

  // SKIP-SCALATESTJS-START
  private val stackDepth = 4
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY private val stackDepth = 12
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * an [Exception] should be thrownBy { ... }
   *                ^
   * </pre>
   */
  def should(beWord: BeWord): ResultOfBeWordForAnType[T] = 
    new ResultOfBeWordForAnType[T](clazz)

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
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass)) {
          val s = Resources.wrongException(clazz.getName, u.getClass.getName)
          throw newAssertionFailedException(Some(s), Some(u), stackDepth)
        }
        else {
          Some(u)
        }
      }
    }
    if (caught.isEmpty) {
      val message = Resources.exceptionExpected(clazz.getName)
      throw newAssertionFailedException(Some(message), None, stackDepth)
    } else org.scalatest.Succeeded
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
    new ResultOfBeWordForAnType[T](clazz)

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
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass)) {
          val s = Resources.wrongException(clazz.getName, u.getClass.getName)
          throw newAssertionFailedException(Some(s), Some(u), stackDepth)
        }
        else {
          Some(u)
        }
      }
    }
    if (caught.isDefined) {
      val message = Resources.exceptionExpected(clazz.getName)
      throw newAssertionFailedException(Some(message), None, stackDepth)
    } else org.scalatest.Succeeded
  }
  
  override def toString: String = "an [" + clazz.getName + "]"
}
