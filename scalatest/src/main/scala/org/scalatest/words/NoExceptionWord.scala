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

import org.scalatest.{MatchersHelper, Resources}

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class NoExceptionWord(matchersHelper: MatchersHelper) {

  // SKIP-SCALATESTJS-START
  private val stackDepth = 1
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY private val stackDepth = 11
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * noException should be thrownBy { ... }
   *             ^
   * </pre>
   */
  def should(beWord: BeWord): ResultOfBeWordForNoException = 
    new ResultOfBeWordForNoException(matchersHelper)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * noException shouldBe thrownBy { ... }
   *             ^
   * </pre>
   */
  def shouldBe(thrownBy: ResultOfThrownByApplication): org.scalatest.Assertion = {
    try {
      thrownBy.execute()
      matchersHelper.indicateSuccess(Resources.noExceptionWasThrown)
    }
    catch {
      case u: Throwable => {
        val message = Resources.exceptionNotExpected(u.getClass.getName)
        matchersHelper.indicateFailure(message, Some(u), stackDepth)
      }
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * noException must be thrownBy { ... }
   *             ^
   * </pre>
   */
  def must(beWord: BeWord): ResultOfBeWordForNoException =
    new ResultOfBeWordForNoException(matchersHelper)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * noException mustBe thrownBy { ... }
   *             ^
   * </pre>
   */
  def mustBe(thrownBy: ResultOfThrownByApplication): org.scalatest.Assertion = {
    try {
      thrownBy.execute()
      matchersHelper.indicateSuccess(Resources.noExceptionWasThrown)
    }
    catch {
      case u: Throwable => {
        val message = Resources.exceptionNotExpected(u.getClass.getName)
        matchersHelper.indicateFailure(message, Some(u), stackDepth)
      }
    }
  }
  
  /**
   * Overrides toString to return "noException"
   */
  override def toString: String = "noException"
}
