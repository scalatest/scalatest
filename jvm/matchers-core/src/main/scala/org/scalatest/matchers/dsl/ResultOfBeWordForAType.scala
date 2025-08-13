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
package org.scalatest.matchers.dsl

import org.scalatest.Resources
import org.scalatest.matchers.MatchersHelper.checkExpectedException
import org.scalatest.Assertion
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.MatchersHelper.indicateSuccess
import org.scalatest.matchers.MatchersHelper.indicateFailure
import org.scalactic._

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfBeWordForAType[T](clazz: Class[T], prettifier: Prettifier, pos: source.Position) {
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * a [RuntimeException] should be thrownBy { ... }
   *                                ^
   * </pre>
   **/
  //DOTTY-ONLY infix def thrownBy(fun: => Any): Assertion = {
  // SKIP-DOTTY-START 
  def thrownBy(fun: => Any): Assertion = {
  // SKIP-DOTTY-END  
    try {
      checkExpectedException(fun, clazz, Resources.wrongException _, Resources.exceptionExpected _, pos)
      indicateSuccess(Resources.exceptionThrown(clazz.getName))
    }
    catch {
      case tfe: TestFailedException => indicateFailure(tfe)
    }
  }
  
  /**
   * Overrides toString to return pretty a[...] should/must be
   */
  override def toString: String = "ResultOfBeWordForAType(classOf[" + clazz.getName + "])"
}
