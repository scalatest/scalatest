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

import org.scalatest.{MatchersHelper, Resources, Assertion}
import org.scalatest.exceptions.TestFailedException

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfBeWordForAType[T](clazz: Class[T], matchersHelper: MatchersHelper) {
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * a [RuntimeException] should be thrownBy { ... }
   *                                ^
   * </pre>
   */
  def thrownBy(fun: => Any): Assertion = {
    // SKIP-SCALATESTJS-START
    val stackDepth = 0
    // SKIP-SCALATESTJS-END
    //SCALATESTJS-ONLY val stackDepth = 10
    try {
      matchersHelper.checkExpectedException(fun, clazz, Resources.wrongException _, Resources.exceptionExpected _, stackDepth)
      matchersHelper.indicateSuccess(Resources.exceptionThrown(clazz.getName))
    }
    catch {
      case tfe: TestFailedException => matchersHelper.indicateFailure(tfe)
    }
  }
  
  /**
   * Overrides toString to return pretty a[...] should/must be
   */
  override def toString: String = "ResultOfBeWordForAType(classOf[" + clazz.getName + "])"
}
