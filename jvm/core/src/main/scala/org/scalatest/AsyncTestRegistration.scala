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
package org.scalatest

import scala.concurrent.Future
import org.scalactic._

/**
 * <strong>The <code>org.scalatest.AsyncTestRegistration</code> has been deprecated and will be removed in a future version of ScalaTest. We do
 * not plan a replacement.</strong>
 *
 * Trait declaring methods that can be used to register by-name test functions that
 * have result type <code>Future[Assertion]</code>.
 */
@deprecated("The org.scalatest.AsyncTestRegistration trait has been deprecated and will be removed in a future version of ScalaTest. No replacement is planned.", "3.1.0")
trait AsyncTestRegistration { theSuite: AsyncTestSuite =>

  /**
   * Registers a test.
   *
   * @param testText the test text
   * @param testTags the test tags
   * @param testFun the test function
   */
  def registerAsyncTest(testText: String, testTags: Tag*)(testFun: => Future[compatible.Assertion])(implicit pos: source.Position): Unit

  /**
   * Registers an ignored test.
   *
   * @param testText the test text
   * @param testTags the test tags
   * @param testFun the test function
   */
  def registerIgnoredAsyncTest(testText: String, testTags: Tag*)(testFun: => Future[compatible.Assertion])(implicit pos: source.Position): Unit
}
