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

import org.scalatest.OutcomeOf._

/**
 * Trait for test registration support.
 */
trait TestRegistration { theSuite: Suite =>

  /**
   * The return type of the registered test.
   */
  type Registration

  protected def transformToOutcome(testFun: => Registration): () => AsyncOutcome =
    () =>
      PastOutcome {
        outcomeOf { testFun }
      }

  /**
   * Register a test.
   *
   * @param testText the test text
   * @param testTags the test tags
   * @param testFun the test function
   */
  def registerTest(testText: String, testTags: Tag*)(testFun: => Registration)

  /**
   * Register an ignored test, note that an ignored test will not be executed, but it will cause a <code>TestIgnored</code>
   * event to be fired.
   *
   * @param testText the test text
   * @param testTags the test tags
   * @param testFun the test function
   */
  def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: => Registration)

}