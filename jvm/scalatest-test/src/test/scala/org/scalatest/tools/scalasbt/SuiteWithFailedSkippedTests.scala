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
package org.scalatest.tools.scalasbt

import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite

class SuiteWithFailedSkippedTests extends AnyFunSuite {
  test("success") {}
  ignore("ignored") {}
  test("pending") { pending }
  test("failed") { fail() }
  test("canceled") { cancel() }
  
  override def nestedSuites = 
    collection.immutable.IndexedSeq(
      new AnyFunSuite() {
        override def suiteId = "nested 1"
          
        test("nested 1 success") {}
        ignore("nested 1 ignored") {}
        test("nested 1 pending") { pending }
        test("nested 1 failed") { fail() }
        test("nested 1 canceled") { cancel() }
      }, 
      new AnyFunSuite() {
        override def suiteId = "nested 2"
          
        test("nested 2 success") {}
        ignore("nested 2 ignored") {}
        test("nested 2 pending") { pending }
        test("nested 2 failed") { fail() }
        test("nested 2 canceled") { cancel() }
      },
      new AnyFunSuite() {
        override def suiteId = "nested 3"
        
        override def run(testName: Option[String], args: Args): Status = 
          throw new RuntimeException("Intended to abort suite")
      }
    )
}
