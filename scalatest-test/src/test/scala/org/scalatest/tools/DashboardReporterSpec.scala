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
package org.scalatest.tools

import org.scalatest._
import SharedHelpers._
import events._

class DashboardReporterSpec extends FunSpec {

  describe("DashboardReporter ") {
    
    it("should work with error message that contains '<' and '>' symbol") {
      val tempDir = createTempDirectory()
      val ordinal = new Ordinal(123)
      val rep = new DashboardReporter(tempDir.getAbsolutePath, 0)
      rep(RunStarting(ordinal.next, 1, ConfigMap.empty))
      rep(SuiteStarting(ordinal.next, "TestSuite", "com.test.TestSuite", Some("com.test.TestSuite")))
      rep(TestStarting(ordinal.next, "TestSuite", "com.test.TestSuite", Some("com.test.TestSuite"), "test 1", "test 1"))
      rep(TestFailed(ordinal.next, "a test using <function1> failed", "TestSuite", "com.test.TestSuite", Some("com.test.TestSuite"),
                     "test 1", "test 1", collection.immutable.IndexedSeq.empty[RecordableEvent], Some(new RuntimeException("a <function1> caused the problem"))))
      rep(SuiteCompleted(ordinal.next, "TestSuite", "com.test.TestSuite", Some("com.test.TestSuite")))
      rep(RunCompleted(ordinal.next))
    }
    
  }
  
}
