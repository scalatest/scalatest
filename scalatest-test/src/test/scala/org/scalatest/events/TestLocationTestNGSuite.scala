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
package org.scalatest.events

import org.testng.annotations.Test
import org.scalatestplus.testng.TestNGSuite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class TestLocationTestNGSuite extends TestNGSuite with TestLocationServices {
  val suiteTypeName = "org.scalatest.events.TestLocationTestNGSuite"
  val expectedSuiteStartingList = Nil
  val expectedSuiteCompletedList = Nil
  val expectedSuiteAbortedList = Nil
  val expectedTestSucceededList = Nil
  val expectedTestFailedList = List(SeeStackDepthExceptionPair("testFail"))
  val expectedInfoProvidedList = Nil
  
  @Test
  def testFail(): Unit = { 
    fail
  }
}
