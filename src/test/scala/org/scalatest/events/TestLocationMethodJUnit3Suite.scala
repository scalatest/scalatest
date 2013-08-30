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

import org.scalatest.junit.JUnit3Suite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class TestLocationMethodJUnit3Suite extends JUnit3Suite with TestLocationMethodServices {
  val suiteTypeName = "org.scalatest.events.TestLocationMethodJUnit3Suite"
  val expectedStartingList = List(TestStartingPair("testSucceed(org.scalatest.events.TestLocationMethodJUnit3Suite)", "org.scalatest.events.TestLocationMethodJUnit3Suite", "testSucceed()"))
  val expectedResultList = List(TestResultPair(classOf[TestSucceeded], "org.scalatest.events.TestLocationMethodJUnit3Suite", "testSucceed()"))
  val expectedScopeOpenedList = Nil
  val expectedScopeClosedList = Nil
  
  def testSucceed() { 
    
  }
}
