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

/**
 * Package object of jmock that contains deprecated type alias that will be removed in the future version of ScalaTest.
 */
package object jmock {

  @deprecated("JMockCycle has been moved from org.scalatest.jmock to org.scalatestplus.jmock. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type JMockCycle = org.scalatestplus.jmock.JMockCycle

  @deprecated("AsyncJMockCycleFixture has been moved from org.scalatest.jmock to org.scalatestplus.jmock. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type AsyncJMockCycleFixture = org.scalatestplus.jmock.AsyncJMockCycleFixture

  @deprecated("JMockCycleFixture has been moved from org.scalatest.jmock to org.scalatestplus.jmock. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type JMockCycleFixture = org.scalatestplus.jmock.JMockCycleFixture

  @deprecated("JMockExpectations has been moved from org.scalatest.jmock to org.scalatestplus.jmock. Please update your imports, as this deprecated type alias will be removed in a future version of ScalaTest.")
  type JMockExpectations = org.scalatestplus.jmock.JMockExpectations

}
